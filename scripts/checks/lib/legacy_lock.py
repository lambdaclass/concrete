#!/usr/bin/env python3
"""Canonical byte grammar and append-only history check for the legacy lock.

Run with python3 -I. `rows FILE|-` validates before emitting any rows;
`history LOCK PATH TRUSTED` checks all first-parent versions against LOCK.
The header is base OID, tree OID, followed by sweep OID OID records.
Only ASCII, single spaces, lowercase 40/64-digit hex OIDs and final LF
are accepted. Consumers must use this module rather than interpret bytes.
"""

import subprocess
import sys
from dataclasses import dataclass

LOCK_PATH = "scripts/checks/legacy.lock"


class LockError(ValueError):
    """Validation failed; problems are stable, ordered user diagnostics."""

    def __init__(self, problems):
        self.problems = problems
        super().__init__("\n".join(problems))


@dataclass(frozen=True)
class Lock:
    rows: tuple[str, ...]

    @property
    def tree(self):
        return self.rows[1].split(" ")[1]


def row_problem(raw, number, terminated):
    """Validate bytes before decoding, then the positional record grammar."""
    if 0 in raw:
        return "holds a NUL byte: the lock is text"
    if 13 in raw:
        return "holds a carriage return: a line ends with a newline (LF) alone"
    if not raw.isascii():
        return "holds a non-ASCII byte"
    if not terminated:
        return "has no newline: every line, the last too, ends with one"
    text = raw.decode("ascii")
    if text == "preview" or text.startswith("preview "):
        return (f"the move is a --preview build ({text}); a lock may only record a move "
                "generated with its landing prerequisites met (the generator is "
                "legacy/scripts/land/legacy_move.sh since the 2026-11-08 sweep)")
    kind, shape, count = (("base", "base <commit>", 1) if number == 1 else
                          ("tree", "tree <tree>", 1) if number == 2 else
                          ("sweep", "sweep <commit> <tree>", 2))
    fields = text.split(" ")
    if (fields[0] != kind or len(fields) != count + 1 or
            any(len(oid) not in (40, 64) or
                any(c not in "0123456789abcdef" for c in oid) for oid in fields[1:])):
        return (f"is not a lock row: line {number} must be '{shape}', one space between "
                "fields, each oid 40 or 64 lowercase hex digits")
    return None


def parse(data, name):
    if not data:
        raise LockError([f"{name}: empty: line 1 must be 'base <commit>'"])
    # split on LF alone: splitlines would silently normalize forbidden separators.
    records = data.split(b"\n")
    final_lf = records[-1] == b""
    if final_lf:
        records.pop()
    errors = []
    first_occurrence = {}
    result = []
    for index, raw in enumerate(records):
        number = index + 1
        problem = row_problem(raw, number, final_lf or number < len(records))
        if problem is None:
            text = raw.decode("ascii")
            if text in first_occurrence:
                problem = f"repeats line {first_occurrence[text]}: a row is recorded once"
            else:
                first_occurrence[text] = number
                result.append(text)
        if problem is not None:
            errors.append(f"{name}:{number}: {problem}")
    if len(records) < 2:
        errors.append(f"{name}:2: missing: line 2 must be 'tree <tree>'")
    if errors:
        raise LockError(errors)
    return result


def load(path):
    if path == "-":
        return Lock(tuple(parse(sys.stdin.buffer.read(), "<stdin>")))
    with open(path, "rb") as source:
        return Lock(tuple(parse(source.read(), path)))


def read(path):
    return list(load(path).rows)


def git(*args):
    process = subprocess.run(["git", *args], capture_output=True)
    return process.returncode, process.stdout


def trusted_versions(path, trusted):
    status, listing = git("log", "--first-parent", "--diff-merges=first-parent",
                          "--no-patch", "--format=%H", trusted, "--", path)
    if status:
        yield None, f"lock: cannot list the trusted history of {path} through {trusted[:10]}"
        return
    for commit in listing.decode("ascii").split():
        label = f"{commit[:10]}:{path}"
        status, content = git("cat-file", "blob", f"{commit}:{path}")
        if status:
            status, entry = git("ls-tree", commit, "--", path)
            if status or entry:
                yield None, f"lock: cannot read trusted version {label}"
            continue  # A genuinely absent path is a deletion, not a failed read.
        try:
            yield (commit, parse(content, label)), None
        except LockError as error:
            for problem in error.problems:
                yield None, f"lock: a trusted version is malformed: {problem}"


def history(lock, lpath, trusted):
    remaining = set(load(lock).rows)
    missing = set()
    failures = []
    for version, failure in trusted_versions(lpath, trusted):
        if failure is not None:
            failures.append(failure)
            continue
        commit, rows = version
        for row in rows:
            if row in remaining or row in missing:
                continue
            missing.add(row)
            failures.append(f"lock: '{row}' (in {commit[:10]}, trusted history) is not in the lock: "
                            "recorded lines are only ever added, never rewritten or removed")
    return failures


def main(argv):
    usage = "usage: legacy_lock.py rows FILE|- | history LOCK PATH TRUSTED"
    if not argv or (argv[0], len(argv)) not in (("rows", 2), ("history", 4)):
        print(usage, file=sys.stderr)
        return 2
    try:
        if argv[0] == "rows":
            output = load(argv[1]).rows
            status = 0
        else:
            output = history(*argv[1:])
            status = int(bool(output))
    except LockError as error:
        output, status = error.problems, 1
    except OSError as error:
        print(f"legacy_lock.py: {error}", file=sys.stderr)
        return 2
    for line in output:
        print(line)
    return status


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
