#!/usr/bin/env python3
"""Print basenames of successful execve calls in a strace -f execve log."""

import os
import json
import re
import sys

CALL = re.compile(r'execve(at)?\((?:[^," ]+,\s*)?"((?:[^"\\]|\\.)*)"')
RESULT = re.compile(r'\)\s*=\s*(-?[0-9]+)(?:\s|$)')
RESUMED = re.compile(r'<\.\.\. execve(at)? resumed>')
PID = re.compile(r'^\s*(?:\[pid\s+)?([0-9]+)\]?\s+')


def parse(lines, paths=False):
    """Keep syscall state per trace; admit only completed successful executions.

    Relative execveat targets cannot be resolved without fd/cwd tracking. Reject
    them explicitly rather than guessing an identity. Absolute execveat is supported.
    """
    found, pending = set(), {}
    for line_number, line in enumerate(lines, 1):
        pid_match = PID.match(line)
        pid = pid_match.group(1) if pid_match else "main"
        call, resumed = CALL.search(line), RESUMED.search(line)
        if call:
            if pid in pending:
                raise ValueError(f"line {line_number}: overlapping exec calls")
            path = json.loads('"' + call.group(2) + '"')
            if "<unfinished ...>" in line:
                pending[pid] = path
                continue
        elif resumed:
            if pid not in pending:
                raise ValueError(f"line {line_number}: resumed exec has no unfinished call")
            path = pending.pop(pid)
        else:
            if "execve" in line:
                raise ValueError(f"line {line_number}: unrecognised exec syscall")
            continue
        result = RESULT.search(line)
        if not result:
            raise ValueError(f"line {line_number}: missing exec result")
        if int(result.group(1)) == 0:
            if not os.path.isabs(path):
                raise ValueError(f"line {line_number}: unresolved relative executable {path!r}")
            found.add(path if paths else os.path.basename(path))
    if pending:
        raise ValueError(f"unfinished exec calls have no result: {', '.join(sorted(pending))}")
    return sorted(found)


def parse_many(log_paths, paths=False):
    """Parse each trace session independently so unfinished PID state cannot cross sessions."""
    found = set()
    for log_path in log_paths:
        with open(log_path, encoding="utf-8", errors="replace") as log:
            found.update(parse(log, paths=paths))
    return sorted(found)


def main():
    paths = "--paths" in sys.argv
    if paths: sys.argv.remove("--paths")
    if len(sys.argv) < 2:
        raise ValueError("usage: execve_names.py [--paths] LOG [LOG ...]")
    for name in parse_many(sys.argv[1:], paths=paths):
        print(name)


if __name__ == "__main__":
    try:
        main()
    except (OSError, UnicodeError, ValueError) as error:
        print(f"execve_names: {error}", file=sys.stderr)
        sys.exit(1)
