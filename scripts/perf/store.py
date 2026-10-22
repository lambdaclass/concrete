#!/usr/bin/env python3
"""Results store on the `perf-data` branch (never merged into main).

The branch is checked out as its own worktree (DATA). Layout:

  README.md                           what the branch is (written by `init`)
  data/<machine>/records.jsonl        one perflib record per measurement, append-only
  data/<machine>/processed            shas whose detection/bisect/notification finished
  data/<machine>/summary.json         detection summary for the newest processed record
  data/<machine>/bisect/<sha12>.json  bisect results for a regression

  store.py init      --data DATA
  store.py append    --data DATA --record REC.json [--no-commit]
  store.py has       --data DATA --machine M --sha SHA [--attempts N]
                     exit 0 when SHA needs no new measurement: a clean (ok/partial, not
                     contaminated) record exists, or N (default 3) attempts were already made
  store.py processed --data DATA --machine M --sha SHA [--mark]
                     exit 0 when SHA was fully processed; --mark records that it was
  store.py pending   --data DATA --machine M   print the newest measured-but-unprocessed sha
  store.py put       --data DATA --machine M --name REL --file F [--no-commit]
  store.py check     --data DATA                               validate every records.jsonl

Every mutation holds an exclusive lock on DATA/.git-adjacent `.store.lock` and is
rolled back when its git commit fails, so a record is never visible without its
commit and two writers never interleave.
"""

import argparse
import contextlib
import fcntl
import json
import os
import re
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import perflib  # noqa: E402

MACHINE_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.\-]*$")

README = """# perf-data

Compiler performance records written by `scripts/perf/` (runner.sh documents the layout).
This branch is DATA ONLY and is never merged into main.

- `data/<machine>/records.jsonl`: one JSON record per measurement of an origin/main commit
  (schema `concrete.perf.record.v1`), appended in measurement order.
- `data/<machine>/processed`: commits whose detection, bisect and notification completed.
- `data/<machine>/summary.json`: regression verdict for the newest processed record
  (schema `concrete.perf.summary.v1`).
- `data/<machine>/bisect/<sha12>.json`: bisect result naming the culprit commit.

Numbers from different machines, or different `suite_version`s, are never compared.
"""


def machine_dir(data, machine):
    if not MACHINE_RE.match(machine or "") or ".." in machine:
        raise ValueError("machine id %r is not path-safe" % machine)
    return os.path.join(data, "data", machine)


def contained(data, machine, rel):
    """Destination for REL under data/<machine>. Refuses anything that resolves outside DATA/data/
    <machine> (.., absolute paths, a symlinked machine directory, symlinks inside it) and refuses a
    destination that is itself a symlink."""
    root = os.path.realpath(data)
    base = os.path.join(root, "data", os.path.basename(machine_dir(data, machine)))
    if os.path.realpath(base) != base and os.path.lexists(base):
        raise ValueError("data/%s is a symlink" % machine)
    lexical = os.path.normpath(os.path.join(base, rel))
    dest = os.path.realpath(lexical)
    if os.path.commonpath([base, dest]) != base or dest == base or os.path.islink(lexical):
        raise ValueError("refusing a path outside data/%s: %r" % (machine, rel))
    return dest


def records_path(data, machine):
    return contained(data, machine, "records.jsonl")


def load(data, machine):
    p = records_path(data, machine)
    if not os.path.exists(p):
        return []
    return perflib.parse_records(open(p).read())


def git(data, *args, check=True):
    return subprocess.run(["git", "-C", data] + list(args), capture_output=True, text=True, check=check)


@contextlib.contextmanager
def locked(data):
    fd = os.open(os.path.join(data, ".store.lock"), os.O_CREAT | os.O_RDWR, 0o644)
    try:
        fcntl.flock(fd, fcntl.LOCK_EX)
        yield
    finally:
        fcntl.flock(fd, fcntl.LOCK_UN)
        os.close(fd)


def atomic_write(path, data_bytes):
    """Readers see the old file or the new one, never a truncated one; a crash leaves at most a
    stray temp file."""
    # mkstemp creates the temp file exclusively (O_EXCL), so a planted symlink is never followed.
    fd, tmp = tempfile.mkstemp(prefix=".%s." % os.path.basename(path), dir=os.path.dirname(path))
    try:
        with os.fdopen(fd, "wb") as f:
            f.write(data_bytes)
            f.flush()
            os.fsync(f.fileno())
        os.chmod(tmp, 0o644)
        os.replace(tmp, path)
    except BaseException:
        try:
            os.unlink(tmp)
        except OSError:
            pass
        raise


def write_and_commit(data, path, new_bytes, msg, no_commit):
    """Write PATH, commit it; restore the previous content if the commit fails."""
    old = open(path, "rb").read() if os.path.exists(path) else None
    os.makedirs(os.path.dirname(path), exist_ok=True)
    atomic_write(path, new_bytes)
    if no_commit:
        return
    try:
        rel = os.path.relpath(os.path.realpath(path), os.path.realpath(data))
        git(data, "add", "--", rel)
        if git(data, "diff", "--cached", "--quiet", check=False).returncode != 0:
            git(data, "-c", "user.name=concrete-perf", "-c", "user.email=perf@localhost",
                "commit", "-q", "-m", msg)
    except subprocess.CalledProcessError as ex:
        if old is None:
            os.unlink(path)
        else:
            atomic_write(path, old)
        git(data, "reset", "-q", "--", os.path.relpath(os.path.realpath(path), os.path.realpath(data)), check=False)
        raise RuntimeError("commit failed, write rolled back: %s" % (ex.stderr or ex))


def clean(rec):
    return rec.get("status") in ("ok", "partial") and not rec.get("contaminated") and rec.get("bench") is None


def cmd_init(a):
    with locked(a.data):
        p = os.path.join(a.data, "README.md")
        if not os.path.exists(p):
            write_and_commit(a.data, p, README.encode(), "perf-data: initialize", a.no_commit)
        ign = os.path.join(a.data, ".gitignore")
        if not os.path.exists(ign):
            write_and_commit(a.data, ign, b".store.lock\n", "perf-data: ignore the store lock", a.no_commit)
    return 0


def cmd_append(a):
    rec = json.load(open(a.record))
    line = perflib.dump_record(rec)          # validates, including the machine id
    machine = rec["machine"]
    with locked(a.data):
        existing = load(a.data, machine)
        if rec.get("bench") is None and any(r["sha"] == rec["sha"] and clean(r) for r in existing):
            sys.stderr.write("store: %s already has a clean record on %s; not appending twice\n"
                             % (rec["sha"][:12], machine))
            return 1
        p = records_path(a.data, machine)
        body = (open(p, "rb").read() if os.path.exists(p) else b"") + (line + "\n").encode()
        write_and_commit(a.data, p, body, "perf: %s %s %s%s" % (
            machine, rec["sha"][:12], rec["status"], " (contaminated)" if rec.get("contaminated") else ""),
            a.no_commit)
    return 0


def cmd_has(a):
    recs = [r for r in load(a.data, a.machine) if r["sha"] == a.sha and r.get("bench") is None]
    return 0 if any(clean(r) for r in recs) or len(recs) >= a.attempts else 1


def processed_set(p):
    return set(open(p).read().split()) if os.path.exists(p) else set()


def cmd_processed(a):
    p = contained(a.data, a.machine, "processed")
    if not a.mark:
        return 0 if a.sha in processed_set(p) else 1
    if not re.match(r"^[0-9a-f]{40}$", a.sha):
        sys.stderr.write("store: %r is not a full sha\n" % a.sha)
        return 2
    with locked(a.data):
        if a.sha not in processed_set(p):
            body = (open(p, "rb").read() if os.path.exists(p) else b"") + (a.sha + "\n").encode()
            write_and_commit(a.data, p, body, "perf: %s processed %s" % (a.machine, a.sha[:12]), False)
    return 0


def cmd_pending(a):
    """Print the sha of the newest full record that is not processed yet (nothing when none), so a
    cycle interrupted after measuring finishes that commit even if main has moved on; older
    unprocessed commits follow at later ticks."""
    done = processed_set(contained(a.data, a.machine, "processed"))
    for r in reversed(load(a.data, a.machine)):
        if r.get("bench") is None and r["sha"] not in done:
            print(r["sha"])
            break
    return 0


def cmd_put(a):
    try:
        dest = contained(a.data, a.machine, a.name)
    except ValueError as ex:
        sys.stderr.write("store: %s\n" % ex)
        return 2
    with locked(a.data):
        write_and_commit(a.data, dest, open(a.file, "rb").read(), "perf: %s %s" % (a.machine, a.name), a.no_commit)
    return 0


def cmd_check(a):
    root = os.path.join(a.data, "data")
    bad = 0
    for m in sorted(os.listdir(root)) if os.path.isdir(root) else []:
        try:
            recs = load(a.data, m)
            if any(r["machine"] != m for r in recs):
                raise ValueError("a record names another machine")
            print("ok   %s: %d records" % (m, len(recs)))
        except ValueError as ex:
            print("FAIL %s: %s" % (m, ex))
            bad += 1
    return 1 if bad else 0


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    for name in ("init", "append", "has", "processed", "pending", "put", "check"):
        p = sub.add_parser(name)
        p.add_argument("--data", required=True)
        if name in ("init", "append", "put"):
            p.add_argument("--no-commit", action="store_true")
        if name == "append":
            p.add_argument("--record", required=True)
        if name in ("has", "put", "processed", "pending"):
            p.add_argument("--machine", required=True)
        if name in ("has", "processed"):
            p.add_argument("--sha", required=True)
        if name == "has":
            p.add_argument("--attempts", type=int, default=3)
        if name == "processed":
            p.add_argument("--mark", action="store_true")
        if name == "put":
            p.add_argument("--name", required=True)
            p.add_argument("--file", required=True)
    a = ap.parse_args()
    try:
        return {"init": cmd_init, "append": cmd_append, "has": cmd_has, "processed": cmd_processed,
                "pending": cmd_pending,
                "put": cmd_put, "check": cmd_check}[a.cmd](a)
    except ValueError as ex:
        sys.stderr.write("store: %s\n" % ex)
        return 2


if __name__ == "__main__":
    sys.exit(main())
