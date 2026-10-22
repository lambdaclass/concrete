#!/usr/bin/env python3
"""Results store on the `perf-data` branch (never merged into main).

The branch is checked out as its own worktree (DATA). Layout:

  README.md                         what the branch is (written by `init`)
  data/<machine>/records.jsonl      one perflib record per measured commit, append-only
  data/<machine>/summary.json       detection summary for the newest record
  data/<machine>/bisect/<sha12>.json  bisect results for a regression

  store.py init    --data DATA
  store.py append  --data DATA --record REC.json [--no-commit]
  store.py has     --data DATA --machine M --sha SHA        exit 0 when SHA is recorded
  store.py put     --data DATA --machine M --name REL --file F [--no-commit]
  store.py check   --data DATA                               validate every records.jsonl
"""

import argparse
import json
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import perflib  # noqa: E402

README = """# perf-data

Compiler performance records written by `scripts/perf/` (see scripts/perf/README.md on the
tooling branch). This branch is DATA ONLY and is never merged into main.

- `data/<machine>/records.jsonl`: one JSON record per measured origin/main commit
  (schema `concrete.perf.record.v1`), appended in measurement order.
- `data/<machine>/summary.json`: regression verdict for the newest record
  (schema `concrete.perf.summary.v1`).
- `data/<machine>/bisect/<sha12>.json`: bisect result naming the culprit commit.

Numbers from different machines, or different `suite_version`s, are never compared.
"""


def records_path(data, machine):
    return os.path.join(data, "data", machine, "records.jsonl")


def load(data, machine):
    p = records_path(data, machine)
    if not os.path.exists(p):
        return []
    return perflib.parse_records(open(p).read())


def git(data, *args, check=True):
    return subprocess.run(["git", "-C", data] + list(args), capture_output=True, text=True, check=check)


def commit(data, paths, msg):
    git(data, "add", "--", *paths)
    if git(data, "diff", "--cached", "--quiet", check=False).returncode != 0:
        git(data, "-c", "user.name=concrete-perf", "-c", "user.email=perf@concrete-ci-1",
            "commit", "-q", "-m", msg)


def cmd_init(a):
    p = os.path.join(a.data, "README.md")
    if not os.path.exists(p):
        with open(p, "w") as f:
            f.write(README)
        if not a.no_commit:
            commit(a.data, ["README.md"], "perf-data: initialize")
    return 0


def cmd_append(a):
    rec = json.load(open(a.record))
    line = perflib.dump_record(rec)          # validates
    machine = rec["machine"]
    existing = load(a.data, machine)
    if any(r["sha"] == rec["sha"] and r.get("bench") is None for r in existing) and rec.get("bench") is None:
        sys.stderr.write("store: %s already recorded for %s; not appending twice\n" % (rec["sha"][:12], machine))
        return 1
    p = records_path(a.data, machine)
    os.makedirs(os.path.dirname(p), exist_ok=True)
    with open(p, "a") as f:
        f.write(line + "\n")
    if not a.no_commit:
        commit(a.data, [os.path.relpath(p, a.data)],
               "perf: %s %s %s%s" % (machine, rec["sha"][:12], rec["status"],
                                     " (contaminated)" if rec.get("contaminated") else ""))
    return 0


def cmd_has(a):
    return 0 if any(r["sha"] == a.sha for r in load(a.data, a.machine)) else 1


def cmd_put(a):
    dest = os.path.join(a.data, "data", a.machine, a.name)
    if os.path.relpath(dest, os.path.join(a.data, "data", a.machine)).startswith(".."):
        sys.stderr.write("store: refusing a path outside data/%s\n" % a.machine)
        return 2
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    with open(a.file) as src, open(dest, "w") as out:
        out.write(src.read())
    if not a.no_commit:
        commit(a.data, [os.path.relpath(dest, a.data)], "perf: %s %s" % (a.machine, a.name))
    return 0


def cmd_check(a):
    root = os.path.join(a.data, "data")
    bad = 0
    for m in sorted(os.listdir(root)) if os.path.isdir(root) else []:
        try:
            recs = load(a.data, m)
            print("ok   %s: %d records" % (m, len(recs)))
        except ValueError as ex:
            print("FAIL %s: %s" % (m, ex))
            bad += 1
    return 1 if bad else 0


def main():
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(dest="cmd", required=True)
    for name in ("init", "append", "has", "put", "check"):
        p = sub.add_parser(name)
        p.add_argument("--data", required=True)
        if name in ("init", "append", "put"):
            p.add_argument("--no-commit", action="store_true")
        if name == "append":
            p.add_argument("--record", required=True)
        if name in ("has", "put"):
            p.add_argument("--machine", required=True)
        if name == "has":
            p.add_argument("--sha", required=True)
        if name == "put":
            p.add_argument("--name", required=True)
            p.add_argument("--file", required=True)
    a = ap.parse_args()
    return {"init": cmd_init, "append": cmd_append, "has": cmd_has, "put": cmd_put, "check": cmd_check}[a.cmd](a)


if __name__ == "__main__":
    sys.exit(main())
