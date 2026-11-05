#!/usr/bin/env python3
"""THE REQUIRED-CHECK VERDICT FOR ONE CANDIDATE (train.sh watch and land).

Usage: required_checks.py BASE_FILE CAND_FILE TRANSITION_FILE JOBS_FILE
  BASE_FILE        required-checks.txt as the base (main) has it; empty file if absent
  CAND_FILE        required-checks.txt as the candidate has it; empty file if absent
  TRANSITION_FILE  required-checks-transition.tsv as the candidate has it; empty file if absent
  JOBS_FILE        the run's jobs, "<tier>\\t<conclusion>\\t<id>\\t<name>" per line (train.sh run_jobs)
  (a missing file and an empty file are the same: "the commit has no such file")

Prints one line per finding and exits 0; train.sh refuses when any line starts with an unmet kind:
  missing: <job>            a required job is absent from the run
  <conclusion>: <job>       a required job did not succeed (skipped, cancelled, failure, pending...)
  invalid: <reason>         the files themselves are unusable (nothing required, a bad transition row)
and prints notes that never refuse and never count as a pass:
  retired: <job> (<reason>) a base requirement the candidate retires with no replacement; NOT RUN
  replaced: <job> by <jobs> a base requirement whose replacements are required (and checked) instead
  stale: <job>              a transition row for a job the base no longer requires (delete the row)

THE RULE. The required set is the candidate's list plus every job the base requires, except a base
job the candidate drops through an explicit transition row. Without a row, a dropped base job stays
required, so a candidate cannot weaken main's requirements by editing its own list. A row is
`old_job<TAB>replacements<TAB>reason`: replacements is a comma-separated list of jobs that must
each be in the candidate's list (so they are required and checked at this exact SHA), or `-` to
retire the old job outright, which needs a reason and is reported as retired, never as passed.
A row for a job the candidate still requires is contradictory and refused. Files are read
separately and newline-terminated, so names from two files never fuse.
"""
import sys

UNMET = ("missing:", "invalid:")


def names(path):
    try:
        text = open(path).read()
    except OSError:
        return set()
    out = set()
    for line in text.splitlines():
        line = line.split("#", 1)[0].strip()
        if line:
            out.add(line)
    return out


def transition(path):
    rows, bad = {}, []
    try:
        text = open(path).read()
    except OSError:
        return rows, bad
    for n, line in enumerate(text.splitlines(), 1):
        if not line.strip() or line.startswith("#"):
            continue
        f = line.split("\t")
        if f[0] == "old_job":
            continue  # header
        if len(f) != 3 or not all(x.strip() for x in f):
            bad.append(f"transition row {n} needs old_job, replacements, reason (tab-separated, non-empty)")
            continue
        old, repl, why = (x.strip() for x in f)
        if old in rows:
            bad.append(f"transition row {n} repeats {old}")
            continue
        if repl == "-":
            rows[old] = ([], why)
            continue
        parts = [r.strip() for r in repl.split(",")]
        if not all(parts):
            # "a,,b" or "," must not degrade into a retirement: only a literal "-" retires
            bad.append(f"transition row {n} has an empty replacement name in {repl!r} (use '-' to retire)")
            continue
        rows[old] = (parts, why)
    return rows, bad


def plan(base, cand, rows, bad):
    """-> (required set, findings list)"""
    out = [f"invalid: {b}" for b in bad]
    if not base and not cand:
        out.append("invalid: required-checks.txt (neither the base nor the candidate has one)")
        return set(), out
    req = set(cand)
    for old in sorted(base):
        if old in cand:
            continue
        if old not in rows:
            req.add(old)  # dropped without a transition row: still required
            continue
        repl, why = rows[old]
        if not repl:
            out.append(f"retired: {old} ({why}); not run, not passed")
            continue
        missing = [r for r in repl if r not in cand]
        if missing:
            out.append(f"invalid: replacement {', '.join(missing)} for {old} is not in the candidate's required-checks.txt")
            req.add(old)
            continue
        out.append(f"replaced: {old} by {', '.join(repl)}")
    for old in sorted(rows):
        if old in cand:
            out.append(f"invalid: transition row for {old}, which the candidate still requires")
        elif old not in base:
            out.append(f"stale: {old} (the base does not require it; delete the row)")
    return req, out


def verdict(req, jobs_path):
    seen = {}
    for line in open(jobs_path):
        f = line.rstrip("\n").split("\t")
        if len(f) >= 4:
            seen.setdefault(f[3], []).append(f[1])
    out = []
    for name in sorted(req):
        c = seen.get(name)
        if not c:
            out.append("missing: " + name)
        elif any(x != "success" for x in c):
            out.append(",".join(sorted(set(c))) + ": " + name)
    return out


def main():
    if len(sys.argv) != 5:
        sys.exit(__doc__)
    base, cand = names(sys.argv[1]), names(sys.argv[2])
    rows, bad = transition(sys.argv[3])
    req, notes = plan(base, cand, rows, bad)
    for line in notes + verdict(req, sys.argv[4]):
        print(line)


if __name__ == "__main__":
    main()
