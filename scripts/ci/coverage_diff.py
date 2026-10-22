#!/usr/bin/env python3
"""Mutation-coverage diff between two nightly campaign runs (markdown on stdout).

Usage: coverage_diff.py PREV_DIR CUR_DIR

Each directory is a `gh run download <run>` of a campaign run: it holds
`mutation-campaign-summary/mutation-campaign-summary.txt` (the aggregate verdict, key=value) and
`mutation-shard-<k>/evidence/<family>/verdict.txt` (each family's `disposition=`). PREV_DIR may be
missing or empty: the first night has nothing to compare against, which is said, not hidden.

Reported: the aggregate counts side by side, families whose disposition changed, and families
added or removed. A REGRESSION is a family that was killed and now is not, a family that is no
longer discovered, or a drop in the discovered count; the exit status is 1 when there is one, so
the nightly report job goes red on lost coverage even when the aggregate itself still qualifies.
Exit 2 when CUR_DIR has no aggregate summary (the campaign did not produce one).
"""
import glob
import os
import sys

KEYS = ("discovered", "killed", "survived", "invalid", "could_not_apply", "pending",
        "completed", "integrity_ok", "qualified")


def summary(d):
    p = os.path.join(d, "mutation-campaign-summary", "mutation-campaign-summary.txt")
    if not os.path.isfile(p):
        return None
    kv = {}
    for line in open(p, encoding="utf-8"):
        if "=" in line:
            k, v = line.rstrip("\n").split("=", 1)
            kv[k] = v
    return kv


def families(d):
    out = {}
    for v in glob.glob(os.path.join(d, "mutation-shard-*", "evidence", "*", "verdict.txt")):
        fam = os.path.basename(os.path.dirname(v))
        for line in open(v, encoding="utf-8"):
            if line.startswith("disposition="):
                out[fam] = line.rstrip("\n").split("=", 1)[1]
    return out


def main(argv):
    if len(argv) != 2:
        print(__doc__, file=sys.stderr)
        return 2
    prev_dir, cur_dir = argv
    cur = summary(cur_dir)
    if cur is None:
        print("### Mutation coverage diff\n\nThe current run has no mutation-campaign-summary artifact: "
              "the campaign did not produce a verdict, so there is no coverage to compare.")
        return 2
    prev = summary(prev_dir) if prev_dir and os.path.isdir(prev_dir) else None
    print("### Mutation coverage diff\n")
    if prev is None:
        print("No previous nightly campaign summary to compare against (first night, or its artifacts "
              "expired). Current counts:\n")
        print("| key | current |\n|---|---|")
        for k in KEYS:
            print(f"| {k} | {cur.get(k, '-')} |")
        return 0
    regress = []
    print(f"Previous head `{prev.get('head', '?')[:12]}`, current head `{cur.get('head', '?')[:12]}`.\n")
    print("| key | previous | current |\n|---|---|---|")
    for k in KEYS:
        mark = "" if prev.get(k) == cur.get(k) else " (changed)"
        print(f"| {k} | {prev.get(k, '-')} | {cur.get(k, '-')}{mark} |")
    try:
        if int(cur.get("discovered", 0)) < int(prev.get("discovered", 0)):
            regress.append(f"discovered families dropped {prev['discovered']} -> {cur['discovered']}")
    except ValueError:
        pass
    pf, cf = families(prev_dir), families(cur_dir)
    changed = sorted(f for f in set(pf) & set(cf) if pf[f] != cf[f])
    added, removed = sorted(set(cf) - set(pf)), sorted(set(pf) - set(cf))
    for f in changed:
        if pf[f] == "killed" and cf[f] != "killed":
            regress.append(f"family {f}: killed -> {cf[f]}")
    for f in removed:
        if pf[f] == "killed":
            regress.append(f"family {f}: killed last night, absent now")
    print("")
    if not pf or not cf:
        print(f"Per-family records: previous {len(pf)}, current {len(cf)} "
              "(shard artifacts missing on one side; family-level diff limited).\n")
    print(f"Families: {len(cf)} now, {len(pf)} before; {len(changed)} changed, {len(added)} added, "
          f"{len(removed)} removed.\n")
    for f in changed:
        print(f"- `{f}`: {pf[f]} -> {cf[f]}")
    for f in added:
        print(f"- `{f}`: new ({cf[f]})")
    for f in removed:
        print(f"- `{f}`: gone (was {pf[f]})")
    if regress:
        print("\n**Coverage regressions:**\n")
        for r in regress:
            print(f"- REGRESSION {r}")
        return 1
    print("\nNo coverage regression.")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
