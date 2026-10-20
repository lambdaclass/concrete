#!/usr/bin/env python3
"""Generate ROADMAP.md's "Implemented, not yet accepted" table from docs/project/candidates.tsv.

  candidate_table.py            print the generated table
  candidate_table.py --write    replace the table between the markers in ROADMAP.md
  candidate_table.py --check    exit 1 when ROADMAP.md's table differs from the record (stale or
                                hand-edited); check_docs_drift.sh runs this

FAIL-CLOSED: a malformed record (wrong header, wrong column count, an empty cell, a SHA that is not
40 hex, a result that is not at the candidate SHA, a stage (c) pass without a run id or without a
stage (b) pass, a row naming neither bugs nor a deliverable, a duplicate id or bug) refuses with exit
2 and generates nothing. Missing markers, or more than one pair, also refuse.

--record / --roadmap override the inputs (test seams for the gate's negative controls).
"""
import argparse
import os
import re
import sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))
COLS = ["id", "bugs", "deliverable", "branch", "sha", "exits",
        "stage_a", "stage_b", "stage_c", "unlocks", "remaining"]
BEGIN = "<!-- BEGIN GENERATED: candidate record (scripts/gen/candidate_table.py; edit docs/project/candidates.tsv) -->"
END = "<!-- END GENERATED: candidate record -->"
RESULT = re.compile(r"^(?:not-run|(passed|failed)@([0-9a-f]{40})(?: run=(\d+))?)$")


def die(msg):
    print(f"candidate record: {msg}", file=sys.stderr)
    sys.exit(2)


def load(path):
    try:
        lines = open(path, encoding="utf-8").read().split("\n")
    except OSError as e:
        die(f"cannot read {path}: {e}")
    body = [ln for ln in lines if ln and not ln.startswith("#")]
    if not body or body[0].split("\t") != COLS:
        die(f"{path}: header must be exactly: {' '.join(COLS)} (tab-separated)")
    rows, ids, bugs_seen = [], set(), {}
    for n, ln in enumerate(body[1:], 2):
        cells = ln.split("\t")
        if len(cells) != len(COLS):
            die(f"{path} data row {n}: {len(cells)} cells, expected {len(COLS)}")
        r = dict(zip(COLS, (c.strip() for c in cells)))
        for k, v in r.items():
            if not v:
                die(f"{path} row {r['id'] or n}: empty {k} (use - for none)")
        if r["id"] in ids:
            die(f"duplicate candidate id {r['id']}")
        ids.add(r["id"])
        if r["bugs"] != "-":
            bl = r["bugs"].split(",")
            if not all(re.fullmatch(r"\d{3}", b) for b in bl):
                die(f"{r['id']}: bugs must be comma-separated three-digit numbers, got {r['bugs']}")
            for b in bl:
                if b in bugs_seen:
                    die(f"bug {b} is in candidates {bugs_seen[b]} and {r['id']}")
                bugs_seen[b] = r["id"]
        if r["bugs"] == "-" and r["deliverable"] == "-":
            die(f"{r['id']}: names neither bugs nor a deliverable")
        if not re.fullmatch(r"[0-9a-f]{40}", r["sha"]):
            die(f"{r['id']}: sha must be a full 40-hex commit, got {r['sha']}")
        res = {}
        for st in ("stage_a", "stage_b", "stage_c"):
            m = RESULT.match(r[st])
            if not m:
                die(f"{r['id']}: {st} must be not-run or passed@<sha>/failed@<sha> [run=<id>], got {r[st]}")
            if m.group(2) and m.group(2) != r["sha"]:
                die(f"{r['id']}: {st} ran at {m.group(2)[:12]}, not the candidate {r['sha'][:12]}; "
                    "reset it to not-run or record the run at the candidate")
            res[st] = m
        if res["stage_c"].group(1) == "passed":
            if not res["stage_c"].group(3):
                die(f"{r['id']}: a stage (c) pass needs its run id (the qualification record)")
            if res["stage_b"].group(1) != "passed":
                die(f"{r['id']}: a stage (c) pass needs a stage (b) pass at the same SHA")
        rows.append(r)
    if not rows:
        die(f"{path}: no candidates (an empty table must be stated, not generated from nothing)")
    return rows


def cell(v):
    return v.replace("|", "\\|")


def stage(v):
    m = RESULT.match(v)
    if not m.group(1):
        return "not run"
    out = f"{m.group(1)} at `{m.group(2)[:12]}`"
    return out + (f" (run {m.group(3)})" if m.group(3) else "")


def render(rows):
    out = [BEGIN,
           "| candidate | bugs | deliverable | branch | candidate SHA | exits | stage (a) | stage (b) | stage (c) | unlocks | remaining acceptance step |",
           "|---|---|---|---|---|---|---|---|---|---|---|"]
    for r in rows:
        bugs = ", ".join(r["bugs"].split(",")) if r["bugs"] != "-" else "—"
        out.append("| " + " | ".join([
            cell(r["id"]), bugs, cell(r["deliverable"]) if r["deliverable"] != "-" else "—",
            f"`{r['branch']}`", f"`{r['sha'][:12]}`", cell(r["exits"]),
            stage(r["stage_a"]), stage(r["stage_b"]), stage(r["stage_c"]),
            cell(r["unlocks"]), cell(r["remaining"])]) + " |")
    out.append(END)
    return "\n".join(out)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--record", default=os.path.join(ROOT, "docs", "project", "candidates.tsv"))
    ap.add_argument("--roadmap", default=os.path.join(ROOT, "ROADMAP.md"))
    g = ap.add_mutually_exclusive_group()
    g.add_argument("--write", action="store_true")
    g.add_argument("--check", action="store_true")
    a = ap.parse_args()
    table = render(load(a.record))
    if not (a.write or a.check):
        print(table)
        return 0
    try:
        road = open(a.roadmap, encoding="utf-8").read()
    except OSError as e:
        die(f"cannot read {a.roadmap}: {e}")
    if road.count(BEGIN) != 1 or road.count(END) != 1 or road.index(BEGIN) > road.index(END):
        die(f"{a.roadmap} must hold exactly one BEGIN/END generated candidate-record marker pair")
    i, j = road.index(BEGIN), road.index(END) + len(END)
    if a.check:
        if road[i:j] != table:
            print(f"candidate record: {a.roadmap}'s table differs from {a.record}; edit the record and run "
                  "scripts/gen/candidate_table.py --write (the table is generated, never hand-edited)",
                  file=sys.stderr)
            return 1
        print(f"candidate record: the roadmap table matches the record ({table.count(chr(10)) - 3} rows)")
        return 0
    with open(a.roadmap, "w", encoding="utf-8") as fh:
        fh.write(road[:i] + table + road[j:])
    return 0


if __name__ == "__main__":
    sys.exit(main())
