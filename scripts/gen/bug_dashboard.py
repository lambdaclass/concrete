#!/usr/bin/env python3
"""Generate ROADMAP.md's bug dashboard from the bug records.

  bug_dashboard.py            print the generated dashboard
  bug_dashboard.py --write    replace the dashboard between the markers in ROADMAP.md
  bug_dashboard.py --check    exit 1 when ROADMAP.md's dashboard differs from the records (stale or
                              hand-edited); check_docs_drift.sh runs this

Inputs, each the single owner of its fact:
  docs/bugs/NNN_*.md                 status: a record whose first **Status:** line starts with OPEN is open
  docs/bugs/STRUCTURAL_CLASSES.md    the summary table's classes; a bug's primary class is the first
                                     class row, in table order, that lists it
  docs/bugs/discovery.tsv            which probe batch found each bug from 079 on, and which are silent
  ROADMAP.md "Phase 1 exits"         which exits name each bug in their completion condition

The discovery-rate signal (Phase 1 exit 2m): a COUNTING batch (an independent probe kind) is quiet
when it found no silent bug and at most one loud bug; the signal holds when the last
QUIET_RUN_NEEDED counting batches are all quiet.

FAIL-CLOSED (exit 2, nothing generated): a malformed discovery record (wrong header, wrong column
count, an empty cell, a bad date or kind, a malformed bug list, a silent bug the row does not list,
dates out of order), a bug from 079 on with a record but no batch, a bug in two batches or in a batch
with no record, a structural-class table that cannot be read, or a missing Phase 1 exits table.
Missing markers, or more than one pair, also refuse.

--root overrides the repository root (a test seam for the gate's negative controls).
"""
import argparse
import glob
import os
import re
import sys

BEGIN = "<!-- BEGIN GENERATED: bug dashboard (scripts/gen/bug_dashboard.py; edit docs/bugs records and docs/bugs/discovery.tsv) -->"
END = "<!-- END GENERATED: bug dashboard -->"
COLS = ["date", "kind", "probe", "effort", "bugs", "silent"]
COUNTING = {"hunt", "examples", "workload", "design-proofs", "semantics", "spec", "fuzz", "interp-coverage"}
ATTRIBUTION = {"ci", "review", "follow-on", "controls", "development"}
FIRST_ATTRIBUTED = 79
QUIET_RUN_NEEDED = 3
RECENT = 6


def die(msg):
    print(f"bug dashboard: {msg}", file=sys.stderr)
    sys.exit(2)


def read(path):
    try:
        return open(path, encoding="utf-8").read()
    except OSError as e:
        die(f"cannot read {path}: {e}")


def bug_list(v, what):
    if v == "-":
        return []
    bl = v.split(",")
    if not all(re.fullmatch(r"\d{3}", b) for b in bl):
        die(f"{what} must be comma-separated three-digit bug numbers or -, got {v}")
    if len(set(bl)) != len(bl):
        die(f"{what} lists a bug twice: {v}")
    return bl


def load_records(root):
    recs = {}
    for p in sorted(glob.glob(os.path.join(root, "docs", "bugs", "[0-9][0-9][0-9]_*.md"))):
        num = os.path.basename(p)[:3]
        if num in recs:
            die(f"bug {num} has two records")
        m = re.search(r"^\*\*Status:\*\*\s*(.*)$", read(p), re.M)
        recs[num] = bool(m and m.group(1).startswith("OPEN"))
    if not recs:
        die("no bug records under docs/bugs")
    return recs


def load_discovery(root, recs):
    path = os.path.join(root, "docs", "bugs", "discovery.tsv")
    body = [ln for ln in read(path).split("\n") if ln and not ln.startswith("#")]
    if not body or body[0].split("\t") != COLS:
        die(f"{path}: header must be exactly: {' '.join(COLS)} (tab-separated)")
    rows, seen, last_date = [], {}, ""
    for n, ln in enumerate(body[1:], 2):
        cells = ln.split("\t")
        if len(cells) != len(COLS):
            die(f"{path} data row {n}: {len(cells)} cells, expected {len(COLS)}")
        r = dict(zip(COLS, (c.strip() for c in cells)))
        for k, v in r.items():
            if not v:
                die(f"{path} data row {n}: empty {k} (use - for none)")
        if not re.fullmatch(r"\d{4}-\d{2}-\d{2}", r["date"]):
            die(f"{path} data row {n}: date must be YYYY-MM-DD, got {r['date']}")
        if r["date"] < last_date:
            die(f"{path} data row {n}: dates must be in order ({r['date']} after {last_date})")
        last_date = r["date"]
        if r["kind"] not in COUNTING | ATTRIBUTION:
            die(f"{path} data row {n}: kind {r['kind']} is not one of {', '.join(sorted(COUNTING | ATTRIBUTION))}")
        r["bugs"] = bug_list(r["bugs"], f"{path} data row {n} bugs")
        r["silent"] = bug_list(r["silent"], f"{path} data row {n} silent")
        if not set(r["silent"]) <= set(r["bugs"]):
            die(f"{path} data row {n}: silent bugs {sorted(set(r['silent']) - set(r['bugs']))} are not in its bugs")
        for b in r["bugs"]:
            if b in seen:
                die(f"bug {b} is in discovery rows {seen[b]} and {n}")
            if b not in recs:
                die(f"{path} data row {n}: bug {b} has no record docs/bugs/{b}_*.md")
            seen[b] = n
        rows.append(r)
    missing = sorted(b for b in recs if int(b) >= FIRST_ATTRIBUTED and b not in seen)
    if missing:
        die(f"bugs {', '.join(missing)} have records but no discovery batch in {path}; add each to the "
            "batch that found it")
    return rows


def load_classes(root):
    text = read(os.path.join(root, "docs", "bugs", "STRUCTURAL_CLASSES.md"))
    classes = []
    for ln in text.split("\n"):
        m = re.match(r"^\| (\d+)\. ([^|]+?) \| ([^|]*) \|", ln)
        if m:
            nums = []
            for b in re.findall(r"\b(\d{3})\b", m.group(3)):
                if b not in nums:
                    nums.append(b)
            classes.append((int(m.group(1)), m.group(2).strip(), nums))
    if not classes or [c[0] for c in classes] != list(range(1, len(classes) + 1)):
        die("STRUCTURAL_CLASSES.md's summary table (| N. class | bugs | owner |) is missing or not numbered 1..N")
    return classes


def load_exits(root, road):
    exits = []
    in_table = False
    for ln in road.split("\n"):
        if ln.startswith("| exit | owner | closes when |"):
            in_table = True
            continue
        if in_table:
            if not ln.startswith("|"):
                break
            cells = [c.strip() for c in ln.strip().strip("|").split("|")]
            if not re.fullmatch(r"\d[a-z]", cells[0]):
                continue
            nums = set()
            for a, b in re.findall(r"\b(\d{3})\s*[–-]\s*(\d{3})\b", cells[2]):
                nums.update(f"{i:03d}" for i in range(int(a), int(b) + 1))
            nums.update(re.findall(r"\b(\d{3})\b", cells[2]))
            exits.append((cells[0], nums))
    if not exits:
        die("ROADMAP.md has no Phase 1 exits table (| exit | owner | closes when | ...)")
    return exits


def joined(bs):
    return ", ".join(sorted(bs)) if bs else "—"


def render(recs, disc, classes, exits):
    silent = {b for r in disc for b in r["silent"]}
    attributed = {b for r in disc for b in r["bugs"]}
    open_bugs = sorted(b for b, o in recs.items() if o)

    def kind(b):
        return "silent" if b in silent else ("loud" if b in attributed else "unrecorded")

    n_sil = sum(1 for b in open_bugs if kind(b) == "silent")
    n_loud = sum(1 for b in open_bugs if kind(b) == "loud")
    n_unrec = len(open_bugs) - n_sil - n_loud
    out = [BEGIN, ""]
    head = (f"**Open bugs: {len(open_bugs)}** ({n_sil} silent, {n_loud} loud"
            + (f", {n_unrec} filed before the discovery record" if n_unrec else "")
            + f") of {len(recs)} bug records.")
    out.append(head)
    out.append("")
    out.append("| open bugs by primary structural class | open | silent | bugs |")
    out.append("|---|---|---|---|")
    placed = set()
    for num, name, nums in classes:
        mine = [b for b in nums if b in open_bugs and b not in placed]
        if not mine:
            continue
        placed.update(mine)
        out.append(f"| {num}. {name} | {len(mine)} | {sum(1 for b in mine if kind(b) == 'silent')} | {joined(mine)} |")
    rest = [b for b in open_bugs if b not in placed]
    if rest:
        out.append(f"| no class yet | {len(rest)} | {sum(1 for b in rest if kind(b) == 'silent')} | {joined(rest)} |")
    out.append("")
    out.append("| open bugs by Phase 1 exit | open | silent | bugs |")
    out.append("|---|---|---|---|")
    in_exit = set()
    for eid, nums in exits:
        mine = [b for b in open_bugs if b in nums]
        if not mine:
            continue
        in_exit.update(mine)
        out.append(f"| exit {eid} | {len(mine)} | {sum(1 for b in mine if kind(b) == 'silent')} | {joined(mine)} |")
    rest = [b for b in open_bugs if b not in in_exit]
    if rest:
        out.append(f"| no exit names it | {len(rest)} | {sum(1 for b in rest if kind(b) == 'silent')} | {joined(rest)} |")
    out.append("")
    counting = [r for r in disc if r["kind"] in COUNTING]
    quiet = lambda r: not r["silent"] and len(r["bugs"]) - len(r["silent"]) <= 1  # noqa: E731
    streak = 0
    for r in reversed(counting):
        if not quiet(r):
            break
        streak += 1
    out.append("| discovery rate: counting probe batch | date | effort | found | silent | loud |")
    out.append("|---|---|---|---|---|---|")
    for r in counting[-RECENT:]:
        nb, ns = len(r["bugs"]), len(r["silent"])
        out.append(f"| {r['kind']}: {r['probe']} | {r['date']} | {r['effort']} | {nb} | {ns} | {nb - ns} |")
    out.append("")
    other = [r for r in disc if r["kind"] not in COUNTING]
    out.append(f"Discovery signal (exit 2m): the last {streak} of {len(counting)} counting batches were quiet "
               f"(no silent bug, at most one loud); the exit needs {QUIET_RUN_NEEDED} in a row. "
               f"{len(other)} attribution-only batches (reviews, follow-ons, CI, controls, development) "
               f"found {sum(len(r['bugs']) for r in other)} further bugs.")
    out.append("")
    out.append(END)
    return "\n".join(out)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--root", default=os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")))
    g = ap.add_mutually_exclusive_group()
    g.add_argument("--write", action="store_true")
    g.add_argument("--check", action="store_true")
    a = ap.parse_args()
    roadmap = os.path.join(a.root, "ROADMAP.md")
    road = read(roadmap)
    recs = load_records(a.root)
    table = render(recs, load_discovery(a.root, recs), load_classes(a.root), load_exits(a.root, road))
    if not (a.write or a.check):
        print(table)
        return 0
    if road.count(BEGIN) != 1 or road.count(END) != 1 or road.index(BEGIN) > road.index(END):
        die(f"{roadmap} must hold exactly one BEGIN/END generated bug-dashboard marker pair")
    i, j = road.index(BEGIN), road.index(END) + len(END)
    if a.check:
        if road[i:j] != table:
            print(f"bug dashboard: {roadmap}'s dashboard differs from the bug records; edit the records and run "
                  "scripts/gen/bug_dashboard.py --write (the dashboard is generated, never hand-edited)",
                  file=sys.stderr)
            return 1
        print("bug dashboard: the roadmap dashboard matches the records")
        return 0
    with open(roadmap, "w", encoding="utf-8") as fh:
        fh.write(road[:i] + table + road[j:])
    return 0


if __name__ == "__main__":
    sys.exit(main())
