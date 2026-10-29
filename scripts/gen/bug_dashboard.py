#!/usr/bin/env python3
"""Generate ROADMAP.md's bug dashboard from the bug records.

  bug_dashboard.py            print the generated dashboard
  bug_dashboard.py --write    replace the dashboard between the markers in ROADMAP.md
  bug_dashboard.py --check    exit 1 when ROADMAP.md's dashboard differs from the records (stale or
                              hand-edited); check_docs_drift.sh runs this

Inputs, each the single owner of its fact:
  docs/bugs/NNN_*.md                 status: the first word of the record's first **Status:** (or
                                     legacy `Status:`) line: OPEN/Open, CONTAINED, Half (closed) and
                                     Partially (fixed) are open; FIXED/Fixed/fixed is closed; anything
                                     else, or no status line, is refused
  docs/bugs/STRUCTURAL_CLASSES.md    the summary table's classes; a bug's primary class is the first
                                     class row, in table order, that lists it
  docs/bugs/discovery.tsv            which probe batch found each bug from 079 on, and which are silent
  the records' `ruling:` lines       each open bug's spec ruling (scripts/gen/bug_rulings.py; read for
                                     their form only: bug_ledger.py resolves them against docs/spec)
  ROADMAP.md "Phase 1 exits"         which exits name each bug in their completion condition

The discovery-rate signal (Phase 1 exit 2m) is kept PER STRUCTURAL CLASS (a bug's primary class, as
above; bugs in no class form one more bucket, "no class yet"). For a class K, a COUNTING batch (an
independent probe kind) is quiet when it found no silent bug of class K and at most one loud bug of
class K. A quiet batch moves K's streak only when its recorded effort reaches the minimum (MIN_HOURS
hours, or MIN_UNITS probe programs, examples or cases); a smaller one is a token batch, skipped. A
batch that found more is never skipped: it ends K's streak. A batch whose probe cell starts with
"class N: " probed a structural migration's own new code: it is a batch of class N only, and every
bug it found counts against class N; for any other class it is skipped, except that a silent bug it
found still ends the streak of that bug's own primary class. Exit 2m holds when every class (and the
"no class yet" bucket) has QUIET_RUN_NEEDED consecutive eligible quiet batches. A counting batch
dated EFFORT_REQUIRED_FROM or later must record its effort in those units.

FAIL-CLOSED (exit 2, nothing generated): a malformed discovery record (wrong header, wrong column
count, an empty cell, a bad date or kind, a malformed bug list, a silent bug the row does not list,
dates out of order), a bug from 079 on with a record but no batch, a bug in two batches or in a batch
with no record, a class tag on an attribution-only batch or naming no class, a structural-class
table that cannot be read, or a missing Phase 1 exits table. Missing markers, or more than one pair,
also refuse.

--root overrides the repository root (a test seam for the gate's negative controls).
"""
import argparse
import glob
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import bug_rulings  # noqa: E402  (beside this file; -I leaves the script's directory off sys.path)

BEGIN = "<!-- BEGIN GENERATED: bug dashboard (scripts/gen/bug_dashboard.py; edit docs/bugs records and docs/bugs/discovery.tsv) -->"
END = "<!-- END GENERATED: bug dashboard -->"
CLASS_TAG = re.compile(r"^class (\d+): ")
COLS = ["date", "kind", "probe", "effort", "bugs", "silent"]
COUNTING = {"hunt", "examples", "workload", "design-proofs", "semantics", "spec", "fuzz", "interp-coverage"}
ATTRIBUTION = {"ci", "review", "follow-on", "controls", "development"}
FIRST_ATTRIBUTED = 79
MIN_HOURS = 4
MIN_UNITS = 10
EFFORT_REQUIRED_FROM = "2026-10-22"
EFFORT = re.compile(r"^(\d+(?:\.\d+)?) (hours?|programs?|probes?|examples?|cases?)\b")
OPEN_WORDS = {"open", "contained", "half", "partially"}
CLOSED_WORDS = {"fixed"}
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
        m = re.search(r"^(?:\*\*Status:\*\*|Status:)\s*(.*)$", read(p), re.M)
        if not m:
            die(f"{os.path.basename(p)} has no **Status:** line")
        word = (re.findall(r"[A-Za-z]+", m.group(1))[:1] or [""])[0].lower()
        if word in OPEN_WORDS:
            recs[num] = True
        elif word in CLOSED_WORDS:
            recs[num] = False
        else:
            die(f"{os.path.basename(p)}: status '{m.group(1)[:40]}' is not one of OPEN, CONTAINED, "
                "Half closed, Partially fixed or FIXED")
    if not recs:
        die("no bug records under docs/bugs")
    return recs


LEDGER_BLOCK = re.compile(r"^<!-- bug-ledger[ \t]*\n(.*?)^-->[ \t]*$", re.M | re.S)
RULING_LINE = re.compile(r"^ruling:[ \t]*(.*?)[ \t]*$", re.M)


def load_rulings(root, recs):
    """{bug: parsed ruling, or None when the record has none} for the open bugs."""
    out = {}
    for p in sorted(glob.glob(os.path.join(root, "docs", "bugs", "[0-9][0-9][0-9]_*.md"))):
        num = os.path.basename(p)[:3]
        if not recs.get(num):
            continue
        blk = LEDGER_BLOCK.search(read(p))
        m = RULING_LINE.search(blk.group(1)) if blk else None
        try:
            out[num] = bug_rulings.parse(m.group(1), os.path.basename(p)) if m else None
        except bug_rulings.RulingError as e:
            die(str(e))
    return out


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
        tm = CLASS_TAG.match(r["probe"])
        r["tag"] = int(tm.group(1)) if tm else None
        if r["tag"] is not None and r["kind"] not in COUNTING:
            die(f"{path} data row {n}: a 'class N: ' tag marks a counting batch, not a {r['kind']} row")
        if r["kind"] in COUNTING and r["date"] >= EFFORT_REQUIRED_FROM and not EFFORT.match(r["effort"]):
            die(f"{path} data row {n}: a counting batch from {EFFORT_REQUIRED_FROM} on records its effort as "
                f"'<N> hours|programs|probes|examples|cases', got '{r['effort']}'")
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
            exits.append((cells[0], bug_numbers(cells[2])))
    if not exits:
        die("ROADMAP.md has no Phase 1 exits table (| exit | owner | closes when | ...)")
    return exits


def bug_numbers(text):
    nums = set()
    for a, b in re.findall(r"\b(\d{3})\s*[–-]\s*(\d{3})\b", text):
        nums.update(f"{i:03d}" for i in range(int(a), int(b) + 1))
    nums.update(re.findall(r"\b(\d{3})\b", text))
    return nums


def joined(bs):
    return ", ".join(sorted(bs)) if bs else "—"


def render(recs, disc, classes, exits, rulings=None):
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
    if rulings is not None:
        groups = {}
        for b in open_bugs:
            r = rulings.get(b)
            groups.setdefault(bug_rulings.label(r) if r else "no ruling", []).append(b)
        order = {k: i for i, k in enumerate(bug_rulings.KINDS)}
        out.append("| open bugs by spec ruling ([RULINGS.md](docs/bugs/RULINGS.md)) | open | silent | bugs |")
        out.append("|---|---|---|---|")
        for lab in sorted(groups, key=lambda g: (order.get(rulings[groups[g][0]]["kind"], 99) if rulings.get(groups[g][0]) else 99, g)):
            mine = groups[lab]
            out.append(f"| {lab} | {len(mine)} | {sum(1 for b in mine if kind(b) == 'silent')} | {joined(mine)} |")
        out.append("")
    counting = [r for r in disc if r["kind"] in COUNTING]
    quiet = lambda r: not r["silent"] and len(r["bugs"]) - len(r["silent"]) <= 1  # noqa: E731

    def substantial(r):
        m = EFFORT.match(r["effort"])
        if not m:
            return False
        return float(m.group(1)) >= (MIN_HOURS if m.group(2).startswith("hour") else MIN_UNITS)

    def verdict(r):
        if not quiet(r):
            return "found bugs"
        return "quiet" if substantial(r) else "token (does not count)"

    primary = {}
    for num, _name, nums in classes:
        for b in nums:
            primary.setdefault(b, num)
    buckets = [(num, f"{num}. {name}") for num, name, _ in classes] + [(None, "no class yet")]

    def streak_of(k):
        """Consecutive eligible quiet counting batches, newest first, for primary class k."""
        n = 0
        for r in reversed(counting):
            if r["tag"] is not None and r["tag"] != k:
                if any(primary.get(b) == k for b in r["silent"]):
                    break  # a silent bug ends its own class's streak, whichever batch found it
                continue
            tagged_k = r["tag"] is not None and r["tag"] == k
            mine = r["bugs"] if tagged_k else [b for b in r["bugs"] if primary.get(b) == k]
            sil = [b for b in mine if b in r["silent"]]
            if sil or len(mine) - len(sil) > 1:
                break
            if substantial(r):
                n += 1
        return n

    streaks = [(label, streak_of(k)) for k, label in buckets]
    held = [label for label, n in streaks if n >= QUIET_RUN_NEEDED]
    tokens = sum(1 for r in counting if verdict(r).startswith("token"))
    out.append("| discovery rate: counting probe batch | date | effort | found | silent | loud | signal |")
    out.append("|---|---|---|---|---|---|---|")
    for r in counting[-RECENT:]:
        nb, ns = len(r["bugs"]), len(r["silent"])
        out.append(f"| {r['kind']}: {r['probe']} | {r['date']} | {r['effort']} | {nb} | {ns} | {nb - ns} | {verdict(r)} |")
    out.append("")
    out.append("| discovery rate by primary structural class | quiet counting batches in a row | holds |")
    out.append("|---|---|---|")
    for label, n in streaks:
        out.append(f"| {label} | {n} | {'yes' if n >= QUIET_RUN_NEEDED else 'no'} |")
    out.append("")
    other = [r for r in disc if r["kind"] not in COUNTING]
    tagged = sum(1 for r in counting if r["tag"] is not None)
    out.append(f"Discovery signal (exit 2m): {len(held)} of {len(buckets)} classes have {QUIET_RUN_NEEDED} consecutive "
               f"eligible quiet counting batches (for a class: no silent bug of that class, at most one loud, "
               f"effort at least {MIN_HOURS} hours or {MIN_UNITS} programs); the exit needs every class. "
               f"{len(counting)} counting batches recorded"
               + (f", {tokens} of them token batches" if tokens else "")
               + (f", {tagged} of them probing one class's migration" if tagged else "") + ". "
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
    disc = load_discovery(a.root, recs)
    classes = load_classes(a.root)
    known = {c[0] for c in classes}
    for r in disc:
        if r["tag"] is not None and r["tag"] not in known:
            die(f"discovery batch '{r['probe'][:60]}' is tagged class {r['tag']}, which STRUCTURAL_CLASSES.md does not have")
    table = render(recs, disc, classes, load_exits(a.root, road), load_rulings(a.root, recs))
    if not (a.write or a.check):
        print(table)
        return 0
    if road.count(BEGIN) != 1 or road.count(END) != 1 or road.index(BEGIN) > road.index(END):
        die(f"{roadmap} must hold exactly one BEGIN/END generated bug-dashboard marker pair")
    i, j = road.index(BEGIN), road.index(END) + len(END)
    new = road[:i] + table + road[j:]
    if a.check:
        if new != road:
            print(f"bug dashboard: {roadmap}'s dashboard differs from the bug records; edit the "
                  "records and run scripts/gen/bug_dashboard.py --write (the dashboard is generated, never hand-edited)",
                  file=sys.stderr)
            return 1
        print("bug dashboard: the roadmap dashboard matches the records")
        return 0
    with open(roadmap, "w", encoding="utf-8") as fh:
        fh.write(new)
    return 0


if __name__ == "__main__":
    sys.exit(main())
