#!/usr/bin/env python3
"""One-time migration of the shared ledgers to one owner per fact (run by scripts/gen/migrate_ledgers.sh).

Reads the hand-maintained views as they are on the tree it runs on, so it can be re-run on a newer main:

  scripts/tests/audit_bug_corpus.sh   BUG_TEST_MAP / SKIP_BUGS     -> each record's fixtures:/coverage:
  docs/bugs/README.md                 Fixed / Open list lines      -> each record's summary:
  docs/bugs/STRUCTURAL_CLASSES.md     summary-table bugs cells     -> each record's class: lines
  ROADMAP.md                          exit 0b's bug list           -> each record's exit: 0b
  docs/bugs/discovery.tsv (if any)    silent column                -> each record's silent:

then writes a ledger block into every record, empties the legacy maps, puts generated-block markers
around the views (scripts/gen/bug_ledger.py fills them), rewrites README's "Filing a bug" section,
stamps CHANGELOG.md (scripts/gen/changelog.py) and writes scripts/tests/ci_gate_inventory.txt from
the workflow (after checking it against the EXPECTED_GATE_COMMANDS pin it replaces).

It refuses a tree that is already migrated. Every fact it cannot place (a bug number named by a view
with no record, a list line it cannot parse) is printed as NOTE so the run's log is the review list.
"""
import glob
import os
import re
import subprocess
import sys

ROOT = os.path.abspath(sys.argv[1] if len(sys.argv) > 1 else os.path.join(os.path.dirname(__file__), "..", ".."))
os.chdir(ROOT)
sys.path.insert(0, os.path.join(ROOT, "scripts", "gen"))
import bug_ledger as L  # noqa: E402

NOTES = []

# The new "Filing a bug" section of docs/bugs/README.md. The discovery paragraph is included only when
# docs/bugs/discovery.tsv exists on the tree being migrated.
FILING_TEMPLATE = """## Filing a bug

**One owner per fact.** To file a bug, write its record; to log a change, add a `changelog.d/`
fragment ([changelog.d/README.md](../../changelog.d/README.md)); never edit a generated view. Each
fact about a bug lives in its record and nowhere else, so two branches filing bugs edit different
files and cannot conflict.

Start with `scripts/gen/new_bug.sh "<title>" [--class N] [--silent yes|no]`. It takes the next number
no branch, worktree or reservation has used and reserves it on origin (`refs/bug-reservations/NNN`),
so two writers never get one number; it writes the record, a reproducer skeleton under
`tests/regressions/` and a `changelog.d/` fragment, regenerates the views and lists every
`FILL-IN(new_bug.sh)` placeholder left to fill in. `check_bug_filing_complete.sh` refuses a
placeholder, so a draft can be backed up on `wip/*` but cannot land.

A bug record is `docs/bugs/NNN_short_name.md`. Every time a bug is found,
ask whether an underlying problem in the compiler's design produced it, and record the answer: a
fix that repairs one spelling of a recurring cause leaves the next spelling open. The record has
these parts:

```
# Bug NNN — one-line statement of the defect

<!-- bug-ledger
summary: the one line shown in the bug list below
fixtures: regress_NNN_shape.con error_NNN_shape.con    (paths relative to tests/programs/)
coverage: check_x.sh leg y covers it                   (instead of fixtures: the gate, never the status)
class: 8 — optional note shown in STRUCTURAL_CLASSES.md's summary table
silent: yes                                            (silent wrong code or unsound claim; no = loud)
exit: 0b                                               (ROADMAP exits whose bug list names it)
-->

**Status:** OPEN | FIXED (branch, commit)
**Found:** date, by what (workload, hunt, review, example)
**Severity:** what goes wrong for a user (wrong code, unsound claim, crash, refusal)

## Reproducer        the smallest program or command, its observed and expected result
## Cause             the code that produces it (file, function)
## Fix               (when fixed) what changed, and the permanent regression leg
## Structural cause  the design fact that let this bug exist: which missing model, duplicated
                     authority, name-based join or fail-open default it is an instance of, and
                     the other bugs of that class
## Structural owner  the roadmap task, exit or verified-pipeline stage that removes the cause
                     for the whole class (not only this spelling), or a proposed owner
```

The record owns the bug's facts:

- **status**: the first `**Status:**` line. Changing it is the whole of recording a fix.
- **the ledger block** (it renders as nothing): `summary` is required, and exactly one of
  `fixtures` (regression fixtures, each registered in `scripts/tests/run_tests.sh`) or `coverage`
  (a rationale naming the gate or fixture that covers it). `class` (one line per structural class;
  the primary class is the first in the summary table's order), `silent` and `exit` are optional.
  Neither `summary` nor `coverage` states the status. The format is in `scripts/gen/bug_ledger.py`.

These views are generated from the records by `python3 scripts/gen/bug_ledger.py --write`; never
edit them:

- the Fixed and Open lists below;
- the bugs column of [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)'s summary table (the class
  name and owner cells are the class's own and are edited there);
- each exit's generated bug list in [ROADMAP.md](../../ROADMAP.md);
- the coverage maps of `scripts/tests/audit_bug_corpus.sh`, built at run time and never committed.

`check_docs_drift.sh` fails when a view differs from the records (a hand edit, or a record changed
without regenerating), and `audit_bug_corpus.sh` and `check_bug_filing_complete.sh` fail on a
record with no coverage or a fixture that does not exist.

<!-- discovery -->
**After a rebase**, a generated view may conflict, because both sides regenerated it. Do not merge
it by hand: run `bash scripts/gen/refresh_generated.sh`, which regenerates every view from the
merged records (replacing the conflicted block whole) and the build identity, then `git add` the
files it names and continue. A conflict in a record, or in prose outside a generated block, is a
real conflict and is resolved by hand.

`Structural cause` and `Structural owner` are required for every bug numbered 102 or higher
(`scripts/tests/check_bug_filing_complete.sh` fails without them). The classes found so far, with
their owners, are in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md); a new bug either joins a class
there (a `class:` line) or adds one (a new table row and section). Whether a new bug is repaired now
or contained until its owner lands follows the roadmap's defect disposition rule
([execution queue](../../ROADMAP.md#the-current-execution-queue)).
"""

FILING_DISCOVERY = """Also record the bug's number in the [discovery record](discovery.tsv), in the row of the probe
batch that found it (adding that batch if it is new), and in that row's `silent` column when it is
silent; the record's `silent:` line must agree. Then regenerate the roadmap's
[bug dashboard](../../ROADMAP.md#bug-dashboard) (`python3 scripts/gen/bug_dashboard.py --write`;
`refresh_generated.sh` runs it too). A probe batch that finds nothing is recorded too: its zero is
the discovery-rate signal of Phase 1 exit 2m.

"""


def note(m):
    NOTES.append(m)
    print(f"NOTE {m}")


def die(m):
    print(f"migrate_ledgers: {m}", file=sys.stderr)
    sys.exit(2)


def rd(p):
    return open(p, encoding="utf-8").read()


def wr(p, s):
    open(p, "w", encoding="utf-8").write(s)


STATUS_WORDS = re.compile(r"\b(FIXED|OPEN|CONTAINED|HALF CLOSED|PARTIALLY FIXED|PARTIAL)\b")


def unstatus(v, n, what, is_open):
    """Lower-case any status word left inside a migrated summary or coverage line (the **Status:** line is
    the only status; history such as 'fixed 2026-08-02' stays as prose). A word that contradicts the
    record's status is reported, since prose cannot be rewritten mechanically."""
    def contradicts(w):
        return (w == "OPEN" and not is_open) or (w == "FIXED" and is_open)
    # A contradicting word inside a parenthesis drops that parenthesis (a stale status note, e.g.
    # "(OPEN, partially repaired ...)" on a fixed bug); one in running text stops the migration.
    while True:
        m = next((m for m in STATUS_WORDS.finditer(v) if contradicts(m.group(1))), None)
        if not m:
            break
        a, b = v.rfind("(", 0, m.start()), v.find(")", m.end())
        if a < 0 or b < 0 or ")" in v[a:m.start()] or "(" in v[m.end():b]:
            die(f"bug {n}: its {what} says {m.group(1)} in running text but its **Status:** line says "
                f"{'open' if is_open else 'fixed'}; fix the wording on main first: {v[max(0, m.start() - 40):m.end() + 40]!r}")
        note(f"bug {n}: dropped a stale status note from its {what}: {v[a:b + 1]!r}")
        v = (v[:a].rstrip() + v[b + 1:]).strip()
    return STATUS_WORDS.sub(lambda m: m.group(1).lower(), v)


def oneline(s):
    s = re.sub(r"\s+", " ", s).strip()
    return s.replace("-->", "-- >")


# ---------------------------------------------------------------------------------------- records
records = {}
for p in sorted(glob.glob("docs/bugs/[0-9][0-9][0-9]_*.md")):
    num = os.path.basename(p)[:3]
    if num in records:
        die(f"bug {num} has two records")
    text = rd(p)
    if "<!-- bug-ledger" in text:
        die(f"{p} already carries a ledger block: this tree is migrated (or half-migrated); refusing")
    records[num] = {"path": p, "text": text, "class": [], "exit": []}

# ------------------------------------------------------------------------------- the coverage maps
AUDIT = "scripts/tests/audit_bug_corpus.sh"
audit = rd(AUDIT)


def bash_unquote(v):
    return re.sub(r'\\(["\\$`])', r"\1", v)


def legacy_map(name):
    m = re.search(r"^declare -A " + name + r"=\(\n(.*?)^\)\n", audit, re.M | re.S)
    if not m:
        die(f"{AUDIT}: no 'declare -A {name}=(' ... ')' block")
    out = {}
    for ln in m.group(1).split("\n"):
        if not ln.strip() or ln.strip().startswith("#"):
            continue
        e = re.match(r'^\s*\[(\d{3})\]="(.*)"\s*$', ln)
        if not e:
            die(f"{AUDIT}: cannot read the {name} line: {ln[:80]}")
        if e.group(1) in out:
            die(f"{AUDIT}: {name} assigns {e.group(1)} twice; resolve it before migrating")
        out[e.group(1)] = bash_unquote(e.group(2))
    return m, out


tm_m, test_map = legacy_map("BUG_TEST_MAP")
sk_m, skip_map = legacy_map("SKIP_BUGS")
for n in set(test_map) & set(skip_map):
    die(f"bug {n} is in both BUG_TEST_MAP and SKIP_BUGS; resolve it before migrating")
for n, v in test_map.items():
    if n in records:
        records[n]["fixtures"] = oneline(v)
    else:
        note(f"BUG_TEST_MAP names {n}, which has no record; dropped")
STATUS_PREFIX = re.compile(r"^(?:FIXED|OPEN|HALF CLOSED|PARTIAL(?:LY)?(?: FIXED)?|CONTAINED)\b(.*?)\s+(?:--|—)\s+(.+)$", re.S | re.I)


def strip_status(v):
    """'FIXED 2026-10-20 (batch 2) -- rest' -> 'rest (2026-10-20 (batch 2))': the record's Status line owns
    the status, and the qualifier after the word (dates, sites, task ids) is kept, not dropped."""
    m = STATUS_PREFIX.match(v)
    if not m:
        if re.match(r"^(FIXED|OPEN|HALF CLOSED|PARTIAL|CONTAINED)\b", v):
            note(f"a coverage rationale starts with a status word but has no ' -- ' after it: {v[:60]}; "
                 "kept with the word lower-cased")
            return v[0] + v[1:].split(" ", 1)[0].lower() + (" " + v.split(" ", 1)[1] if " " in v else "")
        return v
    qual, rest = m.group(1).strip(), m.group(2).strip()
    return f"{rest} ({qual})" if qual else rest


for n, v in skip_map.items():
    if n in records:
        records[n]["coverage"] = oneline(strip_status(v))
    else:
        note(f"SKIP_BUGS names {n}, which has no record; dropped")
for n, r in records.items():
    if "fixtures" not in r and "coverage" not in r:
        die(f"bug {n} has no coverage entry in {AUDIT}; file it before migrating (check_bug_filing_complete.sh)")

SUMMARY_STATUS = re.compile(r"^(?:fixed|open|contained|half closed|partially fixed|partial)\b([^:]*):\s+(.+)$", re.S | re.I)


def strip_summary_status(v):
    """'FIXED (with 126): rest' -> 'rest (with 126)': the generated list files a bug under Fixed or Open
    from its Status line, so the summary does not repeat it (and cannot go stale when the status moves)."""
    m = SUMMARY_STATUS.match(v)
    if not m:
        if re.match(r"^(FIXED|OPEN)\b", v):
            note(f"a README summary starts with a status word and no colon: {v[:60]}; the word is dropped "
                 "(the rest of the line is kept)")
            return v.split(" ", 1)[1] if " " in v else v
        return v
    qual, rest = m.group(1).strip(), m.group(2).strip()
    return f"{rest} ({qual})" if qual else rest


# ------------------------------------------------------------------------------ the README lists
README = "docs/bugs/README.md"
readme = rd(README)
fx = re.search(r"^## Fixed Bugs\n", readme, re.M)
op = re.search(r"^## Open Numbered Bugs\n", readme, re.M)
nx = re.search(r"^## Still Open, But Not Numbered Bugs\n", readme, re.M)
if not (fx and op and nx and fx.start() < op.start() < nx.start()):
    die(f"{README}: expected '## Fixed Bugs', '## Open Numbered Bugs', '## Still Open, But Not Numbered Bugs' in that order")
kept_prose = []
for section, start, end in (("fixed", fx.end(), op.start()), ("open", op.end(), nx.start())):
    for ln in readme[start:end].split("\n"):
        if not ln.strip():
            continue
        m = re.match(r"^- \[(\d{3})_[^\]]*\]\([^)]*\)(?:\s+—\s+(.*))?$", ln)
        if not m:
            kept_prose.append(ln)
            continue
        n, summ = m.group(1), m.group(2)
        if n not in records:
            note(f"README lists {n}, which has no record; dropped")
            continue
        if not summ:
            continue
        r = records[n]
        want = "open" if L.status_open(r["text"], r["path"]) else "fixed"
        # a bug listed twice keeps the line from the section its status puts it in; the other line is
        # reported, since it may carry provenance the kept one lacks
        if "summary" in r and r["summary"] != oneline(strip_summary_status(summ)):
            note(f"bug {n} is listed twice in the README with different text; kept the {want} section's line. "
                 f"Review the other: {summ[:100]!r}")
        if "summary" not in r or section == want:
            r["summary"] = oneline(strip_summary_status(summ))
for n, r in records.items():
    if "summary" not in r:
        t = L.title_of(r["text"], n)
        if not t:
            die(f"{r['path']}: no README line and no '# Bug {n}: title' line to take a summary from")
        r["summary"] = oneline(t)
        note(f"bug {n} had no README summary; using its title")

# ------------------------------------------------------------------- the structural-class table
SC = "docs/bugs/STRUCTURAL_CLASSES.md"
sc = rd(SC)
tbl = re.search(r"^\| class \| bugs \| structural owner \|\n\|---\|---\|---\|\n((?:\|.*\|\n)+)", sc, re.M)
if not tbl:
    die(f"{SC}: no summary table '| class | bugs | structural owner |'")


def top_split(s, sep=","):
    out, depth, cur = [], 0, ""
    i = 0
    while i < len(s):
        c = s[i]
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
        if depth == 0 and s.startswith(sep, i):
            out.append(cur)
            cur = ""
            i += len(sep)
            continue
        cur += c
        i += 1
    out.append(cur)
    return [x.strip() for x in out if x.strip()]


def class_items(cell):
    """(bug, note) pairs from one bugs cell: 'NNN', 'NNN (note)', 'NNN – MMM', '124 (and 068's half)'."""
    out = []
    for item in top_split(cell):
        m = re.match(r"^(\d{3})(?:\s*[–-]\s*(\d{3}))?(?:\s+\((.*)\))?$", item, re.S)
        if not m:
            note(f"{SC}: cannot read the class item '{item}'; place it by hand")
            continue
        a, b, nt = m.groups()
        nt = (nt or "").strip()
        sub = re.match(r"^and (\d{3})'s (.+)$", nt)
        if sub:  # "124 (and 068's binder half)": the parenthesis names another member
            out.append((a, ""))
            out.append((sub.group(1), sub.group(2)))
            continue
        for k in range(int(a), int(b or a) + 1):
            out.append((f"{k:03d}", nt))
    return out


rows = []
for ln in tbl.group(1).rstrip("\n").split("\n"):
    m = L.CLASS_ROW.match(ln)
    if not m:
        die(f"{SC}: cannot read the table row: {ln[:80]}")
    cnum, name, cell, owner = m.groups()
    rows.append((cnum, name, owner))
    for b, nt in class_items(cell):
        if b not in records:
            note(f"class {cnum} names {b}, which has no record; it is no longer in the generated row (the class prose keeps it)")
            continue
        if any(c == cnum for c, _ in records[b]["class"]):
            continue
        records[b]["class"].append((cnum, oneline(nt)))

# ------------------------------------------------------------------------- the ROADMAP exit list
RM = "ROADMAP.md"
road = rd(RM)
row0b = re.search(r"^\| 0b \| ([^|]*) \| (.*)$", road, re.M)
if not row0b:
    die(f"{RM}: no '| 0b |' exit row")
cell_start = row0b.start(2)
closes = row0b.group(2)
lead = re.match(r"^the wrong-code and memory/capability-safety repairs land: ", closes)
if not lead:
    die(f"{RM}: exit 0b's 'closes when' cell no longer starts 'the wrong-code and memory/capability-safety repairs land: '")
lst = re.match(r"((?:\d{3}(?:\s*[–-]\s*\d{3})?)(?:,\s*(?:\d{3}(?:\s*[–-]\s*\d{3})?))*)", closes[lead.end():])
if not lst:
    die(f"{RM}: exit 0b names no bug list")
norecord = []
for item in [x.strip() for x in lst.group(1).split(",")]:
    m = re.match(r"^(\d{3})(?:\s*[–-]\s*(\d{3}))?$", item)
    for k in range(int(m.group(1)), int(m.group(2) or m.group(1)) + 1):
        b = f"{k:03d}"
        if b in records:
            records[b]["exit"].append("0b")
        else:
            norecord.append(b)
            note(f"exit 0b names {b}, which has no record; it stays in the cell as text")
span = L.EXIT_BEGIN.format(x="0b") + "GEN" + L.EXIT_END.format(x="0b")
if norecord:
    span += ", " + ", ".join(f"{b} (no record yet)" for b in norecord)
a = cell_start + lead.end()
road = road[:a] + span + road[a + lst.end():]

# -------------------------------------------------------------------------- discovery (optional)
DISC = "docs/bugs/discovery.tsv"
if os.path.exists(DISC):
    drows = [ln.split("\t") for ln in rd(DISC).split("\n") if ln and not ln.startswith("#")]
    hdr = drows[0]
    bi, si = hdr.index("bugs"), hdr.index("silent")
    for cells in drows[1:]:
        sil = set(cells[si].split(","))
        for b in cells[bi].split(","):
            if b in records:
                records[b]["silent"] = "yes" if b in sil else "no"
            elif re.fullmatch(r"\d{3}", b):
                note(f"{DISC} names {b}, which has no record")

# ------------------------------------------------------------------------------ write the records
for n, r in records.items():
    is_open = L.status_open(r["text"], r["path"])
    r["summary"] = unstatus(r["summary"], n, "summary", is_open)
    if "coverage" in r:
        r["coverage"] = unstatus(r["coverage"], n, "coverage", is_open)
    # A line that still OPENS with a status word ("fixed by R-0003") states a status, not the defect: the
    # summary becomes the record's title with that history kept in parentheses, a coverage line keeps
    # its text behind a neutral lead.
    if L.STATUS_LEAD.match(r["summary"]):
        t = L.title_of(r["text"], n) or f"bug {n}"
        note(f"bug {n}: its summary was only a status ({r['summary'][:50]!r}); it is now the title, with that history in parentheses")
        r["summary"] = oneline(f"{t} ({r['summary']})")
    if "coverage" in r and L.STATUS_LEAD.match(r["coverage"]):
        note(f"bug {n}: its coverage line opened with a status word; prefixed 'coverage notes:': {r['coverage'][:50]!r}")
        r["coverage"] = "coverage notes: " + r["coverage"]
    lines = ["<!-- bug-ledger", f"summary: {r['summary']}"]
    if "fixtures" in r:
        lines.append(f"fixtures: {r['fixtures']}")
    else:
        lines.append(f"coverage: {r['coverage']}")
    for c, nt in r["class"]:
        lines.append(f"class: {c} — {nt}" if nt else f"class: {c}")
    if "silent" in r:
        lines.append(f"silent: {r['silent']}")
    if r["exit"]:
        lines.append("exit: " + " ".join(r["exit"]))
    lines.append("-->")
    block = "\n".join(lines) + "\n"
    text = r["text"]
    t = re.search(r"^# .*\n", text, re.M)
    if t:
        text = text[:t.end()] + "\n" + block + text[t.end():]
    else:
        text = block + "\n" + text
    wr(r["path"], text)

# --------------------------------------------------------------------------- empty the legacy maps
EMPTY = {
    "BUG_TEST_MAP": "declare -A BUG_TEST_MAP=(\n  # Empty since the ledger migration: a bug's fixtures are the `fixtures:` line of its record's\n"
                    "  # ledger block (docs/bugs/NNN_*.md). Do not add entries here.\n)\n",
    "SKIP_BUGS": "declare -A SKIP_BUGS=(\n  # Empty since the ledger migration: a bug without a fixture names its coverage in the\n"
                 "  # `coverage:` line of its record's ledger block. Do not add entries here.\n)\n",
}
audit = audit[:sk_m.start()] + EMPTY["SKIP_BUGS"] + audit[sk_m.end():]
audit = audit[:tm_m.start()] + EMPTY["BUG_TEST_MAP"] + audit[tm_m.end():]
wr(AUDIT, audit)

# ------------------------------------------------------------------------------- the README views
FILING = FILING_TEMPLATE.replace("<!-- discovery -->\n", FILING_DISCOVERY if os.path.exists(DISC) else "")
fa = re.search(r"^## Filing a bug\n", readme, re.M)
if not fa or fa.start() > fx.start():
    die(f"{README}: no '## Filing a bug' section before '## Fixed Bugs'")
tail = ""
if kept_prose:
    tail = "\n" + "\n".join(kept_prose) + "\n"
readme = (readme[:fa.start()] + FILING.rstrip("\n") + "\n\n" + L.README_BEGIN + "\n" + L.README_END + "\n"
          + tail + "\n" + readme[nx.start():])
wr(README, readme)

sc = sc[:tbl.start()] + L.CLASS_BEGIN + "\n\n" + tbl.group(0) + "\n" + L.CLASS_END + "\n" + sc[tbl.end():]
wr(SC, sc)
wr(RM, road)

# ---------------------------------------------------------------------- the gate-count inventory
RUNNER = "scripts/tests/run_ci_gates_local.sh"
runner = rd(RUNNER)
pin = re.search(r"^EXPECTED_GATE_COMMANDS=(\d+)\n", runner, re.M)
if not pin:
    die(f"{RUNNER}: no EXPECTED_GATE_COMMANDS pin to replace")
fn = re.search(r"^_extract_cmds\(\) \{.*?^\}\n", runner, re.M | re.S)
wf = re.search(r'^WORKFLOW="([^"]+)"', runner, re.M).group(1)
cmds = subprocess.run(["bash", "-c", fn.group(0) + '\n_extract_cmds "$1"', "x", wf],
                      capture_output=True, text=True, check=True).stdout
cmds = [c for c in cmds.split("\n") if c]
if len(cmds) != int(pin.group(1)):
    die(f"the workflow yields {len(cmds)} gate commands but the pin says {pin.group(1)}; fix the pin first")
wr("scripts/tests/ci_gate_inventory.txt", "\n".join(sorted(set(cmds), key=lambda c: c.encode())) + "\n")
hist = re.search(r"^# 213 -> 224:.*?^EXPECTED_GATE_COMMANDS=\d+\n", runner, re.M | re.S)
if not hist:
    die(f"{RUNNER}: cannot find the pin's history comment ('# 213 -> 224:' ... the pin)")
runner = (runner[:hist.start()]
          + "# The count history (213 -> " + pin.group(1) + ") is in git history; since the ledger migration the\n"
          "# pin is the inventory file below, and a gate's line is added in the commit that adds the gate.\n"
          "EXPECTED_GATE_COMMANDS=0\n" + runner[hist.end():])
wr(RUNNER, runner)

print(f"migrate_ledgers: {len(records)} records migrated; {len(NOTES)} notes")
