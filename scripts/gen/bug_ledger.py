#!/usr/bin/env python3
"""Generate the bug ledger's shared views from the bug records, the single owner of each bug's facts.

  bug_ledger.py              print a summary of what would be generated
  bug_ledger.py --write      regenerate every view in place (also resolves a rebase conflict inside
                             a generated block: the block is replaced whole, conflict markers included)
  bug_ledger.py --check      exit 1 when a view differs from the records (stale or hand-edited);
                             check_docs_drift.sh runs this
  bug_ledger.py corpus       print the regression-coverage map, one tab-separated row per bug:
                             NNN<TAB>fixtures<TAB>a.con b.con   or   NNN<TAB>coverage<TAB>rationale;
                             audit_bug_corpus.sh and check_bug_filing_complete.sh read it (it is never
                             committed, so it cannot conflict)

ONE OWNER PER FACT. A bug's facts live in its record, docs/bugs/NNN_*.md, and nowhere else:

  status        the record's first `**Status:**` (or legacy `Status:`) line, as bug_dashboard.py reads
                it: OPEN/Open, CONTAINED, Half (closed), Partially (fixed) are open; FIXED/Fixed closed
  everything else, in one ledger block anywhere in the record (it renders as nothing on GitHub):

      <!-- bug-ledger
      summary: one line for the bug list (required)
      fixtures: regress_175_x.con dir/src/main.con   (paths under tests/programs/, or repo paths)
      coverage: the rationale naming the gate or fixture that covers it, when there is no fixture
      class: 8 — optional note shown beside the bug in STRUCTURAL_CLASSES.md's summary table
      class: 12                                       (one line per class; table order picks primary)
      silent: yes | no                                (silent wrong code / unsound claim vs loud)
      exit: 0b                                        (ROADMAP exits whose generated bug list names it)
      -->

  Exactly one of `fixtures` and `coverage` is required.

THE GENERATED VIEWS (each between BEGIN/END GENERATED markers; never edit inside them):

  docs/bugs/README.md              the "## Fixed Bugs" and "## Open Numbered Bugs" lists
  docs/bugs/STRUCTURAL_CLASSES.md  the summary table's bugs column (the class name and owner cells
                                   are owned by the class and kept as written)
  ROADMAP.md                       each `<!-- BEGIN GENERATED: exit X bugs ... -->` span: the bugs
                                   whose record names exit X, ranges compacted

BEFORE THE MIGRATION (scripts/gen/migrate_ledgers.sh) no record carries a ledger block: every mode
says so and exits 0, and `corpus` prints nothing, so the legacy maps in audit_bug_corpus.sh stay the
coverage source. Once any record carries a block, every record must (a mixed ledger exits 2).

FAIL-CLOSED (exit 2, nothing written): two records for one number, a malformed or duplicated block,
an unknown key, a missing summary, both or neither of fixtures/coverage, a class the summary table
does not have, an exit with no generated span, a bad silent value, missing or duplicated markers, a
structural row whose cells disagree between the two sides of a conflict, or a silent value that
disagrees with docs/bugs/discovery.tsv (when that file exists, its `silent` column must name exactly
the records marked `silent: yes` among the bugs it lists).

--root overrides the repository root (a test seam for the gate's negative controls).
"""
import argparse
import glob
import os
import re
import sys

KEYS = {"summary", "fixtures", "coverage", "class", "silent", "exit"}
REPEATABLE = {"class"}
OPEN_WORDS = {"open", "contained", "half", "partially"}
CLOSED_WORDS = {"fixed"}
BLOCK = re.compile(r"^<!-- bug-ledger[ \t]*\n(.*?)^-->[ \t]*$", re.M | re.S)

README_BEGIN = ("<!-- BEGIN GENERATED: bug list (scripts/gen/bug_ledger.py; edit the docs/bugs/NNN_*.md "
                "records, never this list) -->")
README_END = "<!-- END GENERATED: bug list -->"
CLASS_BEGIN = ("<!-- BEGIN GENERATED: class table bugs column (scripts/gen/bug_ledger.py; a bug joins a "
               "class in its record's ledger block; the class name and owner cells are edited here) -->")
CLASS_END = "<!-- END GENERATED: class table -->"
EXIT_SPAN = re.compile(r"<!-- BEGIN GENERATED: exit (\w+) bugs[^>]*-->(.*?)<!-- END GENERATED: exit \1 bugs -->", re.S)
EXIT_BEGIN = "<!-- BEGIN GENERATED: exit {x} bugs (scripts/gen/bug_ledger.py; edit the records' exit lines) -->"
EXIT_END = "<!-- END GENERATED: exit {x} bugs -->"
CONFLICT = re.compile(r"^(<{7}|={7}|>{7}|\|{7})( .*)?$")
CLASS_ROW = re.compile(r"^\| (\d+)\. ([^|]+?) \| ([^|]*?) \| (.*) \|$")


def die(msg):
    print(f"bug ledger: {msg}", file=sys.stderr)
    sys.exit(2)


def read(path):
    try:
        return open(path, encoding="utf-8").read()
    except OSError as e:
        die(f"cannot read {path}: {e}")


def status_open(text, name):
    m = re.search(r"^(?:\*\*Status:\*\*|Status:)\s*(.*)$", text, re.M)
    if not m:
        die(f"{name} has no **Status:** line")
    word = (re.findall(r"[A-Za-z]+", m.group(1))[:1] or [""])[0].lower()
    if word in OPEN_WORDS:
        return True
    if word in CLOSED_WORDS:
        return False
    die(f"{name}: status '{m.group(1)[:40]}' is not one of OPEN, CONTAINED, Half closed, Partially fixed or FIXED")


def title_of(text, num):
    m = re.search(r"^# Bug " + num + r"\s*[:—–-]+\s*(.+)$", text, re.M)
    return m.group(1).strip() if m else ""


def parse_block(body, name):
    facts = {"class": []}
    for ln in body.split("\n"):
        if not ln.strip():
            continue
        m = re.match(r"^([a-z]+):[ \t]*(.*?)[ \t]*$", ln)
        if not m:
            die(f"{name}: ledger line is not 'key: value': {ln[:60]}")
        k, v = m.groups()
        if k not in KEYS:
            die(f"{name}: unknown ledger key '{k}' (known: {', '.join(sorted(KEYS))})")
        if not v:
            die(f"{name}: ledger key '{k}' is empty")
        if k in REPEATABLE:
            cm = re.match(r"^(\d+)(?:\s+—\s+(.+))?$", v)
            if not cm:
                die(f"{name}: class must be 'N' or 'N — note', got '{v[:40]}'")
            if any(c == cm.group(1) for c, _ in facts["class"]):
                die(f"{name}: class {cm.group(1)} listed twice")
            facts["class"].append((cm.group(1), cm.group(2) or ""))
            continue
        if k in facts:
            die(f"{name}: ledger key '{k}' given twice")
        facts[k] = v
    if "summary" not in facts:
        die(f"{name}: the ledger block has no summary")
    if ("fixtures" in facts) == ("coverage" in facts):
        die(f"{name}: the ledger block needs exactly one of fixtures (a regression fixture) or coverage "
            "(the rationale naming the gate that covers it)")
    if facts.get("silent", "yes") not in ("yes", "no"):
        die(f"{name}: silent must be yes or no, got '{facts['silent']}'")
    exits = re.split(r"[,\s]+", facts.get("exit", "").strip()) if "exit" in facts else []
    if not all(re.fullmatch(r"\d[a-z]", x) for x in exits) or len(set(exits)) != len(exits):
        die(f"{name}: exit must be distinct exit ids like 0b or 2c, got '{facts.get('exit')}'")
    facts["exit"] = exits
    return facts


def load_records(root):
    recs = {}
    for p in sorted(glob.glob(os.path.join(root, "docs", "bugs", "[0-9][0-9][0-9]_*.md"))):
        base = os.path.basename(p)
        num = base[:3]
        if num in recs:
            die(f"bug {num} has two records ({recs[num]['file']} and {base})")
        text = read(p)
        blocks = BLOCK.findall(text)
        if text.count("<!-- bug-ledger") != len(blocks):
            die(f"{base}: a '<!-- bug-ledger' block is not closed by a '-->' line")
        if len(blocks) > 1:
            die(f"{base}: more than one ledger block")
        r = {"num": num, "file": base, "open": status_open(text, base), "title": title_of(text, num),
             "facts": parse_block(blocks[0], base) if blocks else None}
        recs[num] = r
    if not recs:
        die("no bug records under docs/bugs")
    with_block = [n for n, r in recs.items() if r["facts"]]
    if with_block and len(with_block) != len(recs):
        missing = sorted(n for n, r in recs.items() if not r["facts"])
        die(f"records without a ledger block: {', '.join(missing)} (once the ledger is migrated every "
            "record carries one; see docs/bugs/README.md, Filing a bug)")
    return recs, bool(with_block)


def check_discovery(root, recs):
    path = os.path.join(root, "docs", "bugs", "discovery.tsv")
    if not os.path.exists(path):
        return
    rows = [ln.split("\t") for ln in read(path).split("\n") if ln and not ln.startswith("#")]
    if not rows:
        return
    hdr = rows[0]
    if "bugs" not in hdr or "silent" not in hdr:
        die(f"{path}: no bugs/silent columns to compare the records' silent lines with")
    bi, si = hdr.index("bugs"), hdr.index("silent")
    for cells in rows[1:]:
        if len(cells) <= max(bi, si):
            continue
        bugs = [b for b in cells[bi].split(",") if re.fullmatch(r"\d{3}", b)]
        silent = {b for b in cells[si].split(",") if re.fullmatch(r"\d{3}", b)}
        for b in bugs:
            if b not in recs:
                continue
            mine = recs[b]["facts"].get("silent")
            theirs = "yes" if b in silent else "no"
            if mine != theirs:
                die(f"bug {b}: its record says silent: {mine or '(nothing)'} but {path} says {theirs}; "
                    "the record owns the fact, make the two agree")


def compact(nums):
    ns = sorted(int(n) for n in nums)
    out, i = [], 0
    while i < len(ns):
        j = i
        while j + 1 < len(ns) and ns[j + 1] == ns[j] + 1:
            j += 1
        if j - i >= 2:
            out.append(f"{ns[i]:03d} – {ns[j]:03d}")
            i = j + 1
        else:
            out.append(f"{ns[i]:03d}")
            i += 1
    return ", ".join(out)


def render_readme(recs):
    def line(r):
        return f"- [{r['file']}]({r['file']}) — {r['facts']['summary']}"
    fixed = [r for r in recs.values() if not r["open"]]
    opn = [r for r in recs.values() if r["open"]]
    out = [README_BEGIN, "", "## Fixed Bugs", ""]
    out += [line(r) for r in fixed]
    out += ["", "## Open Numbered Bugs", ""]
    out += [line(r) for r in opn]
    out += ["", README_END]
    return "\n".join(out)


def class_rows(block, path):
    """The (num, name, owner) rows of the class table, tolerant of a rebase conflict inside it."""
    rows = {}
    order = []
    for ln in block.split("\n"):
        if CONFLICT.match(ln):
            continue
        m = CLASS_ROW.match(ln)
        if not m:
            continue
        num, name, _, owner = m.groups()
        if num in rows:
            if rows[num] != (name, owner):
                die(f"{path}: class {num} appears twice with different name or owner cells (a conflict "
                    "outside the generated column); resolve it by hand, then rerun --write")
            continue
        rows[num] = (name, owner)
        order.append(num)
    if not order or order != [str(i) for i in range(1, len(order) + 1)]:
        die(f"{path}: the generated class table must hold rows | N. class | bugs | owner | numbered 1..N")
    return [(n, rows[n][0], rows[n][1]) for n in order]


def render_classes(recs, rows):
    members = {n: [] for n, _, _ in rows}
    for r in recs.values():
        for c, note in r["facts"]["class"]:
            if c not in members:
                die(f"{r['file']}: class {c} is not a row of STRUCTURAL_CLASSES.md's summary table")
            members[c].append((r["num"], note))
    out = [CLASS_BEGIN, "", "| class | bugs | structural owner |", "|---|---|---|"]
    for n, name, owner in rows:
        bugs = ", ".join(f"{b} ({note})" if note else b for b, note in sorted(members[n])) or "—"
        out.append(f"| {n}. {name} | {bugs} | {owner} |")
    out += ["", CLASS_END]
    return "\n".join(out)


def splice(text, begin, end, new, path):
    if text.count(begin) != 1 or text.count(end) != 1 or text.index(begin) > text.index(end):
        die(f"{path} must hold exactly one generated marker pair {begin[:60]}...")
    i, j = text.index(begin), text.index(end) + len(end)
    return text[:i] + new + text[j:], text[i:j]


HUNK = re.compile(r"^<{7}[^\n]*\n(.*?)(?:^\|{7}[^\n]*\n.*?)?^={7}\n(.*?)^>{7}[^\n]*\n", re.M | re.S)


def resolve_span_conflicts(text):
    """A rebase conflict whose two sides differ only inside generated exit spans (a ROADMAP table row,
    regenerated on both sides) collapses to one side; the spans are regenerated next. Any other hunk is
    left as it is, for a human."""
    blank = lambda t: EXIT_SPAN.sub(lambda m: m.group(0)[:m.start(2) - m.start(0)] + m.group(0)[m.end(2) - m.start(0):], t)  # noqa: E731
    return HUNK.sub(lambda m: m.group(1) if blank(m.group(1)) == blank(m.group(2)) else m.group(0), text)


def exit_text(recs, x):
    return compact(r["num"] for r in recs.values() if x in r["facts"]["exit"]) or "—"


def views(root, recs):
    """[(path, current text, regenerated text)] for every generated view."""
    out = []
    p = os.path.join(root, "docs", "bugs", "README.md")
    cur = read(p)
    new, _ = splice(cur, README_BEGIN, README_END, render_readme(recs), p)
    out.append((p, cur, new))
    p = os.path.join(root, "docs", "bugs", "STRUCTURAL_CLASSES.md")
    cur = read(p)
    _, old_block = splice(cur, CLASS_BEGIN, CLASS_END, "", p)
    new, _ = splice(cur, CLASS_BEGIN, CLASS_END, render_classes(recs, class_rows(old_block, p)), p)
    out.append((p, cur, new))
    p = os.path.join(root, "ROADMAP.md")
    cur = read(p)
    base = resolve_span_conflicts(cur)
    spans = [m.group(1) for m in EXIT_SPAN.finditer(base)]
    if len(set(spans)) != len(spans):
        die(f"{p}: an exit has two generated bug spans: {sorted(x for x in spans if spans.count(x) > 1)}")
    if base.count("<!-- BEGIN GENERATED: exit ") != len(spans):
        die(f"{p}: an exit bug span is missing its END marker")
    named = {x for r in recs.values() for x in r["facts"]["exit"]}
    if named - set(spans):
        die(f"records name exits with no generated bug span in ROADMAP.md: {', '.join(sorted(named - set(spans)))}")
    new = EXIT_SPAN.sub(lambda m: EXIT_BEGIN.format(x=m.group(1)) + exit_text(recs, m.group(1))
                        + EXIT_END.format(x=m.group(1)), base)
    out.append((p, cur, new))
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("mode", nargs="?", choices=["corpus"])
    ap.add_argument("--root", default=os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")))
    g = ap.add_mutually_exclusive_group()
    g.add_argument("--write", action="store_true")
    g.add_argument("--check", action="store_true")
    a = ap.parse_args()
    recs, migrated = load_records(a.root)
    if a.mode == "corpus":
        for r in recs.values() if migrated else []:
            f = r["facts"]
            kind = "fixtures" if "fixtures" in f else "coverage"
            print(f"{r['num']}\t{kind}\t{f[kind]}")
        return 0
    if not migrated:
        print("bug ledger: not migrated yet (no record carries a ledger block); the legacy views are "
              "hand-maintained until scripts/gen/migrate_ledgers.sh runs")
        return 0
    check_discovery(a.root, recs)
    vs = views(a.root, recs)
    if a.check:
        stale = [os.path.relpath(p, a.root) for p, cur, new in vs if cur != new]
        if stale:
            print(f"bug ledger: generated views differ from the records: {', '.join(stale)}; edit the records "
                  "and run python3 scripts/gen/bug_ledger.py --write (the views are generated, never "
                  "hand-edited)", file=sys.stderr)
            return 1
        print(f"bug ledger: {len(vs)} generated views match {len(recs)} records")
        return 0
    if a.write:
        for p, cur, new in vs:
            if cur != new:
                with open(p, "w", encoding="utf-8") as fh:
                    fh.write(new)
                print(f"bug ledger: regenerated {os.path.relpath(p, a.root)}")
        return 0
    n_open = sum(1 for r in recs.values() if r["open"])
    print(f"bug ledger: {len(recs)} records, {n_open} open; views: "
          + ", ".join(os.path.relpath(p, a.root) + (" (stale)" if cur != new else "") for p, cur, new in vs))
    return 0


if __name__ == "__main__":
    sys.exit(main())
