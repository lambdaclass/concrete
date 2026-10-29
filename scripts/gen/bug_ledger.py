#!/usr/bin/env python3
"""Generate the bug ledger's shared views from the bug records, the single owner of each bug's facts.

  bug_ledger.py              print a summary of what would be generated
  bug_ledger.py --write      regenerate every view in place (also resolves a rebase conflict inside
                             a generated block: the block is replaced whole, conflict markers included)
  bug_ledger.py --check      exit 1 when a view differs from the records (stale or hand-edited);
                             check_docs_drift.sh runs this
  bug_ledger.py report       print the bug-area research tables (stage and module, class x stage,
                             month, silent, how found, time to fix, recurring pairs) derived by
                             scripts/gen/bug_areas.py; --json for JSON, --sqlite PATH to also write a
                             throwaway SQLite database (generated, never committed); --history DIR
                             reads another checkout's git history (default: --root)
  bug_ledger.py rulings      print every bug's spec ruling, one tab-separated row per bug:
                             NNN<TAB>open|fixed<TAB>kind<TAB>label (check_bug_filing_complete.sh reads it)
  bug_ledger.py corpus       print the regression-coverage map, one tab-separated row per bug:
                             NNN<TAB>fixtures<TAB>open|fixed<TAB>a.con b.con   or
                             NNN<TAB>coverage<TAB>open|fixed<TAB>rationale;
                             audit_bug_corpus.sh and check_bug_filing_complete.sh read it (it is never
                             committed, so it cannot conflict)

ONE OWNER PER FACT. A bug's facts live in its record, docs/bugs/NNN_*.md, and nowhere else:

  status        the record's first `**Status:**` (or legacy `Status:`) line, as bug_dashboard.py reads
                it: OPEN/Open, CONTAINED, Half (closed), Partially (fixed) are open; FIXED/Fixed closed
  everything else, in one ledger block anywhere in the record (it renders as nothing on GitHub):

      <!-- bug-ledger
      summary: one line for the bug list (required)
      fixtures: regress_175_x.con dir/src/main.con   (paths relative to tests/programs/)
      coverage: the rationale naming the gate or fixture that covers it, when there is no fixture
                (it never restates the status: a leading FIXED/OPEN/... word is refused)
      class: 8 — optional note shown beside the bug in STRUCTURAL_CLASSES.md's summary table
      class: 12                                       (one line per class; table order picks primary)
      silent: yes | no                                (silent wrong code / unsound claim vs loud)
      exit: 0b                                        (ROADMAP exits whose generated bug list names it)
      area: Lower/Lower — why the derivation is wrong (optional: replaces the derived primary area;
                                                      an unknown stage is refused, see bug_areas.py)
      ruling: Violates: OWN-1@2                       (required: the spec ruling, see bug_rulings.py)
      -->

  Exactly one of `fixtures` and `coverage` is required, and a `ruling`: Violates: ID@V, Spec gap:
  PREFIX — <rule>, Open: OPEN-N, Permitted by: ID@V, Outside the spec: REASON — <why>, or, for a fixed
  bug below bug_rulings.RULING_FROM only, Violates: unrecorded. A cited rule must be a key of
  docs/spec/ids.tsv. A value is one line with no tab and no `-->`.

THE GENERATED VIEWS (each between BEGIN/END GENERATED markers; never edit inside them):

  docs/bugs/README.md              the "## Fixed Bugs" and "## Open Numbered Bugs" lists
  docs/bugs/STRUCTURAL_CLASSES.md  the summary table's bugs column (the class name and owner cells
                                   are owned by the class and kept as written)
  ROADMAP.md                       each `<!-- BEGIN GENERATED: exit X bugs ... -->` span: the bugs
                                   whose record names exit X, ranges compacted
  docs/bugs/RULINGS.md             the spec rulings: counts by kind and by spec chapter, every violated
                                   rule with its bugs, the spec gaps (rules to write) and the open owner
                                   questions (scripts/gen/bug_rulings.py)
  docs/bugs/AREAS.md               each bug's stage and module, derived from its fix commits in git
                                   history and docs/bugs/area_map.tsv (scripts/gen/bug_areas.py), with
                                   the stage and class tables. It goes stale when a commit naming a bug
                                   changes code: after committing a fix, --write and commit (or amend);
                                   regenerated views carry no weight, so that settles it. A tree with no
                                   project history (a scratch copy) skips it and says so; a shallow
                                   clone is refused

BEFORE THE MIGRATION (scripts/gen/migrate_ledgers.sh) no record carries a ledger block: every mode
says so and exits 0, and `corpus` prints nothing, so the legacy maps in audit_bug_corpus.sh stay the
coverage source. Once any record carries a block, every record must (a mixed ledger exits 2).

FAIL-CLOSED (exit 2, nothing written): two records for one number, a malformed or duplicated block,
an unknown key, a missing summary, both or neither of fixtures/coverage, a class the summary table
does not have, an exit with no generated span, a bad silent value, missing or duplicated markers, a
structural row whose cells disagree between the two sides of a conflict, or a silent value that
disagrees with docs/bugs/discovery.tsv (when that file exists, its `silent` column must name exactly
the records marked `silent: yes` among the bugs it lists); an `area:` that names an unknown stage, a malformed
docs/bugs/area_map.tsv, a fix-commit path no area_map row matches, or a shallow history.

--root overrides the repository root (a test seam for the gate's negative controls).
"""
import argparse
import glob
import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import bug_areas  # noqa: E402  (beside this file; -I leaves the script's directory off sys.path)
import bug_rulings  # noqa: E402

KEYS = {"summary", "fixtures", "coverage", "class", "silent", "exit", "area", "ruling"}
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
# A status, in the ledger's vocabulary, outside the **Status:** line: an upper-case status word anywhere,
# or a leading status phrase in any case ("Fixed -- ...", "contained: ...").
STATUS_UPPER = re.compile(r"\b(FIXED|OPEN|CONTAINED|HALF CLOSED|PARTIALLY FIXED|PARTIAL)\b")
STATUS_LEAD = re.compile(r"^(fixed|open|contained|half closed|partially fixed|partial)\b", re.I)
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


def status_word(text):
    """True (open), False (fixed) or None (no status line, or a word outside the vocabulary)."""
    m = re.search(r"^(?:\*\*Status:\*\*|Status:)\s*(.*)$", text, re.M)
    word = (re.findall(r"[A-Za-z]+", m.group(1))[:1] or [""])[0].lower() if m else ""
    return True if word in OPEN_WORDS else False if word in CLOSED_WORDS else None


def status_open(text, name):
    m = re.search(r"^(?:\*\*Status:\*\*|Status:)\s*(.*)$", text, re.M)
    if not m:
        die(f"{name} has no **Status:** line")
    st = status_word(text)
    if st is None:
        die(f"{name}: status '{m.group(1)[:40]}' is not one of OPEN, CONTAINED, Half closed, Partially fixed or FIXED")
    return st


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
        if "-->" in v:
            die(f"{name}: ledger key '{k}' holds '-->', which would close the hidden block early")
        if k in REPEATABLE:
            cm = re.match(r"^(\d+)(?:\s+—\s+(.+))?$", v)
            if not cm:
                die(f"{name}: class must be 'N' or 'N — note', got '{v[:40]}'")
            if "|" in (cm.group(2) or ""):
                die(f"{name}: a class note may not contain '|' (it is a table cell)")
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
    for k in ("summary", "coverage"):
        v = facts.get(k, "")
        m = STATUS_UPPER.search(v) or STATUS_LEAD.match(v)
        if m:
            die(f"{name}: the {k} line states a status ('{m.group(0).strip()}'); the **Status:** line is the only "
                f"status a bug has, so the {k} says what the bug is or what covers it, never whether it is open")
    if any("\t" in v for k, v in facts.items() if isinstance(v, str)):
        die(f"{name}: a ledger value holds a tab")
    if facts.get("silent", "yes") not in ("yes", "no"):
        die(f"{name}: silent must be yes or no, got '{facts['silent']}'")
    if "area" in facts:
        try:
            bug_areas.parse_area(facts["area"], name)
        except bug_areas.AreaError as e:
            die(str(e))
    if "ruling" not in facts:
        die(f"{name}: the ledger block has no ruling: the spec rule the bug violates (ruling: Violates: ID@V), "
            "or Spec gap, Open or Outside the spec (scripts/gen/bug_rulings.py; docs/bugs/README.md, Filing a bug)")
    try:
        facts["ruling"] = bug_rulings.parse(facts["ruling"], name)
    except bug_rulings.RulingError as e:
        die(str(e))
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


def resolve_rulings(root, recs):
    """Resolve every record's ruling against docs/spec/ids.tsv; returns the spec's chapter table."""
    try:
        spec = bug_rulings.load_spec(root)
        for r in recs.values():
            r["facts"]["ruling"] = bug_rulings.resolve(r["facts"]["ruling"], spec, r["file"], r["num"], r["open"])
    except bug_rulings.RulingError as e:
        die(str(e))
    return spec[1]


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


def side(text, k):
    """`text` with every conflict hunk replaced by its side k (1 ours, 2 theirs)."""
    return HUNK.sub(lambda m: m.group(k), text)


def class_rows(block, path):
    """The (num, name, owner) rows of the class table. Inside a rebase conflict the two sides must agree
    on every row's presence, order, name and owner (the class-owned cells); only the generated bugs
    column may differ, and it is regenerated. Anything else is a real conflict, left for a human."""
    def rows_of(text):
        out = []
        for ln in text.split("\n"):
            if not ln.strip() or ln.startswith((CLASS_BEGIN[:40], CLASS_END)):
                continue
            if not ln.startswith("|"):
                die(f"{path}: the generated class table holds a line that is not a table row; move prose "
                    f"outside the markers: {ln[:80]}")
            if ln in ("| class | bugs | structural owner |", "|---|---|---|"):
                continue
            m = CLASS_ROW.match(ln)
            if not m or m.group(4).count("|") or m.group(2).count("|"):
                die(f"{path}: an unreadable row in the generated class table (expected '| N. class | bugs | "
                    f"owner |' with no '|' inside a cell): {ln[:80]}")
            out.append((m.group(1), m.group(2), m.group(4)))
        return out
    ours, theirs = rows_of(side(block, 1)), rows_of(side(block, 2))
    if ours != theirs:
        die(f"{path}: the two sides of a conflict in the class table differ in a class row's presence, order, "
            "name or owner (cells the class owns); resolve that by hand, then rerun --write")
    if re.search(r"^(<{7}|>{7})", side(block, 1), re.M):
        die(f"{path}: unbalanced conflict markers in the class table; resolve them by hand")
    if not ours or [n for n, _, _ in ours] != [str(i) for i in range(1, len(ours) + 1)]:
        die(f"{path}: the generated class table must hold rows | N. class | bugs | owner | numbered 1..N")
    return ours


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


def class_names(root):
    p = os.path.join(root, "docs", "bugs", "STRUCTURAL_CLASSES.md")
    _, block = splice(read(p), CLASS_BEGIN, CLASS_END, "", p)
    return {n: name for n, name, _ in class_rows(block, p)}


def derive_areas(root, history, recs, required):
    """(areas, None) from the history, or (None, reason) when it has no usable project history (an
    error when `required`). A malformed input or an unmapped path exits 2."""
    try:
        bug_areas.load_map(root)
        bug_areas.check_history(history)
        return bug_areas.derive(history, root, recs, status_word), None
    except bug_areas.NoHistory as e:
        if required:
            die(f"the bug areas derive from git history, and there is none to read: {e}")
        return None, str(e)
    except bug_areas.AreaError as e:
        die(str(e))


def views(root, recs, chapters, areas=None):
    """[(path, current text, regenerated text)] for every generated view (AREAS.md only when the
    areas could be derived)."""
    out = []
    p = os.path.join(root, "docs", "bugs", "RULINGS.md")
    cur = read(p)
    new, _ = splice(cur, bug_rulings.BEGIN, bug_rulings.END, bug_rulings.render_view(recs, chapters), p)
    out.append((p, cur, new))
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
    if base.count("<!-- BEGIN GENERATED: exit ") != len(spans) or base.count("<!-- END GENERATED: exit ") != len(spans):
        die(f"{p}: an exit bug span's BEGIN/END markers do not pair up (a missing, duplicated or orphan marker)")
    named = {x for r in recs.values() for x in r["facts"]["exit"]}
    if named - set(spans):
        die(f"records name exits with no generated bug span in ROADMAP.md: {', '.join(sorted(named - set(spans)))}")
    new = EXIT_SPAN.sub(lambda m: EXIT_BEGIN.format(x=m.group(1)) + exit_text(recs, m.group(1))
                        + EXIT_END.format(x=m.group(1)), base)
    out.append((p, cur, new))
    if areas is not None:
        p = os.path.join(root, "docs", "bugs", "AREAS.md")
        cur = read(p)
        new, _ = splice(cur, bug_areas.BEGIN, bug_areas.END, bug_areas.render_view(recs, areas, class_names(root)), p)
        out.append((p, cur, new))
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("mode", nargs="?", choices=["corpus", "report", "rulings"])
    ap.add_argument("--root", default=os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")))
    ap.add_argument("--history", help="the git work tree whose history the bug areas derive from (default: --root)")
    ap.add_argument("--json", action="store_true", help="report: print JSON instead of text tables")
    ap.add_argument("--sqlite", metavar="PATH", help="report: also write a throwaway SQLite database to PATH")
    g = ap.add_mutually_exclusive_group()
    g.add_argument("--write", action="store_true")
    g.add_argument("--check", action="store_true")
    a = ap.parse_args()
    if (a.json or a.sqlite) and a.mode != "report":
        ap.error("--json and --sqlite apply to the report mode")
    history = a.history or a.root
    recs, migrated = load_records(a.root)
    if a.mode == "corpus":
        for r in recs.values() if migrated else []:
            f = r["facts"]
            kind = "fixtures" if "fixtures" in f else "coverage"
            print(f"{r['num']}\t{kind}\t{'open' if r['open'] else 'fixed'}\t{f[kind]}")
        return 0
    if not migrated:
        print("bug ledger: not migrated yet (no record carries a ledger block); the legacy views are "
              "hand-maintained until scripts/gen/migrate_ledgers.sh runs")
        return 0
    check_discovery(a.root, recs)
    chapters = resolve_rulings(a.root, recs)
    if a.mode == "rulings":
        for r in recs.values():
            rl = r["facts"]["ruling"]
            print(f"{r['num']}\t{'open' if r['open'] else 'fixed'}\t{rl['kind']}\t{bug_rulings.label(rl)}")
        return 0
    if a.mode == "report":
        areas, _ = derive_areas(a.root, history, recs, required=True)
        try:
            rows = bug_areas.bug_rows(recs, areas, bug_areas.load_discovery_kinds(a.root), chapters)
        except bug_areas.AreaError as e:
            die(str(e))
        names = class_names(a.root)
        if a.sqlite:
            bug_areas.write_sqlite(a.sqlite, rows, names)
        if a.json:
            print(json.dumps({"bugs": rows, "tables": [{"title": t, "header": h, "rows": r}
                                                       for t, h, r, _ in bug_areas.report_tables(rows, names)]},
                             indent=1, sort_keys=True))
        else:
            print(bug_areas.render_report(bug_areas.report_tables(rows, names)), end="")
        return 0
    areas, why_not = derive_areas(a.root, history, recs, required=False)
    if why_not:
        print(f"bug ledger: docs/bugs/AREAS.md is neither regenerated nor compared: {why_not} "
              "(pass --history <checkout> to derive the areas from another checkout's history)", file=sys.stderr)
    vs = views(a.root, recs, chapters, areas)
    if a.check:
        stale = [os.path.relpath(p, a.root) for p, cur, new in vs if cur != new]
        if stale:
            print(f"bug ledger: generated views differ from the records: {', '.join(stale)}; edit the records "
                  "and run python3 scripts/gen/bug_ledger.py --write (the views are generated, never "
                  "hand-edited)", file=sys.stderr)
            if "docs/bugs/AREAS.md" in stale:
                print("bug ledger: docs/bugs/AREAS.md also derives from git history (a bug's fix commits), so it "
                      "goes stale when a commit naming a bug changes code: after committing a fix, run --write and "
                      "commit the result (or amend it into the fix). Regenerated views carry no weight, so one "
                      "regeneration settles it", file=sys.stderr)
            return 1
        print(f"bug ledger: {len(vs)} generated views match {len(recs)} records"
              + ("" if areas is not None else " (AREAS.md not compared: no history)"))
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
