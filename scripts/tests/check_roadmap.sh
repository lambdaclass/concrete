#!/usr/bin/env bash
# THE ROADMAP NAMES REAL TASKS, AND NO ARCHIVED TASK IS LOST.
#
# ROADMAP.md declares one row per live task (`| R-NNNN | objective | done when | depends on |`
# under "## Tasks") and a queue whose rows name those tasks. The 2026-11 archive's disposition table
# (docs/archive/roadmap-2026-11/task_disposition.tsv) says what became of every older task ID. This
# gate checks the two against each other and against the archive:
#   1. every task row has four non-empty cells, and task IDs in ROADMAP.md are unique;
#   2. a task whose ID is in the table is `retained` there, in the same phase; any other task has a
#      new ID above the highest archived one (archived IDs are never reused);
#   3. every `retained` row is a task in ROADMAP.md, or completed (a `### Completed Task R-NNNN`
#      heading in CHANGELOG.md with the changelog.d/ fragments folded in); a completed task is not
#      also live (nothing is dropped silently);
#   4. the table has exactly one row per archived ID: every `### Task R-NNNN` heading of the archived
#      task register and backlog, and every task the changelog completed up to the highest of them;
#   5. every row has a known disposition and the columns that disposition requires: a transfer names
#      a destination that is a task in ROADMAP.md (in that task's phase) or a completed task, and the
#      obligations it carries; a deferral names its trigger; a retirement or a done row its reason;
#   6. every queue row has a unique ID (a phase letter, a number, an optional stage letter: T0, A2b) and names at least one task, each a task in ROADMAP.md of the
#      row's phase; every "depends on" item is a task, a queue row, a phase, `owner: <decision>` or
#      `—`;
#   7. every structural owner named in docs/bugs/STRUCTURAL_CLASSES.md is still owned: a task in
#      ROADMAP.md, a row transferred to one, a done row or a completed task;
#   8. every local link in ROADMAP.md and the archive README resolves, anchors included;
#   9. no bug in the candidate record (docs/project/candidates.tsv, the source of ROADMAP.md's
#      "Implemented, not yet accepted" table) is already recorded fixed in docs/bugs.
# The controls run the same check on edited copies, so a check that accepts everything fails here
# before the real run.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)" || { echo "FATAL: mktemp failed" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT

ARCHIVE=docs/archive/roadmap-2026-11
ROADMAP=ROADMAP.md
TABLE=$ARCHIVE/task_disposition.tsv
README=$ARCHIVE/README.md
CLASSES=docs/bugs/STRUCTURAL_CLASSES.md
CANDIDATES=docs/project/candidates.tsv
export ARCHIVED="$ARCHIVE/ROADMAP_TASKS.md $ARCHIVE/ROADMAP_BACKLOG.md"
for f in "$ROADMAP" "$TABLE" "$README" "$CLASSES" "$CANDIDATES" $ARCHIVED; do
  [ -f "$f" ] || { echo "FATAL: $f missing" >&2; exit 2; }
done
# CHANGELOG.md as it reads with the pending fragments folded in: a branch completes a task with a
# `### Completed Task R-NNNN` heading in its fragment, and that counts before the fold.
CHANGELOG="$TMP/changelog.md"
python3 -I scripts/gen/changelog.py --print > "$CHANGELOG" || { echo "FATAL: scripts/gen/changelog.py --print failed" >&2; exit 2; }
export CHANGELOG

# check ROADMAP TABLE CLASSES BASE CANDIDATES README...: prints one "FAIL <why>" line per violation,
# then "CHECKED ..."; exits 1 on a violation. The roadmap's links resolve against BASE (the
# repository root, also for a control's copy); each other file's against its own directory.
check() {
  python3 -I - "$ROOT_DIR/scripts/tests/lib" "$@" <<'PY'
import os, re, sys
from pathlib import Path
from urllib.parse import unquote
sys.path.insert(0, sys.argv[1])
import doc_links as D
roadmap, table, classes, base, cands = sys.argv[2], sys.argv[3], sys.argv[4], Path(sys.argv[5]).resolve(), sys.argv[6]
linked = [(roadmap, base)] + [(f, Path(f).resolve().parent) for f in sys.argv[7:]]
bad = []
text = open(roadmap, encoding="utf-8").read()
completed = set(re.findall(r"^### Completed Task (R-\d{4})\s*$", open(os.environ["CHANGELOG"], encoding="utf-8").read(), re.M))

def cells(line):
    return [c.strip() for c in line.strip().strip("|").split("|")]

# 1. task rows, by phase subsection under "## Tasks"
tasks, deps, phase, in_tasks = {}, {}, None, False
for n, line in enumerate(text.split("\n"), 1):
    if line.startswith("## "):
        in_tasks = line.strip() == "## Tasks"; phase = None; continue
    m = re.match(r"^### ([A-GT])\. ", line)
    if m and in_tasks:
        phase = m.group(1); continue
    m = re.match(r"^\|\s*(R-\d{4})\s*\|", line)
    if m and in_tasks:
        k = m.group(1); c = cells(line)
        if phase is None:
            bad.append(f"{roadmap}:{n}: task {k} outside a phase subsection"); continue
        if len(c) != 4 or not all(c):
            bad.append(f"{roadmap}:{n}: task {k} needs four non-empty cells (task, objective, done when, depends on)")
        if k in tasks:
            bad.append(f"{roadmap}:{n}: task {k} declared twice")
        tasks[k] = phase
        deps[k] = (n, c[3] if len(c) == 4 else "")
if not tasks:
    bad.append(f"{roadmap}: no task rows under '## Tasks'")

# 6a. the queue
m = re.search(r"^## The queue\s*\n(.*?)(?=^## |\Z)", text, re.M | re.S)
qrows = {}
if not m:
    bad.append(f"{roadmap}: no '## The queue' section")
else:
    for line in m.group(1).split("\n"):
        c = cells(line)
        if len(c) == 4 and re.fullmatch(r"[A-GT](?:0|[1-9][0-9]*)[a-z]?", c[0]):
            if c[0] in qrows: bad.append(f"queue row {c[0]} appears twice")
            ids = re.findall(r"R-\d{4}", c[2])
            qrows[c[0]] = ids
            if not ids: bad.append(f"queue row {c[0]} names no task")
            for i in ids:
                if i not in tasks: bad.append(f"queue row {c[0]} names {i}, which is not a task in {roadmap}")
                elif tasks[i] != c[0][0]: bad.append(f"queue row {c[0]} names {i}, a phase {tasks[i]} task")
    if not qrows: bad.append(f"{roadmap}: the queue has no rows")

# 6b. "depends on" items
for k, (n, cell) in deps.items():
    for item in [x.strip() for x in cell.split(",")]:
        if item == "—" or re.fullmatch(r"owner: \S.*", item):
            continue
        if re.fullmatch(r"R-\d{4}", item):
            if item not in tasks and item not in completed:
                bad.append(f"{roadmap}:{n}: {k} depends on {item}, which is neither a task nor completed")
        elif re.fullmatch(r"[A-GT](?:0|[1-9][0-9]*)[a-z]?", item):
            if item not in qrows: bad.append(f"{roadmap}:{n}: {k} depends on queue row {item}, which does not exist")
        elif re.fullmatch(r"[A-GT]", item):
            if item not in tasks.values(): bad.append(f"{roadmap}:{n}: {k} depends on phase {item}, which has no task")
        else:
            bad.append(f"{roadmap}:{n}: {k} depends on {item!r}: not a task, queue row, phase, 'owner: ...' or '—'")

# 5. the disposition table
rows = {}
lines = open(table, encoding="utf-8").read().rstrip("\n").split("\n")
want = ["id", "title", "disposition", "new_phase", "destination", "obligations", "trigger", "reason", "owner_question"]
if lines[0].split("\t") != want:
    bad.append(f"{table}: header {lines[0].split(chr(9))} is not {want}")
for n, line in enumerate(lines[1:], 2):
    c = line.split("\t")
    if len(c) != len(want):
        bad.append(f"{table}:{n}: {len(c)} columns, expected {len(want)}"); continue
    r = dict(zip(want, c))
    if not re.fullmatch(r"R-\d{4}", r["id"]):
        bad.append(f"{table}:{n}: malformed id {r['id']!r}"); continue
    if r["id"] in rows:
        bad.append(f"{table}:{n}: {r['id']} has two rows")
    rows[r["id"]] = r

# 4. one row per archived ID
headings = set()
for f in os.environ["ARCHIVED"].split():
    headings |= set(re.findall(r"^### Task (R-\d{4})$", open(f, encoding="utf-8").read(), re.M))
top = max((int(k[2:]) for k in headings), default=0)
universe = headings | {k for k in completed if int(k[2:]) <= top}
for k in sorted(universe - set(rows)):
    bad.append(f"{k}: an archived task ID with no row in {table}")
for k in sorted(set(rows) - universe):
    bad.append(f"{k}: a row in {table} for an ID the archive never declared")

for k, r in rows.items():
    d = r["disposition"]
    if d == "retained":
        if not r["obligations"]: bad.append(f"{k}: retained without obligations")
    elif d == "transferred-with-obligations":
        dest = r["destination"]
        if dest in tasks:
            if r["new_phase"] != tasks[dest]:
                bad.append(f"{k}: transferred to {dest} (phase {tasks[dest]}) but records phase {r['new_phase'] or '-'}")
        elif dest not in completed:
            bad.append(f"{k}: transferred to {dest or '(nothing)'}, which is neither a task in {roadmap} nor completed")
        if not r["obligations"]: bad.append(f"{k}: transferred without the obligations it carries")
    elif d == "deferred-with-trigger":
        if not r["trigger"]: bad.append(f"{k}: deferred without a trigger")
    elif d in ("retired", "done"):
        if not r["reason"]: bad.append(f"{k}: {d} without a reason")
    else:
        bad.append(f"{k}: unknown disposition {d!r}")
    if d != "transferred-with-obligations" and r["destination"]:
        bad.append(f"{k}: a destination on a {d} row")

# 2 and 3. roadmap tasks against the table
for k, ph in tasks.items():
    r = rows.get(k)
    if r is None:
        if int(k[2:]) <= top:
            bad.append(f"{k}: not in {table} but at or below R-{top:04d}; new tasks take numbers above the archive")
    elif r["disposition"] != "retained":
        bad.append(f"{k}: reuses an archived ID whose disposition is {r['disposition']}")
    elif r["new_phase"] != ph:
        bad.append(f"{k}: phase {ph} in {roadmap}, {r['new_phase']} in {table}")
for k in sorted(completed & set(tasks)):
    bad.append(f"{k}: completed in the changelog but still a task in {roadmap}")
for k, r in rows.items():
    if r["disposition"] == "retained" and k not in tasks and k not in completed:
        bad.append(f"{k}: retained in {table} but neither a task in {roadmap} nor completed")

# 7. structural owners of bug classes
def owned(k, seen=()):
    if k in tasks or k in completed: return True
    r = rows.get(k)
    if r is None or k in seen: return False
    if r["disposition"] == "done": return True
    if r["disposition"] == "transferred-with-obligations": return owned(r["destination"], seen + (k,))
    return False
for k in sorted(set(re.findall(r"R-\d{4}", open(classes, encoding="utf-8").read()))):
    if not owned(k):
        bad.append(f"{classes} names {k} as an owner, and it is not owned in {roadmap} (disposition {rows.get(k, {}).get('disposition', 'none')})")

# 8. links
for f, fbase in linked:
    p = Path(f).resolve(); src = p.read_text(encoding="utf-8")
    scan = D.blank_code(src)
    for mm in D.INLINE.finditer(scan):
        target = mm.group(1).strip("<>")
        if D.EXTERNAL.match(target): continue
        local, _, frag = target.partition("#")
        dest = (fbase / unquote(local)).resolve() if local else p
        line = src.count("\n", 0, mm.start(1)) + 1
        if not dest.exists():
            bad.append(f"{f}:{line}: missing link target {target}"); continue
        if frag and dest.suffix.lower() == ".md":
            slugs, ids = D.anchors(dest.read_text(errors="replace"))
            if unquote(frag) not in ids and unquote(frag).lower() not in slugs:
                bad.append(f"{f}:{line}: no anchor #{frag} in {dest.name}")

# 9. candidate bugs are not already fixed
body = [l for l in open(cands, encoding="utf-8").read().split("\n") if l and not l.startswith("#")]
hdr = body[0].split("\t") if body else []
if "bugs" not in hdr:
    bad.append(f"{cands}: no bugs column")
else:
    for l in body[1:]:
        c = dict(zip(hdr, l.split("\t")))
        for n in re.findall(r"\b\d{3}\b", c.get("bugs", "")):
            for f in sorted((base / "docs" / "bugs").glob(n + "_*.md")):
                st = re.search(r"^(?:\*\*Status:\*\*|Status:)\s*(\S+)", f.read_text(encoding="utf-8"), re.M | re.I)
                if st and st.group(1).lower().startswith("fixed"):
                    bad.append(f"candidate {c.get('id')}: bug {n} is recorded fixed in {f.name}; remove it from {cands}")

for b in bad: print("FAIL " + b)
print(f"CHECKED tasks={len(tasks)} rows={len(rows)} archived={len(universe)} queue={len(qrows)}")
sys.exit(1 if bad else 0)
PY
}

# copies NAME: fresh copies of the inputs in $TMP/NAME.
copies() {
  local d="$TMP/$1"
  mkdir -p "$d"
  cp "$ROADMAP" "$d/ROADMAP.md"; cp "$TABLE" "$d/t.tsv"; cp "$CLASSES" "$d/c.md"; cp "$CANDIDATES" "$d/k.tsv"
  cp "$CHANGELOG" "$d/changelog.md"
}
# run NAME: the check over the copies in $TMP/NAME, with their changelog.
run() {
  local d="$TMP/$1"
  CHANGELOG="$d/changelog.md" check "$d/ROADMAP.md" "$d/t.tsv" "$d/c.md" "$ROOT_DIR" "$d/k.tsv"
}
# edit NAME PYTHON: apply a python edit to the copies, with r, t, c, k, g bound to the roadmap,
# table, classes, candidate record and changelog texts; the edit must change something.
edit() {
  python3 -I - "$TMP/$1" "$2" <<'PY'
import sys
d, e = sys.argv[1], sys.argv[2]
names = {"r": "ROADMAP.md", "t": "t.tsv", "c": "c.md", "k": "k.tsv", "g": "changelog.md"}
v = {n: open(f"{d}/{f}").read() for n, f in names.items()}
o = dict(v)
exec(e, {}, v)
assert v != o, "the edit changed nothing"
for n, f in names.items():
    open(f"{d}/{f}", "w").write(v[n])
PY
}
# control NAME PYTHON PATTERN: the edited copy is refused, and the output matches PATTERN.
control() {
  copies "$1"
  if ! edit "$1" "$2"; then no "control $1: the edit did not apply"; return; fi
  local out rc=0
  out="$(run "$1" 2>&1)" || rc=$?
  if [ "$rc" = 1 ] && has_match -e "$3" <<<"$out"; then ok "control $1 is refused"
  else no "control $1: exit $rc, expected 1 with /$3/"; printf '%s\n' "$out" | awk 'NR<=5'; fi
}
# accepted NAME PYTHON: the edited copy passes.
accepted() {
  copies "$1"
  if ! edit "$1" "$2"; then no "positive control $1: the edit did not apply"; return; fi
  local out rc=0
  out="$(run "$1" 2>&1)" || rc=$?
  if [ "$rc" = 0 ]; then ok "positive control $1 passes"
  else no "positive control $1: exit $rc"; printf '%s\n' "$out" | awk 'NR<=5'; fi
}

echo "=== controls: the check on edited copies ==="
# An unedited copy passes, so every refusal below is caused by its edit alone.
copies clean
if run clean > "$TMP/clean.out" 2>&1; then ok "positive control: an unedited copy passes"
else no "positive control: an unedited copy is refused"; awk 'NR<=5' "$TMP/clean.out"; fi

control duplicate_task "import re; m=re.search(r'^\| R-0496 \|.*$', r, re.M); r=r.replace(m.group(0), m.group(0)+'\n'+m.group(0), 1)" "R-0496 declared twice"
control empty_cells "import re; r=re.sub(r'^\| R-0496 \|.*$', '| R-0496 | | | |', r, count=1, flags=re.M)" "R-0496 needs four non-empty cells"
control reused_archived_id "r=r.replace('| R-0496 |', '| R-0010 | reused | x | — |\n| R-0496 |', 1)" "R-0010: reuses an archived ID"
control new_id_below_top "r=r.replace('| R-0496 |', '| R-0457 | squatter | x | — |\n| R-0496 |', 1)" "R-0457: not in"
control retained_dropped "import re; r=re.sub(r'^\| R-0507 \|.*\n', '', r, count=1, flags=re.M)" "R-0507: retained in .* neither"
control row_deleted "import re; t=re.sub(r'^R-0012\t.*\n', '', t, count=1, flags=re.M)" "R-0012: an archived task ID with no row"
control row_unexpected "t=t+'R-0457\tghost\tdeferred-with-trigger\t\t\tx\tsome trigger\tx\t\n'" "R-0457: a row in .* never declared"
control queue_unknown "r=r.replace('| R-0553, R-0562,', '| R-0553, R-0999, R-0562,', 1)" "names R-0999"
control queue_wrong_phase "r=r.replace('| A3 | State the contract', '| G3 | State the contract', 1)" "queue row G3 names R-0560, a phase A task"
control queue_duplicate_row "r=r.replace('| A3 | State the contract', '| A2c | State the contract', 1)" "queue row A2c appears twice"
control depends_unknown_syntax "r=r.replace(', owner: approval of the move inventory |', ', approval of the move inventory |', 1)" "not a task, queue row, phase"
control depends_missing_row "import re; r=re.sub(r'(^\| R-0560 \|.*\| )A2c( \|)$', r'\g<1>A9\2', r, count=1, flags=re.M)" "queue row A9, which does not exist"
control dangling_transfer "import re; t=re.sub(r'^(R-0010\t[^\t]*\ttransferred-with-obligations\t[^\t]*\t)R-0561', r'\g<1>R-0010', t, count=1, flags=re.M)" "R-0010: transferred to R-0010"
control transfer_wrong_phase "import re; t=re.sub(r'^(R-0010\t[^\t]*\ttransferred-with-obligations\t)B', r'\g<1>C', t, count=1, flags=re.M)" "R-0010: transferred to R-0561 .phase B. but records phase C"
control deferral_without_trigger "import re; t=re.sub(r'^(R-0012\t[^\t]*\tdeferred-with-trigger\t[^\t]*\t[^\t]*\t[^\t]*\t)[^\t]+', r'\1', t, count=1, flags=re.M)" "R-0012: deferred without a trigger"
control retired_class_owner "c=c+'\nOwner: R-0518.\n'" "names R-0518 as an owner"
control broken_link "r=r.replace('(docs/compiler/CORE_PROPOSAL.md)', '(docs/compiler/NO_SUCH_FILE.md)', 1)" "missing link target"
control broken_anchor "r=r.replace('ROADMAP_DECISIONS.md#2026-11-01--compiler-proof-order', 'ROADMAP_DECISIONS.md#no-such-anchor', 1)" "no anchor #no-such-anchor"
control completed_but_listed "g=g+'\n### Completed Task R-0507\n\ncontrol\n'" "R-0507: completed in the changelog but still a task"
# A candidate naming a bug the ledger records fixed (102 is FIXED).
control candidate_bug_fixed "import re; k=re.sub(r'^(rocksdb-slice1\t)[^\t]*', r'\g<1>102', k, count=1, flags=re.M)" "bug 102 is recorded fixed"
# A completed task leaves the roadmap and its queue row, and transfers into it stay valid: R-0493
# receives R-0222's obligations.
accepted completed_leaves "import re; r,n=re.subn(r'^\| R-0493 \|.*\n','',r,count=1,flags=re.M); assert n==1; assert r.count('R-0492, R-0493, ')==1; assert 'R-0493' not in r.replace('R-0492, R-0493, ',''); r=r.replace('R-0492, R-0493, ','R-0492, '); g=g+'\n### Completed Task R-0493\n\ncontrol\n'"

echo "=== the repository ==="
rc=0; out="$(check "$ROADMAP" "$TABLE" "$CLASSES" "$ROOT_DIR" "$CANDIDATES" "$README" 2>&1)" || rc=$?
printf '%s\n' "$out"
if [ "$rc" = 0 ]; then ok "ROADMAP.md, the archive, the disposition table and the bug-class owners agree"
else no "ROADMAP.md, the archive, the disposition table or the bug-class owners disagree (exit $rc)"; fi

echo ""
echo "ROADMAP: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
