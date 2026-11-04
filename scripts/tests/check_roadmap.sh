#!/usr/bin/env bash
# THE ROADMAP NAMES REAL TASKS, AND NO ARCHIVED TASK IS LOST.
#
# ROADMAP.md declares one row per live task (`| R-NNNN | objective | done when | depends on |`
# under "## Tasks") and a queue whose rows name those tasks. The 2026-11 archive's disposition table
# (docs/archive/roadmap-2026-11/task_disposition.tsv) says what became of every older task ID. This
# gate checks the two against each other:
#   1. task IDs in ROADMAP.md are unique;
#   2. a task whose ID is in the table is `retained` there, in the same phase; any other task has a
#      new ID above the highest archived one (archived IDs are never reused);
#   3. every `retained` row is a task in ROADMAP.md, or completed (a `### Completed Task R-NNNN`
#      heading in CHANGELOG.md with the changelog.d/ fragments folded in); a completed task is not
#      also live (nothing is dropped silently);
#   4. every row has a known disposition and the columns that disposition requires: a transfer names
#      a destination that is a task in ROADMAP.md, in that task's phase, and the obligations it
#      carries; a deferral names its trigger; a retirement or a done row its reason;
#   5. every queue row names at least one task, and every task it names is in ROADMAP.md;
#   6. every structural owner named in docs/bugs/STRUCTURAL_CLASSES.md is still owned: a task in
#      ROADMAP.md, a row transferred to one, a done row or a completed task;
#   7. every local link in ROADMAP.md and the archive README resolves, anchors included;
#   8. no bug in the candidate record (docs/project/candidates.tsv, the source of ROADMAP.md's
#      "Implemented, not yet accepted" table) is already recorded fixed in docs/bugs.
# The negative controls run the same check on edited copies, so a check that accepts everything fails
# here before the real run.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)" || { echo "FATAL: mktemp failed" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT

ROADMAP=ROADMAP.md
TABLE=docs/archive/roadmap-2026-11/task_disposition.tsv
README=docs/archive/roadmap-2026-11/README.md
CLASSES=docs/bugs/STRUCTURAL_CLASSES.md
CANDIDATES=docs/project/candidates.tsv
for f in "$ROADMAP" "$TABLE" "$README" "$CLASSES" "$CANDIDATES"; do
  [ -f "$f" ] || { echo "FATAL: $f missing" >&2; exit 2; }
done
# CHANGELOG.md as it reads with the pending fragments folded in: a branch completes a task with a
# `### Completed Task R-NNNN` heading in its fragment, and that counts before the fold.
CHANGELOG="$TMP/changelog.md"
python3 -I scripts/gen/changelog.py --print > "$CHANGELOG" || { echo "FATAL: scripts/gen/changelog.py --print failed" >&2; exit 2; }
export CHANGELOG

# check ROADMAP TABLE CLASSES BASE CANDIDATES README...: prints one "FAIL <why>" line per violation, then
# "CHECKED tasks=N rows=M"; exits 1 on a violation. The roadmap's links resolve against BASE (the
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

# 1. task rows, by phase subsection under "## Tasks"
tasks, phase, in_tasks = {}, None, False
for n, line in enumerate(text.split("\n"), 1):
    if line.startswith("## "):
        in_tasks = line.strip() == "## Tasks"; phase = None; continue
    m = re.match(r"^### ([A-GT])\. ", line)
    if m and in_tasks:
        phase = m.group(1); continue
    m = re.match(r"^\| (R-\d{4}) \|", line)
    if m and in_tasks:
        if phase is None:
            bad.append(f"{roadmap}:{n}: task {m.group(1)} outside a phase subsection"); continue
        if m.group(1) in tasks:
            bad.append(f"{roadmap}:{n}: task {m.group(1)} declared twice")
        tasks[m.group(1)] = phase
if not tasks:
    bad.append(f"{roadmap}: no task rows under '## Tasks'")

# 4. the disposition table
rows = {}
lines = open(table, encoding="utf-8").read().rstrip("\n").split("\n")
head = lines[0].split("\t")
want = ["id", "title", "disposition", "new_phase", "destination", "obligations", "trigger", "reason", "owner_question"]
if head != want:
    bad.append(f"{table}: header {head} is not {want}")
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
if not rows:
    bad.append(f"{table}: no rows")
top = max((int(k[2:]) for k in rows), default=0)
for k, r in rows.items():
    d = r["disposition"]
    if d == "retained":
        if not r["obligations"]: bad.append(f"{k}: retained without obligations")
    elif d == "transferred-with-obligations":
        dest = r["destination"]
        if dest not in tasks:
            bad.append(f"{k}: transferred to {dest or '(nothing)'}, which is not a task in {roadmap}")
        elif r["new_phase"] != tasks[dest]:
            bad.append(f"{k}: transferred to {dest} (phase {tasks[dest]}) but records phase {r['new_phase'] or '-'}")
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
completed = set(re.findall(r"^### Completed Task (R-\d{4})\s*$", open(os.environ["CHANGELOG"], encoding="utf-8").read(), re.M))
for k in sorted(completed & set(tasks)):
    bad.append(f"{k}: completed in the changelog but still a task in {roadmap}")
for k, r in rows.items():
    if r["disposition"] == "retained" and k not in tasks and k not in completed:
        bad.append(f"{k}: retained in {table} but neither a task in {roadmap} nor completed")

# 5. the queue
m = re.search(r"^## The queue\s*\n(.*?)(?=^## |\Z)", text, re.M | re.S)
qrows = 0
if not m:
    bad.append(f"{roadmap}: no '## The queue' section")
else:
    for line in m.group(1).split("\n"):
        c = [x.strip() for x in line.strip().strip("|").split("|")]
        if len(c) == 4 and re.fullmatch(r"[A-GT][1-9][0-9]*", c[0]):
            qrows += 1
            ids = re.findall(r"R-\d{4}", c[2])
            if not ids: bad.append(f"queue row {c[0]} names no task")
            for i in ids:
                if i not in tasks: bad.append(f"queue row {c[0]} names {i}, which is not a task in {roadmap}")
    if qrows == 0: bad.append(f"{roadmap}: the queue has no rows")

# 6. structural owners of bug classes
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

# 7. links
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
# 8. candidate bugs are not already fixed
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
print(f"CHECKED tasks={len(tasks)} rows={len(rows)} queue={qrows}")
sys.exit(1 if bad else 0)
PY
}

echo "=== controls: the check on edited copies ==="
# control NAME PATTERN: copy the inputs into $TMP/NAME, apply the python edit in $2 (with r, t, c
# bound to the roadmap, table, classes and candidate-record texts), and expect a refusal whose output matches $3.
control() {
  local d="$TMP/$1"
  mkdir -p "$d"
  cp "$ROADMAP" "$d/ROADMAP.md"; cp "$TABLE" "$d/t.tsv"; cp "$CLASSES" "$d/c.md"; cp "$CANDIDATES" "$d/k.tsv"
  if ! python3 -I - "$d" "$2" <<'PY'
import sys
d, edit = sys.argv[1], sys.argv[2]
r = open(f"{d}/ROADMAP.md").read(); t = open(f"{d}/t.tsv").read(); c = open(f"{d}/c.md").read(); k = open(f"{d}/k.tsv").read()
o = (r, t, c, k)
exec(edit)
assert (r, t, c, k) != o, "the edit changed nothing"
open(f"{d}/ROADMAP.md", "w").write(r); open(f"{d}/t.tsv", "w").write(t); open(f"{d}/c.md", "w").write(c); open(f"{d}/k.tsv", "w").write(k)
PY
  then no "control $1: the edit did not apply"; return; fi
  local out rc=0
  out="$(check "$d/ROADMAP.md" "$d/t.tsv" "$d/c.md" "$ROOT_DIR" "$d/k.tsv" 2>&1)" || rc=$?
  if [ "$rc" = 1 ] && has_match "$3" <<<"$out"; then ok "control $1 is refused"
  else no "control $1: exit $rc, expected 1 with /$3/"; printf '%s\n' "$out" | awk 'NR<=5'; fi
}
# An unedited copy passes: every refusal below is caused by its edit alone.
d="$TMP/clean"; mkdir -p "$d"; cp "$ROADMAP" "$d/ROADMAP.md"; cp "$TABLE" "$d/t.tsv"; cp "$CLASSES" "$d/c.md"; cp "$CANDIDATES" "$d/k.tsv"
if check "$d/ROADMAP.md" "$d/t.tsv" "$d/c.md" "$ROOT_DIR" "$d/k.tsv" > "$TMP/clean.out" 2>&1; then ok "positive control: an unedited copy passes"
else no "positive control: an unedited copy is refused"; awk 'NR<=5' "$TMP/clean.out"; fi
# A copy of a roadmap row, a second time.
control duplicate_task "import re; m=re.search(r'^\| R-0496 \|.*$', r, re.M); r=r.replace(m.group(0), m.group(0)+'\n'+m.group(0), 1)" "R-0496 declared twice"
# A transferred ID reused as a live task.
control reused_archived_id "r=r.replace('| R-0496 |', '| R-0010 | reused | x | — |\n| R-0496 |', 1)" "R-0010: reuses an archived ID"
# A never-archived ID below the archive's top (R-0457 was never allocated).
control new_id_below_top "r=r.replace('| R-0496 |', '| R-0457 | squatter | x | — |\n| R-0496 |', 1)" "R-0457: not in"
# A retained task dropped from the roadmap.
control retained_dropped "import re; r=re.sub(r'^\| R-0507 \|.*\n', '', r, count=1, flags=re.M)" "R-0507: retained in .* neither"
# A queue row naming a task that does not exist.
control queue_unknown "r=r.replace('| R-0553, R-0562,', '| R-0553, R-0999, R-0562,', 1)" "names R-0999"
# A transfer to a task that is not in the roadmap.
control dangling_transfer "import re; t=re.sub(r'^(R-0010\t[^\t]*\ttransferred-with-obligations\t[^\t]*\t)R-0561', r'\1R-0010', t, count=1, flags=re.M)" "R-0010: transferred to R-0010"
# A deferral without its trigger.
control deferral_without_trigger "import re; t=re.sub(r'^(R-0012\t[^\t]*\tdeferred-with-trigger\t[^\t]*\t[^\t]*\t[^\t]*\t)[^\t]+', r'\1', t, count=1, flags=re.M)" "R-0012: deferred without a trigger"
# A bug class owned by a retired task.
control retired_class_owner "c=c+'\nOwner: R-0518.\n'" "names R-0518 as an owner"
# A broken link in the roadmap.
control broken_link "r=r.replace('(docs/compiler/CORE_PROPOSAL.md)', '(docs/compiler/NO_SUCH_FILE.md)', 1)" "missing link target"
# A broken anchor in the roadmap.
control broken_anchor "r=r.replace('ROADMAP_DECISIONS.md#2026-11-01--compiler-proof-order', 'ROADMAP_DECISIONS.md#no-such-anchor', 1)" "no anchor #no-such-anchor"
# A retained task removed from the roadmap (and its queue row) counts as kept only when the changelog
# completes it: the removal passes once a fragment says `### Completed Task R-0507`.
d="$TMP/completed"; mkdir -p "$d"; cp "$ROADMAP" "$d/ROADMAP.md"; cp "$TABLE" "$d/t.tsv"; cp "$CLASSES" "$d/c.md"; cp "$CANDIDATES" "$d/k.tsv"
python3 -I -c 'import re,sys; p=sys.argv[1]; s=open(p).read(); s,n=re.subn(r"^\| R-0507 \|.*\n","",s,count=1,flags=re.M); assert n==1; assert s.count("R-0504, R-0507, ")==1; s=s.replace("R-0504, R-0507, ","R-0504, "); open(p,"w").write(s)' "$d/ROADMAP.md"
cp "$CHANGELOG" "$d/changelog.md"; printf '\n### Completed Task R-0507\n\ncontrol\n' >> "$d/changelog.md"
if CHANGELOG="$d/changelog.md" check "$d/ROADMAP.md" "$d/t.tsv" "$d/c.md" "$ROOT_DIR" "$d/k.tsv" > "$TMP/completed.out" 2>&1; then
  ok "positive control: a retained task completed in the changelog may leave the roadmap"
else no "positive control: a completed retained task was refused"; awk 'NR<=5' "$TMP/completed.out"; fi
# ...and a completed task that is still listed is refused.
cp "$ROADMAP" "$d/ROADMAP.md"
rc=0; CHANGELOG="$d/changelog.md" check "$d/ROADMAP.md" "$d/t.tsv" "$d/c.md" "$ROOT_DIR" "$d/k.tsv" > "$TMP/completed2.out" 2>&1 || rc=$?
if [ "$rc" = 1 ] && has_match "R-0507: completed in the changelog but still a task" < "$TMP/completed2.out"; then ok "control completed_but_listed is refused"
else no "control completed_but_listed: exit $rc"; fi
# A candidate naming a bug the ledger records fixed (102 is FIXED).
control candidate_bug_fixed "import re; k=re.sub(r'^(rocksdb-slice1\t)[^\t]*', r'\g<1>102', k, count=1, flags=re.M)" "bug 102 is recorded fixed"

echo "=== the repository ==="
rc=0; out="$(check "$ROADMAP" "$TABLE" "$CLASSES" "$ROOT_DIR" "$CANDIDATES" "$README" 2>&1)" || rc=$?
printf '%s\n' "$out"
if [ "$rc" = 0 ]; then ok "ROADMAP.md, the disposition table and the bug-class owners agree"
else no "ROADMAP.md, the disposition table or the bug-class owners disagree (exit $rc)"; fi

echo ""
echo "ROADMAP: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
