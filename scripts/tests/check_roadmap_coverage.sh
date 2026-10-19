#!/usr/bin/env bash
# EVERY ROADMAP TASK HAS A PLACE IN THE PLAN, AND THE QUEUE AGREES WITH IT.
#
# The 2026-10-19 reconciliation found tasks filed in an area whose schedule did not describe them,
# superseded tasks that still read as live, and open work stated only in prose. Its repair gave every
# `### Task R-NNNN` a **Phase:** line (the execution phases 1-7 were called "stages" until the same
# day's rename), recorded every non-live task in the reconciliation ledger with the owner that
# replaces it, and named the owning task in every queue row. This gate keeps that true:
#
#   1. every task has a **Phase:** line naming a phase (1-7 or after-1.0/research), or a ledger row,
#      or a **Disposition** line giving its retirement/merge reason;
#   2. every row of the task-disposition table in docs/project/ROADMAP_RECONCILIATION.md (the moved
#      ledger, linked from ROADMAP's "Reconciliation record") names an existing task (ROADMAP heading
#      or CHANGELOG `Completed Task`), and every owner it names exists;
#   3. every queue row names at least one owning task, and every task it names exists, ranges such
#      as `R-0491–R-0510` included;
#   4. a task named by a queue row lists that row's phase (from the "queue rows" column of "Phases and
#      releases") in its Phase line — the queue cannot schedule a task in a phase its own record
#      contradicts;
#   5. every task body archived in docs/archive/ROADMAP_RETIRED_TASKS.md (a bare `## R-NNNN` section)
#      has its `### Task R-NNNN` stub in ROADMAP with a **Disposition** line and a link to that
#      section, and every such link resolves to its own task's archived section.
#   6. every transfer the ledger records (followed through superseded owners to a live one), plus the
#      three explicit residuals of done tasks, appears as an entry in that live owner's
#      **Inherited obligations** block, and no block lists a transfer the ledger does not record.
#
# Run with no argument it checks ROADMAP.md with the reconciliation record and then runs its negative
# controls against temporary copies: a removed Phase line, a bogus ledger id, a queue row naming a missing task or no task, a
# phase that contradicts its queue row, an archived body without its stub, a stub without its archived body,
# a removed inheritance entry and a fabricated one must each fail; a crashed or silent checker must fail too.
# With arguments `ROADMAP [LEDGER [ARCHIVE]]` it checks those files only.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

TARGET="${1:-ROADMAP.md}"
LEDGER="${2:-docs/project/ROADMAP_RECONCILIATION.md}"
ARCHIVE="${3:-docs/archive/ROADMAP_RETIRED_TASKS.md}"
CHANGELOG="CHANGELOG.md"
[ -f "$TARGET" ] || { echo "FATAL: $TARGET missing" >&2; exit 2; }
[ -f "$LEDGER" ] || { echo "FATAL: $LEDGER missing" >&2; exit 2; }
[ -f "$ARCHIVE" ] || { echo "FATAL: $ARCHIVE missing" >&2; exit 2; }

check_file() { # roadmap ledger archive -> prints "OK <msg>" / "FAIL <msg>" lines
  python3 - "$1" "$CHANGELOG" "$2" "$3" <<'PY'
import re, sys
road = open(sys.argv[1], encoding='utf-8').read()
chg = open(sys.argv[2], encoding='utf-8').read()
rec = open(sys.argv[3], encoding='utf-8').read()
arc = open(sys.argv[4], encoding='utf-8').read()
out = []
def ok(m): out.append('OK ' + m)
def no(m): out.append('FAIL ' + m)

def expand(text):
    ids = []
    for a, b in re.findall(r'R-(\d{4})\s*[–-]\s*R-(\d{4})', text):
        ids += ['R-%04d' % k for k in range(int(a), int(b) + 1)]
    ids += re.findall(r'R-\d{4}', text)
    return list(dict.fromkeys(ids))

heads = [(m.start(), m.group(1)) for m in re.finditer(r'^### Task (R-\d{4})$', road, re.M)]
active = [h for _, h in heads]
completed = set(re.findall(r'^### Completed Task (R-\d{4})$', chg, re.M))
known = set(active) | completed
if not active: no('no task headings found'); print('\n'.join(out)); sys.exit()

bodies = {}
for pos, tid in heads:
    m = re.compile(r'^#{2,3} ', re.M).search(road, pos + 1)
    bodies[tid] = road[pos:(m.start() if m else len(road))]

def phases_of(body):
    m = re.search(r'^\*\*Phase:\*\* (.*)$', body, re.M)
    if not m: return None
    head = m.group(1).split(' — ')[0]
    st = set(int(d) for d in re.findall(r'(?<![\d.])([1-7])(?![\d.])', head))
    research = 'research' in head
    return st, research

# --- ledger (moved out of ROADMAP into the reconciliation record)
ledger = {}
if not re.search(r'^## Reconciliation record$', road, re.M) or 'docs/project/ROADMAP_RECONCILIATION.md' not in road:
    no('ROADMAP has no "## Reconciliation record" section linking docs/project/ROADMAP_RECONCILIATION.md')
if re.search(r'^#+ Task R-\d{4}$', rec, re.M):
    no('the reconciliation record declares task headings; task IDs belong to ROADMAP/CHANGELOG only')
if True:
    tm = re.search(r'^## Task dispositions$', rec, re.M)
    if not tm:
        no('reconciliation record has no "## Task dispositions" table')
    else:
        nxt = re.compile(r'^## ', re.M).search(rec, tm.end())
        for line in rec[tm.end():(nxt.start() if nxt else len(rec))].splitlines():
            cells = [c.strip() for c in line.strip().strip('|').split('|')]
            if len(cells) >= 4 and re.fullmatch(r'R-\d{4}', cells[0]):
                ledger[cells[0]] = cells
        if not ledger: no('ledger task table is empty')
bad = 0
for tid, cells in ledger.items():
    if tid not in known:
        no(f'ledger row names missing task {tid}'); bad += 1
    for owner in re.findall(r'R-\d{4}', cells[2]):
        if owner not in known:
            no(f'ledger row {tid} names missing owner {owner}'); bad += 1
if ledger and not bad: ok(f'{len(ledger)} ledger rows name existing tasks and owners')

# --- every task placed
unplaced = 0
for tid in active:
    b = bodies[tid]
    st = phases_of(b)
    has_phase = st is not None and (st[0] or st[1])
    has_disp = re.search(r'^\*\*Disposition[^*]*\*\*.*(retire|merge|supersed|delegat|done|duplicate)', b, re.M | re.I)
    if not (has_phase or tid in ledger or has_disp):
        no(f'{tid} has no phase assignment, ledger disposition or retirement/merge reason'); unplaced += 1
if not unplaced: ok(f'all {len(active)} tasks have a phase or a recorded disposition')

# --- queue rows and phase map ("Phases and releases": one row per phase, a "queue rows" column)
gm = re.search(r'^## Phases and releases$', road, re.M)
row_phase = {}
if gm:
    lines = road[gm.end():].split('\n')
    end = next((k for k, l in enumerate(lines) if l.startswith('## ')), len(lines))
    col = None
    for line in lines[:end]:
        cells = [c.strip() for c in line.strip().strip('|').split('|')] if line.startswith('|') else []
        if cells and 'queue rows' in cells:
            col = cells.index('queue rows'); continue
        m = re.match(r'^\*\*Phase (\d)\b', cells[0]) if cells else None
        if m and col is not None and col < len(cells):
            spec = cells[col]; p = int(m.group(1))
            for a, b in re.findall(r'(\d+)\s*[–-]\s*(\d+)', spec):
                for k in range(int(a), int(b) + 1): row_phase[k] = p
            for k in re.findall(r'(?<![\d–-])(\d+)(?!\s*[–-]|\d)', spec): row_phase.setdefault(int(k), p)
if not row_phase: no('could not read the queue-row-to-phase map from "Phases and releases"')
qm = re.search(r'^## The current execution queue$', road, re.M)
qrows = []
if qm:
    for line in road[qm.end():].splitlines():
        if line.startswith('## '): break
        m = re.match(r'^\| (\d+) \| (.*?) \| (.*)\|$', line)
        if m: qrows.append((int(m.group(1)), m.group(2)))
if not qrows: no('no queue rows found')
qbad = 0; named = 0
for order, work in qrows:
    phase = row_phase.get(order)
    if phase is None:
        no(f'queue row {order} has no phase in "Phases and releases"'); qbad += 1; continue
    tids = expand(work)
    if not tids:
        no(f'queue row {order} names no owning task')
        qbad += 1
        continue
    for tid in tids:
        named += 1
        if tid not in active:
            no(f'queue row {order} names nonexistent task {tid}'); qbad += 1; continue
        st = phases_of(bodies[tid])
        if st is None or phase not in st[0]:
            have = sorted(st[0]) if st else 'no Phase line'
            no(f'{tid}: queue row {order} schedules it in Phase {phase}, its Phase line says {have}'); qbad += 1
if qrows and not qbad: ok(f'{len(qrows)} queue rows name {named} existing tasks, each phased consistently')

# --- archived task bodies <-> roadmap stubs
sections = re.findall(r'^## (R-\d{4})$', arc, re.M)
abad = 0
if not sections:
    no('the retired-task archive has no `## R-NNNN` sections'); abad += 1
for tid in sorted(set(t for t in sections if sections.count(t) > 1)):
    no(f'archived body {tid} appears more than once'); abad += 1
# A stub names its own archived body in one fixed sentence; any other link into the archive (an
# owner's inherited-obligation list) must still resolve to an existing section.
STUB = re.compile(r"^The rest of this task's original body is archived in \[ROADMAP_RETIRED_TASKS\.md\]"
                  r"\(docs/archive/ROADMAP_RETIRED_TASKS\.md#(r-\d{4})\)\.$", re.M)
stubs = {}
for tid in active:
    for target in STUB.findall(bodies[tid]):
        stubs.setdefault(tid, set()).add(target.upper())
for tid in dict.fromkeys(sections):
    if tid not in stubs:
        no(f'archived body {tid} has no stub in ROADMAP linking it'); abad += 1
    elif not re.search(r'^\*\*Disposition', bodies[tid], re.M):
        no(f'archived body {tid}: its ROADMAP stub has no Disposition line'); abad += 1
for tid, targets in stubs.items():
    for target in targets:
        if target != tid:
            no(f'{tid}: its stub sentence names the archived body of {target}'); abad += 1
        elif target not in sections:
            no(f'{tid} links an archived body that does not exist'); abad += 1
for tid in active:
    for target in set(re.findall(r'ROADMAP_RETIRED_TASKS\.md#(r-\d{4})', bodies[tid])):
        if target.upper() not in sections and target.upper() != tid:
            no(f'{tid} links the archived body of {target.upper()}, which does not exist'); abad += 1
if sections and not abad: ok(f'{len(set(sections))} archived task bodies each have a linked stub with a disposition, and every stub link resolves')

# --- inherited obligations: every transfer the ledger records appears in its live owner, and no other
def live_owners(tid, seen=frozenset()):
    if tid in seen:
        no(f'ownership cycle involving {tid}')
        return set()
    owners = expand(ledger[tid][2]) if tid in ledger else []
    if not owners:
        return {tid}
    return set().union(*(live_owners(o, seen | {tid}) for o in owners))

expected = {
    (tid, owner)
    for tid in ledger
    for owner in live_owners(tid)
    if tid != owner
}
# Explicit residual obligations of completed tasks.
expected |= {
    ('R-0208', 'R-0532'),
    ('R-0465', 'R-0494'),
    ('R-0484', 'R-0530'),
}
actual = set()
for owner, body in bodies.items():
    if '**Inherited obligations' in body:
        inherited = body.split('**Inherited obligations', 1)[1]
        actual.update((tid, owner) for tid in
                      re.findall(r'^- \*\*(R-\d{4})\*\*', inherited, re.M))
ibad = 0
for tid, owner in sorted(expected - actual):
    no(f'{owner} lacks inherited obligations from {tid}'); ibad += 1
for tid, owner in sorted(actual - expected):
    no(f'{owner} has unexplained inherited obligations from {tid}'); ibad += 1
if not ibad: ok(f'{len(actual)} inherited-obligation entries match the ledger transfers exactly')
print('\n'.join(out))
PY
}

run_on() { # roadmap [ledger [archive]] -> sets RESULT lines, returns 0 iff the checker ran, reported, and found no FAIL
  if ! RESULT="$(check_file "$1" "${2:-$LEDGER}" "${3:-$ARCHIVE}")"; then
    RESULT="FAIL checker execution failed for $1"
    return 1
  fi
  if [ -z "$RESULT" ]; then
    RESULT="FAIL checker produced no results for $1"
    return 1
  fi
  ! printf '%s\n' "$RESULT" | grep '^FAIL ' >/dev/null
}

echo "=== 1. roadmap task coverage ($TARGET) ==="
run_on "$TARGET"
while IFS= read -r line; do
  case "$line" in
    OK\ *) ok "${line#OK }" ;;
    FAIL\ *) no "${line#FAIL }" ;;
  esac
done <<< "$RESULT"

if [ $# -eq 0 ]; then
  echo "=== 2. negative controls ==="
  TMPD="$(mktemp -d)"; trap 'rm -rf "$TMPD"' EXIT
  neg() { # name, python-edit, expected-substring[, "ledger"/"archive" to mutate that file instead]
    local f="$TMPD/$1.md" l="$TMPD/$1.ledger.md" a="$TMPD/$1.archive.md" src="$TARGET" dst
    cp "$TARGET" "$f"; cp "$LEDGER" "$l"; cp "$ARCHIVE" "$a"
    case "${4:-}" in
      ledger) src="$LEDGER"; dst="$l" ;;
      archive) src="$ARCHIVE"; dst="$a" ;;
      *) dst="$f" ;;
    esac
    python3 - "$src" "$dst" "$2" <<'PY'
import sys, re
s = open(sys.argv[1], encoding='utf-8').read()
exec(sys.argv[3])
open(sys.argv[2], 'w', encoding='utf-8').write(s)
PY
    if [ "$?" -ne 0 ]; then no "control $1: could not build its mutated copy"; return; fi
    if run_on "$f" "$l" "$a"; then
      no "control $1: the mutated roadmap passed"
    elif printf '%s\n' "$RESULT" | grep -q -- "$3"; then
      ok "control $1 is rejected ($3)"
    else
      no "control $1 failed for an unexpected reason"
    fi
  }
  neg removed_phase \
    "h='### Task R-0494\n\n'; i=s.index(h)+len(h); j=s.index('\n\n',i); assert s[i:j].startswith('**Phase:**'); s=s[:i]+s[j+2:]" \
    "R-0494 has no phase assignment"
  neg removed_phase_unscheduled \
    "h='### Task R-0522\n\n'; i=s.index(h)+len(h); j=s.index('\n\n',i); assert s[i:j].startswith('**Phase:**'); s=s[:i]+s[j+2:]" \
    "R-0522 has no phase assignment"
  neg bogus_ledger_id \
    "h='|---|---|---|---|\n| R-'; i=s.index(h)+len('|---|---|---|---|\n'); s=s[:i]+'| R-9999 | merged | R-0494 | control |\n'+s[i:]" \
    "ledger row names missing task R-9999" ledger
  neg queue_missing_task \
    "old='| 9 | **Compiler provenance and canonical \`VerificationTask\` (R-0521)** |'; assert old in s; s=s.replace(old,'| 9 | **Compiler provenance and canonical \`VerificationTask\` (R-9998)** |')" \
    "names nonexistent task R-9998"
  neg phase_contradicts_queue \
    "h='### Task R-0511\n\n**Phase:** 2'; assert h in s; s=s.replace(h,'### Task R-0511\n\n**Phase:** 6')" \
    "R-0511: queue row 3 schedules it in Phase 2"
  neg phase_table_row_moved \
    "old='| 3 – 4 | V1a and V1b'; assert old in s; s=s.replace(old,'| 4 | V1a and V1b')" \
    "queue row 3 has no phase"
  neg queue_row_no_owner \
    "old='| 9 | **Compiler provenance and canonical \`VerificationTask\` (R-0521)** |'; assert old in s; s=s.replace(old,'| 9 | **Compiler provenance and canonical \`VerificationTask\`** |')" \
    "queue row 9 names no owning task"
  neg archived_body_without_stub \
    "s=s.rstrip('\n')+'\n\n## R-0491\n\nAn archived body whose task is live and links nothing here.\n'" \
    "archived body R-0491 has no stub" archive
  neg stub_without_archived_body \
    "i=s.index('\n## R-0007\n'); j=s.index('\n## ',i+1); s=s[:i]+s[j:]" \
    "R-0007 links an archived body that does not exist" archive
  neg stub_names_other_body \
    "old='archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0007).'; assert old in s; s=s.replace(old,'archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0019).')" \
    "R-0007: its stub sentence names the archived body of R-0019"
  neg stub_link_removed \
    "old='archived in [ROADMAP_RETIRED_TASKS.md](docs/archive/ROADMAP_RETIRED_TASKS.md#r-0007).'; assert old in s; s=s.replace(old,'archived.')" \
    "archived body R-0007 has no stub"
  neg inheritance_entry_removed \
    "import re; m=re.search(r'^- \*\*R-0007\*\* \(superseded\)[^\n]*\n', s, re.M); assert m; i=s.index('### Task R-0492\n'); assert m.start()>i; s=s[:m.start()]+s[m.end():]" \
    "R-0492 lacks inherited obligations from R-0007"
  neg inheritance_entry_fabricated \
    "old='- **R-0008** (superseded):'; assert s.count(old)==1; s=s.replace(old,'- **R-0019** (merged): fabricated transfer.\n'+old)" \
    "R-0492 has unexplained inherited obligations from R-0019"
  # The wrapper itself must not read a crashed or silent checker as a pass.
  if ( check_file() { return 17; }; run_on "$TARGET" ); then
    no "control checker_exit_failure: a nonzero checker exit passed"
  else
    ok "control checker_exit_failure is rejected (checker execution failed)"
  fi
  if ( check_file() { :; }; run_on "$TARGET" ); then
    no "control checker_empty_output: a checker with no output passed"
  else
    ok "control checker_empty_output is rejected (checker produced no results)"
  fi
fi

echo ""
echo "ROADMAP-COVERAGE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
