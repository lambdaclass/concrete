#!/usr/bin/env bash
# EVERY ROADMAP TASK HAS A PLACE IN THE PLAN, AND THE QUEUE AGREES WITH IT.
#
# The 2026-10-19 reconciliation found tasks that sat in a phase whose stage did not describe them,
# superseded tasks that still read as live, and open work stated only in prose. Its repair gave every
# `### Task R-NNNN` a **Stage:** line, recorded every non-live task in the reconciliation ledger with
# the owner that replaces it, and named the owning task in every queue row. This gate keeps that true:
#
#   1. every task has a **Stage:** line naming a stage (1-7 or after-1.0/research), or a ledger row,
#      or a **Disposition** line giving its retirement/merge reason;
#   2. every row of the task-disposition table in docs/project/ROADMAP_RECONCILIATION.md (the moved
#      ledger, linked from ROADMAP's "Reconciliation record") names an existing task (ROADMAP heading
#      or CHANGELOG `Completed Task`), and every owner it names exists;
#   3. every queue row names at least one owning task, and every task it names exists, ranges such
#      as `R-0491–R-0510` included;
#   4. a task named by a queue row lists that row's stage (from "Roadmap at a glance") in its Stage
#      line — a task cannot be scheduled by the queue in a stage its own record contradicts.
#
# Run with no argument it checks ROADMAP.md with the reconciliation record and then runs its negative
# controls against temporary copies: a removed Stage line, a bogus ledger id, a queue row naming a missing task or no task, and a
# stage that contradicts its queue row must each fail; a crashed or silent checker must fail too.
# With arguments `ROADMAP [LEDGER]` it checks those files only.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

TARGET="${1:-ROADMAP.md}"
LEDGER="${2:-docs/project/ROADMAP_RECONCILIATION.md}"
CHANGELOG="CHANGELOG.md"
[ -f "$TARGET" ] || { echo "FATAL: $TARGET missing" >&2; exit 2; }
[ -f "$LEDGER" ] || { echo "FATAL: $LEDGER missing" >&2; exit 2; }

check_file() { # roadmap ledger -> prints "OK <msg>" / "FAIL <msg>" lines
  python3 - "$1" "$CHANGELOG" "$2" <<'PY'
import re, sys
road = open(sys.argv[1], encoding='utf-8').read()
chg = open(sys.argv[2], encoding='utf-8').read()
rec = open(sys.argv[3], encoding='utf-8').read()
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

def stages_of(body):
    m = re.search(r'^\*\*Stage:\*\* (.*)$', body, re.M)
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
    st = stages_of(b)
    has_stage = st is not None and (st[0] or st[1])
    has_disp = re.search(r'^\*\*Disposition[^*]*\*\*.*(retire|merge|supersed|delegat|done|duplicate)', b, re.M | re.I)
    if not (has_stage or tid in ledger or has_disp):
        no(f'{tid} has no stage assignment, ledger disposition or retirement/merge reason'); unplaced += 1
if not unplaced: ok(f'all {len(active)} tasks have a stage or a recorded disposition')

# --- queue rows and stage map
gm = re.search(r'^## Roadmap at a glance$', road, re.M)
row_stage = {}
if gm:
    for line in road[gm.end():].splitlines():
        if line.startswith('## '): break
        m = re.match(r'^\| \*\*(\d)\. [^|]*\*\* \| ([^|]+) \|', line)
        if m:
            spec = m.group(2)
            for a, b in re.findall(r'(\d+)\s*[–-]\s*(\d+)', spec):
                for k in range(int(a), int(b) + 1): row_stage[k] = int(m.group(1))
            for k in re.findall(r'(?<![\d–-])(\d+)(?!\s*[–-]|\d)', spec): row_stage.setdefault(int(k), int(m.group(1)))
if not row_stage: no('could not read the queue-row-to-stage map from "Roadmap at a glance"')
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
    stage = row_stage.get(order)
    if stage is None:
        no(f'queue row {order} has no stage in "Roadmap at a glance"'); qbad += 1; continue
    tids = expand(work)
    if not tids:
        no(f'queue row {order} names no owning task')
        qbad += 1
        continue
    for tid in tids:
        named += 1
        if tid not in active:
            no(f'queue row {order} names nonexistent task {tid}'); qbad += 1; continue
        st = stages_of(bodies[tid])
        if st is None or stage not in st[0]:
            have = sorted(st[0]) if st else 'no Stage line'
            no(f'{tid}: queue row {order} schedules it in stage {stage}, its Stage line says {have}'); qbad += 1
if qrows and not qbad: ok(f'{len(qrows)} queue rows name {named} existing tasks, each staged consistently')
print('\n'.join(out))
PY
}

run_on() { # roadmap [ledger] -> sets RESULT lines, returns 0 iff the checker ran, reported, and found no FAIL
  if ! RESULT="$(check_file "$1" "${2:-$LEDGER}")"; then
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
  neg() { # name, python-edit, expected-substring[, "ledger" to mutate the reconciliation record]
    local f="$TMPD/$1.md" l="$TMPD/$1.ledger.md" src="$TARGET" dst
    cp "$TARGET" "$f"; cp "$LEDGER" "$l"
    if [ "${4:-}" = ledger ]; then src="$LEDGER"; dst="$l"; else dst="$f"; fi
    python3 - "$src" "$dst" "$2" <<'PY'
import sys, re
s = open(sys.argv[1], encoding='utf-8').read()
exec(sys.argv[3])
open(sys.argv[2], 'w', encoding='utf-8').write(s)
PY
    if [ "$?" -ne 0 ]; then no "control $1: could not build its mutated copy"; return; fi
    if run_on "$f" "$l"; then
      no "control $1: the mutated roadmap passed"
    elif printf '%s\n' "$RESULT" | grep -q -- "$3"; then
      ok "control $1 is rejected ($3)"
    else
      no "control $1 failed for an unexpected reason"
    fi
  }
  neg removed_stage \
    "h='### Task R-0494\n\n'; i=s.index(h)+len(h); j=s.index('\n\n',i); assert s[i:j].startswith('**Stage:**'); s=s[:i]+s[j+2:]" \
    "R-0494 has no stage assignment"
  neg removed_stage_unscheduled \
    "h='### Task R-0522\n\n'; i=s.index(h)+len(h); j=s.index('\n\n',i); assert s[i:j].startswith('**Stage:**'); s=s[:i]+s[j+2:]" \
    "R-0522 has no stage assignment"
  neg bogus_ledger_id \
    "h='|---|---|---|---|\n| R-'; i=s.index(h)+len('|---|---|---|---|\n'); s=s[:i]+'| R-9999 | merged | R-0494 | control |\n'+s[i:]" \
    "ledger row names missing task R-9999" ledger
  neg queue_missing_task \
    "old='| 9 | **Compiler provenance and canonical \`VerificationTask\` (R-0521)** |'; assert old in s; s=s.replace(old,'| 9 | **Compiler provenance and canonical \`VerificationTask\` (R-9998)** |')" \
    "names nonexistent task R-9998"
  neg stage_contradicts_queue \
    "h='### Task R-0511\n\n**Stage:** 2'; assert h in s; s=s.replace(h,'### Task R-0511\n\n**Stage:** 6')" \
    "R-0511: queue row 3 schedules it in stage 2"
  neg queue_row_no_owner \
    "old='| 9 | **Compiler provenance and canonical \`VerificationTask\` (R-0521)** |'; assert old in s; s=s.replace(old,'| 9 | **Compiler provenance and canonical \`VerificationTask\`** |')" \
    "queue row 9 names no owning task"
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
