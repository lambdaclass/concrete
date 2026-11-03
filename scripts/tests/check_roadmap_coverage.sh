#!/usr/bin/env bash
# EVERY ROADMAP TASK HAS A PLACE IN THE PLAN, AND THE QUEUE AGREES WITH IT.
# Checks 1–10 and 13: task identity, phases, reconciliation transfers, archive/stub parity,
# placement, pull reviews, queue ownership/acceptance and candidate integrity. Legacy exits,
# feature prerequisites and capacity shares are historical and are not scheduling inputs.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

TARGET="${1:-ROADMAP.md}"
LEDGER="${2:-docs/project/ROADMAP_RECONCILIATION.md}"
ARCHIVE="${3:-docs/archive/ROADMAP_RETIRED_TASKS.md}"
BACKLOG="${4:-docs/project/ROADMAP_BACKLOG.md}"
TASKSF="${5:-docs/project/ROADMAP_TASKS.md}"
# The changelog as it reads with the pending changelog.d/ fragments folded in: a branch retires a task
# with a `### Completed Task R-NNNN` heading in its fragment, and that counts before the fold.
CHANGELOG="$(mktemp)"
python3 -I scripts/gen/changelog.py --print > "$CHANGELOG" || { echo "FATAL: scripts/gen/changelog.py --print failed" >&2; rm -f "$CHANGELOG"; exit 2; }
[ -f "$TARGET" ] || { echo "FATAL: $TARGET missing" >&2; exit 2; }
[ -f "$LEDGER" ] || { echo "FATAL: $LEDGER missing" >&2; exit 2; }
[ -f "$ARCHIVE" ] || { echo "FATAL: $ARCHIVE missing" >&2; exit 2; }
[ -f "$BACKLOG" ] || { echo "FATAL: $BACKLOG missing" >&2; exit 2; }
[ -f "$TASKSF" ] || { echo "FATAL: $TASKSF missing" >&2; exit 2; }

check_file() { # roadmap ledger archive backlog tasks -> prints "OK <msg>" / "FAIL <msg>" lines
  python3 - "$1" "$CHANGELOG" "$2" "$3" "$4" "$5" <<'PY'
import re, sys
plane = open(sys.argv[1], encoding='utf-8').read()
chg = open(sys.argv[2], encoding='utf-8').read()
rec = open(sys.argv[3], encoding='utf-8').read()
arc = open(sys.argv[4], encoding='utf-8').read()
back = open(sys.argv[5], encoding='utf-8').read()
reg = open(sys.argv[6], encoding='utf-8').read()
out = []
def ok(m): out.append('OK ' + m)
def no(m): out.append('FAIL ' + m)
# ROADMAP.md is the control plane and the task register holds the task bodies. Neither may take over
# the other's part.
PLANE_SECTIONS = ('Phases', 'The queue', 'Implemented, not yet accepted', 'Owner decisions',
                  'Reconciliation record', 'Release map')
for t in re.findall(r'^#+ (?:Completed )?Task (R-\d{4})$', plane, re.M):
    no(f'ROADMAP.md declares task {t}; task bodies live in the task register (docs/project/ROADMAP_TASKS.md)')
for h in PLANE_SECTIONS:
    if re.search(r'^#+ ' + re.escape(h) + r'$', reg, re.M):
        no(f'the task register declares the control-plane section "{h}", which belongs to ROADMAP.md')
for label in ('Active phases', 'Next phases'):
    if re.search(r'^\*\*' + label + r':\*\*', reg, re.M):
        no(f'the task register declares "**{label}:**", which belongs to ROADMAP.md')
# Every control-plane section, table and line below is read from ROADMAP.md alone; only the task
# bodies are read from the register.
road = plane

def expand(text):
    ids = []
    for a, b in re.findall(r'R-(\d{4})\s*[–-]\s*R-(\d{4})', text):
        ids += ['R-%04d' % k for k in range(int(a), int(b) + 1)]
    ids += re.findall(r'R-\d{4}', text)
    return list(dict.fromkeys(ids))

def task_bodies(text):
    found = []
    for h in re.finditer(r'^### Task (R-\d{4})$', text, re.M):
        m = re.compile(r'^#{2,3} ', re.M).search(text, h.start() + 1)
        found.append((h.group(1), text[h.start():(m.start() if m else len(text))]))
    return found

road_tasks = task_bodies(reg)
back_tasks = task_bodies(back)
completed = set(re.findall(r'^### Completed Task (R-\d{4})$', chg, re.M))
if not road_tasks: no('no task headings found'); print('\n'.join(out)); sys.exit()
if not back_tasks: no('the backlog declares no task headings')

# --- one live home per task (ROADMAP.md or the backlog), and no ID lost
home = {}
hbad = 0
for where, tasks in (('ROADMAP_TASKS.md', road_tasks), ('the backlog', back_tasks)):
    for tid, _ in tasks:
        if tid in home:
            no(f'{tid} is declared in both {home[tid]} and {where}' if home[tid] != where
               else f'{tid} is declared twice in {where}'); hbad += 1
        else:
            home[tid] = where
for tid in sorted(completed & set(home)):
    no(f'{tid} is both a live task and a CHANGELOG Completed Task'); hbad += 1
# IDs are allocated in sequence and never reused; these two were never allocated.
NEVER_ALLOCATED = {'R-0457', 'R-0489'}
declared = set(home) | completed
top = max(int(t[2:]) for t in declared)
for k in range(1, top + 1):
    tid = 'R-%04d' % k
    if tid not in declared and tid not in NEVER_ALLOCATED:
        no(f'{tid} is lost: declared in neither the task register, the backlog nor CHANGELOG'); hbad += 1
for tid in sorted(NEVER_ALLOCATED & declared):
    no(f'{tid} is recorded as never allocated but is declared'); hbad += 1
if not hbad:
    ok(f'{len(home)} live tasks ({len(road_tasks)} in the task register, {len(back_tasks)} in the backlog) each have one home; R-0001..R-{top:04d} are all accounted for')

active = list(home)
known = set(active) | completed
bodies = {}
for tid, body in road_tasks + back_tasks:
    bodies.setdefault(tid, body)

def phases_of(body):
    m = re.search(r'^\*\*Phase:\*\* (.*)$', body, re.M)
    if not m: return None
    head = m.group(1).split(' — ')[0]
    vals = {v.strip() for v in head.split(',')}
    allowed = set('ABCDEFGT') | {'after-1.0', 'research', 'after-1.0/research', '-'}
    if not vals <= allowed:
        no('invalid task phase: ' + head)
    return vals & set('ABCDEFGT'), bool(vals & {'after-1.0', 'research', 'after-1.0/research'})

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

# --- one queue; phase membership is encoded by its stable row ID, never duplicated in a summary.
gm = re.search(r'^## Phases$', road, re.M)
phase_rows = set()
if gm:
    section = road[gm.end():].split('\n## ', 1)[0]
    phase_rows = set(re.findall(r'^\| ([A-GT])\. ', section, re.M))
if phase_rows != set('ABCDEFGT'):
    no('Phases table must name A–G and T exactly once')
if gm and len(re.findall(r'^\| [A-GT]\. ',section,re.M)) != len(phase_rows):
    no('duplicate phase in Phases table')
qm = re.search(r'^## The queue$', road, re.M)
qrows = []; row_phase = {}; seen_rows = set()
if qm:
    for line in road[qm.end():].splitlines():
        if line.startswith('## '): break
        cells = [c.strip() for c in line.strip().strip('|').split('|')] if line.startswith('|') else []
        if not cells or cells[0] == 'row' or set(cells[0]) <= set('-:'): continue
        row = cells[0]
        if not re.fullmatch(r'[A-GT][1-9][0-9]*', row):
            no(f'invalid queue row {row}'); continue
        if row in seen_rows: no(f'duplicate queue row {row}')
        seen_rows.add(row)
        if len(cells) != 4 or not cells[1] or len(cells[3]) < 20:
            no(f'queue row {row} has no goal or done-when')
        work = cells[2] if len(cells) > 2 else ''
        qrows.append((row, work)); row_phase[row] = row[0]
        if row[0] not in phase_rows: no(f'queue row {row} names a phase absent from Phases')
if not qrows: no('no queue rows found')
else: ok(f'{len(qrows)} queue rows have a goal and done-when; their phase is read from each row ID')

def table_rows(heading, first):
    m = re.search(r'^#+ ' + re.escape(heading) + r'$', road, re.M)
    if not m: return None
    rows = []; started = False
    for line in road[m.end():].split('\n'):
        if line.startswith('#'): break
        if line.startswith('|'):
            cells = [c.strip() for c in line.strip().strip('|').split('|')]
            if cells[0] == first: started = True; continue
            if started and cells[0] and not set(cells[0]) <= set('-:'): rows.append(cells)
        elif started and rows: break
    return rows
irows = table_rows('Implemented, not yet accepted', 'candidate')
# Columns are located by header name: the table is generated from docs/project/candidates.tsv
# (scripts/gen/candidate_table.py), and a row names bugs or a deliverable ("—" for none).
ihdr = None
_im = re.search(r'^## Implemented, not yet accepted$', road, re.M)
if _im:
    for line in road[_im.end():].split('\n'):
        if line.startswith('#'): break
        if line.startswith('| candidate |'):
            ihdr = [c.strip() for c in line.strip().strip('|').split('|')]; break
icol = {}
if irows is not None:
    for want in ('bugs', 'deliverable', 'branch', 'remaining acceptance step'):
        if not ihdr or want not in ihdr:
            no(f'the "Implemented, not yet accepted" table has no "{want}" column'); irows = []
        else:
            icol[want] = ihdr.index(want)
ibad2 = 0
import os, glob
ledger_dir = os.environ.get('BUG_LEDGER_DIR', 'docs/bugs')
readme = open(os.path.join(ledger_dir, 'README.md'), encoding='utf-8').read() if os.path.exists(os.path.join(ledger_dir, 'README.md')) else ''
fm = re.search(r'^## Fixed Bugs$', readme, re.M)
if not fm:
    no(f'the bug ledger {ledger_dir}/README.md has no "## Fixed Bugs" list to check against'); ibad2 += 1
fixed_list = ''
if fm:
    nx = re.compile(r'^## ', re.M).search(readme, fm.end())
    fixed_list = readme[fm.end():(nx.start() if nx else len(readme))]
if irows is None:
    no('ROADMAP.md has no "## Implemented, not yet accepted" table (| candidate | bugs | deliverable | branch | ... | remaining acceptance step |)'); ibad2 += 1
    irows = []
seen_bug = {}
for cells in irows:
    cells = cells + [''] * (len(ihdr) - len(cells))
    bugs = re.findall(r'\b(\d{3})\b', cells[icol['bugs']])
    deliv = cells[icol['deliverable']].strip('— ')
    if not bugs and not deliv: no(f'implemented-bug row "{cells[0]}" names no bug number and no deliverable'); ibad2 += 1
    if cells[icol['bugs']] not in ('—', '-') and not bugs: no(f'implemented-bug row "{cells[0]}" has a bugs cell with no bug number'); ibad2 += 1
    if not re.findall(r'`[^`]+`', cells[icol['branch']]): no(f'implemented-bug row "{cells[0]}" names no branch'); ibad2 += 1
    if len(cells[icol['remaining acceptance step']]) < 10: no(f'implemented-bug row "{cells[0]}" has no remaining acceptance step'); ibad2 += 1
    for n in bugs:
        if n in seen_bug: no(f'bug {n} is listed in more than one implemented-bug row'); ibad2 += 1
        seen_bug[n] = cells[0]
        # an entry under "Fixed Bugs" counts as fixed unless it says OPEN itself (077 sits there, open)
        fixed = any(re.search(r'\(' + n + r'_', l) and not re.search(r'\bOPEN\b', l)
                    for l in fixed_list.splitlines())
        for f in glob.glob(os.path.join(ledger_dir, n + '_*.md')):
            # both ledger spellings: `**Status:** Fixed ...` and `Status: fixed`
            st = re.search(r'^(?:\*\*Status:\*\*|Status:)\s*(\S+)', open(f, encoding='utf-8').read(), re.M | re.I)
            if st and st.group(1).lower().startswith('fixed'): fixed = True
        if fixed:
            no(f'bug {n} is listed as not yet accepted but the bug ledger records it fixed; remove it from the table'); ibad2 += 1
if irows and not ibad2:
    ok(f'{len(irows)} implemented-but-not-accepted rows name {len(seen_bug)} bugs the ledger does not record fixed, each with a branch and a remaining step')
# No bug at all is legitimate only when every row says so explicitly (deliverable rows): a
# bugs column that parsed as something else would otherwise read as an empty table.
if irows and not ibad2 and not seen_bug and not all(
        (c + [''] * len(ihdr))[icol['bugs']] in ('—', '-') for c in irows):
    no('the "Implemented, not yet accepted" table names no bug at all'); ibad2 += 1
qbad = 0; named = 0
for order, work in qrows:
    for tid in expand(work):
        if home.get(tid) == 'the backlog':
            no(f'queue row {order} names {tid}, which lives in the backlog; a queued task lives in the task register'); qbad += 1
    phase = row_phase.get(order)
    if phase is None:
        no(f'queue row {order} has no phase in "Phases"'); qbad += 1; continue
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
def stub_re(prefix):
    return re.compile(r"^The rest of this task's original body is archived in \[ROADMAP_RETIRED_TASKS\.md\]"
                      r"\(" + re.escape(prefix) + r"ROADMAP_RETIRED_TASKS\.md#(r-\d{4})\)\.$", re.M)
STUB = {'ROADMAP_TASKS.md': stub_re('../archive/'), 'the backlog': stub_re('../archive/')}
stubs = {}
for tid in active:
    for target in STUB[home[tid]].findall(bodies[tid]):
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

# --- active/next phases, placement, pull triggers and the backlog review log
def phase_line(label):
    ms = re.findall(r'^\*\*' + label + r':\*\* (.*)$', road, re.M)
    if len(ms) != 1:
        no(f'ROADMAP.md must have exactly one "**{label}:**" line (found {len(ms)})'); return None
    vals = [v.strip() for v in re.split(r',', re.split(r' — | \(', ms[0])[0])]
    if not vals or not all(re.fullmatch(r'[A-GT]', v) for v in vals):
        no(f'"**{label}:**" must list phase letters, found "{ms[0]}"'); return None
    nums = set(vals)
    for n in sorted(nums - phase_rows):
        no(f'"**{label}:**" names Phase {n}, which is not in the "Phases" table')
    return nums if nums <= phase_rows else None
act_ph = phase_line('Active phases')
next_ph = phase_line('Next phases')
pbad = 0
if act_ph is not None and next_ph is not None:
    if act_ph & next_ph:
        no(f'phases {sorted(act_ph & next_ph)} are listed as both active and next'); pbad += 1
    near = act_ph | next_ph | {'T'}
    queued = set()
    for _, work in qrows: queued |= set(expand(work))
    for tid, body in road_tasks:
        st = phases_of(body)
        if tid not in queued and not (st and st[0] & near):
            no(f'{tid} lives in the task register but is scheduled in no active or next phase ({sorted(near)}) and no queue row names it; move it to the backlog'); pbad += 1
    for tid, body in back_tasks:
        st = phases_of(body)
        hit = sorted(st[0] & near) if st else []
        if hit and not re.search(r'^\*\*Kept in backlog[^*]*:\*\* \S', body, re.M):
            no(f'{tid} is in the backlog but its Phase line schedules it in active/next Phase {hit}; pull it into the task register or record **Kept in backlog:** with a reason'); pbad += 1
    if not pbad:
        ok(f'placement agrees with Active phases {sorted(act_ph)} and Next phases {sorted(next_ph)}')
else:
    pbad += 1
tbad = 0; seen_trig = {}
for tid, body in back_tasks:
    m = re.search(r'^\*\*Pull trigger:\*\* (.+)$', body, re.M)
    if not m or len(m.group(1).strip()) < 20:
        no(f'backlog task {tid} has no **Pull trigger:** line (or an empty one)'); tbad += 1; continue
    live = not re.search(r'^\*\*(Disposition|Ledger)', body, re.M)
    if live:
        key = m.group(1).strip().lower()
        if key in seen_trig:
            no(f'backlog tasks {seen_trig[key]} and {tid} share one pull trigger; write a task-specific trigger'); tbad += 1
        seen_trig.setdefault(key, tid)
if back_tasks and not tbad: ok(f'all {len(back_tasks)} backlog tasks carry a pull trigger; the {len(seen_trig)} live triggers are distinct')
rl = re.search(r'^### Review log$', back, re.M)
rows = []
if rl:
    for line in back[rl.end():].splitlines():
        if line.startswith('#'): break
        cells = [c.strip() for c in line.strip().strip('|').split('|')] if line.startswith('|') else []
        if len(cells) >= 4 and re.fullmatch(r'\d{4}-\d{2}-\d{2}', cells[0]): rows.append(cells)
if not rows:
    no('the backlog has no "### Review log" table with a dated row')
elif act_ph is not None:
    last = set(re.findall(r'\b[A-GT]\b', rows[-1][2]))
    if last != act_ph:
        no(f'the latest backlog review ({rows[-1][0]}) was for active phases {sorted(last)}, not the current {sorted(act_ph)}; run the phase entry review')
    else:
        ok(f'the latest backlog review ({rows[-1][0]}) covers the current active phases')

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

run_on() { # roadmap [ledger [archive [backlog [tasks]]]] -> sets RESULT lines, returns 0 iff the checker ran, reported, and found no FAIL
  if ! RESULT="$(check_file "$1" "${2:-$LEDGER}" "${3:-$ARCHIVE}" "${4:-$BACKLOG}" "${5:-$TASKSF}")"; then
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
  TMPD="$(mktemp -d)"; trap 'rm -rf "$TMPD" "$CHANGELOG"' EXIT
  neg() { # name, python-edit, expected-substring[, "ledger"/"archive"/"backlog"/"tasks"/"both" to mutate that file instead]
    # "tasks" mutates the task register; "both" runs the edit with s = the task register and b = the
    # backlog, and writes both back.
    local f="$TMPD/$1.md" l="$TMPD/$1.ledger.md" a="$TMPD/$1.archive.md" k="$TMPD/$1.backlog.md" t="$TMPD/$1.tasks.md" src="$TARGET" dst
    cp "$TARGET" "$f"; cp "$LEDGER" "$l"; cp "$ARCHIVE" "$a"; cp "$BACKLOG" "$k"; cp "$TASKSF" "$t"
    case "${4:-}" in
      ledger) src="$LEDGER"; dst="$l" ;;
      archive) src="$ARCHIVE"; dst="$a" ;;
      backlog) src="$BACKLOG"; dst="$k" ;;
      tasks|both) src="$TASKSF"; dst="$t" ;;
      *) dst="$f" ;;
    esac
    python3 - "$src" "$dst" "$2" "${4:-}" "$BACKLOG" "$k" <<'PY'
import sys, re
s = open(sys.argv[1], encoding='utf-8').read()
b = open(sys.argv[5], encoding='utf-8').read()
s0, b0 = s, b
exec(sys.argv[3])
if s == s0 and b == b0:
    sys.exit(3)  # the edit matched nothing: a control that mutates nothing proves nothing
open(sys.argv[2], 'w', encoding='utf-8').write(s)
if sys.argv[4] == 'both':
    open(sys.argv[6], 'w', encoding='utf-8').write(b)
PY
    if [ "$?" -ne 0 ]; then no "control $1: could not build its mutated copy"; return; fi
    if run_on "$f" "$l" "$a" "$k" "$t"; then
      no "control $1: the mutated roadmap passed"
    elif has_match -F -- "$3" <<<"$RESULT"; then
      ok "control $1 is rejected ($3)"
    else
      no "control $1 failed for an unexpected reason"
    fi
  }
  neg task_in_control_plane "s+='\n### Task R-0494\n'" 'declares task R-0494'
  neg plane_section_in_register "s+='\n## The queue\n'" 'control-plane section' tasks
  neg phases_in_register "s+='\n**Active phases:** A\n'" 'the task register declares' tasks
  neg missing_phase "s=s.replace('**Phase:** B','**Phase removed:** B',1)" 'has no phase assignment' tasks
  neg invalid_phase "s=s.replace('**Phase:** B','**Phase:** Z',1)" 'invalid task phase' tasks
  neg ledger_id "s=s.replace('| R-0007 |','| R-9999 |',1)" 'ledger row names missing task' ledger
  neg ledger_owner "s=s.replace('| R-0007 | superseded | R-0492, R-0510 |','| R-0007 | superseded | R-9999 |',1)" 'names missing owner' ledger
  neg missing_owner "s=re.sub(r'^(\\| A1 \\|[^|]*\\|)[^|]*\\|',r'\\1 — |',s,count=1,flags=re.M)" 'names no owning task'
  neg nonexistent_owner "s=re.sub(r'^(\\| A1 \\|[^|]*\\|)[^|]*\\|',r'\\1 R-9999 |',s,count=1,flags=re.M)" 'nonexistent task'
  neg phase_mismatch "s=s.replace('| A1 |','| B4 |',1)" 'schedules it in Phase B'
  neg missing_done_when "s=re.sub(r'(^\| A1 \|.*?\|.*?\|)[^\n]*',r'\1 |',s,flags=re.M)" 'has no goal or done-when'
  neg duplicate_row "m=re.search(r'^\| A1 \|.*$',s,re.M);s+='\n'+m[0]+'\n';s=s.replace('## Owner decisions',''+m[0]+'\n\n## Owner decisions',1)" 'duplicate queue row'
  neg bad_active "s=s.replace('**Active phases:** A','**Active phases:** Z')" 'must list phase letters'
  neg missing_active "s=s.replace('**Active phases:** A','')" 'exactly one'
  neg absent_phase "s=s.replace('| A. Core and theorem |','| Z. Core and theorem |')" 'not in the'
  neg overlapping_phases "s=s.replace('**Next phases:** B','**Next phases:** A')" 'both active and next'
  neg stale_review "s=s.replace('| phase A entry: reviewed all 546 classifications | A |','| phase A entry: reviewed all 546 classifications | B |')" 'latest backlog review' backlog
  neg missing_trigger "s=re.sub(r'^\*\*Pull trigger:\*\*[^\n]*\n','',s,count=1,flags=re.M)" 'has no **Pull trigger:**' backlog
  neg duplicate_task "m=re.search(r'^### Task R-0548$.*?(?=^### |^## |\Z)',b,re.M|re.S);s+='\n'+m[0]" 'declared in both' both
  neg lost_task "s=re.sub(r'^### Task R-0548$.*?(?=^### |^## |\Z)','',s,flags=re.M|re.S)" 'R-0548 is lost' backlog
  neg backlog_near "s=s.replace('**Phase:** D','**Phase:** A',1)" 'is in the backlog but' backlog
  neg register_later "s=s.replace('**Phase:** T','**Phase:** G',1)" 'lives in the task register but' tasks
  neg queued_task_in_backlog \
    "m=re.search(r'^### Task R-0553$.*?(?=^### |^## |\\Z)',s,re.M|re.S);assert m;b+='\n'+m[0].replace('**Phase:**','**Pull trigger:** control: queued task moved to backlog.\n\n**Phase:**',1);s=s[:m.start()]+s[m.end():]" \
    'names R-0553, which lives in the backlog' both
  neg stub_names_other_body \
    "old='ROADMAP_RETIRED_TASKS.md#r-0004).';assert old in s;s=s.replace(old,'ROADMAP_RETIRED_TASKS.md#r-0007).',1)" \
    'R-0004: its stub sentence names the archived body of R-0007' tasks
  neg candidate_column_dropped \
    "old='| candidate | bugs |';assert old in s;s=s.replace(old,'| candidate | bug list |',1)" \
    'has no "bugs" column'
  neg archive_missing_stub "s=s.replace('archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0004).','archived.')" 'has no stub' tasks
  neg stub_missing_archive "s=s.replace('## R-0004','## R-9999',1)" 'links an archived body that does not exist' archive
  neg inheritance_missing "s=re.sub(r'^- \*\*R-[0-9]{4}\*\*[^\n]*\n','',s,count=1,flags=re.M)" 'lacks inherited obligations' tasks
  neg inheritance_invented "s=s.replace('**Inherited obligations','**Inherited obligations\n- **R-9999** invented\n',1)" 'unexplained inherited obligations' tasks
  neg shared_trigger "M=list(re.finditer(r'^### Task R-[0-9]{4}$.*?(?=^### |^## |\\Z)',s,re.M|re.S));M=[m for m in M if '**Disposition' not in m[0]];a,b=M[:2];t=re.search(r'^\\*\\*Pull trigger:\\*\\* (.*)$',a[0],re.M)[1];n=re.sub(r'^\\*\\*Pull trigger:\\*\\* .*$',lambda m:'**Pull trigger:** '+t,b[0],flags=re.M);s=s[:b.start()]+n+s[b.end():]" 'share one pull trigger' backlog
  neg archive_disposition_missing "i=s.index('### Task R-0004');j=s.index('### Task ',i+1);b=s[i:j];b=re.sub(r'^\\*\\*Disposition[^\\n]*\\n','',b,flags=re.M);s=s[:i]+b+s[j:]" 'its ROADMAP stub has no Disposition' tasks
  neg duplicate_phase "s=s.replace('| A. Core and theorem |','| B. Core and theorem |')" 'duplicate phase'
  neg candidate_bad_bugs "L=s.split(chr(10)); i=next(i for i,l in enumerate(L) if l.startswith('| rocksdb-slice1 |'));c=L[i].split('|');c[2]=' nonsense ';L[i]='|'.join(c);s=chr(10).join(L)" 'bugs cell with no bug number'
  neg candidate_fixed_bug "L=s.split(chr(10)); i=next(i for i,l in enumerate(L) if l.startswith('| rocksdb-slice1 |')); c=L[i].split('|');c[2]=' 001 ';L[i]='|'.join(c);s=chr(10).join(L)" 'bug 001 is listed as not yet accepted'
  neg candidate_no_branch "s=s.replace(chr(96)+'rocksdb-slice1'+chr(96),'—',1)" 'names no branch'
  neg candidate_no_remaining "L=s.split(chr(10)); i=next(i for i,l in enumerate(L) if l.startswith('| rocksdb-slice1 |')); c=L[i].split('|');c[-2]=' ';L[i]='|'.join(c);s=chr(10).join(L)" 'has no remaining acceptance step'
  neg candidate_no_deliverable "L=s.split(chr(10)); i=next(i for i,l in enumerate(L) if l.startswith('| rocksdb-slice1 |'));c=L[i].split('|');c[3]=' — ';L[i]='|'.join(c);s=chr(10).join(L)" 'names no bug number and no deliverable'
  # Independently exercise both fixed-status spellings and the README-only fixed list.
  for spelling in readme bold plain; do
    d="$TMPD/fixed-$spelling"; mkdir -p "$d"
    printf '## Fixed Bugs\n\n' > "$d/README.md"
    case "$spelling" in
      readme) printf -- '- [901_control.md](901_control.md) — fixed control\n' >> "$d/README.md" ;;
      bold) printf '**Status:** FIXED\n' > "$d/901_control.md" ;;
      plain) printf 'Status: fixed\n' > "$d/901_control.md" ;;
    esac
    python3 - "$TARGET" "$d/road.md" <<'FIXEDCONTROL'
import sys
s=open(sys.argv[1]).read();lines=s.splitlines()
i=next(i for i,l in enumerate(lines) if l.startswith('| rocksdb-slice1 |'))
c=lines[i].split('|');c[2]=' 901 ';lines[i]='|'.join(c)
open(sys.argv[2],'w').write('\n'.join(lines)+'\n')
FIXEDCONTROL
    if BUG_LEDGER_DIR="$d" run_on "$d/road.md"; then
      no "a candidate fixed in $spelling passed"
    elif has_match -F 'bug 901 is listed as not yet accepted' <<<"$RESULT"; then
      ok "a candidate fixed in $spelling refuses"
    else no "the $spelling candidate control failed for an unexpected reason"; fi
  done
  # An explicitly OPEN entry under Fixed Bugs remains eligible as a candidate.
  d="$TMPD/open-entry"; mkdir -p "$d"
  printf '## Fixed Bugs\n\n- [901_control.md](901_control.md) — OPEN control\n' > "$d/README.md"
  cp "$TMPD/fixed-readme/road.md" "$d/road.md"
  if BUG_LEDGER_DIR="$d" run_on "$d/road.md"; then
    ok "positive control open_readme_entry passes (OPEN under Fixed Bugs)"
  else
    no "positive control open_readme_entry: an explicitly OPEN ledger entry was read as fixed"
  fi
  saved_checker="$(declare -f check_file)"
  check_file() { return 1; }
  if run_on "$TARGET"; then no "a crashed checker passed"; else ok "a crashed checker refuses"; fi
  check_file() { return 0; }
  if run_on "$TARGET"; then no "a silent checker passed"; else ok "a silent checker refuses"; fi
  eval "$saved_checker"
fi
rm -f "$CHANGELOG"
echo "ROADMAP-COVERAGE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
