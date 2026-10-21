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
#   7. (backlog split, 2026-10-19) tasks live in ROADMAP.md or docs/project/ROADMAP_BACKLOG.md, each in
#      exactly one, and no ID between R-0001 and the highest declared one is missing from both files and
#      CHANGELOG (except the IDs recorded as never allocated); every task a queue row names lives in
#      ROADMAP.md.
#   8. ROADMAP.md names its **Active phases:** and **Next phases:**, each a phase of the "Phases and
#      releases" table; a ROADMAP.md task is scheduled in one of them or named by a queue row; a backlog
#      task is not, unless it carries a **Kept in backlog:** reason.
#   9. every backlog task has a **Pull trigger:** line, no two live backlog tasks share one trigger
#      text, and the backlog's latest review-log row names the current active phases.
#  10. the queue summary table in "Phases and releases" lists exactly the queue's rows, each with the
#      same task IDs as its queue row.
#  11. (2026-10-20) the "Phase 1 exits" table gives every exit of rows 0-2 an ID (`2a`), an owner among
#      its queue row's tasks, a completion condition, an acceptance test, a class ("blocks features" or
#      "continues alongside") and a reason; each Phase 1 queue row names each of its exits and no other.
#  12. the "Feature prerequisites" table partitions every exit into blocked-by / not-blocked-by for each
#      deliverable; every "blocks features" exit blocks some deliverable and no "continues alongside"
#      exit blocks any; a queue row's "prerequisite exits:" list equals its deliverables' blocked-by set.
#  13. every bug in the "Implemented, not yet accepted" table is not recorded fixed in the bug ledger
#      (docs/bugs: its README's Fixed list or its file's Status line), and every row names a branch and
#      a remaining acceptance step. The table is generated from docs/project/candidates.tsv
#      (check_docs_drift.sh checks freshness); columns are found by header name, and a row names
#      bugs or a feature deliverable.
#  14. (2026-10-21) the "Capacity allocation" table assigns every queue row after Phase 1 and every
#      Phase 1 exit to exactly one track; exactly one track is primary, with an "at least N%" share
#      above 50% and larger than every other track's, two integration slots, and the rows of V1a/V1b/V1c
#      and R-0514; evidence integrity and adversarial validation have positive minimums; the shares add
#      up to 100%; every capped ("at most") track has one integration slot, and RocksDB
#      slice 1's row is in one. V1c joins RocksDB slice 1, V1a and V1b as a required deliverable.
#
# Run with no argument it checks ROADMAP.md and the backlog with the reconciliation record and then runs
# its negative controls against temporary copies: a removed Phase line, a bogus ledger id, a queue row
# naming a missing task or no task, a phase that contradicts its queue row, an archived body without its
# stub, a stub without its archived body, a removed inheritance entry and a fabricated one, a dropped pull
# trigger, a task in both files, a lost task, a bad or missing Active phases value, a misplaced task in
# either direction, a queued task in the backlog, a stale review log and a drifted or incomplete queue
# summary must each fail; so must an exit missing its class or acceptance test, an exit owned outside
# its row, a queue row that omits an exit, a continues-alongside exit that blocks a deliverable, a
# blocks-features exit that blocks none, a prerequisite list that drops an exit or drifts from its queue
# row (or a deliverable deleted, duplicated, weakened alone or relabelled to another row), and an
# implemented-bug row that lists a fixed bug (fixed in the ledger README alone, or by a bold or a plain
# Status line alone) or lacks its branch or remaining step; so must a dropped V1c deliverable, and a
# capacity table that is missing, leaves a row or an exit unallocated, allocates a row twice, gives the
# primary track a minority, fewer or more than two slots or loses V1's row from it, leaves evidence
# integrity or adversarial validation without a positive minimum, does not add up to 100%, gives the
# capped track two slots or leaves RocksDB slice 1 uncapped; a crashed or silent checker must fail too.
# With arguments `ROADMAP [LEDGER [ARCHIVE [BACKLOG]]]` it checks those files only.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

TARGET="${1:-ROADMAP.md}"
LEDGER="${2:-docs/project/ROADMAP_RECONCILIATION.md}"
ARCHIVE="${3:-docs/archive/ROADMAP_RETIRED_TASKS.md}"
BACKLOG="${4:-docs/project/ROADMAP_BACKLOG.md}"
CHANGELOG="CHANGELOG.md"
[ -f "$TARGET" ] || { echo "FATAL: $TARGET missing" >&2; exit 2; }
[ -f "$LEDGER" ] || { echo "FATAL: $LEDGER missing" >&2; exit 2; }
[ -f "$ARCHIVE" ] || { echo "FATAL: $ARCHIVE missing" >&2; exit 2; }
[ -f "$BACKLOG" ] || { echo "FATAL: $BACKLOG missing" >&2; exit 2; }

check_file() { # roadmap ledger archive backlog -> prints "OK <msg>" / "FAIL <msg>" lines
  python3 - "$1" "$CHANGELOG" "$2" "$3" "$4" <<'PY'
import re, sys
road = open(sys.argv[1], encoding='utf-8').read()
chg = open(sys.argv[2], encoding='utf-8').read()
rec = open(sys.argv[3], encoding='utf-8').read()
arc = open(sys.argv[4], encoding='utf-8').read()
back = open(sys.argv[5], encoding='utf-8').read()
out = []
def ok(m): out.append('OK ' + m)
def no(m): out.append('FAIL ' + m)

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

road_tasks = task_bodies(road)
back_tasks = task_bodies(back)
completed = set(re.findall(r'^### Completed Task (R-\d{4})$', chg, re.M))
if not road_tasks: no('no task headings found'); print('\n'.join(out)); sys.exit()
if not back_tasks: no('the backlog declares no task headings')

# --- one live home per task (ROADMAP.md or the backlog), and no ID lost
home = {}
hbad = 0
for where, tasks in (('ROADMAP.md', road_tasks), ('the backlog', back_tasks)):
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
        no(f'{tid} is lost: declared in neither ROADMAP.md, the backlog nor CHANGELOG'); hbad += 1
for tid in sorted(NEVER_ALLOCATED & declared):
    no(f'{tid} is recorded as never allocated but is declared'); hbad += 1
if not hbad:
    ok(f'{len(home)} live tasks ({len(road_tasks)} in ROADMAP.md, {len(back_tasks)} in the backlog) each have one home; R-0001..R-{top:04d} are all accounted for')

active = list(home)
known = set(active) | completed
bodies = {}
for tid, body in road_tasks + back_tasks:
    bodies.setdefault(tid, body)

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
qexit = {}
if qm:
    for line in road[qm.end():].splitlines():
        if line.startswith('## '): break
        m = re.match(r'^\| (\d+) \| (.*?) \| (.*)\|$', line)
        if m: qrows.append((int(m.group(1)), m.group(2))); qexit.setdefault(int(m.group(1)), m.group(3))
if not qrows: no('no queue rows found')
# The queue summary in "Phases and releases" (| row | tasks | goal |) restates each row's task IDs; it
# must list exactly the queue's rows, each naming exactly the tasks its queue row names.
summary = {}
sbad_dup = False
if gm:
    in_sum = False
    for line in road[gm.end():].split('\n'):
        if line.startswith('## '): break
        cells = [c.strip() for c in line.strip().strip('|').split('|')] if line.startswith('|') else []
        if cells[:3] == ['row', 'tasks', 'goal']: in_sum = True; continue
        if not cells: in_sum = False; continue
        if in_sum and re.fullmatch(r'\d+', cells[0]):
            if int(cells[0]) in summary:
                no(f'the queue summary lists row {cells[0]} more than once'); sbad_dup = True
            summary.setdefault(int(cells[0]), set(expand(cells[1])))
sbad = 0
for o in sorted(set(o for o, _ in qrows if [x for x, _ in qrows].count(o) > 1)):
    no(f'the queue has more than one row {o}'); sbad += 1
if not summary:
    no('"Phases and releases" has no queue summary table (| row | tasks | goal |)'); sbad += 1
else:
    qmap = {o: set(expand(w)) for o, w in qrows}
    for o in sorted(set(qmap) | set(summary)):
        if o not in summary:
            no(f'queue row {o} is missing from the queue summary'); sbad += 1
        elif o not in qmap:
            no(f'the queue summary lists row {o}, which the queue does not have'); sbad += 1
        elif qmap[o] != summary[o]:
            no(f'queue summary row {o} names {sorted(summary[o])}, the queue row names {sorted(qmap[o])}'); sbad += 1
    if not sbad and not sbad_dup: ok(f'the queue summary lists the {len(summary)} queue rows with the same task IDs')
# --- Phase 1 exits, feature prerequisites, and implemented-but-not-accepted bugs (2026-10-20)
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
CLASSES = ('blocks features', 'continues alongside')
qtasks = {o: set(expand(w)) for o, w in qrows}
phase1_rows = sorted(o for o in qtasks if row_phase.get(o) == 1)
exits = {}
xrows = table_rows('Phase 1 exits', 'exit')
ebad = 0
if xrows is None or not xrows:
    no('ROADMAP.md has no "### Phase 1 exits" table (| exit | owner | closes when | acceptance test | class | why this class |)'); ebad += 1
    xrows = []
for cells in xrows:
    eid = cells[0]
    if not re.fullmatch(r'\d[a-z]', eid):
        no(f'Phase 1 exit "{eid}" is not an ID like 2a'); ebad += 1; continue
    if eid in exits:
        no(f'Phase 1 exit {eid} is listed more than once'); ebad += 1; continue
    cells = cells + [''] * (6 - len(cells))
    row = int(eid[0]); exits[eid] = cells[4]
    if row not in phase1_rows:
        no(f'exit {eid} belongs to queue row {row}, which is not a Phase 1 row'); ebad += 1
    owners = expand(cells[1])
    if not owners:
        no(f'exit {eid} names no owning task'); ebad += 1
    for t in owners:
        if t not in qtasks.get(row, set()):
            no(f"exit {eid}'s owner {t} is not a task of queue row {row}"); ebad += 1
    if len(cells[2]) < 20: no(f'exit {eid} has no completion condition'); ebad += 1
    if len(cells[3]) < 20: no(f'exit {eid} has no acceptance test'); ebad += 1
    if cells[4] not in CLASSES: no(f'exit {eid} has no class (blocks features / continues alongside)'); ebad += 1
    if len(cells[5]) < 10: no(f'exit {eid} gives no reason for its class'); ebad += 1
for o in phase1_rows:
    mine = sorted(e for e in exits if int(e[0]) == o)
    if not mine:
        no(f'Phase 1 queue row {o} has no exit in "Phase 1 exits"'); ebad += 1; continue
    named = set(re.findall(r'\b(' + str(o) + r'[a-z])\b', qexit.get(o, '')))
    for e in mine:
        if e not in named: no(f'queue row {o} does not name its exit {e}'); ebad += 1
    for e in sorted(named - set(mine)):
        no(f'queue row {o} names exit {e}, which "Phase 1 exits" does not list'); ebad += 1
if exits and not ebad:
    ok(f'{len(exits)} Phase 1 exits each have an owner from their row, a completion condition, an acceptance test and a class, and their queue rows name them')
prows = table_rows('Feature prerequisites', 'deliverable')
pbad2 = 0
blocked_any = set(); row_blocked = {}
# The first feature deliverables, each with its own row in the table and its own comparison against its
# queue row's "prerequisite exits:" list: a union per row would let one deliverable mask another.
# Each is bound to the queue row that schedules it, so relabelling a row cannot borrow another's list.
REQUIRED_DELIVERABLES = {'RocksDB slice 1': 5, 'V1a': 3, 'V1b': 3, 'V1c': 3}
seen_deliv = set()
if not prows:
    no('ROADMAP.md has no "### Feature prerequisites" table (| deliverable | blocked by | not blocked by |)'); pbad2 += 1
    prows = []
for cells in prows:
    cells = cells + [''] * (3 - len(cells))
    d = cells[0]
    dname = d.split(' (')[0].strip()
    if dname in seen_deliv:
        no(f'deliverable {dname} is listed more than once in the prerequisites'); pbad2 += 1
    seen_deliv.add(dname)
    b = re.findall(r'\b(\d[a-z])\b', cells[1]); n = re.findall(r'\b(\d[a-z])\b', cells[2])
    for e in sorted(set(b + n) - set(exits)):
        no(f'prerequisites of "{d}" name exit {e}, which does not exist'); pbad2 += 1
    for e in sorted(set(b) & set(n)):
        no(f'prerequisites of "{d}" list exit {e} as both blocking and not blocking'); pbad2 += 1
    for e in sorted(set(exits) - set(b) - set(n)):
        no(f'prerequisites of "{d}" do not account for exit {e}'); pbad2 += 1
    blocked_any |= set(b)
    rm = re.search(r'\(row (\d+)', d)
    if not rm:
        no(f'deliverable "{d}" names no queue row'); pbad2 += 1
    else:
        drow = int(rm.group(1))
        if dname in REQUIRED_DELIVERABLES and drow != REQUIRED_DELIVERABLES[dname]:
            no(f'deliverable {dname} is labelled row {drow}, but queue row {REQUIRED_DELIVERABLES[dname]} schedules it'); pbad2 += 1
            drow = REQUIRED_DELIVERABLES[dname]
        row_blocked.setdefault(drow, []).append((dname, set(b)))
for e, cls in sorted(exits.items()):
    if cls == 'blocks features' and e not in blocked_any:
        no(f'exit {e} is blocks features but no deliverable lists it as a prerequisite'); pbad2 += 1
    if cls == 'continues alongside' and e in blocked_any:
        no(f'exit {e} is continues alongside but blocks a deliverable'); pbad2 += 1
for dname in REQUIRED_DELIVERABLES:
    if dname not in seen_deliv:
        no(f'the prerequisites table has no row for deliverable {dname}'); pbad2 += 1
for o, delivs in sorted(row_blocked.items()):
    m = re.search(r'prerequisite exits: ([0-9a-z, ]+)', qexit.get(o, ''))
    if not m:
        no(f'queue row {o} has a deliverable but no "prerequisite exits:" list'); pbad2 += 1
        continue
    have = set(re.findall(r'\b(\d[a-z])\b', m.group(1)))
    for dname, want in delivs:
        if have != want:
            no(f'queue row {o} names prerequisite exits {sorted(have)}, the prerequisites table says {sorted(want)} for {dname}'); pbad2 += 1
if prows and not pbad2:
    ok(f'{len(prows)} feature deliverables partition the exits; classes agree with the prerequisite lists and queue rows')
# --- capacity allocation (2026-10-21): every post-Phase-1 queue row and every Phase 1 exit belongs to
# exactly one track; the primary track holds the majority and the rows of V1 and the shared compiler
# structure; the capped feature track has one slot and holds RocksDB slice 1's row.
crows = table_rows('Capacity allocation', 'track')
cbad = 0
if not crows:
    no('ROADMAP.md has no "### Capacity allocation" table (| track | queue rows | Phase 1 exits | share per cycle | integration slots | scope |)'); cbad += 1
    crows = []
row_track = {}; exit_track = {}; tracks = []
for cells in crows:
    cells = cells + [''] * (6 - len(cells))
    name = cells[0]
    rows_c = [int(x) for x in re.findall(r'\b(\d+)\b', cells[1])]
    exits_c = re.findall(r'\b(\d[a-z])\b', cells[2])
    sm = re.fullmatch(r'(at least|at most) (\d+)%', cells[3])
    if not sm:
        no(f'capacity track "{name}" has no share "at least N%" or "at most N%"'); cbad += 1; continue
    slm = re.match(r'(\d+)\b', cells[4])
    if not slm:
        no(f'capacity track "{name}" names no integration slot count'); cbad += 1; continue
    if not rows_c and not exits_c:
        no(f'capacity track "{name}" owns no queue row and no exit'); cbad += 1
    tracks.append((name, sm.group(1), int(sm.group(2)), int(slm.group(1)), rows_c))
    for r in rows_c:
        if r in phase1_rows:
            no(f'capacity track "{name}" lists Phase 1 row {r}; Phase 1 rows are allocated by exit'); cbad += 1
        elif r not in qtasks:
            no(f'capacity track "{name}" lists queue row {r}, which the queue does not have'); cbad += 1
        elif r in row_track:
            no(f'queue row {r} is in more than one capacity track'); cbad += 1
        row_track.setdefault(r, name)
    for e in exits_c:
        if e not in exits:
            no(f'capacity track "{name}" lists exit {e}, which does not exist'); cbad += 1
        elif e in exit_track:
            no(f'exit {e} is in more than one capacity track'); cbad += 1
        exit_track.setdefault(e, name)
if tracks:
    for r in sorted(set(qtasks) - set(phase1_rows) - set(row_track)):
        no(f'queue row {r} is in no capacity track'); cbad += 1
    for e in sorted(set(exits) - set(exit_track)):
        no(f'exit {e} is in no capacity track'); cbad += 1
    prim = [t for t in tracks if '(primary)' in t[0]]
    if len(prim) != 1:
        no(f'the capacity allocation has {len(prim)} tracks marked (primary), not one'); cbad += 1
    else:
        pn, pk, pv, ps, prow = prim[0]
        if pk != 'at least' or pv <= 50:
            no(f'the primary track "{pn}" has share {pk} {pv}%, not a majority minimum'); cbad += 1
        for t in tracks:
            if t is not prim[0] and t[2] >= pv:
                no(f'capacity track "{t[0]}" ({t[2]}%) is not smaller than the primary track ({pv}%)'); cbad += 1
        if ps != 2:
            no('the primary track must have two integration slots'); cbad += 1
        must = {o for d, o in REQUIRED_DELIVERABLES.items() if d.startswith('V1')}
        must |= {o for o, ts in qtasks.items() if 'R-0514' in ts}
        for o in sorted(must - set(prow)):
            no(f'queue row {o} (V1 or the shared compiler structure) is not in the primary track'); cbad += 1
    for marker in ('**B. Evidence integrity**', '**C. Adversarial validation and gate health**'):
        protected = [t for t in tracks if t[0].startswith(marker)]
        if len(protected) != 1 or protected[0][1] != 'at least' or protected[0][2] <= 0:
            no(f'{marker} must have a positive protected minimum'); cbad += 1
    if sum(t[2] for t in tracks) != 100:
        no(f'the capacity shares add up to {sum(t[2] for t in tracks)}%, not 100%'); cbad += 1
    capped = [t for t in tracks if t[1] == 'at most']
    if not capped:
        no('the capacity allocation has no capped ("at most") track'); cbad += 1
    for t in capped:
        if t[3] != 1:
            no(f'capped capacity track "{t[0]}" has {t[3]} integration slots, not one'); cbad += 1
    rk = REQUIRED_DELIVERABLES['RocksDB slice 1']
    if rk not in {r for t in capped for r in t[4]}:
        no(f'queue row {rk} (RocksDB slice 1) is not in a capped capacity track'); cbad += 1
if crows and not cbad:
    ok(f'{len(tracks)} capacity tracks partition the post-Phase-1 queue rows and the Phase 1 exits; the primary track holds the majority, V1 and the shared compiler structure; the feature track is capped with one slot')
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
            no(f'queue row {order} names {tid}, which lives in the backlog; a queued task lives in ROADMAP.md'); qbad += 1
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
def stub_re(prefix):
    return re.compile(r"^The rest of this task's original body is archived in \[ROADMAP_RETIRED_TASKS\.md\]"
                      r"\(" + re.escape(prefix) + r"ROADMAP_RETIRED_TASKS\.md#(r-\d{4})\)\.$", re.M)
STUB = {'ROADMAP.md': stub_re('docs/archive/'), 'the backlog': stub_re('../archive/')}
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
phase_rows = set()
if gm:
    for line in road[gm.end():].split('\n'):
        if line.startswith('## '): break
        m = re.match(r'^\| \*\*Phase (\d)\b', line)
        if m: phase_rows.add(int(m.group(1)))
def phase_line(label):
    ms = re.findall(r'^\*\*' + label + r':\*\* (.*)$', road, re.M)
    if len(ms) != 1:
        no(f'ROADMAP.md must have exactly one "**{label}:**" line (found {len(ms)})'); return None
    vals = [v.strip() for v in re.split(r',', re.split(r' — | \(', ms[0])[0])]
    if not vals or not all(re.fullmatch(r'\d', v) for v in vals):
        no(f'"**{label}:**" must list phase numbers, found "{ms[0]}"'); return None
    nums = set(int(v) for v in vals)
    for n in sorted(nums - phase_rows):
        no(f'"**{label}:**" names Phase {n}, which is not in the "Phases and releases" table')
    return nums if nums <= phase_rows else None
act_ph = phase_line('Active phases')
next_ph = phase_line('Next phases')
pbad = 0
if act_ph is not None and next_ph is not None:
    if act_ph & next_ph:
        no(f'phases {sorted(act_ph & next_ph)} are listed as both active and next'); pbad += 1
    near = act_ph | next_ph
    queued = set()
    for _, work in qrows: queued |= set(expand(work))
    for tid, body in road_tasks:
        st = phases_of(body)
        if tid not in queued and not (st and st[0] & near):
            no(f'{tid} lives in ROADMAP.md but is scheduled in no active or next phase ({sorted(near)}) and no queue row names it; move it to the backlog'); pbad += 1
    for tid, body in back_tasks:
        st = phases_of(body)
        hit = sorted(st[0] & near) if st else []
        if hit and not re.search(r'^\*\*Kept in backlog[^*]*:\*\* \S', body, re.M):
            no(f'{tid} is in the backlog but its Phase line schedules it in active/next Phase {hit}; pull it into ROADMAP.md or record **Kept in backlog:** with a reason'); pbad += 1
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
    last = set(int(d) for d in re.findall(r'\d', rows[-1][2]))
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

run_on() { # roadmap [ledger [archive [backlog]]] -> sets RESULT lines, returns 0 iff the checker ran, reported, and found no FAIL
  if ! RESULT="$(check_file "$1" "${2:-$LEDGER}" "${3:-$ARCHIVE}" "${4:-$BACKLOG}")"; then
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
  neg() { # name, python-edit, expected-substring[, "ledger"/"archive"/"backlog"/"both" to mutate that file instead]
    # "both" runs the edit with s = ROADMAP and b = backlog, and writes both back.
    local f="$TMPD/$1.md" l="$TMPD/$1.ledger.md" a="$TMPD/$1.archive.md" k="$TMPD/$1.backlog.md" src="$TARGET" dst
    cp "$TARGET" "$f"; cp "$LEDGER" "$l"; cp "$ARCHIVE" "$a"; cp "$BACKLOG" "$k"
    case "${4:-}" in
      ledger) src="$LEDGER"; dst="$l" ;;
      archive) src="$ARCHIVE"; dst="$a" ;;
      backlog) src="$BACKLOG"; dst="$k" ;;
      *) dst="$f" ;;
    esac
    python3 - "$src" "$dst" "$2" "${4:-}" "$BACKLOG" "$k" <<'PY'
import sys, re
s = open(sys.argv[1], encoding='utf-8').read()
b = open(sys.argv[5], encoding='utf-8').read()
exec(sys.argv[3])
open(sys.argv[2], 'w', encoding='utf-8').write(s)
if sys.argv[4] == 'both':
    open(sys.argv[6], 'w', encoding='utf-8').write(b)
PY
    if [ "$?" -ne 0 ]; then no "control $1: could not build its mutated copy"; return; fi
    if run_on "$f" "$l" "$a" "$k"; then
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
    "R-0522 has no phase assignment" backlog
  neg dropped_pull_trigger \
    "import re; m=re.search(r'^\*\*Pull trigger:\*\*[^\n]*\n\n', s[s.index('### Task R-0305\n'):], re.M); assert m; i=s.index('### Task R-0305\n')+m.start(); s=s[:i]+s[i+len(m.group(0)):]" \
    "backlog task R-0305 has no" backlog
  neg backlog_stub_link_removed \
    "old='archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0072).'; assert old in s; s=s.replace(old,'archived.')" \
    "archived body R-0072 has no stub" backlog
  neg shared_pull_trigger \
    "import re; m=re.search(r'^\*\*Pull trigger:\*\* ([^\n]*)$', s[s.index('### Task R-0306\n'):], re.M); t=m.group(1); i=s.index('### Task R-0307\n'); m2=re.search(r'^\*\*Pull trigger:\*\* [^\n]*$', s[i:], re.M); s=s[:i+m2.start()]+'**Pull trigger:** '+t+s[i+m2.end():]" \
    "share one pull trigger" backlog
  neg task_in_both_files \
    "i=b.index('### Task R-0305\n'); j=b.index('\n### ',i+1); s=s.rstrip('\n')+'\n\n'+b[i:j]+'\n'" \
    "R-0305 is declared in both ROADMAP.md and the backlog" both
  neg lost_task \
    "i=s.index('### Task R-0305\n'); j=s.index('\n### ',i+1); s=s[:i]+s[j+1:]" \
    "R-0305 is lost" backlog
  neg bad_active_phases \
    "old='**Active phases:** 1'; assert s.count(old)==1; s=s.replace(old,'**Active phases:** 9')" \
    "names Phase 9, which is not in"
  neg missing_active_phases \
    "import re; s,n=re.subn(r'^\*\*Active phases:\*\*[^\n]*\n', '', s, flags=re.M); assert n==1" \
    "exactly one \"\*\*Active phases:\*\*\" line"
  neg backlog_task_in_near_phase \
    "h='### Task R-0305\n\n**Phase:** after-1.0/research'; assert h in s; s=s.replace(h,'### Task R-0305\n\n**Phase:** 2')" \
    "R-0305 is in the backlog but its Phase line schedules it in active/next Phase \[2\]" backlog
  neg roadmap_task_only_later \
    "h='### Task R-0518\n\n**Phase:** '; i=s.index(h)+len(h); j=s.index('\n',i); s=s[:i]+'7'+s[j:]" \
    "R-0518 lives in ROADMAP.md but is scheduled in no active or next phase"
  neg queued_task_in_backlog \
    "i=s.index('### Task R-0521\n'); j=s.index('\n### ',i+1); b=b.rstrip('\n')+'\n\n'+s[i:j].replace('**Phase:**','**Pull trigger:** control: a queued task moved to the backlog.\n\n**Phase:**',1)+'\n'; s=s[:i]+s[j+1:]" \
    "queue row 9 names R-0521, which lives in the backlog" both
  neg stale_review_log \
    "old='**Active phases:** 1'; assert s.count(old)==1; s=s.replace(old,'**Active phases:** 2').replace('**Next phases:** 2, 3','**Next phases:** 3, 4')" \
    "the latest backlog review (2026-10-19) was for active phases \[1\], not the current \[2\]"
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
  neg queue_summary_drift \
    "old='| 9 | R-0521 | Compiler provenance'; assert old in s; s=s.replace(old,'| 9 | R-0522 | Compiler provenance')" \
    "queue summary row 9 names \['R-0522'\], the queue row names \['R-0521'\]"
  neg queue_summary_duplicate_row \
    "old='| 9 | R-0521 | Compiler provenance'; assert old in s; s=s.replace(old,'| 9 | R-0522 | Wrong owner |\n'+old)" \
    "the queue summary lists row 9 more than once"
  neg queue_duplicate_row \
    "old='| 9 | **Compiler provenance and canonical \`VerificationTask\` (R-0521)** |'; assert old in s; s=s.replace(old,'| 9 | **Wrong owner (R-0522)** | control |\n'+old)" \
    "the queue has more than one row 9"
  neg queue_summary_missing_row \
    "import re; s,n=re.subn(r'^\| 17 \| R-0440 \| [^\n]*\n', '', s, flags=re.M); assert n==1" \
    "queue row 17 is missing from the queue summary"
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
  neg exit_missing_class \
    "old='| blocks features | known false proofs and unsound claims |'; assert s.count(old)==1; s=s.replace(old,'|  | known false proofs and unsound claims |')" \
    "exit 0a has no class"
  neg exit_missing_acceptance \
    "import re; m=re.search(r'^\| 2d \|[^\n]*$', s, re.M); assert m; c=m.group(0).split(' | '); c[3]=''; s=s[:m.start()]+' | '.join(c)+s[m.end():]" \
    "exit 2d has no acceptance test"
  neg exit_owner_outside_row \
    "old='| 2a | R-0494 |'; assert s.count(old)==1; s=s.replace(old,'| 2a | R-0511 |')" \
    "exit 2a's owner R-0511 is not a task of queue row 2"
  neg queue_row_omits_exit \
    "old='demonstrated enforcement (2l)'; assert s.count(old)==1; s=s.replace(old,'demonstrated enforcement')" \
    "queue row 2 does not name its exit 2l"
  neg continues_exit_blocks \
    "old='| RocksDB slice 1 (row 5, R-0285) | 0b, 2i | 0a, 0c, 1a, 2a, 2b, 2c, 2d, 2e, 2f, 2g, 2h, 2j, 2k, 2l |'; assert s.count(old)==1; s=s.replace(old,'| RocksDB slice 1 (row 5, R-0285) | 0b, 2i, 2k | 0a, 0c, 1a, 2a, 2b, 2c, 2d, 2e, 2f, 2g, 2h, 2j, 2l |')" \
    "exit 2k is continues alongside but blocks a deliverable"
  neg blocks_exit_unlisted \
    "import re; m=re.search(r'^\| 2c \|[^\n]*$', s, re.M); assert m; c=m.group(0).split(' | '); assert c[4]=='continues alongside'; c[4]='blocks features'; s=s[:m.start()]+' | '.join(c)+s[m.end():]" \
    "exit 2c is blocks features but no deliverable lists it"
  neg prerequisites_drop_exit \
    "old='| V1a (row 3, R-0511) | 0a, 0b, 2a, 2g, 2h, 2i, 2j | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l |'; assert s.count(old)==1; s=s.replace(old,'| V1a (row 3, R-0511) | 0a, 0b, 2a, 2g, 2h, 2i, 2j | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k |')" \
    "prerequisites of \"V1a (row 3, R-0511)\" do not account for exit 2l"
  neg queue_prerequisites_drift \
    "old='prerequisite exits: 0b, 2i;'; assert s.count(old)==1; s=s.replace(old,'prerequisite exits: 0b;')" \
    "queue row 5 names prerequisite exits \['0b'\], the prerequisites table says \['0b', '2i'\]"
  neg prerequisites_deliverable_deleted \
    "import re; s,n=re.subn(r'^\| V1b \(row 3[^\n]*\n', '', s, flags=re.M); assert n==1" \
    "the prerequisites table has no row for deliverable V1b"
  neg prerequisites_deliverable_duplicated \
    "import re; m=re.search(r'^\| V1b \(row 3[^\n]*\n', s, re.M); assert m; s=s[:m.end()]+m.group(0)+s[m.end():]" \
    "deliverable V1b is listed more than once"
  neg prerequisites_one_deliverable_weakened \
    "old='| V1b (row 3, R-0511 with R-0517) | 0a, 0b, 2a, 2g, 2h, 2i, 2j | 0c,'; assert s.count(old)==1; s=s.replace(old,'| V1b (row 3, R-0511 with R-0517) | 0b, 2a, 2g, 2h, 2i, 2j | 0a, 0c,')" \
    "for V1b"
  neg prerequisites_row_relabelled \
    "old='| V1b (row 3, R-0511 with R-0517) | 0a, 0b, 2a, 2g, 2h, 2i, 2j | 0c, 1a, 2b, 2c, 2d, 2e, 2f, 2k, 2l |'; assert s.count(old)==1; s=s.replace(old,'| V1b (row 5, R-0511 with R-0517) | 0b, 2i | 0a, 0c, 1a, 2a, 2b, 2c, 2d, 2e, 2f, 2g, 2h, 2j, 2k, 2l |')" \
    "deliverable V1b is labelled row 5, but queue row 3 schedules it"
  # The implemented-table controls do not depend on which candidates are live: each inserts two
  # control rows (bugs 901, 902 and 903, which no ledger record names) into its copy first.
  CTL_ROWS="hdr='| candidate | bugs | deliverable | branch | candidate SHA | exits | stage (a) | stage (b) | stage (c) | unlocks | remaining acceptance step |\n|---|---|---|---|---|---|---|---|---|---|---|\n'; assert s.count(hdr)==1; s=s.replace(hdr, hdr+'| ctl-a | 901, 902 | — | \`ctl-a-branch\` | \`cccccccccccc\` | 0b | not run | not run | not run | exit 0b progress | control row |\n| ctl-b | 903 | — | \`ctl-b-branch\` | \`dddddddddddd\` | 0b | not run | not run | not run | exit 0b progress | control row |\n');"
  neg implemented_bug_already_fixed \
    "$CTL_ROWS old='| ctl-a | 901,'; assert s.count(old)==1; s=s.replace(old,'| ctl-a | 080, 901,')" \
    "bug 080 is listed as not yet accepted but the bug ledger records it fixed"
  neg implemented_bug_no_branch \
    "$CTL_ROWS old='| \`ctl-a-branch\` |'; assert s.count(old)==1; s=s.replace(old,'| none |')" \
    "implemented-bug row \"ctl-a\" names no branch"
  neg implemented_bug_no_step \
    "$CTL_ROWS import re; s,n=re.subn(r'^(\| ctl-b \|.*\|) [^|]+ \|$', r'\\1  |', s, flags=re.M); assert n==1" \
    "implemented-bug row \"ctl-b\" has no remaining acceptance step"
  neg implemented_row_no_bug_no_deliverable \
    "old='| rocksdb-slice1 | — | RocksDB slice 1 groundwork (R-0285) |'; assert s.count(old)==1; s=s.replace(old,'| rocksdb-slice1 | — | — |')" \
    "implemented-bug row \"rocksdb-slice1\" names no bug number and no deliverable"
  neg implemented_table_column_dropped \
    "old='| candidate | bugs | deliverable | branch |'; assert s.count(old)==1; s=s.replace(old,'| candidate | bug list | deliverable | branch |')" \
    "has no \"bugs\" column"
  neg v1c_deliverable_dropped \
    "old='| V1c (row 3, R-0511) |'; assert s.count(old)==1; i=s.index(old); j=s.index('\n',i); s=s[:i]+s[j+1:]" \
    "no row for deliverable V1c"
  neg capacity_table_removed \
    "old='### Capacity allocation\n'; assert s.count(old)==1; s=s.replace(old,'### Capacity notes\n')" \
    "has no \"### Capacity allocation\" table"
  neg capacity_row_unallocated \
    "old='| **B. Evidence integrity** (protected) | 9, 10, 11, 12,'; assert s.count(old)==1; s=s.replace(old,'| **B. Evidence integrity** (protected) | 10, 11, 12,')" \
    "queue row 9 is in no capacity track"
  neg capacity_row_twice \
    "old='| **D. Bounded features** (capped) | 5, 7, 8, 16 |'; assert s.count(old)==1; s=s.replace(old,'| **D. Bounded features** (capped) | 3, 5, 7, 8, 16 |')" \
    "queue row 3 is in more than one capacity track"
  neg capacity_exit_unallocated \
    "old='| 0c, 2b, 2c, 2d, 2e, 2f, 2l | at least 10%'; assert s.count(old)==1; s=s.replace(old,'| 0c, 2b, 2d, 2e, 2f, 2l | at least 10%')" \
    "exit 2c is in no capacity track"
  neg capacity_primary_minority \
    "old='| at least 55% |'; assert s.count(old)==1; s=s.replace(old,'| at least 45% |').replace('| at most 20% |','| at most 30% |')" \
    "not a majority minimum"
  neg capacity_v1_not_primary \
    "old='| **A. Verified compiler** (primary) | 3, 4, 6 |'; assert s.count(old)==1; s=s.replace(old,'| **A. Verified compiler** (primary) | 4, 6 |').replace('| **D. Bounded features** (capped) | 5, 7, 8, 16 |','| **D. Bounded features** (capped) | 3, 5, 7, 8, 16 |')" \
    "queue row 3 (V1 or the shared compiler structure) is not in the primary track"
  neg capacity_shares_not_100 \
    "old='| at least 15% |'; assert s.count(old)==1; s=s.replace(old,'| at least 25% |')" \
    "add up to 110%"
  neg capacity_feature_slots \
    "old='| at most 20% | 1 |'; assert s.count(old)==1; s=s.replace(old,'| at most 20% | 2 |')" \
    "has 2 integration slots, not one"
  neg capacity_primary_slots \
    "old='| 2: one shared-compiler migration checkpoint'; assert s.count(old)==1; s=s.replace(old,'| 1: one shared-compiler migration checkpoint')" \
    "the primary track must have two integration slots"
  neg capacity_evidence_unprotected \
    "old='| at least 15% |'; assert s.count(old)==1; s=s.replace(old,'| at least 0% |').replace('| at least 55% |','| at least 70% |')" \
    "B. Evidence integrity\*\* must have a positive protected minimum"
  neg capacity_validation_unprotected \
    "old='| at least 10% |'; assert s.count(old)==1; s=s.replace(old,'| at least 0% |').replace('| at least 55% |','| at least 65% |')" \
    "C. Adversarial validation and gate health\*\* must have a positive protected minimum"
  neg capacity_rocksdb_uncapped \
    "old='| **D. Bounded features** (capped) | 5, 7, 8, 16 |'; assert s.count(old)==1; s=s.replace(old,'| **D. Bounded features** (capped) | 7, 8, 16 |').replace('| **A. Verified compiler** (primary) | 3, 4, 6 |','| **A. Verified compiler** (primary) | 3, 4, 5, 6 |')" \
    "queue row 5 (RocksDB slice 1) is not in a capped capacity track"
  # Bug-ledger controls: the same roadmap against a mutated copy of docs/bugs, one way of recording a
  # fix at a time (README's Fixed list alone, a bold Status line alone, a plain Status line alone).
  bugneg() { # name, python-edit over s (file named in $3, relative to docs/bugs), file, expected-substring
    local d="$TMPD/$1.bugs"
    cp -R docs/bugs "$d"
    if ! python3 - "$d/$3" "$2" <<'PY'
import sys, re
p = sys.argv[1]; s = open(p, encoding='utf-8').read()
exec(sys.argv[2])
open(p, 'w', encoding='utf-8').write(s)
PY
    then no "control $1: could not build its mutated ledger"; return; fi
    python3 - "$TARGET" "$TMPD/$1.ctl.md" "$CTL_ROWS" <<'PY' || { no "control $1: could not build its roadmap copy"; return; }
import sys
s = open(sys.argv[1], encoding='utf-8').read()
exec(sys.argv[3])
open(sys.argv[2], 'w', encoding='utf-8').write(s)
PY
    if BUG_LEDGER_DIR="$d" run_on "$TMPD/$1.ctl.md"; then
      no "control $1: the mutated ledger passed"
    elif printf '%s\n' "$RESULT" | grep -q -- "$4"; then
      ok "control $1 is rejected ($4)"
    else
      no "control $1 failed for an unexpected reason"
    fi
  }
  # The controls use the control rows' bugs (901 - 903), which have no record file, so the Status
  # controls create one in the copied ledger.
  bugneg ledger_fixed_in_readme_only \
    "old='## Fixed Bugs\n\n'; assert s.count(old)==1; s=s.replace(old, old+'- [901_control.md](901_control.md)\n')" \
    README.md "bug 901 is listed as not yet accepted but the bug ledger records it fixed"
  bugneg ledger_fixed_bold_status \
    "import os; open(os.path.join(os.path.dirname(p), '902_control.md'), 'w').write('# Bug 902\n\n**Status:** FIXED 2026-10-20\n')" \
    README.md "bug 902 is listed as not yet accepted but the bug ledger records it fixed"
  bugneg ledger_fixed_plain_status \
    "import os; open(os.path.join(os.path.dirname(p), '903_control.md'), 'w').write('# Bug 903\n\nStatus: fixed\n')" \
    README.md "bug 903 is listed as not yet accepted but the bug ledger records it fixed"
  # Positive control: a bug whose README entry sits under "Fixed Bugs" but says OPEN (077) is open.
  cp "$TARGET" "$TMPD/open_entry.md"
  if python3 - "$TMPD/open_entry.md" "$CTL_ROWS" <<'PY'
import sys
p = sys.argv[1]; s = open(p, encoding='utf-8').read()
exec(sys.argv[2])
old = '| ctl-a | 901,'
assert s.count(old) == 1
s = s.replace(old, '| ctl-a | 077, 901,')
open(p, 'w', encoding='utf-8').write(s)
PY
  then
    if run_on "$TMPD/open_entry.md"; then
      ok "positive control open_readme_entry passes (077 says OPEN under Fixed Bugs)"
    else
      no "positive control open_readme_entry: an explicitly OPEN ledger entry was read as fixed"
    fi
  else
    no "positive control open_readme_entry: could not build its copy"
  fi
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
