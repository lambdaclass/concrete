# N2 amendment 3 accounting implementation

Status: **per-analysis interpretation assumed, pending owner** (coordinator option C).
No owner answer or landing is claimed. R-0219 owns the long-term design in
[N2-discharge-solver.md](N2-discharge-solver.md#a3-amendment-per-analysis-accounting-and-the-round-5-regression).
The [owning README](README.md) describes the implementation and the separate
slice-2a solver responsibility.

Forward commits from `f713970b08241a425f905a73fc62b2c642ef96e2`:

- `b705e669c`: shared work accounting across discharge, mentions, mutation and
  protected comparison; deterministic analysis-private caches and module-DAG pruning.
- `406638e06`: frozen scheduler jobs, per-analysis
  deadlines, reservation cap and complete per-identity results; scheduler accounting,
  controls and explicit driver wall bounds.

- `cec5574d5ff313063ef896c9b414b15fefca00b7`: include environment/source setup
  inside the named scheduler allowance and check deadlines on return as well as
  entry; add an exact scheduler-enumeration failure control.

`certificateDependency` through `validateCertificate` is byte-for-byte identical
at the base and implementation head. The only changes in the `Discharge` namespace
are the optional enclosing work-object field and the fuel-charge helper. No solver
repair, certificate policy, language/spec decision or budget increase is included.

## Focused validation

The six focused consumer controls passed before committing the scheduler: both
inline-match controls, the small and long projection exhaustion controls, missing
provenance (including exact dependent INCOMPLETE records), and generated-recursor
provenance.

Final runs against **`cec5574d5ff313063ef896c9b414b15fefca00b7`**, in `nix develop`:

| focused command | result |
| --- | --- |
| `vacuity_round5.py controls` | 32 discharge PASS, 0 FAIL; 5 certificate probes PASS, 0 FAIL |
| `vacuity_inline_match.py` | 2 PASS, 0 FAIL |
| `vacuity_reporting.py` | 5 exact error-report controls PASS; 12 budget controls PASS, 0 FAIL; 3 selected consumer controls PASS, 0 FAIL; exact replay-report rejection PASS |

The outer watchdogs were respectively 300, 90 and 180 seconds. No whole-suite,
whole-tree build, historical whole replay or full Grant/Own run was performed.
The ci-2 shell command below also passed `bash -n`.

The 12 A3 controls use production scheduler entry points and production malformed
Instance validation. They assert the exact budget reason, identity and usage grammar,
actual 20,000-work exhaustion, preserved independent FAILs, finite population and
reservation equations, unchanged earlier terminal records and no capped-body execution.
Nested helpers traverse a definition referencing the subject, not merely leaf terms.

## Grant/Own measurement: deferred to concrete-ci-2

Only the remote-tracking ref was fetched:

```
git fetch origin refs/heads/wip/spec-c5:refs/remotes/origin/wip/spec-c5
```

The exact source `6efdce6136752d51fc74265d61278d90937f24cd` exists and is an
ancestor of that fetched ref. No source branch was checked out or modified.

**No current Grant/Own census was run on this shared host.** The existing
[A3 diagnosis](N2-A3-budget-diagnosis.md) records tens of seconds for individual
20,000-work obligations and a previous 111-second scope run that stopped under
cumulative starvation. Removing starvation admits every remaining independent
analysis, so that previous wall measurement does not bound this schedule. A
whole-scope measurement is outside the authorized light focused checks here.

Consequently the current counts by FAIL/INCOMPLETE class are **unmeasured**, and
whether all five `instance-application` FAILs appear is **unverified**. The old
259 FAIL / 0 INCOMPLETE result at `550fab85e` is not a result for this checker.
Named round-5 cost failures remain legitimate INCOMPLETE results; no historical
completion or five-FAIL preservation is inferred from the focused controls.

After the coordinator pushes the implementation, run the following **on
concrete-ci-2** in its checkout of the exact implementation SHA. Every command
is inside the outer watchdog; the driver's audit watchdog remains unchanged
(default first attempt 120 seconds, retry 240 seconds). A watchdog result is
infrastructure INCOMPLETE, not an analysis census.

```bash
nix develop --command bash scripts/checks/lib/bounded.sh 900 bash -c '
  set -euo pipefail
  test "$(git rev-parse HEAD)" = cec5574d5ff313063ef896c9b414b15fefca00b7
  scratch=$(mktemp -d)
  trap '\''rm -rf "$scratch"'\'' EXIT
  git fetch origin refs/heads/wip/spec-c5:refs/remotes/origin/wip/spec-c5
  git archive --format=tar 6efdce6136752d51fc74265d61278d90937f24cd > "$scratch/source.tar"
  mkdir "$scratch/source"
  tar -xf "$scratch/source.tar" -C "$scratch/source"
  python3 scripts/proofs/vacuity_driver.py --source "$scratch/source" --scope ConcreteSpec.Proofs.Grant,ConcreteSpec.Proofs.Own
'
```

Save the complete stdout and exit status. Count `FAIL <code>` and
`INCOMPLETE <code>` records by code, and budget INCOMPLETE records additionally
by `reason`; report the five retained `FAIL instance-application` names explicitly.
The `analysis-summary` must satisfy `reported=N`, `attempted+unrun=N` and
`completed+incomplete=N`; an enumerator/watchdog failure does not satisfy that
requirement merely because partial diagnostics exist.

No push, review tooling or landing was performed by this writer. Reviews of
record and any scheduling rows are the coordinator's responsibility. Reviewers
must do the best long-term thing rather than cut corners.

## Amendment 3 implementation fix round 1 (2026-11-09)

The ci-2 N12r census at `a55d5880975f` is **INCOMPLETE**, not a successful
population result: the 120/240s watchdog discarded the audit after 389 seconds
and hid all five historical instance-application FAILs. No full-scope census is
run by this writer. The coordinator's wide-watchdog measurement is diagnostic
only and has not yet been supplied here.

F1: planning and parsing have private states; each analysis owns its resolutions
and findings, and terminal verdicts precede global deduplication. Unknown Instance
law controls require the exact `FAIL unknown-name` on classification both without
a cap and with `C=100000`, which stops before evidence-tags. Repeated failing
Planted jobs require FAIL terminals even with identical emitted diagnostics;
repeated Instance job controls require two FAIL terminals and one global finding.

F2: the shared DAG control measures depth 10 at **4093 -> 41** work units,
`cached=2 root=false -> cached=22 root=true`; depth 15 changes from exact
`work-budget` exhaustion to **61** work units, `cached=32 root=true` (bound 63).
This measures a small local synthetic scope, not the cause or wall-time recovery
of Grant/Own. Restoring the early-exit bug must produce exact
`dag-root-not-cached` and `work-budget` diagnostics. Other traversal ownership
and memoization were audited as documented in README.

W: a BEGIN/P/READY/JOB/START/F/END journal makes results recoverable after process watchdog
expiry. The control plants a 15s sleep at evidence-tags and uses 4/8s watchdogs;
it requires the earlier exact unknown-name FAIL, named interrupted/unrun
INCOMPLETEs, and all three population equations. A retry-journal control also
requires an earlier completed exact FAIL to survive a less-progress retry and a
torn final record. Planning restores Meta state on both success and error.
Production watchdogs and work
allowances are unchanged. Per-analysis interpretation stays **assumed, pending
owner**, with shared-budget option B still open. README records wall-policy
options and the coordinator-owned future worker design without allocating IDs.

Coordinator ci-2 command, in this clone's exact final implementation commit
(substitute the reported implementation SHA for IMPLEMENTATION_SHA):

```bash
nix develop --command bash scripts/checks/lib/bounded.sh 900 bash -c '
  set -euo pipefail
  test "$(git rev-parse HEAD)" = IMPLEMENTATION_SHA
  scratch=$(mktemp -d)
  trap '\''rm -rf "$scratch"'\'' EXIT
  git archive --format=tar 6efdce6136752d51fc74265d61278d90937f24cd > "$scratch/source.tar"
  mkdir "$scratch/source"
  tar -xf "$scratch/source.tar" -C "$scratch/source"
  python3 scripts/proofs/vacuity_driver.py --source "$scratch/source" --scope ConcreteSpec.Proofs.Grant,ConcreteSpec.Proofs.Own
'
```

Record stdout, rc, elapsed time, the three equations, class/reason counts, and
whether each of the five instance-application names appears. Recovery accounting
is not evidence that every independent analysis completed. Reviews of record
must do the best long-term thing rather than cut corners. No push, review tool,
OD decision, certificate/solver rewrite, or ROADMAP edit is authorized here.

### Fix-round-1 commits and validation record

Forward commits: `f5446bc9b` (analysis/planning state ownership), `a3fb150af`
(compound and protected-expression DAG memoization), `bafa4f51c` (durable wall
recovery), `45cbd074e` (stronger exact controls), and
`96cec5321c6ad7f154ef8eea684688acd3ee304f` (cross-attempt durability and successful
planning Meta-state restoration). The final consumer blob is
`90ece64f34ae85d0e296d43ca15748d60969797c`.

At `96cec5321c6ad7f154ef8eea684688acd3ee304f`, in `nix develop`,
`vacuity_reporting.py` passed: retry-journal/torn-record recovery; the real planted
wall watchdog with exact interrupted and unrun diagnostics; all five existing
error-report controls; uncapped and capped unknown-name classification; repeated
Planted verdicts; all 13 budget controls (including repeated Instance verdicts);
three selected consumer controls; and exact replay-report rejection. Outer bound:
600s. This final run covers the Meta-restoration and journal-reconciliation delta.

The standalone budget controls also passed (13/13), including the two DAG probes:
normal depth-15 root cached at 61 work units; early-exit mutation rejected by exact
`dag-root-not-cached` and `work-budget`. Those helper/control bytes are unchanged
since `45cbd074e`. The standalone certificate controls passed 5/5 at that revision;
`certificateDependency` through `end Discharge` was separately confirmed identical
to `a55d58809`. Earlier integration runs also passed `vacuity_round5.py controls`
(32/32 plus 5/5 certificate probes) and `vacuity_inline_match.py` (2/2); the full
fixture run below exercises those same consumer cases on the committed journal
implementation. No full compiler/spec suite or whole-scope census was run.

Full fixture controls: **127 PASS, 0 FAIL**, outer bound 900s, using the
`bafa4f51c` compiled consumer/driver snapshot (consumer also unchanged at
`45cbd074e`). This includes all 32 round-5 and both inline-match consumer cases.
The later `96cec5321` delta has the complete final reporting run above, including
freshly compiled normal and injected consumers. All focused processes exited 0;
none was killed by its outer watchdog.

The coordinator has not supplied the wide-watchdog diagnostic measurement during
this run. The historical-scope F2 wall-cost improvement and retention of all five
instance-application FAILs therefore remain **unmeasured/unverified** pending the
ci-2 command above. The small local DAG measurements are not a substitute for it.

## Fix round 2: dependency measurement and reuse

The coordinator's N14 at `e692fa8e9` is INCOMPLETE (389s, 1,808/2,152
attempted, 344 unrun). N14d with the diagnostic-only 2,400s watchdog takes
1,369s, attempts all 2,152, and leaves 119 INCOMPLETE. These are supplied
ci-2 measurements, not writer runs or a passing gate result.

A bounded single-instance replay of historical Own line 4258 instruments the
production analysis work calls. At the first exhausted charge it records:
`workConstants=9398`, `moduleDepends=8452`, `mayMention=1616`,
`mentionsVisit=475`, `mentions=8`, `mutate=52` (the attempted 20,001st
charge is included). Only 4,865 expression entries and 657 module entries exist;
seven root mention queries completed. Negative dependency closures were being
rediscovered across different roots. Completed-closure reuse fixes that defect,
without dropping cache-lookup charges or increasing any budget. The instrumented
heartbeat count is not comparable with a production census.

Focused budget validation: both DAG controls PASS, the new dependency control
completes with 2,430 units, its disabled-publication mutation reports exactly
`CONTROL work-budget work=20000`, and all 13 production scheduler/budget controls
PASS. No scope census or review tooling was run; coordinator reviews must do the
best long-term thing rather than cut corners.
