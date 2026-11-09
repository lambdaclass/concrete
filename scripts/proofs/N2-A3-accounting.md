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

### Remaining discovery redundancy and the historical FAILs

Only transparent definitions can be followed by `mentionsVisit`; performing a
module query on every constructor, recursor or opaque constant was redundant.
Completed `mayMention` answers are now memoized, and mutation prunes unrelated
imported bodies before traversing them. Cache-hit work charges remain unchanged.
The updated alias control completes at 2,330 units and adds a direct production
Instance pair with an explicit inline matcher: the unchanged contract must PASS,
and the changed branch must emit exactly `FAIL instance-application
DependencyControls.changed law=DependencyControls.law case=inline`.

Bounded historical single-Instance runs, source `6efdce6136752d51fc74265d61278d90937f24cd`:

| historical Own line / law | N14d supplied result | repaired work / verdict |
| --- | --- | --- |
| 4224 / step_next_erasure | work-budget, 20,000 | 18,822 / PASS |
| 4237 / step_request_erasure | work-budget, 20,000 | 19,312 / PASS |
| 4258 / Step_erasure | work-budget, 20,000 | 19,136 / PASS |

These PASS results do not silently discard the historical FAILs: in a private
consumer copy, restoring smart unfolding produces the exact `FAIL
instance-application` on each of the same three names, with identical work counts.
Their former comparison false positives were fixed independently by already
accepted `6fe932293`. The accounting defect prevented reaching that corrected
comparison. No semantic change to it is needed or included here.

The line-4258 completed charge profile is `workConstants=10819`,
`moduleDepends=7068`, `mayMention=355`, `mutate=170`, `mentionsVisit=156`,
`mentions=226`, `withProtected=342`, totaling 19,136. The earlier charge profile
stopped before comparison; comparing individual category totals across those
cut points is not a full-run speed comparison. Local cold-cache heartbeats and
wall times also must not be substituted for the coordinator's scope census.

### Refutation profiles and the remaining limit

A bounded cold single-instance baseline for Own line 4381 records 3,425
`refutableContext` calls costing 1,840,724 user heartbeats (raw profiler units
1,840,724,607), approximately 92% of the 2,000,000 allowance. Type refutation
accounts for about 149,931 user heartbeats; inference and whnf are much smaller.
Caching by exact FVarIds alone provides little reuse because opening binders
allocates fresh IDs. The repair keys completed refutation queries by closed,
alpha-canonical contexts, with immutable raw-identity front caches. All local
assumptions, declaration kinds, let values and local instance order remain in the
key; mutable contexts are excluded. The actual proof-state visited key and work
units are unchanged. No admitted certificate or proof is skipped.

The focused refutation controls require two searches across renamed equivalent
Nat contexts and a distinct False context; an erased-context mutation must report
exactly `CONTROL context-cache-crossed-context`. Both pass, including explicit
metavariable exclusion. All 13 production budget controls, both DAG probes and
both completed-dependency probes pass on the final consumer bytes. The latter
also require an exact PASS on the inline-contract witness and exact
`FAIL instance-application` on its changed branch.

Grant line 1478 still reaches 20,000 distinct states under the existing proof-state
identity and reports `false-discharge-budget`. Own lines 4381/4394 still reach
2,000,000 heartbeats. These are **INCOMPLETE**, not recovered verdicts. Further
proof-state/contradiction-search design belongs to R-0219/slice 2a; the current
search's fit within the owner budget is an open question, pending owner. No claim
that its current state graph is a minimal or unavoidable proof graph is made.

No whole-scope Grant/Own census was run locally. The supplied N14/N14d failure
and the surviving expensive single instances leave whole-run wall compatibility
unestablished. The owning README records the conflict and the costs of honest
wall-unrun reporting, sufficient wall capacity for per-analysis work, and option
B's shared budget. Per-analysis interpretation stays **assumed, pending owner**;
none is chosen and no budget/watchdog is raised.

The single-instance reproduction tool retains both Grant/Own imports in the
spec worker's order, even while selecting exactly one declaration. This preserves
the original environment rather than profiling a different import context.

### Front-cache context completeness

Final validation added a same-FVarId context replacement control. A raw-ID-only
front key could return a stale negative after `LocalContext.setType` changes a
Nat binder into a False hypothesis. The front key now includes full raw declaration
snapshots (type, value, let transparency, binder information and kind) and ordered
local instances. The canonical key explicitly retains let transparency too.
Renamed contexts still share completed results, but replacement contexts cannot
hit the old front entry. Exact planted reasons are
`CONTROL context-cache-crossed-context` (canonical-context erasure) and
`CONTROL raw-context-cache-crossed-type` (raw-type erasure); the normal four-search
control passes, including metavariable exclusion. This is an audit-cache repair,
not a spec/OD decision or a solver rewrite.

The complete raw snapshot is captured once per production proof node and refreshed
inside each newly opened lambda telescope. Related queries reuse that immutable
snapshot; standalone helper probes capture the current context themselves. The
same-ID mutation controls and all scheduler/budget controls still pass. This
removes repeated key construction while preserving complete keys and the existing
proof-state work charges.

## Fix-round-2 final record

Forward implementation commits: `cb46908dc` (completed negative closures),
`da10c9150` (provenance pruning, declaration-query memoization and inline contract
control), `031b5cbf4` (alpha-canonical refutation memoization), `14b33ee83`
(complete raw declaration keys and same-ID replacement control), and
`ecc9c5509976aed317459314614b20188969dd8d` (one immutable snapshot per proof node).
Final consumer blob: `68e722750082236752b5c597d1a7b607a0a00f77`.

The implementation was committed as focused checks passed, with no history
rewrite, push, review-tool invocation, spec edit, ROADMAP edit or ID allocation.
Both the original refutation procedures and `certificateDependency` through
`end Discharge` are byte-identical to `e692fa8e9`. The writer's final validation
uses exactly the consumer bytes in `ecc9c5509`, including commands started just
before that metadata-only commit.

[Raw named analysis records](audits/N2-A3-round2-measurements.txt) distinguish
intermediate checkpoints from the final complete-key implementation. Each wall
measurement is one cold local analysis, excludes module elaboration, and is not a
repeatable throughput guarantee on this shared host. Baseline and final runs
retain the spec worker's Grant/Own imports and use the same archived source.

The final cold per-analysis measurements are below. `HB` denotes user
heartbeats, not the raw profiler units; wall is analysis-only seconds. Every row
still labeled INCOMPLETE is an unavailable verdict.

| Instance (historical file:line) | baseline HB / work / seconds | final HB / work / seconds | final result |
| --- | --- | --- | --- |
| Grant:1478, acquire_bounded | 1,029,916 / 20,000 / 34.054 | 706,499 / 20,000 / 23.486 | INCOMPLETE false-discharge-budget |
| Own:4381, runFrom_erasure_of_accepted | 2,000,001 / 13,546 / 72.574 | 2,000,000 / 15,540 / 70.240 | INCOMPLETE heartbeats |
| Own:4394, run_stuck_iff_of_accepted | 2,000,001 / 13,546 / 74.275 | 2,000,005 / 15,540 / 69.834 | INCOMPLETE heartbeats |
| Own:4369, step_stuck_iff_of_accepted | 2,000,019 / 14,591 / 88.126 | 2,000,015 / 16,483 / 71.372 | INCOMPLETE heartbeats |

The small heartbeat overshoots are the existing runtime exception/check
observations, not larger configured allowances. Grant's heartbeat cost falls by
about 31%; the Own cases make more proof-state progress before the same heartbeat
cap, but **do not complete**. Earlier checkpoint records with less-complete front
keys have better intermediate cost numbers; they are not the final measurements
and are deliberately retained as such in the raw record.

The two retained historical failures were checked individually at checkpoint
`031b5cbf4`: Own line 4918 emits exactly `FAIL instance-application` for
`ConcreteSpec.Core.Own.evalArgs_ok_erasure`, and line 4931 does the same for
`ConcreteSpec.Core.Own.evalArgs_trap_erasure`; both have the initial-case key.
Together with the three completed erasure instances and their exact smart-unfolding
mutations, this accounts for all five historical application findings without
claiming a scope census.

### Focused checks and their revision

All commands ran in `nix develop` under the repository's bounded runner. The
[control transcript](audits/N2-A3-round2-controls.txt) retains every named result.

| check | result and revision |
| --- | --- |
| `vacuity_budget.py` | final `ecc9c5509` bytes: 13 scheduler/budget controls, 2 DAG probes, 2 dependency probes (each also validates the inline witness/rejection), 3 refutation probes; all PASS |
| `vacuity_round5.py controls` | final `ecc9c5509`: 32 PASS, 0 FAIL; embedded certificate probes 5 PASS, 0 FAIL |
| `vacuity_reporting.py` | checkpoint `031b5cbf4`: retry/torn-journal and planted wall recovery, five exact injected errors, capped/uncapped name classification, repeated findings, 13 budgets, three selected consumers, exact replay rejection; all PASS |
| `vacuity_controls.py` via `vacuity_driver.py --controls` | checkpoint `031b5cbf4`: 127 PASS, 0 FAIL; standalone reporting record listed separately |
| `vacuity_certificates.py` | checkpoint `031b5cbf4`: 5 PASS, 0 FAIL; final round-5 run repeats all five |
| `vacuity_inline_match.py` | checkpoint `031b5cbf4`: 2 PASS, 0 FAIL; final budget probes also exercise the inline-contract shape |
| `vacuity_fixtures.py` | 10 exact historical provenance excerpts PASS; no historical-scope replay |

The complete-key/snapshot delta after `031b5cbf4` is covered by the final budget
and round-5/certificate runs, including both exact context-erasure mutations.
The 127-control and standalone reporting runs are recorded against their actual
checkpoint rather than being relabeled as final-revision runs. No test watchdog
expiry is counted as a PASS. Analysis-budget INCOMPLETEs in the profile transcript
are measurements, not successful control audits.

### Exact coordinator command on concrete-ci-2

Run in the coordinator's clone at the final reported branch commit after the
coordinator pushes it. This prints the exact runtime SHA and checks the audited
consumer blob. The 960s infrastructure bound includes `nix develop`; the 900s bound encloses
the in-shell command. The actual driver still uses its unchanged default 120/240s watchdog and default allowances.
The pinned source object must be present, as for N14/N14d.

```bash
bash scripts/checks/lib/bounded.sh 960 nix develop --command \
  bash scripts/checks/lib/bounded.sh 900 bash -c '
    set -euo pipefail
    git rev-parse HEAD
    test "$(git rev-parse HEAD:scripts/proofs/vacuity.lean)" = cda204c1bd45de5f8716c804b367dc00c68770e5
    scratch=$(mktemp -d)
    trap '\''rm -rf "$scratch"'\'' EXIT
    git archive --format=tar 6efdce6136752d51fc74265d61278d90937f24cd > "$scratch/source.tar"
    mkdir "$scratch/source"
    tar -xf "$scratch/source.tar" -C "$scratch/source"
    set +e
    python3 scripts/proofs/vacuity_driver.py --source "$scratch/source" --scope ConcreteSpec.Proofs.Grant,ConcreteSpec.Proofs.Own
    rc=$?
    printf "N2 CI2 EXIT %s\n" "$rc"
    exit "$rc"
  '
```

Save the full stdout and rc. Report N/attempted/unrun/completed/incomplete,
FAIL classes, INCOMPLETE reasons, all five historical application identities and
the three population equations. No wide-watchdog diagnostic may replace this
qualifying gate result. The whole-scope result remains pending the coordinator;
remaining wall-policy and search/budget questions remain pending owner.

Coordinator reviewers must do the best long-term thing rather than cut corners.

### Fix round 3: structural cache identity

N15a F1 reproduced on `296cbad05` before repair, with a bounded consumer-only
Lean probe: orders `[b1,b2]` produced `false,false` and `[b2,b1]` produced
`true,true`; expected bad-discharge answers are `false,true` and `true,false`.
Replacing only the function binder type using `LocalContext.setType` gave
`before=true after=true fresh=false` (the probe uses `withFreshCache` to isolate
Lean's own infer-type cache). The repaired answers match all three expectations.

The structural owner is the N2 analysis cache identity contract, not a budget
exception. Every expression-keyed audit map now uses `Expr.equal`, including
pre-existing projection, graph interning, dependency, mutation, collection and
replacement maps. The ordinary Expr hash remains lawful: collisions are resolved
by full equality. Closed refutation keys retain binder names/info and declaration
kind/value; raw keys additionally retain declaration identities, names and indices.
Both include options and meta configuration; visited uses the complete raw context.
Mutable expressions/contexts and executable unfolding predicates bypass refutation
cache publication and lookup. Immutable environment is scoped to each analysis.
`certificateDependency` through `end Discharge` remains byte-identical.

`vacuity_cache_identity.py` checks both orders, same-FVarId replacement, binder
info/name/universe separation in interning and mutation, and options/mode keys.
Per-analysis interpretation and wall policy remain **pending owner**.

The bounded Own:4269 planted reproduction confirms the reported work exhaustion:
20,000 work, 833 instrumented heartbeats; charges were 10,838 constant traversal,
7,925 module ancestry, 483 mutation, 367 may-mention, 234 mentions and 154 DFS.
The importer-order repair rejects forward module references without walking
ancestry: Own:4269 now PASS at 12,941 work / 680 diagnostic heartbeats (34 ms
analysis). Charges: constants 10,841, module ancestry 263, may-mention 417,
mutation 651, withProtected 354, mentions 258, DFS 157. Thus the former bound
was dominated by avoidable module traversal, not a genuine >20,000 obligation.
The control checks every loaded import edge respects Lean's import-before-
importer invariant, plus positive, negative and self dependency queries.

### Demand-driven refutation (round 3)

Refutation certificates exempt suspicious discharges. Every result type is checked before following its producer; lambda contexts
are refuted only after their body reports a suspicious discharge. No
exemption is granted without the same checked certificate. A harmless lambda
control asserts zero refutation-cache entries. Exact supplied-False, unreachable-
branch and both BinderInfo orders remain required controls.

Diagnostic Own:4381 before this change: 70,227 ms, 2,000,014 heartbeats, 12,295
work; 3,025 context searches consumed 1,503,882,132 raw heartbeat ticks. Merely
deferring result-type decide was insufficient (70,302 ms, 2,000,022 heartbeats).
Demand-driven context certificates: 17,616 ms, 614,672 heartbeats, work=20,000;
zero context searches. Own:4394: 18,388 ms / 614,672 heartbeats; Own:4369:
15,628 ms / 499,945 heartbeats; Grant:1478: 12,234 ms / 402,658 heartbeats. All
four remain INCOMPLETE `false-discharge-budget` at 20,000 work. These diagnostic
probes instrument the consumer, so their heartbeat counts are not gate results.
The remaining distinct proof/context traversal is an open structural sharing and
budget question pending owner, owned by R-0219 and the reserved slice-2a line;
this writer does not change its protected dependency/certificate region.

The other two planted companions also complete: Own:4230 PASS at 12,418 work /
624 diagnostic heartbeats; Own:4247 PASS at 13,224 work / 697 heartbeats.
Own:4258 instance PASS at 12,324 work / 4,510 diagnostic heartbeats, compared
with 19,136 work previously. Constant enumeration (10,837) now dominates it;
module ancestry is 232. The ~38% headroom comes from an import-graph invariant,
not a larger allowance or an empirical cutoff.

Cache audit boundary: source/name resolution maps are scoped to one inventory;
module/name dependency maps to one immutable environment; constants/mentions
ignore binder annotations semantically but now also use exact syntax. Mutation,
replacement, projection answers and graph term interning preserve exact syntax.
Discharge query/template maps contain structural IDs and explicit modes/levels.
The protected certificate dependency pending/completed keys still encode FVarIds
and require reconciliation by slice-2a if it permits same-ID declaration updates;
this writer has deliberately not changed that region or claimed it audited away.

Open supplied producers remain evidence even when their argument contains a
hypothetical variable: the focused `reachable-open-impossible-negative` and
`joint-open-supplied-negative` controls require exactly `instance-false-discharge`.
The refutation demand change must retain that rule; a local four-case rerun
passes both negatives and both hypothetical positives.

### Intermediate round-3 receipts (before projection context repair)

Consumer code commit `e5b2dc3c1fcb77ddf7532eb39c00769979cb70bd`, blob
`68e722750082236752b5c597d1a7b607a0a00f77`, SHA256
`29d2e9ab043508d25881771923cd173085fc6cbe29990133cfb766cabd85246d`.
Subsequent commits change tests/records only. The final identity controls are at
`7d155cb0a65a77c57f01ddc203df5e5e2668b7f7`. The following final-byte
**instrumented diagnostic** single-analysis results supersede intermediate
numbers above (heartbeats include instrumentation; no gate result is inferred):

| Source line | Stage | Result | Work / 20,000 | Heartbeats / 2,000,000 |
|---|---|---|---:|---:|
| Own:4381 | instance | INCOMPLETE | 20000 | 632756 |
| Own:4394 | instance | INCOMPLETE | 20000 | 632756 |
| Own:4369 | instance | INCOMPLETE | 20000 | 513301 |
| Grant:1478 | instance | INCOMPLETE | 20000 | 413006 |
| Own:4230 | planted | PASS | 12418 | 624 |
| Own:4247 | planted | PASS | 13224 | 697 |
| Own:4269 | planted | PASS | 12941 | 680 |
| Own:4224 | instance | PASS | 12044 | 4486 |
| Own:4237 | instance | PASS | 12534 | 4520 |
| Own:4258 | instance | PASS | 12324 | 4510 |

All four heavy INCOMPLETEs have the exact reason `false-discharge-budget`.
Detailed charges and analysis milliseconds are in
[audits/N2-A3-round3-measurements.txt](audits/N2-A3-round3-measurements.txt).
The batch infrastructure watchdog stopped during preparation of the ninth
named sample, after eight terminal results; the last two samples were rerun
individually under 180s infrastructure bounds. No analysis budget or production
watchdog was changed. Each call selects exactly one named example.

Focused controls: reporting PASS; budget 20 original probes plus 17 identity
checks PASS; round-5 32 PASS plus 5 certificates PASS; standalone certificates
5 PASS; inline match 2 PASS; provenance 10 byte-exact excerpts PASS. The
standalone identity run adds two planted variants with exact stale-answer/error
checks. Per-control receipts are in
[audits/N2-A3-round3-controls.txt](audits/N2-A3-round3-controls.txt).

On **concrete-ci-2**, from the coordinator clone after pushing the final commit:

```bash
bash scripts/proofs/audits/N2-A3-round3-ci2.sh
```

This is the exact command block above, with the repaired consumer blob pinned.
It preserves the default 120/240s production watchdog. The per-analysis
interpretation and whole-scope wall policy remain **pending owner**.

The first final `vacuity_driver.py --controls` invocation reached its 600s outer
infrastructure timeout without a terminal result (Python output was buffered);
it is **not** a pass. It is rerun with unbuffered progress and a 1,500s outer
test-harness bound. This changes neither a production gate allowance nor the
120/240s per-audit watchdog. Standalone focused checks above remain independent
completed results.

### Projection cache audit completion

The pre-existing projection-answer cache calls `isProof`, so its complete key
also includes the raw local context, options and mode, with the same mutable-
context bypass. The common key definitions now precede both consumers. A same-
FVarId Nat-to-False control requires two projection entries, and an expression-
only planted projection cache must fail with exactly `projection-context-collapsed`.
This repairs the general cache contract; no claim is made that an unchanged
well-typed production proof previously switched universes in place.

The preceding receipts/profiles remain explicitly tied to consumer commit e5b2dc3c1.
This final cache-contract addition changes projection analyses; instance/planted
algorithms and their measured work are unchanged. Final-byte focused reruns are
recorded separately below.

Projection-context checkpoint code: `37b2a0b1a3153493423d90e6070c8da9cec5718a`, blob
`dfee7d3d639677deb628526c54449ea85c6d90d1`, SHA256
`ad9abbba26ebb0fabaeffe1096cfd6c6b764aed2cdd93f08fd6451789f2795b7`.
The last identity controls are committed at `2219b4add4e566d46cbfd4fb08b9465f06407fb5`.
Every standalone requested focused check passed on these consumer bytes:
reporting; budget (20 original probes and 18 identity checks); round-5
(32 controls plus 5 certificates); standalone certificates (5); inline match (2);
fixture provenance (10 exact excerpts). The final standalone identity invocation
passes 19 positive/negative checks plus 3 planted variants, including both
refuted-type caches and the projection context key. Exact per-control results:
[audits/N2-A3-round3-final-controls.txt](audits/N2-A3-round3-final-controls.txt).
The complete driver control invocation is pending and is not counted as passed.

Three final-byte instrumented spot checks confirm unchanged instance/planted
work: Own:4381 INCOMPLETE at 20,000 work / 632,756 heartbeats (17,863 ms);
Own:4269 planted PASS at 12,941 / 680 (35 ms); Own:4258 instance PASS at
12,324 / 4,510 (266 ms). Protected region SHA256 remains
`474a74f351984565847d5239a86707b389970bf1eba33d2b45c4486c1ec11fd4`,
byte-identical to `296cbad05`. The ci-2 command script now pins the final blob.

### Preserve result-type obligations (final audit correction)

An additional bounded negative at consumer `37b2a0b1a` exposed the intermediate
result-type deferral: with `transportP : Prop`, `suppliedTransportP : transportP`,
and `transportEquality : transportP = False`, Eq transport produces a closed
False. The probe returned `TRANSPORT false-result bad=false` while
`refutableType False = true`. This is a confirmed regression, not an accepted
exemption. The result-type check is restored before unfolding every node.
Only the costly context-certificate search remains demand-driven. The prior
profiles/receipts stay attached to their explicit earlier consumer commits; they
are superseded by the final receipts below. No gate budget is changed.

The new exact control requires both `falseDischarge = true` and one
`FAIL instance-false-discharge` from checkInstance. Removing the result-type
obligation in a private consumer must fail with exactly
`transport-false-discharge-missed`. This prevents gaining speed by missing
joint supplied contradictions hidden by nonempty recursors.

### Authoritative final code and measurements

Code `e07d3c6cf2cb823ec1183ac1cedc5ab516666d93`; blob `cda204c1bd45de5f8716c804b367dc00c68770e5`; SHA256
`d44ab851226f4ab4bd4b16bd606899e2c009eb30b8d22bd0089df5943e683dd5`. Result-type refutation is preserved before unfolding; only context
certificate search is demand-driven. The transport regression is repaired and
all four planted cache/obligation variants fail for their exact intended reasons.
All requested standalone focused checks pass on these final bytes, with 24
standalone identity/transport checks (20 production checks plus 4 planted variants).
Per-control results are in
[audits/N2-A3-round3-validated-controls.txt](audits/N2-A3-round3-validated-controls.txt).
The complete final driver invocation exited 0: 127 core controls PASS, followed
by all reporting controls PASS. Its terminal receipt is included in the linked
validated-controls file.

The following ten single-example samples all use these final consumer bytes.
They are instrumented diagnostics, not a whole-scope gate result.

| Source | Stage | Result | Work / 20,000 | HB / 2,000,000 | Analysis ms |
|---|---|---|---:|---:|---:|
| Own:4381 | instance | INCOMPLETE | 20000 | 685930 | 19849 |
| Own:4269 | planted | PASS | 12941 | 680 | 35 |
| Own:4258 | instance | PASS | 12324 | 4510 | 265 |
| Own:4394 | instance | INCOMPLETE | 20000 | 685930 | 19466 |
| Own:4369 | instance | INCOMPLETE | 20000 | 558386 | 16206 |
| Grant:1478 | instance | INCOMPLETE | 20000 | 447040 | 13144 |
| Own:4230 | planted | PASS | 12418 | 624 | 32 |
| Own:4247 | planted | PASS | 13224 | 697 | 36 |
| Own:4224 | instance | PASS | 12044 | 4486 | 252 |
| Own:4237 | instance | PASS | 12534 | 4520 | 257 |

All heavy INCOMPLETEs have exactly `false-discharge-budget`; none is passed
or omitted. Own:4381 improves from 70,227 ms / 2,000,014 heartbeats to 19,849 ms /
685,930 heartbeats while preserving the result obligation; context search calls
fall from 3,025 to zero on that harmless prefix. Further structural sharing
remains with R-0219 and slice-2a. Neither per-analysis interpretation nor wall
policy is decided here; both remain pending owner. The pinned ci-2 command is
`bash scripts/proofs/audits/N2-A3-round3-ci2.sh` on concrete-ci-2.

Final terminal command (local focused controls only):

```bash
timeout 1500 nix develop -c python3 -u scripts/proofs/vacuity_driver.py --controls
```

Exit 0, `N2 controls: 127 PASS, 0 FAIL`, plus reporting PASS. Every requested
focused check has now completed on the final consumer bytes. The 1,500s bound
is test-harness infrastructure, not a production budget change. The code and
identity/transport controls remain at e07d3c6cf; later commits contain records only.
No local whole-scope census, publication, review tooling, language/spec decision,
or owner budget/wall policy decision was performed. The reserved certificate
region remains byte-identical to 296cbad05. Full scope goes to the coordinator
on concrete-ci-2 with the pinned command above.


### Fix round 4: confirmed proof-body omission (A)

Baseline `30c57a17c499254298eb194d2e47776d5296022d` reproduced the exact
same-source differential before edits: def helper → exit 1, exactly
`FAIL projection-law ConcreteSpecControl.growth`; theorem/opaque → exit 0,
N2 PASS. The full baseline and per-analysis counters are retained in
[audits/N2-A3-round4-projection-before.txt](audits/N2-A3-round4-projection-before.txt).
Permanent driver controls retain all three forms. Raw proof delta now explicitly
reads theorem/opaque bodies, substitutes actual levels/arguments, and uses the
existing complete expression/context memo table and 4,096 projection-node bound.
Unavailable proof bodies and unsupported expressions raise INCOMPLETE, with an
exact unavailable-body control. Nonproof nodes and kernel primitives have explicit
classification; a nullary arithmetic proof constructor is not premise projection.
No computational conversion transparency or budgets changed.

The reserved `certificateDependency` through `end Discharge` region is unchanged.
Slice-2a has the same directStep defect and must carry A's proof-body traversal,
nullary-constructor distinction and unavailable-body handling when reconciling;
its certificate evaluator is not a substitute for this projection analysis.
B's typed protection/comparison also belongs to reconciliation of the surrounding
consumer, independently of certificate transfer. Do not overwrite either fix with
the inherited copies from 2a.

**Open contract question C, pending owner:** must all Law and Planted supplied
proofs be free of supplied contradictions, including those hidden only inside
helper theorem bodies? The confirmed `hidden-law-body` and `hidden-planted-body`
controls currently give N2 PASS and are retained as observations of current
behavior. No owner answer is inferred. A yes needs bounded origin-sensitive
validation rooted at each supplied proof, actual level/argument substitution,
sharing by instantiated dependency and local context, and preservation of
supplied/hypothetical provenance through all transfers. Unsupported/missing
transfers would be INCOMPLETE. This sketch is not implemented. The coordinator
assigns the owning roadmap row in the sweep; R-0219 owns the current N2 audit.
Per-analysis interpretation and whole-scope wall policy remain pending owner.
Coordinator reviewers must do the best long-term thing rather than cut corners.


### Fix round 4: typed protection and complete comparison (B)

The structural fix rebuilds subject structures with their original representation
fields, abstracting only computational fields, and typechecks protected terms.
A second typed DAG traversal shares minimal subject-dependent computations at
arbitrary typed results, preserving every motive, binder domain, universe and
constructor alternative. Derived values (including sparse-matcher constructor
masks) use that same result. Type-only occurrences are not computations. Beta
specialization removes phantom matcher parameters; used-binder closure preserves
real parameter dependencies. Matcher definitions are compared by actual ordinary
conversion, with no name/shape exemptions. Every transformed term is checked;
unsupported or ill-typed protection is INCOMPLETE. Existing instance head,
telescope, closed argument, evidence-kind and false-discharge checks remain.

Focused typed control receipts are in
[audits/N2-A3-round4-typed-contracts.txt](audits/N2-A3-round4-typed-contracts.txt).
Five positive contracts PASS: inferred/restated Ok and Trap, and restated Ok
on a concrete trap execution. `Confirm.changedStoreOk` and
`Confirm.unselectedChangedStoreOk` both FAIL with exactly
`instance-application`, the expected law, and case `typed`. Thus even an
unselected success branch is checked. No invalid-projection exception is used
as the counterfeit's rejection evidence.

| Contract | Result | Work / 20,000 | HB / 2,000,000 | Analysis ms |
|---|---|---:|---:|---:|
| inferredOk | PASS | 5584 | 4301 | 225 |
| restatedOk | PASS | 6577 | 4367 | 222 |
| inferredTrap | PASS | 5102 | 4260 | 224 |
| restatedTrap | PASS | 5837 | 4316 | 222 |
| changedStoreOk | FAIL instance-application | 6585 | 4371 | 228 |
| unselectedRestatedOk | PASS | 6529 | 4362 | 229 |
| unselectedChangedStoreOk | FAIL instance-application | 6537 | 4366 | 226 |

These are uninstrumented isolated production analyses on the final consumer
bytes of this fix; source is pinned to `6efdce6136752d51fc74265d61278d90937f24cd`.
They are single-contract measurements, not a scope census. The original B
reproducer's inferred/restated true/false split and independent ill-typed-term
checks are recorded in the confirmation brief. No budget was changed to obtain
these results. `certificateDependency` through `end Discharge` remains untouched.
