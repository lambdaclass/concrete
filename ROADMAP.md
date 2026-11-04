# Concrete Roadmap

Concrete aims to be a systems language with linear ownership, second-class references and
explicit capabilities, verified from source to native execution. Accepted programs preserve memory,
ownership and authority guarantees; their evidence is compositional, transferable and independently
replayable. The [product direction](docs/project/ROADMAP_DIRECTION.md#product-north-star-compositional-transferable-evidence)
and [verified-compiler decisions](docs/project/ROADMAP_DECISIONS.md#decisions) define that aim.

**Lean is the only language source.** `ConcreteSpec` holds Core syntax, independent typing,
ownership and effect judgments, execution semantics, surface desugaring and conformance examples.
Those definitions are the specification: each rule is readable Lean with a plain-words doc comment
and its conformance examples, and no prose chapter is generated from them. The rule registry and
reference interpreter come from the same definitions; derived machine views have a staleness gate.
See the [Lean-source decision](docs/project/ROADMAP_DECISIONS.md#2026-11-01--lean-as-the-only-source)
and its [2026-11-04 revision](docs/project/ROADMAP_DECISIONS.md#2026-11-04--lean-is-the-specification).

**The compiler is built from the spec.** The new compiler imports `ConcreteSpec`, calls its
elaborator and builds a proved lowering chain below Core. Slice 1 supplies a simple, explicitly
unverified BIR → LLVM printer; the Lean Compiler Backend follows. Once slice 1 is accepted, the old
pipeline and its internal gates are deleted together. Programs, regressions, examples, std, CI,
documentation and research are retained; legacy evidence remains replayable from the frozen tag,
and the browser compiler runs from that tag until covered. The [new-compiler decision and retention
contract](docs/project/ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
define the full boundary.

**Authority uses permissions and values.** `with(...)` is compile-time permission; linear values
are resources. Operations require both, and `main` consumes the target's grant values. Allocation
uses explicit allocator values and is fallible; filesystem lookup is confined through directory
values. The [authority decision](docs/project/ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--authority-design)
links the complete rules and language answers.

**The specification comes first.** Open language questions remain undecided until the owner
answers them. Audit and absorption assign every rule an owner; each slice supplies its Lean rules with
doc comments, refusal coverage, conformance examples, matching implementation and outside-reader
test, and retires the prose chapter text those rules replace. The [specification criteria](docs/project/ROADMAP_DECISIONS.md#2026-10-30--specification-first)
remain binding through A–D. Other documentation cites the Lean rules and removes contradictions
as slices land, under the [documentation rule](docs/project/ROADMAP_DECISIONS.md#2026-10-30--documentation-follows-the-spec).

**Proofs grow along one path.** State the semantic contract and final theorem first, including
outcomes, traces, authority, resources and the values-to-bytes boundary. Then build executable Core
semantics and soundness, followed by a minimal end-to-end path whose passes preserve behaviour and
re-establish invariants. Functional models, open-module correctness and C contracts, ghost permissions,
SMT-checked lowering and proof-producing equality saturation follow in that order. Parsing, linking
and native-image assumptions stay explicit; only kernel-checked proofs count. The [proof-order
record](docs/project/ROADMAP_DECISIONS.md#2026-11-01--compiler-proof-order) states the obligations
and owner-review boundaries.

## Where we are

The legacy compiler is frozen at `legacy-compiler-final` at commit 066c9d9b8. The language decisions
are recorded; the shape of Core is open for the owner's target Core proposal decision.

**Where things live.** The [task register](docs/project/ROADMAP_TASKS.md) holds A, B and T tasks;
the [backlog](docs/project/ROADMAP_BACKLOG.md) holds later tasks and the pull rules.
The [spec](docs/spec/README.md) owns language rules; the
[verified-pipeline plan](docs/compiler/VERIFIED_PIPELINE.md) holds the target proof architecture.
The [direction](docs/project/ROADMAP_DIRECTION.md) holds standing reference and product goals.

## Phases

| phase | goal | done when |
|---|---|---|
| A. Core and theorem | the spec audit of 2026-11-02 is disposed of (its gaps and duplications assigned to owners); the target Core proposal is decided by the owner, the whole shape at once and before implementation; the semantic contract with the named values-to-bytes boundary; the end-to-end theorem stated in Lean with its holes; the trusted base listed | Core approved; the theorem statement and trusted base on main |
| B. Slice 1 | `ConcreteSpec` and the new compiler for slice 1: scalars, `let`, the move and use-after-move refusal of an empty struct without `Copy` consumed by a struct-destructuring `let`, one checked operation, return or a defined trap, from parsing through native execution | the slice's rules are judgments, independent of the checker and elaborator, whose correspondence is proved; fuel exhaustion is distinct from divergence; LLVM output is labelled unverified, and the lexer, model-translation and native-image obligations are explicit; its examples pass through the spec and the native build; its theorem is proved; its rules are readable Lean with doc comments and examples, and the prose they replace is retired; the outside-reader test, run on the Lean rules and examples, passes. Then the old pipeline is deleted from main under the retention contract of the new-compiler decision |
| C. File-copy | slices ordered by the file-copy program's dependencies: directories, `File`, `with(Fs)`, fallible operations, explicit cleanup on every error path, `defer`; chapters retired as Lean replaces them; std, examples and tests migrate as their constructs land | the file-copy program, its failure paths included, builds natively from the new compiler with its theorems; **0.1 is cut here**, with the supported subset, the unsupported constructs and the LLVM assumptions stated |
| D. Whole language | the remaining constructs as slices; every prose chapter replaced by readable Lean rules; the conformance suite complete; each paused workload (RocksDB slice 1, the ethlambda workloads, the usability track) resumes when the constructs it needs are supported | every construct of the decided language is a slice, and no prose chapter is left |
| E. Verified growth | the proof order's later items: functional models, open modules and the C boundary, ghost permissions for trusted code, SMT-checked lowering, proof-producing equality saturation, the Lean Compiler Backend native path, the verified parser (lexer, semantic actions and parser execution included); richer verification: typed contracts, exact contract identity, two-state contracts and frames, checked totality, the specification library | each item's theorem with its residual assumptions stated |
| F. Evidence as a product | provenance, the canonical verification task, receipts and the independent consumer, rebuilt on the new Core (minimal provenance and task identity already accompany the first authoritative proof claims) | receipt v1 frozen |
| G. Release and ecosystem | external users, flagships, packages, 1.0 | external-user acceptance, package evidence, conformance, compatibility, distribution and maintenance commitments hold for the released subset |
| T. Development throughput | R-0539 – R-0547 and R-0513, beside the compiler sequence | the targets and item exits in [Development throughput](#development-throughput) hold |

## The queue

| row | goal | owning tasks | done when |
|---|---|---|---|
| A1 | Dispose of the spec audit and draft the boundaries Core must satisfy | R-0555, R-0481, R-0010, R-0447, R-0554, R-0511, R-0211, R-0230 | Each changed rule, gap and duplication has an owner and acceptance artifact; defect views distinguish compilers; [boundary drafts](docs/compiler/A1_BOUNDARIES.md) exist. |
| A2 | Decide the whole target Core shape before implementation | R-0312, R-0316 | The owner approves the target Core proposal, drafted against A1's artifacts, including identities, memory and interface boundaries; before Core is frozen, representative design proofs are checked in Lean for allocator dependencies with reset generativity, deferred cleanup with allocators and loans, heap opening and mutable lending, and one values-to-bytes lemma. If A3 exposes a substantive change, A2 is revisited. |
| A3 | Formalize the approved semantic contract and theorem in Lean | R-0496, R-0500, R-0502, R-0506, R-0508, R-0509, R-0480, R-0223, R-0227, R-0242, R-0464, R-0266 | The theorem with its holes, named values-to-bytes boundary and trusted-base inventory is on main. |
| B1 | Build ConcreteSpec and slice 1 with its theorem | R-0491, R-0492, R-0493, R-0497, R-0498, R-0499, R-0501, R-0503, R-0504, R-0507, R-0514, R-0552, R-0521, R-0528, R-0004, R-0435, R-0440, R-0058, R-0073, R-0074, R-0075, R-0076, R-0077, R-0079, R-0080, R-0082, R-0085, R-0090, R-0107, R-0114, R-0115, R-0116, R-0191, R-0203, R-0205, R-0207, R-0210, R-0217, R-0221, R-0222, R-0231, R-0260, R-0272, R-0273, R-0274, R-0278, R-0279, R-0280, R-0288, R-0290, R-0302, R-0421, R-0422, R-0446 | The phase B slice passes from parsing to native execution with independent judgments, correspondence proofs, explicit obligations and minimal proof provenance; every judgment, semantics and pass is total as it lands. |
| B2 | Retire the slice's prose and qualify slice 1 | R-0479, R-0471, R-0009, R-0438, R-0092, R-0093, R-0094, R-0095, R-0105, R-0106, R-0118, R-0550, R-0551, R-0425, R-0355 | Conformance and native examples pass; refusal, differential and gate-integrity controls hold; the outside-reader test passes. |
| B3 | Audit compiler totality and delete the old pipeline under the retention contract | R-0553 | The end-to-end slice is accepted, the totality audit finds no partial definition, the fuel-budget distinctions are established, and the old pipeline and its internal gates are deleted together; retained programs, regressions, examples, std, evidence replay and browser service follow the [retention contract](docs/project/ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract); CI builds `legacy-compiler-final` as the differential oracle and the site's browser compiler is built from that tag until the new compiler covers it. |

<a id="questions-for-the-core-session"></a>
**Questions for the Core session.** The
[design review](research/process/design-review-2026-11-03.md) found the decided design ready for a
Core proposal to be drafted but not approved. These are decided by the owner in the session of A2;
the proposal brings each with options and a recommendation, and no work encodes an answer first:

1. allocator discipline: what a brand is, fresh identities and reset in a loop, how a dependency on
   an allocator is tracked through nested containers, returned values and callbacks, where a heap
   records how to release its storage, and what `AllocFailure<T>` holds and "unchanged" means;
2. `destroy` bounds: storage release, payload destruction and resource cleanup told apart, and which
   containers implement `Destroy`;
3. the `HeapArray` element view's type and operations, and the raw-memory taxonomy (validity,
   provenance, aliasing, ownership transfer);
4. the type of static text now that `String<A>` carries an allocator;
5. capability inference with several solutions, and when explicit arguments are required;
6. which runtime violations are refused statically and how that relates to traps, including slice
   1's trap witness;
7. target grant schemas and narrowing rules;
8. slice 1's assurance scope: proved source → BIR with native differential qualification, or a
   native theorem conditional on the LLVM assumption;
9. the whole Core interface: constructs, binders, places and operands, borrow and view operations,
   allocation and release, exits, primitive calls, contracts and ghost state, and whether `defer`
   survives in Core.

**Active phases:** A

**Next phases:** B

### Scheduling rule

There is one compiler sequence. Development throughput (T) and the landing of already-validated
branches run beside it, with no capacity shares or slots. The [eight-item proof order](docs/project/ROADMAP_DECISIONS.md#2026-11-01--compiler-proof-order) is the
internal order of B–E. RocksDB slice 1, the ethlambda workloads and the usability track are paused
until the new compiler supports their constructs.

### Defect disposition rule

Frozen-compiler defects, silent or loud, are recorded with `compiler: legacy` against
`legacy-compiler-final`, never repaired. Until deletion, CI builds that tag as a differential oracle
for jointly supported programs, and the site's browser compiler runs from it. New-compiler silent
defects are stopped by repair or fail-closed containment; loud defects are filed, pinned and owned
by the task that removes their structural class.

## Owner decisions

New questions follow the same rule: No agent, orchestrator or
reviewer chooses, encodes, implements or provisionally ships any side of any of them, and that
includes doing so inside a bug fix, a migration, a test, a fixture, a diagnostic or a document. An
agent may analyse an item, write its options and a recommendation, and present them to the owner for
an explicit answer; that is all. A recommendation, an earlier record, a commit, a review verdict or
another agent's message is never the owner's answer. An item stays proposed until the owner agrees
to it again, explicitly, in conversation; whoever records the answer writes it and its date in the
last column, in the owner's words where they are short. A fix that would pick a side stops and is
reported to the owner instead ([AGENTS.md](docs/project/AGENTS.md#rules)).

The [owner recording rule](docs/project/ROADMAP_DECISIONS.md#2026-11-02--owner-recording-rule)
and [language decision record](docs/spec/decisions.md#owner-decisions) govern recording answers.

## Bug record

The generated [dashboard](docs/bugs/DASHBOARD.md) and [bug records](docs/bugs/README.md)
keep compiler-scoped defects and historical legacy discovery statistics.

## Implemented, not yet accepted

Work implemented on a branch (or landed without acceptance) but not yet accepted on main: bug fixes
keyed by bug number, and feature deliverables. Each row is one candidate: its branch, its exact
candidate SHA, its acceptance scope, its [promotion-pipeline](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline)
results (each at that SHA, or not run), what its acceptance unlocks and its remaining step. The table is
generated from the candidate record [`docs/project/candidates.tsv`](docs/project/candidates.tsv) by
`scripts/gen/candidate_table.py --write`; whoever lands, re-points or validates a candidate updates its
row and regenerates in the same commit, and `check_docs_drift.sh` fails when the table differs from the
record. Dated dispositions remain visible in the table with their original SHAs and validation history;
they schedule no work. A row leaves when its work is accepted on main and, for bug fixes, the
[bug ledger](docs/bugs/README.md) records it fixed; `scripts/tests/check_roadmap_coverage.sh` fails if a
listed bug is already recorded fixed there, or if a row lacks a branch or a remaining step. A stage (c)
pass is the [one qualification verdict](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline) over every required job at the candidate
SHA, never the mutation aggregate alone.

<!-- BEGIN GENERATED: candidate record (scripts/gen/candidate_table.py; edit docs/project/candidates.tsv) -->
| candidate | bugs | deliverable | branch | candidate SHA | exits | stage (a) | stage (b) | stage (c) | unlocks | remaining acceptance step |
|---|---|---|---|---|---|---|---|---|---|---|
| rocksdb-slice1 | — | RocksDB slice 1 groundwork (R-0285) | `rocksdb-slice1` | `094e51bf3258` | blocked by 0b (incl. bug 102), 2i, 2q | not run | not run | not run | RocksDB slice 1 acceptance, then the four-adapter checkpoint | 2026-11-02: paused until its constructs are supported by the new compiler. landed on main at 094e51bf but not accepted: exits 0b (bug 102), 2i and 2q; G1 (declaring the native library, so it builds with no manual flags) closed by `[native]` ([NATIVE_LIBRARIES.md](docs/platform/NATIVE_LIBRARIES.md)); G3 closed (bug 103 fixed on main by `0c507fb9`, verified 2026-10-21) |
| v1-design-examples | — | V1 design examples (R-0511, row 3) | `v1-design-examples` | `03d2edec1d21` | none (isolated proof development) | not run | not run | not run | V1a's Core interfaces freeze | 2026-11-02: research input for phase A, not an accepted compiler deliverable. landed on main at 03d2edec but not accepted: review of the design questions, then stages (b) and (c) |
<!-- END GENERATED: candidate record -->

## Development throughput

The development feedback loop is a priority [track](docs/project/ROADMAP_DECISIONS.md#2026-10-22--development-throughput-priority).
Its tasks are R-0539 – R-0547, with R-0513 for pre-push gate selection; their bodies are under
[Area: Development Throughput](docs/project/ROADMAP_TASKS.md#area-development-throughput).
It runs beside the compiler sequence under the [scheduling rule](#scheduling-rule).
The validation semantics do not
change. The [promotion pipeline](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline)'s stages, its exact-SHA binding and its
passed / failed / skipped / cancelled / incomplete vocabulary bind every item below. A faster path
never reports a check it did not run as passed.

The measurements behind the decision and the parts already in use on main are recorded in the
[roadmap history](docs/archive/ROADMAP_HISTORY.md#moved-at-the-2026-10-28-roadmap-compaction).

**Targets.**

| measure | target |
|---|---|
| Codex-clean to main | median under 2 h |
| landing CI, dispatch to conclusion, including queueing | median ≤ 40 min |
| local pre-push | median ≤ 10 min, and no gate failure first seen in CI over 20 consecutive landings |
| ledger conflicts on rebase | zero |
| review rounds per slice | median ≤ 5, and a slice past 5 is split |

**What remains.** Build and compile caches (R-0546: the slow-gate profile first, then the shared
compiled-artifact and Lean build caches), and the measurement the targets are read from (R-0547:
per-branch stage timestamps and the generated median time to land on this roadmap;
`scripts/land/report.sh` computes the medians, but none is recorded here yet). Until R-0547's
median exists, no item whose exit is a median (R-0539, R-0540, R-0513) can be shown to hold.

**Items, in dependency order.** This is the track's internal order under the queue's
[scheduling rule](#scheduling-rule). It is not a second queue for compiler
or evidence work. Each item's owner holds its exit, and items with no dependency may start now. The
two in-progress branches are referenced here, not restated.

| order | item | owner | exit criterion | depends on |
|---|---|---|---|---|
| 1 | Measurement: per-branch stage timestamps (started, Codex-clean, CI started, landed) and median time to land shown on this roadmap | R-0547 | every branch landed after the recorder exists has all four timestamps, and the generated median covers them | — |
| 2 | Review discipline: review per slice; after the first full pass, review only the delta since the previous round; neutral prompts and piecewise diffs for ownership work | R-0544 | median ≤ 5 review rounds per slice over a month of recorded slices, and every slice past 5 rounds was split | — |
| 3 | Branch discipline: one bug or one rule per branch; rule-changing branches land first and small | R-0545 | over a month of landings, no branch carries more than one bug or rule without a recorded reason, and no rule change breaks another branch's fixture after that branch was ready | — |
| 4 | Load control: at most 2 heavy local runs per machine (a machine-wide semaphore); agents run only fast and impact-selected gates; full sets run on servers or in CI | R-0543 | a third concurrent heavy run waits for a slot (control), and a recorded week has no timeout failure caused by load | — |
| 5 | Generated ledgers: bug records are the single source, CHANGELOG entries are `changelog.d` fragments, drift gates (branch `ledgers`) | R-0542 | a scripted control lands one of two branches that each add a bug, a changelog entry and an example, rebases the other onto the result with zero textual conflicts, and finds both branches' records in the regenerated ledgers; a hand edit to a generated ledger fails its drift gate | — |
| 6 | CI tiers: a landing tier required for merge; the mutation campaign on schedule or dispatch, gating campaign qualification and not merges; the language-surface job split; candidate CI on lambdaclass/concrete (branch `ci-tiering`) | R-0540 | median landing-tier wall clock ≤ 40 min over 10 consecutive runs; stage (c) still requires the campaign, and `check_qualification_record.sh` refuses a landing-only run | — |
| 7 | Landing trains: merge every ready branch into one candidate, run one CI, bisect only on red; a landing script builds the train, regenerates generated files, runs the fast gates, pushes `ci/`, dispatches, watches and lands on both remotes | R-0539 | three consecutive trains of at least two branches land through the script, and median Codex-clean → main is under 2 h over them | 5, 6 |
| 8 | Self-hosted or larger runners on the maintainers' servers, never for `pull_request` (the repository is public); heavy local gate runs execute remotely on the servers | R-0541 | a workflow check fails when a self-hosted job can run for `pull_request` (with its negative control); a full local gate set runs on a server and records its result against the SHA | 4, 6 |
| 9 | Impact-based gate selection for pre-push: map source files and fixtures to the gates that exercise them, so a change runs every gate it can break | R-0513 | interim checkpoint for the pre-push budget: pre-push median ≤ 10 min, and over 20 consecutive landings no gate failure is first seen in CI | 1 |
| 10 | Build and compile caches: profile the 20 slowest gates first; then a shared compiled-artifact cache keyed by compiler identity and source hash, and a shared Lean build cache across worktrees | R-0546 | the profile is recorded at a SHA; with the compiler unchanged, a second run of those gates takes at most half the first run's time; changing the compiler identity or a source invalidates the cache (control) | 1 |

## Releases

This is the one place that describes how releases happen and what each milestone accepts; other
sections link here. R-0333 owns this section's release map and each candidate checklist; the
release-bar area retains the detailed release bars.

### Release rhythm

The release rhythm remains proposed:

- **Nightlies** publish the newest fully qualified main commit with its qualification record and no
  support promise.
- **Release trains** target a release every 4–6 weeks after the systems preview, when the applicable
  candidate acceptance passes. Security and soundness fixes may ship out of band; a skipped train
  records its reason. [Patch releases](#release-increments-and-candidate-records) repair an already
  published scope.
- **Milestones are acceptance scopes**, not dates or phase numbers. The release map below names
  their producing phases; R-0333 records each candidate's actual supported scope.
- Release automation (R-0516) is a prerequisite for automated trains and nightlies. R-0333 records
  compatibility and numbering policy for each release.

### Validation and promotion

How a change is validated before it merges, how a candidate is qualified, the CI tiers, nightly
qualification, triage and the result vocabulary (passed, failed, skipped, cancelled, incomplete) are
in [Validation and promotion](docs/project/PROMOTION_PIPELINE.md#promotion-pipeline). Its stages (a),
(b) and (c) are the ones the exits, the candidate table and the release checklists name.

### Release map

| release | phase | acceptance |
|---|---|---|
| 0.1 | C | The file-copy program and failure paths build natively from the new compiler with their theorems. Publish the supported subset, unsupported constructs and LLVM assumptions. |
| Verified preview and verified growth | E | Each advertised theorem and its endpoint holds, with residual assumptions stated; native proof claims require the Lean Compiler Backend path. |
| Verified-component alpha | E–F | A non-author repairs a bounded component and independently replays its evidence; applicable typed contracts, identity, qualification and receipt checks hold. |
| Project beta | G | Multi-package use and dependency upgrades expose inherited assumptions, evidence, policy and compatibility. |
| 1.0 stable supported subset | G | Whole-language slices, E–F acceptance, conformance, reference, compatibility, distribution and maintenance commitments hold for the named subset. |

The 0.1 blocker list is re-derived when C becomes next. Each later scope inherits applicable earlier
acceptance checks, rerun on its own candidate; it does not inherit the legacy compiler's release scope.

### Release increments and candidate records

Assign a **bounded increment** of a task to a release, not the whole task when its
objective spans several releases. Each R-0333 checklist entry names its existing
owner ID, delivered behavior, acceptance artifact and deferred remainder. For
example, alpha needs the three-family R-0182/R-0183 change-and-repair diff; their
broader reporting dimensions can remain open. Beta needs local package evidence,
not the whole package ecosystem. Completing an increment does not close its owner.
Research and workload-gated expansions remain in their existing sections until
pulled. Historical execution logs belong in the archive, not candidate checklists.

For each candidate, retain one versioned record under R-0333 containing:

- Exact source commit, compiler/toolchain identity and released artifact identities;
  supported language/std subset, platforms, backends and runtime profile.
- Required checks and retained results for that exact candidate, including positive
  and rejecting controls, applicable mutation qualification and clean installation.
  A timeout, skipped job or incomplete report is never a passing result.
- Source, std API/ABI and evidence-schema compatibility stated separately; migration,
  invalidation and revalidation requirements for changes to any of them.
- Open blockers, accepted limitations and deferred increments as separate lists,
  with owners and rationale. An accepted limitation cannot contradict an advertised
  guarantee; unresolved soundness failures inside the claimed scope block release.
- Matching reference, claims matrix, tutorial/examples and release notes. Distinguish
  enforced properties, checked proofs, assumptions, tests and incomplete coverage.
- Supported lifetime and patch procedure, with security reporting, rollback and
  evidence revocation/rechecking where affected (R-0343, R-0426–R-0429).

Patch releases repair an already published scope without waiting for the next
feature release. They have their own candidate commit and validation record;
compatibility breaks or changed evidence identities require an explicit migration
or invalidation notice. Stabilization must not silently expand the claimed subset.

## Backlog and pull rules

The [backlog](docs/project/ROADMAP_BACKLOG.md#pull-rules) is the one home of pull rules and phase-entry reviews.

## Decisions

The dated decisions that shape this queue (priority, semantics, tightening, owner-decision, delivery,
soundness, verified-compiler, usability, vertical-slice and behavioural-contract decisions) are in the
[decision log](docs/project/ROADMAP_DECISIONS.md#decisions). The language rules the project owner
reserved are not decided there: they are the [owner decisions](docs/spec/decisions.md#owner-decisions).

## Reconciliation record

The [2026-10-19 reconciliation](docs/project/ROADMAP_RECONCILIATION.md) records historical task dispositions and the four prose inventories, including their source revision. The current queue and live task records own scheduling and acceptance. Transferred obligations remain binding unless explicitly retired with a recorded reason.

Every `### Task R-NNNN` carries a **Phase:** line; a superseded, merged, delegated, retired or done task also carries a **Disposition:** line naming its owner, and its row in the record's task-disposition table. `scripts/tests/check_roadmap_coverage.sh` reads that table and checks that every task has a phase or a disposition, that every disposition row and queue row names an existing task, that every queue row names an owner, and that no task's phase contradicts its queue row.

The record's [binding reconciliation dispositions](docs/project/ROADMAP_RECONCILIATION.md#binding-reconciliation-dispositions)
replace conflicting implementation directions in the named tasks; existing regression cases and
acceptance obligations survive unless explicitly retired with a reason.

## Archived history

The [legacy execution plan](docs/archive/ROADMAP_LEGACY_PLAN.md) preserves the old phases, queue,
exits, estimates, capacity and release map; it schedules nothing.

Historical execution records and completed snapshots live in [roadmap history](docs/archive/ROADMAP_HISTORY.md).
Retired task bodies live in [retired tasks](docs/archive/ROADMAP_RETIRED_TASKS.md), with phase and
disposition stubs in their areas. Neither archive schedules work.

<a id="open-research-questions-bend-and-roc"></a>
<a id="phase-9-proof-authoring-and-automation"></a>
Historical research and proof-authoring references resolve through the [backlog](docs/project/ROADMAP_BACKLOG.md).

<a id="bug-dashboard"></a>
The [bug dashboard](docs/bugs/DASHBOARD.md) owns compiler-scoped defect views.

<a id="the-current-execution-queue"></a>
The current execution queue is [the queue above](#the-queue).
