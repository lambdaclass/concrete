# Concrete Roadmap Tasks

This file holds the full text of every roadmap task that a queue row names or whose **Phase:** line
includes an active or next phase, or T. [ROADMAP.md](../../ROADMAP.md) itself keeps only the control plane:
the phases, [the queue](../../ROADMAP.md#the-queue), scheduling and defect rules,
owner-decision pointers, candidate records and release acceptance. A
task's position here is topical filing under its **area**; it schedules nothing. The queue and the
active and next phases named in ROADMAP.md decide what happens next.

Every other task lives in the [roadmap backlog](ROADMAP_BACKLOG.md) with its pull trigger. The bodies
of done, superseded, merged, retired and delegated tasks are archived in
[ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md); each keeps a stub here or in the
backlog. Completed tasks are in [CHANGELOG.md](../../CHANGELOG.md) under `### Completed Task R-NNNN`.

The tasks moved here verbatim from ROADMAP.md on 2026-10-28 (only heading-relative links were
adjusted). The 2026-11-02 restructure applies the immutable classification record to phases,
surviving requirements, retirements and placement; task IDs stay unchanged. `scripts/tests/check_roadmap_coverage.sh`
and `scripts/tests/check_roadmap_linear.sh` read ROADMAP.md together with this file.

## Where every area fits

Areas are topical filing locations, never scheduling units. Until 2026-10-19 they had the
historical phase numbers below. Each task's **Phase:** line and the queue own its current
schedule; the [legacy catalog](../archive/ROADMAP_LEGACY_PLAN.md#area-scheduling-before-the-restructure)
preserves the former counts and scheduling map.

| area | historical area phase |
|---|---|
| Standard library and core APIs | 7 |
| QBE backend and independent validation | 7.5 |
| Flagship depth and examples | 8 |
| Incremental compiler driver | 8.5 |
| Proof authoring and automation | 9 |
| Audit commands and review artifacts | 10 |
| Proof status and trust gates | 11 |
| Provable and predictable subsets | 12 |
| Runtime safety obligations | 13 |
| Compiler soundness bridge | 14 |
| Verified-compiler pipeline, stabilization and usability | — |
| Backend, target and stdlib contracts | 15 |
| WebAssembly | 15.25 |
| Unsafe Concrete and trusted systems profile | 15.5 |
| Performance and optimization evidence | 15.75 |
| Freestanding and embedded target | 16 |
| Public release bar | 17 |
| Post-release operations | 17.5 |
| Packages and dependency evidence | 18 |
| Editor and human tooling | 19 |
| Concurrency and research-gated extensions | 20 |

## Area: Development Throughput

These tasks carry the [development throughput](../../ROADMAP.md#development-throughput) track, which was decided on
2026-10-22. The measurements behind them, the targets and the dependency order are in that section.
Pre-push gate selection belongs to R-0513, so it is not restated here. None of these tasks
changes what a validation result means. Every result stays bound to its exact SHA under the
[promotion pipeline](PROMOTION_PIPELINE.md#promotion-pipeline).

### Task R-0539

**Phase:** T

**Objective:** Landing trains. Merge every ready branch into one candidate, run one CI, and bisect only
on red.

**Work:** a landing script (new) that takes the ready branches and does the following:

- builds the train;
- regenerates the generated files (R-0542's ledgers, `Concrete/BuildIdentity.lean`, the candidate
  table) once on the train rather than on each branch;
- runs the fast gates and pushes the train to `ci/`;
- dispatches CI and watches it to its conclusion;
- on green, lands the train on both remotes.

On red, it bisects the train by member and rebuilds it without the offending branch. Every result is
recorded against the SHA of the candidate that actually ran: the train, or the bisection candidate.
The attribution to a member is a separate field of that record. A member's own SHA receives a result
only from a run at that SHA. A rerun never stands in for the failed result. The train's record lists
each member's SHA, so a landed member can be traced to the train candidate whose result it relies
on.

**Exit:** three consecutive trains of at least two branches land through the script with no manual
step except approval. Median Codex-clean → main is under 2 h over those trains (R-0547's record). A
seeded red member is identified by bisection, and the remaining members land without it.

**Depends on:** R-0542 (a train whose ledgers conflict cannot be built mechanically) and R-0540 (the
landing tier).

### Task R-0540

**Phase:** T

**Objective:** CI tiers. A landing tier is required for merge, with a target of ≤ 40 min. The mutation
campaign runs on schedule or dispatch and gates campaign qualification ([stage (c)](PROMOTION_PIPELINE.md#promotion-pipeline)),
not merges.

**Work:** in progress on branch `ci-tiering`, which edits the workflow and the promotion-pipeline tier
text. It moves the mutation campaign out of the landing tier, splits the language-surface job and
routes jobs to runners. Candidate CI runs on lambdaclass/concrete, whose GitHub Enterprise plan allows
about 500 concurrent jobs against the free plan's roughly 20.

**Exit:** median landing-tier wall clock, from dispatch to conclusion including queueing, is ≤ 40 min
over 10 consecutive runs. No landing-tier job takes more than 40 min. Stage (c) still requires every
job, the campaign included, and `check_qualification_record.sh` refuses a landing-only run.

### Task R-0541

**Phase:** T

**Objective:** Self-hosted or larger runners on the maintainers' servers, and remote execution of
heavy local gate runs on the same servers.

**Work:** register the runners with labels and route only `push`, `schedule` and `workflow_dispatch`
jobs to them. They are never used for `pull_request`, because the repository is public and a fork's
pull request would run untrusted code on them. The runbook is `docs/project/SELF_HOSTED_RUNNERS.md` on
branch `ci-tiering`. Heavy local gate runs (`make pre-push-full`, closure-mode runs and local mutation
campaigns) run remotely on a server against an exact SHA, so they keep the laptop's load low.

**Exit:** a workflow check fails when a job with a self-hosted label can run for `pull_request`, and
its negative control proves the check bites. A full local gate set runs on a server and records its
result against the candidate SHA in the same form as a local closure-mode summary.

**Depends on:** R-0540 (routing), R-0543 (where heavy runs go).

### Task R-0542

**Phase:** T

**Objective:** Generated ledgers. Every shared fact has one owner that is edited by hand, and every
other copy is generated and drift-gated. This is the "one owner per fact" rule of the approved
repository reorganization ([R-0480](#task-r-0480)).

**Work:** in progress on branch `ledgers`, which edits the bug README, the audit corpus and the
CHANGELOG mechanics:

- bug records are the single source for the bug README's lists, STRUCTURAL_CLASSES' instances and the
  audit corpus;
- CHANGELOG entries are `changelog.d` fragments;
- the example manifest's and gate inventory's counts are derived;
- each generated copy has a drift gate.

**Exit:** a scripted control starts two branches from one base, each adding a bug record, a changelog
entry and an example. It lands the first, rebases the second onto the result with zero textual
conflicts, and finds both branches' records in the regenerated ledgers. A hand edit to a generated
ledger fails its drift gate.

### Task R-0543

**Phase:** T

**Objective:** Load control. At most 2 heavy local runs per machine.

**Work:**

- a machine-wide semaphore in a directory shared by every worktree and clone, which every heavy runner
  acquires and releases even when it is interrupted;
- agents run only the fast gates and the impact-selected gates (R-0513);
- full sets run on the servers (R-0541) or in CI.

**Exit:** a third concurrent heavy run waits for a slot (control), and a stale slot from a killed run is
reclaimed. A recorded week of runs has no timeout failure attributed to load.

### Task R-0544

**Phase:** T

**Objective:** Review discipline. The target is ≤ 5 review rounds per slice.

**Work:**

- review per slice;
- after the first full pass, each round reviews only the delta since the previous round;
- a slice still not clean after 5 rounds is split;
- ownership and memory-safety work is reviewed with neutral prompts and piecewise diffs, so a filter
  does not block the round.

**Exit:** R-0547's record shows a median of ≤ 5 rounds per slice over a month, and every slice past 5
rounds has a recorded split.

### Task R-0545

**Phase:** T

**Objective:** Branch discipline. One bug or one rule per branch, and rule-changing branches land first
and small.

**Work:**

- a branch that changes a language or checker rule lands before branches that add fixtures under the
  old rule;
- the rule change carries the migrations of main's fixtures, and nothing else;
- a branch that needs two rules is two branches.

**Exit:** over a month of landings, no branch carries more than one bug or rule without a recorded
reason. No rule change breaks a fixture of a branch that was already ready.

### Task R-0546

**Phase:** T

**Objective:** Build and compile caches.

**Work:**

- first, profile the 20 slowest gates;
- then a shared compiled-artifact cache keyed by compiler identity and source hash;
- a shared Lean build cache across worktrees.

**Exit:**

- the profile is recorded at a named SHA;
- with the compiler unchanged, a second run of those gates takes at most half the first run's time;
- a new worktree's first build reuses the shared Lean cache;
- changing the compiler identity or a source file invalidates the affected entries (negative control).

**Depends on:** R-0547 (the profile is recorded with the measurement).

### Task R-0547

**Phase:** T

**Objective:** Measurement. Record per-branch stage timestamps (started, Codex-clean, CI started,
landed) and show median time to land on this roadmap.

**Work:**

- one record per branch, so recording never conflicts on rebase;
- review rounds per slice and the place each gate failure was first seen (pre-push or CI) are recorded
  with the timestamps;
- a generated median time to land next to the candidate table under
  [Implemented, not yet accepted](../../ROADMAP.md#implemented-not-yet-accepted), regenerated like that table.

**Exit:** every branch landed after the recorder exists has all four timestamps. The generated median
covers them, and `check_docs_drift.sh` fails when the median is stale.

## Area: Verified-Compiler Pipeline, Stabilization And Usability

These tasks carry the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md)
(R-0491 – R-0512), the gate and shared-front-end work it depends on (R-0513, R-0514), the usability
and release-automation tracks (R-0515, R-0516), the Lean Compiler Backend dependency (R-0517) and the
open work promoted from roadmap prose by the 2026-10-19 reconciliation. They were filed under the backend-contracts area
until 2026-10-19; their queue rows, not this position, schedule them.

### Task R-0491

**Phase:** B

**Objective:** Retain the defect inventory, independently justified expected outcomes, classified spec/native differential comparisons, evidence and consumer controls, and non-vacuous campaign coverage as new slices admit their constructs.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

The three-way oracle is specified Core semantics / LLVM profile / LCB-native, with
independently justified expected outcomes; disagreement labels are hypotheses, not proof of the
faulty component. Cover loop-body sequencing, `let` after assignment, returns in loops,
if-without-else fall-through, discarded expressions, shadowing, renamed match binders, boundary
literals at 2^63/2^64 and every cast-width pair. Classify value, trap, rejection, unsupported,
exhaustion and crash without skipping outcomes. Compare zero-argument, boolean and aggregate
signatures and native binaries, with a pinned claimed-function inventory.

Keep the permanent defect inventory through 101. Every bug from 102 on names structural cause,
class and owning task/pipeline stage in `docs/bugs/STRUCTURAL_CLASSES.md`; filing gates reject
missing causes/owners. A newly reproduced class without a scheduled owner enters the queue under
the entry rule. Before each release candidate run the four 2026-10-18 hunts and checker hunt.

Acceptance: bugs 080–095 have records, reproducers, expected outcomes and pre-fix/fixed SHA pairs.
The deterministic differential detects 080 and 084–088; name-binding controls detect 081–082;
evidence/cross-surface controls detect 083/089; checker mutants detect 090–094; the binary leg detects
095. Each assigned control fails on its historical pre-fix SHA and passes at its fix. Missing
inventory entries fail; generated features meet pinned minimum completed-comparison counts.
Retain hand/reference/metamorphic, debug/release, sanitizer and backend/profile matrix controls.
Historical comparison does not authorize a legacy repair.

Every `@run` in `scripts/tests/example_manifest.txt` names an oracle: `interp` only with
`interp=yes`, a recorded `spec:<path>[#anchor]` derivation/reference result in
[GOLDEN_ORACLES.md](GOLDEN_ORACLES.md), or `unverified:<reason>` only if the interpreter cannot
run it. `check_example_outputs.sh` enforces this with negative controls and prints/lists unverified
goldens as tracked gaps. Retire each gap with supported interpreter coverage or a recorded derivation;
a compiler-produced golden alone is insufficient. This rule covers every new/adversarial expected result.

Retain the following hand-targeted suite under `examples/adversarial/`, with a separate leg in
`scripts/tests/check_adversarial_examples.sh` as each construct becomes supported:

| Example | Required coverage |
|---|---|
| `linear_flow` | Moves/destruction across branches/matches, early return, break/continue with live handles, consume-then-exit; reject arm leaks and guard moves (078/087/090/093) |
| `generic_destroy` | Trait-bounded Destroy, arrays/generic structs and nested collections; reject hidden generic capabilities (052/091/099/100) |
| `nested_names` | Nested/sibling type/function/method names, generated specialization names, type arity, relative submodule calls, builtin versus user Result (054/070/073/077/096–098) |
| `import_aliases` | Function/type/method/sibling aliases, local collisions, order permutations and collision rejection (039/055/069/082/E0264) |
| `casts_widths` | Every narrowing/widening/sign-change width pair, min/max literals, checked negation and claims (053/080/088) |
| `mut_receivers` | Mutable parameters/fields/array elements and branches; raw-borrowed stack storage, extracted arrays and RHS short-circuit borrowing with observed mutation (018/019/031/034) |
| `match_and_fnptrs` | Effectful guards, renamed/positional binders, branch-reassigned and field-stored function pointers with capabilities (056/063/066/084/090) |
| `statement_sequencing` | Discarded expressions, assignment followed by let, loop steps, branch-local let and if fall-through, with claims (085/086/101) |

Bug 063 runs derived-pointer cases through function and method call paths and retains generic-return
E0242 controls. Historical unresolved 054/055/077/E0264 cases keep separate fixtures pinning the
recorded collision/rejection/containment outcome until their new-compiler construct is admitted;
runnable cases require semantic-interpreter/native/expected agreement. Reconcile provisional
082/087/088/090–100 shape mappings against checked-in records. Every leg preserves exit status and
trap identity; rejection legs assert diagnostic codes; claim legs compare functional models and
assert claim status. Missing examples/legs and perturbed expected results fail. Preserve existing
controls for evidence/report cases 058–062/064/065/067/068/075/076/079/081/083/089, capability 071
(`check_cross_package_caps.sh`, `check_cap_sibling_module.sh`), runtime 072 (`check_spawn_exit.sh`),
package collision 074, collection 057, reference case 092, and checker/binary 094–095. Newly
reproduced authority/safety defects enter the queue as immediate exits and keep their reproducer leg.

Nightly seed rotation keeps 2,000 depth-3 and 500 depth-4 differential programs plus 400 linearity
programs per date-derived seed. Once the third side is available, run campaigns at least 100 times
those counts, sharded by seed range, first on short-lived remote servers (R-0513); no provider
credentials enter the repository. Keep nightly counts meanwhile. Each campaign records SHA,
toolchain, seeds, classified outcomes and per-feature completed comparisons against pinned minima;
automatically minimize failures into reproducers and queue them. Accept this separate campaign
when one run meets scale and all minima, then repeat before each release candidate; smaller suite
acceptance does not wait for it.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0092** (superseded): superseded — the three-way differential is Core semantics / LLVM profile / LCB-native under R-0491. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0092)
- **R-0093** (merged): duplicate of R-0092: merge; labels are hypotheses, not proof of faulty component. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0093)
- **R-0094** (superseded): superseded by R-0491: merge; retain hand/reference/metamorphic controls.
- **R-0095** (superseded): superseded by R-0491/R-0118: merge manifests and minimum completed comparisons.
- **R-0096** (superseded): superseded by R-0513/R-0491: merge; remove mandatory QBE availability.
- **R-0105** (superseded): superseded by R-0491/R-0504: merge debug/release and sanitizer controls. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0105)
- **R-0106** (merged): duplicate of R-0009/R-0491: merge new backend predicates into shared reducer/harness.
- **R-0273** (superseded): superseded by R-0491/R-0498: merge; map every listed family to a retained control. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0273)
- **R-0290** (merged): Duplicate of R-0491: merge into R-0491, retaining backend/profile matrix.

### Task R-0492

**Phase:** B

**Objective:** Use scope-resolved identities throughout the new compiler, distinguish lexical from persistent evidence identity, and retain alias, symbol, binding, substitution and artifact-forgery controls.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Allocate `LocalId` in the lexical resolution walk for every binder; checker and elaboration
consume that same identity. Allocate compilation-scoped `DefId` before body elaboration,
preserving callable namespaces, specialization arguments and extern/intrinsic distinctions;
validate the `DefId → DefinitionIdentity` mapping at evidence boundaries. Use owner-checked
`FieldIdx`/`VariantIdx` and register/block identities in lowered IRs. No `String → identity`
coercions: checked adapters obtain identities from scope-aware environments. Perform
capture-preserving alpha-canonicalization before hashing; preserve frozen identity bytes or
explicitly version them. Distinguish lexical handles from persistent evidence identities.

Refer to builtin `Option`/`Result` by resolved identity; a user `Result` remains distinct.
Concrete-defined symbols derive their disambiguator from `DefId`. Preserve the recorded scoping,
import-alias collision and match-binder rules without answering an open owner question.

Acceptance: `check_ir_no_string_identity.sh` covers resolved tables, checker environments,
Core, lowered IRs and proof expressions. Bugs 081, 082, 087, 093 and 094 reject or bind correctly;
alias permutations, imported aliased-method calls, function-symbol and metamorphic fixtures pass.
Targeted mutations prove checker consumption, block-local accounting, elaboration correspondence
and theorem-table identity checks load-bearing. Compile-fail controls reject artifact forgery
and diagnostic-path bypass.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0007** (superseded): superseded by R-0492/R-0510: merge, preserving function-symbol and metamorphic controls. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0007)
- **R-0008** (superseded): superseded by R-0492: merge reproducer and alias permutations into identity migration.
- **Aliased imported method (E0264)**, from the archived 2026-08 gate-health record: a method call on an
  aliased imported type is rejected today; the identity migration resolves it and adds an exact
  alias-method fixture (reconciliation prose item, merged through R-0008).
- **R-0103** (superseded): superseded by R-0492/R-0505: merge; general separate compilation arrives in V6.
- **R-0114** (superseded): superseded by R-0492: merge; distinguish compilation-local IDs from persistent evidence identity. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0114)

### Task R-0493

**Phase:** B

**Objective:** Define typed arithmetic and casts from one specified semantics, preserving widths, signedness, operation modes, literal ranges and defined-trap regression controls.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Integer operations carry `IntTy`, width and signedness; checked, wrapping and saturating
operations are distinct kinds. Pointer arithmetic is its own operation; boolean, float and
pointer operations remain distinct. Canonicalize signed literals before range checking, resolve
contextual literal types before constructing operations, and revalidate after specialization.
Casts are typed. One specified arithmetic semantics supplies evaluation, folding, model generation
and emitted-operation checks; `check_single_arithmetic.sh` enforces that single authority.

Acceptance: every integer operation in Core and lowered IR carries kind and width; the arithmetic
lint passes. Differential controls cover every supported width, operation family, cast and defined
trap. Signed-minimum fixtures pass and bug 088's reproducers agree on every side.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0079** (superseded): superseded by R-0493/R-0501/03: merge into typed operations and pass obligations. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0079)
- **R-0221** (superseded): superseded by R-0493/S6: merge semantic ownership into R-0493. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0221)
- **R-0464** (superseded): superseded by R-0493/R-0495/R-0460: merge; archive completed H24 fix and retire SSA-first prescription. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0464)

### Task R-0496

**Phase:** A

**Objective:** Specify the new Core semantic contract and values-to-bytes boundary, then implement its clock-indexed evaluator and instrumented semantics with representative memory, clock and trace proofs.

**Work:** Dispose of the audit and draft the whole Core interface for owner approval before implementation. State the approved semantic contract, theorem holes and trusted base in Lean; implementation and proofs grow with the supported slices.

**Deliverables and acceptance:**

Specify value semantics with allocation identities, capability-tagged external events,
defined traps, behaviours from clocks, and ghost instrumentation for moved places, loans,
frames and capability use. FFI and OS interactions have external-call contracts. The new
interpreter is the specified semantics plus an IO driver and serves as the differential oracle.
Language rules must be recorded before their semantic interfaces freeze.

Before Core freezes, specify the raw-memory interface jointly with S11 / R-0502: initialization,
alignment, pointer fragments, allocation lifetime, address exposure and foreign mutation.
Prove a representative typed-value/raw-memory abstraction lemma, clock stability and partial-trace
monotonicity with the evaluator.

Acceptance: semantics covers every construct the checker accepts; the differential fuzzer compares
it with the binary; each language rule and semantic clause link bidirectionally; the representative
lemmas are proved. Record implementation coverage separately from proved-subset coverage.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0223** (superseded): superseded by R-0496/S7: merge into R-0496’s semantic contract.
- **R-0234** (superseded): superseded by R-0496/S7, V7: merge into V7 float-profile work. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0234)
- **R-0235** (superseded): superseded by R-0496/R-0495: retire PVal/PBinOp expansion; implement Core float semantics. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0235)
- **R-0261** (merged): duplicate of R-0496/R-0460: merge; already explicitly reconciled.
- **R-0268** (superseded): superseded by S5–S8: retire undecided architecture question; plan has chosen the chain.
- **R-0276** (superseded): superseded by R-0506/R-0496: retire general legacy rewrite; retain migration-needed wrappers only. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0276)
- **R-0408** (superseded): Superseded in core scope by R-0496/S7: merge required instrumentation into R-0496; separately specify any extra research.
- **R-0409** (superseded): Superseded by R-0496/R-0512: retire legacy redesign; carry demonstrated proof debt into Core proof engineering.
- **R-0456** (superseded): superseded by R-0496 for proposed PExpr port: retire PExpr-port design; reconsider only a measured Core-metatheory proposal. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0456)

### Task R-0497

**Phase:** B

**Objective:** Make new pipeline artifacts immutable and bound to exact certified payloads, with separate diagnostic types and compile-fail controls for forgery, mutation and bypass.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Each stage artifact is immutable, bound by its certificate to the exact payload, and constructible
only by its producer/verifier. Mutation yields an unverified artifact. Diagnostic/subset artifacts
have separate uncertified types rejected by downstream stages and evidence APIs. Bind elaborated
output to its input and evidence records to their payloads. Extend typed stages along HIR → Core →
`CheckedCore` → `CheckedMonoCore` → Place MIR → Memory CFG → BIR → LLVM printer or Machine IR.
Private constructors provide API discipline; stated soundness theorems provide assurance.

Acceptance: pipeline APIs expose only typed stages; compile-fail controls reject artifact forgery,
certificate reuse after mutation and diagnostic-path bypass, including report-driven payload
substitution before specialization.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0073** (superseded): superseded by R-0497/R-0503: merge into payload-bound validated BIR artifacts.
- **R-0082** (superseded): superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0082)
- **R-0116** (superseded): superseded by R-0497/R-0510/R-0514: merge; use payload binding and new IR chain. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0116)

### Task R-0498

**Phase:** B

**Objective:** Map every new enforcement claim to its production gate leg and targeted mutation, and label unproved shape checks honestly.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Maintain a claim → production gate-leg → targeted-mutation manifest for verification and
language documentation, enforcement-bearing comments in the new proof, checker and backend code,
and emitted contract text. Generate the backend contract from BIR operation semantics when S11
lands. R-18–R-20 remain labelled shape-only until their proofs exist.

Acceptance: every in-scope enforcement phrase has a manifest row and its production leg kills the
specified mutation in a qualified campaign; inherited wrong-code families each name their control.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0273** (superseded): superseded by R-0491/R-0498: merge; map every listed family to a retained control. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0273)

### Task R-0499

**Phase:** B

**Objective:** Define independent Core judgments and a corresponding authoritative checker, preserving ownership, authority, bounds and reference regressions with positive and rule-breaking mutation controls.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Define independent declarative Core judgments for use-once, conservation at reachable exits,
borrow validity (no move, destruction or incompatible mutation of a live reference's source;
shared/exclusive aliasing), no escaping/stored references, capability containment through pointers,
generics and FFI, raw-dereference authority and bounds. Apply the recorded rules: only `Unsafe` is
absorbed at a `trusted` call site; the remaining required set must fit the caller's declared set;
raw dereference requires `trusted` or `Unsafe`; an access establishes range or executes its bounds
trap. This restates recorded requirements, without resolving any owner question.

The authoritative checker emits exact-payload `CheckedCore` plus derivation; recheck capabilities
after specialization. The source checker retains no ownership, borrow, reference, capability or
Unsafe rule. A rule-violation fuzzer makes one rule-breaking mutation to accepted programs, including
source destruction with live local/downward reference, conflicting borrows, pointer/generic hidden
capabilities and unauthorized raw dereference. Valid programs required to trap are not rejections.
Accept trusted raw-pointer operations and trusted Unsafe absorption as positive controls; reject
absorption of any other effect. Instrumented semantics supplies the runtime witness.

Acceptance: every rule has an enforcing site, R-0498 manifest row and mutation class. Qualified CI
seeds reject every mutant and accept every well-formed positive control with minimum completed
counts per rule/feature combination; the instrumented corpus/fuzzer runs have zero violations.
Reject 090–094 and 087's borrow-block case. Pin normative expected rejection of historical 092/095
aggregate-reference programs, versioned historical binary witnesses, and an accepted loop-mutation /
downward-borrow fixture detecting stale storage. Explicitly record intended acceptance changes
against the normative rules.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0227** (superseded): superseded by R-0499/R-0507: merge into checker rules and soundness.
- **R-0228** (superseded): superseded by R-0499/S8 and V6: merge enforcement into R-0499; raw-operation extensions into V6. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0228)
- **R-0267** (superseded): superseded by R-0499/R-0507/R-0495: merge; capabilities and purity remain distinct.
- **R-0274** (superseded): superseded by R-0499/R-0514: retire source-checker/CompilerDB authority prescription. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0274)

### Task R-0500

**Phase:** A

**Objective:** Dispose of the listed language rules under the approved decisions and give each its Lean judgment, semantic clause and accepted, refused or trapping conformance examples as slices land.

**Work:** Dispose of the audit and draft the whole Core interface for owner approval before implementation. State the approved semantic contract, theorem holes and trusted base in Lean; implementation and proofs grow with the supported slices.

**Deliverables and acceptance:**

For block scope/shadowing, match-binder positions/names, statement versus value position,
sequential assignment in loop iterations, contextual literal types/ranges including signed minima,
every cast-width pair, evaluation order, short-circuiting and second-class references, provide the
Lean judgment/semantic clause, generated normative paragraph, accept/reject/trap examples and cited
fixtures. Generator coverage meets pinned minima before dependent implementation advances; language
rules link bidirectionally to semantic clauses as those clauses land.

Use the recorded reference/scoping, evaluation/assignment and arithmetic policy decisions, including
renamed-pattern syntax and versioned diagnostics. Preserve SCOPE-1 and REF-6 controls, E0302/E0303
for forbidden reference bindings/assignment, constant-value treatment, the cast lossless-class table,
three named-operation families and three stages. Open questions remain for the owner.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0435** (superseded): superseded by R-0500: merge migration, renamed-pattern syntax and controls. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0435)

### Task R-0501

**Phase:** B

**Objective:** Build proved new lowering passes with executable semantics for sequencing, matching, cleanup and check expansion, retaining differential and early-exit cleanup controls.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Implement sequencing, match compilation, cleanup/defer edges and check expansion as separate
Core → Place MIR passes consuming `CheckedMonoCore`. Give Place MIR executable semantics and each
pass a differential against Core, with pinned minimum counts. Each pass is total. Prove cleanup
expansion for a representative function with early exits and `defer`; retain early-exit, sequencing,
matching and trap/check-expansion regression controls. These jobs have one owner each in the new
pipeline, with no duplicate lowering implementation.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0079** (superseded): superseded by R-0493/R-0501/03: merge into typed operations and pass obligations. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0079)
- **R-0082** (superseded): superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0082)
- **R-0295** (superseded): Superseded by R-0501/S10: retire workload gate; MIR is already an architectural decision.

### Task R-0502

**Phase:** A

**Objective:** Specify the shared value/raw-memory boundary and one layout authority, then deliver checked representations and memory lowering with allocation, alignment, store and memory-injection proofs.

**Work:** Dispose of the audit and draft the whole Core interface for owner approval before implementation. State the approved semantic contract, theorem holes and trusted base in Lean; implementation and proofs grow with the supported slices.

**Deliverables and acceptance:**

Jointly own the raw-memory specification with S7 / R-0496, including block–offset/provenance
memory, initialization, alignment, pointer fragments, allocation lifetime, address exposure and
foreign mutation. One `Layout` authority supplies all size, alignment, offset and enum-representation
facts. Layout remains unspecified except for documented `repr` guarantees; programs rely only on
`sizeof`, `alignof` and array stride = `sizeof`; generic `Option` and `Result` use per-instantiation
layout as recorded in [LAYOUT_CONTRACT](../archive/design-records/LAYOUT_CONTRACT.md).

Acceptance: Memory CFG has executable semantics and a verifier; `Layout` emits checked representation
certificates. Place MIR → Memory CFG comparisons cover related values and stores, traces, traps,
allocation and alignment. Prove a representative memory-injection lemma. Gate agreement between
`Layout` and emitted types in the new pipeline.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0076** (superseded): superseded by R-0502: merge into single proved Layout authority. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0076)
- **R-0085** (merged): duplicate of R-0076: merge through R-0076 into R-0502.
- **R-0088** (superseded): superseded — aggregate/memory ABI realization moves to R-0502 (layout) and the adapter boundary (R-0505/R-0517) across V2–V6.
- **R-0434** (superseded): superseded by R-0502: merge into layout certificates and Memory CFG verification. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0434)

### Task R-0503

**Phase:** B

**Objective:** Emit the new compiler’s explicitly unverified LLVM path from validated BIR with executable semantics, runtime contracts, support labels and differential qualification.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Own Memory CFG → BIR, executable BIR semantics and verification, LLVM adapter legalization,
Concrete `rt` semantics/contracts, generated backend contracts and complete replacement-path
qualification. Use Concrete runtime helpers with named libc FFI, without requiring helpers to be C.

Acceptance: LLVM is emitted only from exact-payload validated BIR under the adapter contract;
BIR semantics/differential gates run, support inventory and profile labels are emitted, and runtime
helpers use the specified `rt` boundary. Preserve inspection, verification and parity controls,
backend/profile capability coverage and trust accounting. The LLVM path remains explicitly
unverified at its trusted native edges; old-pipeline deletion waits for the complete new path's gates.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0073** (superseded): superseded by R-0497/R-0503: merge into payload-bound validated BIR artifacts.
- **R-0074** (superseded): superseded — merge into validated BIR (R-0503) and R-0517; the closed `llvm`/`qbe` backend set becomes the LLVM and LCB-native profiles. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0074)
- **R-0077** (superseded): superseded by R-0503/R-0509: merge with `rt` contracts, support inventory and trust accounting.
- **R-0079** (superseded): superseded by R-0493/R-0501/03: merge into typed operations and pass obligations. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0079)
- **R-0082** (superseded): superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0082)
- **R-0090** (superseded): superseded by R-0503: retire preferred-C-runtime prescription; implement Concrete `rt`, named libc FFI.
- **R-0109** (superseded): superseded by R-0503/R-0146: merge; remove assumption helpers must be C.
- **R-0278** (superseded): Superseded by R-0503/S11: merge into R-0503; BIR replaces this architecture.
- **R-0288** (superseded): Superseded by R-0503/S11: merge inspection, verification and parity requirements into R-0503. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0288)

### Task R-0504

**Phase:** B

**Objective:** Give new validators explicit soundness propositions and reject semantic substitutions, unsupported certificates and wrong retained LLVM artifacts without inflating native assurance.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Deliver the validator interface and validators for SSA construction and optimization certificates.
Each validator kind has a stated soundness `Prop` and one representative proved soundness instance.
Pin the proved-function inventory, recording validation status per semantic edge and call closure.
Unsupported closures stay `translation_trusted`/`blocked`; mismatches are `translation_mismatch`
and never grant native-code evidence.

Acceptance: SSA and optimization validators reject targeted semantic substitutions and unsupported
certificates. R-0299's admitted BIR validation and R-0300's independent replay pass admitted-subset,
mismatch, unsupported and substitution controls; retain debug/release and sanitizer controls.
The emitted-LLVM profile checks exact retained `.ll` bytes, including helper bodies. LLVM
optimization, emission, linking and runtime/ABI behaviour remain explicitly trusted for that
profile; emitted-IR checking cannot claim native assurance.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0105** (superseded): superseded by R-0491/R-0504: merge debug/release and sanitizer controls. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0105)

### Task R-0506

**Phase:** A

**Objective:** State each new proof boundary and validator obligation with explicit premises and status, enforce transitive axiom classification and totality, and prove representative lemmas before freezing boundaries.

**Work:** Dispose of the audit and draft the whole Core interface for owner approval before implementation. State the approved semantic contract, theorem holes and trusted base in Lean; implementation and proofs grow with the supported slices.

**Deliverables and acceptance:**

As each boundary lands, state every pass's `Refines` and validator's soundness as type-checked
Lean `Prop`s with explicit premises and a ledger status of stated / differentially tested / proved.
Include native backend, encoding and linking. Reject transitive `sorryAx` and unregistered axioms;
kernel-only evidence permits only `propext`, `Classical.choice` and `Quot.sound`; registered native
and float axioms require separate labels. Enforce this at status, issuance and consumption with
positive controls and forbidden-transitive-dependency mutations.

Acceptance: every boundary's obligation and status are recorded; dependencies pass the axiom policy.
Semantics, checkers, validators and proved passes pass totality (untrusted search may remain partial
behind a total validator). Before freezing each boundary, its relations/obligations type-check and
a representative preservation or validator-soundness lemma is proved for the motivating feature,
including native encoding and linking. R-0553 supplies the environment inventory and ratchet.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0102** (superseded): superseded — the operation gap ledger becomes typed pass obligations (R-0506) and the adapter's obligations (R-0517).
- **R-0236** (superseded): superseded by R-0506/R-0509: merge axiom classification and inventory.
- **R-0260** (superseded): superseded by R-0506/S12a: merge into pass/validator obligation ledger; preserve legacy historical view. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0260)
- **R-0276** (superseded): superseded by R-0506/R-0496: retire general legacy rewrite; retain migration-needed wrappers only. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0276)

### Task R-0507

**Phase:** B

**Reconciliation (2026-10-19):** Keep; cumulative proofs, not a deferred final phase.

**Objective:** Type and ownership soundness — stage S14 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned (Part 2; starts when its prerequisites exist).**

Progress and preservation for the Core checker over
Core's instrumented semantics; ghost erasure; trusted and FFI code enter as their contracts. Can
start as soon as S7 and S8 exist.

**Exit:** proved for the full checked subset; S8's hunt finds nothing the theorem does not exclude.

**Proof-library reuse decision (2026-10-23; research within R-0507).** Before building custom
ownership-proof automation for V2's downward borrows and V3's owned heap, run one bounded
comparison against ordinary Lean proofs over the same Concrete Core semantics. Start once the
relevant S7 semantics and S8 rules exist; this does not block V1 or the shared-compiler migrations.
The output is a reuse/adapt/reject decision for specific modules, not a new verifier project or a
commitment to adopt a framework. Stop after the representative proof comparison; expand only if
it exposes a concrete missing obligation in this task.

Candidate shortlist, based on a source/documentation survey (not integration tests):

| Candidate | Code or technique to evaluate | Concrete use and boundary |
| --- | --- | --- |
| [Iris-Lean](https://github.com/leanprover-community/iris-lean/tree/3f808f5542f14e3a621734a9da92b81a6f74651a/Iris/Iris) | `BI`, `ProofMode`, `Instances/Classical`, and `Algebra/{Excl,Frac,Auth}.lean`; evaluate generic `ProgramLogic` only if needed | First reuse candidate: ownership contexts, framing, exclusive and shared permissions. The proof interface supports a custom logic; HeapLang's allocation/load/store/free proofs are examples, not proofs of Concrete operations. |
| [Loom](https://github.com/verse-lab/loom/tree/78928abc9054b31d0bea85985496490baae95244/Loom/MonadAlgebras) and [Velvet](https://github.com/verse-lab/velvet) | Loom's `WP/Basic.lean` and `WP/Gen.lean`, effect instances; Velvet's integration of specifications and proof automation | Compare compositional weakest-precondition automation only where it fits Core's effects. A shallow embedding needs a proved connection to Core; adopting its language or solver verdicts is not this task's exit. |
| [Aeneas](https://github.com/AeneasVerif/aeneas/tree/74aabd52f77001cf9eaad51b4f4b867a7dc6b111) | Ownership-guided functional reasoning; Lean backend's scalar specifications, including `backends/lean/Aeneas/Std/Scalar/CheckedOps/Add.lean` | Check whether Concrete's restricted borrows permit simpler value-based proofs before paying for a general heap logic. Learn from operation-specific specification lemmas; importing Rust translations or scalar types does not establish Concrete semantics. |
| [RustBelt](https://plv.mpi-sws.org/rustbelt/) | Semantic ownership/borrowing interpretations and obligations for unsafe abstractions | Design reference for the proof boundary, not a Lean dependency or a reason to add Rust lifetimes or concurrency. |

**Useful-result test.** Prove one scoped mutable-borrow update and restoration of the owner's
permissions, including preservation of an unrelated place, both with ordinary Lean and the most
promising reusable component. At V3, extend the example to owned allocation and explicit destruction.
Account for every resource on normal completion and the normative trap/divergence behavior;
exclusive ownership alone does not prove exactly-once consumption. Standard affine Iris assertions
may be discarded, so resource conservation must be an explicit proved obligation (or use a suitable
non-affine logic). Keep a double-owner counterexample and an omitted-cleanup counterexample that
the claimed invariant actually excludes. Match Concrete's checked arithmetic and failure behavior.

Record the exact imports and upstream revisions, license, Lean compatibility, proof and adapter
size, build/check time, and the cost of updating both proofs after one semantic change. Inspect
the resulting theorem's transitive axioms: no `sorryAx` or unclassified oracle may establish the
claim. Reuse wins only if the complete adapter plus proof demonstrably reduces work or maintenance
without weakening the theorem; otherwise retain plain Lean and record the rejection. Any adopted
library stays a pinned proof-side dependency, with no new compiler/Wasm runtime requirement.
Concrete semantics, ghost erasure and compiler refinement remain separate obligations. No dependency,
language change, soundness claim or change to the execution queue is authorized by this survey alone.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0227** (superseded): superseded by R-0499/R-0507: merge into checker rules and soundness.
- **R-0267** (superseded): superseded by R-0499/R-0507/R-0495: merge; capabilities and purity remain distinct.

### Task R-0508

**Phase:** A

**Objective:** State and progressively prove the new compiler’s maximal-behaviour refinement, independent frontend correspondence, resource adequacy and exact-image claim transport with explicit residual assumptions.

**Work:** Dispose of the audit and draft the whole Core interface for owner approval before implementation. State the approved semantic contract, theorem holes and trusted base in Lean; implementation and proofs grow with the supported slices.

**Deliverables and acceptance:**

Prove every ledger
obligation by simulation or validator soundness. Define refinement over maximal behaviours under
related initial states and admissible environments, with the explicit observation relation for
values, external memory and ghost erasure, and a preserved target-memory validity invariant; prove
native-to-Core refinement for executions satisfying the stated contracts, composed across separate
compilation and linking; state the resource relation and prove the sufficient-resource theorem.
Prove frontend correspondence: source bytes and import roots → HIR → `CheckedCore` against
independently specified syntax, resolution and elaboration judgments (a relation defined as
"whatever the elaborator produces" does not count). Prove functional-model / obligation generation against the new Core semantics (R-0460).
Each binary-transported claim cites a theorem that its proposition is preserved by the refinement
relation; receipts record the theorem, profile and compiler build.

**Exit:** the composed source-bytes-and-import-roots-to-native-image theorem, including
independently specified frontend correspondence, is proved and re-checked by con-leche and nanoda.
HIR-only and `CheckedCore`-only theorems are intermediate milestones. Every binary-transported claim
cites its preservation theorem.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

### Task R-0509

**Phase:** A

**Objective:** Maintain the new compiler’s explicit trusted-base inventory and prove evidence binding and interpretation soundness, retaining independent-kernel replay and hostile consumer controls from the first authoritative claims.

**Work:** Dispose of the audit and draft the whole Core interface for owner approval before implementation. State the approved semantic contract, theorem holes and trusted base in Lean; implementation and proofs grow with the supported slices.

**Deliverables and acceptance:**

Start the published trusted-base inventory immediately. It is non-increasing for a fixed supported
scope; additions for newly admitted scope are recorded and reviewed separately. Record a separate
compiler/checker execution-trust reduction plan with milestones; R-0413 explores only the
Concrete-hosted path. Verify `std` trusted implementations against contracts or list them with an
audit; the fixed-scope unaudited/unverified list only shrinks.

R-0353 independently checks retained evidence: decoding, authenticated roots, subject/dependency
binding, exact propositions, replay and policy, without trusting stored verdicts. Envelope or replay
success never upgrades unchecked source/model/native semantic edges. Require a minimal independent
retained-artifact consumer for every publicly authoritative first-slice claim; codec/hostile-input
prototypes need not wait for the later complete cross-profile consumer.

Prove soundness of `ClaimSpec` statement generation, evidence-to-subject/proposition binding,
authoritative verdict aggregation and retained-artifact checking. Audit transitive elaborated axioms:
kernel-only verdicts admit only named standard logical axioms; native-evaluation and float axioms
remain separately labelled.

Acceptance: every remaining assumption has an explicit owner; R-0353's acceptance criteria pass,
with every 079–095 fixture pinning its expected result vector. The four soundness proofs pass the
transitive axiom-policy gate. Independent checking and hostile-consumer controls exercise the
production path, without laundering semantic assumptions.

The inventory must name these trust entries and mitigations (with current state):

| Assumption | Required mitigation | Owner |
|---|---|---|
| Lean code generator/runtime and C compiler building Lean output | Kernel-checked per-compilation certificates; pin Lean/C toolchain until discharged; development execution remains trusted | R-0552 |
| `implemented_by` / `extern` replacements reachable from proved code | Inventory each replacement; prove correspondence or label it trusted; callable-identity controls also reject unchecked `csimp` replacement | R-0509 with R-0506 |
| Native-evaluation axioms (`bv_decide`, `native_decide`) | Register and label each use; replace with kernel proofs where feasible | R-0506 |
| Lean kernel | Recheck with both nanoda and con-leche | R-0509 |
| Assembler/linker | Named native-theorem premises until LCB encoding/composition discharges them; check final image | R-0517, R-0505 |
| Machine/ISA model | Named native-claim premise and model/hardware differential testing | R-0517 |
| Semantics fidelity to intent | Outside expert review, independent source judgments and semantics/interpreter/binary differential tests | R-0511 |
| libc, OS, FFI and trusted code | Every foreign call has a stated contract; audit or verify trusted implementations; formalize trusted-code obligations | R-0509, R-0514 |
| LLVM profile's BIR → LLVM printer, LLVM and system toolchain | Explicit profile trust accounting and final-artifact checks | R-0505 |

The inventory records decided behaviour and flags remaining owner questions; it cannot settle them.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0077** (superseded): superseded by R-0503/R-0509: merge with `rt` contracts, support inventory and trust accounting.
- **R-0211** (superseded): superseded by R-0509/S16: merge into R-0509; retain audit command acceptance. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0211)
- **R-0230** (superseded): superseded by R-0509/S16: merge into R-0509 and slice profiles.
- **R-0236** (superseded): superseded by R-0506/R-0509: merge axiom classification and inventory.
- **R-0264** (superseded): superseded by R-0494/R-0509: merge implementation into R-0494, soundness proof into S16.
- **R-0265** (superseded): superseded by R-0495/R-0509: merge into statement/binding/aggregation soundness.
- **R-0266** (superseded): superseded by R-0509/S16: merge; distinguish trusted assumptions from ordinary proof dependencies. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0266)

### Task R-0511

**Phase:** A

**Objective:** Review representative Core design proofs before interface freeze and deliver cumulative new-compiler slices with honest theorem scopes, retained target programs, exact artifact binding and independent checking.

**Work:** Dispose of the audit and draft the whole Core interface for owner approval before implementation. State the approved semantic contract, theorem holes and trusted base in Lean; implementation and proofs grow with the supported slices.

**Deliverables and acceptance:**

Record bounded proof-library deliverables with every slice increment: refinement, clock,
memory-injection, substitution and ownership libraries. Before Core interfaces freeze, land
machine-checked design examples for move/destroy, downward exclusive/shared borrows, reborrowing,
structured cleanup and typed-value representation in byte/provenance memory. Record an outside
expert review of the semantics and design examples.

Deliver the first checked-Core → executable-BIR refinement for `i64`, `u64`, `bool`, locals,
assignment, conditionals, general loops with structured exits, non-recursive direct calls,
comparisons, checked `+ - *` and capability-authorized `print_int`, covering termination, traps and
divergence. Add remaining widths, casts, division, shifts, recursion and loop forms cumulatively.
Native AArch64 uses the proved BIR → LCB adapter and an applicable pinned preservation theorem
(R-0517). C01–C03 certify typed assembly only; rendering, encoding, assembler, linker, loader and
ISA assumptions remain named until discharged. R-0505 supplies final-image correspondence and
R-0508 proves composition through the exact image, runtime and startup; mutation-tested checks
alone do not prove validator soundness. Restricted-source correspondence uses independently
specified syntax, resolution and elaboration judgments, proved or certificate-checked. Each
published slice claim has canonical task identity and an independent retained-artifact consumer.

Acceptance: under related initial states, admissible environments and recorded resource premises,
prove maximal-behaviour refinement to the identified native image, the explicit observation
relation, target-memory validity invariant and sufficient-resource theorem. Prove subset checker
soundness and capability containment and every used pass/validator. Transport at least one
nontrivial Core claim to its binary. Recheck the closure with both con-leche and nanoda, bound to
exact retained artifacts. Publish a `CheckedCore`-origin theorem until frontend correspondence is
proved; source-to-native wording requires that proof. Admission and profile selection never
silently fall back.

Retain `sum_to`, `max3`, `checked_trap`, `counter_u64` and `diverge` under `examples/v1a/` as
acceptance/regression targets. The first four carry source-linked proved claims: a Lean-reference
sum, maximum membership/order, non-trapping checked arithmetic, and final counter value.
`checked_trap` also pins overflow/underflow behaviour. `diverge` checks a `print_int` prefix and
bounded exhaustion, carries no claim, and does not claim finite observation proves divergence.
`scripts/tests/check_v1a_targets.sh` gives every target a leg: interpreter/native/independently
justified expected outcomes agree (including trap identity); functional-model comparisons agree
on every claimed function and claims read proved; checked-Core → BIR preserves recorded behaviour.
A subset control rejects another width, cast, division, shift, recursion, other loop form, aggregate
or reference before admission. Targets do not expand the theorem scope. Retain adapter semantic
vectors and cumulative, orthogonal slice/profile admission matrices.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0083** (retired): retired — there is no Concrete QBE backend to select; the no-silent-fallback rule moves to R-0511/R-0517. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0083)
- **R-0086** (superseded): superseded — its semantic vectors become adapter test vectors for V1 (R-0511/R-0517); later widths, casts, memory and floats follow the V-slices. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0086)
- **R-0097** (superseded): superseded by R-0511/R-0512: merge profile admission; retain no silent fallback. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0097)
- **R-0111** (superseded): superseded by R-0511/R-0137: merge reusable exits; retire required QBE showcase. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0111)
- **R-0231** (superseded): superseded by R-0511/R-0512: merge into each slice’s admission controls.
- **R-0238** (superseded): superseded by R-0511/R-0512: merge into cumulative slice matrices; retain orthogonal policies.
- **R-0263** (superseded): superseded by R-0511/R-0512: merge into V1–V7 slice ordering.
- **R-0277** (superseded): superseded by R-0511/R-0512: merge into each slice’s cumulative acceptance.

### Task R-0513

**Phase:** T

**Objective:** Trustworthy, affordable gates: checked inventories instead of incidental counts, and a
fast push tier separate from full qualification.

**Status (2026-10-19): planned; queue row 2.** On 2026-10-18 – 19 every legitimate corpus change tripped
several count pins (the corpus split, the convergence inventory, the identity freeze), and the full set
took hours under load, then an hour of CI per push.

Work: replace incidental corpus cardinality pins with independently checked inventory and coverage
properties — every expected claim and comparison accounted for, exclusions with explicit reasons, and
missing entries, skipped work or unexpected eligibility shrinkage failing; keep minimum completed
comparisons by semantic feature and permanent positive/negative controls; test the gate machinery with
deleted inventory entries, disabled generators, empty suites and altered eligibility. A fast push tier
targeting five minutes, selected by the dependency closure of changed semantics, implementations and
proof infrastructure; the full tier on the exact candidate commit before qualification or release, and
nightly; semantic, checker, evidence and trusted-base changes run their complete affected proof and
regression closure; remote runners for the full tier with local reproduction kept; the Linux-only
claim-inventory step in the local full tier.

**Impact-based selection (2026-10-22, [development throughput](../../ROADMAP.md#development-throughput) item 9).** Today the
pre-push hook selects about 92 of 255 gates by area name, and stack-budget, trust-gate, float
round-trip and linear-discard failures were first seen in CI. The fast tier is to select by an
impact map from source files and fixtures to the gates that exercise them, so a change runs every
gate it can break. A path the map does not cover is to select the full set, so the map fails closed.
Interim checkpoint before the five-minute budget: pre-push median ≤ 10 min, and over 20 consecutive
landings no gate failure is first seen in CI. **Depends on:** R-0547, which records where each
failure was first seen.

**Exit:** adding a legitimate proved example requires no incidental count edit; deliberate coverage loss
fails; the fast tier meets its recorded budget; full results bind exact commits, toolchains and
artifacts; a fast-tier pass is never presented as full qualification.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0096** (superseded): superseded by R-0513/R-0491: merge; remove mandatory QBE availability.

### Task R-0514

**Phase:** B

**Objective:** Make the new compiler consume the exact identity-resolved Core payload checked against independent judgments, expanding by slice while retaining checker, capability and artifact-substitution controls.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

New compilation consumes exactly the identity-resolved payload checked against independent Core
judgments. Use R-0492 identities, R-0497 immutable certificates and R-0499's single rule implementation
through the supported language. Both profiles consume exact `CheckedCore`; specialization produces
payload-bound `CheckedMonoCore`, the only lowering input, with R-0510 specialization validation and
post-specialization capability checks. Each slice uses the same checker/artifact interfaces and
restricted admission predicate, proving the trivial specialization instance where applicable.

Acceptance: string-identity lint covers the front end/checker; Core checking is authoritative for
every supported construct; the source checker holds no ownership, borrow, reference, capability or
Unsafe rule. Reject or correctly compile 087's borrow-block case, 090–094 and 096–098. Reject every
qualified rule-breaking fuzzer mutant and accept well-formed positives at pinned completed counts
per rule and feature combination. Explicitly record normative expected outcomes and reject
artifact/capability substitutions; checker implementation coverage and subset soundness proof
coverage remain separate. Checker rollout does not wait for whole-language compiler proofs.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0116** (superseded): superseded by R-0497/R-0510/R-0514: merge; use payload binding and new IR chain. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0116)
- **R-0274** (superseded): superseded by R-0499/R-0514: retire source-checker/CompilerDB authority prescription. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0274)
- **R-0437** (superseded): superseded by R-0514/R-0510: merge declaration and specialization checks; preserve distinct diagnostics. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0437)

### Task R-0552

**Phase:** B

**Objective:** Per-compilation certificates (approved by the project owner 2026-10-25), beside
R-0517. A release build emits, with each output, a certificate that the output preserves the source
program's semantics, and the certificate is checked by the Lean kernel or by an independent kernel
(nanoda, con-leche). For artifacts whose certificate checks, Lean's unverified code generator, the C
compiler that builds the Concrete binary and the Lean runtime leave the trusted base: the theorem is
about the checked artifact, not about the binary that produced it. Development builds may keep
trusting the Lean-built binary and need not emit a certificate.

The check must run in a kernel. A separately compiled checker binary would not close the gap: it is
itself built by the same unverified code generator and C compiler, so it would only move them from the
compiler to the checker.

The certificate is stated against the same theorems the slices prove (V1a's checked Core → BIR
refinement first, then each later slice, and R-0517's composition for the native path); it adds no
semantics of its own. R-0509's trusted-base inventory records, per profile, which entries a checked
certificate removes. Self-hosting, that is bootstrapping the compiler through itself in the style of
CakeML, is recorded as an optional, much later alternative for removing the same entries; it is not
planned.

**Exit:** for V1a's admitted programs, a release build emits a certificate per output; the Lean kernel
and at least one independent kernel accept it; a mutated output, a certificate for a different input,
and a certificate checked against a different source each fail the check; and R-0509's inventory marks
Lean's code generator, the C compiler and the Lean runtime as removed for certified artifacts of that
subset and still trusted for development builds.

### Promoted from roadmap prose (2026-10-19 reconciliation)

The tasks below give an owner and an exit to open work that was previously stated only in prose
(the source lines, at commit `7b134645`, are listed in the [reconciliation record](ROADMAP_RECONCILIATION.md)).
Each records what it absorbs; none deletes the prose it came from.

### Task R-0518

**Phase:** T

**Objective:** Close the mutation-harness lifecycle defects H-1, H-4, H-7 and H-9 recorded under the
post-R-0004 qualification checkpoint, each independently.

**Status (2026-10-19): planned; under R-0513's gate-health checkpoint.** H-2 and H-6 merge into
R-0513 directly; H-3 and H-5 are closed. H-7 carried a "before the first qualified campaign"
deadline that the 2026-10-11 qualification passed without meeting; that deadline is recorded as
missed, and the published sound-by-argument invariant (one authority consumer, inside
`candidate_incoherent`'s qualification branch, reached only when `SCOPE_NOTES` is empty) is gated
until the fields are separated.

**Exit:** H-1 — a supervisor signalled during launch keeps the lock while any campaign child
survives, with a signal-injection control; H-4 — the golden copy re-checks its own `run_id` after
copying, with a concurrent-replacement control; H-7 — integrity refusals and scope notes are
published as separate fields and the authority consumer reads only the former; H-9 — lock ownership
records a namespace-independent identity so a sandboxed run's lock is reclaimable, with an
`--unshare-pid` control. Each item closes separately with its control and commit.

### Task R-0519

**Phase:** T

**Restructured (2026-11-02):** retired with the legacy compiler: Mutation 52 targets old Elab, while worktree support and campaign integrity are independent surviving obligations. surviving requirement: Retain worktree-safe campaign locking, measured shard balancing and serial/sharded verdict agreement, explicitly disposing of mutations whose legacy target is deleted. This line governs over older implementation directions for the legacy compiler.

**Objective:** Finish the mutation campaign's remaining completeness work: the mutation-52 survivor,
full worktree support and measured shard balancing.

**Status (2026-10-19): planned.** Mutation 52 ("Elab V2 input: resolved field use omitted") survived
on main and on the R-0484 branch; the later sharded campaign does not establish its closure, because
it ran a different inventory. Worktree support has only its early refusal (2026-10-03). Shard
balancing is performance-pulled.

**Exit:** mutation 52 is killed by a named gate leg in a qualified run, or retired with a recorded
reason; the harness discovers checkout-local and shared Git state with `git rev-parse` and passes a
two-worktree lock test; shard assignment uses measured durations once a campaign exceeds its job
budget, with serial/sharded verdict agreement checked.

### Task R-0521

**Phase:** B

**Objective:** Compiler provenance and one canonical production `VerificationTask` (queue row 9).
Separate source-build provenance from the identity of the executable bytes that actually ran, and
build every verification task through one constructor binding the exact subject, proposition,
contract state, rule set, dependency material and environment.

**Status (2026-10-19): planned.** A minimal form is required with V1 (every publicly authoritative V1
claim needs minimal task identity and executable provenance); the cross-profile form completes at
row 9. Receipt `importsId` and proof-workspace binding gaps merge here.

**Exit:** a receipt names the executable digest it was produced by, not only the source and build
recipe; substituting a differently built compiler with the same source identity is detected; every
production task is built by the one constructor, and a hand-built task is refused by status,
issuance and R-0353's consumer.

### Task R-0528

**Phase:** B

**Objective:** Cross-command resource limits: bound input size, memory, time, import closure and
diagnostic volume for every command that reads untrusted inputs (receipts, tasks, manifests,
policies, packages, source).

**Status (2026-10-19): planned.** Each limit names its owning command; a limit is a refusal with a
diagnostic, never a silent truncation.

**Exit:** each listed input class has a documented limit and a hostile fixture exceeding it that is
refused with a stable diagnostic; inputs at the limit are accepted.

### Task R-0553

**Phase:** B

**Objective:** Require new semantics, checkers, validators and proved passes to be total and executed as proved, with an environment inventory and controls for opaque or replacement execution gaps.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

R-0506 owns the totality requirement; R-0553 owns the environment inventory, ratchet and total
implementations. Inventory every `partial`, `unsafe`, `opaque`, `implemented_by` and `extern`
declaration reachable from the new pipeline. The first three hide proof-visible bodies; the latter
two require executed-replacement correspondence even when the Lean body unfolds. Include `csimp`
replacement controls. CI and pre-push ratchet the inventory so gaps only shrink for fixed scope.

Use structural recursion, proved decreasing measures for worklists/fixpoints, and explicit fuel
with a distinct exhaustion outcome for possibly divergent evaluation. Remove execution replacements
or record why they remain and how correspondence is checked. Each conversion updates its baseline
rows in the same commit. Work stage by stage ahead of dependent pass proofs; a prevention-gate
milestone cannot stand in for total implementations.

Acceptance: semantics, checker, validator and proved-pass stages contain no non-total baseline row;
only genuine untrusted search checked by a total validator may remain, each with its reason. The
gate then refuses other non-total declarations there. Extend this requirement to the whole new
compiler: convert every remaining row or justify genuine untrusted search. Do not port the frozen
compiler's 664-definition conversion programme.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

### Task R-0554

**Phase:** A

**Objective:** Keep unique specification and document authority, manifest-driven tests, drift and dead-code controls, generated-artifact discipline and branch hygiene, applying structural boundaries to the new compiler as components appear.

**Work:** Dispose of the audit and draft the whole Core interface for owner approval before implementation. State the approved semantic contract, theorem holes and trusted base in Lean; implementation and proofs grow with the supported slices.

**Deliverables and acceptance:**

Owners: the cleanup agent (`wip/cleanup-*`) and spec agent (`wip/spec`). It is scheduled by its queue
row under the [scheduling rule](../../ROADMAP.md#scheduling-rule), with no independent early-phase exit.
Each sub-item lands independently, never preempts defect work or weakens an exit, and chooses no owner
language decision.

- Spec agent: absorb remaining `docs/language/*.md` from `docs/spec/unabsorbed.tsv` into the one
  normative specification and delete absorbed files (R-0555); book language chapters teach by
  example and link rule IDs rather than duplicate rules.
- Cleanup agent: finish `docs/project/` consolidation into `PROJECT.md` plus contributor documents;
  finish `docs/verification/` consolidation into one normative verification specification plus rationale.
- Cleanup agent: retain the `docs/NOTES/` → `research/` move and canonical ownership of moved notes.
- Cleanup agent: make gates, `run_tests.sh`, CI and Makefile manifest-driven, merge duplicates and
  establish the `tests/` taxonomy.
- Cleanup agent: build a dead-code finder with a ratchet gate.
- Cleanup agent: keep generated artifacts out of git; `concrete.wasm` is built from the published
  commit by the site workflow, with one generated-artifact authority.
- Cleanup agent: delete merged remote branches and periodically prune merged/dead worktrees.
  Execution follows explicit session publication constraints.

Report splitting, the old Proof/ProofCore overlap audit and old string-identity repairs are
specifically superseded by the [new-compiler retention decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract).
The new report/evidence and identity boundaries remain required under R-0481/R-0492; this does not
schedule repair of frozen compiler code. Acceptance requires the named consolidation, manifest,
ratchet, artifact and hygiene deliverables, not merely a general cleanup survey.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

### Task R-0555

**Phase:** A

**Objective:** Complete audit disposition and maintain one Lean-generated normative specification with independent judgments, rule-indexed conformance, validated bug-rule references and chapter acceptance artifacts through the language slices.

**Work:** Dispose of the audit and draft the whole Core interface for owner approval before implementation. State the approved semantic contract, theorem holes and trusted base in Lean; implementation and proofs grow with the supported slices.

**Deliverables and acceptance:**

Owner: spec agent (`wip/spec`) for specification completion; a separate agent owns bug-rule
integration and backfill. It is scheduled by its queue row under the
[scheduling rule](../../ROADMAP.md#scheduling-rule). Work is paused for soundness repairs and never
becomes a predecessor for unrelated work.
No workstream answers an owner question: unresolved behaviour becomes an `open` decision entry.

Each chapter closes independently with absorbed prose/rationale, rule-ID accept/reject fixtures,
its Lean definition and generated `Semantics:` link. Every chapter must have a required mechanized
link to an independent formal definition; linked chapters cannot regress. Preserve arithmetic,
typing, reference well-formedness, pattern binding, capabilities and trust rule coverage in
`ConcreteSpec`, rather than copying legacy implementation as semantics. Absorb all remaining
`docs/language/*.md` and archived language-scope text before dependent migration; tracking moves to
roadmap, holes or research and absorbed files are deleted. Cover expression evaluation, generics /
specialization, lexical structure, compiler-special builtins and abort/failure semantics; every
refusal has a reject example. Soundness proofs wait for their chapter links, not whole-spec completion.

Use the existing rule-ID fixture index, not a second conformance corpus. It runs on every supported
implementation, including native and interpreter. Report every forbidding rule without a reject
case; that list must be empty or each entry explicitly `open`.

Bug filing requires `Violates: <RULE-ID>@<version>`, `Spec gap`, or `Open: OPEN-n`;
`scripts/gen/new_bug.sh` requires `--violates RULE-ID`, `--spec-gap` or `--open OPEN-n`.
A spec-gap fix starts by writing the rule as spec work; an open-question fix stays parked until
the owner answers. Not-a-bug verdicts cite the permitting rule. Validate IDs against
`docs/spec/ids.tsv`; ledger/dashboard counts cover violated rule, spec gap and open disposition.
Backfill all open records and 2026-10 records, and older records within their absorbing chapter,
without blocking unrelated deliverables. Older records may say `Violates: unrecorded`, but that
allowance's share only shrinks; no record filed after the gate lands may use it. Acceptance: the
rule-reference gate holds on every record, every chapter is linked and complete, `docs/language/`
holds no document, and the conformance reject-gap list meets the empty-or-open rule. Current
measurement belongs in `docs/spec/MAINTAINERS.md`, not a copied status count here.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

## Area: Standard Library And Core APIs

Goal: build the small standard library people need before real workloads,
packages, editor tooling, freestanding targets, and release work can be honest.

Done when: a normal C/Rust-style Concrete program can use documented core APIs
for errors, bytes/text/path, collections, formatting/parsing, I/O capabilities,
tests, and oracle helpers, with every public stdlib item carrying an evidence
class and authority/allocation story.

**Excellence exit contract.** the stdlib area is judged on *honesty, not breadth* — the
one stdlib where every public API's authority, allocation, failure, ownership,
and evidence are visible and gated, over a proven pure core. Breadth is copied
only after that holds. Four properties gate the phase, each backed by a check,
not prose:

- **Capability-complete.** Every hosted operation carries its exact domain
   capability — `fs`→`File`, `env`→`Env`, `time`→`Time`, `process`→`Process`,
   `net`→`Network`, `console`/`io`→`Console`. `trusted` internals may use
   `Unsafe`, but the public wrapper must re-expose the real cap (a `trusted`
   extern requires *no* capability, so authority collapses into `Unsafe` unless
   re-added by hand). The stdlib manifest gate fails if a hosted op is behind
   `Unsafe` alone.
- **One IO spine.** All output targets a single `io.Writer` (fn-pointer handle,
   authority at acquisition, no `dyn`), and there is one `File` type: `fmt`,
   files, console, `std.test` output, logs/progress, and future sockets are all
   `Writer` producers/consumers. No second sink, no duplicate `Writer`/`File`.
- **Linearity-real.** Collections destroy live non-`Copy` elements on
   `drop`/`clear`/`remove`/overwrite (not merely reclaim the buffer); no hidden
   `Clone`, no implicit move-out from indexed containers; deterministic map/set
   traversal. Until non-`Copy` element drop is wired, the limitation is recorded,
   not silent.
- **Self-enforcing + selectively proven.** The five-fact manifest (allocate / consume /
   fail / capability / proof-class), derived from compiler facts and gated
   (the stdlib manifest gate); the strict error rule (`Option` for absence, `Result` for
   recoverable domain/environment failure, trap/abort for invariants / OOM /
   bounds / arithmetic, explicit cross-module wrapping, no implicit conversion
   web); and the stable, workload-pulled pure core (`option`/`result`, selected
   `bytes`/`numeric` helpers, and checked conversion boundaries) carrying Lean
   evidence where the API has stopped moving. Do not try to prove all stdlib
   APIs before workloads validate their usefulness; every API needs an evidence
   class, but only stable central pure APIs should be upgraded to `proved`.

These enforce a three-layer shape: a **pure core** (no capabilities, no
allocation unless explicit), an **alloc layer** (`with(Alloc)`, later explicit
allocator values), and a **hosted authority layer** (every API names its domain
cap). Iteration is internal only (`for_each`/`fold`/`_ctx`, optional
`Continue | Break`, no iterator trait or lazy adapter tower). Hold breadth —
broad crypto/compression/networking/threading and framework-style APIs — until
the four properties hold and a workload pulls it; the pure core is the priority,
not the periphery.

### Confirmed Stdlib Boundary Corrections

This is a specification set for verified drift in the
stdlib that already exists. It is not a coordinated parity migration and does
not block unrelated workload-pulled the stdlib area slices. Only confirmed contract
violations and the narrow infrastructure needed to prevent their recurrence
are immediate; broader API shapes below remain owned by the workload that
demonstrates the need.

Canonical rule:

> A public ordinary stdlib API exposes typed values, ownership, its exact
> domain capability, allocation, and recoverable failure. Raw pointers,
> platform sentinels, C strings, ABI structs, and `Unsafe` stop at a narrow
> trusted adapter boundary. Explicitly raw modules and `_unchecked` functions
> are the visible exception.

Execution rule: land each confirmed correction as its own green vertical slice
with a discriminating fixture, mutation check, manifest/API-snapshot update,
and migrated consumers. Do not build Command/Rng/SystemTime/digest abstractions,
unify every handle, or redesign every error type merely for symmetry. Deferred
notes become work only when their named workload or failing gate pulls them.

> Moved to [ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md#global-task-sequence) (historical record; nothing in it schedules work).

### Task R-0004

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The historical closure needs no migration, but the body explicitly retains future identity, task, consumer and assurance obligations. surviving requirement: Preserve exact-subject freshness, complete dependency binding, explicit trust and kernel-replay authority for new claims, with minimal provenance and independent consumption before later evidence productization. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** done — closed 2026-08-20; the body is retained for its explicit follow-ups and historical boundary; physical move to the archive under R-0481.

**[shipped] Objective:** Fix proof-subject freshness and fail closed. R-0004 closed on 2026-08-20;
its closure result and accepted limits follow.

**Closed 2026-08-20** on a protected serial closure run at `a839d519`: 205 of 205 gates discovered,
executed and passed, clean tree, `HEAD == origin/main`. `proved` now requires a current exact subject,
a complete dependency root, explicit trust and assumptions, and a receipt only a successful kernel
replay can mint. The closure record is archived in
[ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md#r-0004--closed-2026-08-20).

**Accepted limits at closure (each now owned by a live task):**

- `PackageIdentity` over-binds (whole dependency closure; definitions scoped by package content) —
  fail-safe demotion and renaming, never a misattributed proof; the five-way identity redesign is R-0482.
- Post-build executable bytes are unclaimed: the compiler identity names the build, not the bytes —
  R-0521 (compiler provenance).
- Ten v1 proof links stay pinned because the structural producer refuses intrinsic casts; they migrate
  with claims on Core (R-0495/R-0512).
- The closure run did not include the mutation campaign; the post-closure campaign qualified on
  `670ec85a` (2026-10-11, run 38104755470).

The **Post-R-0004 milestones and longer-term research** subsection below remains current future work.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0004).

### Post-R-0004 milestones and longer-term research

These are recorded so they remain visible without distracting from the strict critical path below.
They are not R-0004 blockers. This is the canonical sequencing/decision map, not a second copy of the
detailed task specifications: R-0482, R-0353, R-0473/R-0474, R-0440 and the later phase tasks own
their implementation detail and gates.

**Evidence architecture**
- Five-way identity migration under R-0482: `PackageScopeIdentity` names declared publisher/package
  scope without dependency content; `PackageArtifactIdentity` binds canonical `(module identity,
  content digest)` pairs for the package's own artifact; `ResolutionContextIdentity` binds the
  resolved dependency/toolchain context; `DefinitionIdentity` binds scope, module, declaration and
  implementation; claim dependency roots carry reachable dependency content. Add declared
  origin/namespace and bootstrap attestations, generated references, receipts and consumers
  atomically. No mixed identity-version fallback. Fix duplicate enumeration at its producer; never
  erase legitimate duplicate-content modules to stabilize a digest.
- Compiler provenance hardening: retain the portable source-build identity, then bind source/build
  provenance, build recipe/toolchain and final executable bytes as distinct facts through a portable
  streaming digest or signed build manifest. The current build identity deliberately does not detect
  post-build binary patching.
- Canonical production `VerificationTask`: one authoritative constructor binding exact subject,
  proposition, contract state, rule set, dependency material, and environment. Complete before
  freezing receipt v1.
- R-0353/schema conformance: publish experimental canonical task/receipt encodings and hostile
  conformance fixtures, implement a consumer independently of the compiler pipeline, and freeze
  receipt v1 only after both implementations agree. Receipt validity remains distinct from policy
  acceptance.
- Model-faithfulness evidence: explicit relation kinds and independently checked
  `CorrespondenceEvidence` under R-0482.
- Package-level evidence composition: how dependencies consume partial evidence, how upgrades
  invalidate claims, and how absent dependency evidence is represented.
- Typed `PolicyDecision` under R-0440: policy consumes fresh receipt facts, trust, assumptions,
  advisories, and release requirements; a valid receipt is not automatically a release acceptance.
- Axiom and assumption inventories: record Lean axioms, trusted declarations, runtime assumptions,
  and imported hypotheses per claim.
- Evidence revocation and rollback: withdrawn proofs, compromised toolchains, revoked package
  versions, rolled-back receipt stores.
- Signed transport without authority inflation: DSSE/in-toto envelope authenticates bytes, never
  upgrades the evidence class.
- Schema lifecycle and migration: compatibility windows, unknown-field handling, canonical decoding,
  schema bumps, migration tooling, and controlled downgrade of old receipts to `needs_recheck`.

**Trust and compiler boundaries**
- Source-to-VIR bridge audit: keep it small, versioned, and independently reviewable; gradually
  prove or translation-validate critical portions.
- Toolchain-upgrade migration: a complete recheck plan for Lean, compiler, proof-library, and
  rule-set upgrades.
- Reproducible release artifacts: compiler source, dependencies, build recipe, target, linker/
  runtime inputs, executable digest; separate from ordinary proof receipts.
- FFI, `Unsafe`, runtime, and backend boundaries: explicit dispositions for claims crossing foreign
  code, trusted runtime, platform adapters, or backend assumptions.
- Platform qualification: exercise receipt production and consumption on every supported host,
  including filesystem semantics, process interruption, and line endings.

**Language and specification work**
- Exact contract witnesses under R-0473/R-0474: canonical typed `ContractIdentity`, theorem-to-spec
  binding and imported hypotheses distinguish contract dependence from implementation dependence.
- Narrow two-state mutation follows that substrate: `old`, `modifies`/frames, record/array state,
  then ghost locals/parameters/state. Only afterward does the narrow mutable-state extraction convert
  the ninth table; its existing fail-closed/no-authority disposition remains until then.
- Checked total functions and `#[decreases]`, followed by the spec standard library, precede richer
  or relational logic.
- Specification adequacy and vacuity controls: policy-visible adequacy checks, contradiction
  detection, and permanent vacuity controls.
- Relational properties: noninterference, constant-time behavior, equivalence, refinement.
- Closed-world library evolution: enumerable callable sets, compatible upgrades, and escape hatches.

**Robustness and operations**
- Parser and decoder fuzzing: receipt, task, manifest, policy, and package decoders.
- Resource limits and denial-of-service behavior: bounded input sizes, memory, time, import closure,
  and diagnostic output.
- Atomic evidence persistence: temporary files, fsync, rename, permissions, concurrent readers,
  interrupted writes, crash recovery.
- Gate ownership and reachability: every gate names its owner, freshness producer, mutation
  coverage, CI lane, and last successful execution.
- Long-running stability and nondeterminism: scheduling, locale, directory, process-count, and
  filesystem variations.
- Performance baselines: replay time, receipt size, dependency-closure cost, memory use,
  invalidation breadth.
- `ProofCache` remains performance-pulled rather than early post-R-0004 work. When a measured
  bottleneck justifies it, cache-off/on, cold/warm, missing, stale and corrupt cases must agree on
  authority before the cache graduates; cache contents never mint or upgrade evidence.

**Product and governance**
- External-user dependency-upgrade workflow: receive package/evidence, replay independently, replace
  a dependency, and obtain an exact machine-readable and human explanation of every claim that stayed
  current or moved without access to the producer checkout.
- IDE/CI evidence lenses and a recurring non-author ergonomics audit follow that vertical workflow
  rather than substituting for it. The public example portfolio is capped at three deep flagships:
  HMAC-SHA256, a secure update-bundle verifier that combines bounded parsing with file integrity,
  rollback policy and transferable evidence, and a small protocol state machine. New compiler
  behaviours use dedicated fixtures; they do not create another showcase by default.
- Human-readable evidence explanations: which exact subject, theorem, dependency, trust boundary,
  or environment field moved.
- Evidence-diff format: stable machine-readable diff for upgrades, toolchain changes, trust widening,
  schema migration, and proof replacement.
- Independent implementation and security review of R-0353, schema, verifier, and hostile corpus.
- Release assurance bundles: claims, receipts, policy decisions, assumptions, dependency evidence,
  toolchains, replay commands, and unresolved refusals.
- Privacy and disclosure policy: avoid exposing checkout paths, usernames, private dependency
  locations, or confidential proof material.
- Ecosystem compatibility policy: compatible package upgrades from the evidence perspective.

**Longer-term research**
- Second proof-producing kernel (Rocq or another kernel) in shadow first; Why3 remains a comparative
  reference only.
- Verified or translation-validated compiler bridges.
- Certificate-producing automation; keep non-certificate SMT results explicitly `solver_trusted`.

**Language posture after the evidence-and-contract sequence**

Concrete should be described as an **evidence-bearing sequential-language core with stated,
bounded trust assumptions and an explicit, shrinking TCB**. Do not call the language or compiler
"verified" while any of these boundaries remains:

- **H19:** the Core-to-obligation bridge is unproven. Four of five Register A rows have their
  semantics half discharged, but none proves that the emitted obligation denotes that condition.
- **H20:** `bv_decide` certificate checking still runs as native code, so bitvector evidence trusts
  the checker binary at that boundary.
- **H21:** nonlinear SMT evidence cannot yet be certificate-replayed and therefore remains
  `solver_trusted` by an upstream limitation.

Keep three categories distinct when describing what follows:

1. **Philosophical-core gaps.** Model correspondence is first: exact identity prevents
   substitution, but it does not prove that a Lean model describes the selected implementation.
   R-0482 owns independent model identity, explicit relation kinds, and independently checked
   `CorrespondenceEvidence`. H19-H21 are the other named boundaries that the word "verified" waits
   on.
2. **Scheduled critical-path work.** Package evidence and typed policy (the packages area/R-0440 and the
   five-way identity migration), developer tooling (the editor area after schema stability), and resource
   bounds (the provable-subsets area profiles plus the runtime-safety area obligations) are owned and sequenced work, not unowned
   language holes.
3. **Optional, workload-pulled research.** Typestate remains research-gated unless the
   `protocol_state_machine` flagship demonstrates that ordinary contracts cannot express the
   required compile-time state discipline. If that pressure appears, pull forward only the smallest
   explicit mechanism that preserves auditability.

Permanent exclusions remain design assets rather than missing features: garbage collection,
exceptions/unwinding, hidden async runtimes, trait-object dispatch, capturing closures,
source-generating macros, implicit conversions, ambient authority, and broad invisible `unsafe`.
Postfix `?` joins this list: recoverable failure uses `Result`, exhaustive `match`, and explicit
`return Result::Err ...`. Concrete does not hide a function exit behind expression punctuation;
there is no implicit conversion, cleanup, drop, or capability use during propagation.

> Moved to [ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md#strict-post-r-0004-critical-path) (historical record; nothing in it schedules work).

No evidence-affecting language feature crosses the initial identity/task/verifier stabilization seam
unless its change includes the corresponding identity version, atomic migration, replay behavior,
consumer update and adversarial controls. Why3 remains a comparative reference and never enters this
chain as a backend. A second proof-producing kernel remains separately research-gated; it does not
block this product path.

> Moved to [ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md#historical-record-and-detailed-slice-traceability) (historical record; nothing in it schedules work).

> Moved to [ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md#binder-frames-position-and-meaning-are-separate-obligations) (historical record; nothing in it schedules work).

> Moved to [ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md#multi-kernel-where-the-ladder-actually-stands-2026-08-08) (historical record; nothing in it schedules work).

### Test-harness principles (2026-08-08) — rules, not tasks

Extracted from a review of the contract-validation work, and written here rather than in a commit
message because they are reusable and were each paid for once. R-0476 is the task that drains the
backlog they describe; these are the rules that stop it refilling.

**1. Every universal assertion needs a non-emptiness witness.** The hazard is a theorem, not a
style preference:

> all properties hold over the empty collection

`all(P(x) for x in coll)` is TRUE when `coll` is empty. So is "the forbidden string does not
appear in this output" when there is no output. Both report a pass having checked nothing. Unless
emptiness is the intended case, a universal assertion must be paired with a witness that its
collection is inhabited.

**2. A helper that asserts something about a SELECTED BLOCK must first prove the block exists.**
Empty selection must never mean success. `assert_block_absent` should distinguish three distinct
failures, which are not the same event:

1. missing anchor — the thing being inspected is not there;
2. malformed or unterminated block — it is there but cannot be parsed;
3. forbidden content present — the actual property failed.

Implemented today: 1 and 3. Case 2 is deliberately not implemented, because no report shape was
found that reproduces it, and an untested branch inside an assertion helper is worse than a named
gap. Recorded so the omission is a decision rather than an oversight.

**3. A fatal negative must not share a fixture with an unrelated positive.** Several positives may
live together, and several *accumulating* diagnostics may live together — but a negative that
becomes fatal erases the positive's subject, and the positive then passes for want of anything to
inspect. This is not hypothetical: promoting the impure-call defect from a report line to a check
error made `spec_ghost_totality`'s pure-helper control pass BECAUSE the file was rejected
wholesale and its report was empty. The control was green and proved nothing. The positive now
lives in `examples/contract_positive/pure_contract_calls`, where nothing else can fail first.

**4. Audit the shape, not the instance.** When one of these is found, the same shape is usually
live elsewhere. Fixing `assert_block_absent` left the identical defect in two `assert_absent`
helpers and in 23 universal JSON assertions. The places to look: `sed`/`awk` block extraction,
`grep` pipelines accepting empty input, assertions using `! grep`, command substitutions hiding a
failed producer, section checks where an earlier fatal diagnostic removed the section, and JSON
filters where an empty list makes `all(...)` true.

**5. Never describe a check as more than it enforces.** The impure-call check rejects
capability-requiring calls and reuses a diagnostic saying "spec/ghost must be pure and total" —
which claims more than it does. Capability-free does not mean valid in a proposition: such a
function may still trap, diverge, allocate through a badly summarised path, depend on runtime
state the contract semantics do not model, call trusted or foreign code, or have no total logical
interpretation. The rule that would be correct — *contract calls must resolve to admitted spec fn
declarations, or to executable functions with an explicit, checked logical interpretation* — needs
a notion that does not exist yet. Until it does, this is containment of EFFECTFUL calls and is
described that way (H27, R-0473).

**6. A failed search is not evidence of absence.** I reported "Register C has no row table" after
grepping `docs/verification/VC_BRIDGE_REGISTER.md` and finding nothing. Register C's rows were never in that
file — C1-C4 are compile-time theorems in `Concrete/Report/Evidence.lean`, the module they
constrain, which is the better place for them. The claim reached this roadmap and two status
summaries before an external audit caught it. Before recording an absence, establish where the
thing WOULD be if it existed; a grep that returns nothing has told you about the file you
grepped, and nothing else.

**7. A comment that claims a gate exists must be checked like any other claim.** While writing
`Check.impureFnsIn` I wrote that it and `ReportVC.impureFnNames` "are held together by a shared
gate assertion". No such assertion existed. The comment was load-bearing — it is the reason a
reader would accept the duplication — and it was false from the moment it was written. A claim
about the test suite, made inside the code the suite is supposed to constrain, is exactly as
checkable as a claim about behaviour, and nothing was checking it. The gate now exists
(`check_contract_negatives.sh`), which is the right repair: make the sentence true rather than
delete it.

**8. Resolved typed records must never be built on names.** Storing a name now with a promise to
replace it by an identity later puts the name in the record and in every consumer built against
it. This is why the identity substrate moved into R-0473 rather than waiting for R-0474, and why
`ResolvedContractRef` is a sum rather than a string with a companion type table.

### Task R-0480

**Phase:** A

**Reconciliation (2026-10-19):** Keep remaining lifecycle/authority gates; archive completed physical migration.

**Documentation information architecture.**

**Scheduling: incremental; physical moves wait for a stable integration boundary.** The ownership
and lifecycle rules may land immediately. Do not bulk-move files while R-0004 and its evidence
modules are changing: link churn, merge conflicts and stale references would obscure the work the
reorganization is meant to clarify. Move one topic family at a time, with link and drift gates
green before and after.

The physical migration of the stable documents into the `docs/` families landed 2026-08-14 (record
archived in [ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md#r-0480-physical-documentation-migration-2026-08-14)). Phase A's full lifecycle-header inventory and Phase B's
one-owner/duplicate-current-status controls remain open; physical neatness is not completion of the
authority audit.

**Objective:** make the repository answer three questions without requiring institutional memory:

1. Which document is authoritative for this fact?
2. Is this file normative, an active design, a stable reference, historical, or exploratory?
3. Where should a change to this subject be made so that a second source of truth is not created?

The canonical ownership map and precedence order live in `docs/README.md`; the research promotion
and retirement workflow lives in `research/README.md`. This task implements and enforces those
rules. It must not create a second roadmap, a second current-status dashboard, or a second copy of
normative prose under a cleaner directory name.

**Phase A — inventory before movement:**

- inventory every file under `docs/` and `research/` with lifecycle status, topic, canonical owner
  and—where applicable—the roadmap task or decision that consumes it;
- identify duplicate present-tense claims, stale status snapshots, adopted research that still
  reads as a competing proposal, and active designs with no named normative destination;
- add or normalize visible status/owner headers; dated measurements must name the commit,
  toolchain and denominator they describe;
- keep `CLAIMS_TODAY.md` as the shipped-claims authority and this file as the sole canonical task
  status. Changelogs and research notes may link to those facts but may not restate them as current.

**Phase B — executable information architecture:**

- gate internal Markdown links and repository-relative file references;
- gate the mechanically checkable subset of document ownership: required status headers, canonical
  owner links, no advisory state represented as a receipt fact, and no known duplicate current-
  status headings;
- require every `adopted` or `closed` research note to link to its normative owner; retain rejected
  hypotheses and dated measurements as history rather than silently rewriting them;
- distinguish the manual contradiction audit from automated checks. Prose judgment is not made
  trustworthy by putting an arguable heuristic in a shell gate.

**Phase C — staged physical layout:** use the target families documented in `docs/README.md`:
`docs/language/`, `docs/compiler/`, `docs/verification/`, and `docs/platform/`, while retaining the
existing `docs/stdlib/`, `docs/bugs/`, `docs/book/`, and `docs/NOTES/` roles. `research/` already has
topic directories; reorganize its contents by promotion/retirement state, not by duplicating the
normative hierarchy. For every moved family:

1. choose and record its canonical owner;
2. update all inbound links and code comments in the same commit;
3. leave a small pointer only where an external or durable path requires compatibility;
4. run documentation, reference and roadmap-drift gates before and after;
5. do not mix the move with semantic compiler changes.

**Acceptance:**

- every normative document has one lifecycle status and one owner entry;
- every adopted/closed research note points to the normative result, and exploratory notes contain
  no undated claim of current implementation status;
- one subject has one present-tense authority; contradiction searches name concrete forbidden
  formulations rather than relying on reviewer memory;
- broken-link and ownership gates fail non-vacuously under a removed owner, stale link and duplicate
  current-status mutation;
- at least one document family completes the staged move with zero broken references before the
  remaining families are scheduled;
- repository-root navigation remains small: `README.md`, `ROADMAP.md` and `CHANGELOG.md` point into
  the hierarchy instead of accumulating detailed design prose themselves.

### Task R-0481

**Phase:** A

**Objective:** Preserve unique temporal authority, canonical generated inventories, shared non-vacuous assertions and mutation registration, and enforce producer/consumer boundaries as new compiler and evidence components are built.

**Work:** Dispose of the audit and draft the whole Core interface for owner approval before implementation. State the approved semantic contract, theorem holes and trusted base in Lean; implementation and proofs grow with the supported slices.

**Deliverables and acceptance:**

#### Axis 1 — subject location and temporal authority are separate

Do NOT replace the subject hierarchy with `docs/normative/` and `docs/reference/`. A file's subject
determines its directory; its lifecycle and authority are typed metadata. Otherwise all normative
material becomes a second flat miscellaneous directory and cross-subject navigation degrades.

Every maintained document uses a visible lifecycle from R-0480 and, where it is not itself the
owner, names the canonical owner. The intended header model is:

```text
Status: normative contract | stable reference | active design | historical
Owner: docs/<subject>/<owner>.md
Current implementation: ROADMAP R-NNNN
Historical record: journal/<class>/<dated-entry>.md
```

`CLAIMS_TODAY.md` owns shipped public claims. Normative design documents own semantics and trust
contracts. `ROADMAP.md` owns active sequencing and qualitative transition state. `CHANGELOG.md`
owns landed milestone history. Research is never normative merely because an idea was adopted.

#### Axis 2 — journal for dated narrative

Create `journal/` for material whose value is historical context rather than current authority:

- `journal/incidents/` — failures, regressions, security advisories and fire drills;
- `journal/audits/` — dated corpus measurements, red-team reports and architecture reviews;
- `journal/decisions/` — dated decision rationale and supersession records;
- `journal/milestones/` — implementation narratives and release retrospectives.

Migrate dated roadmap narratives and bug write-ups selectively, not mechanically. (The four
`docs/NOTES/` notes already moved to `research/proof-evidence/` on 2026-10-29; they are research
records, not journal entries.)
An active reproducible defect specification remains in `docs/bugs/`; an incident narrative moves
to the journal and links the living bug/contract. A journal entry records date, commit, toolchain,
denominator, conclusion-at-the-time, current normative owner and supersession state.

"Append-only" means historical claims are never silently rewritten to appear prescient. It does
NOT forbid correction: an erratum or superseding entry explicitly preserves the original record
and links the correction. Journal text cannot define current status, policy or shipped claims.

#### Axis 3 — canonical generated status and artifacts

Add an `artifacts/` ownership layer. Canonical machine data does not live conceptually inside docs;
human documentation and CI render projections of it.

At minimum, generate and schema-version:

- `artifacts/status/corpus.json` — every discovered example/test/proof input, its stable identity,
  classification, eligibility, measured/unmeasured disposition and named refusal;
- `artifacts/status/verification.json` — subjects, stored links, current digests, correspondence,
  dependency roots, receipts, assumptions, trust and multidimensional evidence state;
- `artifacts/status/mutations.json` — mutation families, attacked invariant, production consumer,
  killing gate, positive control, last verified commit and toolchain;
- `artifacts/manifests/` — package, proof-link, implementation, theorem, table and dependency
  inventories consumed by migration/replay tooling;
- `artifacts/schemas/` — canonical wire/status schemas and compatibility/revocation rules.

`make status` (or its eventual equivalent) reads authoritative compiler/gate outputs and emits the
canonical structured facts plus an optional generated Markdown view. It never scrapes decorative
report prose when typed or structured facts exist. Generation must fail on missing denominators,
duplicate identities, unknown classifications, partial joins, failed producer commands and schema
mismatch.

Generated counts replace hand-copied numbers in status summaries: discovered files, measurable
files, proof subjects, stored links, root coverage, correspondence coverage, receipt migration and
mutation coverage each have one denominator-owning producer. A dashboard with an incomplete input
inventory is a refusal, not a smaller confident number.

The roadmap keeps qualitative status that cannot be safely inferred from counts: shadow-only,
blocked on a decision, implemented without a production consumer, acceptance condition open, or
authority transition forbidden. It LINKS generated quantitative facts rather than copying them.

#### Axis 4 — one gate assertion library and one mutation registry

Consolidate gate helpers into `scripts/tests/lib/`. The shared vocabulary must encode the failure
shapes repeatedly found in this repository, including:

- presence before content checks;
- absence only after proving the anchor exists;
- universal assertions only with an explicit non-empty witness/denominator;
- exact denominator and exact join checks;
- paired moving/non-moving controls;
- positive controls for negative validators;
- producer exit-status validation before stdout interpretation;
- typed/named refusal preservation;
- mutation registration and killed/survived/inconclusive dispositions.

Required conceptual helpers include `assert_present`, `assert_absent_after_anchor`,
`assert_nonempty_then_all`, `assert_exact_denominator`, `assert_join_complete`,
`assert_positive_and_negative_control`, `assert_command_succeeded`, and
`assert_mutation_killed`; exact names may follow existing shell conventions. Private copies retire
only after the shared helper has discriminating self-tests, including deliberate vacuity attacks.

Create one machine-readable mutation registry. Every security/evidence mutation records its stable
family ID, invariant, subsystem, production path, killing gate, positive control, authority state,
and last execution identity. "Which boundaries lack negative controls?" and "which mutations were
not run under this toolchain?" become queries, not manual audits. A registered-but-never-run or
synthetic-only mutation cannot be reported as production coverage.

#### Axis 5 — one corpus manifest

Generate a repository corpus manifest spanning `examples/`, `tests/`, `proofs/`, proof tables and
stored proof links. Each row has a stable owner and classification such as positive fixture,
negative fixture, showcase, workload, oracle, performance case, migration link or hostile input.
The manifest records whether the item reached the relevant phase and, if not, its typed refusal.

The manifest is also the prerequisite for reducing `examples/`. Classify before moving or deleting:

- `examples/` ultimately contains the three public flagships, one deliberately small onboarding
  sequence and only complete user-facing workflows; prefer documentation snippets over another
  standalone tutorial project;
- minimal compiler/language regressions belong under `tests/fixtures/` or the existing equivalent;
- hostile and substitution inputs belong under `tests/adversarial/`;
- performance inputs belong with benchmarks; and
- larger compatibility/porting programs are workloads, not additional flagships.

Existing paths are not renamed merely to make the tree look tidy. A shallow example moves only
after its regression value has a manifest row, an owning gate and—when needed—a replacement fixture.
This keeps fewer examples from becoming fewer tests.

This manifest owns denominators such as discovered/measured/unmeasured files and prevents two gates
from using identical-looking but independently maintained regexes. Per-file and per-subject joins
must be explicit; equal totals are not evidence that two inventories describe the same population.

#### Producer/consumer boundaries in the new compiler

Proof produces candidates; Evidence owns typed subjects, domain-separated identities/digests,
contracts, dependencies, correspondence, roots, receipts and policy inputs. Report renders validated
objects only: it cannot mint receipts, recompute roots, classify dependencies, reconstruct identities
from strings or independently derive evidence. Evidence must not import Report. The evidence ownership targets are Identity, Subject, Attestation, Contract, Dependency,
Receipt and Policy: package/definition/model/contract/proposition identities and typed digest
domains; implementation/proof-subject preimages; implementation/model/relation/scope attestations;
exported behaviour/effect/frame/resource interfaces; typed dependency edges, classifications,
table bindings, correspondence and roots; replay/environment/envelope serialization; and
assumption/trust/advisory acceptance. Do not collapse proof production into portable Evidence.
The proposition language has one semantics, canonical codec and domain-separated proposition identity; its structural consumer imports
neither frontend parsing/loading, reporting nor ambient prover elaboration. The standalone verifier
shares canonical schemas/semantics, not compiler/report implementation, and does not establish
source-to-proposition correspondence. Unknown constructors/imports/schema fields/dispositions fail
non-current; success is enumerated. Enforce import directions, closed constructors and production
mutations/compile-fail probes. Moving modules preserves behaviour, acyclic imports and updates every
code/comment/gate reference in one commit after the new identity boundary is stable.

The old Proof/Evidence relocation and TermIR-to-VIR promotion are specifically superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract).
Build those semantic ownership boundaries directly in the new architecture; do not duplicate the
proposition language. Its declared closed fragment, total structural evaluator, canonical
serialization, hostile parsing and Farkas/linear and DRAT/bitvector certificate bindings remain
explicit deliverables. Exclude ambient metaprogramming and irrelevant proof-kernel machinery;
review any extension's semantics, codec, evaluator, negative controls, compatibility/versioning,
trust surface and independent-parser implications together. Keep proof-production and portable-evidence
responsibilities separate; never pass raw string digests where typed domains exist.

#### Sequencing and completion

Preserve subject-based documentation; do not introduce flat normative/reference directories.
Pilot one dated audit/incident migration with explicit errata/supersession. Establish canonical
schemas and identity-complete old/new status comparisons before removing hand-kept counts; migrate
one gate family at a time after helper self-tests. Do not delete a count producer until the
replacement is denominator-complete and mutation-tested. Do not mix semantic edits with broad path
moves or create empty directory scaffolding for deferred work. Keep the conventional root entry
points, existing research/paper/site/std and corpus locations unless semantic ownership requires a
move; no second repository or decorative renaming programme.

Acceptance requires lifecycle/owner metadata on every maintained doc, adopted research naming its
normative owner, dated forward-linked journal entries and non-vacuous link/owner/contradiction /
journal-authority controls. Corpus/status/mutation artifacts reproduce across checkout paths and
reject missing, duplicate, partial, unknown and failed-producer inputs. Each canonical denominator
has one generated owner or is explicitly historical with SHA/toolchain. Shared assertion helpers
replace private copies after vacuity self-tests; the mutation registry answers coverage by invariant
and production consumer without counting unexecuted/synthetic-only controls as killed.
Import gates enforce the boundaries above. Removing report-side reconstruction cannot change
authoritative evidence, while mutation of the owning object changes all projections consistently.
The roadmap retains capability map, order, acceptance, blockers and qualitative authority transitions
(shadow-only, decision-blocked, no production consumer, open acceptance, forbidden authority), while
linking generated quantitative facts instead of duplicating status boards. Build, docs drift, local
links, roadmap linearity, import direction, corpus/status generation, gate vacuity and mutation-registry
gates pass from a clean checkout. Each begun component has its semantic ownership layout; deferred
components remain named targets. Journal, generated Markdown, changelog and research cannot become
competing current authorities; quantitative generators cannot replace qualitative owner decisions.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0447** (merged): duplicate of R-0481: merge unique operational controls; retire conflicting directory blueprint. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0447)
- **R-0478** (merged): duplicate of R-0481 / R-0474: merge each half into its actual owner. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0478)

### Task R-0479

**Phase:** B

**Objective:** Represent unavailable or failed computations without fabricating facts, and preserve refusals through typed results, text, JSON and exit codes with production controls.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Survey new report/diagnostic defaults, including `.getD`, string catch-alls and empty strings,
for claims not established by a failed lookup. Return a distinct absent/not-computed constructor
or `Option`, rather than an in-domain fact such as provenance `transitive` or an existing-record
`missing` status. Known honest markers (ABSENT, NO-SUBJECT, UNPINNED, no call-graph entry) stay honest.
A computed missing result is allowed; a failed read may use a ladder's bottom only with a per-type
allowlist reason establishing that it asserts no unsupported fact. Never rename a value to evade
the ratchet.

Preserve the distinction through typed results, text, JSON/artifact dispositions and process exit
codes. Unknown/new refusals default to non-success; success is enumerated. Gates check producer
exit status before interpreting stdout. Extend the ratchet per discovered closed vocabulary with
a non-vacuity probe on its actual assertion path.

Acceptance: every converted site uses an honest marker or justified per-type bottom; a production
report control proves an uncomputed fact cannot render as a valid fact. Count/ratchet suspect
catch-all and in-domain-default patterns so they cannot silently grow. Evidence cases join R-0494;
the broader diagnostic audit remains owned here.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

### Task R-0476

**Phase:** T

**Reconciliation (2026-10-19):** Merge gate-health checkpoint into R-0513; retain standalone freshness guards.

**Objective:** drain two ratchets to zero, so neither becomes a permanent debt marker that
everyone learns to read past.

**23 universal assertions have no non-emptiness witness.** `all(P(x) for x in coll)` is true when
`coll` is empty, so each passes when its collection is empty — proving nothing while reporting a
pass. `check_gate_vacuity.sh` counts them and fails if the count RISES, which stops the population
growing but drains none of it. A ratchet with no drain task is a debt marker, and debt markers
stop being read.

The fix per assertion is small (a `len(...) > 0` conjunct, or an explicit witness), so this is
volume rather than difficulty. Lower `BASELINE_UNIVERSAL` with each batch; the gate prints a NOTE
whenever the count drops below the baseline, so the ratchet tightens as a side effect of the work
rather than needing a separate step.

This matters more than its size suggests. The class is not hypothetical: a positive control in
`check_contract_negatives.sh` went green while proving nothing, because the fixture it inspected
was rejected wholesale and its report was empty. Two absence helpers had the same defect. Each of
these 23 is a place the same thing can happen silently.

**A third shape was added 2026-08-08, after an audit found a live instance**: unintended command
substitution inside a diagnostic string. `check_transform_register.sh` had backticks around
`partial` in an `echo`, so every run executed it, printed `partial: command not found`, and still
reported PASS=80 FAIL=0 — these scripts do not use `set -e`, so nothing counted the failure. The
vacuity shapes could not see it: the ASSERTIONS genuinely passed, and what was unclean was the
HARNESS. A harness that runs stray commands can also swallow the output an assertion depends on.
Six gates put backticks in diagnostic strings; five escape them, so this was the only live one.

**Shapes the gate STILL does not count**, listed so their absence is not read as their
non-existence: `sed`/`awk` block extraction over a missing block; `grep` pipelines accepting empty
input; command substitutions hiding a failed producer; section checks where a fatal earlier
diagnostic removes the section. `assert_block_absent` distinguishes a missing anchor from
forbidden content but not a malformed or unterminated block — left deliberately, because no report
shape was found that reproduces it, and an untested branch inside an assertion helper is worse
than a known gap.

**Centralise the freshness build.** `require_fresh_binary` runs `lake build` at the top of each of
145 gates. A no-op build is ~130ms, so a full suite pays roughly 19 seconds of redundant builds.
Correct but wasteful: the runner should build ONCE and hand the binary to N gates, with each gate
KEEPING its guard for standalone execution. Deleting the per-gate guard would remove the
protection exactly where it earned its place — running one gate directly while iterating is how
four stale-binary incidents happened in a single session.

### Task R-0471

**Phase:** B

**Objective:** Make new diagnostics distinguish invalid command forms, identify retired syntax and provide valid next actions or candidates, retaining class-level and blank-hint mutation controls.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Under R-0137/R-0515, distinguish invalid command arity/form from unknown spelling; cover every
known query kind in bare and argument forms and never list the rejected token as known for that
same form. Disambiguate a query name that serves incompatible fact-filter/backend meanings. Measure
the corpus of retired syntax before fixing instances; diagnostics name retirement and the recorded
replacement syntax, including the retired `#` enum separator, without changing a language rule.

Populate hints where candidates already exist: unknown names, fields and variants give in-scope
candidates and a valid next action in text and diagnostics JSON. Acceptance: class-level query and
retired-form fixtures, every diagnostic code reached by the corpus has a nonempty next action, and
a mutation blanking a hint fails. This bounded increment does not narrow R-0137's full bar.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

### Task R-0009

**Phase:** B

**Objective:** Provide crash reduction only with isolated bounded execution, preserved failure class, final-predicate validation and healthy-program, timeout and self-failure controls.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Do not expose `reduce --predicate crash` in help/CLI until it evaluates candidates in an isolated
subprocess with bounded time, memory and output and preserves the original crash boundary/class.
Parse success is never a crash predicate. Verify the final candidate still satisfies the predicate;
report stage/error-class drift and unsupported multi-module reductions explicitly.

Acceptance: controls cover a healthy program (cannot reduce as crash), signal/tool/compiler/runtime
crashes, timeout, changed failure class, empty candidate and reducer self-failure. Share backend
predicates and reduction machinery with R-0491.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0106** (merged): duplicate of R-0009/R-0491: merge new backend predicates into shared reducer/harness.

### Task R-0010

**Phase:** A

**Reconciliation (2026-10-19):** Keep; connect to R-0491/R-0513 inventories.

**Objective:** Make the bug-corpus truth gate honest Replace the current skip-based audit summary with an explicit per-bug state:


```text
fixed_with_regression
open_with_reproducer
documented_no_fixture
missing
```

Only `fixed_with_regression` contributes to “regression coverage.” An open bug
requires a checked-in minimal reproducer plus exact expected bad observation,
watchdog/resource limits where needed, owner/priority/containment, and replay
command; it remains red or explicitly quarantined and can never be summarized
as covered/fixed. Mutation-sensitive class gates replace individual reproducers
when the fix lands. The reverse inventory fails on undocumented `bug_*.con`
files and on numbered bug docs absent from the state table.

**Untracked audit corpus prerequisite.** Before the reverse inventory becomes
enforcing, classify every file under `.audit_me/`: promote a minimal reproducer
with its control, expected observation, and case manifest into
`tests/bugs/<id>/`, or delete it deliberately after recording why it is
superseded. Existing `tests/wrong-code`/fixture paths may migrate when touched,
but no second bug registry is created. The directory currently contains the
bug-050–055 audit probes, four mini-projects, and generated binaries/LLVM files;
leaving them untracked makes `git clean` a data-loss event and makes the future
reverse inventory fail for reasons no owner has adjudicated. Generated
artifacts are never promoted as source fixtures, and new `.audit_me/` files are
forbidden once this gate lands.

**Publication discipline.** A milestone is not “pushed everywhere” until the
same intended tip and bug/roadmap artifacts are present on every declared
remote and required CI conclusions are recorded. Remote parity is process
state, not a compiler bug; report it separately and do not let a green origin
implicitly describe a lagging mirror.

The publication state machine is fixed: run the required local gates; push the
primary; wait for the named remote-CI conclusions; verify the primary ref still
equals the intended tip; then fast-forward the mirror and verify both refs.
The mirror may never lead the primary, and local hook success is not a synonym
for hosted CI success. A divergence requiring anything other than a
fast-forward stops publication for an explicit reconciliation; the helper may
not hide it with a force push.

### Task R-0439

**Phase:** T

**Restructured (2026-11-02):** retired with the legacy compiler: The named legacy worktree hunks retire, while the three-level publication and milestone integrity requirements survive. surviving requirement: Retain narrow publishing-branch checks, explicit aged-branch audits and milestone gates requiring every claimed slice to be merged or retired, with discriminating controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep bounded gate; merge hook-budget implementation with R-0513.

**Objective:** Gate work that is finished but not landed — the local counterpart
of publication discipline.

The Definition of Done covers *done*. Nothing represents "evidence exists, not
merged", and that state has now cost real work twice. R-0001's root fix sat as
uncommitted working-tree state in a locked worktree for a day while three
documents on `main` described the pre-fix language. `worktree-h18-drop-glue`
still holds three commits — a bug reproducer, a `MonoVerify` residual verifier,
and classified fuzzer legs — that no merged branch contains, on a branch named
after unrelated work.

Do not merge that branch wholesale: R-0001's enum-monomorphization
implementation already landed independently. Review and promote unique hunks by
owner: `MonoVerify` type/layout invariants to R-0434;
callable/application-identity material to R-0442; classified generator cases to
R-0272/R-0095; and an independently reproduced user-Option defect to a newly
numbered bug record. Discard superseded R-0001 implementation hunks only after
that inventory is recorded. This selective review is the required disposition
for the named worktree, not permission to mix implementation into a
documentation slice.

This is not a request for task/branch status tags in this file: those are
forbidden, and for good reason. A task-internal slice table such as R-0004's may
record landed versus pending acceptance boundaries while that one migration is
active, but it cannot represent unmerged work as landed. R-0439 is a gate —
scoped in three levels, because a pre-push hook
that fails whenever *any* local branch has unmerged commits would fire on
ordinary feature work, depend on unrelated local state, and behave differently
from CI. An opt-out registry under that design becomes mandatory bookkeeping.

1. **Pre-push, narrow:** only the branch being published. Dirty tracked state,
   commits that belong to the milestone but are not in the push, and the required
   gate evidence for what is being published. This level must remain routinely
   usable: target under five minutes. If measurement shows the full CI gate set
   exceeds that budget or regularly hits the hook timeout, run
   `make test-fast-surface-gates` plus path-matched load-bearing gates locally
   and leave the exhaustive `make test-ci-gates` run mandatory in CI. Do not
   preserve a predictably bypassed hook merely because it is stricter on paper.
2. **Repository audit, explicit command:** report worktrees and branches with no
   upstream and no recorded parked status past an age threshold. This is where
   `worktree-h18-drop-glue` should surface — reported, not blocking.
3. **Milestone gate:** every slice a milestone claims is merged or explicitly
   retired. This is the level that would have caught R-0001's root fix sitting
   uncommitted while three documents described the pre-fix language.

Gate: fixtures for a dirty publishing branch (must fail at level 1), an unrelated
unmerged feature branch (must NOT fail level 1, must appear at level 2), an aged
branch with no upstream, a parked branch with its recorded status, a clean tree,
and a milestone with an unmerged owned slice; plus a mutation removing the
dirty-state leg. Cheap to run — level 1 must not need a compiler build.

### Task R-0435

**Phase:** B

**Disposition (2026-10-19):** superseded by R-0500: merge migration, renamed-pattern syntax and controls.

**Objective:** Ban lexical value shadowing while preserving explicit assignment
rebind.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0435).

### Task R-0440

**Phase:** B

**Objective:** Represent claim scope, method, validity, binding, dependencies and policy independently, retaining typed digest separation and controls against scope inflation or trust laundering.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Represent subject binding, claim kind, semantic scope, evidence method, status, assumptions,
producer, validator, trusted dependencies, freshness and replay receipt as orthogonal facts.
`SourceCorrespondence`, `LogicalValidity`, `DependencyClosure`, immutable `ReplayReceipt` and
time-relative `PolicyDecision` use shared envelopes with typed variants and authority-state
constructors, not a bag of optional fields. CLI/package reports project those objects. Friendly
names such as `proved_by_lean` cannot inflate scope. A policy preference may compare only equivalent
subjects/scopes; unlike scopes/methods are not a universal ladder. Remove residual total-ladder
claims and reconcile evidence-class/taxonomy documentation with the implemented architecture.

Use domain-separated `SubjectDigest`, `ImplementationDigest`, `ContractDigest`, `PropositionDigest`,
`TheoremArtifactDigest`, `CertificateDigest`, `DependencyRoot` and `ReceiptDigest`; swapping two
well-formed digest families must fail at compile time. Inventory raw string/tuple evidence boundaries.
Receipts bind checker, rule set, artifact and environment; current advisory disposition is computed
later from advisory registry, authentication and policy. A retained policy attestation separately
binds policy/advisory-registry identities.

As approved authorship modes land, certificate, proof-object, trusted-proof and assumption forms
select distinct variants; existing `proof_by` cannot silently acquire the strongest class. Exercise
each mode through production report, receipt, package and policy consumers. Source-to-model trust
remains separate from independent proof-term validity; compiler enforcement depends on compiler
correctness until soundness and artifact production are independently established. Proof callables
keep the direct-call/value distinction and resolved identity, never source-spelling recovery.

Acceptance: one fixture per existing scope × method cell; reject ordering incomparable claims,
missing method-required producer/validator/trust accounting, and weaker-scope rendering under a
stronger name. Revoking a checker changes logical validity/policy while preserving independently
established source binding and dependency closure. Reject any renderer collapsing those dimensions
to stored `proved`. Compile-time digest-swap and production trust-laundering controls fail.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0059** (merged): duplicate of R-0021/R-0440: merge; replace single-class taxonomy with orthogonal facts.

### Task R-0447

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The disposition explicitly merges unique controls and retires only the conflicting layout. surviving requirement: Preserve unique repository ownership and operational integrity controls under R-0481 while discarding the conflicting legacy directory blueprint. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** duplicate of R-0481: merge unique operational controls; retire conflicting directory blueprint.

**Objective:** Freeze repository ownership and layout before broad stdlib,
test, proof, and documentation growth multiplies the current ambiguity.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0447).

### Task R-0438

**Phase:** B

**Objective:** Generate construct coverage and validate structured documentation claims against live gates, retaining missing-row and stale-claim negative controls.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Share one generated coverage inventory with R-0118 and enforcement rows with R-0498. Derive its
construct set from AST, Core, resolution and project models, including modules, imports/aliases,
manifests and target/module selection. Until generation covers them, explicitly list defer,
newtypes, generics, specialization, function-pointer values and indirect calls with current status,
evidence, gap and owner; temporary rows are replaced, not duplicated.

Keep structured claim checking separate from constructor coverage. Each claim record names ID,
current/planned status, stable owning gate ID, fixture, supporting compiler fact and last-verified
commit. Consume the shared ownership manifest (R-0447/R-0481) and registered gate verdict/liveness;
resolve artifact/test/document references by stable IDs without copying ownership facts. Check gated
`docs/` claims, not English by heuristic; research remains non-normative under its recorded convention.

Correct arithmetic/bounds and proof-eligibility claims; describe semantics independence/differential
testing honestly; make KNOWN_HOLES an index over numbered defects; remove stale defer, language-gap,
identity, error-convention and anti-feature claims. Replace hand-counted theorem totals by generated
coverage, distinguish authority-free from purity and use R-0440 evidence dimensions. Stable-reference
claims cite backing gates/fixtures; planned behaviour cannot masquerade as a current guarantee.

Acceptance: new constructors without a declared story fail in each AST/Core/resolve/project inventory;
missing rows or claim records fail. Reproduce stale-claim controls for unenforced language invariants,
false empty open-hole lists, already-shipped remaining findings, defer rules falsely unspecified,
contradictory linear-rebind rules and false no-destructor claims; the claim gate rejects each.
Reject planned records stated as current and invalid/dead backing gates.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

### Stdlib Area Completion Contract

The final stdlib-area task may close only when:

- Tasks R-0001–R-0010 are fixed with committed
  load-bearing regression/class gate, the bug-corpus report distinguishes open
  reproducers from fixed coverage, and declared remotes contain the intended
  milestone artifacts before publication is claimed;
- private-by-default enum variants and raw newtype payload access complete the
  one-keyword construction-rights model without weakening the shipped struct-field gate;
- A3 supports the workload-pulled linear combinators with exactly-once
  conservation and updated proof evidence;
- `fs::read_to_string` rejects invalid UTF-8 without losing the Bytes path;
- every current String-to-C consumer rejects interior NUL through one checked
  adapter and the unchecked primitive is explicitly named/private;
- the public-safety rule is documented and mechanically gated, `std.net` no
  longer makes ordinary callers request `Unsafe`, and every remaining legacy
  exception is a ratcheted manifest row rather than an invisible exemption;
- Vec/Bytes expose provenance-preserving Slice/MutSlice constructors,
  `Vec::get_mut` is scoped or explicitly `_unchecked`, and the compiled
  `std.slice` exemption is gone;
- the API snapshot, compiled behavioral inventory, docs/examples, authority/
  allocation/failure manifest, and evidence report agree;
- borrowed Bytes views and owning raw-parts handoff are different typed
  contracts with mutation-tested destruction/provenance behavior; and
- the documented `io::read_all` partial-parser contract remains pinned rather
  than being silently changed under the cleanup.

S2 and S4–S7 are trigger records, not the stdlib boundary-corrections area closure criteria. Move their work
into the earliest honest global task position only with the named workload,
failing gate, or already-ratified contract violation. Do not wait for broad File/process/time/
random/crypto redesign or JSON/DNS/compression/threading breadth to close this
correction slice.

### Task R-0058

**Phase:** B

**Reconciliation (2026-10-19):** Keep checked onboarding material.

**Objective:** Define the canonical **consume / destroy / handoff** conventions for linear code. The guide must distinguish explicit cleanup (`destroy(x)` or the type's consuming `.drop()`/Destroy verb), ownership transfer by by-value call, ownership transfer by return, destructuring into owned fields, `defer` as explicit scheduled cleanup, and the forbidden cases (bare non-`Copy` statement, `_` over non-`Copy`, `let _`, non-`Copy` sub-place projection by value). This is documentation plus examples and diagnostics, not automatic `Drop`: no hidden cleanup and no hidden control flow. Include a short **Copy and Linear Values** guide: when to mark a type

 `Copy`, why `Option<i32>` is `Copy` but `Option<String>` is linear, how
 value-mode reads move non-`Copy` bindings by default, why `_` ignores only
 `Copy`, how params are owned locals unless they are borrows, and the
 ordinary consume paths (`destroy`, handoff, return, destructure, `defer`).
 This guide is the user-facing explanation of the conditional-`Copy` and
 mode-based-checker refactors; it should be validated by small runnable
 examples and linked from README/site docs.

## Area: Usable QBE Backend And Independent Validation

**Reconciled 2026-10-19 (R-0113):** the Lean Compiler Backend (R-0517) is Concrete's QBE path; Concrete builds
no QBE backend. The goal, contract and tasks below are retained as historical rationale and as the source of
controls transferred to R-0517, R-0505 and the named owners; each task's **Phase** line and the
[reconciliation record](ROADMAP_RECONCILIATION.md) give its current disposition.

Goal: add QBE as a real, user-selectable native backend for ordinary Concrete
programs and use its independent implementation to find shared lowering,
interpreter, ABI/layout, and LLVM-emission bugs before the flagships area flagship
workloads deepen the public claims. Differential validation is a required duty
of the backend, not its sole purpose. QBE need not replace LLVM or prove that
native execution preserves source semantics to be useful and genuinely usable.

Done when: a QBE backend consumes verified/cleaned Concrete SSA directly,
supports a documented useful subset with no silent LLVM fallback, and works
through the normal `build`, `run`, `test`, project, artifact-retention, and
diagnostic workflows—not only a special differential-test harness. At least one
The flagships area-class program must be buildable and runnable by a user with
`--backend qbe`. CI must also classify interpreter/LLVM/QBE disagreements over
an edge-heavy corpus by stdout, stderr, exit status, and failure class. The
audit/evidence surface labels QBE results `tested` or `backend_trusted`; this
phase cannot upgrade a source/Core proof to native-code evidence. Release-grade
target breadth, translation certificates, full debug information, optimization
policy, and long-running platform qualification remain owned by the backend-contracts area.

### Product Contract: A Backend, Not A Test Adapter

- `--backend qbe` is accepted anywhere `--backend llvm` is accepted: single
  file compilation, project builds, `run`, `test`, examples, retained
  artifacts, debug bundles, and reproducible replay commands.
- A successful QBE build produces a normal native executable with an explicit
  target/toolchain/link manifest. Users do not need to invoke the differential
  harness or understand QBE IL to run it.
- Supported programs are defined by a generated capability matrix. Unsupported
  operations, ABI shapes, targets, or builtins receive a stable diagnostic
  before partial code generation; they never fall back to LLVM silently.
- QBE owns real backend concerns: instruction selection, QBE IL legality,
  runtime-helper selection, object/link inputs, entry and test wrappers,
  platform ABI realization, artifact naming, diagnostics, and toolchain facts.
- The backend ships initially as `experimental`, which describes compatibility
  and release guarantees—not a toy implementation. Its documented supported
  subset must work end to end and remain protected by ordinary user-workflow
  tests in addition to differential tests.

### Preflight Refactors: Make The Backend Boundary Real

These are enabling refactors, not a new optimizer or a premature second middle
end. Zig's useful lesson is the stable analyzed input shared by every backend;
Concrete already has that candidate in verified/cleaned SSA. Make the boundary
explicit before duplicating LLVM-era assumptions in QBE.

Opening dependency order: provision and pin the QBE toolchain on eligible CI
hosts; extract backend-neutral `LayoutFacts`; define the typed QBE IL/fragment
model; introduce only the `CodegenInput` and driver surface needed to run the
first vertical slice; then expand QBE coverage. The numbered preflight inventory
below records all obligations, not permission to complete every abstraction
before `main -> 42`. Layout panic-to-diagnostic conversion follows the layout
split. Existing LLVM emission moves to fragments only when a shared consumer or
profiled bug 027 justifies it.

The correspondence is architectural, not literal:

| Zig role | Concrete role in the QBE backend-validation area |
| --- | --- |
| AST / ZIR | parsed syntax / checked Core |
| AIR | verified and cleaned Concrete SSA |
| isolated codegen-job input | immutable `CodegenInput` + `CodegenFacts` |
| LLVM backend | structured LLVM emitter + existing toolchain |
| self-hosted native backend | QBE emitter + pinned QBE toolchain |
| target MIR / encoder | deliberately delegated to QBE; not built in the QBE backend-validation area |
| linker job | backend-neutral Concrete driver invoking the declared linker |

Hard boundary: do not rename Concrete IRs to imitate Zig, introduce ZIR/AIR as
extra layers, or build target MIR/register allocation/object writers merely for
architectural symmetry. Copy the separation of responsibilities, immutable
job shape, coverage discipline, and gradual backend enablement. QBE provides a
useful native compilation path while letting Concrete defer target MIR,
register allocation, and object writing until owning those layers is justified.

Execution discipline: build this phase as thin executable vertical slices, not
as a backend-framework rewrite followed by a distant first program. Every slice
must end with an eligible fixture running through interpreter, LLVM, and QBE;
the same commit adds its capability row, differential observation, negative or
mutation case, and retained failure artifacts. A refactor-only slice is allowed
only when an existing LLVM behavior/IR parity gate consumes it immediately and
the next QBE slice names it as a prerequisite. Do not land unused interfaces,
generic plugin machinery, speculative target abstractions, or a second
representation merely because a future backend might need them.

First executable milestone (before broad preflight completion): compile
`fn main() -> Int { return 42; }` through cleaned SSA into typed QBE IL, validate
it, invoke the pinned QBE toolchain, assemble/link it, observe clean stdout and
exit status 42, and compare that structured observation with interpreter and
LLVM. `--emit-qbe`, `--backend qbe`, unavailable-tool diagnostics, no-fallback
behavior, and a debug bundle containing source/Core/SSA/QBE IL/tool facts must
all work for this one program. Then expand in this order unless a failing
backend or differential case forces a different dependency: branches/phi -> direct calls -> integer widths
and casts -> checked arithmetic/traps -> stack memory -> f32/f64 arithmetic,
comparisons, and checked conversions -> globals/strings -> aggregates/enums ->
indirect/extern calls -> builtins -> test mode. Each step must stay runnable;
no "all operations implemented, testing later" batch.

LLVM-refactor rule: moving existing code behind `CodegenInput`, layout facts,
the runtime manifest, or the backend driver must initially produce
byte-identical LLVM IR for the gated corpus. If byte identity is impossible or
undesirable, the slice must state the exact textual change, prove equal native
observations in debug/release modes, run LLVM validation, and add a regression
for the changed invariant. QBE progress cannot be used to waive LLVM evidence.

### Task R-0073

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The cleaned-SSA wrapper is obsolete, but payload binding and elimination of conflicting emitter facts remain required. surviving requirement: Bind backend input to validated immutable IR and canonical typed program facts, retaining identity and order-independence regression controls. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0497/R-0503: merge into payload-bound validated BIR artifacts.

**Objective:** Introduce an immutable `CodegenInput` wrapper around cleaned `ValidatedSSA`, plus `CodegenFacts` derived once per program: target-neutral type/layout facts, function signatures, symbol identities, module-local aliases, entry/test metadata, string/global inventory, builtin identities, runtime-helper requirements, and source-span mappings. This is not a new IR and must not rewrite instructions. Both LLVM and QBE consume the identical value; a hash of its canonical form appears in backend artifacts and differential bundles. Eliminate emitter-owned rescans that can disagree by program/module order (the bug-039 class).

### Task R-0074

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The Pipeline.emit replacement retires with old SSA, while the structured artifact boundary survives. surviving requirement: Return structured native-code artifacts through a driver with explicit profile and toolchain identity. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded — merge into validated BIR (R-0503) and R-0517; the closed `llvm`/`qbe` backend set becomes the LLVM and LCB-native profiles.

**Objective:** Replace `Pipeline.emit : SSAProgram -> String` with a statically dispatched backend driver returning a structured `CodegenArtifact`:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0074).

### Task R-0075

**Phase:** B

**Objective:** Use a shared native compilation driver with structured diagnostics, retained artifacts, bounded tools and failure-injection controls at every boundary.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Provide one backend-neutral driver for file/build/test compilation:
`prepare → emit → verify → assemble/codegen → link`. Backend profiles supply extensions,
verifier/tool invocations, target flags and link inputs; shared CLI policy owns structured
diagnostics, artifact retention and process handling. LCB-native is the forcing consumer alongside
LLVM. Native test-mode entry generation, explicit profile names and evidence attribution use it.
Bound external tools in time/memory/output and preserve their failure classes. Acceptance: inject
failures at every boundary for both profiles; neither profile takes a less structured failure path,
and retained artifacts and diagnostics identify the actual selected profile.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0091** (merged): merged — native test-mode entry generation joins R-0075's driver and the LCB-native profile (R-0517).
- **R-0107** (merged): duplicate of R-0075: merge bounded external-tool execution contract. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0107)
- **R-0110** (merged): merged — profile naming and evidence attribution join R-0075/R-0333; the profiles are `llvm` and the LCB-native `verified` profile. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0110)

### Task R-0076

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: Moving functions out of old Check/Layout retires, while canonical size, alignment and ABI facts remain necessary. surviving requirement: Establish one checked layout authority, separate target rendering, and preserve layout and backend-consistency regression controls. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0502: merge into single proved Layout authority.

**Objective:** Split target-neutral layout computation from LLVM rendering. Rename or move `tyToLLVM`, LLVM type-definition construction, target triple/data-layout text, and LLVM ABI spellings out of `Concrete/Check/Layout.lean`; retain one checked `LayoutFacts` source for size, alignment, field offsets, enum tag/payload placement, pass-by-pointer decisions, and canonical builtin aggregates. LLVM and QBE render those facts independently. No emitter may recompute layout from syntax or host assumptions.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0076).

### Task R-0077

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: Extraction from EmitSSA retires, but explicit runtime contracts and support classification survive. surviving requirement: Record runtime symbol, signature, ownership, authority, failure, helper and profile-support facts with fail-closed builtin coverage. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0503/R-0509: merge with `rt` contracts, support inventory and trust accounting.

**Objective:** Extract a versioned `RuntimeABI`/`BuiltinCodegenManifest` from `EmitSSA.lean`: symbol names, signatures, ownership/capability class, failure/trap behavior, required helper, and backend support status. Keep builtin semantic behavior detection in the existing interpreter/compiled gate; this manifest owns codegen availability and linkage, not a second signature or semantic truth source. A new builtin must fail closed until both backend rows are classified.

### Task R-0079

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The QBE coverage preflight retires, but responsibility for semantic legalization survives in the new pass chain. surviving requirement: Assign operation legalization responsibilities to named passes with explicit typed obligations. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0493/R-0501/03: merge into typed operations and pass obligations.

**Objective:** Define a legalization responsibility table before QBE instruction coverage:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0079).

### Task R-0080

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The SInst/STerm matrix is obsolete, while generated honest support coverage remains required. surviving requirement: Generate profile support coverage from new IR, ABI, builtin, target and mode inventories, failing on unclassified operations. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** duplicate of R-0118: merge backend/profile dimensions into one inventory.

**Objective:** Add a generated backend capability matrix keyed by the constructors of `SInst`, `STerm`, `Ty`, ABI shape, builtin, target, and mode (normal/test/debug/release). Coverage derives from compiler constructors, not a hand-maintained prose list, so adding an IR operation makes the gate fail until LLVM/QBE support is declared and tested. Expose the matrix in `concrete agent features --json`, debug bundles, and the QBE backend-validation area capstone.

### Task R-0082

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old SSA chain retires, while the explicit single-frontend and single-spine requirement remains. surviving requirement: Preserve one specification elaborator and one proved lowering spine shared by native profiles. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR.

**Objective:** Preserve one frontend and one lowering spine:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0082).

### Task R-0085

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old Layout.lean split retires, but the no-duplicate-layout invariant and gate survive. surviving requirement: Ensure all native profiles consume one layout authority while realizing its decisions independently. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** duplicate of R-0076: merge through R-0076 into R-0502.

**Objective:** Keep layout facts backend-neutral. Split LLVM spelling out of `Concrete/Check/Layout.lean` where necessary, but do not create a second size/alignment calculator. Add a gate proving LLVM and QBE consume the same Concrete layout decisions while independently realizing them.

### Staged Implementation

### Task R-0090

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: Porting old compiler-emitted builtins retires, while runtime behavior and support obligations survive. surviving requirement: Provide explicit runtime contracts and fail-closed builtin support classification without prescribing a shared C implementation. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0503: retire preferred-C-runtime prescription; implement Concrete `rt`, named libc FFI.

**Objective:** Port the compiler-emitted builtin/runtime surface needed by the validation corpus. Prefer a small shared C runtime for allocation, IO, traps, and other platform services when that keeps semantics explicit; keep pure scalar builtins in the emitter when independence is more valuable. Extend the builtin-semantics inventory so every builtin is one of `qbe_equal`, `qbe_runtime_helper`, or `qbe_pending`; adding a builtin without an explicit class fails CI.

### Task R-0092

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old source interpreter and QBE harness retire, while differential validation survives from slice 1. surviving requirement: Compare structured observations from the Lean reference evaluator and supported native profiles using reusable differential fixtures. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded — the three-way differential is Core semantics / LLVM profile / LCB-native under R-0491.

**Objective:** Add `scripts/tests/check_qbe_backend.sh` and a reusable differential driver that runs each fixture through the source interpreter, LLVM, and QBE and compares a structured observation:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0092).

### Task R-0093

**Phase:** B

**Disposition (2026-10-19):** duplicate of R-0092: merge; labels are hypotheses, not proof of faulty component.

**Objective:** Classify every mismatch rather than printing a generic wrong-code failure:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0093).

### Task R-0094

**Phase:** B

**Disposition (2026-10-19):** superseded by R-0491: merge; retain hand/reference/metamorphic controls.

**Objective:** Require an independent expected result for cases capable of exposing a shared lowering error: a hand-authored expectation, checked-in reference model, system-tool/Python oracle, or metamorphic property. Two backends agreeing is corroboration, not correctness, because both consume the same lowered SSA.

### Task R-0095

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old SInst and QBE inventory retires, but retained corpora and fault-detection obligations survive. surviving requirement: Maintain semantic-family validation manifests and mutation controls that detect faults in supported lowering, emission, evaluator, layout and builtin boundaries. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0491/R-0118: merge manifests and minimum completed comparisons.

**Objective:** Build the validation corpus by semantic family, not file count:
every `SInst`, terminator, scalar width, signedness-sensitive operation,
aggregate/layout class, direct/indirect call shape, generic specialization
shape, builtin class, distinct trap identity, and main/test wrapper path needs a
manifest row. Seed it with the existing arithmetic, cast,
enum-union-layout, nested-place, callable-value, fuzz, workload, wrong-code, and
exit-model corpora. Add mutation checks proving the differential suite notices
at least one injected fault in lowering, LLVM emission, QBE emission,
interpreter semantics, layout, and builtin behavior.

### Task R-0096

**Phase:** T

**Restructured (2026-11-02):** retired with the legacy compiler: The mandatory QBE campaign changes, but CI replay and qualification integrity are enduring process duties. surviving requirement: Preserve deterministic presubmit and scheduled differential campaigns, faithful local workflow replay, explicit tool availability and retained failure artifacts. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0513/R-0491: merge; remove mandatory QBE availability.

**Objective:** Run a deterministic quick differential matrix on every CI change
and a larger rotating fuzz/differential matrix on schedule. Before QBE is
available, the presubmit smoke compares interpreter and LLVM over a fixed,
replayable seed set; once QBE lands it compares all three. Job-level workflow
conditions must be honored by local workflow replay so a local “all gates” run
does not silently execute or omit a different campaign. QBE-unavailable CI is a
hard configuration failure for the QBE job, not a skip. Keep LLVM-only release
jobs intact until the backend-contracts area graduates QBE; report QBE flakes separately and
retain their artifacts.

### Phase Boundary And Graduation

### Task R-0105

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old three-way QBE matrix retires, but optimization-sensitive validation is needed on the first native path. surviving requirement: Compare supported debug and release native configurations with the reference semantics, retaining optimization-sensitive and sanitizer controls. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0491/R-0504: merge debug/release and sanitizer controls.

**Objective:** Add optimization-sensitive triangulation. For every eligible fixture, compare interpreter, LLVM at the supported debug/release optimization levels, and QBE followed by the supported assembler/linker configurations.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0105).

### Task R-0106

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old QBE predicates change, while reducer correctness and replayability requirements survive. surviving requirement: Preserve mismatch-aware reduction, target/tool identities and replay commands, with controls rejecting reductions that merely stop compiling. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** duplicate of R-0009/R-0491: merge new backend predicates into shared reducer/harness.

**Objective:** Extend fuzzing and reduction for the new path. Add backend-aware predicates for QBE compile crash/rejection, QBE runtime mismatch, three-way semantic split, LLVM-only mismatch, shared-compiled mismatch, optimization mismatch, and nondeterministic QBE emission. Reducers must preserve the mismatch classification and relevant target/tool versions, save the smallest reproducer plus replay command, and prove by mutation tests that a reduced case is not accepted merely because one backend stopped compiling.

### Task R-0107

**Phase:** B

**Disposition (2026-10-19):** duplicate of R-0075: merge bounded external-tool execution contract.

**Objective:** Separate compiler defects from toolchain defects operationally. The bundle and CI report must identify the first failing boundary:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0107).

## Area: Flagship Depth And Examples

Goal: produce examples that outside systems engineers find impressive, not only
internally coherent.

Done when: the showcase set is capped at **three deep flagships** — one
cryptographic correctness example, one systems-security verifier, and one
temporal/stateful protocol example — each with proof/evidence strong enough to
anchor the public pitch.

Maintain only those flagships, deepen theorem coverage where it strengthens
public claims, and add examples only when they force a named surface or claim.
Everything else moves to the owning fixture, adversarial, benchmark, workload or proof-pattern
corpus rather than becoming a user-facing example.

This phase is also the external-credibility probe for the compiler/evidence
pipeline: old phase 6B's `diff-caps` artifact gives the first narrow reviewable
delta, and this phase turns that style of replayable evidence into a non-author
example or workload transcript before the larger package/release machinery
depends on it.

### Incremental Architecture Track

Execute the following migrations after the QBE boundary has supplied concrete
consumers. Each is an independently green vertical slice, never a compiler-wide
rewrite; production entry points remain runnable and LLVM output is byte-stable
unless the task records and gates an intentional delta.

These tasks execute the compiler-code half of R-0447's repository contract.
Move a module only when its semantic owner and typed boundary are established;
in the same slice, update the generated import graph and remove the corresponding
baseline exception. The target ownership is frontend/resolve/check/typed Core/
mono/SSA/backend, with shared Semantics owning operation meaning, Proof/Evidence
consuming typed artifacts, Report rendering rather than deriving facts, and
Driver orchestrating rather than becoming a second pipeline. Do not precede
these migrations with a path-only `Concrete/` reshuffle.

### Task R-0114

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: Retrofitting old names retires, but identity correctness and the named forcing regressions survive. surviving requirement: Use typed semantic identities from definition boundaries and retain alias, binder, indirect-call, generic-layout and monomorphization collision regressions. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0492: merge; distinguish compilation-local IDs from persistent evidence identity.

**Objective:** Introduce stable semantic identities at definition boundaries.
Give modules, definitions, fields, variants, binders, monomorphized instances,
blocks, values, runtime checks, and proof subjects stable typed identities where
text names or list position still act as identity. Treat bugs 039–045 and
050/051/054/055 as the forcing family: alias capture, first-match binder/type
tables, indirect-call hijacking, generic-enum layout identity, non-injective
monomorphized names, and renamed-import callee identity. Alpha-renaming remains
the completed local fix for bug 045; this task eliminates the recurring
architectural precondition without relabeling narrow fixes as temporary.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0114).

### Task R-0115

**Phase:** B

**Objective:** Derive immutable indexed canonical program facts with typed keys, explicit invalidation and duplicate, missing-key and conflicting-consumer controls.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Derive one immutable indexed `ProgramFacts` store containing module/definition indexes,
signatures, layouts, builtin identities, aliases, target/runtime facts, call relationships and
source/semantic identity, once per program. Share indexes with R-0140. Passes query typed keys,
without rescanning lists or making private first-match tables. Facts stay immutable within a
compilation; transformations produce a new version with an explicit invalidation boundary.

Acceptance: fail on duplicate/missing keys and conflicting values from two consumers; record
deterministic hashes and construction cost and demonstrate linear-time builders. Compare canonical
facts with independently justified corpus expectations in the new compiler.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

### Task R-0116

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The legacy stage-wrapper list retires, while type-safe pass boundaries survive in the new IR chain. surviving requirement: Require payload-bound typed pass results whose constructors establish the invariants needed by subsequent consumers. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0497/R-0510/R-0514: merge; use payload binding and new IR chain.

**Objective:** Make successful compiler pass boundaries type-safe Introduce thin opaque results such as `ParsedProgram`, `ResolvedProgram`, `CheckedProgram`, `ElaboratedCore`, `CanonicalCore`, `MonomorphizedCore`, `LoweredSSA`, `ValidatedSSA`, `CleanedSSA`, and `CodegenInput`. Constructors live with the pass that earns each invariant; reports, backends, tests, and projects cannot call a later consumer with an earlier-stage value.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0116).

### Task R-0118

**Phase:** B

**Objective:** Generate exhaustive new-IR pass and consumer coverage with implementation/test links and missing-row or synthetic-constructor negative controls.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Generate a single exhaustive matrix from new compiler constructors and interface facts across
source checking, Core checking, interpreter, lowering, validation, cleanup, LLVM and LCB-native
profiles, reports, source identity, obligations/proofs and tests. Include backend/profile dimensions
and minimum completed differential comparisons; share the inventory with R-0438 and later slices.
Every constructor is explicitly classified as implemented, intentionally no-op, rejected-before-here,
runtime-helper, pending or not-applicable; catch-all matches earn no coverage.

Acceptance: a synthetic constructor or deleted consumer row makes the gate fail; each row links its
implementation and discriminating test. Publish the matrix through debug/agent artifacts. Delete
manual inventories or reduce them to generated snapshots.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0080** (merged): duplicate of R-0118: merge backend/profile dimensions into one inventory.
- **R-0095** (superseded): superseded by R-0491/R-0118: merge manifests and minimum completed comparisons.

## Area: Incremental Compiler Driver And Artifact Reuse

Goal: make unchanged compiler, proof, report, and backend work reusable without
creating a second semantic pipeline or letting stale evidence remain green.

Trigger: start implementation only after the flagships area external-validation gate
returns **GO**, old phase 6B has stable identities/dependency/invalidation contracts
and structured validation records, old phase 6C's shadow edit corpus is green, and
at least one medium the flagships area workload is large enough to choose granularity from
measurements rather than toy programs.

Its position before later external-user phases is a dependency statement, not a
reason to build caching ahead of demand. Without the measured workload and GO
record above, the pull-gated doctrine requires an explicit defer/reject decision
rather than speculative implementation.

**Conditional phase rule.** the incremental-driver area retains its measured-workload and external-validation
GO prerequisites. A NO verdict records rejection or deferral and its effect on dependent
editor/package releases. Independent proof work may proceed under the current queue;
no dependent editor or package checkpoint bypasses the required architecture.

Prerequisites: old phase 6B determinism, one composed pipeline, stable fact IDs,
dependency graph, pass locality/invalidation, and validation-record contract;
Old phase 6C telemetry/replay/shadow manifests; the flagships area workload edit traces. The
general engine must land before the proof-authoring area's proof cache so proof reuse does not
become a second private database.

Non-goals for V1: no per-token/per-expression query graph, no Datalog or
user-facing rule language, no public long-term compatibility promise for the
opaque internal cache encoding, no remote cache, and no evidence upgrade from a
cache hit or hash match. Start serial and conservatively module/function-grained;
refine granularity and add parallelism only after measurements and equivalence
gates justify them.

Done when: clean, incremental-off, incremental-on, and verify/dual-run modes
produce the same facts, diagnostics, obligations, reports, Core/SSA/LLVM
behavior, and native result; the edit matrix proves conservative bounded
invalidation; no-op and leaf edits reuse semantic and object artifacts; cache
corruption/schema/tool drift recompute or fail closed; and no cached partial or
stale artifact can fabricate a validated boundary or stronger evidence class.

Timing of query-shaping (a deliberate decision, not an omission): the
query/`CompilerSession` form below is built HERE, as part of this
validation-gated driver — it is NOT pulled forward as a pre-GO "query monad
seam." Because this whole engine is gated on the external-validation GO,
threading a query monad through the pipeline earlier would be speculative
insurance against a build that may never happen, plus churn against the
still-moving old phase 6B fact graph. The bounded call-site conversion from the
batch/eager pipeline into queries is accepted as part of this build, post-GO.
Do not re-open a "do the seam early" debate: the seam's only payoff is avoiding
that conversion, and that payoff exists only if the bet validates.

## Area: Proof Authoring And Automation

Goal: make flagship proofs a repeatable engineering workflow, not a collection
of one-off `simp` scripts.

Done when: new flagship proofs can start from useful generated stubs, standard
lemmas, and actionable failure diagnostics.

This phase owns the proof-cost probe from the roadmap spine: before assuming the
proof discipline scales, measure manual proof effort on real flagships, then
measure review effort for the LLM-guided synthesis loop if manual authoring is
too expensive.

## Area: Audit Commands And Review Artifacts

Goal: let a reviewer answer "what can this program do, what is proved, what is
assumed, and what changed?" without reading compiler internals.

Done when: `concrete audit`, semantic diff, and an artifact viewer cover the
three graduated flagships and one package-scale workload.

### Task R-0191

**Phase:** B

**Reconciliation (2026-10-19):** Keep; minimum persistence joins R-0491 immediately.

**Objective:** Add counterexample-to-regression persistence for obligation witnesses from SMT, property tests, oracle failures, fuzzers, differential mismatches, runtime traps, proof failures, and future fuzzed contracts. Command surface: `concrete counterexample save <obligation_id> --out tests/counterexamples/<name>.con` plus JSON mode. The saved fixture must include source inputs, expected failing obligation id, expected status (`counterexample`, `tested_by_property_failure`, `oracle_failure`, etc.), replay command, and the original tool provenance. Wire `scripts/tests/check_counterexample_regressions.sh` with one SMT overflow

 witness, one property-test contract witness, one oracle mismatch, one
 fuzzer/differential mismatch, one runtime-trap witness, and one proof
 failure/minimized obligation witness. The
 gate must fail if a future refactor turns the same counterexample into
 `proved_*` without changing the checked fixture expectation.

## Area: Proof Status And Trust Gates

Goal: make every green proof/evidence status precise, traceable, and hard to
misread.

Done when: all existing production proof specs are directly and transitively
FnTable-complete, proof dependencies and provenance are visible, assumptions
and trust boundaries have lifecycle reports, and weaker evidence cannot appear
under a stronger badge.

### Task R-0203

**Phase:** B

**Reconciliation (2026-10-19):** Keep; use canonical dependencies and R-0494.

**Objective:** Add proof dependency tracking: if proof/spec for `f` depends on `g`, drift in `g` must affect `f`'s proof/evidence status or surface an explicit dependency warning.

### Task R-0205

**Phase:** B

**Disposition (2026-10-19):** superseded by R-0494: merge into R-0494’s evidence-class controls.

**Objective:** Keep oracle-tested evidence separate from Lean/spec refinement. Oracles are implementation sanity and regression evidence, not proof completion.

### Task R-0207

**Phase:** B

**Reconciliation (2026-10-19):** Keep; canonical task owns shared fields.

**Objective:** Add evidence provenance to proof/evidence facts: source file/span, compiler commit, theorem name, spec name, policy file, assumption file, tool version, and replay command where available.

### Task R-0210

**Phase:** B

**Objective:** Add assumption lifecycle checks: every assumption has an owner, scope, rationale, review date, affected claims, and a diff gate when it widens.

### Task R-0211

**Phase:** A

**Disposition (2026-10-19):** superseded by R-0509/S16: merge into R-0509; retain audit command acceptance.

**Objective:** Add a trust-boundary inventory report: all `trusted`, `Unsafe`, extern, backend, runtime, and target assumptions in one machine-readable list.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0211).

### Task R-0217

**Phase:** B

**Reconciliation (2026-10-19):** Merge axiom policy into R-0506; keep clean-replay increment.

**Objective:** Add an axiom-inventory and clean-checkout proof replay gate. Every `proved_by_lean` / `proved_by_kernel_decision` fact must record the Lean axioms its theorem transitively depends on (the `#print axioms` walk):

 `propext`, `Classical.choice`, and `Quot.sound` are the expected baseline;
 any `sorryAx`, `ofReduceBool`/`native_decide`, or user-declared axiom must
 downgrade the evidence class or surface as an explicit trusted-boundary
 fact in the §10 inventory — never sit silently under a kernel-evidence
 badge. Pair it with clean-checkout replay: the proof corpus must re-green
 from a fresh checkout in a clean temp directory, proving no green badge
 depends on stale `.lake` build artifacts, uncommitted files, or local
 machine state. Wire `scripts/tests/check_axiom_inventory.sh` (red-team
 case: a spec theorem patched to use `sorry` or a new `axiom` must lose its
 kernel-evidence class) and
 `scripts/tests/check_clean_checkout_replay.sh` (the flagship corpus —
 `hmac_sha256`, `constant_time_tag` — must replay green from a pristine
 copy, and a deliberately corrupted source-vs-artifact mismatch must fail
 loudly, not reuse the stale artifact). Seed the incremental-driver area's local cache with a
 stale proof result, wrong dependency root, truncated entry, and valid entry
 from another compiler/toolchain; clean-checkout replay must reject/recompute
 all four before any evidence becomes green.

## Area: Provable And Predictable Subsets

Goal: give users a named small subset they can rely on for serious
proof/evidence work.

Done when: the subset family has public names, allowed constructs, rejected
constructs, arithmetic-site policy, runtime-error policy, and compatibility
promises.

### Task R-0221

**Phase:** B

**Disposition (2026-10-19):** superseded by R-0493/S6: merge semantic ownership into R-0493.

**Objective:** Freeze arithmetic-site semantics for subset claims. This task is reconciled with historical phase 6 item 10 and `docs/archive/design-records/ARITHMETIC_POLICY.md`: build/profile names are policy bundles, not arithmetic modes, and the same source expression must not mean wrap in one profile and checked in another. Ordinary `+ - *` are checked in every profile; intentional modular arithmetic is written as `wrapping_*`; intentional clamping is written as `saturating_*`. Subset reports must classify arithmetic sites as `checked`, `proved`, `runtime- checked`, `explicit-wrapping`, or `explicit-saturating`; they must never infer an ambient arithmetic mode from `debug`, `release`, `predictable`, or `proof`.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0221).

### Task R-0222

**Phase:** B

**Objective:** Arithmetic-site facts must distinguish checked, wrapping and saturating semantics across new-compiler proofs and outputs, with profile-invariance and confusion-rejection controls.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Carry explicit arithmetic-site facts into diagnostics, reports, assumptions, proof obligations,
release bundles and backend contracts. Every checked/wrapping/saturating operation is classified;
modular operations need explicit modular-model proofs, and not-yet-modelled forms stay outside the
default proof subset. A checked theorem cannot certify wrapping arithmetic.

Deliver arithmetic-site evidence documentation, checked/wrapping/saturating/profile-invariant
example cases and `scripts/tests/check_arithmetic_site_evidence.sh`. Acceptance: the same expression
has identical semantics across build profiles, every site is classified, and claims confusing checked
and wrapping semantics are rejected or downgraded. Replace the old width/operator snapshot with
actual new-compiler model coverage.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

### Task R-0223

**Phase:** A

**Disposition (2026-10-19):** superseded by R-0496/S7: merge into R-0496’s semantic contract.

**Objective:** Define a first runtime failure model: abort, assertion failure,
OOM, stack overflow, checked arithmetic/bounds traps, `defer`/cleanup,
impossible branches, and what each does to proof/resource claims. Abort is a
profile-dependent terminal operation: hosted execution terminates the process;
freestanding execution invokes a declared non-returning handler. Neither form
promises unwinding, `defer`, destruction, or recovery.

### Task R-0227

**Phase:** A

**Disposition (2026-10-19):** superseded by R-0499/R-0507: merge into checker rules and soundness.

**Objective:** Define source-level memory-safety claims precisely: what linearity, borrows, cleanup, trusted code, raw pointers, and FFI do and do not guarantee.

### Task R-0230

**Phase:** A

**Disposition (2026-10-19):** superseded by R-0509/S16: merge into R-0509 and slice profiles.

**Objective:** Define the v1 threat model: adversary, trusted base, proof scope, backend scope, side-channel scope, dependency scope, and what remains out of model.

### Task R-0231

**Phase:** B

**Disposition (2026-10-19):** superseded by R-0511/R-0512: merge into each slice’s admission controls.

**Objective:** Add negative examples for every `ProvableV1` and `PredictableV1` exclusion.

## Area: Runtime Safety Obligations

Goal: generate SPARK-like obligations for boring runtime failures instead of
relying only on examples and prose.

Done when: parser/security examples can show obligations for bounds, div/mod
zero, overflow profile, casts, and loop bounds with statuses
`proved`, `enforced`, `assumed`, `missing`, or `blocked`.

This phase is the **absence of runtime errors** story, stated honestly. A bounds
check, overflow trap, cast check, or loop-bound assertion may be enforced at
runtime, proved statically, assumed by policy, or still missing; those are
different evidence classes and reports must not collapse them. The SPARK/Ada
lesson to keep is the framing: runtime safety is not just "the program probably
doesn't crash"; it is a set of named obligations with source spans, replay
commands, and visible proof/enforcement status.

### Task R-0242

**Phase:** A

**Reconciliation (2026-10-19):** Keep; align terminal semantics with R-0496.

**Objective:** Define the user-level error model: `Result`, `Option`, assertion failure, abort/panic, recoverable errors, test failures, and how error flow interacts with capabilities, proofs, runtime obligations, and audit output.

### Task R-0464

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The shared semantics survives while old SSA-first obligation generation and H24 machinery retire. surviving requirement: Define one normative trap semantics shared by new-Core execution, compilation and runtime-safety reasoning. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0493/R-0495/R-0460: merge; archive completed H24 fix and retire SSA-first prescription.

**Objective:** ONE trap-semantics definition, consumed by SSA construction,
interpretation, optimization, obligation generation and the backends — and, second,
generate obligations from SSA rather than the surface AST.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0464).

### Task R-0468

**Phase:** T

**Reconciliation (2026-10-19):** Merge into R-0513.

**Objective:** Make the nightly-pinned gates reachable in this repository, and fix the
hole-closure conditions that currently depend on them.

The scheduled jobs are gated on `github.repository == 'lambdaclass/concrete'`. This
repository is `unbalancedparentheses/concrete2`, so **the scheduled path can never fire
here** — `proof-replay`, the fuzz campaigns and `check_gate_mutation_coverage.sh` are
reachable only by manual `workflow_dispatch`. The pin is correct in intent (a fork should
not run the canonical nightly) and wrong in effect (the fork where the work happens gets
no nightly at all).

Two consequences, the second worse than the first:

- **H22's closure condition is unsatisfiable as written.** It says the entry can be
  deleted "once the nightly confirms the `checked-arith-trap` family reports KILLED". No
  nightly will run. Either dispatch `check_gate_mutation_coverage.sh` manually and close
  it, or restate the condition against something that can actually happen here. Corrected
  in `KNOWN_HOLES.md` to say "manual dispatch" pending this task.
- **Nobody knows what else the nightly would have caught.** `check_gate_mutation_coverage.sh`
  is the gate that finds decorative gates — it is how H22 was found in the first place, on
  a run someone did by hand. It has run in this repository approximately never. Assume
  more H22s until it has run clean once.

Minimum fix: make the repository pin a variable, or add this fork to it. Better fix: run
the mutation-coverage gate on merges to `main` here, since it is the gate whose absence is
self-concealing — a decorative gate stays green, so nothing else will report its absence.

## Area: Compiler Soundness Bridge

Goal: move the flagship-used `Core -> ProofCore` rules from "extracts to the
expected ProofCore shape" toward source-semantics agreement and checked
trust-gate correctness.

Prior-art peer group for this phase: CompCert for proved compiler passes,
CakeML for verified bootstrap discipline, Cogent for linear systems code with
refinement proof artifacts, F*/Low* for obligation-to-SMT-to-extraction
workflow, and Alive2 for SMT-checked LLVM IR equivalence/optimization
validation. Keep these as reference points for the shape of evidence, not as
marketing claims that Concrete already matches them.

Done when: each flagship-used ProofCore construct is classified as
shape-preserved, eval/source-semantics-preserved, or still trusted; proof-report
facts agree with compiler state; a small independent checker re-derives the
named `CoreCertificateV1` predicate from canonical artifacts rather than
trusting producer summaries; and the remaining trusted base is machine-readable.

**Endgame (north star): the compiler becomes a mechanically-verified artifact,
out of the trusted base.** The per-rule discharge below (R-01..R-21), the
`CoreCertificateV1` independent checker, and Task R-0299 translation validation
are not the finish line — they are the incremental ladder toward proving each
pass semantics-preserving in Lean against the interpreter reference, so the
compiler leaves the TCB entirely. Concrete's intended differentiator is that its
Lean-hosted compiler-correctness work and selected user-program proofs can share
one kernel and one evidence pipeline. CompCert and CakeML remain peer references
for verified-compilation and bootstrap discipline, not projects this roadmap
claims to surpass. The 6B fact-centralized IR and interpreter-as-oracle are the
runway: each pass is a clean function over committed facts with a reference
semantics to prove against, so passes should be built PROVABLE-FIRST, not merely
consistent. This names the endpoint so intermediate design choices bend toward
it; it does not schedule a multi-year proof now, and it must not starve the
near-term or the external trial. The complementary rewrite-passes-in-Concrete
route is Task R-0413; the shorter path is proving the existing Lean-hosted
passes here.

### Task R-0260

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The obligation-ledger requirement survives while the old R-01 through R-21 dashboard implementation retires. surviving requirement: Track each new-compiler pass or validator obligation by named proof status and residual assumptions, retaining the legacy dashboard as historical data. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0506/S12a: merge into pass/validator obligation ledger; preserve legacy historical view.

**Objective:** Add a compiler-soundness rule-status dashboard over R-01..R-21:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0260).

### Task R-0266

**Phase:** A

**Disposition (2026-10-19):** superseded by R-0509/S16: merge; distinguish trusted assumptions from ordinary proof dependencies.

**Objective:** Record a machine-readable trusted computing base for proof/evidence claims:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0266).

### Task R-0272

**Phase:** B

**Objective:** Retain differential generation, trap-identity checks, nightly minimization, alpha and hostile-spelling controls, historical witnesses and accepted raw-pointer-return regressions as new-compiler slices admit them.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

- extend the differential generator with string, heap/allocation,
   linear-value, effectful-I/O, generic-function, generic-struct, and
   multi-instantiation shapes; generate division/modulo including `MIN / -1`,
   shifts at and around the width boundary, and arithmetic that can actually
   reach checked overflow rather than bounding every expression back into a
   safe range. Preserve the rule that an E07xx/panic on a generated well-typed
   program is a compiler bug, and compare the identity of traps rather than
   treating any trap as equivalent to every other trap;
 - rotate seeds in a nightly CI campaign and automatically minimize
   failures rather than relying only on the fixed presubmit seeds; and
 - preserve accepted trusted raw-pointer (`*const`/`*mut`) return fixtures separately
   from historical rejected reference-return witnesses; the frozen dead-path extra-load
   repair is superseded by the new-compiler decision linked below;
 - scale R-0007's metamorphic identity oracle across generated and corpus
   programs. Its strict alpha leg performs capture-avoiding fresh renames in
   the correct lexical namespace and requires identical acceptance, exit,
   output, trap identity, interpreter behavior, and compiled behavior modulo
   honest display/span facts. Its distinct hostile-spelling leg pressures
   generated mangles, builtin/intrinsic names, import aliases, and
   local/global collisions; that transform may preserve behavior or fail
   closed with an allowed collision diagnostic, but may never silently
   diverge or trigger an internal error. Bugs 044/050/051/054 were
   collision-sensitive, so random fresh renaming alone is not coverage.
   Seed the historical witnesses as fixed legs, and keep IR-only spellings
   such as `@fnref.*` in lowering mutation fixtures rather than the source
   renamer.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

### Task R-0273

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The disposition explicitly requires every listed family to map to a retained control. surviving requirement: Audit semantic dependencies and retain a negative control for every listed semantic-darkness family against the new spec and compiler. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0491/R-0498: merge; map every listed family to a retained control.

**Objective:** Add a semantic-darkness audit and red-team gate. The goal is to catch the checked-arithmetic class of bug before it repeats: a construct looks ordinary in source, but its real behavior depends on width, profile, target, allocation, authority, runtime checks, or an outdated proof/interpreter model. Add `docs/SEMANTIC_DARKNESS_AUDIT.md` and `scripts/tests/check_semantic_darkness.sh`; wire the gate into CI or the soundness-bridge area validation artifact. The audit must cover:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0273).

### Task R-0274

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The objective demands one typing truth, while its source-checker and CompilerDB prescription is obsolete. surviving requirement: Prove checker and elaborator correspondence to independent normative typing judgments so the checked and compiled programs agree. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0499/R-0514: retire source-checker/CompilerDB authority prescription.

**Objective:** **Preserve the one source of typing truth through proofs.** Every 2026-07 front-end bug was two passes holding different opinions about the same program: Check typed literals from the hint while Elab typed them from the sibling operand;

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0274).

## Area: Backend, Target, And Stdlib Contracts

Goal: make backend/toolchain/stdlib assumptions explicit, and state exactly
where source-level proof stops.

Done when: SSA, target/toolchain, optimization, ABI/layout, stdlib evidence,
and the incremental-driver area incremental build contracts are explicit enough for release
evidence; the supported SSA-to-BackendIR translation slice is independently
replayable; and every boundary after that slice remains explicitly trusted.

### Task R-0278

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The objective's single-contract requirement survives, but old SSA as that contract is superseded by BIR. surviving requirement: Establish one explicit backend contract through the new BIR boundary. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** Superseded by R-0503/S11: merge into R-0503; BIR replaces this architecture.

**Objective:** Stabilize SSA as the only backend contract.

### Task R-0279

**Phase:** B

**Reconciliation (2026-10-19):** Keep; define separate LLVM/native profiles.

**Objective:** Document target/toolchain model: triple, data layout, linker, runtime/startup, libc expectation, clang/llc boundary, sanitizer/coverage hooks.

### Task R-0280

**Phase:** B

**Disposition (2026-10-19):** Duplicate of R-0421: merge into R-0421; connect certificate obligations to R-0504.

**Objective:** Define optimization policy: allowed optimizations, evidence preservation, debug/release behavior, report/codegen validation. Follow the QBE-style "small auditable backend contract first" principle: prefer a stable, inspectable subset with explicit assumptions over early attempts to match LLVM-level optimization coverage.

### Task R-0288

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The body retains backend-contract requirements but prescribes a deleted ValidatedSSA-to-BackendIR migration. surviving requirement: Make new BIR inspectable and validated while preserving runtime checks, traps, source identities, ABI decisions and trust labels. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** Superseded by R-0503/S11: merge inspection, verification and parity requirements into R-0503.

**Objective:** Add `BackendIR` as a structured backend contract between `ValidatedSSA` and LLVM/native emission. This is useful even if Concrete never ships its own native backend: it gives the compiler a typed, inspectable place to preserve runtime checks, trap/source-span facts, layout/ABI decisions, target constants, trusted/runtime helper calls, and capability/trust labels before they disappear into LLVM text or target code. The intended ladder is `ValidatedSSA -> BackendIR -> ValidatedBackendIR -> EmitLLVM` first; later emitters (native, C, WASM, QBE-style) may consume the same `ValidatedBackendIR` only after the contract is stable.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0288).

### Task R-0290

**Phase:** B

**Disposition (2026-10-19):** Duplicate of R-0491: merge into R-0491, retaining backend/profile matrix.

**Objective:** Add backend/codegen differential validation where executable oracles exist.

### Task R-0302

**Phase:** B

**Objective:** Pin the new compiler's supported LLVM toolchain and require wrong-code, contract and output controls on upgrades, retaining bitcode emission as conditional work.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Declare a supported LLVM/clang version or narrow tested range in the backend contract; connect
candidate/toolchain identity to R-0357. Refuse or warn on unsupported versions through
`scripts/tests/check_backend_contracts.sh`. Every version bump reruns the wrong-code corpus,
backend-contract gate and IR goldens before being declared supported.

The hand-written LLVM bitcode serializer, without LLVM C++ API linkage, remains deferred and
below translation validation (R-0299) in priority: reproducibility/decoupling does not establish
correctness. Pull it into scheduled work only when either a freestanding/no-`clang` target is
pursued or recurring LLVM churn becomes intolerable despite pinning. It is a backend-contract
subcomponent that blocks nothing downstream; it consumes the new validated BIR adapter boundary,
not a resurrected legacy emitter.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

## Area: Unsafe Concrete And Trusted Systems Profile

Goal: make deliberately unsafe Concrete code boring to write, easy to audit, and
hard to misuse. This phase borrows the useful Zig lesson: low-level programs
need excellent tools for pointers, slices, allocators, debug allocators, and
runtime instrumentation; the answer is not a global unsafe mode or Rust-style
lifetime algebra, but small explicit unsafe islands with visible authority and
replayable checks.

Done when: every raw-pointer, unchecked, trusted, allocator, and FFI escape hatch
has a named safe wrapper pattern, audit/report output shows the underlying
assumption, debug/profile instrumentation can catch representative misuse, and a
validation project proves safe code cannot reach these operations accidentally.

This phase does **not** weaken Concrete's philosophy. Safe Concrete remains
linear, capability-visible, second-class-reference, and no-hidden-cleanup.
Unsafe Concrete is a deliberately marked profile for the code that must cross
those boundaries: runtimes, allocators, FFI, VM/interpreter internals,
freestanding/MMIO, sanitizer probes, and backend/ABI glue. Its evidence class is
`trusted`, `runtime_checked`, `tested_by_oracle`, or `assumed` unless a later
proof actually discharges the exact claim.

Reference languages and reading:
- Zig for pointer/slice taxonomy, allocator-passing, arena/fixed-buffer/debug
  allocators, and the "prefer slices over raw many-item pointers when length is
  known" rule:
  `https://ziglang.org/documentation/master/` and
  `https://zig.guide/standard-library/allocators/`.
- Hare for checked slices by default, unbounded arrays as an explicit escape
  hatch, small Unix-shaped wrappers, and honest docs about what remains manual:
  `https://harelang.org/blog/2022-06-21-safety-features/` and
  `https://harelang.org/blog/2021-02-09-hare-advances-on-c/`.
- Odin for tracking allocators, sanitizer/runtime-instrumentation pragmatism,
  foreign-system ergonomics, layout/endian control, and fine-grained
  bounds-check controls:
  `https://odin-lang.org/docs/overview/` and
  `https://odin-lang.org/docs/faq/`.
- Rust/Rustonomicon as cautionary input: unsafe reference/aliasing validity is
  subtle, pervasive unsafe can become harder to audit than raw pointer code, and
  Concrete should avoid silently upgrading raw pointers into ordinary safe
  references:
  `https://dev-doc.rust-lang.org/beta/nomicon/what-unsafe-does.html` and
  `https://doc.rust-lang.org/stable/reference/behavior-considered-undefined.html`.
- Counterpoint reading, to keep the claims honest:
  `https://rustmagazine.org/issue-3/is-zig-safer-than-unsafe-rust`.

Borrow the techniques, not the philosophy. Concrete must not adopt hidden
authority, implicit allocation, implicit cleanup, lifetime syntax, trait-object
unsafe abstractions, ambient context, or a broad "unsafe mode." Debug allocators,
sanitizers, and runtime checks are `runtime_checked` / `tested` evidence, never
proof. Unsafe ergonomics are allowed only when they make the trusted assumption
more legible in source and reports.

### Task R-0312

**Phase:** A

**Reconciliation (2026-10-19):** Keep core decisions before memory interfaces freeze; surface breadth in V6.

**Objective:** Freeze the unsafe-surface taxonomy:

- safe references remain second-class and may not be returned;
- raw single pointers, many-item pointers, fixed-size array pointers, and
  slices are distinct source/report concepts;
- nullable pointers are explicit (`Option<Ptr>`-style), not the default;
- every raw pointer/slice carries mutability, provenance/trust class,
  alignment fact where known, and optional length/bounds facts where known.
Add `docs/UNSAFE_CONCRETE.md` and make it point back to the provable-subsets area's
`UNSAFE_ISLAND.md`, not duplicate it.
The design note must include a comparison table mapping Zig's `*T`, `[*]T`,
`[*c]T`, `?*T`, slices, fixed array pointers, Hare unbounded arrays/slices,
and Odin pointers/slices onto Concrete's chosen surface. Every row must name
whether bounds/null/alignment/provenance are statically known, runtime-
checked, trusted, or unavailable.

### Task R-0316

**Phase:** A

**Reconciliation (2026-10-19):** Keep; settle with R-0496/R-0502 before interface freeze.

**Objective:** Add an unsafe-aliasing policy that avoids Rust's hidden reference-UB trap:

safe references keep Concrete's ordinary borrow/linearity rules; raw pointers
do not silently become safe references. Any operation that temporarily
upgrades raw memory to a safe borrow must state its exclusivity, lifetime, and
frame assumptions in the wrapper contract/report. No "trust me, this raw
pointer is an ordinary `&mut` now" shortcut.

## Area: Performance And Optimization Evidence

Goal: make generated-code speed, size, memory, and optimization claims follow
the same evidence discipline as correctness claims. This phase owns measured
program performance; compiler throughput/caching remains primarily the incremental-driver area,
semantic equivalence remains the backend/differential gates, and release budgets
consume rather than reinvent these facts.

Trigger: start after Task R-0416 has a stable comparable corpus. R-0417–R-0419
are required evidence infrastructure for the affirmed project claim; actual
optimization work remains conditional on a reproduced gap, regression, or
release-facing comparison. If no such gap exists, retain measured baselines and
do not build speculative optimizers.

Done when: every public performance claim names its workload/input, comparable
reference, backend/target/profile/toolchain, metric and uncertainty, raw data,
replay command, and evidence class; generated-code regressions are caught within
calibrated noise; optimization work is pulled by measured gaps; and the release-bar area's
release budgets consume the same ledger.

### Task R-0421

**Phase:** B

**Objective:** Define each new-compiler optimization's owner, semantic preconditions, identity policy, validation controls and performance witness without changing observable behavior or evidence.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Every optimization has a named owner, semantic preconditions, affected operations/failure
semantics, debug/source-identity policy, verifier before and after, interpreter/differential/mutation
coverage, performance witness and applicable R-0504 certificate obligation. Apply this ownership
map across the new IR chain, LLVM and LCB-native. Prefer target-neutral canonicalization/legalization
only where semantics requires it; target instruction selection stays backend-specific. Do not add
a pass merely because another compiler has it.

Acceptance: optimization levels select documented pass sets and cannot change observable checked
arithmetic, traps, capabilities, ownership or evidence status. The named verification and performance
controls demonstrate each pass's contract.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0280** (merged): Duplicate of R-0421: merge into R-0421; connect certificate obligations to R-0504.

### Task R-0422

**Phase:** B

**Objective:** Use profiling and adversarial scaling inputs to prevent superlinear compiler work in the new pipeline with explicit complexity and memory budgets.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Profile before refactoring; retain adversarial scaling inputs, measure time and peak memory over
increasing program sizes, record expected complexity envelopes and gate the structural class,
not one file. Adopt typed emitter fragments only when profiles justify them; another backend's
fragment model does not prove this emitter fixed. Keep compiler-throughput measurements distinct
from generated-program runtime measurements in reports and release claims. R-0422 owns performance
repairs; R-0550 owns the class scaling gate and R-0551 continuous profiling.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

### Task R-0550

**Phase:** B

**Objective:** Retain the scaling corpus and controlled complexity, instruction, allocation and host-normalized timing gates for new-compiler stages as their constructs land.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

The gate generates inputs of 10^4 to 10^5 elements or statements in each shape (array literals, long
functions, many locals, many declarations, deep nesting), compiles them, and times each front-end and
back-end stage. It fails when a stage exceeds its recorded budget, or when the ratio between two sizes
is above the recorded near-linear bound. The budgets are measured on the CI host and recorded with the
SHA they were measured at. A retry budget keeps host noise from failing the gate; it never lets the
pre-fix code pass. Since bug 294 the gate separates the compiler's work from the host's speed: where
the CI host can count retired instructions (perf), each run's instruction count and each stage's
allocations are held to exact budgets, growth between sizes is judged on the instruction counts
against the linear expectation (a wall-time ratio between 10^4 and 10^5 measures the caches as much
as the work: linear stages grew x45 – x60, close to the bound a quadratic had to exceed), and wall
time is judged only against budgets normalized to the host speed measured at `--measure` (capped
at 2x); a host that cannot count keeps the wall bounds. **Exit:** `scripts/tests/check_compile_scaling.sh` (new, in the full gate set) passes
at the closing commit, and each reproducer's pre-fix commit fails it.

R-0550 owns the scaling gate with R-0422 owning repairs and R-0551 profiles. Apply the corpus to
supported new-compiler stages as constructs land; report every unsupported/excluded shape by name.
Remove exclusions when its construct is supported, and grow declaration/nesting shapes to the same
10^4–10^5 scale. Retain bug 027, 248–250, 265–282 and long-function workload witnesses as historical
controls; no exclusion may hide a newly supported superlinear stage. Host noise retry never turns
the deliberately quadratic control into a pass.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

### Task R-0551

**Phase:** B

**Objective:** Continuously profile supported new-compiler workloads at exact landed SHAs and prove hotspot detection with a deliberate scaling regression.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Use leanprof, the owner's Lean profiler, under R-0422 and the performance track. A scheduled CI
server job profiles fixed supported new-compiler workloads at every landed main SHA: supported
large examples, R-0550's scaling corpus and the supported std build. Grow the workload set as its
constructs land. Publish top hotspots and per-stage times, compare with the previous landed commit,
and open a tracked finding when a hotspot grows or appears above a recorded threshold.

This complements rather than replaces instruction-count tracking and its approximately 1.5% alarm
([PERF_TRACKING.md](PERF_TRACKING.md)). Keep server provisioning in the owner's private dotfiles;
only portable scripts belong here. Acceptance: profiles and comparisons at exact SHAs for ten
consecutive landed commits, and a deliberately introduced quadratic hotspot opens a finding.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

## Area: Public Release Bar

Goal: make Concrete understandable and usable by someone who did not build the
compiler.

Done when: a fresh user can install Concrete, run a proof-bearing example,
inspect its audit bundle, independently verify its evidence root offline, and
understand the claim matrix and remaining trust boundary in under ten minutes.

**Language graduation is a gate, not a date.** Concrete can graduate individual
examples before the language graduates. The language reaches **alpha** only
after the source-contract/proof-link path is usable outside flagship hero work;
it reaches **beta/release** only after ordinary project workflow and external
validation are in place.

The [release map](../../ROADMAP.md#release-map) separates the systems preview, verified-component
alpha, project beta and stable supported subset. The preview makes no alpha
claim; the alpha and beta bars below remain required for their respective scopes.
Stable release additionally requires the named compatibility and maintenance bars.

**Alpha bar (verified-component alpha):**
- At least one non-author writes, proves, or contract-annotates a useful
  Concrete program and reports that ProofKit + contracts + `concrete prove`
  were worth the discipline.
- Diagnostics teach the discipline: on the linearity, capability, and
  must-use surfaces, errors name the remedy (consume / drop / return /
  declare the capability), not only the rule. A first-user session stalling
  on an undiagnosed E-code is filed as a compiler bug, not a user error —
  this is measured through the external-validation transcript, not
  self-assessed.
- Source contracts are primary for at least one flagship, with source-linked
  proofs and no JSON dependency for that flagship's main proof surface.
- `concrete prove` guides a user through contract -> obligation -> Lean/omega/
  `bv_decide` evidence -> source link -> audit.
- Small evidence examples exist for every public claim class:
  `proved_by_lean`, `proved_by_kernel_decision`, `tested_by_oracle`,
  `assumed`, `trusted`, `partial`, and `stale`.
- Core language surface blockers are settled or explicitly deferred with
  examples: pattern cleanup, bytes/text/path, iteration, collections,
  ignored-result diagnostics, and capability polymorphism.
- Runtime safety obligations have defined evidence classes for bounds,
  div-zero, overflow, casts, panic/abort, and unchecked conversion behavior.
- README, website, papers, examples, and roadmap agree and do not outclaim
  audit evidence.

**Beta/release bar (language can be released to users beyond the thesis
audience):**
- `concrete fmt`, `concrete test`, and a minimal project model (`Concrete.toml`)
  exist.
- The **minimal local package/evidence foundation** has exercised the public artifact boundary
  before it freezes: manifest, lockfile, workspace, interface/body split, local dependency
  certificate verification, trust/assumption inheritance and evidence-schema compatibility. A
  public registry, hosted publishing and remote cache are not beta prerequisites.
- A **minimal editor path** exists for incremental structured diagnostics, capability/proof-status
  hover, obligation navigation and replay commands. It consumes the compiler session/query facts;
  advanced refactoring, dependency dashboards, migration UI and playground work remain later.
- In-source proof links are the only proof path; `proof-registry.json` support
  must remain absent.
- Release bundles have stable schemas, replay commands, assumption/trust
  reports, and proof-link provenance.
- An installed independent `concrete-cert verify-bundle` path can validate a
  release evidence root offline from untrusted bundle bytes, report the precise
  Core/backend predicates it checked, replay kernel evidence where claimed, and
  keep every unsupported/native boundary visibly trusted.
- A verified-profile policy gate exists and is used by every release-facing
  proof/audit claim; public examples cannot bypass it with stale proofs,
  unapproved assumptions, or hidden trusted/Unsafe boundaries.
- The stdlib/runtime boundary is stable enough for daily examples.
- At least one external user completes the first-user tutorial and a useful
  audit/proof workflow without compiler-author intervention.

### Task R-0425

**Phase:** B

**Reconciliation (2026-10-19):** Keep; grow alongside normative semantics.

**Objective:** Publish a versioned language conformance suite derived from the
normative language reference, runnable by every backend and any future
independent implementation.

Do not make the current compiler's output the expected answer. For each
reference rule classify fixtures as required behavior, required rejection,
implementation-defined behavior with a declared choice, target/profile-
conditional behavior, or unsupported extension. Name the interpreter as the
executable reference only for the subset whose interpreter semantics are
specified and independently checked against normative expected observations;
interpreter/backend agreement alone remains corroboration, not definition.

Version the corpus with the language reference. Each case records source,
reference-section/rule ID, expected stdout/stderr/exit/trap or stable diagnostic
class, capabilities/profile/target, permitted variation, and minimum language
version. Run it through interpreter, LLVM, QBE, WebAssembly when supported, and
future backends without silent fallback. Backend-specific exclusions require a
declared capability-matrix row and cannot make the language case disappear.

Acceptance: coverage mapping from every normative construct and rejection rule
to at least one positive/negative case or explicit untestable rationale;
mutation tests for wrong value, missing trap, wrong rejection, backend drift,
and stale reference links; an external runner with JSON results; deterministic
fixtures independent of host locale/time/network unless those facts are the
declared subject; and a release gate that refuses a claimed-supported backend or
language version with an unexplained conformance failure.

### Task R-0355

**Phase:** B

**Objective:** Establish exact-SHA development-speed baselines for the new compiler and grow release budgets to cover compilation, proofs, reports and independent verification with regression controls.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

`scripts/tests/check_release_performance.sh` measures compiler startup, cold small-project and
stdlib builds, warm incremental no-op, private-leaf and public-interface edits, proof-query and
object-cache hits, relinking, `concrete test`, audit/report generation, proof-check latency and
independent bundle-verification latency against `release/perf-baseline.json`. Include proof
scalability budgets before broad proof expansion: elaboration/recheck memory, proof-term size,
artifact size and validator scaling.

Establish the development-speed baseline before structural implementation of the new compiler,
using a named corpus in `release/dev-speed-corpus.txt`: compiler, supported std, one large example
and one multi-module program. Measure compiler/program clean builds, one-function compiler and
Concrete-program edit rebuilds, peak compiler memory, and fast-push turnaround (R-0513). Record each
measurement with exact main SHA, host class and toolchain; every budget declares noise tolerance.
`scripts/tests/check_dev_speed.sh` fails beyond-tolerance regressions; raising a budget requires a
commit stating the reason. A deliberately injected regression must fail, and the series records
each structural change's effect. As new constructs land, extend this corpus to the full named scope.
Acceptance remains the full release budgets: release CI blocks unexplained regressions and prints
the regressed command.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

## Area: Post-Release Operations And Stewardship

Goal: keep Concrete's claims, users, artifacts, and dependency evidence
trustworthy after the first release. This phase starts only after Task R-0360
ships a public release; it is operational stewardship, not a pre-release
community-building program.

Done when: supported versions and response windows are public; releases can be
reproduced, rolled back, revoked, and repaired; security and evidence incidents
have rehearsed procedures; current/previous-version compatibility is gated;
user failures become permanent corpus evidence; and ecosystem-health claims use
opt-in or public data rather than hidden telemetry.

## Area: Packages And Dependency Evidence

Goal: let package users inspect proof, trust, capability, and assumption facts
before adopting a dependency.

Done when: packages have manifests, lockfiles, package-aware facts, trust
policies, provenance, independently checked interface/evidence receipts,
The incremental-driver area-aware reuse/invalidation, and registry protocol.

Prerequisite: the incremental-driver area completed. If it was rejected/deferred, the packages area may
start only after an explicit roadmap amendment replaces these artifact/query
dependencies; package work may not grow a private cache/driver as a workaround.

**Sequencing split — local foundation before beta, ecosystem scale after.** Phase numbering is
topical, not permission to freeze a public release artifact before its primary dependency consumer
exists. The beta/release bar pulls forward only the minimal local foundation: R-0364–R-0369's
manifest/lockfile/workspace/interface split and certificate codec/verification boundary, plus the
narrow trust/assumption/evidence compatibility needed to prove an upgrade cannot silently weaken a
consumer. Public registry/publishing, hosted docs, remote reuse and ecosystem-scale policy remain
post-foundation work. This split avoids both bad extremes: releasing a public evidence schema never
tested across a dependency boundary, or blocking a narrow release on operating a package registry.

**Integrated package management (decided 2026-10-17; implementation planned).**
Package management belongs to the Concrete language toolchain and ships inside
the `concrete` executable. `Concrete.toml`, `Concrete.lock` and source-level
imports describe one package model. The driver owns add/remove, resolution,
fetching, updates, workspaces and native builds/linking; the compiler checks
imports, visibility, capabilities, contracts and dependency evidence against
that resolved graph. CLI, compiler sessions, reports and editor tooling share
the same graph and identities. Reading an import never implicitly downloads a
package or runs dependency code; the frontend receives resolved driver inputs.

Primary references are Cargo's project workflow, Go modules' explicit resolution
discipline, Zig's integrated compiler/build/package experience and Nix's declared
inputs and reproducibility. Decisions and upstream references live in
[package-manager-design.md](../../research/packages-tooling/package-manager-design.md).

R-0285's queued interop checkpoint pulls forward the minimal local manifest,
lockfile and native-build graph needed by its automatic application workflow;
it does not wait for the full packages-area package protocol or registry. R-0364–R-0367
own the shared package model, explicit updates, locked/offline builds, workspace
lockfile, host-versus-target separation and dependency-path explanations.
Reuse the existing compiler graph as incremental/package infrastructure lands;
do not introduce a separate resolver or cache for foreign bindings. Normal
builds preserve selected versions; explicit package commands own manifest/lock
changes. R-0182/R-0183 own the authority/assumption/evidence upgrade diff.
Public publishing and a separate package-manager executable are outside this
local foundation. This decision scopes existing tasks without reordering the
current execution queue or reducing R-0285's automatic generation/build exit.

## Area: Editor And Human Tooling

Goal: make evidence visible where developers work.

Done when: editor/LSP/tooling exposes the same facts as CI and command-line
reports through the incremental-driver area session/query graph, with bounded edit
invalidation and certificate freshness, without inventing a second truth source.

Prerequisite: the incremental-driver area completed. If it was rejected/deferred, the editor area needs a
recorded replacement architecture before starting; an editor-only fact database
or cache remains forbidden.

**Minimal pre-release slice, advanced tooling later.** The beta bar pulls forward the smallest
honest editor consumer of the incremental-driver area session: compiler-as-service entrypoint, incremental
structured diagnostics, capability/proof-status hover, obligation navigation and replay-command
links. It must reuse CLI/CI facts and may not wrap batch compilation as its normal edit path.
Proof-aware refactoring, dependency dashboards, migration/deprecation UI, hosted playground and
other breadth stay in the editor area after the core path proves useful. First users should not meet a
proof language exclusively through batch reports, but release is not blocked on a complete IDE.

## Area: Concurrency And Research-Gated Extensions

Goal: keep speculative ideas gated until Concrete's proof/evidence foundation
can contain them honestly.

Done when: each research idea is either pulled into an earlier phase by a
forcing example, explicitly deferred, or rejected.

### Task R-0446

**Phase:** B

**Objective:** Generate the new Core operation and trap inventory for backend-neutral consumers, fuzzing and capability reporting, with controls proving automatic constructor coverage and actual consumer use.

**Work:** Build this boundary in `ConcreteSpec` and the new compiler for slice 1, then extend it as constructs land. Independent judgments, correspondence proofs, total definitions and explicit assumptions accompany the implementation. Acceptance feeds B1–B2; B3 deletes the old pipeline after the end-to-end slice is accepted.

**Deliverables and acceptance:**

Generate operation semantics and trap inventory from the new Core constructors. Folding, dead-code
elimination, interpreter and retained backends consume one backend-neutral semantic artifact, with
emitter independence. Extend consumers to fuzz generation and the capability matrix.

Acceptance: adding a new trapping constructor produces generated trapping fuzz cases without
editing the generator; prove every declared consumer reads the generated artifact, not a local copy.
Retain the trap-inventory gate and unary/binary trap mutation controls corresponding to #25–#27.

**Supersession scope:** Only frozen-compiler repairs, legacy shadow cutovers and in-place
migration mechanisms are superseded by the
[new-compiler decision](ROADMAP_DECISIONS.md#2026-11-01-and-2026-11-02--new-compiler-and-retention-contract)
and its [editorial scope](ROADMAP_DECISIONS.md#2026-11-03--editorial-supersession-of-legacy-task-directions).
The requirements above apply to the new compiler as their constructs land; this staging does not
weaken the complete task exit.

## Area: WebAssembly Portability And Sandbox Backend (provisional)

Status: provisional, after 1.0. This phase records a possible later portable backend; it
does not authorize WebAssembly implementation before the entrance criteria below pass. QBE is no
longer a prerequisite (R-0113's 2026-10-19 decision: the Lean Compiler Backend is the QBE path).

Goal: add an independently lowered, portable, sandbox-oriented backend that
pressures Concrete's memory, control-flow, capability, layout, and trap
semantics differently from the LLVM and LCB-native paths.

Entrance criteria:

- The backend-neutral driver (R-0075), generated support matrix, diagnostics and
  artifact/replay model are stable for the LLVM and LCB-native profiles.
- Validated BIR (R-0503), translation-validation status and explicit
  target/toolchain/runtime contracts exist that WebAssembly can consume without
  inventing a parallel middle end.
- At least one named portability, sandboxing, deployment, or validation
  workload demonstrates value that LLVM plus the LCB-native profile do not provide.
- A short design record confirms Wasm remains preferable to Cranelift for that
  workload. If fast native/JIT compilation is the actual requirement, schedule
  Cranelift instead and leave this phase provisional.

Initial scope:

## Area: Freestanding And Embedded Target

Goal: make Concrete's hosted-vs-freestanding boundary explicit enough for
embedded, kernel, and audit-critical targets without destabilizing the hosted
language.

Prerequisite: the hosted stdlib/runtime boundary, allocator story, target model,
and daily workflow in the flagships, proof-authoring and provable-subsets areas must be stable. Freestanding is not
a second backend escape hatch; it is a target profile with fewer assumptions.

Trigger: start this phase when at least one serious workload needs no libc,
explicit allocator/runtime setup, or embedded/kernel-style startup, and the
hosted evidence pipeline can already explain what will be removed.

Done when: a freestanding Concrete program can build under a named target
profile, with no ambient hosted stdlib assumptions, explicit allocator/startup
choices, and an audit report naming every remaining target/runtime assumption.
