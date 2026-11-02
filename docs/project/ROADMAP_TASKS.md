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

**Restructured (2026-11-02):** retired with the legacy compiler: The body combines extracted-model and old report machinery with explicit source, evidence, binary and inventory regression duties. surviving requirement: Retain the defect inventory, independently justified expected outcomes, classified spec/native differential comparisons, evidence and consumer controls, and non-vacuous campaign coverage as new slices admit their constructs. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; extend explicit defect inventory through 101. Extend the permanent defect inventory and per-surface expected results through bug 101.

**Objective:** Prevention harness — stage S1 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; starts now.**

Depends on bug 080 landing (`--report model-diff`).
The extracted model, then Core's semantics, as a third side of the differential fuzzer over a
grammar covering every shape that broke (loop-body sequencing, `let` after assignment, `return` in
loops, if-without-else fall-through, discarded `expr;`, shadowing, renamed match binders, literals
at 2^63/2^64, every cast width pair). Outcomes classified (value, trap, rejection, unsupported,
exhaustion, crash), never skipped. model-diff for zero-argument, `bool` and aggregate signatures
plus a binary leg, with a pinned claimed-function inventory. The four 2026-10-18 hunts and the
checker hunt run before every release candidate.

**Structural classes (2026-10-20).** Prevention covers causes, not only spellings. This task keeps
[docs/bugs/STRUCTURAL_CLASSES.md](../bugs/STRUCTURAL_CLASSES.md): every bug record from 102 on
states its structural cause and owner (`check_bug_filing_complete.sh` refuses one without them), and
each new bug joins a class there, with the owning task, exit or verified-pipeline stage, or adds a
class with a proposed owner in the same change. A class whose owner is not yet scheduled is raised in
the queue under the entry rule when a new bug of that class is reproduced.

**Exit:** each of 080 – 095 has a checked-in bug record, reproducer, expected outcome and
pre-fix/fixed SHA pair. The deterministic differential suite detects 080 and 084 – 088;
name-binding controls detect 081 – 082; evidence and cross-surface controls detect 083 and 089;
checker mutants detect 090 – 094; the binary leg detects 095. Each assigned control fails at its
pre-fix SHA and passes at its fix; generated features have pinned minimum completed-comparison
counts; missing inventory entries fail. The adversarial example suite below passes every leg (exit 2c).

**Independent golden oracles (exit 2c; 2026-10-22).** An expected output is evidence only when
something other than the compiler under test agrees with it. Bug 121's `conlog` golden was recorded
from the miscompiled binary and blessed its wrong count, because the interpreter could not run
`conlog` and nothing else checked it. Every `@run` rule of `scripts/tests/example_manifest.txt`
therefore declares its oracle, enforced by `check_example_outputs.sh` with negative controls:
`interp` (interpreter agreement, which requires `interp=yes`), `spec:<path>[#anchor]` (a hand
derivation from the program's specification or a reference tool's output, recorded in
[GOLDEN_ORACLES.md](GOLDEN_ORACLES.md)), or `unverified:<reason>` (allowed only when the
interpreter does not run the case). The gate prints the number of unverified goldens and lists
them; each one is a tracked gap, retired by an interpreter repair (exit 2c's interpreter gaps,
projects that import `std` modules, argv) or a recorded derivation. The same rule applies to the
adversarial examples below and to every new expected result.

**Adversarial example suite (exit 2c; 2026-10-20; planned).** Phase 7's workloads (`base64_cli`,
`png_chunks`, `wordfreq`, `httpget`, `tcpserve`) and interpreter-vs-compiled probing found many of the
compiler's wrong-code bugs; generated programs are bounded by their grammar. This suite writes small
hand-targeted programs for each current bug class, under `examples/adversarial/`:

| example | exercises | classes |
|---|---|---|
| `linear_flow` | linear handles moved, consumed and destroyed across `if`/`else` and `match` arms, early `return` from inside loops and branches, `break`/`continue` with a live handle, consume-then-exit; rejection legs for a handle leaked on one arm and one moved by a match guard | 078, 087, 090, 093 |
| `generic_destroy` | generic `Destroy` through trait bounds and impls, collections of arrays and of generic structs, nested composition; a rejection leg for an impl's capability hidden behind generic dispatch | 052, 091, 099, 100 |
| `nested_names` | same-named types, functions and methods in nested and sibling modules and against generated specialization names, type-argument arity, relative calls into a nested submodule (its leg asserts today's refusal until 077 is fixed); builtin versus local and imported user-defined `Result` | 054, 070, 073, 077, 096 – 098 |
| `import_aliases` | function, type, method and sibling-module aliases, an alias against a same-named local definition, alias order, colliding aliases rejected | 039, 055, 069, 082, the E0264 aliased-method case |
| `casts_widths` | every cast width pair (narrowing, widening, sign change) at boundary values, literals at each type's minimum and maximum, checked negation; claims on cast functions | 053, 080, 088 |
| `mut_receivers` | `&mut self` methods called through `&mut` parameters, struct fields and array elements and in both arms of a branch; stack-array storage mutated through a borrowed raw pointer (018), mutation of an array extracted from a struct field (019), and receiver borrowing on the right of `&&`/`||` (034), with mutations observed afterwards | 018, 019, 031, 034 |
| `match_and_fnptrs` | match guards with effects, renamed and positional binders, function pointers reassigned across branches and read from struct fields, with their capabilities | 056, 063, 066, 084, 090 |
| `statement_sequencing` | discarded `expr;`, `let` after assignment, loop-step sequencing, branch-local `let`, `if` without `else` falling through; claims on each shape | 085, 086, 101 |

Bug 063's fixed derived-function-pointer cases compile and run through both the function-call and the
method-call path, and its generic-return controls keep E0242 as in `check_cap_inference.sh`. For cases
still unresolved (054, 055, 077 and the E0264 aliased-method case), separate fixtures pin
the documented collision diagnostic, rejection or containment result until the owning repair lands;
runnable cases still require interpreter, native and expected agreement. The suite does not make those
repairs prerequisites of any deliverable. The number-to-shape mappings for 082, 087, 088 and 090 – 100
are provisional until their checked-in records and reproducers land under 2b, and are reconciled
against those records before the suite is accepted.

Each example has its own leg in `scripts/tests/check_adversarial_examples.sh` (new, in the full gate
set): interpreter and native observable output and normalized outcomes must agree with a checked-in
expected result, where normal exits keep their exit status and traps keep their identity rather than
counting every trap as equivalent; a rejection leg asserts its diagnostic code; where the example carries claims, its leg also
runs `--report model-diff` on the claimed functions and asserts each claim's status. The gate fails on a
missing example or leg, and a perturbed expected result must fail its leg (positive control). The
evidence and report cases 058 – 062, 064, 065, 067, 068, 075, 076, 079, 081, 083 and 089 keep their
existing controls (exit 2b covers only 080 – 101). Capability case 071 keeps
`check_cross_package_caps.sh` and `check_cap_sibling_module.sh`; runtime case 072 keeps
`check_spawn_exit.sh`; package-name collision 074 keeps its rejection controls; collection case 057
keeps its collection controls; 092 keeps the 2i controls; 094 and 095 keep the checker and binary
controls of the detection map above. A defect an example newly reproduces is a
reproducer: it enters the queue under the [entry rule](../../ROADMAP.md#the-queue), as a new row-0
exit when it is an authority or safety defect, and the example keeps its leg as that defect's gate.

**Scaled differential campaigns (2026-10-20; planned).** The nightly `fuzz-campaign` job in
`.github/workflows/lean_action_ci.yml` runs `fuzz_differential.py` at 2,000 depth-3 and 500 depth-4
programs and `fuzz_linearity.py` at 400 per date-derived seed (R-0272's seed rotation, merged here).
Once the third side exists, the same generators compare interpreter, native binary and proof model
(model-diff on generated claimed functions) at campaign scale: at least a hundred times the nightly
counts per campaign, sharded by seed range. This CPU-bound work runs first on short-lived remote build
servers once they are available (R-0513 owns the remote runner infrastructure); no provider
credentials live in the repository, and the GitHub nightly keeps its current counts meanwhile. Each
campaign records its commit SHA, toolchain, seed ranges, outcome classification and per-feature
completed comparisons against exit 2e's minima; failures are minimized automatically and checked in as
reproducers, which enter the queue under the entry rule. This campaign is not part of exit 2c's
acceptance and no deliverable waits for it; it is accepted when one campaign at that scale completes
with every outcome classified and every per-feature minimum met, and recurs before each release
candidate with the hunts above.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The remaining migration names old Elab, SSA and PExpr, but the exit preserves identity and forgery invariants. surviving requirement: Use scope-resolved identities throughout the new compiler, distinguish lexical from persistent evidence identity, and retain alias, symbol, binding, substitution and artifact-forgery controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep under R-0514; avoid unnecessary legacy migration before retirement.

**Objective:** Resolved identities — the identity half of S5; R-0497 owns immutable, payload-bound artifacts — stage S5 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-23): slice 1 landed on branch `vp-r0492`; the slice contract is S5 slice
contract 2 in [VERIFIED_PIPELINE.md](../compiler/VERIFIED_PIPELINE.md).** `LocalId` is minted by
the one lexical walk (`Resolve/Locals.lean`), which runs inside `Pipeline.resolve`. The checker's
environment is keyed by `LocalId`, and E0301 is rendered from the same walk. Checks are in
`check_local_identity.sh`, plus mutation family 98. The slice found bug 202: project builds
skipped desugaring. Remaining: Elab, Core, SSA and `PExpr` binders; `DefId`, `FieldIdx` and
`VariantIdx`; contract expressions; `ElabVerify`.
**Slice 6 (2026-10-25, branch `wip/r0492-decl-identity`):** declaration identity for module-level
types and constants, design in [DECLARATION_IDENTITY.md](../compiler/DECLARATION_IDENTITY.md).
Steps 1 – 2 landed: one type environment per module in Resolve (bug 230 fixed) and Core spells a
user `Result` apart from the builtin (bug 227 fixed). Step 3 landed on `wip/r0492-steps34`: types
are compared, and methods and trait impls looked up, by `TypeId` in Resolve, Check and Elab (bug
223 fixed; the inherited aliased-imported-method obligation is met by its fixtures). Step 4 landed
there too: a public constant is imported by its `ConstId`, referenced in Core by its defining module
(`cfg::BASE`) and inlined by Lower and the interpreter in that module's scope (bug 233 fixed). The
probes filed bugs 242 (imported signature types read by spelling) and 243 (aliased newtype import).

`LocalId` for every binder, allocated at
Resolve so the source checker and Elab both consume it; `DefId` as a compilation-scoped declaration
handle allocated before body elaboration, preserving callable namespaces, specialization arguments
and extern/intrinsic distinctions, with a validated `DefId → DefinitionIdentity` mapping at evidence
boundaries; owner-checked `FieldIdx`/`VariantIdx`; `RegId`/`BlockId` in SSA. No `String → identity`
coercions: identities come from scope-aware environments through checked adapters;
capture-preserving alpha-canonicalization before hashing, frozen identity bytes preserved or
explicitly versioned. Pipeline artifacts (owned by R-0497) become immutable and payload-bound (slice 1 sealed `ValidatedCore`,
`MonomorphizedProgram` and `SSAProgram`, 2026-10-23), with diagnostic artifacts typed separately.

**Semantics decisions (2026-10-21):** the compiler refers to `Option` and `Result` only by resolved
identity, never by spelling, so a user enum named `Result` is a distinct type (LAYOUT_CONTRACT §3.4);
mangled symbols for every Concrete-defined function take their disambiguator from `DefId` (FFI.md,
"Symbol Names").

Language: no block `let` shadowing a visible name; colliding import aliases rejected; match binders
equal or name their field.

**Exit:** `check_ir_no_string_identity.sh` rejects string identity joins in resolved tables, the
source checker's environments, Core, SSA and `PExpr`; 081, 082, 087, 093 and 094 reject or bind
correctly; targeted mutations show checker consumption/block-local accounting, `ElabVerify` and the
theorem-table identity checks load-bearing; compile-fail controls cover artifact forgery and
diagnostic-path bypass.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The immediate checkpoint repairs the existing pipeline, while typed operations and boundary-value agreement survive. surviving requirement: Define typed arithmetic and casts from one specified semantics, preserving widths, signedness, operation modes, literal ranges and defined-trap regression controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep immediate and slice checkpoints separate.

**Objective:** Typed operations — stage S6 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; the immediate checkpoint is queue row 2.**

**Immediate row-2 checkpoint:** make `IntArith` the arithmetic authority for the existing pipeline's
evaluation, folding and arithmetic-model generation, with emitted operations checked against the same
specified semantics. `check_single_arithmetic.sh` and differential controls cover every supported width
and operation family, including casts and defined traps. This checkpoint does not wait for S5; the
typed-operation migration below does.

Depends on S5. Integer ops typed by `IntTy` with
checked/wrapping/saturating as distinct op kinds and signedness on the op; pointer arithmetic its
own op; boolean, float and pointer ops distinct; signed literals canonicalized before range
checking; contextual literal types resolved before construction and re-validated after Mono; typed
casts; `IntArith` the only arithmetic (`check_single_arithmetic.sh`).

**Exit:** every integer op in Core and SSA carries its kind and width; the arithmetic lint passes;
signed-minimum fixtures pass; 088's reproducers agree on every side.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0079** (superseded): superseded by R-0493/R-0501/03: merge into typed operations and pass obligations. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0079)
- **R-0221** (superseded): superseded by R-0493/S6: merge semantic ownership into R-0493. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0221)
- **R-0464** (superseded): superseded by R-0493/R-0495/R-0460: merge; archive completed H24 fix and retire SSA-first prescription. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0464)

### Task R-0496

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The old interpreter replacement retires, while the body requires the semantic contract and raw-memory interface before Core freezes. surviving requirement: Specify the new Core semantic contract and values-to-bytes boundary, then implement its clock-indexed evaluator and instrumented semantics with representative memory, clock and trace proofs. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; state implementation versus proved-subset coverage separately.

**Objective:** Core semantics — stage S7 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned.**

Depends on S5, S6. The semantic contract above for Core: value
semantics with allocation identities, capability-tagged external events, defined traps,
behaviours from clocks, an instrumented mode with ghost ownership (moved places, loans, frames,
capability use); FFI and OS as external-call contracts. The interpreter becomes this semantics plus
an IO driver. It is the oracle for S1 from here on. S4 fixes the relevant language rules before
this stage freezes their semantics. Before freezing Core, the raw-memory interface is specified
jointly with S11 (initialization, alignment, pointer fragments, allocation lifetime, address
exposure, foreign mutation), and a representative typed-value/raw-memory abstraction lemma is
proved. Representative proofs of clock stability and partial-trace monotonicity land with the
evaluator.

**Exit:** covers every construct the checker accepts; the interpreter is the semantics; the
differential fuzzer compares it with the binary; S4's paragraphs link bidirectionally to their
clauses; the representative lemmas are proved.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The landed wrappers belong to the old pipeline, but payload binding and diagnostic-path exclusion are explicit exits. surviving requirement: Make new pipeline artifacts immutable and bound to exact certified payloads, with separate diagnostic types and compile-fail controls for forgery, mutation and bypass. This line governs over older implementation directions for the legacy compiler.

**Objective:** Immutable, payload-bound pipeline artifacts — the artifact half of stage S5 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md); R-0492 owns the identity half.

**Status (2026-10-23): slice 1 landed on branch `verified-pipeline`; the slice contract is in
[VERIFIED_PIPELINE.md](../compiler/VERIFIED_PIPELINE.md) (S5 slice contract 1).** `ValidatedCore`,
`MonomorphizedProgram` and `SSAProgram` are sealed and carry erased certificates about their exact
payload. `ElaboratedProgram` is sealed. `UnverifiedSSA` and `CoreSubset` are separate uncertified
types. Compile-fail controls are in `check_pipeline_artifacts.sh`. The slice found bug 180: a project
report replaced a certificate's payload and ran Mono on the result. Remaining: the stage types
after SSA as each IR lands; binding `ElaboratedProgram` to its input; payload-bound evidence
records (class 10); the identity half under R-0492.

Each stage's output is an immutable artifact bound to its exact payload, constructible only by its producer or verifier (slice 1, 2026-10-23, sealed `ValidatedCore`, `MonomorphizedProgram` and `SSAProgram` in `Pipeline.lean`); any mutation yields an unverified artifact; diagnostic artifacts (including `lowerUnverified`) have separate types that downstream stages and evidence APIs reject. The stage sequence follows the target IR chain (HIR → Core → `CheckedCore` → `CheckedMonoCore` → Place MIR → Memory CFG → BIR → LLVM printer or Machine IR), extended as each IR lands. Private constructors are API discipline; soundness comes from the stated theorems.

**Exit:** compile-fail controls cover artifact forgery, certificate reuse after mutation and the diagnostic-path bypass; `Pipeline.lean` exposes only typed stages.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0073** (superseded): superseded by R-0497/R-0503: merge into payload-bound validated BIR artifacts.
- **R-0082** (superseded): superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0082)
- **R-0116** (superseded): superseded by R-0497/R-0510/R-0514: merge; use payload binding and new IR chain. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0116)

### Task R-0498

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old Proof, Check and Backend inventory retires, while the exit requires demonstrated enforcement rather than comments. surviving requirement: Map every new enforcement claim to its production gate leg and targeted mutation, and label unproved shape checks honestly. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; grow manifest with new boundaries.

**Objective:** Enforcement demonstrated — stage S3 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; starts now.**

A claim → gate-leg → targeted-mutation manifest over
verification and language docs, enforcement-bearing comments in `Concrete/Proof`, `Check`,
`Backend`, and emitted contract text; the backend contract is generated from BIR op semantics once
S11 lands. R-18 – R-20 relabelled shape-only until their proofs exist.

**Exit:** every in-scope enforcement phrase has a manifest row whose mutation its leg kills in a
qualified campaign.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0273** (superseded): superseded by R-0491/R-0498: merge; map every listed family to a retained control. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0273)

### Task R-0499

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: Shadowing the old source checker is obsolete, but declarative rules and their discriminating controls remain required. surviving requirement: Define independent Core judgments and a corresponding authoritative checker, preserving ownership, authority, bounds and reference regressions with positive and rule-breaking mutation controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; R-0514 owns whole-language rollout.

**Objective:** Checker rules, authoritative on Core — stage S8 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned.**

Depends on S5, S7. Declarative judgments in
`docs/language/CHECKER_RULES.md` for use-once, conservation at reachable exits, borrow validity
(a source may not be moved, destroyed or incompatibly mutated while a reference to it is live;
shared/exclusive aliasing), no escaping or stored reference, capability containment (a call's
resolved required capabilities, after removing `Unsafe` only at a `trusted` call site, are a subset
of the caller's declared set, through pointers, generics and FFI), raw dereference only in a
`trusted` body or an `Unsafe`-covered context, and bounds (an access establishes an in-range index
or executes the bounds trap). A Core checker implementing them runs as a shadow beside the source
checker with a disagreement gate, then becomes authoritative and emits `CheckedCore` plus its
derivation; capabilities are re-checked after Mono. A rule-violation fuzzer applies one
rule-breaking mutation to accepted programs (including destroying a source while a
permitted local or downward reference is live, conflicting borrows, reaching a capability through a pointer or
generic, raw dereference outside both `trusted` and `Unsafe`); valid programs that must trap are
not checker rejections. Positive controls: trusted raw-pointer operations and trusted absorption of
`Unsafe` accepted; trusted absorption of any other effect rejected. The instrumented semantics is
the runtime witness.

**Exit:** every rule has an enforcing site, an S3 manifest row and a mutation class; the CI seed
set rejects every mutant and accepts every well-formed program with minimum completed counts per
rule; the instrumented semantics runs the corpus and fuzzer programs with zero violations; 090 –
094 and the 087 borrow-block case are rejected by the Core checker; the source checker holds no
ownership, borrow or capability rule. Shadow gates compare the two checkers on the common supported
subset; intended acceptance changes (such as references in aggregates, now rejected) have normative
expected results in an explicit migration manifest. The historical 092/095 aggregate-reference
programs remain as rejection fixtures, with versioned historical binary checks, and an accepted
loop-mutation/downward-borrow fixture detects stale storage.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0227** (superseded): superseded by R-0499/R-0507: merge into checker rules and soundness.
- **R-0228** (superseded): superseded by R-0499/S8 and V6: merge enforcement into R-0499; raw-operation extensions into V6. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0228)
- **R-0267** (superseded): superseded by R-0499/R-0507/R-0495: merge; capabilities and purity remain distinct.
- **R-0274** (superseded): superseded by R-0499/R-0514: retire source-checker/CompilerDB authority prescription. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0274)

### Task R-0500

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The body’s prose-first homes and old implementation checkpoints are superseded, while its rule and example obligations survive. surviving requirement: Dispose of the listed language rules under the approved decisions and give each its Lean judgment, semantic clause and accepted, refused or trapping conformance examples as slices land. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; row 6 adds the rules each slice admits.

**Objective:** Written language rules — stage S4 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; starts now.** 2026-10-21: exit 2i's SCOPE-1, REF-6, renamed pattern binder and versioned diagnostics are enforced (see the [2i row](../archive/ROADMAP_LEGACY_PLAN.md#phase-1-exits) for what remains).

Normative paragraphs in `docs/language/` for block scoping
and shadowing, match-binding positions and names, statement vs value position, sequential
assignment within a loop iteration, literal typing and range (signed minima preserved), cast
semantics per width pair, evaluation order, short-circuiting, and second-class references; each
with accepted/rejected examples and fixtures.

**Exit before advancing past queue row 2:** every rule has a normative paragraph, examples, cited
fixtures and S1 generator coverage meeting its minima. S7's exit later adds links from each
paragraph to its semantic clause.

**Semantics decisions (2026-10-21):** the rules this task enforces are now written in their
canonical homes ([Decisions](ROADMAP_DECISIONS.md#decisions)): references and scoping in MEMORY_SEMANTICS §15 (REF-6
rewritten: no `let`-bound reference, no assignment to a reference binding, E0302 / E0303, enforced since 2026-10-21;
constants are values; SCOPE-1 staged behind the renamed pattern binder), evaluation order and
assignment in STATEMENT_EXPRESSION_MODEL and MEMORY_SEMANTICS §10 (exit 2j), and casts in
ARITHMETIC_POLICY §8 (exit 2k: the lossless class table, the three named-operation families and the
three stages). Each records its status on main and its open gaps.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0435** (superseded): superseded by R-0500: merge migration, renamed-pattern syntax and controls. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0435)

### Task R-0501

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: Carving jobs out of old Lower and shadow comparison retire, but pass semantics and cleanup preservation remain explicit exits. surviving requirement: Build proved new lowering passes with executable semantics for sequencing, matching, cleanup and check expansion, retaining differential and early-exit cleanup controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep by slice.

**Objective:** Place MIR — stage S10 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18; amended 2026-10-21): planned.** Queue row 4 carries a whole-language structural
checkpoint for the existing profile after R-0514's checker is authoritative: `Lower`'s sequencing,
match compilation, cleanup/`defer` and check expansion move into Place MIR passes consuming
`CheckedMonoCore`, as shadow paths with disagreement gates against the existing lowering (structural
class 8). The Place MIR semantics, the per-pass differential against Core's semantics and the proofs
grow per slice (rows 3 and 6).

Depends on S7, S8a (consumes `CheckedMonoCore`). Carve sequencing, match compilation, cleanup/`defer`
edges and check expansion out of `Lower.lean` into Core → Place MIR passes, one job each, with a
Place MIR semantics; per-pass differential against Core's semantics.

**Exit:** `Lower` no longer performs these jobs; each pass is total and differentially tested with
minimum counts; cleanup expansion is proved for one representative function with early exits and
`defer`.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0079** (superseded): superseded by R-0493/R-0501/03: merge into typed operations and pass obligations. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0079)
- **R-0082** (superseded): superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0082)
- **R-0295** (superseded): Superseded by R-0501/S10: retire workload gate; MIR is already an architectural decision.

### Task R-0502

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The existing-profile layout repair retires, while the body places shared memory design before implementation and requires representation proofs. surviving requirement: Specify the shared value/raw-memory boundary and one layout authority, then deliver checked representations and memory lowering with allocation, alignment, store and memory-injection proofs. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; early design obligations precede full implementation.

**Objective:** Memory model, layout and Memory CFG — stage S11 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18; amended 2026-10-21): planned.** Queue row 4 carries a whole-language checkpoint
for the existing profile: one `Layout` authority supplies every size, alignment, offset and enum
representation the existing lowering and LLVM emission use, with a gate comparing it with the emitted
types (structural class 12). Representation certificates, Memory CFG and the proofs grow per slice.

Owns the shared raw-memory specification with S7, block–offset/provenance memory, `Layout`, representation certificates and Place MIR → Memory CFG lowering.

**Semantics decisions (2026-10-21):** layout is unspecified unless a `repr` guarantees it, each `repr`
guarantees only its documented properties, code relies on nothing but `sizeof`, `alignof` and array
stride = `sizeof`, and one `Layout` owns every fact; `Option` and `Result` are laid out per
instantiation like every generic enum ([LAYOUT_CONTRACT §3.3 – §3.4](../archive/design-records/LAYOUT_CONTRACT.md)).
Bug 122's per-instantiation repair is not deferred to this task; it lands under exit 0b.

**Exit:** Memory CFG has executable semantics and a verifier; `Layout` emits checked
representation certificates; Place MIR → Memory CFG comparisons cover related values and stores,
traces, traps, allocation and alignment cases; a representative memory-injection lemma is proved.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0076** (superseded): superseded by R-0502: merge into single proved Layout authority. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0076)
- **R-0085** (merged): duplicate of R-0076: merge through R-0076 into R-0502.
- **R-0088** (superseded): superseded — aggregate/memory ABI realization moves to R-0502 (layout) and the adapter boundary (R-0505/R-0517) across V2–V6.
- **R-0434** (superseded): superseded by R-0502: merge into layout certificates and Memory CFG verification. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0434)

### Task R-0503

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: EmitBuiltins migration and old Lower retirement disappear, but validated BIR, runtime semantics and honest profile labels survive. surviving requirement: Emit the new compiler’s explicitly unverified LLVM path from validated BIR with executable semantics, runtime contracts, support labels and differential qualification. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; absorb R-0278/R-0288.

**Objective:** BIR, the LLVM adapter and the rt module — stage S11 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned.**

Depends on R-0502. Owns Memory CFG → BIR, BIR semantics and verification, LLVM adapter legalization, the `rt` migration, generated backend contracts and replacement-path qualification.

**Exit (depends on R-0502):** LLVM is emitted only from validated BIR, under the adapter
contract above; BIR semantics and differential gates run; `EmitBuiltins.lean`'s LLVM-AST helpers
are replaced by `rt`; profile labels are emitted; the old `Lower` is retired only after the complete
replacement path meets its gates.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The old SSA and BackendIR paths retire, while the exit preserves validator soundness and substitution controls. surviving requirement: Give new validators explicit soundness propositions and reject semantic substitutions, unsupported certificates and wrong retained LLVM artifacts without inflating native assurance. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; retain R-0299/R-0300’s distinct assurance scopes.

**Objective:** Validators — stage S12 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned.**

The validator interface; validators for SSA
construction and for optimization certificates, each with a stated soundness `Prop` and one
representative proved instance. R-0299/R-0300 validate the LLVM profile: a pinned inventory of
proved functions records validation status per semantic edge and call closure; unsupported closures
stay `translation_trusted`/`blocked`; mismatches are `translation_mismatch` and never grant
native-code evidence.

**Exit:** SSA and optimization validators reject targeted semantic substitutions and unsupported
certificates; R-0299's BackendIR V1 validation and R-0300's independent replay pass their
admitted-subset, mismatch, unsupported and substitution controls; the emitted-LLVM profile checks
the retained `.ll` bytes including helper bodies; LLVM optimization, emission, linking and
runtime/ABI behaviour remain explicitly trusted for that profile.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0105** (superseded): superseded by R-0491/R-0504: merge debug/release and sanitizer controls. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0105)

### Task R-0506

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: Maintenance of today’s authoritative evidence retires, but stated obligations and axiom policy begin before pass proofs. surviving requirement: State each new proof boundary and validator obligation with explicit premises and status, enforce transitive axiom classification and totality, and prove representative lemmas before freezing boundaries. This line governs over older implementation directions for the legacy compiler.

**Objective:** Stated obligations and totality — stage S12a of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; the immediate checkpoint is queue row 2.**

**Immediate row-2 checkpoint:** enforce the transitive axiom policy on today's authoritative evidence
at status, issuance and consumption. Reject `sorryAx` and unregistered axioms; kernel-only evidence
permits only `propext`, `Classical.choice` and `Quot.sound`; registered native-evaluation and float
axioms require distinct trust labels. Positive controls and forbidden-transitive-dependency mutations
qualify this checkpoint independently of S5 and the new pass ledger.

Starts with S5 and grows as each boundary lands
— including S13 and linking — rather than after the lower pipeline: every pass's `Refines` and
every validator's soundness are type-checked Lean `Prop`s in `PassObligations.lean`, with explicit
premises and a ledger (stated / differentially tested / proved); `sorryAx` and unregistered axioms
are rejected transitively, kernel-only evidence permits only `propext`, `Classical.choice` and
`Quot.sound`, and registered native-evaluation and float axioms require separate trust labels; a totality lint covers semantics, checkers, validators and proved
passes (untrusted search may stay partial).

**Exit:** every registered boundary, including the native backend and linking, has a type-checked
obligation with explicit premises and status; evidence dependencies pass the declared axiom policy;
semantics, checkers, validators and proved passes pass the totality gate. Before each boundary is frozen, its
semantic relations and obligations type-check and a representative preservation or
validator-soundness lemma is proved for its motivating feature, including native encoding and
linking.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The compiler theorem survives, while the old VC-generation implementation is replaced by proofs over new Core functional models. surviving requirement: State and progressively prove the new compiler’s maximal-behaviour refinement, independent frontend correspondence, resource adequacy and exact-image claim transport with explicit residual assumptions. This line governs over older implementation directions for the legacy compiler.

**Objective:** Compiler correctness and proof transport — stage S15 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned (Part 2; starts when its prerequisites exist).**

Prove every ledger
obligation by simulation or validator soundness. Define refinement over maximal behaviours under
related initial states and admissible environments, with the explicit observation relation for
values, external memory and ghost erasure, and a preserved target-memory validity invariant; prove
native-to-Core refinement for executions satisfying the stated contracts, composed across separate
compilation and linking; state the resource relation and prove the sufficient-resource theorem.
Prove frontend correspondence: source bytes and import roots → HIR → `CheckedCore` against
independently specified syntax, resolution and elaboration judgments (a relation defined as
"whatever the elaborator produces" does not count). Prove VC generation against Core (R-0460).
Each binary-transported claim cites a theorem that its proposition is preserved by the refinement
relation; receipts record the theorem, profile and compiler build.

**Exit:** the composed source-bytes-and-import-roots-to-native-image theorem, including
independently specified frontend correspondence, is proved and re-checked by con-leche and nanoda.
HIR-only and `CheckedCore`-only theorems are intermediate milestones. Every binary-transported claim
cites its preservation theorem.

### Task R-0509

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The body names old ProofCore controls but explicitly starts the trusted inventory immediately and requires a minimal independent consumer. surviving requirement: Maintain the new compiler’s explicit trusted-base inventory and prove evidence binding and interpretation soundness, retaining independent-kernel replay and hostile consumer controls from the first authoritative claims. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; inventory starts now, minimal consumer at V1. The trusted list is non-increasing for a fixed supported scope; additions for newly admitted scope are recorded and reviewed separately. A separate compiler/checker execution-trust reduction plan with milestones is recorded here; R-0413 explores only the Concrete-hosted path.

**Objective:** Trusted base and independent evidence check — stage S16 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; the trusted-base inventory starts now, proofs in Part 2.**

`std`'s trusted
implementations verified against contracts or listed with audit (the list only shrinks); the
published trusted-base inventory. R-0353 independently checks retained evidence — decoding,
authenticated roots, subject/dependency binding, exact propositions, replay and policy — without
trusting stored verdicts; it does not by itself establish source-to-model or native-code
correspondence, which carry their own recorded assurance. Codec and hostile-input prototypes may proceed beside queue rows 3 – 8. R-0353's complete cross-profile
consumer remains at row 10, after row 9 and R-0004's prerequisites; a minimal independently checked
retained-artifact consumer is required for every publicly authoritative V1 claim. Prove soundness of `ClaimSpec` statement generation, evidence-to-subject/proposition
binding, authoritative verdict aggregation and retained-artifact checking; audit exact elaborated
axiom dependencies, so kernel-only verdicts admit only the named standard logical axioms while
native-evaluation and float axioms stay separately labelled. R-0413 dogfoods the evidence model onto
the compiler.

**Exit:** the inventory names every remaining assumption and owner; R-0353's acceptance criteria
pass with every 079 – 095 fixture pinning its expected result vector, and no envelope or replay
success upgrades an unchecked semantic edge. Statement generation, subject/proposition binding, verdict aggregation and
retained-artifact checking have proved soundness, with transitive assumptions classified by the
axiom-policy gate.

**The trusted-base inventory (listed 2026-10-25).** Each entry is trusted until its mitigation
removes or narrows it; the published inventory carries the same rows with their current state.

| trusted | why it is trusted | mitigation | owner |
|---|---|---|---|
| Lean's code generator and runtime | the Concrete compiler, its checkers and validators run as code Lean compiled, and the kernel never sees that code | per-compilation certificates checked in a kernel, so a checked artifact does not depend on the binary that produced it; development builds keep trusting the binary | R-0552 |
| the C compiler that builds Lean's output | Lean's generated C and its runtime are compiled by a C compiler, and its output is run, not checked | the same certificates (R-0552); until then it is pinned with the Lean toolchain (`lean-toolchain`, the Nix build) | R-0552 |
| `@[implemented_by]` and `@[extern]` in proved code | the kernel reasons about the stated definition while execution runs the replacement | an inventory of every such attribute reachable from proved code, each with a stated equivalence or listed as trusted; `check_proofcore_callable_identity.sh` already refuses an `@[implemented_by]` or `@[csimp]` replacement on the ProofCore wrappers it pins | R-0509, with R-0506 |
| native-trust axioms (`bv_decide`, `native_decide`) | they rest on compiled evaluation (`Lean.ofReduceBool`), not on the kernel alone | every use registered and labelled (`check_axiom_inventory.sh`), never an unqualified kernel-only verdict; replace a use with a kernel proof where the cost allows | R-0506 (exit 2g) |
| the Lean kernel | every proof is accepted by it | independent re-checking by nanoda and con-leche (`check_kernel_recheck.sh`, `check_multi_kernel.sh`) | R-0509 |
| the assembler and linker (the QBE path) | LCB emits assembly text that an external assembler and linker turn into an image | LCB's V04/V05 milestones (encoding and composition) remove them; until then they are named premises of the native theorem, and R-0505 checks the final image | R-0517, R-0505 |
| the machine (ISA) model | the native theorem is stated against a model of AArch64, not the hardware | a named premise of every native claim; the model is LCB's and is differentially tested against the hardware | R-0517 |
| the semantics' fidelity to intent | a proof shows the compiler matches the formal semantics, not that the semantics says what the language means | an outside expert review of Core's V1 semantics, source judgments specified independently of the implementation (V1c), and differential testing of the semantics against the interpreter and the compiled output | R-0511 |
| libc, the OS, FFI and `trusted` code | their behaviour is assumed through external-call contracts and audited trusted code, not proved | stated contracts for every foreign call; `std`'s trusted implementations verified against contracts or listed with audit, and the list only shrinks; the trusted-code obligations formalized in V6 | R-0509, R-0514 |

For the LLVM profile, the BIR → LLVM IR printer, LLVM and the system toolchain are also trusted (R-0505). Whether a module marker
or a block can confer trust is an open owner decision (OD7, OD22); the inventory lists today's
behaviour and does not settle it.

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

**Restructured (2026-11-02):** retired with the legacy compiler: Old V1 staging and today’s evidence path retire, while design proofs, target regressions and minimal claim consumers survive. surviving requirement: Review representative Core design proofs before interface freeze and deliver cumulative new-compiler slices with honest theorem scopes, retained target programs, exact artifact binding and independent checking. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Proof-library engineering (refinement, clock, memory-injection, substitution and ownership libraries) has bounded deliverables recorded with each V1 increment.

**Objective:** The first cumulative verified releases — a proved path from checked Core to a native
image for a small language, carried through every stage of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices).

**Status (2026-10-19; amended 2026-10-20 and 2026-10-21): planned; queue row 3.** The design examples and isolated proof development start now; V1a, V1b and V1c are accepted only when their [prerequisite exits](../archive/ROADMAP_LEGACY_PLAN.md#feature-prerequisites) hold, together with the subset obligations below. V1c names the restricted-source correspondence below as its own deliverable (2026-10-21).

Work (details and premises in the plan's *V1 milestones*): reviewed machine-checked design examples for
move/destroy, downward exclusive and shared borrows, reborrowing, structured cleanup and typed-value
representation in byte/provenance memory, before Core's interfaces freeze; **V1a**, checked Core →
executable BIR refinement for `i64`, `u64`, `bool`, locals and assignment, conditionals, a general loop
with structured exits, non-recursive direct calls, comparisons, checked `+ - *` and one
capability-authorized `print_int` event, with terminating, trapping and divergent behaviours; subsequent
V1 increments for the remaining widths, casts, division, shifts, recursion and loop forms; **V1b**,
native AArch64 through Concrete’s proved BIR → LCB-IR adapter and an applicable pinned LCB preservation theorem (R-0517). C01–C03 establish only the certified typed-assembly endpoint; rendering and encoding remain named assumptions until separately covered. R-0505 supplies final-image correspondence, with R-0508 proving the composition through the exact image, runtime and startup. Any assumed rendering, assembler, linker, loader or ISA correspondence remains explicit in the claim; mutation-tested image checks alone do not establish validator soundness; **V1c**, restricted-source correspondence (syntax,
resolution, elaboration judgments specified independently, then proved or certificate-checked); a
minimal canonical task identity and an independent retained-artifact consumer for every published slice
claim. Stages exercised, restricted to the admitted subset: R-0492 – R-0493, R-0495 – R-0497, R-0499,
R-0501 – R-0510 and R-0517, with minimal task identity and independent replay delivered under the V1 requirements.

**Exit:** for successful compilation of an admitted `CheckedCore` program to an identified image, under
related initial states, admissible environments and recorded resource premises, a Lean proof of S15's
maximal-behaviour refinement through the explicit observation relation, the preserved target-memory
validity invariant and the sufficient-resource theorem; checker soundness and basic capability
containment for the subset; every used pass or validator proved; one nontrivial Core claim transported
to its binary; the closure re-checked by con-leche and nanoda and bound to the exact retained artifacts.
V1 is described as source-to-native only once its frontend correspondence (V1c) is proved; until then the
published theorem states that it starts at `CheckedCore`. An outside expert's review of Core's V1
semantics and the design examples is recorded.

**V1a target programs (2026-10-20; planned).** Small programs inside V1a's scalar subset as the
[plan](../compiler/VERIFIED_PIPELINE.md#delivery-verified-vertical-slices) defines it, under
`examples/v1a/`, with `#[proof_by]` claims proved through today's evidence pipeline on the four
non-divergent targets:

- `sum_to`: an `i64` loop with a structured exit accumulating with checked `+` and printing its result
  through `print_int`; a claim relates the result to a Lean reference function.
- `max3`: `bool` conditions, comparisons and non-recursive direct calls; a claim that the result is one
  of the arguments and no smaller than any.
- `checked_trap`: checked `*` and `-` on `i64` and `u64` reaching overflow and underflow; the trapping
  behaviour is asserted, with a claim on the non-trapping inputs.
- `counter_u64`: `u64` locals and assignment in a loop with comparisons; a claim on the final value.
- `diverge`: a loop with no exit after a `print_int` prefix; its regression leg checks the prefix and
  nontermination within a fixed bound, recording exhaustion rather than claiming that this finite
  observation proves divergence; it carries no claim.

`scripts/tests/check_v1a_targets.sh` (new, in the full gate set) gives each program a leg: terminating
and trapping runs compare interpreter and native output and normalized outcomes with a checked-in
expected result, and `diverge` uses the bounded prefix and exhaustion check above; `--report model-diff`
agrees on every claimed function and each claim reads proved; and a subset check fails when a program
steps outside V1a's subset (another width, a cast, division, a shift, recursion, another loop form, an
aggregate or a reference). Once V1a's pipeline exists these programs are its acceptance programs and its
regression set: each compiles from checked Core to BIR and its BIR behaviour matches the recorded one.
They add nothing to V1a's theorem: its scope stays the subset the plan defines, and their claims are
candidates for this task's one transported claim, not new theorem statements.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The old whole-language shadow rollout retires, but the body’s checked-program identity and rejection regressions remain essential. surviving requirement: Make the new compiler consume the exact identity-resolved Core payload checked against independent judgments, expanding by slice while retaining checker, capability and artifact-substitution controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; broader proof coverage remains Phase 4.

**Objective:** Make the shared front end sound for every user now: identities from Resolve on and one
authoritative checker over Core for the whole language, consumed by both the existing lowering and the
`verified` profile.

**Status (2026-10-19; amended 2026-10-20): planned; queue row 4, alongside R-0511.** The parts V1a's subset exercises proceed with V1a now and are accepted with it once its [prerequisite exits](../archive/ROADMAP_LEGACY_PLAN.md#feature-prerequisites) hold; the whole-language rollout starts once row 0's in-flight candidates (batch 2, type-identity, generic-destroy) are accepted on main, implementing the rules as exits 2i – 2k state them, without waiting for row 2's other exits (amended 2026-10-21, [priority decision](ROADMAP_DECISIONS.md#decisions)). Neither task waits for the other's complete exit; both use the same identities, Core representation, checker implementation and checked-artifact interfaces. The source-tree checker returns `Unit`
and elaboration rebuilds the program independently, so the program checked is not the program compiled;
bugs 087 (borrow-block use-after-free), 090 and 092 – 094 lived in that gap, and name-keyed
environments produced 082, 087, 093, 094 and 096 – 098. The slices prove these rules only as they reach
them (V1 – V7, beginning with scalar typing and basic capabilities in V1, downward borrows in V2, and extending through resources, polymorphism and unsafe/FFI interactions), years out; this task makes the checker authoritative for the whole language before then.

Work: deliver R-0492's identities and R-0497's immutable, payload-bound artifacts across the shared
front end. Implement R-0499's declarative rules once over Core, extending coverage to the whole
supported language through shadow checking, qualified disagreement gates and an explicit migration
manifest. Both profiles consume the exact `CheckedCore` payload; specialization consumes that artifact
and produces payload-bound `CheckedMonoCore`, which is the only input to lowering. R-0510 supplies
whole-language specialization validation and post-Mono capability checks. V1 uses the same
implementation restricted by its admission predicate, with a proved trivial specialization instance
where applicable. R-0514 depends on the Core representation and rule specifications needed for its
checks, not on whole-language compiler-correctness proofs; S7/S14/S15 proof coverage stays recorded per
subset.

**Exit:** `check_ir_no_string_identity.sh` covers the shared front end and the source checker; the Core
checker is authoritative for the whole language and the source checker holds no ownership, borrow,
reference, capability or `Unsafe` rule; 087's borrow-block case, 090 – 094 and 096 – 098 are rejected or
compiled correctly through it; R-0499's rule-violation fuzzer rejects every rule-breaking mutant in the
qualified suite and accepts its well-formed positive controls, with minimum completed counts per rule
and feature combination.

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

**Restructured (2026-11-02):** retired with the legacy compiler: Converting the old 664-definition baseline retires, while proof visibility and executed-definition correspondence remain required. surviving requirement: Require new semantics, checkers, validators and proved passes to be total and executed as proved, with an environment inventory and controls for opaque or replacement execution gaps. This line governs over older implementation directions for the legacy compiler.

**Objective:** Compiler totality: every pipeline function provable (no `partial`, `unsafe` or opaque
definitions). A proof about a compiler pass cannot see inside a `partial` function: Lean compiles it
to an opaque constant, so a theorem about the pass can only treat it as an unknown. The same holds for
`unsafe` and `opaque` declarations. `@[implemented_by]` and `@[extern]` are a different case: the
definition keeps its Lean body, so proofs can unfold it, but the compiled compiler runs the replacement
instead, so a theorem about the Lean body says nothing about the executed code until the replacement's
correspondence is checked or the attribute is removed. The inventory tracks all five so that both gaps
(no proof possible, proof about code that does not run) only shrink. Proving the verified-pipeline
passes (R-0491 – R-0510, queue rows 3, 4 and 6) therefore needs the functions those passes run to be
total and executed as proved first, which is why this task sits ahead of them in the queue (row 2,
exit 2p).

**Scope and owner.** This task builds the mechanism behind the totality gate that R-0506 (stage S12a)
owns: R-0506 states which code must be total (semantics, checkers, validators and proved passes) and
keeps its exit; R-0553 supplies the environment inventory, the ratchet and the conversions. As the
[verified-compiler plan](../compiler/VERIFIED_PIPELINE.md) allows, untrusted search (heuristics,
elaboration search, register-allocation search) may stay partial: its result is checked by a total
validator, and its baseline row records that reason instead of being converted.

**Queue entry.** It enters under the entry rule as an explicit dependency of V1a, the next proof
deliverable: V1a proves checked Core → BIR refinement for the passes its subset runs, and that proof
cannot be completed while those passes' functions are `partial`. Making V1a's own stages total is part
of R-0511's V1a work and is supplied by step 2 below; step 1, the ratchet that exit 2p closes on, is a
prevention gate that no deliverable's acceptance waits for, so exit 2p continues alongside.

**Depends on:** nothing. **Blocks:** the pass proofs of R-0491 – R-0510 for each stage it has not yet
made total.

1. **Step 1, the ratchet gate, has landed** (675871b4; see CHANGELOG.md):
   `scripts/tests/check_totality_ratchet.sh` inventories, from the Lean environment, every `partial`,
   `unsafe`, `opaque`, `implemented_by` or `extern` declaration in the `Concrete.*` modules against
   the committed baseline `docs/compiler/totality_baseline.tsv`, in CI and in the pre-push hook, so the
   count only goes down. What remains is steps 2 and 3: converting the declarations the baseline lists
   (664 `partial` definitions when step 1 landed; the baseline file is the current count).
2. **Step 2: convert the baseline's `partial` definitions in batches.** Batches go in pipeline-stage
   order, closest-to-proven stage first, guided by the per-stage inventory
   `research/compiler/partial-def-inventory-2026-10.md`. Each definition falls in one of three classes:
   - *structural*: the recursion is already structural, and the change is often only deleting the
     keyword;
   - *measure-based*: the parser, worklists and fixpoints, given a `termination_by` measure and its
     decreasing proof;
   - *fuel*: the interpreter, which runs programs that may diverge, takes an explicit fuel argument
     and reports running out of fuel as its own outcome.
   Each batch removes its rows from the baseline in the same commit, and the ratchet gate checks that.
   For an `implemented_by` or `extern` row, the batch removes the attribute, or records why the
   replacement must stay and what checks its agreement with the Lean body.
3. **Exit:** for the stages the verified pipeline proves (R-0491 – R-0510), the baseline holds no
   semantics, checker, validator or proved-pass declaration; the only rows left in those stages are
   untrusted search whose result a total validator checks, each with its reason. From then on the same
   gate refuses outright any other non-total declaration in those stages, in CI and in the pre-push
   hook. Later, under this task, the same requirement extends to the whole compiler: every remaining
   row is converted or is genuine untrusted search with its reason.

Exit 2p closed on step 1; steps 2 and 3 continue through Phases 2 and 4, stage by stage ahead of the
pass proofs that need them.

### Task R-0554

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The cleanup list mixes surviving repository controls with old Report splitting, ProofCore overlap and string-identity repairs. surviving requirement: Keep unique specification and document authority, manifest-driven tests, drift and dead-code controls, generated-artifact discipline and branch hygiene, applying structural boundaries to the new compiler as components appear. This line governs over older implementation directions for the legacy compiler.

**Objective:** Repository cleanup: one owner per fact, document and test, so the repository says each
thing once and the gates read manifests instead of hand-kept lists (the repository reorganization plan
approved 2026-07-28). It is queued in row 2 after R-0553 and has no Phase 1 exit of its own: its
sub-items continue alongside the queue, each landing on its own branch under the normal promotion
pipeline, and none may choose, encode or implement a side of an [owner decision](../spec/decisions.md#owner-decisions).
Completed sub-items are recorded only in [CHANGELOG.md](../../CHANGELOG.md): the ROADMAP compaction
(sub-item 3), the grep benchmark data out of git (part of 12), the grammar three-way cross-check (13)
and the compile-scaling gate's fix (15, bug 315) have landed. The list below keeps what remains, with
its owner and branch.

**Queue entry and capacity.** The task names no reproducer and no dependency of RocksDB slice 1 or
V1a, so the queue's entry rule alone would send it to the backlog. It is in row 2 because the project
owner asked, on 2026-10-28, for the whole cleanup programme to be queued after R-0553; that request is
its admission, recorded here as an explicit exception like the development-process exception. Its
integration effort is charged to track C (adversarial validation and gate health) of the
[capacity allocation](../archive/ROADMAP_LEGACY_PLAN.md#capacity-allocation), since most sub-items consolidate gates,
tests and the documents gates read, and it never preempts a defect repair or weakens an exit.

Owners: the cleanup agent (branches `wip/cleanup-*`) and the spec agent (`wip/spec`).

1. One normative language specification, `docs/spec/` (kept as chapters rather than one SPEC.md file,
   so no rule is stated twice), in progress: the S12 and S13 gates and most absorptions have landed;
   the remaining `docs/language/*.md` files (listed in `docs/spec/unabsorbed.tsv`) are absorbed, then
   deleted. Its full target is [R-0555](#task-r-0555). Owner: the spec agent, `wip/spec`.
2. The book's language chapters (`docs/book/src/language/`) teach by example and link to SPEC rule IDs
   instead of restating rules. Owner: the spec agent, `wip/spec`.
4. Consolidate `docs/project/` (36 documents) into `PROJECT.md` plus the contributor documents. Owner:
   the cleanup agent, `wip/cleanup-*`.
5. Consolidate `docs/verification/` (59 documents) into one normative verification specification plus
   its rationale. Owner: the cleanup agent, `wip/cleanup-*`.
6. Move `docs/NOTES/` into `research/`. Owner: the cleanup agent, `wip/cleanup-*`.
7. Gate and test manifest: about 284 `check_*` gates; `run_tests.sh` (686 KB), the CI workflow (2,556
   lines) and the Makefile become manifest-driven; duplicate gates merge; a new `tests/` taxonomy.
   Owner: the cleanup agent, `wip/cleanup-*`.
8. Split the report code (`Report.lean`, 6.5k lines, and `ReportObligations`). Owner: the cleanup
   agent, `wip/cleanup-*`.
9. Proof-module overlap audit (`Proof.lean` against `ProofCore.lean`), research only. Owner: the
   cleanup agent, `wip/cleanup-*`.
10. Replace string-encoded identities (184 `== "literal"` comparisons). Owner: the cleanup agent,
    `wip/cleanup-*`.
11. A dead-code finder with a ratchet gate. Owner: the cleanup agent, `wip/cleanup-*`.
12. Generated artifacts out of git: `concrete.wasm` is no longer committed; the site workflow
    builds it from the published commit (`scripts/browser/build-site-compiler.sh`). Owner: the
    cleanup agent, `wip/cleanup-*`.
14. Branch hygiene: merged remote branches are deleted (192 on 2026-10-28), and merged and dead
    worktrees are pruned periodically. Owner: the cleanup agent.

### Task R-0555

**Phase:** A

**Restructured (2026-11-02):** retired with the legacy compiler: The body’s old semantic modules and prose-link workflow change, but complete mechanization and conformance obligations survive. surviving requirement: Complete audit disposition and maintain one Lean-generated normative specification with independent judgments, rule-indexed conformance, validated bug-rule references and chapter acceptance artifacts through the language slices. This line governs over older implementation directions for the legacy compiler.

**Objective:** Language spec: the WebAssembly model. `docs/spec/` becomes a specification in the sense
of the WebAssembly specification: every rule is mechanized against a formal definition, the language
is covered completely, a conformance suite indexed by rule ID runs on any implementation, and the
spec decides what is a bug. It continues R-0554 sub-item 1 and is placed ahead of the
verified-pipeline proof rows it serves (R-0496, R-0499): those proofs are stated against the rules
this task makes exact. No workstream may choose, encode or implement a side of an
[owner decision](../spec/decisions.md#owner-decisions); a question one of them surfaces becomes an `open`
entry (`docs/spec/decisions.md`) for the owner. Owner: the spec agent, `wip/spec`.

**Queue entry.** Queue row 0s, the first row after row 0, approved by the project owner on
2026-10-29 as an explicit exception to the entry rule, like R-0554. It is charged to track A and
shares the active proof slot (not a third integration slot); it proceeds continuously and is paused
only for row-0 soundness repairs, and it never preempts tracks B or C. It is not a predecessor of
rows 1 or 2. Each chapter closes independently: its prose absorbed, then its accept and reject
examples under rule-ID fixtures, then its Lean definition and `Semantics:` link landed together.
Absorbing the remaining `docs/language` files and the archived language-scope text completes before
exit 2q's migration starts. The chapter gates each deliverable waits for are under the ROADMAP's
feature prerequisites; the spec does not gate R-0496 as a whole (R-0496 lands each Core definition
with its chapter's links, R-0499 implements the answered rules construct by construct and refuses
anything unspecified, and the soundness proofs wait for each link).

(a) **Mechanized.** Every entry names the Lean definition of the rule (`Semantics:`), and the spec is
checked against that formal definition, not only against the implementation. The link becomes
required chapter by chapter (check_spec.sh S7), so a linked chapter never regresses. It starts with
the definitions that already exist in `Concrete/Semantics/`: `IntArith` (the ARITH chapter),
`TypeJudgment` (the TYPE typing rules), `RefWellFormed` (REF), `BindingMode` (pattern binder modes),
`Capabilities` and `TrustEdges` (TRUST and capabilities). Exit: every chapter has a required link.

(b) **Complete.** Every `docs/language/*.md` file is absorbed (rules and rationale into the spec,
tracking material to ROADMAP, KNOWN_HOLES or research, the file deleted; S13), and so is the archived
language-scope text (`docs/archive/roadmap-language-scope.md`). The areas the spec covers thinly get
chapters: expression evaluation (the dynamic semantics beyond statement outcomes), generics and
monomorphization, lexical structure, the built-ins the compiler treats specially, and abort and
failure semantics. Exit: `docs/language/` holds no document, and each listed area has a chapter whose
refusals all have reject examples.

(c) **Conformance suite.** Accept and reject cases indexed by rule ID, runnable by any implementation
(today the native compiler and `--interp`), in the style of the WebAssembly spec tests. The suite is
the existing rule-ID fixture index, not a second corpus. Exit: the gate reports every rule that forbids something and has no reject
case, and that list is empty or each entry is an `open` entry.

(d) **The spec decides what is a bug** (owned by a separate agent, not the spec agent; new bugs cite
rule IDs now, and older records are backfilled inside the chapter that absorbs their rule, which
blocks no deliverable). Every bug record names the rule it violates
(`Violates: <RULE-ID>@<version>`), or records `Spec gap` (no rule decides the behaviour: the fix starts
by writing the rule, filed as spec work) or `Open: OPEN-n` (the behaviour waits on an owner question:
the fix is parked until the owner decides, as bugs 305 – 307 and 311 are). A not-a-bug verdict cites
the rule that permits the behaviour ("permitted by RULE-ID", as the class-13 audit's G5 did). This is
enforced, not a convention: `scripts/gen/new_bug.sh` requires one of `--violates RULE-ID`,
`--spec-gap` or `--open OPEN-n`; the bug ledger and dashboard count bugs by the rule violated and by
spec gap and open; a gate checks that every cited rule ID resolves in `docs/spec/ids.tsv`. The open
records and the 2026-10 records are backfilled; older records may say `Violates: unrecorded`, and the
share of those only shrinks. Exit: the gate holds on every record, and no record filed after the gate
lands says `unrecorded`.

Measured state (2026-10-29, after the owner decided OD1 – OD32): 0 of 161 entries link a
Lean definition; 138 of 161 have `Evidence: tested`; 18 open entries (OD33 to OD46 and OD48 to OD51, raised while
recording those answers). `docs/spec/MAINTAINERS.md` ("Path to the complete spec") keeps the current figures.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The old module relocation and TermIR promotion retire, but the completion gates retain corpus, authority and gate-integrity requirements. surviving requirement: Preserve unique temporal authority, canonical generated inventories, shared non-vacuous assertions and mutation registration, and enforce producer/consumer boundaries as new compiler and evidence components are built. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep canonical owner; absorb overlapping R-0447/R-0478 work.

**Final repository architecture: temporal honesty, generated facts, and enforced dependency
boundaries.**

**Status and scheduling:** RATIFIED TARGET; implement in vertical slices. R-0480's subject-based
documentation move is complete and is NOT to be undone. Journal/status infrastructure and the gate
library may proceed independently. The `Concrete/` module moves wait until R-0004 finishes scoped
definition identity and the affected evidence boundaries are stable; then align them with
R-0114–R-0118 rather than moving the same modules twice. VIR moves when TermIR promotion begins.

**Why this task exists.** Repository disorder is primarily informational, not cosmetic. Four kinds
of material have repeatedly occupied the same documents and data structures:

1. what is normatively true now;
2. what is measured now;
3. what happened at a dated point in history; and
4. what the project intends to build.

Moving files without separating those temporal roles only relocates drift. The durable design uses
subject-oriented directories for discovery, explicit lifecycle/owner metadata for authority, a
journal for dated narrative, canonical machine artifacts for measured state, and compiler module
boundaries that mirror producer/consumer trust boundaries.

#### Canonical target layout

```text
/
├── Concrete/                 compiler implementation
│   ├── Frontend/
│   ├── Resolve/
│   ├── Check/
│   ├── Core/
│   ├── Proof/                proof/extraction producer machinery
│   ├── Evidence/             portable typed evidence objects
│   ├── VIR/                  canonical proposition language and certificates
│   ├── Report/               projections only; never reconstructs evidence
│   └── Backend/
│
├── docs/                     maintained documentation, organized by subject
│   ├── language/
│   ├── compiler/
│   ├── verification/
│   ├── platform/
│   ├── project/
│   ├── stdlib/
│   ├── bugs/
│   └── book/
│
├── research/                 non-normative exploration, organized by subject
├── journal/                  dated historical records
│   ├── incidents/
│   ├── audits/
│   ├── decisions/
│   └── milestones/
│
├── artifacts/                schemas and checked/generated repository facts
│   ├── schemas/
│   ├── status/
│   └── manifests/
│
├── proofs/                   attached proof developments
├── std/                      Concrete standard-library source
├── examples/                 user-facing programs and evidence workloads
├── tests/                    compiler, oracle, adversarial and negative corpora
├── scripts/
│   ├── tests/
│   │   └── lib/              shared non-vacuous assertion vocabulary
│   ├── generators/
│   └── tooling/
├── site/
├── paper/
├── editor/
└── bench/
```

The root remains conventional and small. `README.md`, `ROADMAP.md`, `CHANGELOG.md`, `LICENSE`,
`Makefile`, Lean/Lake/Nix manifests and toolchain files, `Concrete.lean`, `Main.lean`, and the shared
logo are legitimate root entry points. Ignored `.lake`, `.cache`, `.build`, and `.concrete-cache`
directories are generated local state, not repository information architecture, and are not moved
merely to make a checkout look emptier.

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

#### Axis 6 — Proof, Evidence, VIR and Report are different layers

Keep `Concrete/Proof/` and `Concrete/Evidence/` separate:

- `Proof/` owns compiler-side extraction, theorem attachment, prover integration, classification
  production and proof replay orchestration;
- `Evidence/` owns portable typed subjects, identity wrappers, digests, theorem/table bindings,
  dependency material, correspondence results, roots, assumptions, trust propagation, receipts and
  policy inputs;
- `VIR/` owns the promoted TermIR proposition language, closed fragment, canonical codec,
  structural evaluator and certificate attachment/checking interfaces;
- `Report/` owns human/JSON views over validated objects only.

The intended dependency direction is:

```text
Frontend -> Resolve -> Check -> typed Core -> Proof producer
                                      |             |
                                      v             v
                                     VIR -------> Evidence -> Report
                                                     |
                                                     v
                                          standalone verifier artifacts
```

This diagram is a constraint, not permission for every displayed arrow. In particular:

1. `Evidence` MUST NOT import `Report`.
2. `Report` may render validated evidence but MUST NOT mint receipts, recompute roots, classify
   theorem dependencies, reconstruct identity from strings, or independently derive evidence.
3. `VIR` MUST NOT import frontend parsing, project loading, compiler reporting or ambient prover
   elaboration. Its consumer grammar and evaluator remain small and structural.
4. The standalone verifier shares canonical schemas/VIR semantics, not compiler implementation or
   report code.
5. Proof producers may emit candidate objects; only closed constructors/validators in the owning
   evidence layer produce authoritative values.
6. Unknown imports, constructors, schema fields and evidence dispositions fail non-current by
   default. Success is enumerated; catch-alls do not grant it.

Enforce these rules through generated import-graph gates, private constructors, compile-failure
probes and production-consumer mutations—not comments alone.

#### Concrete/Evidence target ownership

After R-0004 stabilizes its identities, migrate by semantic owner rather than by today's filenames:

```text
Concrete/Evidence/
├── Identity/        PackageIdentity, DefinitionIdentity, ModelIdentity,
│                    ContractIdentity, PropositionIdentity, typed digest domains
├── Subject/         implementation and proof-subject preimages/digests
├── Attestation/     implementation references, models, relation/scope/evidence
├── Contract/        exported behavior/effect/frame/resource interfaces
├── Dependency/      typed edges, classification, table bindings, correspondence, roots
├── Receipt/         envelope, replay binding, environment identities, serialization
└── Policy/          assumptions, trust/advisories, multidimensional acceptance
```

Do not collapse all existing Proof modules into Evidence. PExpr/ProofCore extraction, theorem
metaprograms and backend proof production remain producer machinery. A relocation lands only when
the semantic owner is clear, imports remain acyclic, behavior is unchanged, and every code/comment/
gate reference moves in the same commit.

#### Concrete/VIR promotion

Do not invent a parallel VIR. Promote and extend `Concrete/Semantics/TermIR.lean` when its roadmap
work begins. The target adds canonical serialization and domain-separated proposition identity,
the closed declared fragment needed by obligations, a total structural consumer evaluator,
adversarial parsing, and certificate bindings for Farkas/linear and DRAT/bitvector witnesses.

The fragment permanently excludes ambient metaprogramming and proof-kernel features irrelevant to
Concrete claims. Extensions are explicit trust-surface decisions with semantics, codec, evaluator,
negative controls, compatibility/versioning and independent-parser implications reviewed together.

#### Sequencing — one move per stable boundary

1. **Preserve the completed subject-based docs migration.** Finish R-0480 lifecycle/owner headers
   and duplicate-current-status gates; do not create `docs/normative/` or `docs/reference/`.
2. **Introduce journal conventions and a pilot migration.** Move one dated audit/incident family,
   preserve errata/supersession semantics, and gate that journal entries cannot become current
   authorities.
3. **Create canonical corpus/status schemas and generators.** Establish exact denominators before
   deleting hand-maintained numbers. Keep old/new shadow comparison until they agree by identity,
   not merely total.
4. **Consolidate gate assertions and mutation registration.** Migrate one gate family at a time;
   keep behavior and deliberate failure controls green before deleting local helpers.
5. **Finish R-0004 scoped identity migration.** `PackageIdentity` and `DefinitionIdentity` become
   mandatory at all evidence boundaries; correspondence reaches its honest closure; roots and
   receipts become authoritative only afterward.
6. **Perform the Evidence module split with R-0114–R-0118.** Move stabilized identity/subject/
   dependency/receipt/policy objects once, while typed ProgramFacts/pass boundaries remove the
   remaining reverse dependencies. No path-only compiler reshuffle beforehand.
7. **Promote TermIR into VIR when the codec/evaluator/certificate work begins.** Preserve one
   proposition authority; do not duplicate semantics under a new name.
8. **Build the standalone verifier from canonical artifacts.** It checks envelope/accounting and
   replayable certificates without importing the compiler; it never claims to verify source-to-VIR
   correspondence.
9. **Shrink historical roadmap prose only after replacement authorities exist.** Move dated
   narratives to the journal, quantitative snapshots to generated artifacts, and completed work to
   the changelog. The roadmap retains the capability map, execution order, acceptance conditions,
   blockers and qualitative authority states.

Each step is independently green and reviewable. Do not combine semantic compiler edits with broad
path moves. Do not delete an old status/count producer until its replacement is denominator-complete
and mutation-tested.

#### Explicit non-goals

- no second repository or monorepo split—the compiler, proofs, corpus and gates deliberately
  cross-validate one another;
- no deep decorative taxonomies beyond the named ownership boundaries;
- no grand renaming campaign and no path churn without a semantic owner;
- no relocation of `research/`, `paper/`, `site/`, `std/`, examples or tests merely for symmetry;
- no machine-generated replacement for decisions that require qualitative judgment;
- no journal entry, generated Markdown page, changelog entry or research note as a competing source
  of current truth;
- no certificate/verifier claim that closes source-to-VIR correspondence—the producer-side semantic
  bridge remains separately measured and explicitly weaker;
- no Evidence/Report cycle, no report-side fact reconstruction and no raw string digest crossing an
  evidence module boundary where a typed digest domain exists.

#### Completion gates

R-0481 is complete only when all of the following are true:

1. the directory layout above exists for every component whose implementation phase has begun, and
   deferred directories remain named targets rather than empty scaffolding;
2. every maintained doc has lifecycle/owner metadata, every adopted research note names its
   normative owner, and every journal entry is dated and linked forward;
3. local-link, owner, contradiction and journal-authority gates have non-vacuous negative controls;
4. canonical corpus/status/mutation artifacts reproduce across checkout paths and reject missing,
   duplicate, partial, unknown and failed-producer inputs;
5. every hand-copied canonical denominator has been replaced by one generated owner or explicitly
   marked historical with its commit/toolchain;
6. shared assertion helpers replace private copies and their vacuity/self-test suite is green;
7. the mutation registry answers coverage by invariant and production consumer, with no
   registered-but-unexecuted mutation counted as killed;
8. import gates prove `Evidence !-> Report`, `VIR !-> Frontend/Report`, and the standalone verifier
   imports neither compiler nor report implementation;
9. reports are projections: removing any report-side reconstruction cannot change authoritative
   evidence, while mutating the owning evidence object changes every projection consistently;
10. Proof/Evidence relocation is behavior-preserving and happens after R-0004 identity closure;
11. TermIR/VIR has one semantics, one canonical codec and one proposition identity;
12. ROADMAP contains no duplicated quantitative status board, while retaining the qualitative
    state needed to understand blockers and authority transitions;
13. build, docs drift, local links, roadmap linearity, import direction, corpus/status generation,
    gate vacuity and mutation-registry gates all pass from a clean checkout.

**The governing rule:** organize by semantic subject, represent temporal authority explicitly, and
make every structural boundary executable. A cleaner tree without unique fact ownership and gates
is not completion; it is future archaeology with better folder names.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0447** (merged): duplicate of R-0481: merge unique operational controls; retire conflicting directory blueprint. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0447)
- **R-0478** (merged): duplicate of R-0481 / R-0474: merge each half into its actual owner. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0478)

### Task R-0479

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The remaining survey targets old Report defaults, but the body states a general fact and refusal preservation invariant. surviving requirement: Represent unavailable or failed computations without fabricating facts, and preserve refusals through typed results, text, JSON and exit codes with production controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Merge evidence-surface cases into R-0494; retain broader diagnostic audit.

**Scheduling: opportunistic.** Nothing blocks on it, and each instance is a one-line change. The
survey is the work, not the fix.

**Objective:** make "this was not computed here" unrepresentable as a valid fact value.

**The defect, from a real failure.** `shadowEdgeKinds` reported edge kinds as

```
if e.fn.isTrusted then "trusted" else "unclassified"
```

having consulted no classification table. `unclassified` is a LEGITIMATE `DependencyEdge`
constructor, so the output was indistinguishable from a real classification of an unclassified
edge. That is why it did not read as a bug: it read as a finding. On 2026-08-13 it caused a
coverage fail-open to be reported that did not exist, with a prediction that root coverage would
fall 62/64 → ~53/64, committed and then retracted. Had the stub returned `NOT-CLASSIFIED-HERE`, the
staleness would have been visible on the first line of output.

**Why this is not the same task as one-producer-per-fact.** `check_one_producer.sh` (landed
2026-08-13) would now catch a second producer of edge kinds. It would NOT catch a producer that
computes the right fact from too little information and reports a plausible default — that is a
separate failure mode, and the more deceptive of the two, because a wrong value that is in-domain
survives every type check and every set-comparison gate.

**The rule:** a function that may not have looked returns a value OUTSIDE the fact's domain —
`ABSENT`, `NOT-COMPUTED-HERE`, `Option`, a distinct constructor — never a domain value that also
means something specific. This is already the discipline for
`ProofEvidenceReceipt` (private constructor, no `Inhabited`), `ImplementationManifest` (private
`ofRows`), and `ManifestRefusal` (named reasons rather than a smaller count). Diagnostics were
exempt because they "only report", which is exactly the assumption that made this one costly.

**The rule continues through every projection.** Replacing an in-domain default with an honest
marker is not complete if a later string catch-all maps that marker to success. The typed result,
rendered text, JSON/artifact disposition and process exit code must preserve the same distinction.
Unknown or newly added refusal constructors default to non-success; success is enumerated rather
than supplied by `_ => ok`. Report gates must likewise validate the producer's exit status before
interpreting its stdout, so a failed command cannot contribute apparently valid evidence.

**PROGRESS 2026-08-13 — the `.getD` half is DONE and ratcheted; the `| _ =>` half is MEASURED.**

Five sites fixed, all of them a failed read rendering as a real value of the fact:

- `Report.lean:4203`, `Main.lean:1559`, `Main.lean:1722` — a failed obligation lookup defaulting to
  `"missing"`, which is a genuine canonical status (`.notProved => "missing"`), so "no obligation
  record" was indistinguishable from "an obligation exists and is missing". Now
  `no_obligation_record`.
- `Report.lean` capability-origin, two sites — `| _ => "transitive"` served as BOTH the real fourth
  origin and the fallback for an absent `origin` key or a malformed trace, so an unreadable origin
  became a positive claim that the capability arrived transitively. `transitive` is now matched
  explicitly and the fallback is `origin_unavailable`.

Two of the three obligation sites were found BY THE RATCHET after I believed the sweep was
complete, which is the argument for the ratchet over a one-time survey.

**Ratcheted:** `check_one_producer.sh` refuses any `.getD` default drawn from a closed vocabulary of
12 (canonical statuses, dependency-edge kinds), reading code rather than prose via
`lib/code_refs.sh`, with a non-vacuity probe on the same path the assertion uses.

**REMAINING, and smaller than expected.** Measured: of 36 `| _ =>` branches returning a string
literal across `Concrete/` and `Main.lean`, **zero** return a value from the status or edge-kind
vocabularies — that half was already clean for the closed sets the ratchet knows. What remains is
the vocabularies the ratchet does NOT yet know: the 36 include `"source"`, `"self"`,
`"missing_proof_link"` and 15 bare `""`. Each needs the same question asked — is this literal a
meaningful value of the fact being reported? — and the ratchet's vocabulary extended per closed set
found. `""` is the ambiguous case and deserves a decision: it is out-of-domain for a digest but
reads as "empty" rather than "not computed".

**NOT a violation, and written into the gate so it is not "fixed" later:**
`if dep.isEmpty then ("none", "missing", "")` is a COMPUTED determination that the obligation is
missing, not a failed lookup papered over. The rule targets the failed-read shape.

**RULE REFINED 2026-08-13, after `docs/verification/EVIDENCE_ARCHITECTURE.md` landed.** The test is NOT "is the
default a member of the vocabulary". It is **"does the default ASSERT something the failed read did
not establish"**. Those come apart at the bottom of a ladder:

- `transitive` has positive content — it claims the capability arrived by a particular route — so a
  parse failure rendering as `transitive` fabricates provenance. Violation.
- obligation `missing` describes an EXISTING record whose proof is absent, so using it for "no
  record at all" conflates two different facts. Violation.
- `SourceCorrespondence.missing` means exactly "no correspondence evidence established" — the ⊥ of
  its ladder. A failed read defaulting to it asserts nothing and is fail-CLOSED. **Legitimate.**

**Consequence for the ratchet, and it is over-strict as written.** `check_one_producer.sh` blacklists
`.getD "missing"` wholesale, so it would reject a correct `SourceCorrespondence` default once the
typed-evidence objects are implemented. When that happens the fix is an explicit allowlist entry
naming the type and stating why its ⊥ is a safe default — NOT deleting the check, and NOT renaming
the value to evade it. A ladder's ⊥ is the one value a failed read may legitimately produce, and it
must be justified per type rather than assumed per name, since `missing` means different things in
the two vocabularies that already use it.

**Scope:** survey `Concrete/Report/Report.lean` for defaults standing in for "did not look".
Known-good examples to preserve, since they already follow the rule: `ABSENT (no structural body
threaded)`, `NO-SUBJECT`, `UNPINNED`, `none (no call-graph entry)`. The suspects are any branch
returning a domain value on a `none` or a fallback.

**Acceptance:** each converted site returns an out-of-domain marker; at least one gate asserts that
a report line for an uncomputed fact does NOT render as a valid value; and the `| _ =>` /
`.getD <domain value>` pattern is counted so the number cannot silently rise.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The old query/report and resolver repairs retire, but the body specifies diagnostic-legibility regressions beyond those sites. surviving requirement: Make new diagnostics distinguish invalid command forms, identify retired syntax and provide valid next actions or candidates, retaining class-level and blank-hint mutation controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep bounded diagnostics increment under R-0137/R-0515.

**Objective:** Close the diagnostic-legibility defects the 2026-07-31 surface
audit reproduced, as the early slice of R-0137's diagnostics bar rather than
behind its flagships-area dashboard.

R-0137 owns the full bar and the measured baseline (12 of 15 rejected programs in
`tests/invalid_programs/` emit an empty `hint`; ~25 `hint :=` sites against 215
`E0NNN` codes). But R-0137 is a flagships-area validation artifact, and these three are
hours of work that R-0466 needs in place to have anything to measure. Pulling
them forward is the point of this task; it does not narrow R-0137.

- **Bug 064 — the rejection that cannot be acted on.** Every bare semantic
  `--query KIND` exits 1 with `unknown query kind 'predictable'` and then lists
  `predictable` first among known kinds. The arity is wrong, not the spelling:
  the bare-form branch validates against `knownFactKinds`
  (`Concrete/Report/Report.lean:4430`) while the message renders both lists
  (`:4383`). Split the two cases, and give `traceability` — currently a fact
  filter AND a separate backend path under one name — either one meaning or two
  names. The gate is class-level: every member of both kind lists, in bare and
  argument form, asserting the message never lists the rejected token as known.
  That catches the next kind added to one list and not the other, which is the
  mechanism that produced this bug.
- **Retired syntax gets a generic parse error.**
  `tests/invalid_programs/missing_variant.con` uses the DELETED `#` enum
  qualifier and gets `E0001 expected ;, got #`, with no statement that the
  separator was retired or that `Option::<i32>::Some` replaces it. This is the
  error a model trained on older material will hit most. The class gate is a
  corpus grep for fixtures exercising retired surface forms — the class size is
  unmeasured, so measure it before fixing instances, and the fixture has also
  drifted from its own name.
- **Populate `hint` where the candidate set is already in hand.** `E0100`
  prints no candidates although the resolver holds the in-scope set; the
  field-not-found and missing-variant codes likewise know the legal set they
  choose from. The field is already wired end to end and reaches
  `--diagnostics-json`, so this is population, not plumbing. Gate that every code
  reachable from the corpus carries a non-empty next action, with a mutation
  blanking one hint.

### Task R-0009

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The old reducer defect retires, while the body defines reusable crash-predicate correctness requirements. surviving requirement: Provide crash reduction only with isolated bounded execution, preserved failure class, final-predicate validation and healthy-program, timeout and self-failure controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; remove unusable predicate until implemented.

**Objective:** Fix bug 049 — vacuous `reduce --predicate crash` Remove the predicate from help/CLI immediately unless the same slice evaluates candidates in an isolated subprocess with bounded time/memory/output and preserves the original crash boundary/class. Parse success is never a crash predicate.

    The reducer must verify the final candidate still satisfies the predicate,
    report stage/error-class drift, and say explicitly when a multi-module
    input has no supported reductions. Gate a healthy program (must not reduce
    as crash), signal/tool/compiler/runtime crash classes, timeout, changed
    failure class, empty candidate, and reducer self-failure.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The old evidence migration and PExpr callable prescription retire, while the body’s multidimensional assurance requirement applies to initial claims. surviving requirement: Represent claim scope, method, validity, binding, dependencies and policy independently, retaining typed digest separation and controls against scope inflation or trust laundering. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Split immediate verdict honesty from later package-policy productization.

**Objective:** Make evidence multidimensional instead of a ladder, and record
compiler trust per claim.

**Decision status:** ratified 2026-07-25; implementation pending. A ladder may
remain as a policy preference among claims with the same subject and semantic
scope, but it is not a universal ordering of unlike evidence.

`docs/verification/EVIDENCE_CLASSES.md` and `docs/verification/CLAIM_TAXONOMY.md` mix categories that are
not comparable. `proved`, `tested`, `enforced`, and `trusted` are evidence
*methods*; `stale`, `partial`, `missing`, and `counterexample` are *statuses*;
`reported` is an observation, not evidence at all; `runtime_checked` is a
disposition; and source / Core / SSA / native / target are *scopes*. Ranking them
on one axis produces false comparisons: a native differential-oracle test covers
the backend and runtime, while a ProofCore theorem reasons more strongly over a
narrower semantic layer. Neither is "below" the other — they are incomparable
evidence about different scopes, and the current model cannot say so.

Represent a claim as orthogonal fields — subject binding, claim kind, semantic
scope, evidence method, status, assumptions, producer, validated_by,
trusted_dependencies, freshness, replay receipt — and let the CLI keep rendering
friendly composites like `proved_by_lean`. The scope axis is the one genuinely
missing today, and it is the axis where the compiler already has the facts.

The normative target is now
`docs/verification/EVIDENCE_ARCHITECTURE.md`: five objects—`SourceCorrespondence`,
`LogicalValidity`, `DependencyClosure`, immutable `ReplayReceipt`, and
time-relative `PolicyDecision`. Implement them as a shared envelope plus typed
variants and authority-state constructors, not one record of optional fields.
Reports and package formats are projections of these objects.

Replace interchangeable digests at authoritative boundaries with
domain-separated types (`SubjectDigest`, `ImplementationDigest`,
`ContractDigest`, `PropositionDigest`, `TheoremArtifactDigest`,
`CertificateDigest`, `DependencyRoot`, and `ReceiptDigest`). Gate that swapping
two well-formed digest families fails at compile time. Migration inventories
every raw `String`/tuple boundary, runs old/new structured facts in shadow, and
deletes the old path after corpus conversion.

Advisory disposition is deliberately **not** a replay-receipt field. A receipt
records the checker/rule-set/artifact/environment that was replayed; current
acceptance is computed later from that receipt, the current advisory registry,
authentication, and consumer policy. A retained policy-decision attestation
binds its policy and advisory-registry identities separately.

Move evidence honesty to authorship as the language surface matures:
`#[certificate_by(...)]`, `#[proof_object_by(...)]`,
`#[trusted_proof_by(...)]`, and `#[assume(...)]` select distinct variants.
Existing `#[proof_by]` migrates conservatively and cannot silently inherit the
strongest class. Gate each mode through the production report, receipt, package,
and policy consumers; syntax alone earns no authority.

Trust is three fields, not one, because "who emitted the artifact", "who checked
it", and "what must be trusted for the claim to hold" are different questions.
The compiler may *produce* a Lean proof term that the kernel independently
replays: the producer need not be trusted for proof-term validity, while the
compiler is still trusted for the source-to-ProofCore correspondence. Collapsing
those into one `producer_trust` would lose exactly the distinction that makes
independent checking worth building.

What must stop being elided is that "compiler-enforced" reads as trust-free and
is not: until checker soundness and artifact production are independently
verified, enforcement depends on this compiler being correct. Being implemented in
Lean does not remove that dependency; mutation testing and duplicate boundary
checks are strong engineering evidence, not a soundness proof.
`docs/compiler/ARCHITECTURE.md` already admits `ValidatedCore` does not remove the
compiler from the TCB and `docs/verification/TRUSTED_COMPUTING_BASE.md` holds the accounting —
the change is that the dependency appears per claim.

Migration scope is larger than the two evidence documents. Residual total-ladder
language elsewhere in this file recreates the ordering this task rejects —
`tested_by_property` described as "always below proof and below solver evidence",
and a cross-checked solver result as "stronger than" another method. A
policy-specific partial order is fine, but it may only compare claims of
equivalent scope and subject; rewrite those sites as part of this task rather than
leaving a third vocabulary.

Gate: a fixture set covering one claim per (scope × method) cell that exists
today, a negative fixture proving two incomparable claims cannot be ordered by
the renderer, a fixture proving a claim without the producer/validator/trusted-
dependency accounting required by its method is rejected, and a red-team case
proving no rendering path can present a weaker-scope claim under a stronger
composite name. Reconcile both documents against the implemented model rather
than leaving a third vocabulary.

Also gate graceful degradation: revoking one checker changes logical validity
and policy acceptance while leaving independently established source binding
and dependency closure intact. A renderer that collapses those facts into one
stored `proved` field must fail.

ProofCore callable identity is pulled forward as R-0442 in the global sequence.
Every proof-status-area dependency/completeness task consumes that direct-call versus
callable-value distinction; none may recover callable identity from a source
spelling or reintroduce the old ambiguous `PExpr.call` representation.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The body combines old constructor inventories and interpreter repairs with enduring coverage and claim-integrity duties. surviving requirement: Generate construct coverage and validate structured documentation claims against live gates, retaining missing-row and stale-claim negative controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Merge enforcement rows into R-0498; share inventory with R-0118/V7.

**Objective:** Generate the semantically-dark-construct inventory from compiler
constructors, and make the docs-drift gate check claims rather than paths.
Also expand the gate's coverage: `PRESENT_DOCS` currently lists five files,
while `docs/NOTES/` and `research/` accumulate claim-bearing design notes
outside any drift check — bring both under the path check at minimum.
(2026-10-29: `docs/NOTES/` no longer exists; its notes moved to `research/proof-evidence/`,
which stays outside the gate by the convention in `research/README.md`. What remains here is
the claim check over gated `docs/`.)

`docs/verification/PROOF_STORY_MATRIX.md` is what `docs/README.md` calls "the 'no semantically
dark constructs' inventory": every construct must be `proved`, `enforced`,
`reported`, `assumed`, or `trusted`. It has 30 hand-written rows, and grep counts
zero occurrences of `defer`, `newtype`, `function pointer`, `monomorph`,
`indirect`, `generic`, `import`, and `module`.

Line that up against the defect corpus. Bug 050 was indirect calls through
function pointers; 051 was generic-enum monomorphization; 054 is monomorphized
name collision; 044 was renamed generic imports; 055 is sibling import aliases;
039 and 045 were import-alias and match-binder identity. Every one lived in a row
the inventory does not have. The constructs the language added on top of the
proved fragment — generics, indirect dispatch, module/import identity — are
exactly the ones never enumerated. A hand-maintained inventory of a growing
language lags, and this one lagged by seven constructs and seven bugs.

**Immediate stopgap, before generation lands:** add explicit hand-maintained
rows for `defer`, newtypes, generics, monomorphization, function-pointer values,
indirect calls, modules, and imports/aliases. Each row names current status,
evidence, gap, and roadmap owner. Mark the block as temporary so it is replaced,
not duplicated, when constructor-derived generation lands.

Two mechanisms, deliberately separate — they share a motivation but not an
implementation, and merging them would produce a gate that greps English prose.

**Mechanism 1 — constructor coverage, generated.** Derive the row set from
constructors the way the backend capability matrix is keyed on
`SInst`/`STerm`/`Ty`, so adding a construct fails the gate until its story,
evidence, and named gap are declared. AST and Core constructors alone are not
enough: imports, modules, manifests, and target/module selection live in the
resolve and project models, and those are precisely the identity-bearing surfaces
the missing rows cover. Take the inventory from all three.

**Mechanism 2 — structured claim records, not prose analysis.** A gate cannot
infer from English whether a normative sentence is still true. Give each
claim-bearing statement a record — claim id, `current` or `planned`, owning gate,
fixture, the compiler fact it rests on, and the commit where it was last verified
— and have the docs gate check the records and their backing, not the sentences.
`planned` is what makes the "no aspirational behavior in the present tense" rule
mechanical rather than stylistic.

Second leg motivation: extend
`scripts/tests/check_docs_drift.sh` past path resolution to claim checking. It is
91 lines and 3 checks (paths resolve, `--report` kinds exist, roadmap IDs are
well-formed) over 126 docs of which 106 assert a `Status:`. Nothing checks
whether a normative claim is still true, which is why in one session
`LANGUAGE_INVARIANTS.md` #4 asserted a rule the checker does not enforce,
`KNOWN_HOLES.md`'s OPEN section read empty while seven defects were open,
`LANGUAGE_GAPS.md`'s two "remaining findings" were both already shipped,
`DEFER.md` called move-after-defer "not yet specified or gated" when it is
diagnosed as E0206, `LANGUAGE_SHAPE.md:47` contradicted
`OWNERSHIP_MODEL.md:92-95` on linear rebind, and `FAILURE_STRATEGY.md:74` said
"there are no destructors" while `impl Destroy` and generated drop glue ship. A
`Status: stable reference` doc must cite the gate or fixture backing its
normative claims, and aspirational behavior may not appear in the present tense
beside a current guarantee.

The one-time truth repair is part of this task's migration input, not another
roadmap task. It aligns the canonical arithmetic/bounds and proof-eligibility
claims; rewrites `INTERPRETER_TRUST.md` around a second semantics tested for
divergence rather than a fictional tiny width-blind oracle; narrows
`KNOWN_HOLES.md` to an index over the numbered corpus; and corrects stale
`DEFER.md`, `LANGUAGE_GAPS.md`, `IDENTITY.md` (now the identity section of `PROJECT.md`), error-convention, and anti-feature
statements. The repair may land as documentation commits before the generated
claim records exist; R-0438 owns preventing recurrence. The same repair removes
stale hand-counted theorem totals, replaces “empty capability set means pure”
with the narrower “authority-free” claim, and makes proof-boundary documents
use R-0440’s orthogonal evidence dimensions instead of assigning every fact to
one total tier. Stable documents link to generated proof coverage rather than
freezing a count that changes when a theorem lands.

Consume R-0447's repository ownership manifest rather than inventing a second
file registry. Constructor/claim records may point to test-case manifests,
example lifecycle records, normative-document owners, and generated artifacts;
those references must resolve through the shared ownership map. R-0438
generates semantic and claim coverage, while R-0447 classifies where the
supporting artifacts belong—the two schemas join by stable IDs and neither
copies the other's facts.

An `owning_gate` in a claim record is one of R-0447's stable gate IDs, not a
workflow filename or extracted shell fragment. Claim validation consumes the
registered verdict and liveness record for that ID. This keeps the claims gate
from blessing an assertion because a permanently failing command extractor was
mistaken for a compiler/test failure.

Gate: the generated matrix fails on a newly added constructor with no declared
story, in each of the AST, Core, resolve, and project inventories; the claim gate
fails on each of the six stale claims above, reproduced as records whose backing
no longer holds; a `planned` record asserted in the present tense fails; and a
mutation removing a row or a claim record proves both go red.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The Main.lean consolidation is legacy-specific, but its operational and inherited release/test controls survive. surviving requirement: Use a shared native compilation driver with structured diagnostics, retained artifacts, bounded tools and failure-injection controls at every boundary. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep the shared prepare/emit/verify/assemble/link driver; the LCB-native profile (R-0517) is its forcing consumer.

**Objective:** Consolidate the duplicated file/build/test compilation paths in `Main.lean` behind one backend-neutral driver:

 `prepare -> emit -> verify -> assemble/codegen -> link`. Backend-specific
 code supplies artifact extension, verifier/tool invocation, target flags,
 and link inputs; CLI code owns diagnostics, artifact retention, and process
 policy once. Add failure-injection tests at every boundary so a QBE error
 cannot take a less structured path than an LLVM error.

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

**Restructured (2026-11-02):** retired with the legacy compiler: Old/new derivation cutover retires, while canonical facts and their integrity requirements remain. surviving requirement: Derive immutable indexed canonical program facts with typed keys, explicit invalidation and duplicate, missing-key and conflicting-consumer controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep canonical facts; share indexes with R-0140.

**Objective:** Build one immutable indexed ProgramFacts store Derive module/definition indexes, signatures, layouts, builtin identities, aliases, target/runtime facts, call relationships, and source/semantic identity once per program. Passes query typed keys rather than rescanning lists or building private first-match tables. Facts are immutable within a compilation;

transformations produce a new version with an explicit invalidation boundary.

Acceptance: compare old and new derivations over the corpus before cutover,
fail on duplicate/missing keys, record deterministic hashes and construction
cost, demonstrate linear-time builders, and gate that two consumers cannot
derive conflicting values for the same fact.

### Task R-0116

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The legacy stage-wrapper list retires, while type-safe pass boundaries survive in the new IR chain. surviving requirement: Require payload-bound typed pass results whose constructors establish the invariants needed by subsequent consumers. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0497/R-0510/R-0514: merge; use payload binding and new IR chain.

**Objective:** Make successful compiler pass boundaries type-safe Introduce thin opaque results such as `ParsedProgram`, `ResolvedProgram`, `CheckedProgram`, `ElaboratedCore`, `CanonicalCore`, `MonomorphizedCore`, `LoweredSSA`, `ValidatedSSA`, `CleanedSSA`, and `CodegenInput`. Constructors live with the pass that earns each invariant; reports, backends, tests, and projects cannot call a later consumer with an earlier-stage value.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0116).

### Task R-0118

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The named legacy passes retire, but self-enforcing coverage and inherited differential controls survive. surviving requirement: Generate exhaustive new-IR pass and consumer coverage with implementation/test links and missing-row or synthetic-constructor negative controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep one inventory; integrate R-0438/R-0080 and V7 coverage.

**Objective:** Generate a self-enforcing IR/pass/consumer coverage matrix Generate, from compiler constructors and interface facts, the matrix spanning Check, CoreCheck, Interp, Lower, Verify, Cleanup, LLVM, QBE, reports, source identity, obligations/proofs, and tests. Every constructor is explicitly classified as implemented, intentionally no-op, rejected-before-here, runtime-helper, pending, or not-applicable; catch-all matches do not count.


Acceptance: adding a synthetic constructor or deleting any consumer row makes
the gate fail; every row links to its implementation and discriminating test;
the matrix is published through debug/agent artifacts; and the former manual
inventories are deleted or reduced to generated snapshots.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The evidence requirement survives while the old wrapping_add-to-addw coverage snapshot does not. surviving requirement: Arithmetic-site facts must distinguish checked, wrapping and saturating semantics across new-compiler proofs and outputs, with profile-invariance and confusion-rejection controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; replace obsolete operator-coverage snapshot.

**Objective:** Carry arithmetic-site facts into diagnostics, reports, assumptions, proof obligations, release bundles, and backend contracts. A theorem about checked arithmetic is not a theorem about modular arithmetic; a function using `wrapping_*` is either proved against the proof model's explicit modular operator (today only `wrapping_add@u32 → addw 32`, as SHA-256 uses) or, for the not-yet-modeled forms (`wrapping_sub`/`wrapping_mul`, other widths, saturating), outside the default proof subset. Add `docs/ARITHMETIC_SITE_EVIDENCE.md`, `examples/arithmetic_site_evidence/{checked,wrapping,saturating,profile_invariant}/`,

and `scripts/tests/check_arithmetic_site_evidence.sh`; the gate must prove
profile-invariance (the same expression has the same semantics under
different build profiles), explicit classification of every arithmetic site,
and rejection/downgrade of proof claims that confuse checked and wrapping
semantics.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The body combines a dead legacy reference-return lowering repair with explicit surviving test obligations. surviving requirement: Retain differential generation, trap-identity checks, nightly minimization, alpha and hostile-spelling controls, historical witnesses and accepted raw-pointer-return regressions as new-compiler slices admit them. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Merge shared harness into R-0491; split obsolete reference-return repair.

**Objective:** [historical origin: old phase 4 items 44f/44g] Finish the remaining compiler-correctness hardening:

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
 - repair the dead-path ref-return lowering case where a reference-typed
   return materialized from a ref identifier or `&place` emits a spurious
   extra load. This path is unreachable from source while reference returns
   are rejected, so its regression fixtures must separately preserve the
   accepted trusted raw-pointer (`*const`/`*mut`) return cases;
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

**Restructured (2026-11-02):** retired with the legacy compiler: The body mixes essential version-drift control with legacy emitter references and a deferred bitcode option. surviving requirement: Pin the new compiler's supported LLVM toolchain and require wrong-code, contract and output controls on upgrades, retaining bitcode emission as conditional work. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; connect candidate identity to R-0357.

**Objective:** Pin the supported LLVM/clang version and gate version drift. The backend today targets whatever `clang` defaults to on the host (`docs/platform/EXECUTION_MODEL.md`), yet the TCB already conditions backend correctness on "LLVM version" — so a floating toolchain leaves the trusted boundary a moving, un-audited target and makes native output non-reproducible across hosts (`docs/compiler/DETERMINISM.md` already disclaims binary reproducibility for exactly this reason). Declare one supported LLVM version (or a narrow tested range) as part of the backend contract, and make an upgrade a deliberate, gated event: on a version bump the wrong-code corpus, the backend-contract gate, and IR golden tests must re-pass before the new version is

 declared supported. This is the cheap control that turns the moving boundary into
 a fixed one; the structured `BackendIR` contract (Task R-0288) and a future hand-written
 LLVM bitcode emitter (the Zig/Roc decoupling technique) are the heavier long-term
 options, pulled only if version pinning proves insufficient. Wire the check into
 `scripts/tests/check_backend_contracts.sh`: the compiler refuses or warns on an
 unsupported LLVM version, and the drift corpus re-runs on a bump.
 - 20a. LLVM bitcode emitter — the heavy decoupling rung (pull-gated, deliberately
   NOT its own phase). *What it is:* emit LLVM **bitcode** (LLVM IR's stable
   serialized binary format) from a hand-written serializer that never links or
   calls the churning LLVM C++ API. Zig's self-hosted compiler is the reference
   implementation — it writes bitcode in Zig and hands it to whatever LLVM is
   present, making it immune to the C++-API breakage that costs most frontends an
   upgrade tax; Roc reused Zig's bitcode writer wholesale in its 2026 Rust→Zig
   rewrite. Concrete today emits *textual* LLVM IR to `clang`
   (`Concrete/Backend/EmitLLVM.lean`), which already avoids C++-API linkage but
   stays coupled to textual-IR *syntax* drift across LLVM versions (the
   opaque-pointer migration was one such event). A bitcode writer would remove
   that coupling and enable `clang`-free, bit-identical cross-host emission.
   *Why an item and not a phase, and why deferred:* (1) it is a sub-component of the
   backend contract that gates nothing downstream; (2) it is *less* proof-relevant
   than translation validation (Task R-0299) — it buys decoupling/reproducibility,
   not correctness — so elevating it above Task R-0299 would invert priorities for
   a proof-carrying compiler; (3) the cheap version pin (Task R-0302) already fixes the moving
   trusted boundary, and LLVM churn has not been shown intolerable (one migration
   eaten so far), so a phase-sized commitment now would be speculative scheduling.
   *Pull-trigger* — promote to scheduled work when EITHER a freestanding / no-`clang`
   target is pursued (the freestanding area), where self-contained emission stops being optional,
   OR LLVM version churn recurs intolerably despite Task R-0302's pin. Same ladder as
   `BackendIR` (Task R-0288): `ValidatedSSA → BackendIR → ValidatedBackendIR → EmitLLVM`
   today; bitcode / own-backend later. Prior art: Zig (hand-written bitcode writer,
   self-hosted backend), Roc (reused it in the Zig rewrite).

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

**Restructured (2026-11-02):** retired with the legacy compiler: The obligation survives, but its Concrete SSA, old backend IR and QBE ownership map is obsolete. surviving requirement: Define each new-compiler optimization's owner, semantic preconditions, identity policy, validation controls and performance witness without changing observable behavior or evidence. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; absorb R-0280 and require S12 certificates where proved.

**Objective:** Define the optimization ownership and evidence-preservation
contract across Concrete SSA, backend IR, LLVM, and QBE.

Every optimization has a named owner, preconditions, affected operations and
failure semantics, debug/source-identity policy, verifier before/after,
interpreter/differential/mutation coverage, and performance witness. Prefer
target-neutral canonicalization/legalization only when semantics require it;
keep target instruction selection backend-specific. Do not add a pass because
another compiler has it. Optimization levels select documented pass sets and
must not change observable checked arithmetic, traps, capabilities, ownership,
or evidence status.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0280** (merged): Duplicate of R-0421: merge into R-0421; connect certificate obligations to R-0504.

### Task R-0422

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The body mixes legacy quadratic-emitter repairs with lasting class-level scaling controls. surviving requirement: Use profiling and adversarial scaling inputs to prevent superlinear compiler work in the new pipeline with explicit complexity and memory budgets. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; do not equate throughput with program speed.

**Objective:** Close measured compiler and emitter performance defects under one
owned queue, beginning with bug 027's quadratic emission class.

Profile before refactoring; retain adversarial scaling inputs; measure time and
peak memory across increasing program sizes; require an explicit expected
complexity envelope; and gate the class rather than one file. Typed emitter
fragments may be adopted when the profile pulls them, but QBE's fragment model
does not by itself prove LLVM emission fixed. Keep compiler-throughput facts
separate from generated-program runtime facts in reports and release claims. The class's scaling
gate is R-0550 (Phase 1 exit 2o), and its continuous profiling is R-0551.

### Task R-0550

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The body combines repairs of frozen EmitSSA, Lower and frontend defects with a reusable load-bearing scaling gate. surviving requirement: Retain the scaling corpus and controlled complexity, instruction, allocation and host-normalized timing gates for new-compiler stages as their constructs land. This line governs over older implementation directions for the legacy compiler.

**Objective:** A compile-time scaling gate for structural class 17 (pass state in association lists
and per-query recomputation), owned with R-0422, which repairs the class. It closes Phase 1 exit 2o.

It enters the queue under the entry rule with these reproducers: bug 027 (EmitSSA's quadratic
rendering), bugs 248, 249 and 250, and the long-function front-end slowdown (forty thousand
statements: 8.5 s parse, 13.4 s resolve, 9.5 s check and 9.5 s elab; being filed). leanprof profiles
add two hot spots in an `examples/lox --emit-llvm` build: EmitSSA's scan for generic arguments (13.6%
of the build) and a linear search of the pointer-register list (6.8%).

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

The gate is in the full gate set (2026-10-26), with budgets measured on concrete-ci-2 and recorded
in `scripts/tests/lib/compile_scaling_budgets.tsv`. Building it found ten more superlinear stages
over declaration count and nesting depth (bugs 273 – 282, open); they and Lower's loop header phis
(bug 270; bugs 265 – 267 are fixed and their shapes judged) are named exclusions the gate reports on
every run. What remains for the exit: each fix removes its exclusion, and the declaration and
nesting shapes move to 10^4 – 10^5 elements.

### Task R-0551

**Phase:** B

**Restructured (2026-11-02):** retired with the legacy compiler: The objective's profiling process survives, but the named legacy workloads and compiler stages must be replaced. surviving requirement: Continuously profile supported new-compiler workloads at exact landed SHAs and prove hotspot detection with a deliberate scaling regression. This line governs over older implementation directions for the legacy compiler.

**Objective:** Continuous compiler profiling with leanprof, the project owner's Lean profiler, owned
by R-0422 with the performance track.

A scheduled job on the CI server (concrete-ci-2) runs leanprof on fixed compiler workloads at every
commit that lands on main: `examples/lox --emit-llvm`, R-0550's scaling corpus and the full std
build. It publishes the top hot spots and the time per compiler stage, compares them with the
previous landed commit, and opens a tracked finding when a hot spot grows, or a new one appears,
above a recorded threshold. It complements the instruction-count tracker and its alarm of about 1.5%
([PERF_TRACKING.md](PERF_TRACKING.md)) and does not replace it: the
alarm says that a commit got slower, and the profile says where. Server provisioning lives in the
owner's private dotfiles (`servers/`); this repository holds only the portable scripts.
**Exit:** for ten consecutive landed commits the job has published a profile and a comparison at
each commit's SHA, and a control that adds a known hot spot (a deliberate quadratic loop in one
stage) opens a finding.

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

**Restructured (2026-11-02):** retired with the legacy compiler: The performance obligations survive, but deadlines tied to old structural migrations do not. surviving requirement: Establish exact-SHA development-speed baselines for the new compiler and grow release budgets to cover compilation, proofs, reports and independent verification with regression controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; consume performance ledger and add proof scalability metrics. Before large R-0512 migrations, budgets include proof scalability: elaboration/recheck memory, proof-term size, artifact size and validator scaling.

**Objective:** Add release performance budgets:

 `scripts/tests/check_release_performance.sh` must measure compiler startup,
 cold small-project/stdlib builds, the incremental-driver area warm no-op, private-leaf and
 public-interface edits, proof-query/object-cache hits, relink latency,
 `concrete test`, audit/report generation, proof-check latency, and
 independent bundle-verification latency against
 `release/perf-baseline.json`. Done when
 release CI blocks unexplained regressions and prints the regressed command.

**Development-speed baseline (2026-10-21; checkpoint of queue row 4, precedes its structural
changes).** Before the first whole-language change of R-0492, R-0499/R-0514, R-0502 or R-0501 lands on
main, record a baseline over a small named representative corpus (the compiler, `std`, one large
example and one multi-module program, listed in `release/dev-speed-corpus.txt` (new)): clean build
time of the compiler and of each corpus program, rebuild time after a one-function edit in the
compiler and in a Concrete program, peak compiler memory on each corpus program, and the fast push
tier's turnaround (R-0513). Each measurement is recorded per main commit with its SHA, host class and
toolchain, and has a budget with a stated noise tolerance; `scripts/tests/check_dev_speed.sh` (new)
fails a commit that exceeds a budget beyond its tolerance, and a budget is raised only by a commit that
states the reason. **Checkpoint exit:** the baseline exists at a named main commit before that first
structural change lands; a deliberately injected regression beyond tolerance fails the gate; the
recorded series shows each structural change's effect. The release budgets above remain this task's
exit; this checkpoint is their first, smaller corpus.

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

**Restructured (2026-11-02):** retired with the legacy compiler: IntArith and EmitSSA are legacy machinery, but generated coverage and consumer-integrity gates remain required. surviving requirement: Generate the new Core operation and trap inventory for backend-neutral consumers, fuzzing and capability reporting, with controls proving automatic constructor coverage and actual consumer use. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; misplaced in the research phase — its generated trap/fuzzer/consumer coverage is an increment of R-0491/R-0493.

**Objective:** Generate the operation-semantics/trap inventory rather than
hand-maintaining it, and extend its consumer set.

R-0005 removed the recurrence mechanism for bug 053: `IntArith` now answers
whether and how an operation traps, for the unary family as it already did for
the binary one, and folding, DCE, the interpreter and EmitSSA read that one
answer (`scripts/tests/check_trap_inventory.sh`, mutations #25-#27). Any retained
backend consumes the shared semantic inventory through backend-neutral
interfaces; its emitter-independence requirements remain in force.

What R-0005 asked for and did not build is the *generated* form. Two consumers
remain outside the inventory entirely: **fuzz generation**, which would emit
trapping cases per constructor and is the thing that would surface the next
trapping operation without an audit, and the **capability matrix**. A
hand-written inventory still requires someone to remember to extend it when a
constructor is added; a generated one does not, and generation is what makes
the consumer list checkable rather than conventional.

Gate: a new trapping constructor added to the inventory must appear in
generated fuzz cases without editing the generator, and every declared consumer
must be shown to read the generated artifact rather than a local copy.

---

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
