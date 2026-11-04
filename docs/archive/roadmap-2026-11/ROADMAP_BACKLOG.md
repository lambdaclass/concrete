# Concrete Roadmap Backlog

This file holds the roadmap tasks that no active or next phase schedules. [ROADMAP.md](../../ROADMAP.md)
keeps the phases and the execution queue, and the [task register](ROADMAP_TASKS.md) every task a queue
row names or whose **Phase:** line includes an active or next phase (in ROADMAP.md itself until
2026-10-28); every other `### Task R-NNNN` lives here, with its full text, stable ID and
**Phase:** line unchanged. A task in this file is not abandoned and not lower quality: it waits for its
phase or its trigger. Task IDs remain immutable across the move, so a reference such as `R-0305` means
the same task in either file.

The tasks were split out of ROADMAP.md on 2026-10-19. They are filed under the same topical
**area** headings as in the task register (an area's prose and its scheduled tasks stay there; an area with
no scheduled task, such as WebAssembly or the freestanding target, moved here whole). Stubs of
superseded, merged, duplicate, retired and delegated tasks whose **Phase:** line names no active or
next phase are here too; their archived bodies stay in
[ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md) and their dispositions in the
[reconciliation record](ROADMAP_RECONCILIATION.md).

## Pull rules

Every task here carries a **Pull trigger:** line, written from the task's own text: the specific
workload, defect, user need or prerequisite that makes the task worth doing now. A task whose
**Disposition** retires or merges it has no trigger of its own; its line names the owner that carries its
obligations.

1. **Pull triggers.** A task moves into the task register when any of these happens:
   - a workload, bug report, pilot or user need hits it (its **Pull trigger:** names which ones);
   - a phase that depends on it becomes active or next, or a queue row needs it; or
   - a release checkpoint in the [release section](../../ROADMAP.md#releases) needs a bounded
     increment of it.

   Pulling moves the whole task, unchanged, into its area in the task register, in the same change that
   updates its **Phase:** line or the queue row that needs it. The **Pull trigger:** line may be dropped
   once the task lives in the task register.
2. **Phase entry checkpoint.** When a phase becomes active (the **Active phases:** line in
   ROADMAP.md changes), every task in this file is reviewed. Each is either pulled into the task register,
   kept here with a reason, or retired to the archive with a reason recorded in the reconciliation
   record. A task kept here although its **Phase:** line names an active or next phase carries a
   **Kept in backlog:** line giving the reason. The review is recorded in the
   [review log](#review-log).
3. **Per-release review.** Before each release candidate (R-0333's candidate record), the backlog is
   reviewed the same way, with attention to tasks whose triggers the release's pilots, workloads or
   bug reports have hit. The review is recorded in the review log.
4. **Placement is gated.** `scripts/tests/check_roadmap_coverage.sh` checks that every task lives in
   exactly one of the task register and this file; that every task here has a **Phase:** or disposition
   line and a **Pull trigger:** line, with no two live tasks sharing the same trigger text; that
   ROADMAP.md names its **Active phases:** and **Next phases:**; that no task here is scheduled in
   one of those phases without a **Kept in backlog:** reason; that no task in the task register is scheduled
   only in later phases unless a queue row names it; and that the latest review-log row names the
   current active phases.

### Review log

| date | occasion | active phases | outcome |
|---|---|---|---|
| 2026-10-19 | split from ROADMAP.md during Phase 1 | 1 | 154 tasks moved here: each is scheduled only in Phases 4 – 7 or after 1.0/research (or, for a retired stub, in no phase); none is named by a queue row |
| 2026-11-02 | phase A entry: reviewed all 546 classifications | A | A, B and T tasks pulled; all others kept for their new-compiler phase or trigger, or retired with legacy-compiler-final; immutable classification record applied in full |

## Area: Verified-Compiler Pipeline, Stabilization And Usability

These tasks carry the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md)
(R-0491 – R-0512), the gate and shared-front-end work it depends on (R-0513, R-0514), the usability
and release-automation tracks (R-0515, R-0516), the Lean Compiler Backend dependency (R-0517) and the
open work promoted from roadmap prose by the 2026-10-19 reconciliation. They were filed under the backend-contracts area
until 2026-10-19; their queue rows, not this position, schedule them.

### Task R-0494

**Phase:** F

**Pull trigger:** R-0494: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The body mixes deletion of old report authorities with exact verdict semantics and permanent consumer tests. surviving requirement: Derive every rendered claim and clause verdict from one checked judgment binding proposition, subject, coverage, dependencies and environment, with cross-surface and hostile dependency controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; row 6 extends it per slice. Hypothesis dependencies: cycles (O1 needs O2 needs O1), self-support and missing references are rejected or handled by a stated well-founded/SCC rule, with hostile cycle fixtures (with R-0460). The structured subject-facts schema replaces the scraped human report here (with R-0481).

**Objective:** One verdict authority — stage S2 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned; starts now.**

`Verdict` with a private discharging constructor;
`Verdict.decide` consumes checked evidence binding proposition, clause, subject, coverage,
dependency closure and environment; one `ClaimJudgment` per claim and contract clause; receipt
decoding and currency checks feed it; contracts, vcs, obligation-ledger, audit, check-proofs (text
and JSON), proof-status and issuance render it. Delete `Proof.provedFunctions` and name-substring
replay checks; split `Report.lean` (6,405 lines) and `Main.lean` (4,194) by surface.

**Exit:** status spellings only in the renderer table; 089's reproducers read not-proved
everywhere; a cross-surface gate pins the expected positive and negative judgment for every claim
and clause.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0171** (superseded): superseded by R-0494: merge into R-0494; preserve route-specific attacks. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0171)
- **R-0194** (merged): duplicate of R-0209: merge into R-0209.
- **R-0204** (superseded): superseded by R-0494: merge into R-0494.
- **R-0205** (superseded): superseded by R-0494: merge into R-0494’s evidence-class controls.
- **R-0209** (superseded): superseded by R-0494: merge into R-0494; retain audit/diff tests.
- **R-0218** (superseded): superseded by R-0494: merge into R-0494.
- **R-0264** (superseded): superseded by R-0494/R-0509: merge implementation into R-0494, soundness proof into S16.
- **R-0465** (done): done for its enumerated wiring; the remaining universal-verdict-authority requirement merges into R-0494. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0465)

### Task R-0495

**Phase:** E

**Pull trigger:** R-0495: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: PExpr porting and legacy issuance conditions are superseded, but the body explicitly requires exact generated statements and substitution controls. surviving requirement: Generate exact claims against new Core semantics and reject wrong propositions, narrowed domains, wrong coverage and substituted loops through issuance, consumption and independent replay. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; coverage and loop binding are essential exits.

**Objective:** Claims on Core — stage S9 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned.**

Depends on S2, S7, S8. Typed `ClaimSpec`; statements generated against
Core's semantics on the exact checked term; ProofKit's symbolic-evaluation simp set; existing
`PExpr` theorems ported (through an explicitly proved bridge where cheaper), keeping weaker coverage
where that is what was proved; Core-proved and binary-transported status remain distinct. Legacy
issuance and replay retirement follow R-0512's inventory, compatibility and deprecation conditions.

R-0495 checks coverage relation and domain against the exact generated proposition; author
annotations confer no authority. Loop claims bind the exact checked Core loop and its
initialization, preservation and exit obligations; missing binding remains `spec_unbound`.
**Exit for each migration cohort:** every migrated positive claim replays against its exact generated
statement; negative fixtures refuse; wrong-coverage, narrowed-domain and substituted-loop controls are
refused through status, issuance, consumption and independent replay. Unmigrated supported claims
retain their declared maintenance and replay path. Legacy issuance code is removed only under R-0512's
final retirement conditions; historical replay remains available.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0123** (superseded): superseded by R-0495/R-0512: retire new legacy-model semantics; add admitted Core features/proof lemmas instead.
- **R-0176** (superseded): superseded by R-0495/R-0512: retire proposed legacy architecture; retain ownership goals in R-0151. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0176)
- **R-0202** (superseded): superseded by R-0495/R-0512: merge dependency-closure requirement into Core migration; maintain legacy checks until retirement.
- **R-0229** (superseded): superseded by R-0495 and V2–V4: merge into slice admission/claim coverage.
- **R-0235** (superseded): superseded by R-0496/R-0495: retire PVal/PBinOp expansion; implement Core float semantics. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0235)
- **R-0262** (superseded): superseded by R-0495/R-0512: retire blanket legacy bridge; retain only migration-pulled bridges. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0262)
- **R-0265** (superseded): superseded by R-0495/R-0509: merge into statement/binding/aggregation soundness.
- **R-0267** (superseded): superseded by R-0499/R-0507/R-0495: merge; capabilities and purity remain distinct.
- **R-0269** (superseded): superseded by R-0495: merge composition needs into R-0164; retire PExpr table architecture.
- **R-0464** (superseded): superseded by R-0493/R-0495/R-0460: merge; archive completed H24 fix and retire SSA-first prescription. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0464)

### Task R-0505

**Phase:** E

**Pull trigger:** R-0505: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Re-pointed 2026-10-19: integrate the Lean Compiler Backend (R-0517); no Concrete machine backend. Absorbs R-0297's scheduling.

**Objective:** Native AArch64 profile: LCB integration, linking and image — stage S13 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design. Concrete integrates the Lean Compiler Backend (R-0517) and builds no machine backend of its own.

**Status (2026-10-19): planned; re-pointed from an owned Concrete machine backend to LCB integration.**

For the full first profile (AArch64, Apple ARM64 ABI, macOS, Mach-O): LCB supplies machine IR,
instruction selection, frame construction, register allocation, assembly, encoding and their proofs
(LCB's C01 – C03 and V01 – V05, external dependencies under R-0517). Concrete owns the profile around
it: the BIR → LCB-IR adapter (R-0517); **separate compilation and linking** — module interfaces,
compatible linking, symbol resolution and imported-contract satisfaction, source-module linking
related to independently compiled object linking, and compiler, profile, LCB version, interface,
runtime and dependency identities bound into the image; the `rt` module and startup; FFI ABI
realization at the adapter boundary (callbacks, external memory, aggregate argument/return
classification, as LCB's P07 ABI coverage admits them); and Concrete-side object/image validation.
R-0508 owns the semantic composition theorem.

**Exit:** corpus execution and performance against the LLVM profile are intermediate milestones.
The stage closes only when final-image validation checks actual instruction and data bytes, symbols,
relocations, entry state and startup against the recorded adapter output, LCB artifact and profile;
wrong-instruction, wrong-relocation, wrong-symbol, ABI and startup mutations are rejected; every
assembler, linker or loader behaviour LCB still assumes is named in the profile and theorem premises
and shrinks only as LCB's V04/V05 milestones land.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0081** (merged): merged — emitter independence between the LLVM adapter and the LCB adapter, and shared-helper disclosure, move to R-0505/R-0517.
- **R-0088** (superseded): superseded — aggregate/memory ABI realization moves to R-0502 (layout) and the adapter boundary (R-0505/R-0517) across V2–V6.
- **R-0089** (superseded): superseded — call/FFI/ABI matrices apply at the LCB adapter boundary (R-0505/R-0517); LCB's own ABI coverage (its P07) is an external dependency. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0089)
- **R-0099** (retired): retired — graduation of the native path is R-0517/R-0505's LCB-native profile; no QBE graduation exists. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0099)
- **R-0103** (superseded): superseded by R-0492/R-0505: merge; general separate compilation arrives in V6.
- **R-0296** (retired): retired — see the disposition in the task body. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0296)
- **R-0297** (delegated): delegated — see the disposition in the task body. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0297)

### Task R-0510

**Phase:** C

**Pull trigger:** R-0510: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep whole-language implementation in R-0514’s checkpoint.

**Objective:** Checked specialization — stage S8a of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), which holds the full design.

**Status (2026-10-18): planned.**

Depends on S8. Core semantics under type/capability
substitutions and trait evidence; Mono as specialization search plus a total validator checking
substitution, implementation identity, reachable-instance closure, signatures, Copy constraints,
literal ranges and capability requirements; produces `CheckedMonoCore` bound to `CheckedCore` and
its interfaces. Unbounded specialization or exhausted search produces an explicit incomplete
result.

**Exit:** every lowering entry point consumes `CheckedMonoCore`; missing or substituted instances,
wrong impls, invalid conditional Copy and effect-erased callbacks are rejected by targeted controls
(091's impl-capability case among them); direct and indirect generic calls are covered by
differential tests and a proved substitution/dispatch example.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0007** (superseded): superseded by R-0492/R-0510: merge, preserving function-symbol and metamorphic controls. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0007)
- **R-0116** (superseded): superseded by R-0497/R-0510/R-0514: merge; use payload binding and new IR chain. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0116)
- **R-0271** (superseded): superseded by R-0510/S8a, V5: merge into R-0510, retaining explicit law-evidence acceptance. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0271)
- **R-0437** (superseded): superseded by R-0514/R-0510: merge declaration and specialization checks; preserve distinct diagnostics. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0437)

### Task R-0512

**Phase:** F

**Pull trigger:** R-0512: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The superseded maintenance and migration plan also explicitly requires these responsibilities and transferred bug-class controls in its replacement. surviving requirement: Rebuild subject identity, dependency binding, provenance and profile-admission controls on new Core, preserve historical replay from the frozen tag, and grant no authority through legacy name-keyed evidence. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; enumerate V2–V7 delivery ownership explicitly. Delivery ownership for V2 – V7: each slice's cumulative exit from the plan is recorded as a milestone record under this task, including the maintained V7 feature inventory.

**Objective:** Migrate every claim onto compiler-generated Core statements and retire the legacy
evidence implementation, keeping its responsibilities.

**Status (2026-10-19): planned; proceeds with the slices (queue row 6).** The legacy implementation
(`PExpr` and `Proof.eval`, name-keyed tables, the theorem-type classification generator, the frozen
identity rows and amendments, attestations, per-surface status logic) receives soundness, compatibility
and security maintenance only, with no new semantic claim mechanisms.

Work (the plan's *Evidence migration*): an inventory of every supported claim and consumer with its exact
subject, proposition, coverage, transitive dependencies, assumptions and replay version; a claim migrates
once its complete semantic dependency closure has supported Core statements and sound statement
generation, binding and evidence interpretation — native compilation coverage is not required —
re-proved or carried by a proved bridge, with any change in domain, coverage or assumptions recorded for
review; each migration ships user-facing assurance and compatibility diffs; historical artifacts and a
versioned replay path are preserved; a superseded or invalidated claim never silently stays
authoritative. Subject identity, dependency binding and provenance remain required in the replacement.

**Exit:** every supported claim replays against a generated Core statement or has completed explicit
deprecation; legacy issuance code is deleted with its bug-class controls transferred to the replacement;
historical replay remains available; no name-keyed lookup grants current authority.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0087** (superseded): superseded by VP V7/R-0512: merge with total float model and profile-specific trust labels. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0087)
- **R-0097** (superseded): superseded by R-0511/R-0512: merge profile admission; retain no silent fallback. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0097)
- **R-0112** (superseded): superseded by R-0512: merge; retirement requires compatibility and rollback, not parity alone. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0112)
- **R-0123** (superseded): superseded by R-0495/R-0512: retire new legacy-model semantics; add admitted Core features/proof lemmas instead.
- **R-0176** (superseded): superseded by R-0495/R-0512: retire proposed legacy architecture; retain ownership goals in R-0151. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0176)
- **R-0202** (superseded): superseded by R-0495/R-0512: merge dependency-closure requirement into Core migration; maintain legacy checks until retirement.
- **R-0231** (superseded): superseded by R-0511/R-0512: merge into each slice’s admission controls.
- **R-0238** (superseded): superseded by R-0511/R-0512: merge into cumulative slice matrices; retain orthogonal policies.
- **R-0262** (superseded): superseded by R-0495/R-0512: retire blanket legacy bridge; retain only migration-pulled bridges. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0262)
- **R-0263** (superseded): superseded by R-0511/R-0512: merge into V1–V7 slice ordering.
- **R-0277** (superseded): superseded by R-0511/R-0512: merge into each slice’s cumulative acceptance.
- **R-0409** (superseded): Superseded by R-0496/R-0512: retire legacy redesign; carry demonstrated proof debt into Core proof engineering.

### Task R-0515

**Phase:** D

**Pull trigger:** The new compiler supports a bounded non-author pilot project’s imports, dependency, hosted APIs and error paths.

**Reconciliation (2026-10-19):** Keep coordination; existing tasks retain implementation ownership.

**Objective:** Make Concrete a language people choose to use, measured continuously alongside the proof
track rather than after it, by advancing bounded checkpoints of the tasks that already own each concern.

**Status (2026-10-19; amended 2026-10-21): planned; queue row 8, delivered through bounded checkpoints
within the feature track of the [capacity allocation](../archive/ROADMAP_LEGACY_PLAN.md#capacity-allocation).** After rows 0 – 2 qualify,
each checkpoint records its scope, accountable maintainer, reviewer, acceptance evidence and integration
slot before starting. Usability, interop and application work share the feature track's single slot and
capped share; the verified compiler's share and slots are reserved. Quarterly pilots are a
continuing feedback obligation, not a prerequisite for advancing rows 9 – 17; missed pilots and their
recovery dates are recorded rather than counted as completed.

R-0515 owns pilot coordination and acceptance evidence. R-0485/R-0488 retain ownership of ergonomics
changes and their prerequisites; row 7 supplies application workloads; row 16 retains
dependency-upgrade/replay acceptance and flagship graduation. R-0385–R-0388 own editor implementation,
R-0281 owns native debugging, and R-0416–R-0424 own generated-code performance evidence. R-0355 consumes
that evidence and owns release budgets. R-0515 coordinates their bounded user-facing checkpoints; it
creates no separate debugger or benchmark infrastructure. R-0358 owns stability and deprecation, R-0346/R-0347 onboarding, R-0359 contribution and
governance, R-0399 – R-0401 concurrency. A shared workload may supply evidence to several tasks, but does
not close their distinct exits.

Work, in bounded checkpoints:
- **Before the first pilot.** Pull forward the minimal R-0346/R-0347 onboarding path: install on a named
  supported host, create, build, run and test a project, use one dependency, and repair one
  representative error. Retain a non-author transcript, elapsed time and all author assistance; keep
  tutorial examples executable in CI.
- **Now.** (1) *External pilot:* each quarter, a non-author attempts a bounded program with a stated user
  goal and executable acceptance checks on the supported existing profile. Record installation and
  first-success time, failed attempts, author assistance, error-recovery effort, missing APIs and
  build/runtime measurements. Publish a consented or redacted friction log. Each blocking finding
  receives an owner and disposition; implemented fixes retain regression evidence and are retried by the
  participant. Reuse row 7 workloads where suitable without requiring the full application port.
  (2) *Diagnostics quality bar:* inventory diagnostic families encountered by the pilot and maintain a
  reviewed coverage list. New or changed diagnostics, and existing diagnostics blocking the pilot, must
  identify the violated rule, semantic consequence, relevant source or input location, and a valid next
  action or explicit unsupported boundary. Reuse R-0137/R-0466 and R-0488's structured-path criteria.
  Test representative failing programs and repaired positive controls; suggested repairs must preserve
  the intended ownership and authority policy. Snapshots supplement these checks
  ([DIAGNOSTIC_UX](../compiler/DIAGNOSTIC_UX.md)).
- **Practical debugging (2026-10-21; before workloads expand beyond RocksDB slice 1).** A non-author
  diagnoses two injected failures in a program built on RocksDB slice 1 (a runtime trap, and a
  foreign call that fails) with source locations, stack traces through Concrete, runtime and foreign
  frames, and inspected values, on the supported existing profile. R-0281 owns the implementation.
  The transcript, elapsed time and author assistance are retained. This checkpoint is a prerequisite
  for the four-adapter checkpoint and for R-0490's component workloads, not for RocksDB slice 1.
- **Continuing maintenance (2026-10-21).** The pilots measure keeping a program working, not only
  the first build: a participant other than the authors maintains a small project across one
  compiler upgrade and one dependency upgrade, recording the breakage, migration diagnostics, effort
  and author assistance. Row 16 still owns the external-user dependency-upgrade and replay acceptance.
- **V1 tooling checkpoints.** At V1a, qualify the minimal R-0385 – R-0388 editor path over the shared
  compiler session: types, diagnostics, definition navigation, proof/trust/capability status and replay
  links. At V1b, qualify DWARF line mappings and source-level trap traces for the admitted native subset
  on each declared supported profile/target. These checkpoints retain the incremental-driver area prerequisites; any
  unresolved dependency and its effect on the release schedule is recorded before the checkpoint is
  committed. R-0355 owns performance baselines and regression gates, extended with equivalent-workload
  comparisons against pinned C, Rust and Zig toolchains and the verified-versus-LLVM comparison.
- **Before public release.** R-0358 owns the language/stdlib stability and deprecation policy. Publish a
  supported/missing `std` coverage inventory and assign owners to pilot-blocking gaps. Begin contributor
  onboarding and succession planning under R-0359 and the verified-pipeline ownership discipline; name
  critical-area reviewers and retain an independently completed review or release-replay exercise.
- **Pull-gated.** Expand `std` and the tutorial into a book when recorded workloads justify them. Each
  activated item records an owner, bounded scope and executable acceptance criteria before work starts.
  R-0399 – R-0401 retain concurrency design and implementation ownership; record a dated decision or an
  explicit deferral with a reopening trigger.

**Exit (per checkpoint):** *onboarding* — a non-author completes the first-project path with the
transcript, time and assistance retained, and its examples run in CI; *now* — two consecutive quarterly
pilots retain their program, acceptance results and assistance record, each with a participant-validated
improvement to a recorded obstacle (author-completed steps remain unresolved usability limitations;
pilots continue quarterly after this checkpoint), and the diagnostic coverage list has no unowned
pilot-blocking gaps, with gates exercising failing inputs and valid repairs for each selected family;
*V1 tooling* — scripted editor tests exercise every promised operation and verify CLI/editor agreement
after source, dependency and profile changes, including stale and unsupported evidence; a debugger
resolves known source lines and nested trap fixtures produce expected source frames on every declared
supported profile/target; benchmark sources, correctness checks, hardware, toolchain versions, build
flags, safety settings, repeated measurements and numeric regression thresholds are published, cold
builds, incremental edits and runtime are measured separately, CI rejects a deliberately injected
threshold violation, and unsupported comparisons are reported explicitly and do not count as covered;
*practical debugging* — a non-author diagnoses the injected trap and the failing foreign call from
source locations, stack traces and inspected values, with the transcript, time and assistance retained,
and scripted fixtures check that the trap and the foreign failure each resolve to their source frame;
*continuing maintenance* — one non-author maintenance record across a compiler upgrade and a dependency
upgrade is retained, each blocking finding has an owner and disposition, and the record repeats with
later pilots; *before public release* — the stability policy, `std` coverage inventory and named reviewers are
published.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0108** (merged): duplicate of R-0416/R-0515: merge profile-labelled budgets and measurements. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0108)

### Task R-0516

**Phase:** C

**Pull trigger:** R-0516: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; required after preview, not only at final ecosystem stage.

**Objective:** Release automation, so that nightly builds, the release train and milestone releases cost
minutes of attention instead of days.

**Status:** planned; Phase 3 release-preparation checkpoint coordinated by R-0333/R-0515
within the feature track of the capacity allocation. Its nightly-publication subset precedes the first
automated nightly; its complete exit precedes the first automated train or milestone.

Work: one command produces, from an exact main commit whose full tier passed (R-0513), the R-0333
candidate record (source and toolchain identities, artifact identities, supported subset and profiles,
retained check results, compatibility statements, open blockers and accepted limitations), the release
artifacts for every declared platform, a changelog section and migration notes drafted from merged
changes, and the claims matrix for the release. Nightly builds publish automatically with their records;
train and milestone releases add the human review of notes and claims.

**Exit:** from immutable source, toolchain, policy and reviewed release-input records,
produce the candidate record and packaged artifacts without manual edits to generated
outputs. Full qualification must match the exact source/toolchain identities, and
candidate installation, claims and artifact checks run against the artifacts to be
published. Demonstrate rejection of failed, missing or wrong-commit qualification,
artifact substitution and unapproved milestone publication. Demonstrate one reviewed
train/milestone publication rehearsal, retry after partial failure without replacing
an existing release identity, and the applicable R-0427 rollback procedure. Two
successive scheduled nightlies publish unattended with retained records. Semantic,
compatibility and limitation decisions remain explicit reviewed inputs.

### Task R-0517

**Phase:** E

**Pull trigger:** R-0517: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Coordinated under queue row 3 (V1b) with R-0505.

**Objective:** Lean Compiler Backend as Concrete's native backend. The native AArch64 code
generation of the `verified` profile (V1b onward) is the separately developed, general-purpose
[Lean Compiler Backend](https://github.com/unbalancedparentheses/lean-compiler-backend) (LCB), a Lean 4 port of QBE 1.3 whose correctness is
developed alongside its implementation. Concrete **uses** LCB; it does not build it, and it builds no
machine backend of its own.

**Status (2026-10-19): planned; coordinated under queue row 3 (V1b) and R-0505.** The existing
restricted adapter is [Concrete draft PR #8](https://github.com/unbalancedparentheses/concrete2/pull/8)
(branch `feat/lean-backend-integration`): an experimental `--emit-qbe` path from today's checked SSA,
covering wrapping arithmetic, comparisons, branches and loops and rejecting checked arithmetic, calls
and memory. It is a consumer experiment, not the adapter this task delivers.

**Dependency contract.**
- LCB is a pinned dependency: an exact LCB version (commit and release identity) enters through a
  lake/nix input whose lock records LCB's own `upstream-lock.json` hash, so the QBE 1.3 and Zig
  reference sources LCB ports are part of Concrete's recorded build identity.
- Concrete and LCB build with the same Lean toolchain (4.34.1 today); an LCB upgrade that changes the
  toolchain is a Concrete toolchain upgrade under R-0302/R-0357's procedure.
- Concrete imports LCB’s versioned source and target semantics, initial/final-state relations, behaviours, ABI projection, certified-domain predicates, admission soundness and preservation/safety theorem statements. Each theorem must identify the actual compilation function and exact checked input/output artifacts, its direction, premises and transitive axiom dependencies. Compatibility-mode support is not certified-domain membership.
- Before freezing the adapter, Concrete states type-checked bridge obligations from BIR to these exported interfaces and proves them for each admitted slice. They relate signed/unsigned values and widths; checked overflow and division failures; defined traps versus stuckness, undefined behaviour and compiler rejection; and external traces, capability tags and ghost erasure. No defined Concrete behaviour may be discarded by an LCB domain restriction without a proved admission precondition.
- Memory bridges relate Concrete’s block/provenance, permissions, initialization, lifetime and address-exposure semantics to LCB’s memory and target frames, preserving target-memory validity as well as observations. Call bridges establish the exact Apple ARM64 argument/result layout, stack alignment, clobbers, saved registers, external-memory effects and imported contracts. Features without these bridges remain outside the certified profile.
- Composition proves compatibility of related initial states, environments and observation relations; adequacy between Concrete’s clock semantics and LCB’s execution relations; preservation of maximal behaviours, including divergence; and well-founded administrative stuttering. Forward-to-backward conversion requires proved determinacy/receptiveness hypotheses or an applicable oracle-based alternative. Leaf-source determinism alone does not discharge external-interaction hypotheses.
- The resource bridge distinguishes compile-time limits and evaluator exhaustion from program outcomes, establishes available frame space, address non-wrap and other target premises, and discharges Concrete’s sufficient-resource obligation. LCB admission limits are not runtime stack-safety proofs.
- Every dependency upgrade records semantic, domain, ABI, artifact-schema, theorem and axiom changes alongside Lean/toolchain changes. CI rebuilds the imported proofs and complete composition, replays retained evidence, and runs admission, adapter, ABI and image mutation controls before promotion. Changed definitions require reviewed proof migration and versioned claim boundaries; cached evidence cannot cross identities. Preserve a qualified pin and rollback path when upstream changes cannot yet be adopted.

**Ownership.** Concrete owns: the BIR → LCB-IR adapter, preserving Concrete's semantics — checked
overflow is lowered to explicit checks or to LCB's checked operations once LCB's E02 Concrete
adapter work provides them (wrapping LCB arithmetic does not implement checked operations by itself),
and traps, capability-tagged effects and the runtime ABI are preserved; the adapter's refinement
proof (BIR semantics to LCB-IR semantics under the explicit observation relation); version pinning
and upgrade qualification; Concrete-side final-image checks (with R-0505); and the composition of
theorems (with R-0508). LCB owns its IR semantics, lowering, register allocation, encoding and their
proofs. LCB's milestones — C01 – C03 (first composed preservation result), V01 – V05 (semantics,
pass preservation, allocation, encoding, composition) and E02 (Concrete-facing adapter support) —
are tracked here as external dependencies, never as Concrete work; Concrete records their status
from LCB's published reports and does not schedule them.

**V1b theorem.** V1b's native theorem is the composition: Concrete's checked Core → BIR refinement
(V1a), the adapter's BIR → LCB-IR refinement, and LCB's preservation theorem for its exact certified
domain. It is V1b only when that pinned domain covers V1's declared subset (see the release gate); it carries
every trust boundary LCB names (today: rendering, ISA model, assembler, linker, host OS and the Lean
kernel, until LCB's V04/V05 remove them) as explicit premises in Concrete's theorem and profile.

**Release gate.** LCB’s Stage 1 exit is necessary but insufficient for V1b. V1b requires a pinned certified LCB domain covering every reachable operation and behaviour of Concrete’s declared V1 subset, including checked-arithmetic trap paths, direct calls and the `print_int` external-event contract, together with Concrete’s proved adapter, semantic bridges, final-image correspondence and native-exit obligations. A smaller leaf-function intersection may ship as a separately named intermediate theorem with explicit admission restrictions; it does not complete V1b.

**Fallback:** whenever any V1b obligation remains unmet — LCB’s Stage 1, a certified LCB domain covering V1, or Concrete’s composed V1b exit — a candidate may ship V1a’s proved CheckedCore → BIR result and an explicitly selected experimental LCB-native path, with native execution tested and labelled `tested`/`backend_trusted`. That native path does not satisfy the `verified` profile, complete V1b or queue row 3, or graduate the verified-preview acceptance scope. R-0333 records the narrower delivered scope and outstanding native obligations; no source-to-native or native-transport claim is issued. Fallback requires an explicitly selected native assurance mode distinct from a request for certified native output. A request for certified native output fails when V1b evidence is unavailable; it never silently downgrades. Here “validated” means only the listed structural or differential checks unless a named validator-soundness theorem establishes a stronger result.

**Transferred controls.** The useful controls of the retired QBE tasks (R-0072 – R-0112, R-0296) apply
at the adapter: emitter independence from the LLVM adapter, a generated adapter support matrix with
explicit unsupported diagnostics and no silent fallback to LLVM, three-way differential observations
(Core semantics, LLVM profile, LCB-native) with classified disagreements and retained reductions, ABI
classification against native C round trips, deterministic emission and full toolchain identity
(including the LCB lock), and performance comparison between the LLVM and LCB-native profiles under
R-0416 – R-0424.

**Per-compilation certificates.** R-0552, beside this task, has release builds emit a
kernel-checked certificate for each output, so that the code generator and C compiler that build the
Concrete binary leave the trusted base for checked artifacts.

**Exit:** a pinned LCB version builds inside Concrete's Nix/lake build with the shared toolchain and its
upstream lock recorded in the candidate identity; the adapter covers V1's admitted subset with checked
arithmetic, traps, `print_int` and direct calls, and rejects everything else with a stable
diagnostic; the adapter refinement is proved against LCB's imported semantics; the composed V1b
statement type-checks with LCB's trust boundaries as named premises; wrong-check, dropped-trap,
wrong-width and substituted-callee mutations of the adapter are rejected by the proof or by the
differential suite; and the release record states which LCB milestones the shipped claim depends on.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0072** (retired): retired — LCB pins QBE 1.3 in its own upstream lock; Concrete pins the LCB version under R-0517. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0072)
- **R-0074** (superseded): superseded — merge into validated BIR (R-0503) and R-0517; the closed `llvm`/`qbe` backend set becomes the LLVM and LCB-native profiles. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0074)
- **R-0081** (merged): merged — emitter independence between the LLVM adapter and the LCB adapter, and shared-helper disclosure, move to R-0505/R-0517.
- **R-0083** (retired): retired — there is no Concrete QBE backend to select; the no-silent-fallback rule moves to R-0511/R-0517. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0083)
- **R-0084** (delegated): delegated — a typed QBE-style IR and printer are LCB's input layer; Concrete's adapter constructs LCB-IR under R-0517.
- **R-0086** (superseded): superseded — its semantic vectors become adapter test vectors for V1 (R-0511/R-0517); later widths, casts, memory and floats follow the V-slices. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0086)
- **R-0088** (superseded): superseded — aggregate/memory ABI realization moves to R-0502 (layout) and the adapter boundary (R-0505/R-0517) across V2–V6.
- **R-0089** (superseded): superseded — call/FFI/ABI matrices apply at the LCB adapter boundary (R-0505/R-0517); LCB's own ABI coverage (its P07) is an external dependency. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0089)
- **R-0091** (merged): merged — native test-mode entry generation joins R-0075's driver and the LCB-native profile (R-0517).
- **R-0099** (retired): retired — graduation of the native path is R-0517/R-0505's LCB-native profile; no QBE graduation exists. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0099)
- **R-0101** (delegated): delegated — legality verification of the QBE-style IR is LCB's (its F04/V02); Concrete validates adapter output under R-0517. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0101)
- **R-0102** (superseded): superseded — the operation gap ledger becomes typed pass obligations (R-0506) and the adapter's obligations (R-0517).
- **R-0104** (merged): merged — deterministic emission and toolchain identity join R-0146; the LCB version and upstream lock are pinned by R-0517.
- **R-0296** (retired): retired — see the disposition in the task body. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0296)
- **R-0297** (delegated): delegated — see the disposition in the task body. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0297)

### Task R-0520

**Phase:** E

**Pull trigger:** R-0520: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The three existing gate sites retire with old reports, but solver failure semantics and consumer agreement survive. surviving requirement: Distinguish absent, crashing and malformed solvers from genuine proof difficulty across every new verification surface, preserving solver-error and unproven controls. This line governs over older implementation directions for the legacy compiler.

**Objective:** Produce `solver_error`, so that an absent or broken solver is distinguishable from a
proof-difficulty `unproven`.

**Status (2026-10-19): planned.** Absent solver, garbage solver and the negatives fixture all report
`unproven` today. That is fail-closed, so nothing over-claims, but consumers cannot tell an
environment defect from a proof result.

**Exit:** the three gate sites that expected `solver_error` restore their strict assertions; absent,
crashing and malformed-output solvers produce `solver_error` on every surface R-0494 renders; a
genuine `unproven` control is unchanged.

### Task R-0523

**Phase:** E

**Pull trigger:** R-0523: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The fixed_capacity comparison uses old obligation paths, but the exit separately requires general coverage integrity. surviving requirement: Account for every new verification obligation as generated, explicitly unsupported or a recorded defect, and reject unexplained drops with a negative control. This line governs over older implementation directions for the legacy compiler.

**Objective:** Explain the unclassified `fixed_capacity` obligation that both the old and new
obligation paths drop, and make that class of drop visible.

**Status (2026-10-19): planned.** One obligation is rejected by both paths without a recorded reason.

**Exit:** the dropped obligation is classified (generated, unsupported with a reason, or a defect
with a bug record); an unclassified drop fails the coverage report; a control reintroducing an
unexplained drop is rejected.

### Task R-0524

**Phase:** E

**Pull trigger:** R-0524: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Two-state mutation contracts (queue row 13): a narrow record/array state model, `old`,
`modifies`/frames, then ghost locals and parameters with erasure and admissibility rules.

**Status (2026-10-19): planned; after R-0473/R-0474.** Links R-0175 (frame inference) and R-0239
(frame/flow contracts). Unsupported heap and reborrow shapes refuse explicitly.

**Increment A:** `old` and frame clauses on records and arrays are typed, generated against Core's
two-state semantics and replayed; a contract omitting a mutated field is refused. This increment
can serve R-0486 before ghost support is complete.

**Increment B:** ghost locals and parameters erase with a proved erasure lemma for the admitted
forms. Ghost observations must not consume runtime linear resources, extend runtime borrows, or
influence executable decisions. Logical definitions obey R-0526's totality/admissibility boundary;
unsupported observations of moved values or reborrows refuse until their snapshot semantics exists.

**Exit:** both increments accepted; unsupported shapes report `unsupported` rather than `proved`.
Reference: [Dafny specifications and ghost code](https://dafny.org/latest/DafnyRef/DafnyRef.html).

### Task R-0525

**Phase:** E

**Pull trigger:** R-0525: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The proofFnsExt conversion and R-0512 migration retire, but the exit explicitly preserves fail-closed exclusion controls. surviving requirement: Retain the mutable-state claim requirements represented by the three links and their no-authority controls until exact new Core statements and independently checked proofs support them. This line governs over older implementation directions for the legacy compiler.

**Objective:** Ninth-table conversion (queue row 14): extract and replay the three currently
fail-closed `proofFnsExt` links using R-0524's mutable-state model.

**Status (2026-10-19): planned; after R-0524.** Retargeted to Core migration (R-0512): the conversion
produces generated Core statements, not new legacy table rows.

**Exit:** the three links replay against generated Core statements; until then they provide no
authority, which a control asserts; no-authority exclusions remain gated during migration.

### Task R-0526

**Phase:** E

**Pull trigger:** R-0526: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Checked recursion and total functions (queue row 15, with R-0270's totality requirement
and R-0060's specification library): `#[decreases]`, well-founded descent VCs over every recursive
edge including mutual recursion, recursive definitions extracted to each admitted proof host, and
induction-capable automation; only checked-total definitions are callable from contracts.

**Status (2026-10-19): planned.** A decrease of `n - 1 < n` alone is not well-foundedness: lower
bounds, every recursive edge and combined measures across cycles are required. Termination is not
liveness. Symbolic stack bounds from admitted measures need the frame/resource correspondence of
R-0224/R-0248 before they become byte bounds.

**Exit:** recursive functions with checked measures are admitted, including one mutually recursive
pair; a measure that fails to decrease on one edge, or is unbounded below, is refused; a contract
calling a non-total helper is refused; one recursive claim is proved by induction and replayed.

### Task R-0530

**Phase:** D

**Pull trigger:** A program supported by the new compiler needs dup, dup2 or fdopen to manage independently owned descriptor aliases.

**Objective:** Typed descriptors and `dup`/`dup2`/`fdopen` alias ownership — the "second step" left
open when R-0484 closed.

**Status (2026-10-19): planned; conditional on a workload that needs descriptor duplication.**
R-0484's closure stands; this is new work, not a reopening.

**Exit:** descriptors are typed handles; duplication produces an explicitly owned second handle;
closing one handle does not invalidate the other's capability accounting; double-close and
use-after-close of an alias are rejected or trap with a regression fixture.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0484** (done): done — closed 2026-10-08 at `245cd51c`; the body is retained for its accepted limits; the typed-descriptor second step is R-0530. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0484)

### Task R-0536

**Phase:** D

**Pull trigger:** R-0536: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Explicit enum discriminant values: honour explicit tags, reject collisions, and
validate representation/FFI agreement.

**Status (2026-10-19): planned (previously prose item "7a" without an R-id).** The representation
proof enters V2/V6 through R-0502's layout certificates.

**Exit:** explicit tags are emitted as written; duplicate or out-of-range tags are rejected; a C round
trip confirms ABI agreement for a `repr`-annotated enum.

### Task R-0556

**Phase:** C

**Pull trigger:** R-0556: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Generated module interfaces and the interface-diff gate (MOD-7 of
`docs/spec/modules.md`; owner decision OD9, decided 2026-10-29). The compiler generates each module's
interface artifact from its `pub` declarations (no handwritten interface file), and a gate diffs every
module's generated interface between commits, so any change to a module's public API appears in
review as an explicit interface change. It builds on the interface data of R-0492 and R-0497, and
separate compilation (V6) reads the same artifacts. Exit: the generator emits one interface per
module for std and the examples, the gate fails a commit whose public API changed without its
interface diff being part of the commit, and MOD-7 is `enforced`.

### Task R-0557

**Phase:** C

**Pull trigger:** R-0557: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The root-token mechanism is superseded, while constant linearity and explicit external-state authority survive. surviving requirement: Reject duplicable linear constants, pass explicit target grants to main, and inventory narrow trusted wrappers for singleton external state under the approved value-and-with authority design. This line governs over older implementation directions for the legacy compiler.

**Objective:** Global state as Austral-style capabilities (TRUST-8, TRUST-9 and TRUST-10 of
`docs/spec/trusted.md`; owner decision OD8, decided 2026-10-29). Check refuses a constant that holds
a capability, token or handle (today a constant of a linear type is accepted, and each use yields a
fresh copy); `main` receives the root resource value and splits it (STMT-7 changes with it); every
hardware or runtime singleton (allocator arena, UART and MMIO, panic buffer, `errno`, C libraries with
internal state) is produced or taken only by a `trusted fn` returning or taking a linear token, and a
gate lists those wrappers as the inventory of trusted external state, beside R-0509's trusted base.
Exit: TRUST-8's pending example is refused, the root-token API and the wrapper gate are on main, and
TRUST-8 to TRUST-10 are `enforced`.

### Task R-0548

**Phase:** D

**Restructured (2026-11-02):** retired with the legacy compiler: The listed Check, Elab, Mono and interpreter tables retire while the identity and dispatch obligations survive. surviving requirement: Method identity must use typed owner, target and method keys throughout the new compiler, with distinct same-name trait dispatch tested when permitted by the decided language. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0548: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Key every method table by the typed `MethodKey` (owner, target `TypeId`, method name)
that bug 186's coherence judgment introduced, instead of the `Type_method` spelling: the impl-method
summaries (`FileSummary.implMethodSigs`), Check's function table, Elab's method resolution and module
prefixing, Mono's `T$m` specialization, the interpreter's `traitImplKey`, and the evidence and report
consumers that read method names. The link symbol becomes a derived spelling of the key.

**Status (2026-10-23): planned.** Bug 186 made the spelling injective within a type by refusing a
method name supplied by two impls (TRAIT-5, E0594), so the spelling is safe today but still carries
the identity.

**Exit:** no pass recovers a method's owner or type from a string; a type implementing two traits
that both declare `m` compiles, with each bound call reaching its own trait's method natively and
under `--interp`, once TRAIT-5 is relaxed in the same change.

### Task R-0549

**Phase:** D

**Pull trigger:** The new compiler supports a decided trait-bound call whose same-name methods require explicit trait qualification.

**Objective:** Qualified trait-method calls, `Trait::method(x)`, which name the trait a call
dispatches through, so an ambiguous bound call can be written instead of refused.

**Status (2026-10-23): pull-gated.** `Trait::method(x)` parses today as a static call on a type named
`Trait` and fails as an unknown method.

**Exit:** `A::m(x)` and `B::m(x)` on `x: &T` with `T: A + B` call their own traits' methods natively
and under `--interp`; an unqualified ambiguous call stays E0595.

### Task R-0522

**Phase:** -

**Pull trigger:** R-0522: no implementation pull; retained as the retirement record for legacy-compiler-final.

**Disposition:** retired with `legacy-compiler-final` on 2026-11-02 — The task only consolidates duplicated batch and single-goal paths in the old Isabelle evidence builder.

**Objective:** Collapse the Isabelle builder's duplicated batch and single-goal paths into one

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0522).

### Task R-0527

**Phase:** research

**Pull trigger:** A new-compiler workload requires a relational leakage claim beyond bounded Core revision equivalence.

**Objective:** Scope noninterference, constant-time properties, and relational extensions beyond R-0531’s bounded Core-level revision equivalence before implementing them. Select the observation model, permitted declassification and additional obligations for native transport.

**Status (2026-10-19): research.** Ordinary refinement does not preserve noninterference; the
verified-compiler plan's refinement is not a relational theorem. R-0459's 2-safety proposal and the
paired-execution prose merge here; R-0136/R-0225/R-0226 keep source-flow restrictions.

**Research questions (Jasmin comparison, 2026-10-22):** specify whether termination, traps,
external calls and variable-latency operations are observable, and justify each declassification.
On the existing bounded example, determine whether R-0136's enforced restriction implies the
selected leakage property; supply a soundness argument or a counterexample that identifies the
missing restriction. Separately enumerate what each lowering/optimization and target must preserve.
Proceed to implementation only with a named admitted fragment, observation model, validation plan
and workload benefit; otherwise record the limitation and reopening trigger. Functional refinement
alone does not discharge these obligations. Reference: [Jasmin leakage models](https://jasmin-lang.readthedocs.io/en/latest/tools/ct.html).

**Exit:** a decision record with the observation and declassification contract, one worked
self-composition example on a bounded program, and an explicit statement of what native transport
would additionally require; or a recorded deferral with its reopening trigger.

### Task R-0529

**Phase:** F

**Pull trigger:** R-0529: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Atomic evidence persistence: crash-safe writes, permissions and concurrent readers for
the evidence store. Distinct from R-0144's compiler cache.

**Status (2026-10-19): planned.**

**Exit:** an interrupted write leaves either the previous or the new record, never a partial one
(fault-injection control); concurrent readers never observe a partial record; store permissions are
checked on open.

### Task R-0531

**Phase:** E

**Pull trigger:** A reviewer needs equivalence between two new-Core function revisions after typed contracts and versioned dependency environments are available.

**Objective:** Selected-function equivalence across revisions: compare a function's two revisions
with their original dependencies and semantics, as the follow-on to R-0183's diff gate.

**Status (2026-10-19): planned; after typed contracts.**

Before implementation, define the admitted Core fragment, input relation, observation relation, treatment of termination and traps, and versioned dependency environments. This task owns bounded Core-level equivalence across revisions; it makes no native-transport, constant-time or noninterference claim.

**Exit:** an equivalence claim between two revisions of one function is generated against Core and
replayed; a behaviour-changing edit is refused; a dependency change is reported as a changed
premise, not hidden.

### Task R-0532

**Phase:** F

**Pull trigger:** R-0532: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** R-0208's residual limits: native-trust receipt issuance, advisory-policy rollback
protection and fragment-scoped advisory narrowing — each either implemented or kept as an explicit,
published limitation.

**Status (2026-10-19): planned.** R-0208 is closed; these are its recorded accepted limits. Native-
evaluation receipts remain withheld until issued with distinct trust labels (R-0506/R-0440).
Reverting both policy and minimum defeats the local rollback guard; that limitation stays published
until a threat-model-specific guard exists. Narrowing an advisory to a fragment requires an
independently checked environment footprint; whole-version refusal is the safe fallback.
It also owns KNOWN_HOLES H29 (2026-10-22), external-checker pins lagging upstream soundness
fixes: controlled by single-threaded con-leche and `check_checker_pin_freshness.sh` (warning when
landing, finding in the nightly campaign); docs/verification/EXTERNAL_CHECKER.md.

**Exit:** each of the three has an implemented control or a published limitation with its threat
model and owner.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0208** (done): done — closed 2026-10-13 at `2b570c5a`; the body is retained for its accepted limits, now owned by R-0532. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0208)

### Task R-0533

**Phase:** after-1.0

**Pull trigger:** Users repeatedly shrink trusted wrappers in new-compiler programs after trusted-boundary reports stabilize.

**Objective:** Optional trusted-boundary shrinker: suggest smaller trusted wrappers without upgrading
any evidence.

**Status (2026-10-19): pull-gated.**

**Exit:** suggestions are advisory only; a suggestion never changes a verdict; a control shows an
applied suggestion still requires the ordinary checks.

### Task R-0534

**Phase:** research

**Pull trigger:** A new-Core workload needs real-analysis error bounds after its exact IEEE floating-point profile is established.

**Objective:** Floating-point error-bound specifications (epsilon bounds via real analysis) after the
exact IEEE float profile (V7).

**Status (2026-10-19): deferred; requires V7's float semantics.**

**Exit:** a decision record on the real-analysis library and one bounded worked example, or a recorded
deferral.

### Task R-0535

**Phase:** research

**Pull trigger:** A consumer of new-compiler evidence requests freshness-tracked reviewed human proof arguments.

**Objective:** Decide whether reviewed human proof arguments (`proved_by_hand`) can be recorded as
freshness-tracked reviewed evidence without ever presenting as machine-checked `proved`.

**Status (2026-10-19): research.** R-0461 forbids `proved` for unresolved conditions; a human
argument is reviewed evidence, not a kernel-checked proposition.

**Exit:** a policy decision; if adopted, the evidence class is distinct on every surface and a control
shows it never satisfies a kernel-only policy.

### Task R-0537

**Phase:** E

**Pull trigger:** R-0537: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Closed-world library evolution: callable-set compatibility and escape hatches when a
library's set of callables changes, respecting open imported callbacks.

**Status (2026-10-19): planned.**

**Exit:** additions, removals and changes to a dependent’s reachable callable set trigger dependency invalidation and compatibility checking. Previously closed-world evidence is retained only after rechecking the enlarged or changed set against its contracts, capabilities and proof obligations. Adding an unreachable export may remain compatible. Controls cover both cases; open imported callbacks are never treated as closed.

### Task R-0538

**Phase:** research

**Pull trigger:** A new-compiler claim requires bitvector evidence without native-evaluation trust and a replay route is measurable.

**Objective:** Reduce the native `bv_decide` checking trust (H20): bitvector proofs currently rely on
native evaluation in the checker.

**Status (2026-10-19): research.** R-0506 owns honest classification of the native-evaluation axiom
now; this task owns eliminating the trust, which was previously "unowned".

**Exit:** a decision record on certificate replay or kernel-reducible checking for the admitted
bitvector fragment, with measured cost; or a recorded deferral.

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

### Task R-0482

**Phase:** E

**Pull trigger:** R-0482: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old frozen-row migration and PFnDef differential path retire, while exact-property acceptance and twenty-five attacks retain independent value. surviving requirement: Separate implementation, model, proposition, contract, package and policy identities with exact relation and totality checks, preserving the full substitution matrix and later partial evidence-linking consumer tests. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep P1 qualification; split later increments among R-0495, R-0473/74 and R-0440.

**Identity separation, model attestations, exported interfaces, and proof-aware package linking.**

**Scheduling:** R-0004's prerequisite is satisfied. Production work starts after the post-closure
91-family qualification checkpoint and R-0208's toolchain/revocation fire drill. This task is
deliberately separate so R-0004 remains finite rather than becoming the entire future verification
language.

**Current entrance state:** `IDENTITY-FREEZE` (11/0) pins the pre-migration populations and
`IDENTITY-OVERBINDING` (6/0) reproduces the fail-safe defect: an unused std-content change moves 7
of 21 package identities. Those count gates are entrance evidence, not a migration map. Before any
identity bytes change, freeze the canonical full rows so a rewrite cannot preserve every `sort -u`
count while silently changing associations. **Rows frozen (2026-10-13):** `IDENTITY-ROWS` (19/0) pins 323 canonical rows
(`docs/verification/identity-rows-pre-r0482.tsv`, from `scripts/tools/identity_rows.sh`) with package
digests replaced by their producing fixture, plus the raw baseline with every digest byte
(`docs/verification/identity-raw-baseline-pre-r0482.tsv`, tied to `61b2a261`) that the migration map
is written against; a model swap that keeps every count moves the rows, and an unused std edit moves
none. **Definitions RATIFIED 2026-10-13** (D1–D7 with amendments):
`docs/verification/IDENTITY_SEPARATION.md` — tagged scope origins (declared / undeclared / standalone /
reserved toolchain), version excluded from scope, content-addressed standalone scope, std as its own
scope, ambiguous undeclared same-named packages refused at existing boundaries, and context changes
distinguished from root changes. **Migration landed** (plan `docs/verification/IDENTITY_MIGRATION_PLAN.md`,
P1–P4 accepted 2026-10-13; see the changelog): the three identities below replace `PackageIdentity`,
the old-to-new map is total (323/323, 0 shared, 0 refused) and the normalized freeze is unchanged;
`IDENTITY-SEPARATION` holds the migration's controls and the over-binding and rows gates assert their
positive forms. **Remaining from the migration (P1):** claims whose closure reaches std or another
package stay fail-closed (`unscopedEdge`); admitting such nodes into report roots is its own slice,
with a claim that calls std and the reachable-std and cross-package sensitivity controls.

#### Five identities that must not substitute for one another

- **`PackageScopeIdentity`** names declared publisher/origin, package namespace/name and the
  manifestless-scope rule. It does not bind dependency names or module content.
- **`PackageArtifactIdentity`** binds the package's own canonical `(module identity, content digest)`
  pairs. Pairing is mandatory: independently sorted names and contents lose the association.
- **`ResolutionContextIdentity`** binds the resolved dependency graph, toolchain and other ambient
  inputs that can change how the same package artifact is interpreted.
- **`DefinitionIdentity`** binds package scope, module, declaration and implementation. Legitimate
  many-to-one attestation rows mean package-level injectivity is not the migration criterion;
  collisions and ambiguity are decided at the complete definition/row key.
- **Claim dependency root** binds only proof-reachable contract/body/trusted/missing dependencies.
  It is what must move when a reachable dependency changes; package scope must not be used as a
  substitute.

The old-to-new migration map is typed and row-complete. It proves total accounting, records
many-to-one cases and named refusals, and bootstraps generated symbols, attestations, receipts and
all production consumers atomically. Two controls graduate together: editing an unused std module
does not remap unrelated scope/attestations, while editing a reachable definition invalidates the
affected dependency roots. Either control alone permits a vacuous or over-bound implementation.

**Objective:** make the relationship among a mathematical model, an exact implementation, an
exported behavioral interface, a proved proposition and an accepting policy explicit and
machine-checkable. The authoring promise is:

> Proof authors define or select a mathematical model, select an opaque generated implementation
> reference, state the exact relation and claim scope, and provide correspondence evidence. The
> compiler recomputes implementation identity, validates the attachment, keeps contract/model/body
> dependencies separate, and refuses every missing, stale, ambiguous, downgraded or cross-package
> association.

#### Two arrows, neither substituting for the other

```text
PFnDef / ModelIdentity
       |  model-correspondence evidence
       v
DefinitionIdentity / exact source implementation

PFnDef / canonical proposition
       |  certificate or kernel/solver proof
       v
claimed logical property
```

The generated implementation reference identifies the endpoint of the first arrow. It does not
prove the arrow. A kernel-valid theorem over a model proves the second arrow only. Reports,
receipts and policy retain both results independently.

#### Required typed objects

**`ModelIdentity`:** identity of the mathematical semantics being used, distinct from both the
implementation and the theorem proposition. It binds model schema, canonical model digest,
parameterization, logical dependencies and model codec/toolchain identity. A proof-script-only
change does not move it; a semantic model change does.

**`ImplementationReference`:** opaque generated selector for an exact `DefinitionIdentity`:
package, module, declaration and authoritative implementation digest. Its constructor is private.
Authors select a generated typed symbol; they never transcribe its components. Compiler
recomputation is mandatory and a mismatch refuses.

**`ModelAttestation`:** the complete attachment, containing implementation reference, model
identity, relation kind, claim scope, provenance and correspondence disposition. Required relation
kinds are at least:

```text
implements
refines
equivalent
abstracts
contract_only
```

These are not aliases. A one-direction refinement cannot be rendered or consumed as equivalence.
Trust is an evidence mode/disposition, not a semantic relation kind; a trusted model remains an
assumption even when the implementation reference is exact.

**Exact property acceptance (2026-10-18; planned):** bug 079's body/spec binding
does not establish that the theorem states the annotated `#[proof_coverage]`
(`iff`, `one_direction`, etc.). For every
supported claim family, construct the expected typed proposition from the selected
subject/model, parameters, domain, relation and scope, then check the attached
theorem's type against it under the admitted normalization rules. A theorem about
the right body is insufficient if it proves a different property. An arbitrary
theorem that cannot be matched remains explicitly unclassified/unsupported for
that coverage; its author's annotation cannot grant a stronger verdict.

Retain a sorting example whose properties are distinct: preserving length,
producing sorted output, and preserving the input elements. A length-only theorem
must never authorize either of the latter claims. Gate a matching theorem, a wrong
property over the same body, a narrower input domain, and one-direction evidence
relabeled as equivalence. Drive those controls through status, receipt issuance,
receipt consumption and independent replay as each consumer graduates. Bind the
checked proposition, relation, domain, assumptions and exact implementation/model
and dependency identities in the existing canonical evidence objects. Tests,
assumptions and kernel proofs retain their separate dispositions; proposition
matching does not establish model-to-machine correspondence or that the chosen
specification captures the user's intent. No new coverage label becomes
authoritative before its matching rule and rejecting controls exist.
Until this lands, `#[ensures]` obligations have no discharge path at all: bug 089
found every report printing `proved_by_lean` from an `#[ensures_proof]` link or an
`iff` label alone, and `Report.ensuresVerdictOf` now reports every linked
postcondition `unproven`. The matcher plugs in there and nowhere else.

**Total-model admission is a semantic prerequisite, not merely sequencing.** A model used as
logical contract semantics may carry a current `implements`, `refines`, `equivalent`, or
`contract_only` attestation only when it is effect-free, terminating, free of unhandled traps, and
has a complete logical interpretation over the claimed domain. Recursive models require accepted
`#[decreases]`/well-foundedness evidence. Missing or invalid totality evidence leaves correspondence
`missing`/`unsupported`, or explicitly `assumed` through the trusted surface. Exact selection,
kernel-valid proof over the model, or finite differential agreement cannot manufacture totality.

**`ExportedContract`:** the package/caller interface, with a distinct `ContractIdentity`, covering:

```text
functional behavior
effects and capabilities
reads set
modifies/frame set
failure behavior
termination disposition
resource contract
assumptions and trusted boundaries
```

Every component records whether it is authored, inferred-exact, inferred-conservative,
runtime-checked, assumed or missing. Conservative inference is safe but weaker and must never
render identically to an exact or proved fact.

#### Attachment cardinality

Concrete deliberately does not impose F*'s one-interface/one-implementation restriction. One
implementation may have several legitimate claim scopes; one model may be reused across several
implementations; platform implementations may share one contract; and several proof modes may be
possible. The authoritative key is:

```text
(DefinitionIdentity, ModelIdentity, ClaimScope)
```

Relation kind and evidence mode are payload under a coherence rule, not key fields. Otherwise an
`equivalent` and `refines` row—or trusted and proved rows—could silently coexist as two answers for
one attachment. Zero matches is missing; more than one active row for the key is ambiguous—even if
byte-identical—and refuses. A stronger relation explicitly supersedes or retires the weaker active
row; it does not shadow it. The relation algebra records only valid entailments (at minimum,
`equivalent` entails its directional refinements, and a suitable refinement may satisfy a selected
`contract_only` claim). `implements`/`abstracts` direction and contract satisfaction remain explicit
rather than being forced into an unsound total order. Evidence-mode downgrade is trust
widening/laundering and requires an explicit transition, never coexistence.

#### Authoring surface and evidence modes

The intended source/proof distinction is explicit at authorship rather than reconstructed by a
report:

```text
certificate_by      independently checkable certificate
proof_object_by     retained/replayable proof object
trusted_proof_by    explicit trusted proof attachment
assume              explicit assumption
```

Exact syntax is a language-design decision, but these modes remain distinct types/dispositions.
No general `proof_by` fallback may silently choose among them. Generated reference use is
conceptually:

```lean
def model : PFnDef := { ... }
def table : FnTable :=
  FnTable.ofAttested [AttestedPFnDef.of model GeneratedImplementations.package_module_fn]
```

Raw package names, paths, digests and display strings are not author inputs. Failure to generate or
resolve the symbol is a typed refusal, never a placeholder authors repair by copying hex.

#### Correspondence dispositions

Model correspondence is independently retained as:

```text
proved
checked
assumed
missing
stale
ambiguous
unsupported
```

Attaching a model creates a named correspondence obligation whose default state is `missing`; it
does not discharge one and silence is never `assumed`. Following Why3's safe default, abstract
assumptions become obligations unless the author explicitly uses the assumption/trusted surface.
Runtime checks or finite exhaustive validation may earn `checked` only under their declared domain.
Signature agreement, identity agreement and proof validity do not earn `proved`.

**Near-term evidence path:** reuse the existing differential-lowering/reference-evaluator machinery
to compare the PFnDef model with implementation semantics. Exhaustive comparison earns `checked`
only over a declared finite domain; sampled comparison over infinite domains records its sample
strategy, seed/domain and translation-assurance strength and remains weaker. This is correspondence
evidence, not a kernel proof, and producer/shared-evaluator trust remains visible. A mismatch emits a
counterexample and refuses current correspondence. This turns the new axis into evidence rather
than bookkeeping without pretending sampling proves universal equivalence.

#### Three dependency strengths

Replace the eventual two-way contract/body simplification with three exact dependency kinds:

```text
contract edge        depends on ContractIdentity and exported evidence
model edge           depends on ModelIdentity and correspondence evidence
implementation edge  depends on exact DefinitionIdentity/body semantics
```

`trusted` and non-current `missing`/`unclassified` remain explicit. A contract-preserving body
change does not stale contract callers. It does stale implementation-edge callers. A model change
stales model-edge callers even when the executable body is unchanged. Edge kind is derived from
what the theorem/consumer actually uses, never selected as a freshness optimization flag.

#### Proof-aware package linking is partial

Define package evidence composition as a checked partial operation:

```text
linkEvidence : EvidenceBundle -> EvidenceBundle -> Except LinkRefusal EvidenceBundle
```

The linker refuses at least:

- duplicate or colliding definition identities;
- incompatible exported contracts or claim scopes;
- schema/codec/rule-set incompatibility;
- trust widening or assumption deletion/conflict;
- model/implementation substitution;
- ambiguous attestations;
- missing, stale or incomplete dependency closure;
- contract evidence weaker than the consumer policy requires.

Composition is not list concatenation followed by hashing. It preserves identity uniqueness,
propagates trust/assumptions monotonically, recomputes dependency closure/root, and records which
contract/model/implementation relation justified each imported claim. The package manager rejects
an upgrade that weakens a required contract even if ABI and names are unchanged.

#### Receipt and artifact bindings

Extend the canonical evidence schema and receipt with independently typed fields for:

```text
implementationReference
modelIdentity
relationKind
claimScope
correspondenceEvidence
contractIdentity
propositionIdentity
policyIdentity / rule-set identity
```

Receipts continue to bind theorem artifact, tables, environment/toolchain, dependency root, trust
and assumptions. A valid certificate copied to another implementation, model, proposition,
contract or policy refuses. Advisory/revocation state remains a validation-time registry input,
not a retroactively mutable receipt field.

#### Required adversarial matrix

Every item below has a positive control and a production-consumer mutation; a synthetic validator
probe alone is insufficient:

1. correct implementation, wrong model;
2. correct model, wrong implementation;
3. same callable/module name in another package, and the same declared package name/version from a
   different registry, repository, source namespace or origin identity;
4. same body with changed signature, capability, generic or contract facts;
5. same implementation with narrower claim scope;
6. refinement relabeled as equivalence;
7. trusted attachment relabeled as proved;
8. model changed while proof artifact remains fixed;
9. proof artifact changed while proposition/model remains fixed;
10. contract preserved while body changes—contract callers remain current;
11. model changed while contract remains equivalent—model callers stale;
12. one implementation with two legitimate claim scopes—both remain representable;
13. duplicate attachment for the same full key—ambiguous refusal;
14. package link with conflicting exported contracts;
15. certificate/receipt copied under another policy or rule set;
16. forged or manually constructed implementation reference—construction failure;
17. inferred-conservative dependency/effect presented as exact;
18. vacuous or empty exported contract receiving a strong proved presentation;
19. package scope removed from identity;
20. implementation scope removed from identity;
21. stale generated reference with a fresh proof;
22. model/body correspondence removed while logical proof remains valid;
23. implementation/model/contract identities swapped independently;
24. duplicate, surplus or unmatched package-link evidence discarded by filtering.
25. termination, trap-freedom or logical-interpretation evidence removed from a contract-level
    model attestation—the attestation and every dependent contract claim refuse or downgrade.

#### Documentation and reporting requirements

The normative object model lives in `docs/verification/EVIDENCE_ARCHITECTURE.md`; status vocabulary
in `VERIFICATION_STATUS.md`; author meaning in `PROOF_CONTRACT.md`; author procedure in
`PROOF_WORKFLOW.md`; hostile cases in `THREAT_MODEL.md`; product invariant in
`VERIFICATION_CHARTER.md`; proposition boundary in `VERIFICATION_IR.md`; shipped limitations in
`CLAIMS_TODAY.md`; theorem categories in `PROOF_THEOREM_SHAPES.md`; feature admission in
`docs/project/PROJECT.md` (design policy). No roadmap-only architecture counts as implemented or normative.

Reports expose at least these independent dimensions:

```text
implementation_selection
model_correspondence
logical_validity
dependency_closure
policy_acceptance
```

No single `proved`/green value may erase a weaker dimension. A kernel CVE can degrade logical
validity without rewriting selection/correspondence/closure; a source change can stale selection or
correspondence without claiming the old theorem became logically false.

#### Sequencing and exit gates

1. finish R-0004 exact implementation-reference migration and reach identity-safe 10/10;
2. make correspondence completeness mandatory for root usability and connect roots to verdicts;
3. finish the total contract-callable model fragment; an attested model that may diverge, trap, or
   invoke trusted execution cannot serve as logical contract semantics without an explicit weaker
   disposition;
4. introduce `ModelIdentity` with canonical sensitivity/invariance gates;
5. introduce `ModelAttestation`, explicit relation algebra, named default-missing correspondence
   obligations and differential checked correspondence;
6. generate author-facing typed references and remove raw attachment inputs;
7. complete R-0473/R-0474 typed contract records, identity-safe substitution, `old`, and frame/
   `modifies`; `ExportedContract` cannot honestly precede that substrate;
8. introduce `ExportedContract`/`ContractIdentity` with functional/effect/frame/failure/termination/
   resource components and provenance;
9. migrate dependency material to contract/model/implementation kinds;
10. implement partial package-evidence linking and behavioral compatibility checks;
11. extend canonical artifacts, receipts and standalone verifier;
12. run and mutation-kill the complete adversarial matrix;
13. publish a multi-implementation package whose body can change without staling contract-only
    consumers, while model/body consumers stale exactly as specified.

**Done when:** all identities are typed and domain-separated; authors write no raw identity/digest;
every attachment states relation/scope/mode; correspondence and logical validity are independently
replayable; exported contracts compose across packages; the partial linker refuses every unmatched
or weakened case; dependency strength produces the intended freshness behavior; receipts bind
code/model/proposition/contract/policy separately; and all twenty-five attacks are killed by the
production path without collapsing evidence into one status.

### Task R-0478

**Phase:** E

**Pull trigger:** R-0478: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The disposition merges both halves into R-0481 and R-0474 rather than removing their obligations. surviving requirement: Retain unique fact ownership and identity-based capture-safe contract substitution under their respective new-compiler owners. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** duplicate of R-0481 / R-0474: merge each half into its actual owner.

**Objective:** two refactors that are cheap individually and keep producing defects because they
are not done — recorded together because both are "the same thing exists in N places and nothing
holds them together".

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0478).

### Task R-0477

**Phase:** E

**Pull trigger:** R-0477: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The vcgen/calculus branch and authoritative walkers retire, but its body records lost obligations and a false-green regression. surviving requirement: Retain constructor-complete obligation accounting, nested assert/assume regressions and non-vacuous differential controls for verification built from new Core models. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep disposition task; retarget useful calculus work to R-0460/Core.

**Objective:** bring `vcgen/calculus` back onto current `main`, or retire it — it is tracked
nowhere in this file, which is how a branch quietly becomes a fork.

Four commits sit off `main`, based at `b97dfdf0`, which `main` has since passed by 80 commits:

* `66c56883` enroll obligation discovery in constructor coverage, with its measured limit;
* `d17be8a1` per-walker constructor coverage — found an eighth discovery defect immediately;
* `43e15a6c` obligations inside `assert`/`assume` were lost entirely, plus a false green made
  en route;
* `eaa3cf9b` split the VC calculus onto its own branch, off `main`, with no language change.

**Its earlier differential result must not be assumed to still hold.** That run reported the
calculus agreeing with the walkers on 41,807 obligations across 902 files with zero
disagreements — measured against a tree that predates contract validation, the source-order
language decision, and the evidence merges. Re-run the structured differential and the
merged-tree gates before treating that number as current.

**The hand-written walkers remain authoritative.** The differential has caught 3 defects in the
calculus and 0 in the walkers, which is exactly why the walkers are the oracle rather than the
thing being replaced. That does not change until a family graduates with its own negative
controls and a Register A row.

Sequencing: AFTER R-0473/R-0474, not before. Rebasing onto a foundation that is about to change
means rebasing twice, and the calculus is the natural consumer of resolved typed contract records
rather than of the AST it reads today.

### Task R-0473

**Phase:** E

**Pull trigger:** R-0473: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The seven old consumers retire, while the acceptance boundary explicitly forbids retaining a name-based AST after validation. surviving requirement: Build one identity-resolved typed contract record with precise scope, operand and result types, checked logical-call admission and declaration-level diagnostic and measurement controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; consume Resolve-owned identities; archive unrelated R-0004 history.

**typed contract records, and contract type checking that earns the name**

**Objective:** Replace the seven consumers' independent re-reading of raw `List Expr` contract
metadata with one checked, typed contract record, and close the H27 gaps that make the current
validation name-and-sort only.

Contracts were erased metadata that no pass validated. `#[requires(nosuchvar > 0)]` was accepted
and `nosuchvar` reached the HYPOTHESES of every obligation in the function, including ones
reported `proved_by_kernel_decision`. That failed closed — a hypothesis over a fresh universally
quantified variable is unprovable at the call site — but the failure surfaced in a *different*
function than the typo, and a precondition nobody can establish read exactly like one nobody had
established yet.

Name-and-sort validation now runs (`Concrete/Check/Check.lean`) and is deliberately not called
type checking. H27 records what remains accepted, each measured against the compiler and each
pinned by a gate assertion written to flip when the gap closes:

* ~~a boolean `#[variant]`~~ — **CLOSED 2026-08-07**. It was accepted because the proposition
  check *skipped* variants rather than holding them to their own rule; absence of a rule read as
  absence of a requirement. `i < n` does not decrease, it flips;
* ~~a contract calling an IMPURE (capability-requiring) function~~ — **CLOSED 2026-08-07**. Its
  meaning would depend on runtime effects, so it is not a proposition. Pure executable calls stay
  legal — purity is the property that matters, not spec-only;
* operand type compatibility — `#[requires(x < 9999999999)]` on an `i32`;
* a bool as an arithmetic operand — `#[requires((x > 0) + 1 > 0)]`;
* `result`'s type (its scope is enforced; its type is not);
* precise loop-binder scope — the bound set is every name bound anywhere in the function, so a
  name bound only *after* the loop is admitted and reaches the VC.

**Language boundary added:** purity alone is not the final contract-callable
criterion. R-0526 owns totality/admissibility for `spec fn` and checked `total fn`;
this task consumes that boundary when distinguishing them from ordinary runtime functions
and trusted functions. A `total fn` enters contracts only after effect freedom,
termination, trap obligations, logical interpretation, and partial-operation
preconditions close. Until that type exists, executable calls accepted by the
current narrow checker must not be documented as a general total fragment.
Coordinate with `#[decreases]`, recursion/loop total correctness, `old`, frame/
`modifies`, and the spec library rather than adding another call-name filter.

**Compiler measurement boundary:** declaration-level isolation must report
`valid`, `locally_invalid`, `blocked_by_invalid_dependency`,
`parse_unreachable`, `timed_out`, and `unmeasured`. Continue through independent
declarations using poisoned summaries; do not accumulate speculative cascades.
The current first-error population remains a named denominator until this
lands.

**The four that remain share one root cause, and it is the argument for doing this properly.**
Each needs the per-function environment: widths and operand domains need the types of locals,
`result`'s type needs the signature at the annotation site, and scope needs the bindings live at
the loop rather than "bound somewhere in the function". The two that closed did not need it, which
is exactly why they closed cheaply — and why the remaining four should not be attacked the same
way. Faking an environment in this module-level pass would be the same shape of mistake as the
walkers keeping their own weaker copy of the trap rules (R-0464): a second, weaker answer to a
question that already has an authoritative one.

The R-0004 Slice-6 record that accumulated here — migration-manifest closure (44/44), dependency-root
integration steps 1–8, the Slice-6 acceptance boundary and the three-digest boundary — closed with R-0004
and is archived in [ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md#r-0473-the-embedded-r-0004-slice-6-record-2026-08-07--2026-09-02). Its one live remainder is this task's and R-0474's:
contract dependency edges are witnessed conservatively today; exact contract witnesses replace them
(queue row 12).

**Acceptance boundary, from review and worth restating because it is easy to satisfy the letter
of this task and miss it:** the record must retain resolved identities and types. Rejecting the
six fixtures while still storing the original name-based AST expression would let VC generation
reconstruct exactly the unsafe information the record exists to eliminate — and R-0474 would
inherit the problem intact.

**Identity dependency (reconciled 2026-10-22):** R-0492 owns allocation of Resolve's
`LocalId`/`DefId`. This task consumes those identities through checked adapters for parameters,
locals, ghost bindings, loop binders, `result`, and callable references; it does not create a
second identity allocator during elaboration. R-0474 owns identity-based substitution and retires
name-based substitution after its evaluation-law gate passes. The `BindingId`/`CallableId` names
below describe contract-reference roles, not independent identity authorities.

The resolved reference must be a SUM, not a string with a companion type table — a string field
is how the name survives in practice:

```
ResolvedContractRef
  = local     BindingId
  | parameter BindingId
  | ghost     BindingId
  | result    BindingId
  | constant  ConstId
  | spec      CallableId
```

`BindingId` here stays the *internal lexical* identity from `docs/compiler/BINDING_IDENTITY_DESIGN.md` —
supplied by R-0492's Resolve-owned identities and regenerated every build. It must not be serialized
as stable evidence identity, which has to survive harmless source edits.

**Boundary caveat on the closed gap 6.** It rejects capability-requiring calls, which is narrower
than purity or totality: a capability-free function that traps, diverges, or is `#[trusted]` is
still admitted (all three measured). The rule that would be right — "resolves to a spec fn, or to
an executable function with an explicit, checked logical interpretation" — needs that second
notion to exist, and it does not. It belongs with the typed record.

**Diagnostic accumulation, narrowly.** The corpus denominator problem does not justify a
compiler-wide error-accumulation refactor before typed contracts. Enough: contract validation
collects all contract diagnostics for a declaration, runs once resolution and type information
exist, and lets one invalid contract not mask its siblings. Runtime-body errors may stay
first-error-wins. Reports must not be emitted from invalid typed records.

**Also in scope:** a diagnostic-accumulation or contract-only analysis mode. Checking stops at the
first error, so for 285 of 1249 corpus files the effect of contract validation is *unmeasured* —
a contract defect can sit behind an unrelated earlier error. Until that mode exists, the
defensible claim is "no unexpected rejection among the 964 files that reached contract checking",
which is materially weaker than "the corpus is compatible".

**Depends on:** R-0492. **Blocks:** R-0474.

### Task R-0474

**Phase:** E

**Pull trigger:** R-0474: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old substExpr refactor retires, but the stated substitution invariant and evaluation law survive. surviving requirement: Use capture-safe identity-based contract substitution with its evaluation law, keeping lexical IDs distinct from persistent evidence identity and retaining binding regressions. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep substitution work; move identity foundation to R-0492.

**lexical binding identities and capture-safe substitution**

**Objective:** Retire by-name substitution as proof infrastructure, and graduate H25 and H27's
shadowing restriction.

`refineObligations` instantiates a spec body by substituting parameters via `substExpr`, which
replaces by NAME with no capture avoidance. A binder in the body has the binding substituted away.
No unsound evidence escaped — the corrupted obligation reports `outside the fragment` because
`renderTerm` has no `.ifExpr` case — and *that is the finding*: soundness rested on an unrelated
downstream renderer's narrowness, and adding `.ifExpr` (an obvious extension the refinement tier
invites) would have made the corruption live with no test failing.

**The invariant:** refinement substitution may act only on expressions whose free-variable
identities are distinguished from all locally bound identities.

**Graduation condition:** substitution over identities, with the evaluation law
`eval(subst(Q, x, e), rho) = eval(Q, rho[x |-> eval(e, rho)])` proved or exhaustively gated over
the supported fragment.

Scope is fixed in `docs/compiler/BINDING_IDENTITY_DESIGN.md` before any editing, because this refactor
becomes invasive quietly. The load-bearing decision there is to keep **two** notions apart rather
than build one identifier for both: a *lexical binding ID* (internal, unique within one elaborated
function, regenerated every build, never serialized) and a *stable evidence identity* (canonical
across harmless edits, appearing in artifacts and fingerprints). Their requirements contradict,
and a lexical ID reaching stored evidence is a defect — a rebuild would read as a different
program. Only the lexical ID is in scope here. Monomorphization and inlining are explicitly out of
scope, with the point named at which the answer becomes mandatory.

**Currently contained, both temporary and both saying so in their diagnostics:** binder-bearing
spec bodies are rejected, and contracts naming a parameter shadowed by a local are rejected.
Shadowing is the gap unknown-name rejection cannot close by construction — every name resolves, no
diagnostic fires, and nothing records *which* binding the contract means.

**Depends on:** R-0473. **Blocks:** `old(...)`, frame/`modifies` conditions, reliable loop-state
reasoning, call-site contract instantiation, and quantified predicates with binders.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0478** (merged): duplicate of R-0481 / R-0474: merge each half into its actual owner. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0478)

### Task R-0475

**Phase:** F

**Pull trigger:** R-0475: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The shipped snapshot fix is historical, while the body explicitly retains origin-labelled reporting as the remaining increment. surviving requirement: Carry module origin from new project resolution into verification records and label project, dependency, stdlib and generated facts without suppressing dependency obligations. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Archive shipped snapshot fix; retain origin-labelled reporting increment.

**decouple fixture snapshots from the stdlib**

**Objective:** Stop a standard-library contract edit from silently invalidating a negative
fixture's golden file, and stop the resulting failure from pointing at the wrong place.

The H26 investigation resolved the immediate drift: `assume_taint` is one of 3 of 21
contract-negative fixtures carrying a `Concrete.toml`, so it compiles as a *project* against the
stdlib, and `a3ba761b` (R-0464) added contracts to `std/src/sha256.con` after the snapshot was
last written. The diff was 94 additions, 0 removals, nothing touching the fixture's own module —
verified before regenerating.

The defect that remains is structural: **a negative fixture's golden file transitively depends on
every contract in the standard library.** The next stdlib contract edit drifts it again, and the
failure presents as a regression in `assume_taint`, which is not where anything changed. The first
attribution in this investigation was wrong for exactly that reason.

Two workable fixes: scope the contracts snapshot to the fixture's own modules, or report "N stdlib
entries changed" as a separate line from fixture content so the next occurrence names the stdlib.
The second is cheaper and keeps stdlib coverage visible.

The 2026-08-07 exposure audit (exactly one project-mode snapshot was coupled) and the shipped fix
(`31710d4e`: the contracts comparison is scoped to modules the fixture defines, and that invariant is
asserted on every run) are archived in [ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md#r-0475-exposure-audit-and-shipped-snapshot-fix-2026-08-07).

**What remains, and why it was not done here.** The *compiler-side* improvement is to label the
report's sections by origin — "Source Contracts (this project)" versus "(dependencies)" — so the
structure carries the distinction instead of the test harness reconstructing it. That needs module
origin recorded where modules ENTER THE PROJECT GRAPH — not, as first written here, plumbed
through the parser. Whether a module is project-owned or dependency-owned is project-resolution
knowledge; the parser's job is spans and paths, and teaching it package policy would put a
resolution decision in the wrong pass. The shape is

```
ModuleOrigin { package/project identity; module identity; source path;
               local | dependency | stdlib | generated }
```

classified at project assembly and carried through Core into obligation records. What is missing
today is any distinction at all: `buildFnLocMap` stamps ONE file across all modules, so nothing
downstream can tell a project module from a stdlib one. Worth doing, and deliberately
not bundled into a snapshot fix. Note the design constraint discovered while scoping this: simply
dropping dependency contracts from the report would LOSE safety-relevant information, because an
undischarged obligation inside stdlib code still affects the user's program. The right shape is
separate labelled sections, not a filter.

### Task R-0472

**Phase:** E

**Pull trigger:** R-0472: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old walker and multi-kernel delivery assumptions retire, but the body identifies surviving property families and their proof prerequisites. surviving requirement: Preserve a reasoned verification-coverage inventory for traps, bitvectors, enums, induction, loops and heap reasoning, assigning relational properties to research and recording unsupported or trusted boundaries explicitly. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Split into existing Core/VC, totality and research owners; remove obsolete absence claims.

**what could be provable but is not yet, by reason**

Numbered R-0472 because R-0470 and R-0471 were already allocated — the first draft of this
section collided with the existing R-0470 (capability-variable inference), caught by grepping the
heading list rather than by anything failing.

Placed after R-0004 because it is the map of where proving goes next, and grouped by the REASON
each item is blocked — the reason determines the cost far more than the feature does.

#### Trivially close: the obligation kind simply does not exist

```
let x: i8 = -128;  return -x;      // traps at runtime. No obligation generated.
let f: f64 = 1e300; return f as i32;  // traps. No obligation generated.
```

Both trap in the compiled binary, both are expressible in all three kernels today, and nobody
wrote the rule. Under `docs/verification/VC_GENERATOR_DESIGN.md` these are **two table rows**. That the current
four-walker design made them expensive enough to skip is itself an argument for that design.

#### Blocked by one rung, already unblocked

Bitwise reasoning — `(x & y) ^ (~x & z)`, i.e. SHA-256's `Ch`. The model bridge is proved in Lean
against `BitVec 32`, the shared type is confirmed in all three (`Nat`/`N`/`nat`, the last needing
`unbundle bit_operations_syntax`); only the tier is unbuilt. **This is the one that unlocks real
corpus coverage**, because `hmac_sha256`'s meaningful property is exactly this shape.

#### Blocked by one missing rung: enums

Any property mentioning which variant a sum type holds. Needs constructor injectivity and
disjointness emitted per kernel — the same shape as the struct rung already built, and the last
quantifier-free construct missing.

#### Blocked by the recursion ban

```
fn sum(n: i32) -> i32 { if n <= 0 { return 0; } return n + sum(n - 1); }
```

Not provable, not even *eligible* — recursion is banned from the profile. With `#[decreases(n)]`
it becomes provable, and its termination obligation is **linear arithmetic**, which the existing
three-kernel machinery already discharges.

Everything downstream is gated on this: a sort is ordered, a parser terminates, an output is a
permutation of its input. All need induction; induction needs recursion admitted.

#### Blocked by a missing design

- **Loop-carried properties.** Bounds *inside* a loop are proved today; "this sum equals the total
  of `a[0..n]`" is not expressible, because there are no invariants. WP forces this question
  rather than answering it.
- **Heap and pointers** — needs a memory model, separation-logic shaped. Largest single item.
- **Constant-time / non-interference** — "runtime does not depend on `secret`" is a RELATIONAL
  property comparing two executions, and cannot be a per-site obligation at all. Handled today as
  a capability discipline, not a proof. Genuinely different machinery.

#### Never automatic

Termination in general (halting problem — hence author-supplied measures), and **print fidelity**:
that emitted Rocq text *denotes* the intended obligation needs a formal semantics of Rocq's
surface syntax, which is not ours. Validated, never proved.

#### Shape of the list

The nearest wins — `MIN` negation, casts, bitwise, enums — are small and mechanical. The big jump
is `#[decreases]`, because it converts an entire category from "not eligible" to "provable", and
it is cheaper than it sounds since its own obligation is linear arithmetic. The genuinely hard
ones are heap and relational properties, and both need design before code, as the VC generator
did.

### Task R-0466

**Phase:** D

**Pull trigger:** The new compiler can run the fixed authoring and diagnostic-repair corpus against its generated specification and stdlib manifest.

**Reconciliation (2026-10-19):** Keep under R-0515; do not subordinate human pilots to it.

**Objective:** Measure whether the language is legible to the consumer that
actually writes it — a language model — by gating two numbers in CI: can a model
author correct Concrete from the documentation alone, and can it repair a
rejected program from the diagnostic alone.

The premise, stated because it reorders other tasks: Concrete's code is written
by models, so the ergonomic cost the external-validation trial was designed to
price is largely not charged. A model does not resent `defer x.drop()` or
`with(Console)` — keystrokes are the cheapest thing it spends, and linearity plus
capability headers are close to free on the consumer side. What a model cannot do
is guess. So explicitness is not one virtue among several here; it is the whole
adoption surface, and every rule that lives only in a maintainer's head, and
every rejection that does not name its own repair, is a defect rather than
friction.

Two measurements, both regression-tracked so a change can be shown to move them:

- **Authoring.** Give a model the language reference and stdlib manifest and
  nothing else; ask for N programs against fixed prompts; record the fraction
  that compile and, for each failure, which invariant was violated. The output
  that matters is not the score but the histogram: an invariant that models
  breach repeatedly is one the documentation does not state, and that is a
  reference defect with a named location.
- **Repair.** For each rejected program, feed back ONLY the diagnostic — never
  the fix — and measure rounds to green. One round is the bar. A diagnostic that
  cannot close the loop at all is the strongest possible finding, and bug 064 is
  the existence proof: it names a token unknown while displaying that token among
  the known set, so no repair is derivable from it in any number of rounds.

Seed the corpus from what exists rather than inventing one, and it is already
large enough to start: measured 2026-07-31, `tests/wrong-code/cases/` holds 23
documented WC cases and `tests/invalid_programs/` holds 15 programs, plus the
`docs/bugs/` repros — roughly 38 rejected programs with known intended causes
before anything is written. Report that denominator with both numbers; a repair
rate without its corpus size is the single-badge failure this project refuses
everywhere else.

Why this sits here, ahead of the prover-neutral arc: it is the cheap empirical
check on the language surface, in the same relation to R-0137 that R-0462 has to
R-0460 — an afternoon-scale continuous measurement standing in for an argument
that would otherwise be settled by assertion. It also front-runs R-0137's three
human sessions without replacing them (a diagnostic that defeats a model defeats
a newcomer), and unlike recruiting, it runs per commit. R-0137 owns the
diagnostics bar itself and the measured 12-of-15 empty-hint baseline; this task
owns the harness and the two numbers.

Kill criterion, per the spike-first rule: if authoring failures cluster on
prompt-shape or harness artifacts rather than on identifiable language or
documentation facts, the trial is measuring the harness and should be stopped
rather than tuned.

Definition of done: the harness is deterministic enough to gate (pinned model
version and prompts recorded in the artifact, so a number is comparable across
runs or is explicitly marked incomparable); the repair number is reported per
diagnostic code, not as one average, so a regression names its own code; and a
mutation that blanks a populated `hint` must make the repair number visibly
worse. Report both numbers with the corpus size, and never a single green badge —
same no-erased-dimensions rule as R-0440.

### Task R-0469

**Phase:** E

**Pull trigger:** R-0469: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The immediate summary fix is historical, while the remaining exit is a class-level recursion and stack-report gate. surviving requirement: Validate stack-unboundedness propagation across the new call graph and recursion reports, refusing budgets when reachable recursion lacks a bound and retaining reachability controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep immediate fail-closed repair; native resource proof remains separate.

**Objective:** Fix bug 065 — make stack unboundedness propagate through the call
graph, and give `--report stack-depth` and `--report recursion` the gate neither
has.

`computeCallDepths` filters recursive callees out of every caller's callee set
(`Concrete/Report/Report.lean:496`), so a `main` calling recursive code reports
`depth: 0  stack: 8 bytes` and the summary prints `Max stack bound: 8 bytes` as
the program maximum. A function is unbounded if it is itself recursive OR reaches
anything unbounded; render the reached callee by name, and report the summary
maximum as `unbounded` rather than maximizing over the bounded subset.

**The number is a policy verdict and a published evidence field, which is why
this is not cosmetic.** `scripts/tests/check_policy.sh:161` greps
`Max stack bound:` and enforces `[policy] max_stack_bytes`;
`check_assumptions.sh:128` does the same for `[allocation] stack_max_bytes`; and
`capture_release_bundle.sh:128` publishes it as `evidence.max_stack_bytes`. The
fail-closed branch that should catch this cannot fire — it triggers when the
report has NO numeric max, and the defect guarantees a finite one is always
printed. So a budget would pass on unbounded stack rather than refuse it.

Not live: all five budgeted regression/evidence projects (`crypto_verify`, `parse_validate`,
`fixed_capacity`, `hmac_sha256`, `constant_time_tag`; only HMAC remains in the ratified public
flagship portfolio) are
recursion-free, so every enforced number is currently correct. That is a property
of the corpus, not the mechanism, and it is the reason this sits after R-0471 and
R-0470 rather than at the front: lowest urgency of the three, but an unpredictable
trigger, so not late either. **It must precede any work that implements or
tightens a stack budget.** If a recursive function lands in one of those five
first, this becomes an H-numbered evidence hole with no further investigation
needed.

Gates, since `scripts/tests/check_stack_depth.sh` does not exist and that absence
is how the defect shipped: a fixture with direct recursion, mutual recursion, and
a bounded caller of each; a class-level assertion that no function reported with a
finite `stack:` can reach one reported `unbounded`, cross-checked against
`--report recursion` — which is itself currently ungated, has zero references in
`scripts/tests/`, and is folded in here because it is the same script and the
cross-check needs both reports; a negative budgeted project with recursion that
`check_policy.sh` and `check_assumptions.sh` must REFUSE; and confirmation that
bundle consumers tolerate a non-numeric `evidence.max_stack_bytes` — the fallback
is `"?"` today, so prefer an explicit `"unbounded"`. A mutation restoring the
`!isRecursive c` filter must fail the gate.

The summary and its consumers are fixed (bug 065, 2026-10-18): `Max stack bound:` reads
`unbounded` whenever any listed function has no bound, the policy gate, the assumptions gate and
the release bundle read it through `scripts/tests/lib/stack_bound.sh` (an unbounded stack refuses
every budget; the bundle publishes `"unbounded"`), and `check_stack_budget.sh` gates that with a
recursive budgeted fixture and a bounded positive control. What remains of this task is the
class-level reachability assertion cross-checked against `--report recursion`, and a gate for
`--report recursion` itself.

### Task R-0450

**Phase:** E

**Pull trigger:** R-0450: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: Promoting old TermIR and merging old VC renderers retire, but proposition integrity and consumer substitution tests survive. surviving requirement: Use one validated proposition semantics and canonical identity for any new external-proof interface, with explicit unsupported cases, renderer agreement, independent decoding and wrong-goal certificate controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; generate obligations against Core, not a new SSA proof authority.

**Objective:** Unify obligation lowering into one prover-neutral IR with per-backend drivers (Why3 shape) — implementation already in progress off-repo; land it in a worktree and merge once green per the operating rules. The problem is duplicated expression lowerings of the same obligations: several answers to one question, free to drift — and drift here means the Lean proof and the external check silently prove DIFFERENT obligations, an evidence-integrity defect in the system R-0004 is hardening. Define one typed obligation IR (linear integer arithmetic, bitvectors, bools, arrays: the deliberate intersection fragment; everything else `not_supported`), one semantics (a single `eval` in Lean, the IntArith single-source discipline), lowering as small named transforms, and per-backend drivers that select which transforms run.

**Ownership decision:** promote `Concrete.Semantics.TermIR`; do not mint a
parallel VIR. TermIR owns internal proposition syntax and semantics. VIR is the
validated, versioned public codec/digest boundary described in
`docs/verification/VERIFICATION_IR.md`. ObligationCore owns provenance/status and references
the canonical proposition identity. Receipts and policy remain separate.

Current TermIR is a seed, not yet a typed authority: `Srt` exists beside an
unindexed `Term`, and variables/symbols are strings. Add `RawTerm`/`RawGoal` to
closed `ValidatedTerm`/`ValidatedGoal` validation (or an equivalent
intrinsically sorted representation), stable identities and symbol signatures,
then permit only validated goals into renderers, profiles, digests, and receipts.

**Promotion slices:** inventory current obligation shapes/refusals; validate
sorts/arity/width/context; define canonical context and full proposition digest;
specify hostile canonical bytes; implement a second parser; attach exact
certificate profiles; migrate one family in structured shadow comparison; make
all renderers consume validated goals; bind ObligationCore/receipts; delete
direct `Expr -> prover string` paths. Arrays and records are reviewed semantic
extensions, not incidental constructor additions.

Acceptance includes alpha/order invariance, semantic-change sensitivity,
malformed/ambiguous context refusal, digest context binding, parser agreement,
wrong-goal certificate swaps, renderer constructor totality, and a mutation
showing no legacy string path can retain a friendly verdict.

This task's pulled-forward position is conditional: it may land representation,
semantics, and differential gates after R-0004 only when the existing branch is
merge-ready. It does not issue receipts or upgrade any verdict to authoritative
evidence; if it is not ready, R-0006 proceeds rather than waiting for it.

   **The count has grown since this task was written, and the growth is the
   argument.** It was two lowerings (`toLeanProp`, `exprToSmt`). Today there are
   three expression lowerings — `toLeanProp`
   (`Concrete/Report/ReportVC.lean:592`), `exprToSmt`
   (`ReportObligations.lean:791`), `exprToLeanProp` (`:863`) — plus
   `exprToProver` (`:930`) with four driver instances (`rocqLowering`,
   `isabelleLowering`, `rocqNiaLowering`, `isabelleSmtReplayLowering`).
   `exprToProver`'s own comment reads "Same linear fragment as
   `exprToLeanProp`": the fragment is now *defined twice*, in prose, in two
   functions that must agree. That is the exact drift this task exists to
   remove, and every kernel added before it lands adds another copy.

   **The generator feeding this IR may change under it (R-0464).** That task moves
   obligation generation from the surface AST to SSA. It does not alter this
   task's shape — the IR still receives an obligation and lowers it — but the
   terms arriving will be over SSA names, so do not bake source-level naming
   assumptions into the transforms. Landing this IR first is preferable, so the
   SSA generator emits into it rather than into today's ad-hoc lowerings.

   **Point the agreement technique at the older paths.** R-0448 invented a way to test
   that a rendering denotes the obligation without writing a parser per target: pin the
   driver's own output to ground assignments, make the target decide it, compare against
   `evalBoolEnv`. Status 2026-08-03: `exprToSmt` IS now checked this way
   (`smtAgreementGoals`, wired at all three consumers) — an earlier version of this
   paragraph said it was not, and that stale claim misdirected a round of prioritisation.
   Lean's own rendering is still unchecked and is the only remaining place validation is
   asserted by construction rather than minted.

   The reason once given for that — "its rendering is the reference the others are
   validated against, so nothing exists to validate it with" — is FALSE. The agreement
   check validates a rendering against the reference EVALUATOR (`safeOn`/`evalBoolEnv`,
   which walk the AST); Rocq and Isabelle are not compared to Lean, they are compared to
   what the expression means. The real obstacle was that Lean's lowering was not
   expressible as a driver, which this task's 2026-08-03 slice removed: the linear fragment
   is lowered by one parameterised function (`exprToProverU`) and `exprToLeanProp`
   delegates to it. What remains is to run the agreement scripts through a Lean driver and
   mint its witness.

   Slice 1 unifies the *existing* backends only — no further provers; the diff
   should be net-negative in lowering code. Note the original "two existing
   backends" framing was overtaken by R-0448's spike, which landed four prover
   drivers ahead of the IR: absorbing them is now part of slice 1's scope, not
   a follow-on. Gates: constructor coverage
   (every transform/emitter handles or explicitly rejects every IR
   constructor, fail-closed), Lean/SMT verdict agreement on generated
   obligations including identical `unsupported`s, and identity carried as
   resolved callable references (R-0442's output), never name strings.
   R-0170's auto-discharge must generate INTO the IR, not the ad-hoc
   lowerings. Version-stamp the IR in every emitted artifact so R-0004
   receipts record what produced them.

### Task R-0006

**Phase:** D

**Pull trigger:** R-0006: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old Mono rewrite retires, while the body names remaining container and failure-path regressions. surviving requirement: Implement structural type-directed destruction for admitted arrays and containers, preserving exactly-once counters, nested and partial-failure cases and no-op fallback mutations. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep containment now; integrate proof obligations into V3/V4.

**Objective:** Fix bug 052 — array element destruction becomes a no-op Immediate containment: reject `T: Destroy` for arrays/unnamed element types whose drop glue cannot be resolved; never synthesize an empty destroy function from “name lookup missed.” Root fix: structural, type-directed drop-glue identity for arrays and nested aggregates, independent of `tyName`. Gate destructor counters for arrays of linear values in Vec/Deque/heap containers, nesting, partial construction/failure, clear/remove/drop, exactly-once behavior, and a mutation replacing structural glue with the old no-op fallback.

   The bug 052 fix landed the containment and the array root fix: Mono
   decides a missing `_destroy` from the argument type (per-element glue for
   `[E; N]`, nested and generic elements included; the empty no-op only for
   Copy; E0814 for unresolved or destructor-less types), Check's
   `destroyable` lost its accept-anything catch-all, and the interpreter
   destroys array elements too. Gated by six legs in
   `check_collections_copy_only.sh` (Vec drop/clear, nesting, `[Vec<T>; N]`,
   Deque, compiled vs `--interp`, two refusal controls). What remains here:
   BinaryHeap and keyed-container legs with array elements, partial
   construction/failure, the mutation that restores the no-op fallback, and
   retiring the name-based `{Type}_destroy` rewrite for named types in favour
   of type identity.

### Task R-0007

**Phase:** C

**Pull trigger:** R-0007: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old monomorphized-name repair retires, but the body explicitly preserves function-symbol and collision requirements. surviving requirement: Keep semantic identities separate from injective versioned link symbols and retain collision, deterministic-symbol and metamorphic specialization controls. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0492/R-0510: merge, preserving function-symbol and metamorphic controls.

**Objective:** Fix bug 054 — non-injective monomorphized names First fail closed on every generated/user symbol or type-name collision. Then separate semantic `TypeId`/`FunctionId` from link/display symbols and use an injective, versioned mangling encoding that user identifiers cannot forge. Layout lookup is by identity and missing fields diagnose instead of returning a past-end offset. Gate ambiguous type-argument boundaries, user names resembling specializations, module/basename pressure, deterministic symbols, and collision mutations.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0007).

### Task R-0008

**Phase:** C

**Pull trigger:** R-0008: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old alias-string repair retires, while the objective requires identity preservation across all import forms. surviving requirement: Resolve imports to canonical definitions once and preserve alias, generic, module-order and undefined-symbol regression controls through new lowering. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0492: merge reproducer and alias permutations into identity migration.

**Objective:** Fix bug 055 — sibling renamed import emits an undefined callee Resolve an import once to canonical definition identity and carry that identity through Mono/codegen; do not repair only one alias-string orientation. Gate plain and generic sibling imports, qualified/unqualified and renamed forms, duplicate basenames, module-order permutations, and undefined-symbol failure injection. This is a rejected-valid-program bug unless a wrong-code witness appears.

### Task R-0434

**Phase:** C

**Pull trigger:** R-0434: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old declaration defect is historical, but the body explicitly identifies the missing general store-bound invariant. surviving requirement: Establish that every aggregate payload store fits its declared representation through new layout certificates and memory verification, retaining discriminating overflow controls. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0502: merge into layout certificates and Memory CFG verification.

**Objective:** Verify that every aggregate payload write fits its emitted declaration Bug 051's defect was a payload written through a declaration too small to hold it. R-0001 fixed the cause (one declaration per enum instantiation) and closed the forged-name variant fail-closed (E0809), so no known program reaches the shape today. What is still missing is the general invariant: nothing checks, for an arbitrary program, that a store through an aggregate actually lands inside the aggregate that was declared.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0434).

### Task R-0437

**Phase:** C

**Pull trigger:** R-0437: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old silent-no-op repair retires, but the disposition explicitly preserves declaration and specialization checks. surviving requirement: Validate generic bounds against a closed resolved vocabulary at declaration and specialization, rejecting unknown or unsatisfied bounds with distinct diagnostics. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0514/R-0510: merge declaration and specialization checks; preserve distinct diagnostics.

**Objective:** Give generic bounds a closed, validated vocabulary — an unknown
bound name is currently a silent no-op.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0437).

### Task R-0444

**Phase:** D

**Pull trigger:** The new compiler runs the numeric, PNG and ELF parser paths with scoped bytes, callbacks and checked offset arithmetic.

**Reconciliation (2026-10-19):** Keep implementation workload; proofs follow Core/VC prerequisites.

**Objective:** Pull the fired parser trigger: build a combinator core for binary/text parsing and port the duplicated spellings onto it. R-0406's gate was "binary-format DSLs only if packet/ELF examples show repeated parser boilerplate" — the 2026-07-25 audit count found it already fired: seven combinator shapes spelled ~45–50 times across std.numeric, std.parse, std.base64, std.hex, std.cli, std.text, and five example parsers, with `numeric.con` writing the same bounds-guard-plus-endian-fold sixteen times. The seven: `take(n)` bounds-guard-then-advance, `tag` literal match, length-prefixed consume, bounded repeat over remaining, `take_until`/`skip_while` scan, byte→`Option` table map, verify-over-region — plus one pure fixed-width assembly fold.

   Slice 1 (no Lean): the seven combinators + assembly fold as a
   `std.parse` module; port `numeric.con` onto it (deletes more code than
   it adds). Gate against the existing ByteCursor/byte_view fixtures. The
   length-prefix overflow obligation (attacker-controlled length in offset
   arithmetic) lives in the length-prefixed combinator ONCE, checked,
   instead of being re-derived per parser by width accident — PNG's
   `off + 12 + dlen` is unreachable today only because `be32` caps the
   length, not by construction.

   Slice 2: port png_chunks and elf_header. elf_header is the demo: its
   validation core reads via trusted pointer reads with no bounds relation
   to the bytes requested (it predates ByteCursor); after the port the
   trusted boundary shrinks to fopen/fread and the core is
   combinator-built — same parser, smaller boundary, one diff that tells
   the whole story.

   Proof handoff, deliberately outside this early task: R-0170 proves the
   seven combinators + fold once after operational auto-discharge has an exact
   semantic foundation, so parsers built from them can inherit
   memory-safety/non-malleability obligations instead of accumulating
   hand-written bridge theorems. R-0244 owns the DNS message parser and its
   compression-pointer bound: cycle-prone backward offsets must show “cannot
   loop more than N times the input length,” not merely reuse an index-loop
   argument. Claims apply only to parsers built from the combinator core;
   Concrete remains a general language and nothing stops hand-rolled pointer
   arithmetic outside that discipline. Never claim “Concrete parsers are
   safe.”

   Composes with: R-0244 (bounded repeat exposes its symbolic iteration
   bound as `complexity_guarded`), R-0443 (a combinator-built parser is the
   first real customer for whole-program authority certificates), and R-0442
   (proof links survive extraction). DSL generation proper (EverParse-style
   spec→source) stays research-gated in R-0406 until the R-0170 combinator
   proof leg exists.

   Immediate handoff after slice 2: the numeric, PNG, and ELF ports are the
   first forcing corpus for R-0445's source-level resource certificate. The
   task is complete at that handoff; do not hold the early resource report
   behind R-0170/R-0244 proof work. The ports must expose bounded repetition
   and allocation facts through the shared schema rather than grow
   parser-private cost annotations.

### Task R-0445

**Phase:** E

**Pull trigger:** The new compiler exposes canonical loop, call-target, layout and allocation facts for the numeric, PNG and ELF resource corpus.

**Reconciliation (2026-10-19):** Keep source scope; do not substitute for native sufficient-resource theorem.

**Objective:** Ship a compile-time source-resource certificate early, before
broad stdlib expansion. For each monomorphized function and selected program
root, `--report resources` (human and deterministic JSON) reports a conservative
upper bound or an explicit `unknown` for:

- source-model execution steps, as a constant or symbolic expression over
  named input bounds;
- maximum call depth;
- source-model stack bytes under a stated source layout model;
- cumulative allocation count and allocated bytes;
- peak live allocated bytes when the source ownership/lifetime facts suffice,
  otherwise an explicit `unknown`; and
- blocking, extern, trusted, or opaque boundaries that prevent a complete
  bound.

Every fact carries its subject identity, inputs, assumptions, provenance,
completeness, a versioned cost/layout-model identifier,
`semantic_scope = source`, and—when incomplete—a stable blocker code plus a
remedy-oriented explanation. V1 publishes the small weighting table that
defines a source step. Its stack value is the maximum sum of source-frame
footprints along a call path under the named source layout/target-width model;
it excludes backend spills, red zones, prologues, and ABI frame growth. A
missing fact is never converted to zero. Cumulative allocated bytes and peak
live allocated bytes are distinct facts and must never share a label or value
by convenience. A source-step or source-stack bound is never presented as
elapsed time, cycles, native frame size, or a hardware WCET claim.

Every symbolic input bound names its origin: a checked syntactic bound, a
validated contract/obligation, a profile/policy fact, or an explicit trusted
assumption. An arbitrary annotation or unchecked caller promise cannot become
a usable bound; it remains `unknown` or is surfaced as an assumption with its
scope and trust dependency. Substitution must preserve this provenance through
the call graph.

Build the certificate from canonical loop, call-edge, target-set, layout,
allocation, and stack facts. Do not add a private AST walk or second cost truth
source. Pull forward only the minimal shared resource-fact family that later
R-0178 will stabilize. Conditionals compose by conservative maximum, sequential
work by checked sum, direct calls by substitution, and indirect calls over the
conservative union/maximum of possible targets. Recursion, an unbounded loop,
an incomplete indirect target set, FFI/trusted code without a resource
contract, overflow in the bound expression, or an opaque intrinsic produces
`unknown` with the exact reason.

Use R-0444's fixed-width assembly, numeric parser, PNG, and ELF paths as the
first real corpus. Include one non-author session focused on whether the report
and its `unknown` remedies can be understood and acted on; record it as one
attempt in R-0137, but do not let it replace the later three-user validation
trial or its completion criterion.

Gate constant, linear, and nested-loop bounds; branch maximum; direct-call
composition; indirect-target maximum; zero-, fixed-, and symbolic cumulative
allocation; peak-live reuse versus simultaneous-live allocation; bound
provenance and rejection of an unchecked bound; stack/call-depth composition;
and honest unknowns for recursion, unbounded iteration, incomplete lifetime
facts, FFI/trusted boundaries, and opaque calls. Mutations that drop a loop,
call, stack, cumulative-allocation, or peak-live term must fail. Report
formatting and JSON must be deterministic, and adding the report may not change
interpreter or native behavior.

This is the early usability and design-pressure layer. R-0244 later turns the
same symbolic bounds into dischargeable obligations, R-0224/R-0248 bridge and
enforce target stack budgets, R-0416–R-0419 measure generated code, and R-0405
researches hardware WCET under a fixed target model. Backend cost preservation,
formal discharge, benchmark infrastructure, and hardware timing are explicit
non-goals here.

   Falsification probes before the machinery: (1) hand-write the symbolic
   bound for five real functions (HashMap.find_slot, one parse loop, one
   Vec grow, two others) — if the bounds cannot be stated cleanly for real
   std code, the cost model is wrong; (2) the step-count vs measured-time
   correlation experiment on the R-0416–R-0419 workloads — poor correlation
   kills every timing-adjacent reading early, by design.

### Task R-0011

**Phase:** C

**Pull trigger:** R-0011: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old multi-pipeline wiring retires, while the body defines language visibility and interface requirements. surviving requirement: Enforce private-by-default construction and representation access at exact module boundaries, preserving exhaustiveness, interface-leak, alias and trusted-non-bypass controls as modules arrive. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; enforce through authoritative Core/front-end boundaries.

**Objective:** Finish construction rights with private-by-default enum variants Struct-field privacy and direct-newtype construction are historical milestones, recorded in the changelog and TYPE-11 of `docs/spec/types.md`. Finish the same one-keyword model for the still-open construction paths: `pub` remains the only visibility word; exporting a type never implicitly exports its variants or raw representation.


- `pub enum T` exports the type name. A variant is externally constructible and
  matchable only when marked `pub`, for example `pub Ok { value: T }`. The
  marker governs the whole variant payload in v1; do not add payload-field
  visibility or a second `private`/`sealed`/`opaque` vocabulary.
- Preserve the existing newtype spelling (`pub newtype N = u32`) and make raw
  payload projection/unwrap obey the same defining-module rule as construction.
  Public checked constructors and accessors are the ordinary cross-module path.
- Representation privilege follows the exact defining module. External impls,
  parent/child/sibling modules, `trusted`, `Unsafe`, direct/renamed/wildcard
  imports, and umbrella re-exports do not widen variant or constructor access.
- Define exhaustiveness before migration: a client unable to see all variants
  must use a wildcard, and diagnostics must not reveal inaccessible payload
  structure or synthesize private patterns.
- Public interfaces may not transitively leak a module-private type through a
  public variant, callback, generic argument, capability signature, alias, or
  nested composite. ABI/layout knowledge remains separate from source access.
- Compiler-generated Copy/Destroy/equality/formatting/serialization/layout,
  manifests, debug views, and backend glue may inspect representation without
  exposing it. Visibility changes must update interface hashes, API snapshots,
  manifests, dependency invalidation, and docs.
- Mechanically mark intentionally public stdlib variants, audit invariant and
  state-machine enums rather than exporting them reflexively, and apply the
  same result in Resolve, Check, Elab, projects, interpreter, LLVM, and QBE
  artifacts.

Acceptance: positive public construction/matching and negative private
construction/matching fixtures through direct, renamed, wildcard, and re-export
paths; wildcard/exhaustiveness and stable diagnostic/span fixtures; external-
impl and `trusted` non-bypass tests; interface-leak tests; interpreter/compiled
parity; API/manifest snapshots; and mutations that accidentally export a
variant or omit one enforcement path.

### Task R-0012

**Phase:** D

**Pull trigger:** A new-compiler workload needs consuming Option<String> or Result<String,E> map or and_then without manual ownership matches.

**Reconciliation (2026-10-19):** Keep; use ordinary ownership rules and V3/V5 proof coverage.

**Objective:** Generalize workload-pulled Option/Result combinators for linear payloads This is an ergonomics/ownership blocker, not a current soundness defect, but workloads already pay the manual-match tax. Do not merely delete Copy bounds: prove arm-wise conservation and explicitly dispose of every unused owned default/error.


- Reuse the checker's existing branch-join, divergence, return-path, and linear-
  conservation machinery. Combinators are ordinary functions and must not gain
  callback- or Option/Result-specific ownership exceptions.

- Support at least `Option<String>.map/and_then` and
  `Result<String,E>.map/and_then/map_err` where each present payload is consumed
  or passed through exactly once.
- Design `unwrap_or`, `ok`, `err`, and `ok_or` separately because the unused
  default or opposite payload may require `Destroy`; do not hide disposal
  behind Clone, leaks, or broad Copy bounds.
- Gate transformed, passed-through, short-circuit, callback-failure where
  expressible, and unused-payload paths with destructor counters; add negative
  fixtures for leak/double-consume callbacks and open match arms.
- Require interpreter/LLVM agreement, proof fingerprint/spec updates, and
  representative kernel proofs to remain alpha-invariant after generalization.

### Task R-0013

**Phase:** C

**Pull trigger:** R-0013: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep confirmed contract defects ahead of API breadth.

**Objective:** Close confirmed boundary correctness holes

1. Validate UTF-8 in `std.fs.read_to_string`, just as `env::get`, argv, and
   `Bytes::to_string` do. Prefer `Result<String, ReadTextError>` with distinct
   filesystem and `InvalidUtf8` cases. A file containing invalid bytes must
   remain available through `read_file -> Bytes` and must never become a
   contract-violating `String`.
2. Replace unchecked public C-string conversion at hosted boundaries with an
   owning checked `CStr`/`CString` adapter that rejects interior NUL and always
   terminates. Translate `InteriorNul` into `FsError`, `EnvError`, `NetError`,
   or `ProcessError` at the owning boundary. Keep raw conversion private or
   explicitly `_unchecked`. Gate all 13 C-string FFI consumers with empty,
   exact-capacity, embedded-NUL, invalid-UTF-8 where relevant, and normal cases.
3. Audit every external return value before constructing a typed success, but
   promote a site only with a concrete failure witness or a normative contract
   that already requires propagation:
   `fseek`/`ftell`/`fread`/`fwrite`/`fclose`/`fflush`, `clock_gettime`,
   `nanosleep`, socket operations, environment mutation, and process calls.
   Negative sizes must never cast to huge allocation sizes; zeroed FFI storage
   must never be returned as a fabricated `Instant` or address after failure.
4. Correct two already-ratified hosted contracts: `env.set`/`env.unset` return a
   typed environmental failure instead of unconditional Unit, and `args::get`
   no longer maps both out-of-range and invalid OS bytes to the valid empty
   String. Preserve a simple UTF-8 accessor; add a raw-bytes argv API only when
   a workload actually needs non-UTF-8 arguments.
5. Make `random_range(lo, hi)` enforce its documented half-open-range
   precondition by trapping when `lo >= hi`; the current `hi == lo -> lo`
   special case contradicts the bucket-2 invariant policy. Pin equality,
   reversed bounds, ordinary ranges, and overflow edges. A recoverable
   `checked_range`/`try_range` remains a separate workload-pulled API, as does
   any explicit Rng/system-entropy redesign.

**Additional input (2026-10-02, owner confirmed 2026-10-04):** R-0484's
construction/caller audit (`docs/stdlib/HANDLE_CAPABILITIES_AUDIT.md`) assigns F7/F8 to
item 3 here: ignored write/close results (`console_err_write`, `TextFile::write`,
`TextFile::close`, `fs.File::close`, `TcpListener::close`, `TcpStream::close`) and unchecked
`ftell` failure in `fs.read_file`/`fs.read_to_string`. Repair each with an injected-failure
regression and a successful-operation control; preserve explicit cleanup and report any
failure-path limitations. **Validation is R-0030's:** the injected failures run through its
deterministic fault schedules at the `Writer`/`File` seam, so R-0013 owns the repair and
R-0030 owns the dynamic evidence that the error now surfaces — neither duplicates the
other, and R-0485 does not own these.

Gate: `scripts/tests/check_stdlib_boundary_correctness.sh`, mutation-tested at
each discarded-error/unchecked-conversion site. Add a source inventory that
fails when a hosted module introduces unchecked `String` construction from
external bytes or an ignored classified FFI result without an explicit
justification row.

Boundary rule: validate UTF-8 only when an API promises text. File, process,
environment, argument, and network byte surfaces preserve `Bytes`; explicit
String/Text adapters validate and return the declared absence/error shape.
Never force arbitrary process output, argv bytes, paths, or network payloads
through UTF-8 merely to make adjacent APIs symmetric.

`std.io.read_all` is explicitly not a confirmed hole. Its workload-2 parser
contract intentionally ends the drain on a Reader error and returns partial
bytes so the parser reports a short/invalid payload. Preserve that behavior and
its documentation. Revisit only when a socket-to-Reader workload demonstrates
that EOF, partial data, and mid-stream transport failure need a different API;
then choose an explicit split such as fallible `read_to_end` versus a named
partial-read result rather than retroactively calling the existing contract a
bug.

### Task R-0014

**Phase:** C

**Pull trigger:** R-0014: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old wrapper migration and checker patch retire, while safe boundaries and trusted ownership duties survive. surviving requirement: Expose exact domain authority through safe hosted wrappers and narrow trusted adapters, retaining raw-memory provenance, non-Copy dereference and manifest regression controls under the decided authority model. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep library migration; merge checker rules into R-0514/V6.

**Objective:** Put Unsafe behind domain-capability wrappers and close trusted dereference duplication Split hosted modules into ordinary public wrappers and private trusted/raw adapters:


```text
public typed API: with(File | Env | Time | Network | Process | Console | Random)
        -> private trusted adapter: with(Unsafe), pointers, ABI/platform values
        -> std.libc/raw platform declarations
```

Ordinary `File::open`, `env::get`, `Instant::now`, `TcpStream::connect`,
`TcpListener::accept`, `Child::wait`, and random acquisition must not require
callers to add `Unsafe`. Allocation remains visible when the returned operation
allocates. Keep `std.alloc`, `std.ptr`, raw buffer constructors, FFI helpers,
and explicitly named `_raw`/`_unchecked` operations capability-visible and out
of the ordinary prelude.

Building on the repaired manifest derivation, extend the stdlib manifest with
`public_safety = safe | raw | unchecked`
and an owning trusted-boundary id. Start by migrating `std.net`, where compiled
workloads already exercise the boundary and `std.io`/`std.fmt` demonstrate that
trusted adapters can absorb `Unsafe` today. Ratchet the remaining reviewed
legacy rows with an owner and workload trigger; fail CI on any new ordinary
hosted API that exposes `Unsafe`, a raw pointer, or a platform sentinel. Raw
allowlist entries require a narrow purpose and no safer claimed surface. Audit
output shows both the public domain capability and the underlying trusted
assumption.

`trusted` may authorize raw implementation operations, but it is not intended
to suspend linearity. The implemented guarantee is currently narrower: E0205
(or its stable successor) rejects ordinary local/binding duplication inside
trusted functions/impls, while bug 046 proved that dereferencing
`*(p : *mut K)` can still duplicate a non-Copy owned value. Pin the working
local-copy path, and schedule a checker slice for the dereference path. Its
negative fixture must include the bug-046 shape; taking ownership through a raw
pointer is accepted only through an explicit audited primitive such as
`read_owned` whose contract consumes/invalidates the source ownership. Add
mutations for both local-copy and dereference paths. Until that slice lands,
reports describe trusted pointer ownership as an open assumption rather than an
enforced linearity property.

The raw-pointer rule is semantic, not decorative: a public operation
that accepts caller-controlled memory must take a provenance-safe Slice, be
private, or be explicitly raw/`_unchecked` with `Unsafe`. Inventory and migrate
`Vec::get_mut`, String/Text raw constructors, numeric cursor/writer `from_raw`,
IO `read`/`write_raw`, test sink pointers, and similar signatures. Conversely,
do not make callers request `Unsafe` merely because HashMap/HashSet or another
trusted implementation manipulates its own private storage; normalize the
current set/map capability mismatch according to public authority.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0051** (merged): duplicate of R-0014: merge wrappers, documentation and boundary fixtures. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0051)

### Task R-0015

**Phase:** C

**Pull trigger:** The new compiler’s file-copy implementation needs shared short-write or close-failure handling across File and Writer adapters.

**Reconciliation (2026-10-19):** Keep workload-gated; use capability-parameterized handles.

**Objective:** Converge File and Reader/Writer when its recorded trigger fires Direction only; not a stdlib boundary-corrections-area closure condition. Pull this when a real socket, file, or buffered-IO workload otherwise needs a second implementation of short-read/write, close, or ownership behavior.


Workload 8 executed the public listener/stream path but left borrowed net-buffer
overloads at two independent asks, so it does not by itself authorize a full
socket Reader/Writer adapter or File/TextFile migration. Preserve the existing
parser-oriented `read_all` contract; a future socket adapter must state whether
mid-stream transport failure needs fallible `read_to_end`, an explicit partial
result, or a distinct operation.

Remove the overlapping `std.fs.File` versus `std.io.TextFile` split. Choose one
linear `File` handle and adapt it into the existing `Reader`/`Writer` contracts:

```concrete
File::open(path: &Path, options: OpenOptions) with(File)
  -> Result<File, FsError>
File::reader(self) -> Reader
File::writer(self) -> Writer
read_file(path: &Path) with(File, Alloc) -> Result<Bytes, FsError>
read_to_string(path: &Path) with(File, Alloc) -> Result<String, ReadTextError>
write_file(path: &Path, data: Slice<u8>) with(File) -> Result<Unit, FsError>
```

All reads distinguish `Ok(0)` EOF from `Err`; writes expose short writes;
flush/seek/tell/close return typed results. Define `SeekFrom` rather than public
integer `whence`, and never encode EOF/error as `-1`. Formatting, files,
console, fixed buffers, tests, and sockets reuse the same read-all/write-all and
fault-simulation logic. When a bidirectional resource creates adapters, use
borrowed adapters or one explicit ownership split so two independently owned
handles cannot close the same resource.

Gate: `scripts/tests/check_io_spine.sh` inventories exactly one public File,
Reader, and Writer contract; runs file/fixed-buffer/console/socket adapters
through identical short-read, short-write, EOF, flush-failure, close-failure,
and exactly-once-close schedules; and forbids a new public sink/source loop.
The forcing inventory must retain the specific legacy discrepancies rather
than hiding them under “unify IO”: TextFile write/flush/close discard
`fwrite`/`fflush`/`fclose` results; `read_byte` uses `-1` for EOF or failure;
`read_line` cannot distinguish EOF from an empty line; and console stdout/stderr
writers do not apply the same external-write result policy. Fix each when its
vertical consumer moves to the common spine.

### Task R-0016

**Phase:** C

**Pull trigger:** R-0016: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; retire already-fixed R-0470 prerequisite. The R-0470 prerequisite is retired (R-0470 done).

**Objective:** Add provenance-safe scoped Slice access Promote the currently exempt `std.slice` surface because existing public raw accessors already need a safe counterpart and the compiled-coverage exemption is concrete evidence. V1 deliberately uses scoped access rather than escapable slice returns:


```concrete
Vec::with_slice<R, cap C>(&self, f: fn(&Slice<T>) with(C) -> R) with(C) -> R
Vec::with_mut_slice<R, cap C>(&mut self, f: fn(&mut MutSlice<T>) with(C) -> R) with(C) -> R
Bytes::with_slice<R, cap C>(&self, f: fn(&Slice<u8>) with(C) -> R) with(C) -> R
Bytes::with_mut_slice<R, cap C>(&mut self, f: fn(&mut MutSlice<u8>) with(C) -> R) with(C) -> R
```

**Confirmed prerequisite (2026-07-25): derived callable capability
inference.** Capability checking fails closed today, but a callback reached
through a struct field, array element, or call result cannot be passed to a
`cap C` parameter even though its function type carries the exact capability
set. It is rejected as E0220; bare function identifiers and locals infer and
enforce their capabilities correctly. This is not an escalation hole, but it
blocks the API above. R-0016 must make `cap C` unify from any checked function
value origin, diagnose a genuinely underconstrained variable with
`.cannotInferCapVariable`, and retain E0240 when the caller lacks the inferred
authority. Gate direct/local/field/array/call-result callbacks, exact cap-set
preservation, and a mutation that widens or erases the inferred set.

`Slice`/`MutSlice` are private-field, non-Copy view tokens; the callback receives
only a second-class reference to the token, not an owned pointer pair it can
return. This adds no user-visible lifetime or region syntax. The callback scope
is the lifetime: the existing reference-escape rules keep the view inside the
call while the owner remains borrowed. Explicit non-goal:
do not add Rust-style lifetime annotations, escapable borrows, or a region
language for this slice.

An ordinary `as_slice(&self) -> Slice<T>`/`as_mut_slice` return remains private,
trusted-internal, or explicitly `_unchecked` until a workload demonstrates that
scoped callbacks are insufficient. A non-Copy linear Slice alone is not a
solution: exactly-once consumption does not stop the owner from being dropped
while the view exists without a checkout/give-back protocol. A future escapable
slice therefore requires a separately reviewed owner/region mechanism or an
explicitly documented unsafe/hazard contract; it is not smuggled into S3.

Convert `Vec::get_mut -> *mut T` into the existing scoped `with_at_mut` ordinary
path; if the pointer escape remains for trusted internals, name it
`get_mut_ptr_unchecked`. Raw pointer+length Slice constructors remain private or
`from_raw_unchecked`. Preserve owner provenance through the existing scoped-
reference rules—do not make slices forgeable or escapable Copy pointer pairs
without an owning borrow contract. Reader/Writer/File/TcpStream/numeric/crypto
migration is workload-pulled after these scoped constructors exist, not an all-
at-once mandate.

Gate: convert the compiled-coverage `std.slice` exemption into positive and
negative fixtures; prove the scoped callback cannot return/store/capture the
slice, the owner cannot be freed/reallocated/mutated incompatibly during the
callback, checked access rejects OOB, mutation is visible only through
`MutSlice`, and the ordinary Vec/Bytes access path no longer returns a raw
pointer or escapable Slice. Track remaining pointer-length hosted signatures in
the manifest rather than forcing an unpulled whole-stdlib migration in this
slice.

### Task R-0017

**Phase:** D

**Pull trigger:** A new-compiler hosted workload or manifest control exposes an unhandled failure shape beyond the confirmed R-0013 corrections.

**Reconciliation (2026-10-19):** Keep unpulled API work; merge confirmed defects into R-0013.

**Objective:** Normalize hosted failure shapes as their recorded triggers fire The normative direction below follows `ERROR_CONVENTIONS.md`, but each signature change is pulled by a workload or a failing manifest leg. It is not a demand to migrate every hosted module in one campaign.


Converge domain surfaces on explicit errors:

- `net`: `read/write/write_all/read_all/close -> Result`; `Ok(0)` alone means
  EOF; add at least `ListenFailed`, `ReadFailed`, `WriteFailed`, and
  `CloseFailed`; retain portable categories while optionally attaching reviewed
  platform detail.
- `fs/io`: eliminate raw counts/status sentinels and discarded flush/close
  failures; distinguish open, metadata/seek, read, write, flush, close, invalid
  path/C-string, and invalid text boundaries where a caller can react.
- `env`: `set`/`unset -> Result<Unit, EnvError>`; provide a representation that
  distinguishes absent from malformed. Keep invalid-value fallback policy in
  the workload, not irreversibly inside the only stdlib accessor.
- `args`: stop mapping both out-of-range and invalid OS bytes to `""`. Make the
  UTF-8 accessor optional/fallible and add a raw-bytes accessor only when a
  workload needs non-UTF-8 argv.
- `time`: `Instant::now`, `sleep`, and wall-clock acquisition surface platform
  failure; validate normalized nonnegative Duration values; keep monotonic
  `Instant` distinct from wall-clock `SystemTime`.
- `process`: typed spawn/wait/kill/close outcomes; do not expose argv pointer
  arrays or raw errno/signal conventions as the primary portable API.
- `rand`: define `random_range(lo, hi)` over the half-open range `[lo, hi)` and
  trap when `lo >= hi`, because invalid bounds are a bucket-2 caller-invariant
  failure under `ERROR_CONVENTIONS.md`. If a workload needs recoverable range
  validation, add a separately named `checked_range`/`try_range` returning
  `Result`/`Option`; never soften the existing API with a mode flag. Separately,
  distinguish deterministic generator state from hosted OS entropy and
  document modulo-bias policy when those shapes are pulled.

Use `Option` only for ordinary absence, `Result` for recoverable malformed or
environmental failure, and trap for documented invariants/OOM. Add
`scripts/tests/check_stdlib_error_contracts.sh` with one success, absence/EOF,
recoverable error, cleanup error, and invalid-input leg per hosted module.

### Task R-0018

**Phase:** C

**Pull trigger:** R-0018: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; update stale borrowed-Text description after R-0483. The borrowed-Text description is updated to R-0483's owner-backed Text.

**Objective:** Stabilize boundary type roles under pull discipline Freeze and gate these roles:


```text
Bytes / Slice<u8>       owned / borrowed arbitrary bytes
String / Text           owned / borrowed valid UTF-8
PathBuf / Path          owned / borrowed OS path bytes
CString / CStr          owned / borrowed checked FFI string
```

Filesystem APIs accept `Path`, not UTF-8 `String`; display conversion is
fallible. Keep operations semantically distinct and do not inflate pull counts:
`index_of(byte, from)`, future `find_subslice(needle, from)`, and future
`starts_with_at(offset, expected)` are different APIs. If a workload needs
recoverable endian bounds, add checked readers and name trapping forms
`_unchecked`; do not churn them without that consumer. `fmt::write_char` is
correct under Concrete's current byte-sized `char` contract and is not a stdlib
defect. Revisit its name/encoding only if the language character model changes.
Raw views and ownership-taking raw-parts APIs remain explicitly trusted and
document their lifetime/allocation handoff.

`BytesRaw` currently names both a non-owning result of `raw_view` and the
ownership handoff from `into_raw_parts`; audit this immediately because borrowed
and owning values cannot share an ambiguous destruction contract. Split the
owning form into `BytesParts` (pointer/length/capacity plus explicit allocator/
handoff rules) and keep the borrowed form provenance-bound, unless a narrower
design proves the same distinction. Do not collapse Slice, Text, ByteView,
ByteCursor, and the text parser Cursor merely to reduce the type count: record
their owner, validation, mutability, position, and destruction roles, then let
parser workloads decide whether ByteView is a refined Slice and whether cursors
share implementation. Move binary cursor APIs out of `numeric` only with that
consumer-backed module decision.

Gate: a generated conversion matrix covers valid/invalid UTF-8, arbitrary path
bytes, embedded NUL, empty buffers, multibyte scalars, split code points, and
owned/borrowed round trips. The API snapshot fails if a checked boundary loses
its fallibility or an unchecked operation loses the suffix.

### Task R-0019

**Phase:** D

**Pull trigger:** A new-compiler workload reaches R-0031’s recorded pull threshold for an additional collection operation.

**Disposition (2026-10-19):** duplicate of R-0031: merge with explicit pull counts and safety requirements.

**Objective:** Add collection ergonomics as recorded workloads pull them Preserve the strong existing decisions: scoped callback access instead of escaping references, `Destroy` for live non-Copy contents, capability- polymorphic traversal, and ascending traversal only for ordered containers.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0019).

### Task R-0020

**Phase:** D

**Pull trigger:** A new-compiler workload reaches a recorded module-shape pull threshold or demonstrates a concrete contract failure in that candidate API.

**Reconciliation (2026-10-19):** Keep trigger ledger; assign each candidate to module owner below.

**Objective:** Add remaining module shapes as recorded workloads pull them The following are design candidates, not scheduled the stdlib boundary-corrections area migrations. Each requires independent workload evidence or a confirmed contract failure before implementation.


1. Replace public `spawn(cmd, *const *const u8)` with a typed `Command` builder
   over `Path`/owned arguments. Put fork and Unix signals in an explicitly Unix
   or raw module; generic `std.process` must not imply portability it lacks.
2. Store Duration canonically and define overflow/negative policy. Remove
   hard-coded clock ids from generic code; target facts choose the platform
   implementation. Add deterministic clock/failure adapters for tests.
3. If a workload pulls it, introduce explicit deterministic `Rng` state and
   reserve `with(Random)` for hosted entropy. Gate reproducibility, range
   bounds, invariant-trap behavior for `lo >= hi`, any separately named
   recoverable range API, and bias claims. Never describe libc `rand` as
   cryptographic.
4. Keep scalar parsing's `Option` surface; use positional `ParseError` results
   when cursor/structured-parser workloads need diagnostics. Do not create an
   implicit conversion web between module errors.
5. Keep Writer-first formatting and allocation-visible String wrappers. Test
   all Writer failures; do not use `unwrap_or` to erase a formatting sink error.
6. Make test assertion messages borrowed/static where possible, add typed
   byte/String/path assertions, and keep exit status/stdout/stderr distinct.
7. Replace `sha256::hash_raw(*mut u8) -> Vec<u32>` with a borrowed byte input
   and nominal/fixed 32-byte digest once fixed-array ergonomics support it; input
   is not mutable. Apply the same Slice input policy to checksum/base64/hex.
   Streaming crypto/encoding remains workload-pulled.
8. Replace `std.cli`'s untyped `u64` flag ids and unchecked lookups with nominal
   declaration handles, reject duplicate declarations, keep unknown/missing/
   malformed/arity errors distinct, and define repeated-flag policy. Argument
   acquisition must carry its authority consistently (`count` and `get` may not
   disagree), while pure token parsing remains capability-free.
9. A3 owns the already-pulled Option/Result consuming-combinator slice. Any
   additional convenience combinator remains pull-gated; extend `std.test`
   helpers alongside A3 so linear Option/Result values can be asserted without
   becoming unconsumable.
10. Audit pure scalar modules (`ascii`, `math`, `parse`, `numeric`, `hash`) for
    totality and naming: invalid clamp/range bounds, `abs(MIN)`, float domain/
    NaN behavior, cursor bounds, integer overflow, endian width, and hash
    stability must be documented as checked result, trap, or defined value and
    pinned by edge vectors. Avoid generic APIs whose ordering/arithmetic
    constraints are only implicit in the body.
11. Keep the remaining workload-gap ledger evidence-counted: `String::cmp` at
    its recorded ask count; `Vec::with_capacity`; SHA/hex borrowed-Bytes input;
    stdin Reader; and safe process argv construction. Promote only at the pull
    threshold or on a confirmed contract defect. Do not combine them into a
    polish release merely because neighboring modules already have symmetric
    names.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0025** (merged): duplicate of R-0047/R-0020: merge; no separate umbrella implementation task.

### Task R-0021

**Phase:** C

**Pull trigger:** R-0021: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; R-0117 supplies producer, R-0056 supplies diff.

**Objective:** Complete the mechanical manifest and fail-closed coverage gate Extend `concrete std snapshot --json` and the compiled-coverage inventory so every public symbol records: canonical signature, safety class, capabilities, allocation, ownership/consumption, success/absence/error/trap behavior, platform availability, evidence class, examples, and trusted-boundary id.

Generate rather than hand-count the inventory.

The inventory is fail-closed over every `std/src/*.con` module, including the
low-level `alloc`, `ptr`, and `libc` modules and umbrella `lib`. A module may be
classified pure, alloc-backed, hosted, raw/unsafe, umbrella, or intentionally
surface-less, but none may disappear because the focused migration examples do
not mention it. Adding a module or public symbol without a compiled behavioral
fixture or reasoned exemption, API snapshot entry, and evidence/authority/
failure classification fails CI. This extends the existing five-fact manifest
and compiled-coverage machinery; do not create a parallel review database.

**Usable `std` subset (2026-10-21; graduation criterion).** Name, in a checked list, the `std` APIs the
first two workloads need: RocksDB slice 1 with its companions, and the first R-0490 component workload
(or the first pilot's program if it comes first). Validate those APIs' ownership, error and foreign
boundary behaviour together, in one composed program fixture per workload rather than one fixture per
API. This subset and its fixtures are a graduation criterion for this inventory and for R-0488's
library-usability checkpoint; `std` work beyond it before V1b follows recorded workload asks
([capacity allocation](../archive/ROADMAP_LEGACY_PLAN.md#capacity-allocation)).

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0059** (merged): duplicate of R-0021/R-0440: merge; replace single-class taxonomy with orthogonal facts.
- **R-0062** (merged): duplicate of R-0021/R-0056: merge behavioral regression gates.

### Task R-0023

**Phase:** C

**Pull trigger:** R-0023: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The Int-main retirement is legacy migration machinery, while entry signatures and error rendering remain language requirements. surviving requirement: Implement and test the decided entry-point signatures and explicit exit/error behavior in the new compiler. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep versioned language change; coordinate startup with R-0505.

**Objective:** Retire the remaining entry-point tails Deliberately decoupled from the completed exit-model migration: retire Int-main in a later surface pass by narrowing the entry signature to `fn main() -> u8 | Unit`; workload-gate any `main -> Result<Unit, E>` form and nominal error rendering to stderr.

### Task R-0024

**Phase:** C

**Pull trigger:** R-0024: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: Old Layout, alias, classifier and emitter repairs retire, but the listed API, diagnostic and regression duties survive. surviving requirement: Preserve safe Slice construction, differential failure coverage, non-finite-literal diagnostics, checked C-string boundaries, and gate-hygiene regression controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Split concrete residuals into owners; remove obsolete phase-number priority rule.

**Objective:** Finish compiler hygiene and known-defect tails Each item lands as its own evidence-ranked slice with the battery:

   - Layout panic hygiene: convert `Concrete/Check/Layout.lean` `panic!`
     paths (unknown struct/named type in fieldOffset/tySize) into
     structured internal-error diagnostics — bug 035's fix made them
     unreachable for well-formed programs, which is exactly when a panic
     should become a diagnostic. SCOPED (2026-07-17): ~69 call sites
     (tyAlign 21, tySize 27, isPassByPtr 12, fieldOffset 8, tyToLLVM 2
     incl. EmitSSA), all cascading through pure code — no small slice;
     it is an Except-threading grind that must land green in ONE piece,
     with the full battery as backstop. Dedicated fresh session.
   - Known-defects ledger (each owns a slice and battery when its stated trigger
     fires):
     These are historical/pull-gated tails inside this task, not another source
     of priority; do not select one while an earlier-numbered task lacks
     containment.
     1. Bug 039 residual (docs/bugs/039): two SUBMODULES of one top-level
        module importing different same-named fns still share a flat
        alias list (collectAllLinkerAliases). Pull-gated: fix when a
        workload or fixture hits it.
     2. `std.slice` has no public constructor — user code cannot obtain a
        provenance-backed `Slice` at all (coverage-gate exemption records it).
        the stdlib boundary-corrections area S3 owns the narrow fix because existing Vec/Bytes raw access
        already needs a safe counterpart; convert the exemption into positive
        and negative compiled fixtures. Migration of IO/fs/net/numeric
        signatures remains workload-pulled after the constructor exists.
     3. Bug 027 (EmitSSA O(n^2) rendering) — open perf item, not
        correctness; profile-driven fix when compile times matter.
     4. Differential fuzzer/oracle residuals: heap/alloc/IO shapes still
        compiled-only, trap class not compared interp-vs-compiled, no
        float support in interp comparisons.
     5. Check-side divergence/capability classifiers still match raw Core names
        after intrinsic identity was fixed in Elab/Lower. The remaining
        direction is conservative misclassification only; pull with Task R-0274's
        typing-truth work or a failing user-shadowing fixture.
     6. Non-finite float literals leak an external tool error (audit
        2026-07-16): a literal with a ≥309-digit integer part lexes to +inf,
        and EmitLLVM prints `double inf`, which llvm-as rejects — an
        external-tool diagnostic reaching the user instead of a compiler one.
        Fix fail-closed: reject non-finite literals at lex time with a
        spanned diagnostic (or emit the LLVM hex-float form); add a corpus
        leg to check_float_literals.sh.
     7. Silent defaults sweep (audit 2026-07-16; adjacent to §F's panic
        inventory but distinct — these fabricate rather than crash):
        `sizeof` with no type args defaults to `Ty.int` (Lower),
        `vec_pop`'s element type defaults to `.placeholder` (Elab), an
        unknown enum's payload offset defaults to 8 (Lower). Each becomes a
        structured internal-error diagnostic with a sweep proving no current
        program reaches them.
     8. Bug-043 class rule is prose-only (docs/bugs/043): nothing stops a
        new FFI site handing a raw `.ptr` to a NUL-delimited C API again.
        Add a std-hardening-style grep/lint leg over trusted FFI sites
        (no bare `.ptr` argument to a C string function; ptr+len sites
        exempt).
     9. printf|grep -q pipefail flake class (10b9a776 fixed 123 sites, 43
        files): add a grep-lint to gate-hygiene so the pattern cannot be
        reintroduced.

### Task R-0025

**Phase:** D

**Pull trigger:** R-0025: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0047/R-0020: merge; no separate umbrella implementation task.

**Objective:** CLI/env/process helpers for real tools (stdlib APIs, not compiler CLI).

### Task R-0026

**Phase:** C

**Pull trigger:** R-0026: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Merge concrete exits into R-0515’s debugging/diagnostics checkpoints.

**Objective:** Finish trap/debug UX and verified-profile/ proof-obligation UX.

### Task R-0030

**Phase:** C

**Pull trigger:** R-0030: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; F7/F8 already provide a forcing trigger.

**Objective:** Deterministic capability-fault simulation (pull-gated; the *dynamic* complement to H3). H3's manifest gate proves an IO error *can* be surfaced (static shape); this proves it *is* surfaced under real failure (dynamic).

  Because every effect flows through an explicit capability/handle value — e.g.
  `Writer`'s `write_fn: fn(...) -> Result<u64, IoError>` — faults are injected by
  swapping the backend at the seam, with no whole-world simulator; the capability
  model makes fault injection cheap by construction, which is why Concrete's
  version is far smaller than Turso-style DST. Deterministic failure schedules
  drive properties static analysis cannot reach: `write_all` never `Ok` after a
  short/failed write; close-fail-after-flush is reported; allocation #N fails;
  `Reader` treats `Ok(0)` as EOF only at the right boundary. First consumers:
  R-0013's F7/F8 repairs (ignored write/close results, unchecked `ftell`), whose
  injected-failure controls run here. Do NOT re-litigate
  already-static properties (capability erasure = manifest gate; exit-status ≠
  stdout = MAIN_EXIT_MODEL). Proof stays separate — the pure core's first line;
  simulation covers only the effectful failure interleavings proofs do not, and
  upgrades no evidence class to `proved`. Pull-gated: pulled when H3 lands or an IO
  bug appears; run against `base64_cli`/`png_chunks`/workload 3, starting at the
  `Writer`/`File` seam and expanding only as each capability (`Alloc`, `Time`)
  gains an injectable backend. Design record:
  [`docs/project/DETERMINISTIC_SIMULATION.md`](DETERMINISTIC_SIMULATION.md), which
  refines the tier-J+ "deterministic simulation backend" earmark in
  `docs/platform/EXECUTION_MODEL.md`. Gate: `scripts/tests/check_effect_simulation.sh`
  replays a fixed fault schedule and asserts each effect-boundary property.

**Shared fault-injection checkpoint (2026-10-21; pulled by RocksDB slice 1's companions).** One
seam-based failure-schedule mechanism serves the runtime, `std` and FFI: allocation failure where the
allocator supports injection, short reads and writes, failed initialization, failed cleanup and
interrupted persistence. It is exercised composed in real programs, not only per API: R-0285's
`ffi_error_release` and `ffi_resource_pool` companions and the RocksDB slice 1 consumer run under
schedules that fail each acquisition, write, flush and release in turn. **Checkpoint exit:** each
failure kind has a deterministic schedule that a composed program runs, asserting the typed error,
exactly-once release of everything acquired and no use of a released resource; a control with the
injected failure ignored by the program under test fails; unsupported injection points (an allocator
without a seam) are listed, not counted as covered. It does not block RocksDB slice 1 itself.

### Task R-0031

**Phase:** C

**Pull trigger:** R-0031: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The body mixes surviving collection contracts with obsolete allocator research and deferred language proposals. surviving requirement: Preserve collection ownership, explicit allocation and destruction, traversal and dictionary coherence, callback alias controls, and workload-gated API and proof extensions under the decided language. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; separate research extensions listed below. Hash-flood policy: a fixed deterministic hasher gives collision correctness, not denial-of-service resistance. Choose and document bounded input/work, a collision-resistant strategy for attacker-controlled keys, or an explicit non-adversarial-use limitation (with R-0036).

**Objective:** Build the remaining collection APIs across `std.vec`, `std.map`, `std.set`, `std.ordered_map`, `std.ordered_set`, `std.deque`, `std.heap`, `std.bitset`, and `std.slice`: fixed arrays/slices, `Vec<T>`, maps, sets, buffers, parser cursors, and capacity-aware helpers. Add the ordinary APIs C/Rust users expect: `contains`, `remove`, `iter` for maps and sets;

`insert`, `remove`, `retain`, `sort`, `search` for vectors and slices;
stable ordering helpers for ordered collections. Each API must state whether
it requires `with(Alloc)`, whether it can fail, and which runtime
obligations it creates.

**Scope until V1b (2026-10-21):** new collection APIs are added for a recorded workload ask or R-0021's
usable `std` subset, not for completeness ([capacity allocation](../archive/ROADMAP_LEGACY_PLAN.md#capacity-allocation)); the list above
stays this task's eventual scope.

Every new collection API must preserve the H18 ownership contract documented
in [CHANGELOG.md](../../CHANGELOG.md) and `docs/stdlib/RUNTIME_COLLECTIONS.md`: a
collection owns its live elements until they are explicitly moved out.
`clear`, `drop`, overwrite,
set/replacement, and compaction paths destroy displaced live elements;
`pop`, `remove`, and `swap_remove` transfer ownership to the caller.
Disposal stays forced-explicit (`x.drop()` / `defer x.drop()`), never silent
scope-end auto-drop. `get -> Option<T>` remains `T: Copy`; non-`Copy` indexed
access remains scoped callback / borrow / explicit move-out, with no returned
mutable references. The remaining pull-gated v1 fence is associated
destructor-capability machinery, pulled by the first infallible
non-`Alloc` destructor, and raw `*_unchecked` overwrite escapes remain
documented trusted escapes.

Traversal must be **deterministic but not ordered**:
`for_each`/`fold` over `std.map`/`std.set` need a fixed hasher and no per-run
random seed (so the oracle/differential story is stable given the same
operations), but the visitation order is deliberately **not** a defined key
order — `HashMap`/`HashSet` are permanently unordered (a map is unordered in
the proof model, so this costs the evidence story nothing). A defined key
order is `OrderedMap`/`OrderedSet`'s job; their `for_each`/`fold` APIs have
landed and must remain ascending-key traversal. Do not add insertion-order
tracking to the hash collections.
- 1a. Keep `Clone` and indexed move/swap as **workload-gated value-model
  research**, separate from H1 and not assumed inevitable. If admitted,
  `Clone` is explicit semantic duplication (capability-visible, usually
  `with(Alloc)`, audit-visible, and not silently proof-eligible), not a
  convenience patch for borrowed reads. The sibling move-out primitive for
  indexed containers is `swap(i, new) -> V`, which transfers ownership out
  while preserving the linear one-value-per-slot invariant. Build either only
  after a real workload repeatedly needs owned duplication or indexed
  ownership-out and the existing `Copy` / move / borrow APIs are the wrong
  fit.
- 1b. Design arena/index safety before any arena-backed structure becomes a
  flagship or stable stdlib API. Array-backed linked structures use indices,
  and a stale index into a removed/reused slot is a logic-level dangling
  pointer that ordinary ownership does not see. V1 answer should compare
  typed index newtypes (`NodeId`, not `u64`), generation-counted slots,
  arena validity invariants, and proof obligations that inject newtype
  invariants as scoped hypotheses. Public stdlib arenas remain workload-
  gated: do not add a generic `Arena<T>` until a parser/graph/runtime
  workload needs it, and never expose raw integer indices as the stable API.
  Any admitted arena-backed container must preserve linear destruction,
  allocator visibility, and stale-index rejection or explicit trust. Add
  `docs/ARENA_INDEX_SAFETY.md`, `examples/arena_indices/{stale_index,generation_checked,typed_node_id}/`,
  and `scripts/tests/check_arena_index_safety.sh`; the gate must prove stale
  index reuse is rejected, trapped, or explicitly trusted, never silently
  treated as ordinary memory-safe access.
- 1c. Decide explicit-dictionary coherence before binary collection
  operations harden. `HashMap<K, V>` values built with different `hash_fn` /
  `eq_fn`, or ordered maps/sets built with different comparators, have the
  same type but incompatible semantics; `union`, `merge`, `intersection`,
  equality, and bulk transfer across them can silently produce nonsense.
  Choose one v1 rule: canonical registered dictionary per key type,
  dictionary/brand carried in the collection type or value, debug/runtime
  identity checks on binary operations, or a documented sharp edge that
  forbids binary ops across explicitly-different dictionaries. Add
  `docs/COLLECTION_COHERENCE.md`,
  `examples/collection_coherence/{same_dictionary_merge,different_eq_rejected,different_cmp_rejected}/`,
  and `scripts/tests/check_collection_coherence.sh`; the gate must show the
  chosen verdict for incompatible dictionaries and prove the API cannot
  silently combine them.
- 1d. Future collection APIs must preserve the `with_value_mut` / `modify`
  `E0293`
  container-not-in-context guard: the callback's `&mut ctx` may not alias the
  `&mut self` container. Gated in `check_callable_values.sh`.
- 1e. Keep scalar `from(param)` returned references deeply deferred and
  evidence-gated. If ever added, they stay flat and scalar: no `Option`,
  `Result`, structs, arrays, containers, callback contexts, or generic
  wrappers. This is a research escape valve only if real workloads prove
  operation APIs, owned views, and scoped callbacks insufficient.
- 1f. Pull narrow const generics forward only when fixed-capacity stdlib APIs
  need reusable capacities. Follow `research/language/const-generics-v1-decision.md`; implementation
  is deferred until a real stdlib-area consumer
  appears (`BoundedVec<T, N>`, `RingBuffer<T, N>`, `PacketBuf<N>`, fixed hash
  table, parser scratch buffer, freestanding reusable buffer, or a
  capacity-indexed proof API). When triggered, implement the staged V1 from
  that doc and wire `scripts/tests/check_const_generics_v1.sh` to prove
  distinct capacities specialize separately, layout is capacity-specific,
  runtime-safety obligations name the instantiated size, and unsupported
  comptime/reflection/runtime-bound forms are rejected.
- 1g. Research **allocator-as-value** before allocator-backed collections,
  arenas, or freestanding APIs harden. Keep the distinction sharp:
  `with(Alloc)` is permission to allocate; an allocator value names which
  allocator/arena/pool is used. Do not replace capabilities with allocator
  parameters, and do not add an ambient implicit allocator. The research
  note should compare Zig-style explicit allocators, arena/test allocators,
  embedded/freestanding pools, hot-reload/plugin ownership, allocation
  failure policy, and audit output that records both the capability and the
  allocator identity/strategy. If admitted, every allocator-taking API must
  keep allocation authority visible in function headers and reports. This
  item should move ahead of allocator-heavy collection stabilization if the
  validation project needs arenas, test allocators, reload-safe allocation,
  or freestanding pools.
- 1h. Pilot **behavioral laws for explicit interfaces** (planned 2026-10-18).
  R-0031 owns the API example; R-0271 owns its proof/instantiation semantics,
  using R-0473/R-0474's typed contracts and R-0482's evidence identities when
  formal composition is admitted. Start with the existing explicit equality
  and hash functions: state the equality laws and that equal keys have equal
  hashes. This complements 1c's dictionary compatibility: two maps can use
  the same dictionary while that dictionary's own functions disagree.
  Retain a matching pair and a deliberately inconsistent pair that treats
  `"Hello"` and `"hello"` as equal while hashing their original case
  differently; exercise insertion and lookup to expose the bad behavior.
  Finite tests report their tested domain; they do not prove the universal law.
  Bind each law's exact proposition and assumptions to the selected callable
  implementations, their dictionary and the admitted generic instance. An
  implementation change invalidates dependent evidence. Missing evidence
  withholds the corresponding functional guarantee; an explicit assumption
  remains visible through callers and package boundaries. A safe API must
  preserve memory/resource safety even when an unproved behavioral law fails.
  Keep ordinary programs usable without proofs. Begin with normal declarations,
  existing static/explicit dispatch and selected proof attachments; this does
  not admit new trait syntax, implicit instance search or blanket proof
  requirements. Use the example and measured authoring/review friction to decide
  whether to expand to comparator ordering or codec round trips. Pull this
  bounded pilot when collection/generic-proof work reaches it; it is not a
  prerequisite for the first R-0285 consumer or the R-0490 executable port.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0019** (merged): duplicate of R-0031: merge with explicit pull counts and safety requirements. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0019)

### Task R-0032

**Phase:** D

**Pull trigger:** A new-compiler workload needs reverse internal traversal or allocation-explicit builders beyond existing collection APIs.

**Reconciliation (2026-10-19):** Keep workload-pulled; no external iterator/lifetime expansion.

**Objective:** Build internal-iteration and builder APIs in proposed `std.iter` and `std.builder` after the collection shape is known. This is NOT Rust's external-iterator / adapter-tower model: `research/stdlib/iterators.md` resolved the v1 design as per-container internal traversal (`for_each`, `fold`, context-threaded callbacks, and optional early-exit via an explicit `Continue | Break` tag), with no iterator trait, no lazy adapter chain, and no cursor/lifetime model. Add known-length reporting and reverse traversal (`rev_fold`/`rev_for_each` — today every backwards walk is a manual index-decrement loop; extend the iteration protocol in `docs/project/STYLE.md` when these land),

plus byte/text builders and tree/buffer builders inspired by Gleam's
`BytesTree` and `StringTree`. Do not hide allocation; builder APIs either carry
`with(Alloc)` or operate over fixed buffers.

### Task R-0033

**Phase:** C

**Pull trigger:** R-0033: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep over one capability-preserving spine.

**Objective:** Complete the Hare/Zig-shaped IO adapter layer on top of the one `std.io.Reader` / `std.io.Writer` spine: buffered reader/writer helpers (`bufio` shape), fixed-memory readers/writers (`memio` shape), byte-counting and tee/discard adapters, and small copy/drain helpers. These are adapters, not a second IO abstraction. Every adapter must preserve the underlying capability story, keep allocation explicit (`with(Alloc)` or caller-provided buffers), and be covered by `std.test` sink/source oracles.

### Task R-0034

**Phase:** C

**Pull trigger:** R-0034: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; consume the single `IntArith` semantics.

**Objective:** Build numeric helper APIs in `std.numeric`, `std.math`, and `std.mem`:

checked/wrapping/saturating arithmetic helpers,
narrowing/conversion helpers, endian conversions, byte/word packing, and
evidence classes for each helper.

### Task R-0035

**Phase:** D

**Pull trigger:** A new-compiler workload requires stable sorting or binary search absent from the migrated library.

**Reconciliation (2026-10-19):** Keep; bind coherence/laws to R-0031.

**Objective:** Build sorting and searching primitives in proposed `std.sort` and `std.search`: comparison conventions, stable/unstable sort decision, binary search, min/max helpers, and evidence/oracle tests over edge cases.

### Task R-0036

**Phase:** D

**Pull trigger:** A new-compiler collection workload requires a documented hasher or deterministic checksum/RNG oracle.

**Reconciliation (2026-10-19):** Keep; distinguish entropy authority and security claims.

**Objective:** Build hashing, checksums, and deterministic random helpers in `std.hash`, proposed `std.checksum`, and `std.rand`: stable hash APIs for maps/sets, non-cryptographic checksums, seeded deterministic RNG for tests/oracles, and a clear split from cryptographic randomness. Any OS entropy source is hosted-only and capability-visible.

### Task R-0037

**Phase:** G

**Pull trigger:** A new-compiler security flagship requires fixed-byte comparison with an explicitly bounded source-shape timing claim.

**Reconciliation (2026-10-19):** Keep source-shape claims only; no automatic binary timing transport.

**Objective:** Build constant-time helper APIs in proposed `std.ct` only for narrow, auditable cases: equality/compare over fixed-size bytes, no secret-dependent branches or early exits, source-shape audit evidence, and clear machine-level timing assumptions. This belongs to the narrow security surface, not broad crypto.

### Task R-0038

**Phase:** D

**Pull trigger:** A new-compiler workload needs clock readings or timeout calculations through explicit clock grants.

**Reconciliation (2026-10-19):** Keep workload-pulled.

**Objective:** Build time and duration helpers in `std.time`: monotonic versus wall-clock distinction, timestamp formatting/parsing if admitted, timeout helpers, and explicit hosted authority for reading the clock.

### Task R-0039

**Phase:** C

**Pull trigger:** R-0039: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; R-0444 owns combinator core.

**Objective:** Build formatting and parsing helpers in `std.fmt` and `std.parse`:

 integer/text formatting, simple
 scanners, structured parse results, error-set reports, and oracle-friendly
 output conventions. R-0444 owns the shared parser-combinator and fixed-width
 assembly core; this task consumes that core for the remaining formatting and
 text-parsing surface rather than creating a second parser vocabulary.

### Task R-0040

**Phase:** D

**Pull trigger:** A new-compiler parser workload needs streaming text adapters and source positions beyond the shared combinator core.

**Reconciliation (2026-10-19):** Keep unique adapters; merge overlapping scanner work into R-0039.

**Objective:** Complete the scanner/parser surface left after R-0444 over
`std.bytes.Bytes` and `std.text.Text`: span/position tracking, structured error
reporting, and streaming/text adapters, with no hidden allocation unless the
API carries `with(Alloc)`. R-0444 owns the reusable combinator core (`peek`,
`advance`, bounded take/consume/repeat/scan shapes); do not duplicate it here.
Retire any originally listed row that R-0444 demonstrably ships rather than
keeping two nominal owners.

### Task R-0041

**Phase:** D

**Pull trigger:** A workload running on the new compiler requires streaming base64 encoding or decoding.

**Reconciliation (2026-10-19):** Keep pull gate.

**Objective:** Extend `std.base64` only when a workload pulls streaming encode/decode; preserve canonical padding-bit strictness and the args -> bytes/text -> parse/errors -> Writer oracle path.

### Task R-0042

**Phase:** D

**Pull trigger:** A new-compiler workload requires URI component access or percent encoding after byte/text/path migration.

**Reconciliation (2026-10-19):** Keep pull gate and syntax/authority separation.

**Objective:** Add `std.uri` parsing/formatting after the byte/text/path split is stable:

 component accessors, percent encoding/decoding, normalization policy, and
 clear distinction between syntax validation and network authority.

### Task R-0043

**Phase:** D

**Pull trigger:** A new-compiler workload requires bounded JSON parsing with reference-oracle validation.

**Reconciliation (2026-10-19):** Keep workload-pulled.

**Objective:** Add `std.json` as the first structured data module: tokenization, string/number handling, error spans, bounded recursion policy, optional DOM-like representation only if the allocation story is explicit, and oracle tests against a reference implementation.

### Task R-0044

**Phase:** D

**Pull trigger:** A new-compiler JSON consumer needs typed field decoding beyond the admitted JSON API.

**Reconciliation (2026-10-19):** Keep only with a forcing consumer.

**Objective:** Add a small typed decoding layer in proposed `std.decode` after `std.json`: dynamic value decoding, field access, error paths, and examples comparable to Gleam's `dynamic/decode`, without broad reflection or hidden runtime typing.

### Task R-0045

**Phase:** D

**Pull trigger:** A new-compiler binary-format workload requires bounded length-prefixed or endian-aware serialization.

**Reconciliation (2026-10-19):** Keep; reuse R-0444/R-0034.

**Objective:** Add binary serialization helpers in proposed `std.bin`: endian-aware reading/writing, fixed-width integers, length-prefixed fields only with explicit bounds, byte-span diagnostics, and no hidden allocation.

### Task R-0046

**Phase:** G

**Pull trigger:** New-compiler package/build tooling begins consuming semantic versions or configuration manifests.

**Reconciliation (2026-10-19):** Keep workload-pulled.

**Objective:** Add semantic-version and config-format helpers in proposed `std.semver` and `std.config` if package/build work starts depending on them:

 `SemVer`, INI/TOML-style scanner, and manifest parsing support. Prefer a
 small Hare-like `format/ini`-class module before any broad configuration
 framework; keep TOML/YAML/package manifests workload-gated. These are
 stdlib/package-boundary helpers, not general metaprogramming.

### Task R-0047

**Phase:** D

**Pull trigger:** A new-compiler user tool needs typed flag and positional parsing beyond raw std.args.

**Reconciliation (2026-10-19):** Keep; absorb R-0025’s CLI portion.

**Objective:** Add command-line parser helpers in proposed `std.cli`: flags, positional arguments, usage text, typed parse errors, no ambient environment access except through `std.args`, and examples that keep authority visible. This is for user programs. The `concrete` compiler's own command taxonomy and help behavior are old phase 6E, and `std.cli` should learn from that surface without coupling to compiler internals.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0025** (merged): duplicate of R-0047/R-0020: merge; no separate umbrella implementation task.

### Task R-0048

**Phase:** D

**Pull trigger:** A new-compiler tool requires shell-word quoting or pure file-pattern matching.

**Reconciliation (2026-10-19):** Keep narrow workload gate.

**Objective:** Add narrow shell/path helper modules only as tool workloads demand them:

  `std.shlex` for shell-word splitting/quoting, `std.glob` / `std.fnmatch`
  for file-pattern matching, and no `wordexp`-style command/variable
  expansion in the core. These APIs are byte/path aware, carry no hidden
  filesystem authority, and must keep expansion separate from matching so a
  pure parser cannot accidentally become a hosted operation.

### Task R-0049

**Phase:** D

**Pull trigger:** A new-compiler workload needs levelled writer-based logging beyond direct diagnostic output.

**Reconciliation (2026-10-19):** Keep workload-pulled.

**Objective:** Add simple logging/diagnostic output APIs in proposed `std.log`: levels, writers, formatting integration, capability requirements, and policy for release builds. Keep this small; it is not a tracing framework.

### Task R-0050

**Phase:** D

**Pull trigger:** New-compiler word_count or base64_cli needs visible progress through an explicit console handle.

**Reconciliation (2026-10-19):** Keep named consumer trigger.

**Objective:** Add progress/status output helpers for CLI tools in proposed `std.progress` only after `examples/daily/word_count` and `examples/base64_cli` need visible progress output. V1 surface: `ProgressWriter`, `quiet`, `verbose`, `set_total`, `advance`, `finish`, terminal detection through an explicit console handle, and no ambient terminal authority. Wire `scripts/tests/check_stdlib_progress.sh` when the module is admitted.

### Task R-0051

**Phase:** C

**Pull trigger:** R-0051: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0014: merge wrappers, documentation and boundary fixtures.

**Objective:** Build capability-scoped console, file, network, process, and time APIs in `std.io`, `std.fs`, `std.env`, `std.args`, `std.process`, `std.net`, and `std.time`. Authority must be visible in function types and audit reports; no API may smuggle ambient authority through a convenience wrapper. New output/input APIs target the `std.io.Reader` / `std.io.Writer` spine; a separate public `std.writer` sink surface must not reappear.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0051).

### Task R-0052

**Phase:** C

**Pull trigger:** R-0052: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; include external-call assumptions in V6/V7.

**Objective:** Build handle-relative filesystem APIs in `std.fs` as the preferred file/path shape: directory/file handles carry authority; operations are relative to handles for `open`, `create`, `read`, `write`, `metadata`, `remove`, `rename`, and `list`. Ambient absolute-path helpers must be hosted-only convenience wrappers with explicit authority. Temp-file, symlink, path-normalization, and TOCTOU behavior must appear in `docs/stdlib/STDLIB_GUIDE.md` and `scripts/tests/check_stdlib_fs.sh`.

### Task R-0053

**Phase:** D

**Pull trigger:** R-0053: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The useful helper requirement survives, but the body’s with(File) authority prescription must follow the new decisions. surviving requirement: Provide workload-needed temporary-file and directory-discovery helpers with explicit cleanup and the decided directory and permission model. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep behind R-0052.

**Objective:** Add small `std.temp` and `std.dirs` helpers after the handle-relative filesystem shape is gated. `std.temp` owns temporary files/directories with explicit cleanup (`drop`/`defer`, no implicit scope-end delete) and visible `with(File)` authority; `std.dirs` exposes only conservative hosted directory discovery needed by tools. Do not add XDG/platform policy sprawl until package/build workloads pull it.

### Task R-0054

**Phase:** D

**Pull trigger:** A new-compiler validation workload needs socket handles or address/header parsing.

**Reconciliation (2026-10-19):** Keep workload-pulled; no general networking expansion.

**Objective:** Build handle-based network surface in `std.net` only as far as the validation workloads require: address parsing, socket handle wrappers, HTTP header parsing as a pure parser first, and no full HTTP client/server until package/workload evidence demands it.

### Task R-0055

**Phase:** C

**Pull trigger:** R-0055: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; share public-symbol denominator with R-0021.

**Objective:** Build stdlib test/oracle helpers in `std.test`: expected failures, capability-scoped fixtures, temp directories, oracle vector runners, interpreter-vs-compiled helpers, and report snapshots. Stdlib tests must also serve as runnable API documentation: every public stdlib type/function gets a tiny positive usage test and, where meaningful, a negative or edge case. Public docs should link to or quote those tests rather than carrying stale hand-written examples. This is the Zig stdlib lesson adapted to Concrete: usage examples should be executable fixtures, not prose that can rot. Add a gate that fails when a new public stdlib symbol lacks a test/doc

 example, when a referenced example no longer compiles, or when a doc claims
 an evidence class/capability/allocation behavior that the test/report does
 not produce.

### Task R-0056

**Phase:** C

**Pull trigger:** R-0056: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep consumer of R-0021/R-0117, not a second scanner.

**Objective:** Add a public stdlib API snapshot/diff immediately after the first real workload (`base64_cli`) and before broad URI/JSON/CLI/log/progress breadth.


  Deliverable: `concrete std snapshot --json` (or an equivalent test helper)
  and `scripts/tests/check_stdlib_api_snapshot.sh`.

  Snapshot fields: module path, public names, signatures, `Copy`/linear
  requirements, capabilities, allocation behavior, trap/recoverable-failure
  profile, evidence class, deprecation status, and linked tests/docs.

  Done when adding/removing a public function, changing a signature, widening
  a capability, changing allocation/trap behavior, or changing an evidence
  class fails the gate until the snapshot and docs are updated. This is the
  stdlib-local version of the audit-commands area's proof/capability diff and the packages area's
  package API artifacts; it prevents accidental drift while the library is
  still small.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0062** (merged): duplicate of R-0021/R-0056: merge behavioral regression gates.

### Task R-0057

**Phase:** C

**Pull trigger:** R-0057: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The conventions survive, but the body’s blanket fatal-OOM prescription conflicts with the new allocation decision. surviving requirement: Document and gate Result/Option, explicit error conversion and fatal-failure conventions consistent with the decided fallible-allocation semantics. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep enforcement/documentation remainder; avoid duplicating settled policy.

**Objective:** Define stdlib error-handling conventions: when APIs return `Result`, `Option`, panic/abort, or require a policy gate; how ignored-result diagnostics apply; and how accumulating error sets are reported. Split recoverable domain/environment failures from fatal invariant failures:

 `Result`/`Option` for user, file, network, parse, and domain errors that a
 caller can handle; abort/trap for OOM, bounds, arithmetic traps, impossible
 invariants, and explicitly unrecoverable runtime failures. Audit output
 should identify which public APIs can recover, which can trap, and which
 require a policy gate. Do not add a second Zig-style error-union mechanism;
 improve the `Result`/`Option` surface instead. Define how a module error
 composes into a caller's error across a boundary (`FsError` -> `ServiceError`)
 WITHOUT Rust's `From`/`Into` trait web: an explicit conversion function or a
 small wrapping variant named at the call site, not an implicit trait
 conversion. State the pattern once so error-heavy code does not each invent
 its own.

### Task R-0059

**Phase:** C

**Pull trigger:** R-0059: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0021/R-0440: merge; replace single-class taxonomy with orthogonal facts.

**Objective:** Define stdlib evidence classes per public API: `proved`, `enforced`, `reported`, `tested_by_oracle`, `assumed`, or `trusted`. The evidence class must appear in docs and audit artifacts, not just implementation comments.

### Task R-0060

**Phase:** E

**Pull trigger:** R-0060: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep after totality; rational theory remains separately pull-gated. Absorbs the canonical int/list/map/set/bitvector theory modules, spec enums (constructor injectivity/disjointness) and spec pattern matching as scoped modules.

**Objective:** Add proof-facing formal stdlib models only when a proof or contract workload pulls them. These are not runtime containers: `std.formal_vec`, `std.formal_map`, and `std.formal_set` model mathematical sequences, maps, and sets for contracts, loop invariants, and Lean obligations; `std.bigint` models unbounded mathematical integers for specs before any runtime big-number API exists; `std.rational` stays deferred until exact-ratio specs need it. Each formal module must state its erasure/runtime story, its evidence class, the Lean artifact it lowers to, and the refinement relation to runtime containers such as `Vec`, `HashMap`, `OrderedMap`, and `HashSet`.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0028** (merged): duplicate of R-0060: merge.

### Task R-0062

**Phase:** C

**Pull trigger:** R-0062: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0021/R-0056: merge behavioral regression gates.

**Objective:** Add stdlib authority/allocation/runtime-obligation gates so core helpers cannot silently widen capabilities, allocation behavior, trusted assumptions, or runtime-risk obligations.

### Task R-0063

**Phase:** C

**Pull trigger:** R-0063: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; feed VP S7’s instrumented-semantics validation.

**Objective:** Add sanitizer/runtime-instrumentation hooks as debug and validation surfaces, not proof evidence. Inspired by Odin's sanitizer-facing base surface, Concrete should expose named hooks for bounds, overflow, use-after-free-like trusted/FFI probes where applicable, allocator checks, and generated-code validation. Audit output must classify this as `runtime_checked`, `tested`, or `instrumented`, never `proved`.

### Task R-0064

**Phase:** C

**Pull trigger:** R-0064: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep API inventory now; freestanding implementation after 1.0.

**Objective:** Split hosted versus freestanding-ready stdlib modules at the API level:

 no-alloc/no-OS core modules, allocator-backed modules, hosted OS modules,
 and modules that are explicitly unavailable under freestanding profiles.
 The freestanding target implementation still lands in the freestanding area.

### Task R-0066

**Phase:** C

**Pull trigger:** R-0066: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep under onboarding; no additional flagship gallery.

**Objective:** Add stdlib documentation and checked recipes for C/Rust users without creating a
gallery of standalone public examples:

 `docs/stdlib/STDLIB_GUIDE.md` contains `bytes_text`, `path_fs`, `result_errors`, `vec_map`,
 `parser_cursor`, `json_scan`, `base64_cli`, `uri_parse`, `checksum`, `deterministic_rand`,
 `time_log`, and `capability_io` snippets sourced from an owning test-fixture corpus so they compile
 rather than rot. Each recipe must show the exact `std.*` imports,
 capability set, allocation behavior, and expected audit line.
 Add one blessed systems-program shape, inspired by Elm's success at
 teaching a single architecture but adapted to Concrete's domain:
 **read/acquire -> parse -> validate -> transform -> emit/release**. This is
 not a framework and not hidden control flow; it is an example/documentation
 convention for ordinary tools and protocol handlers. The guide must show
 which stages are pure, which carry capabilities, where `Result` flows, where
 runtime obligations attach, and which report proves each claim. Exercise the pipeline shape in the
 minimal onboarding sequence and later in `secure_update_verifier`/`protocol_state_machine`; its
 tiny byte-oriented CLI and no-allocation parser fixtures must include the source and expected
 `--report caps`, `--report contracts`, and `--report profile` snippets.
 Include a small **stdlib style-coherence pass** in this item: each module
 must choose and document when an operation is a method versus a free
 function. Do not mix `s.len()`, `string_length(s)`, and
 `string_push_char(&mut s, c)` arbitrarily in the same public surface. The
 rule should serve explicitness and capability visibility: methods are fine
 for operations whose receiver clearly owns/borrows the authority; free
 functions are fine for cross-type operations or functions whose capability
 story would be clearer outside a receiver. Add examples that teach the
 chosen style instead of preserving accidental historical names. These are checked documentation
 snippets and fixtures, not additional flagships.

### Task R-0067

**Phase:** C

**Pull trigger:** R-0067: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; preserve oracle mode per vector.

**Objective:** Add a stdlib compatibility/oracle corpus under `tests/compat/stdlib/` (or the corpus
manifest's canonical compatibility home): `fmt_parse_vectors`, `bytes_text_vectors`, `path_vectors`,
`collection_vectors`, `base64_vectors`, `uri_vectors`, `json_vectors`, `semver_vectors`,
`sort_search_vectors`, `checksum_vectors`, `rand_vectors`, and `cli_io_vectors`. Wire it with
`scripts/tests/check_stdlib_compat.sh`; every vector must declare exactly one mode in `manifest.toml`:
`oracle_python`, `oracle_system_tool`, `interp_vs_compiled`, `audit_only`, or
`negative_expected_failure`. Compatibility vectors are not public examples.

### Task R-0068

**Phase:** D

**Pull trigger:** The new compiler runs base64_cli and a selected additional workload that forces an uncovered stdlib surface.

**Reconciliation (2026-10-19):** Keep selected workloads, not the entire candidate list.

**Objective:** Add real stdlib workload checks before the flagships area relies on the library.

 Start with the existing `base64_cli`, classifying it as a workload rather than a flagship. Then
 select forcing workloads from `json_validator`, `ini_parser`, `checksum_cli`, `http_headers`,
 `path_normalizer`, and `lru_cache` or `ring_buffer`; do not instantiate the whole list when the
 three flagships or an existing workload already exercise the same surface. Keep them in the corpus
 manifest's workload home and wire them with
 `scripts/tests/check_stdlib_workloads.sh`. Each workload must build, run,
 compare interpreter-vs-compiled output, and report authority/allocation/
 evidence classes. The oracle requirement is explicit per workload:
 `base64_cli` against Python `base64`, `json_validator` against Python
 `json`, `ini_parser` against a checked-in Python reference,
 `checksum_cli` against Python/hashlib or a checked-in reference,
 `http_headers` against checked-in vectors, `path_normalizer` against
 checked-in platform-specific vectors, and `lru_cache`/`ring_buffer` against
 a checked-in reference model.

### Task R-0069

**Phase:** C

**Pull trigger:** R-0069: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old checked-arithmetic repair campaign retires, but its stdlib correctness and coverage requirements survive. surviving requirement: Audit migrated stdlib sentinel and modular arithmetic, preserving explicit wrapping and non-arithmetic regression coverage. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep confirmed wrong behavior urgent; use `IntArith` contract.

**Objective:** Add a stdlib sentinel/arithmetic audit before broadening hosted APIs. The checked-arithmetic flip exposed syscall/sentinel-style code that relied on silent wrap (`-1 as unsigned` followed by `+ 1`, size/error sentinels, bit-packed flags, checksum/hash arithmetic). Add `scripts/tests/check_stdlib_sentinel_arithmetic.sh` or fold the checks into `check_stdlib_workloads.sh`: every intentional modular/sentinel operation in `std.fs`, `std.process`, `std.net`, `std.io`, hash/checksum code, and binary parsers must use explicit `wrapping_*`/bit operations with a comment or a report-visible classification; every non-intentional trap must be fixed as a

 real bug, not papered over. The gate should include at least one non-
 arithmetic workload path (text/path/UTF-8, fs/process/env capabilities,
 maps/iterators, formatter/parser round-trip, or proof/report views) so the
 post-arithmetic validation does not overfit to numeric examples.

### Task R-0070

**Phase:** D

**Pull trigger:** The new compiler supports the stdlib_client module set sufficiently to run its composed library acceptance fixture.

**Reconciliation (2026-10-19):** Merge coverage requirements into R-0068 if existing workloads suffice.

**Objective:** Add the stdlib area validation workload under the corpus manifest's workload home (for
example `tests/workloads/stdlib_client/`) plus `scripts/tests/check_phase6_stdlib.sh`. It validates
the library and is not a fourth public flagship.

 The client must use `std.option`, `std.result`, `std.bytes`, `std.text`,
 `std.path`, `std.vec`, `std.map`, `std.fs`, `std.io`, `std.fmt`,
 `std.parse`, either `std.json` or `std.base64`, deterministic RNG or
 checksums, sanitizer/runtime-instrumentation hooks where available, and
 `std.test`. CI must build, run, test, audit authority/allocation/evidence
 classes, and compare interpreter-vs-compiled behavior.

### Task R-0071

**Phase:** C

**Pull trigger:** R-0071: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old area-closure scheduling retires, while acceptance and invariant retention remain useful. surviving requirement: Apply explicit stdlib acceptance checkpoints and preserve future-relevant invariants when completed work is archived. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Objective revised by the audit: closing the stdlib area no longer activates QBE; per-release std acceptance and compaction stay with R-0333/R-0515.

**Objective:** Close the stdlib area against its Completion Contract, archive completed work
while preserving future-relevant invariants, and select the next checkpoint from the
current execution queue. Phase closure does not automatically activate QBE.

### Task R-0483

**Phase:** D

**Pull trigger:** R-0483: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The retained body records specific safety controls and an accepted limitation worth preserving despite its closed legacy implementation. surviving requirement: Preserve owner-bound zero-copy view safety and its wrong-buffer, owner-mutation, release and cross-package regression controls, stating the coordinate-provenance limit. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** done — closed 2026-10-09 at `4f005715`; the body is retained for its accepted coordinate/provenance limit.

**Objective:** Make safe zero-copy parsing preserve owner lifetime through every
stdlib wrapper, not only through syntactic `&T` references.

**Status (2026-10-09): CLOSED at `4f005715`** (main CI
[37917463687](https://github.com/unbalancedparentheses/concrete2/actions/runs/37917463687)
passed; `check_view_lifetime.sh` 28/0 there). Owner-bound results landed as
`std.numeric.BoundView`, which owns the `Bytes` it describes; design, fixtures and measured
cost are in [tests/regressions/owner_bound/README.md](../../tests/regressions/owner_bound/README.md).
Wrong-buffer substitution is rejected statically: `BoundView::byte` takes no buffer
argument (`bound_no_buffer_argument`, E0262). The owner is a private field, so it cannot be
mutated while bound (`bound_no_owner_mutation`, E0298), and the view is linear, so it must
be released (`bound_must_release`, E0208). Positive controls cover the zero-copy read, the
moved owner, the cross-package case and rebinding after release. `examples/packet` reads
its payload through a `BoundView` with no copy and no `Alloc`; its predictable profile is
1 failed (`main`, I/O) / 16 passed.

**Accepted limit:** binding re-validates coordinates against the owner's bounds, not
their provenance — coordinates computed from a different buffer of sufficient length still
bind. The guarantee is that reads go to the owner held. `ByteView` deliberately stays
reusable coordinates, and the survey rows asserting that remain in the gate.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0483).

### Task R-0484

**Phase:** C

**Pull trigger:** R-0484: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The legacy summary and opacity machinery retire, but the retained accepted limits and audit-integrity duties survive. surviving requirement: Keep authority and trust meanings coherent across checks and claims, explicitly disclose incomplete dependency analysis, and retain capability-laundering and qualification controls. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** done — closed 2026-10-08 at `245cd51c`; the body is retained for its accepted limits; the typed-descriptor second step is R-0530.

**Objective:** Give capability headers, resource handles and operational effects
one coherent meaning that checking, reports, proof eligibility and policy share.

**Status (2026-10-08): CLOSED at `245cd51c`** (fast-forward from `8187a2f7`; main CI
[37771982899](https://github.com/unbalancedparentheses/concrete2/actions/runs/37771982899)
passed every job). Closure covered proof admission on the shared assumption summary
(bug 076), named trusted boundaries with the obligations they absorb, descriptor-audit
coverage complete over std (audit F6 carried out, `_exit` audited, inventory drift
gated), cross-dependency capability explanations, `std.mem.sizeof` classified as a
compiler intrinsic, and the docs/authority audit. Per-surface status (checked, human
assumption, incomplete coverage, unsupported) is in
[HANDLE_CAPABILITIES.md](../archive/design-records/HANDLE_CAPABILITIES.md#implementation-status-2026-10-07).

**Accepted limits, each gated rather than hidden:**

- single-file mode and `--query` do not analyse dependencies and say so
  (`dependencies_analysed=false`); `--query` refuses a project file
  (`check_capability_explanations.sh`);
- bug 077 (relative call into a nested submodule) is contained: the build refuses it
  (`check_bug077_contained.sh`);
- descriptor classification is a human assumption covered only by the construction/caller
  audit; typed descriptors remain the second step;
- a bodiless user `fn` declaration cannot carry `with(...)`, so user code cannot declare
  an intrinsic;
- reports still compute the older indirect-call opacity analysis beside the summary and
  treat a function as opaque if either says so.

**Not qualified:** the full mutation campaign never completed on a closure candidate —
the Nightly job's mutation-coverage step was cancelled at GitHub's 6-hour limit on both
dispatched runs (37464836373 on `8187a2f7`, 37707710553 on `245cd51c`). R-0484 closed
under the checkpoint exception, which does not certify the campaign or carry over to a
release; the campaign qualified on `670ec85a` (2026-10-11, run 38104755470). F7/F8 stay with R-0013, F9's runtime contract
with spawn's stated assumptions, typed descriptors with the second step.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0484).

### Task R-0485

**Phase:** D

**Pull trigger:** The new compiler runs the migrated parser and base64_cli so explicit cleanup and error propagation can be compared for review friction.

**Reconciliation (2026-10-19):** Keep; refresh obsolete “during R-0483” scheduling.

**Objective:** Improve review clarity without changing explicit error propagation
or implicit-drop policy.

**Status (2026-09-15): planned; apply during R-0483's parser migration.** Keep
`match`/`return`, explicit error conversion and explicit cleanup. Align deferred-call
argument coercions and diagnostics with ordinary calls; specify failure/divergence
during cleanup and test normal-exit cleanup ordering. Improve named library helpers
and ownership/error diagnostics where parser and CLI workloads expose friction.

**Exit:** compare the migrated parser and base64 CLI for nesting, duplicated cleanup,
missed I/O errors and reviewer ability to locate ownership transfers and exits.
Retain concrete before/after examples and regression controls for any implemented
change. Do not equate generated verbosity with human reviewability. `?`, implicit
error conversion, implicit drop and unwinding remain excluded.

### Task R-0486

**Phase:** F

**Pull trigger:** A bounded component runs on the new compiler with public typed contracts, frames and independent evidence replay available.

**Reconciliation (2026-10-19):** Keep workload now; formal graduation after Phase 5/6 prerequisites.

**Objective:** Use one bounded parser/state-machine component to validate compositional
contracts, audit diffs and the external-consumer workflow end to end.

**Status (2026-09-15): planned; workload selection begins with R-0483, proof graduation
depends on the existing identity/task, typed-contract and two-state queue milestones.**
Reuse the parser in the secure update-bundle verifier or protocol-state-machine
flagship already selected in the queue's external-user workflow row. Expose a small C interface and exercise
malformed input, bounded storage, state mutation, zero-copy access, recoverable
failure, resource limits and named FFI assumptions. No fourth public flagship.

**Shared change-and-repair scenario (2026-10-04):** prefer the bounded parser/state
machine in `secure_update_verifier`; use the existing protocol flagship if its admitted
fragment provides the smaller forcing case. Do not require the complete update verifier
or all its cryptographic claims for this milestone. Implement the component, establish
a useful behavioral contract that is not merely a restatement of its body, and retain
baseline evidence. Change a dependency in separate controlled variants: add authority,
introduce/widen an assumption, and invalidate contract/proof evidence. Show the precise
diff and applicable policy rejection, perform an explicit repair, then replay the
resulting evidence with the independent consumer. A contract-preserving body edit is
the positive control for preservation after implementation revalidation. Missing facts
or incomplete analysis produce an explicit incomplete result, never a false-clean diff.

Use this one scenario across typed contracts, R-0182/R-0183 review, existing policy and
package tasks, and R-0353 replay. R-0150 records annotation and manual proof burden,
feedback latency and repair effort from the initial baseline; set improvement targets
after observing that baseline. Measure the non-author session, including failed attempts
and required author assistance. The scenario does not reorder qualification, identity,
contract or independent-consumer prerequisites, or require a registry, new backend or
general heap model.

**Staged acceptance (2026-10-22; Dafny/Jasmin review):** use this same component at each
checkpoint; partial results do not graduate the final exit or reorder prerequisite gates.

| Checkpoint | Prerequisites and acceptance |
|---|---|
| Baseline | Record R-0150's proof/annotation burden and the declared FFI assumptions in the currently admitted fragment. Wrong-contract and stale-evidence controls run as their existing contract/identity/replay owners become available; unavailable checks remain explicitly pending. |
| Compositional calls | After R-0473/R-0474, prove a caller using the exported contract. A separate negative control requests a fact true of the private body but not implied by the public contract: contract-only verification must not establish it. |
| Mutation | After R-0524's frame increment, establish `old` and preservation of untouched parser state. Ghost state is not required merely to pass this checkpoint. |
| Model and lemma | After the relevant R-0526/R-0060 support, connect the implementation to a small total specification model and reuse one lemma. Use existing loop obligations; claim termination only when its obligations close. |
| Consumer | Complete the existing change-and-repair and non-author replay exit with R-0353. Every checkpoint names its theorem boundary and remaining backend/runtime trust; native transport is claimed only for the subset its existing owner has accepted. |

The FFI inventory distinguishes export preconditions required of foreign callers from imported
foreign guarantees. Each condition is checked at the boundary, established by evidence, or explicitly
assumed/trusted. Body-derived safety conditions may suggest a contract but must not silently strengthen
the published precondition or become accepted assumptions. Declared contracts remain authoritative;
implementation revalidation establishes that they suffice. This does not pull R-0284's alignment
surface into the queue. Reference: [Jasmin safety preconditions](https://jasmin-lang.readthedocs.io/en/latest/tools/safety_checker.html).

**Exit:** a consumer relies on a public contract without inspecting private bodies;
calls establish preconditions and use exported postconditions without inlining;
frame/`old` semantics cover the admitted mutation fragment. A contract-preserving
body change refreshes implementation evidence while leaving contract consumers
current; behavior/authority/resource/trust changes produce precise policy and audit
diffs. A non-author builds and consumes the component and replays its admitted
evidence. Measure proof effort, source review clarity, callback/context friction and
runtime costs; use those observations to prioritize subsequent features. Unsupported
heap, relational, resource-proof and backend claims remain explicit.

**Fixed-requirement repair boundary (2026-10-25; planned):** reuse R-0215's
frozen-baseline mutation controls before adding an agent as the repair author.
Keep the accepted requirements outside the agent's writable candidate tree and
reject changes to their identity or allowed assumptions in this experiment.
Use this task's already-owned bounded component when its admitted fragment and
prerequisites are available; the protocol flagship is planned, not an existing
ready-made example. Retain candidate attempts, failed/refused cases, exact diffs
and measurement conditions through the existing workflow.

Replay without the agent/network demonstrates independence from candidate search;
it does not establish an independently implemented checker. Name same-kernel
replay separately from R-0353's independent-consumer evidence, and keep
source/model correspondence separate from logical validity. The agent variant
follows this task's identity, contract and replay prerequisites; it does not
introduce a new harness requirement or reorder the current queue.

### Task R-0488

**Phase:** D

**Pull trigger:** The new compiler supports the first workloads’ mutable-buffer, callback and fallible-handle APIs for a composed usability audit.

**Reconciliation (2026-10-19):** Keep safety audit early; later contract/diff additions stay with existing owners.

**Objective:** After R-0484, audit parameter-mutation guarantees, then pressure-test
the library model before adding more language machinery.

**Status (2026-09-30): planned; depends on R-0484, not yet scheduled in the execution queue.**
Preserve R-0483 and the existing safety priorities; schedule this checkpoint explicitly
after those priorities are reviewed. Qualify
existing `&T`, `&mut T` and ownership semantics before proposing new syntax. This is
distinct from the post-R-0004 **mutation-testing qualification** campaign. The
bounded audit does not bypass identity, receipt or typed-contract prerequisites.

1. **Parameter mutation first.** Inventory mutation through parameters, nested
   fields, aliases, callbacks, trusted wrappers and imported APIs. Verify that safe
   read-only access cannot become mutable and that intended mutation is visible in
   parameter types. Retain an accepted mutable-buffer update, a rejected read-only
   write, and an indirect/cross-package laundering case. An empty `with(...)` may
   coexist with parameter mutation; reports and contract admission must not infer
   purity from it. Repair concrete violations, with causal mutation tests where
   they demonstrate enforcement.
2. **Library usability next.** Exercise buffered I/O, fixed and growable buffers,
   sockets, stored callbacks, explicit cleanup and package boundaries in existing
   libraries/examples. Include closing an owned handle versus transferring it, and
   `Writer<{}>` versus `Writer<Alloc>`. Record annotation propagation, diagnostic
   clarity, duplicated cleanup, missed errors and audit effort. Reuse R-0485's
   review criteria and R-0486's flagship component; no new public flagship. Require
   repeated workload evidence before introducing more expressive language features.
   Graduation also needs R-0021's usable `std` subset (2026-10-21): the APIs the first two workloads
   need, validated for ownership, errors and the foreign boundary together.
   Include one fallible writer or owned-handle operation with R-0285's failure
   contract: record partial progress, ownership and usable state on error, and
   whether retry is valid. Retain a successful operation and a controlled failure
   after partial progress, checking explicit cleanup and preventing a retry from
   silently duplicating completed work. Reuse the existing effect-simulation seam
   when applicable. State guarantees first with ordinary types and API contracts;
   formal before/after and `Result`-case specifications follow R-0473/R-0474 and
   the queued two-state mutation work. Fatal abort/OOM remains a separate boundary.
3. **Optional read/write footprints — existing contract owners.** Feed a concrete
   preservation need into R-0473/R-0474, the queue's two-state mutation contracts,
   and R-0175. Start with bounded records/fixed arrays: update a cache or counter
   while preserving configuration. Specify permitted reads and writes, with a valid
   update control and a rejected out-of-footprint access. Keep footprints distinct
   from external authority and input/output dependencies. Preserve R-0175's workload
   gate; this does not authorize a general heap or separation-logic implementation.
4. **Auditable dependency upgrades — R-0182/R-0183/R-0184/R-0372.** Review diffs must
   expose added `Network`, new foreign assumptions, and widened mutation contracts
   once supported. Include an unchanged-contract control and a transitive dependency
   change. Configured policies reject forbidden widening. Reports retain assumption
   provenance and distinguish checked declarations from audited foreign behavior.
   Reuse package/evidence infrastructure and its existing prerequisites.
5. **Explanations at failure sites — R-0137/R-0466.** Missing-capability diagnostics
   trace the source-located path from caller through helper to `Writer<Console>` and
   name the required permission. Assumption reports identify the declaration or
   conversion where trust entered, including across packages. Gate structured paths
   and valid remedies, not exact prose; do not suggest broad authority simply to
   silence an error.

**Exit:** the parameter audit and bounded library pass have reproducible results,
positive/negative controls and owned follow-ups. Items 3–5 strengthen their existing
owners' acceptance criteria; their later implementation is not required to close this
checkpoint and does not advance ahead of the strict queue's prerequisites. Capability
completeness alone does not establish that everything touched is known.

### Task R-0448

**Phase:** research

**Restructured (2026-11-02):** retired with the legacy compiler: The old spike and emitters retire, but explicit badge, consumer and unsound-hypothesis regression controls survive. surviving requirement: Optional cross-kernel replay on new-Core claims must bind identical obligations, retain independent attestations and reject vacuous, missing-kernel, trusted-laundered or disagreeing results. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0448: when phase research becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Retire scheduled graduation; retain experimental replay work and honest labels.

**Nonlinear tier (workload-gated; carried from the archived rung probes of 2026-08-05):** Rocq `nia`
and Isabelle close the probed nonlinear goals, but Lean has no general nonlinear procedure and
Mathlib is not a dependency, so a nonlinear tier can attest at most two kernels. Build it only when a
workload needs nonlinear claims, label it as two-of-three rather than "multi-kernel", and keep H21's
trusted SMT label; adding Mathlib is a separate dependency decision. See
[the archived probe](../archive/ROADMAP_HISTORY.md#rung-3--nonlinear-arithmetic-2-of-3-kernels-and-that-is-a-ceiling).

**Objective:** Graduate multi-kernel evidence from `spike/multi-prover-evidence` to a supported feature, per `research/proof-evidence/multi-kernel-evidence-graduation.md`. The product is portable evidence — "replay our claims with the kernel you trust" — not agreement for its own sake. Status is DERIVED by composing per-kernel receipts on one obligation digest (R-0004's receipt mechanism), never emitted by a coordinator code path. The claim record carries structured per-kernel `validated_by` entries plus the independence field (`independent_of`: spec / kernel implementation / foundations / bridge); the composite badge string is display only (R-0440's no-erased-dimensions rule). The spike branch may continue as an experiment, but supported-feature graduation follows R-0004, R-0450, R-0440, and the pulled-forward correctness queue; active ownership is not permission to displace known semantic defects.

   **Blocking merge item added 2026-07-31, CLEARED 2026-08-03 (R-0461).** The badge was
   attachable to a vacuous obligation: an unproven loop invariant was assumed without
   composition, so a guaranteed out-of-bounds access reported
   `proved_by_multi_kernel (3: lean, rocq, isabelle)` and the binary aborted. Graduating a
   badge with that property would have shipped the defect with a stronger name on it.
   R-0461 closed H23 — the obligation now caps to `assumed`, and BOTH release stances
   reject the fixture (`E0617` under forbid-assume, `E0616` under require-two-kernels).
   `examples/unsound_hypothesis/` is retained as a regression guard.

   Merge bar (the graduation note's verified list): badge-teeth negative
   case (a weakly-bounded `a * b` closes with no kernel and stays
   `unproven`), kernel-absent honesty (no `coqc` → no attestation), class
   distinctness, no laundering past `trusted`, and a mutation proving the
   badge disappears when a kernel leaves the agreement set. The
   emitter-agreement differential runs generated obligations across kernels
   and reports DISAGREEMENT as signal (a lowering defect or a
   decision-procedure discrepancy), never noise. Credibility gate: one
   flagship row (`hmac_sha256` or `vc_suite`) showing
   `proved_by_two_kernels`, replayable by an outsider with either kernel.
   Provers stay optional tooling (`nix develop .#provers`); the linear
   fragment boundary stays a gate, with scope growth only through R-0450's
   named transforms. Dependencies: R-0450 (the IR), R-0004 (receipts),
   R-0440 (the independence field ships inline here as its first consumer
   if the full model has not landed), and **R-0459 for the credibility gate
   specifically** — measured 2026-07-31: `vc_suite` produces *no* linear
   runtime-safety obligations at all, and `elf_header`/`crypto_verify` produce
   exactly one badged row between them, `rand.random_range#div0`, a stdlib
   divisor obligation rather than anything the flagship itself asserts. Every
   family generated today is arithmetic (R-0459's diagnosis), so the flagship row
   this criterion asks for cannot exist until non-arithmetic families do. Do not
   read the criterion as merely un-run: it is currently unsatisfiable.
   On merge, TRUSTED_COMPUTING_BASE.md
   records both trust directions: agreement reduces kernel-soundness
   trust; bridge trust is untouched until realization (R-0449).

### Task R-0027

**Phase:** E

**Pull trigger:** R-0027: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0061: merge.

**Objective:** Extend the shipped pure-core proof arc over actual `Option`/`Result`, `Bytes`/slice, numeric checked helpers, and checked text/path conversions against their documented contracts.

### Task R-0028

**Phase:** E

**Pull trigger:** R-0028: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0060: merge.

**Objective:** Proof-facing formal stdlib models (`formal_vec`, `formal_map`, `formal_set`, `bigint`, lemma helpers) once contracts need them.

### Task R-0029

**Phase:** after-1.0

**Pull trigger:** A new-compiler application needs a deferred compression, crypto, networking or threading family covered by this merged ledger.

**Disposition (2026-10-19):** duplicate of R-0065: merge deferral policy; retain narrow Phase 3 module owners.

**Objective:** Broad compression/crypto/networking/threading only after workload demand.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0029).

### Task R-0061

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The selective proof arc survives, but its supported legacy replay requirement is superseded by tag-only replay. surviving requirement: Prove selected stable pure stdlib helpers against new-Core semantics and gate evidence against body or specification drift, while legacy evidence remains replayable only from the tag. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0061: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep on generated Core statements; preserve supported legacy replay.

**Objective:** Add a **selective shipped pure-core stdlib proof arc**, distinct from the pull-gated formal-container work in 24a. This proves stable stdlib code users actually call, not separate mathematical containers, and it is not a mandate to prove all 414+ public API rows before the API surface has been validated by workloads. Scope v1 narrowly:

 `Option`/`Result` helpers (`map`, `and_then`, `map_err`, identity /
 composition where expressible), numeric checked helpers (success/failure
 agreement with documented overflow, divide-by-zero, and narrowing
 behavior), `Bytes`/slice pure helpers (`view`, `cmp`, checked `get`/`set`,
 raw-data preservation, bounds behavior), and checked text/path conversions
 (valid UTF-8 boundary, raw bytes preserved, unchecked constructors named
 `_unchecked`). Non-goals: hosted APIs, allocation behavior, filesystem,
 network, process, clock, console effects, broad collection proofs, and the
 `formal_vec`/`formal_map`/`formal_set` model-container layer. Treat tests,
 oracle vectors, manifest facts, mutation gates, and capability/allocation
 gates as the right evidence for unstable or broad APIs; upgrade an API to
 `proved` only after it is small, pure, central, and workload-pulled. Done
 when a small Lean proof suite is CI-gated, each covered API's manifest/report
 row distinguishes `proved` from `tested`/`enforced`, and a negative fixture
 proves the report cannot keep `proved` after a body/spec drift. This is the
 "proved pure core" part of the stdlib area excellence contract; do not let it be
 deferred behind breadth modules such as JSON, CLI, or networking, but also
 do not turn it into an exhaustive stdlib-proof sweep before API ergonomics
 have been validated.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0027** (merged): duplicate of R-0061: merge.

### Task R-0065

**Phase:** after-1.0

**Pull trigger:** A new-compiler workload demonstrates a concrete API gap in the deferred-family ledger and selects only that family for implementation.

**Reconciliation (2026-10-19):** Keep explicit deferral ledger, not a phase-completion blocker.

**Objective:** Record deliberately deferred stdlib families so they do not disappear from planning: compression/archive formats, broad crypto beyond the narrow hash/HMAC/constant-time story, PEM/ASN.1/X509, MIME helpers beyond the first workload, full HTTP client/server, DNS/Unix-socket breadth, dynamic libraries, OS debug formats (`ELF`, DWARF, image parsing), atomics, threads, SIMD, target/ABI databases, and platform-specific C/POSIX wrappers. Each stays package-later, backend-later, freestanding-later, or research-later until a workload forces it.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0029** (merged): duplicate of R-0065: merge deferral policy; retain narrow Phase 3 module owners. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0029)

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

### Task R-0078

**Phase:** E

**Pull trigger:** R-0078: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old CodegenInput job refactor retires, while determinism and parallelism controls remain useful. surviving requirement: Keep native emission deterministic through immutable function inputs and canonical merging, gating any parallel emission against serial output. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; share scheduling/reproducibility with R-0146/47.

**Objective:** Make backend emission function-local and deterministic where possible.

 Each function job receives immutable `CodegenInput`/target facts and returns
 a function fragment plus declarations/helper requirements; it may not
 mutate program-wide alias, type, string, or symbol state. Merge fragments in
 canonical symbol/module order and deduplicate declarations through typed
 keys. Start sequentially, add a serial-versus-parallel byte-identity gate,
 and only then allow parallel codegen. This follows Zig's successful isolated
 codegen-job shape without pulling in its linker architecture.

### Task R-0081

**Phase:** E

**Pull trigger:** R-0081: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old QBE module firewall retires, but oracle independence and correlation disclosure survive. surviving requirement: Keep LLVM and LCB adapters independent and disclose shared helpers, with import-direction and cross-emitter-call controls. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** merged — emitter independence between the LLVM adapter and the LCB adapter, and shared-helper disclosure, move to R-0505/R-0517.

**Objective:** Preserve oracle independence with a review/firewall gate. QBE modules may import SSA, target-neutral layout/codegen facts, diagnostics, and semantic identities, but not LLVM AST/printer/emitter modules. LLVM and QBE may share runtime helpers only when the manifest discloses the correlation. Add an import-direction test and a source scan preventing calls from one emitter into the other.

### Architecture And Independence Rules

### Task R-0086

**Phase:** E

**Pull trigger:** R-0086: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The standalone QBE implementation slice is superseded, but the body explicitly transfers its semantic vectors. surviving requirement: Retain scalar, width, cast, memory, float and wrapper semantic vectors as staged LCB-adapter tests. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded — its semantic vectors become adapter test vectors for V1 (R-0511/R-0517); later widths, casts, memory and floats follow the V-slices.

**Objective:** Land a scalar smoke slice first: integer and boolean constants, arithmetic, comparisons, casts, direct calls, returns, blocks, branches, phi/copy lowering, stack slots, loads/stores, globals, string data, and the Phase 2 main wrapper. Map Concrete `i8`/`i16`/`i32`/`bool` temporaries deliberately onto QBE word operations with explicit truncation and signed/unsigned extension; map `i64`/native pointers to long and `f32`/`f64` to single/double.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0086).

### Task R-0091

**Phase:** E

**Pull trigger:** R-0091: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old QBE entry generator retires, but equivalent native test semantics remain required. surviving requirement: Run the same discovered tests and filters under each native profile with matching result accounting and output controls. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** merged — native test-mode entry generation joins R-0075's driver and the LCB-native profile (R-0517).

**Objective:** Add QBE test-mode entry generation so `concrete test` can execute the same discovered tests and module filter under both native backends. Preserve the test runner's result accounting and output exactly; do not special-case a smaller, silently different QBE test semantics.

### Differential Validation And Failure Localization

### Task R-0097

**Phase:** E

**Pull trigger:** R-0097: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by R-0511/R-0512: merge profile admission; retain no silent fallback.

**Objective:** the QBE backend-validation area may close with a declared subset, but that subset must be honest:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0097).

### Task R-0098

**Phase:** G

**Pull trigger:** R-0098: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The interpreter/LLVM/QBE prescription retires, while profile-aware flagship validation survives. surviving requirement: Run flagship workloads across their admitted new-compiler profiles with explicit owned exclusions and differential observations. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** duplicate of R-0137: merge profile-aware workload matrix and owned exclusions.

**Objective:** Exercise the completed subset immediately in the flagships area: every new flagship workload that stays inside it runs interpreter/LLVM/QBE differential checks;

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0098).

### Task R-0102

**Phase:** E

**Pull trigger:** R-0102: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old SSA-to-QBE ledger retires, while the enumerated equivalence obligations remain. surviving requirement: Record and discharge LCB-adapter semantic gaps for arithmetic, floats, memory, traps and conversions using explicit checks or unsupported classifications. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded — the operation gap ledger becomes typed pass obligations (R-0506) and the adapter's obligations (R-0517).

**Objective:** Write a QBE semantic-gap ledger before broad coverage. For every Concrete SSA operation, record the QBE instruction sequence and the assumptions that make it equivalent: signed division overflow and divide-by-zero, shift counts, float-to-int out-of-range behavior, NaNs and signed zero, sub-word truncation/extension, uninitialized bytes, invalid pointers, alignment, aliasing, aggregate padding, unreachable paths, traps, and integer/pointer conversions. Where LLVM uses poison/undef, flags, or intrinsics that have no direct QBE analogue, lower to explicit checks/helpers or mark the operation pending; never inherit accidental host behavior as Concrete semantics.

### Task R-0103

**Phase:** D

**Pull trigger:** R-0103: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The QBE-unit implementation changes, but the named identity regressions remain language and linking requirements. surviving requirement: Retain module and symbol collision, escaping, visibility and deterministic-composition controls, extending them to separate units when supported. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0492/R-0505: merge; general separate compilation arrives in V6.

**Objective:** Make symbol and module composition a first-class gate. Test duplicate basenames, import aliases, colliding private names, generic specializations, extern/defined-symbol collisions, linker aliases, string and type-name escaping, multi-module declaration deduplication, visibility, and deterministic ordering. Compile both one combined QBE unit and, once supported, separate units linked together; their observations and exported symbol sets must agree.

### Task R-0104

**Phase:** E

**Pull trigger:** R-0104: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The cleaned-SSA/QBE emission comparison retires, while reproducibility and tool identity survive. surviving requirement: Check deterministic native artifacts and bind toolchain, helper, target and flags into manifests and cache keys. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** merged — deterministic emission and toolchain identity join R-0146; the LCB version and upstream lock are pinned by R-0517.

**Objective:** Add deterministic-emission and reproducibility checks: identical cleaned SSA plus an identical target/toolchain configuration must produce byte-identical QBE IL, assembly where the pinned toolchain permits it, link manifests, and backend observation records. Debug paths, temporary paths, hash-map order, host locale, parallel test scheduling, and environment variables must not perturb semantic artifacts. Record the QBE version, target, assembler, C compiler/linker, runtime-helper hash, and flags in the build/debug bundle and cache key.

### Task R-0108

**Phase:** C

**Pull trigger:** R-0108: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0416/R-0515: merge profile-labelled budgets and measurements.

**Objective:** Establish performance guardrails without making QBE win benchmarks. Track compiler wall time, peak memory where available, emitted IL/assembly/object size, final binary size, and runtime for a small stable corpus against LLVM.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0108).

### Task R-0109

**Phase:** C

**Pull trigger:** R-0109: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The shared-C prescription retires, but helper ABI, ownership and validation requirements survive. surviving requirement: Version runtime-helper contracts, deterministic builds and semantic vectors, invalidating dependent caches and disclosing reduced oracle independence. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0503/R-0146: merge; remove assumption helpers must be C.

**Objective:** Define runtime-helper ownership and versioning. Shared C helpers must have a narrow, documented ABI; checked-in source; deterministic build; symbol namespace; target/feature manifest; sanitizer tests; and interpreter/LLVM/ QBE semantic vectors. Decide explicitly which helpers are common semantic infrastructure and which remain independently emitted oracle logic. A helper change invalidates both backend cache keys and reruns every builtin/ ABI consumer; sharing a helper must be disclosed when it reduces oracle independence.

### Task R-0110

**Phase:** C

**Pull trigger:** R-0110: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** merged — profile naming and evidence attribution join R-0075/R-0333; the profiles are `llvm` and the LCB-native `verified` profile.

**Objective:** Add release-surface and compatibility discipline even while experimental:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0110).

### Task R-0111

**Phase:** E

**Pull trigger:** R-0111: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The mandatory QBE showcase retires, while the disposition explicitly retains reusable admission exits. surviving requirement: Provide an end-to-end LCB-native capstone with reusable profile-admission evidence and honest supported scope. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0511/R-0137: merge reusable exits; retire required QBE showcase.

**Objective:** Add a QBE backend-validation-area capstone artifact:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0111).

### Task R-0112

**Phase:** -

**Pull trigger:** R-0112: no implementation pull; retained as the retirement record for legacy-compiler-final.

**Disposition:** retired with `legacy-compiler-final` on 2026-11-02 — The task only retires the legacy direct Pipeline.emit path and duplicate LLVM branches during an in-place backend refactor.

**Objective:** Add a refactor-retirement gate. Once LLVM runs through `CodegenInput`, the structured backend driver, and the extracted layout/runtime facts with byte/behavior parity, delete the old direct `Pipeline.emit -> String` path, duplicated CLI compile/link branches, and emitter-local program-wide fact discovery. Do not carry two LLVM pipelines through the QBE implementation;

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0112).

### Task R-0113

**Phase:** E

**Pull trigger:** R-0113: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old CodegenInput/QBE architecture trials retire, but the evidence-based external-backend decision survives. surviving requirement: Preserve the external LCB decision and require measured slice evidence on independence, fault detection, performance and duplication before expanding or reopening backend architecture. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Kept as the QBE reconciliation decision record; decided 2026-10-19 that the Lean Compiler Backend is the QBE path (R-0517).

**Decision (2026-10-19):** the Lean Compiler Backend is Concrete's QBE path. LCB is a Lean 4 port of QBE 1.3
developed as a separate general-purpose project; Concrete consumes it through R-0517's adapter and builds
no QBE backend of its own. Each the QBE backend-validation area task carries its disposition (kept, merged with its new owner,
or retired with its useful controls transferred to R-0517 or a named owner) in its **Phase** line and the
[reconciliation record](ROADMAP_RECONCILIATION.md). R-0296, R-0305 – R-0311, R-0331, R-0424 and the Wasm
entrance criteria were updated in the same change. This task remains the decision record and owns any
reopening, which requires measured evidence that a separate QBE path finds bugs or serves users LCB does not.

**Objective:** Keep a QBE backend-validation-area decision log tied to implementation evidence. For every vertical slice record: the supported semantic families, newly shared facts, deliberately independent logic, LLVM textual changes (if any), three-way result counts, mutations caught, performance deltas, and remaining blocker.

 If the first three executable slices show that `CodegenInput`, the typed QBE
 AST, or the driver boundary creates duplication without improving failure
 localization, revise or delete that abstraction before expanding coverage;
 the roadmap is a hypothesis, not permission to preserve a bad framework.

### Task R-0072

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: Direct QBE provisioning is delegated to LCB, but tool identity and discovery requirements remain. surviving requirement: The new compiler's LCB-native toolchain must have pinned, verified dependencies, explicit discovery and no implicit compilation-time downloads. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0072: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** retired — LCB pins QBE 1.3 in its own upstream lock; Concrete pins the LCB version under R-0517.

**Objective:** Provision QBE as an explicit toolchain dependency. Pin a supported release and checksum in Nix/CI, verify the installed binary rather than accepting an arbitrary `PATH` match, document source/bootstrap and system-package paths, and make local discovery available through `concrete doctor` / agent feature JSON. Tool download is never an implicit compilation side effect.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0072).

### Task R-0083

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The disposition removes the direct QBE selector but expressly transfers the no-silent-fallback rule. surviving requirement: Backend selection in the new compiler must be explicit and must never silently fall back to another backend. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0083: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** retired — there is no Concrete QBE backend to select; the no-silent-fallback rule moves to R-0511/R-0517.

**Objective:** Add an explicit backend selection surface:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0083).

### Task R-0084

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The proposed Concrete QBE AST, printer and SSA translation are replaced by LCB's input layer. surviving requirement: The new compiler must construct typed LCB input through a separate adapter and record the pinned backend tool identity. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0084: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** delegated — a typed QBE-style IR and printer are LCB's input layer; Concrete's adapter constructs LCB-IR under R-0517.

**Objective:** Add `Concrete/Backend/QBE.lean` for a small typed QBE IL AST, `EmitQBE.lean` for the pure text printer, and an SSA-to-QBE translation module separate from `EmitSSA.lean`. Do not build QBE text through ad-hoc concatenation inside CLI code. Pin the supported QBE release/tool identity in CI and record it in debug/audit output.

### Task R-0087

**Phase:** D

**Pull trigger:** R-0087: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by VP V7/R-0512: merge with total float model and profile-specific trust labels.

**Objective:** Give floating point its own executable slice rather than treating the `f32`/`f64` type mapping as coverage. Add arithmetic; ordered/unordered comparisons; NaN, infinity, signed-zero, subnormal, and precision-change vectors; int-to-float conversions; and checked float-to-int conversions.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0087).

### Task R-0088

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: Direct QBE aggregate emission is superseded, but layout and ABI realization obligations transfer to the adapter. surviving requirement: The LCB adapter must realize aggregate and memory layouts explicitly and test offsets, alignment, padding, payload initialization and aggregate calls. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0088: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded — aggregate/memory ABI realization moves to R-0502 (layout) and the adapter boundary (R-0505/R-0517) across V2–V6.

**Objective:** Add memory and aggregate support: fixed arrays, structs, packed/explicitly aligned layouts, enums, canonical `Option`/`Result`, strings, vectors, nested places, byte-union payloads, aggregate copies, and small/large aggregate calls/returns. QBE aggregates are memory/ABI objects rather than general SSA temporaries, so make materialization explicit and test tag/payload offsets, padding, alignment, and partially initialized payload storage.

### Task R-0089

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The direct QBE implementation is superseded while call and platform matrices remain adapter obligations. surviving requirement: The LCB-native boundary must validate supported calls, C ABI shapes and targets and reject unsupported cases before emission with stable diagnostics. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0089: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded — call/FFI/ABI matrices apply at the LCB adapter boundary (R-0505/R-0517); LCB's own ABI coverage (its P07) is an external dependency.

**Objective:** Add calls and platform boundaries: indirect calls, externs, variadics used by the runtime, argc/argv accessors, symbol visibility, linker aliases, C ABI scalar/aggregate classification, and Linux/macOS target selection for the architectures QBE and Concrete both declare supported. Unsupported target, calling convention, TLS, or ABI shapes must fail before emission with a stable diagnostic.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0089).

### Task R-0099

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: Direct QBE graduation disappears, but the body names surviving native-profile acceptance requirements. surviving requirement: Graduating the LCB-native profile requires validated input, target and C ABI matrices, source mapping, artifact identities and release soak controls. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0099: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** retired — graduation of the native path is R-0517/R-0505's LCB-native profile; no QBE graduation exists.

**Objective:** Graduation to a supported release backend occurs only in the backend-contracts area: migrate QBE from the direct validation path onto `ValidatedBackendIR`, pass the full target/toolchain and C ABI matrices, integrate debug/source maps and incremental artifacts, attach translation-validation status, define optimization/tool-version policy, and pass release/platform soak gates.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0099).

### Task R-0100

**Phase:** after-1.0

**Pull trigger:** A new-compiler deployment requires Wasm32/WASI and the validated BIR entrance criteria are met.

**Disposition (2026-10-19):** superseded — Wasm/Cranelift no longer wait for QBE; the WebAssembly area entrance criteria (R-0305) consume validated BIR.

**Objective:** After the QBE backend is stable, evaluate a complementary target rather than starting one in parallel. WebAssembly is the provisional next backend, governed by the WebAssembly area's entrance criteria and narrow Wasm32/WASI scope.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0100).

### Task R-0101

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The standalone QBE verifier is delegated to LCB while adapter validation and diagnostic controls survive. surviving requirement: The new compiler must validate its LCB adapter output, reject illegal artifacts structurally and retain failing artifacts with compiler-owned diagnostics. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0101: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** delegated — legality verification of the QBE-style IR is LCB's (its F04/V02); Concrete validates adapter output under R-0517.

**Objective:** Define and gate QBE-IL legality independently of execution. Add a pure verifier for Concrete's typed QBE AST (definition-before-use where QBE requires it, unique temporaries/blocks/symbols, legal base/extended type positions, phi predecessor agreement, terminator completeness, entry-block restrictions, call/return ABI agreement, aggregate definitions before use, alignment validity, and escaped identifier/string syntax), then run the pinned `qbe` parser as a second validation layer. An emitter failure must be a structured Concrete diagnostic with the `.qbe` artifact retained; raw QBE/assembler/linker errors must not be the first user-facing diagnosis for a compiler-owned invalid artifact.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0101).

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

### Task R-0117

**Phase:** C

**Pull trigger:** R-0117: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: Legacy scanner parity and cutover retire, while canonical interface facts and fail-closed consumers survive. surviving requirement: Derive public interface facts once from the new compiler and share them across snapshots, docs, audits and manifests, retaining fact-mutation controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; source of R-0021/R-0056 facts.

**Objective:** Replace the temporary stdlib scanner with compiler-derived interface facts Emit canonical public symbol/signature, visibility/construction rights, capabilities, allocation, ownership, failure, trust, platform, and evidence facts from the compiler's resolved/checked interface artifact. Compare them against the repaired balanced scanner until every row agrees, then delete the scanner rather than retaining two authorities.


Acceptance: nested generics/function pointers, externs, trusted impls,
overloads/renames/re-exports, private representation, duplicate names, and all
public std modules are covered; mutations of each fact class fail closed; API
snapshots, docs, audit output, incremental keys, and backend manifests consume
the same artifact.

### Task R-0119

**Phase:** G

**Pull trigger:** R-0119: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep workload/graduate distinction.

**Objective:** Graduate and maintain exactly three public flagships, keeping their evidence bundles
green:

1. **`hmac_sha256`** — cryptographic correctness and exact extraction/spec-drift refinement.
2. **`secure_update_verifier`** — systems-security showcase that absorbs the parser/serializer and
   file-integrity stories: canonical manifest decoding, target identity, segment
   bounds/overlap/overflow, payload hash or HMAC, version monotonicity/rollback prevention,
   trusted-key/advisory changes, exact manifest-to-payload binding, receipt currency and R-0353
   independent verification.
3. **`protocol_state_machine`** — temporal/stateful correctness with a canonical bounded packet
   codec, explicit state transitions, invalid-input/transition refusal, bounded resources and a
   replayable safety claim.

`parse_validate`, `crypto_verify`, `fixed_capacity`, and `constant_time_tag`
are demoted from flagship status; they may remain as tests, fixtures, proof
patterns, or components absorbed into the three flagships above, but they do
not expand the public example set.

Do not add a new public flagship before R-0353 supplies an independently implemented consumer.
Deepen HMAC first. Build the secure-update evidence workflow after R-0353, add its bounded mutable
parser after two-state contracts, and add richer rollback/chunk/Merkle properties only after the
specification library supports them. Graduate the protocol state machine after the same
identity/task/verifier seam and when its state/totality requirements are supported rather than
simulated.

A public showcase is admitted only when it has a distinct user/security purpose, README and threat
model, positive and negative workflows, evidence production and independent consumption,
stale/revoked/substitution demonstrations, permanent CI ownership and a maintenance owner. A
proposed fourth showcase must replace or merge with an existing role. This cap does not limit
regression, adversarial, compatibility or performance coverage.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0131** (merged): duplicate of R-0119: merge as portfolio acceptance criterion.

### Task R-0124

**Phase:** E

**Pull trigger:** R-0124: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: ProofKit refactoring and R-0512 migration retire, but exact-extraction and spec-drift regressions remain essential. surviving requirement: Retain HMAC exact-source/spec correspondence and source-perturbation stale-proof controls using fresh new-Core proofs without a fixed proof count. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep controls; replace fixed count and migrate under R-0512.

**Objective:** Keep `hmac_sha256` as the regression anchor for exact-extraction, spec-drift-tied refinement: source perturbations must make the registered proof stale, and ProofKit refactors must keep the 11 proof checks green.

### Task R-0125

**Phase:** E

**Pull trigger:** R-0125: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The stored-fingerprint and pre-R-0004 caveat are legacy-specific, while ongoing claim alignment survives. surviving requirement: Keep HMAC publication claims aligned with freshly established correspondence, exact binding coverage, assumptions and backend trust boundaries. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep ongoing claim alignment; retire pre-R-0004 caveat.

**Objective:** Keep the paper, website, README, and showcase manifest aligned
with HMAC's actual claim: the stored-fingerprint extracted source body refines
an independent SHA-256/HMAC spec under named assumptions and trusted backend
boundaries. Until R-0004 lands its full `ProofSubjectDigest`, do not broaden
“body-fresh” into “every signature, contract, dependency, and replay-context
fact is pinned.”

### Task R-0126

**Phase:** E

**Pull trigger:** R-0126: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep in Core-oriented ProofKit; require real reuse.

**Objective:** Use HMAC-derived proof patterns only after they move into ProofKit or an explicit example guide; do not let future flagships copy private `Sha256Refine` scaffolding as hidden infrastructure.

### Task R-0128

**Phase:** G

**Pull trigger:** R-0128: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; use authoritative capability rules/certificates.

**Objective:** Satisfy the authority/capability claim through the `secure_update_verifier` flagship: a privilege-separated verification core that cannot touch files/network/processes except through named wrappers, and whose authority boundary is independently auditable.

### Task R-0129

**Phase:** E

**Pull trigger:** R-0129: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep bounded interop and named trust assumptions.

**Objective:** Demonstrate one trusted C/FFI boundary as part of the `secure_update_verifier` or as a standalone evidence-class fixture. Do not promote it to a fourth flagship; keep the public example set capped at three.

### Task R-0130

**Phase:** G

**Pull trigger:** R-0130: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; normal-exit conservation, not cleanup-on-abort promise.

**Objective:** Satisfy the ownership-heavy resource claim through the `secure_update_verifier` or
`protocol_state_machine` flagship: explicit cleanup, borrow-heavy APIs, no leaks/double-use and
evidence explaining why. If a separate resource demo is needed, place it in the owning test or
benchmark corpus, not `examples/`.

### Task R-0132

**Phase:** D

**Pull trigger:** R-0132: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep byte/path/exit/error oracle coverage.

**Objective:** Add Unix-tool/protocol compatibility as a workload and fold the relevant behavior
into `secure_update_verifier` or `protocol_state_machine`: byte-preserving I/O, path/OS-string
handling, handle-relative filesystem authority, exit-code/error compatibility, ignored-result
diagnostics and oracle tests. This work does not create a fourth public flagship.

### Task R-0133

**Phase:** F

**Pull trigger:** R-0133: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The production-readiness checks survive while the enumerated legacy reports must be rebuilt. surviving requirement: The protocol credibility slice must exercise realistic buffer code, contracts, kernel proofs, runtime obligations, persisted counterexamples and independently consumable evidence through the new compiler. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep implementation early; independent consumer before public graduation.

**Objective:** Add a thin end-to-end credibility slice before the larger workload ladder, so skeptical users can replay one compelling artifact before the full old phases 5/6 and the provable-subsets and runtime-safety areas surface is complete. This is the entrance version of `protocol_state_machine`, not a temporary fourth showcase. Target:

 `examples/showcase/protocol_state_machine/`, beginning with a no-allocation
 packet-window parser/verifier (WebSocket frame decoder, TLS record-header parser,
 DNS packet parser, or HTTP/1 header parser) and growing in place. The slice must demonstrate the
 actual production-readiness claim, not a toy proof: fixed buffers, `Bytes`,
 `ByteView`, arrays, `Result`, pattern matching, explicit capabilities,
 value-model accessors / scoped borrows, no returned references, and bounded
 allocation or no allocation. It must include positive and negative corpus
 fixtures, fuzz/property-style generated cases with persisted
 counterexamples, an oracle or interpreter-vs-compiled check, one
 runtime-safety obligation, one source contract, one Lean-checked proof, one
 `proved_by_kernel_decision` discharge, and one audit/release bundle. Wire
 `scripts/tests/check_credibility_slice.sh`; the gate must compile and run
 the example, replay the proof/evidence checks, emit `--report contracts`,
 `--report obligation-ledger`, `--report caps`, `--report layout`, and
 `--report fingerprints`, persist any counterexample as a regression, and
 produce a README transcript that a non-author can run without understanding
 compiler internals. This is intentionally a vertical slice, not a new
 parallel track; it exists to validate the bet before the later 10k-line
 workload ladder. Do not start this slice until the immediate old phase 6
 blockers that directly affect parser/buffer code are either done or
 explicitly deferred with examples: the historical phase 6 array-literal
 element-inference item, match guards / OR patterns / match-on-reference item,
 and `defer`/cleanup item if the chosen slice owns resources. It also does not graduate or become
 public before R-0353 supplies the independent-consumer workflow required by R-0119.

### Task R-0134

**Phase:** G

**Pull trigger:** R-0134: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep bounded selection; align large-port priority with R-0490.

**Objective:** Add a graduated real-workload ladder. The goal is to make sure Concrete builds real things that can be checked against references, not only tiny proof demos. Each workload must name the surface or public claim it forces;

 otherwise it does not belong in this phase. Do not jump straight to multiple
 10k-line ports before the old phase 5 core slab, the stdlib area stdlib, and daily
 workflow can support them; that would mostly test missing ergonomics.
 Each accepted workload and port tier must leave behind a checked gap
 report (`WORKLOAD_REPORT.md` or manifest fields) naming: missing stdlib
 APIs, painful syntax, ownership/copy friction, error-handling friction,
 proof/contract friction, runtime trap/debug friction, capability/authority
 surprises, cold and representative edit/rebuild latency, predicted old phase 6C
 invalidation fanout, docs/tooling gaps, and which later roadmap item should absorb
 each lesson. A workload is not complete if it only builds; it must either
 feed the linear roadmap or prove that no new item is needed.
 Sequence:
 - **Main compiler repo:** keep tiny proof patterns
   (`examples/proof_patterns/` until the corpus-manifest migration assigns their final home),
   evidence-class fixtures, small real programs that gate the compiler, and the three showcase
   flagships here. These protect compiler/proof correctness and should stay close to the tests, but
   only the three portfolio programs are presented as public flagships.
 - **Medium in-repo real programs after the old phase 5 core slab and the stdlib area
   stdlib:** maintain a forcing-surface candidate list—`mini_toml`, `http_headers`, `tar_index`,
   `bytecode_vm`, `lru_cache`, and `checksum_cli`—but do not pre-commit to six new public examples.
   Select at most two active medium workloads whose surfaces are not already exercised deeply by
   `secure_update_verifier` or `protocol_state_machine`, keep them under a workload-owned path such
   as `tests/workloads/medium/`, and wire `scripts/tests/check_medium_workloads.sh` from the corpus
   manifest. `mini_toml` forces
   bytes/text/path diagnostics and parse errors; `http_headers` forces
   ignored-result diagnostics and byte-preserving parsing; `tar_index`
   forces path/OS-string boundaries, archive offsets, overflow/cast
   obligations; `bytecode_vm` forces modules, dispatch, bounded loops;
   `lru_cache` forces collections, ownership, frame facts; `checksum_cli`
   forces project model, `concrete test`, and oracle comparison. A candidate already covered by a
   flagship is absorbed there rather than duplicated. The tier exits only when the selected medium
   workloads build, run, pass
   interpreter-vs-compiled checks, carry full evidence/trust classification,
   and are covered by runtime-obligation audit.
   The selected workloads must follow the stdlib area pipeline recipe
   (`read/acquire -> parse -> validate -> transform -> emit/release`) and
   include a short transcript showing where capabilities enter and where the
   pure core starts. This borrows the teaching value of Elm's one clear app
   shape and Roc's host/application split, while keeping Concrete's
   authority/evidence boundaries explicit.
 - **Separate workload repo later** (`concrete-workloads`): use it for
   larger ports, 2k-10k line
   programs, compatibility suites, external-user programs, benchmark
   workloads, and reference/oracle data that would bloat the compiler repo.
   Do not create this repo until modules/imports, `Concrete.toml`,
   `concrete test`, bytes/text/path, collections, stdlib core APIs, and basic
   diagnostics are usable. The first external-user workload in this repo is the
   external-validation-gate trial from the cross-cutting checkpoint. The repo
   must pin a Concrete compiler/toolchain version in
   `concrete-workloads/Concrete.lock`, include `workloads.toml` with
   `name`, `size_lines`, `source_language`, `oracle`, `evidence_class`, and
   `required_concrete_version`, and be validated by release CI through
   `scripts/tests/check_external_workloads_repo.sh` so it does not silently
   rot as the language changes.
 - **Ported compatibility examples after the daily workflow is stable:**
   `wc`/`cat`/`sha256sum`-style tools, QOI image decoder, base64 library,
   INI parser, tiny glob/regex matcher, arena allocator demo, MMIO-mock
   driver, or protocol codec. Annotate forcing surfaces: glob/regex must
   stay bounded or explicitly outside `PredictableV1`; arena allocator demo
   waits for allocation-profile work that needs it; MMIO-mock driver waits
   for the freestanding area `with(Device)`/MMIO evidence decision or explicitly
   pulls that decision forward.
 - **10k-line stress ports** only after daily workflow is stable enough that
   the port tests Concrete rather than fighting missing basics. First
   accepted port set: `inih`, `cJSON`, `qoi`, `tiny_regex`, and `tiny_tar`.
   Each port must live under `concrete-workloads/ports/<name>/` and include
   `PORT.toml` with `upstream_url`, `upstream_commit`, `license`,
   `ported_subset`, `line_count`, `unsupported_features`, `oracle_command`,
   `known_trust_boundaries`, and `lessons_for_concrete`. Wire
   `scripts/tests/check_10k_ports.sh`. Tier exits only when at least one
   large port has a pinned reference/oracle suite, interpreter-vs-compiled
   differential coverage, runtime-obligation audit, and release-CI replay.
 Each workload must have a check story: oracle/reference comparison,
 interpreter-vs-compiled differential tests, runtime-obligation audit, and
 explicit evidence/trust classification for what is proved, tested, assumed,
 or trusted.

### Task R-0416

**Phase:** D

**Pull trigger:** R-0416: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep early under R-0515; remove QBE dependency.

**Objective:** Establish the first generated-code performance evidence corpus
from the flagships area flagships and workloads, before public comparison pressure turns
informal speed impressions into claims.

**Decision status:** the project affirms predictable generated-code performance
as an evidence-bearing claim; `docs/project/PROJECT.md` (language decisions) records the posture.
Individual comparisons remain `performance_not_claimed` until this task supplies
their workload-scoped evidence.

Select deterministic kernels and end-to-end programs from the workload ladder,
pair each with a pinned C reference and, only where maintenance is affordable,
a Rust/Zig reference implementing the same algorithm, allocation strategy,
input validation, and I/O boundary. Record source/toolchain/profile/target,
warmup and sample policy, input hashes, wall time distribution, peak memory,
allocation count/bytes, output hash, binary size, and emitted-code size. Publish
raw samples and replay commands; never collapse them into a single “fast” badge.
Carry the matching R-0445 resource certificate and its unknown reasons beside
each workload as source-scope evidence. Correlation with measured time or
memory is an experiment, not a reinterpretation of the certificate.

Define the pull trigger for the performance area optimization work: a stable workload
shows a material, reproduced generated-code gap against its comparable
reference; a backend optimization level regresses beyond its noise budget; or a
release-facing performance claim needs evidence. Until then, report
`performance_not_claimed` rather than extrapolating from microbenchmarks.

Acceptance: committed benchmark manifests and inputs, clean-host replay,
calibrated noise/variance handling, debug/release and LLVM/QBE attribution,
result-equivalence checks before timing, a deliberately slowed mutation that
the comparison detects, and no gating threshold derived from a single sample.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0108** (merged): duplicate of R-0416/R-0515: merge profile-labelled budgets and measurements. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0108)

**Explicit reuse comparison (2026-10-25; planned):** only when a real workload
in this corpus exposes allocation/copy costs, compare the allocating operation
with existing explicit mutation before proposing consuming collection APIs.
Retain allocation count/bytes, bytes copied, capacity growth, peak live memory
and runtime alongside result equivalence. If instrumentation is unavailable,
record that gap before drawing a reuse conclusion. Begin with a same-type
operation; arbitrary element-type changes need separate layout and cleanup work.

Include the trusted-code delta, scoped-reference rules, cleanup and partial-failure
behavior in the comparison. A consuming API is justified only by measured benefit
and an explicit owner decision; this amendment mandates no new API, implicit
reference counting, copy-on-write or hidden allocation. Proof-guided optimization
uses the same fixed requirements and independent conformance/differential controls;
a proof of one property does not certify external compatibility, resource bounds
or emitted-binary correctness.

### Task R-0135

**Phase:** C

**Pull trigger:** R-0135: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The refresh obligation survives while legacy proof-link migration and obsolete surface recommendations do not. surviving requirement: Refresh migrated examples and teaching material at phase closures, preserving lifecycle ownership and recording justified omissions. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep candidate checks; use release checkpoints instead of phase linearity.

**Objective:** Do not run broad examples cleanup/polish sweeps. Clean examples opportunistically when a roadmap task touches them. Improve examples only when they serve proof-link migration, `concrete prove` authoring, external validation, or a release-facing tutorial. Add an example-refresh checkpoint at every phase closure, and after every two substantial Phase-6/7 usability increments. The checkpoint is not a broad rewrite; it is a small, gate-backed audit that asks whether the three graduated flagships, minimal onboarding sequence and release-facing docs still teach the current language. It must remove stale "deferred" language for newly landed features; update examples that should

 now use the preferred surface (`if let` / `while let`, range patterns,
 future guards/OR patterns, value-model collection access, `ByteView`,
 explicit capabilities); keep legacy examples only when they intentionally
 demonstrate the old/low-level form; and record any skipped update with a
 reason. At the next release-facing checkpoint, judge rather than assume three
 concrete candidates: whether `crypto_verify`'s `Int` 0/1 API is an intentional
 C-compatible boundary or stale truthiness teaching; whether `grep` should use
 the shipped `String::to_lower`; and whether integer-tag/string-pool
 representations in JSON/TOML/Lox are workload-appropriate runtime models or
 obsolete pre-enum workarounds. Add `scripts/tests/check_example_refresh.sh`
 once the first refresh
 has enough concrete assertions; until then, each phase closure commit must
 name the examples/docs it checked and why no refresh was needed. The goal is
 to prevent tutorial/showcase drift without turning every feature into a
 repo-wide churn pass.

Apply R-0447's lifecycle classification whenever an example is touched:
`tutorial`, `showcase`, `workload`, `evidence`, or `known_hole`. A file whose
purpose is compiler regression or bug reproduction moves to the owning
test/bug corpus instead of remaining user-facing by accident. Do not bulk-move
the examples tree for taxonomy alone; ratchet the unclassified count to zero
through these refresh checkpoints, and fail new examples that omit their
lifecycle/owner record.

### Task R-0137

**Phase:** D

**Pull trigger:** R-0137: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The diagnostics and trial requirements survive while old checker population work and shadow invalidation machinery retire. surviving requirement: The new compiler must provide actionable diagnostic controls and workload validation records, retaining all external-user attempts, the alpha completion bar and measured evidence for later automation decisions. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; split early usability checkpoint from flagship graduation.

**Objective:** Add the flagships area validation artifact: a showcase/workload dashboard that proves every flagship and graduated workload has a check story, evidence bundle, oracle or reference when appropriate, interpreter-vs-compiled coverage, property-test/counterexample-regression coverage where relevant, runtime-obligation audit, trust/assumption classification, and release-CI replay. The first external-user workload in this dashboard is the external-validation-gate trial. Also publish representative cold pipeline timings and old phase 6C shadow invalidation traces for no-op, private-body, public-interface, proof/contract, policy, and target edits; those traces are

 the workload-derived input to the incremental-driver area, not a cache implementation here.
 The external-validation trial recruits three non-authors and records every
 attempted session, including completion, abandonment point, time to first
 useful result, diagnostics encountered, and whether the proof/evidence
 discipline paid for its cost. Alpha still requires at least one non-author
 to complete a useful workflow; the other attempts are evidence even when
 they stop early and may not be omitted from the verdict. Run the trial once
 the minimum teaching path and remedy-oriented diagnostics are usable, before
 the large proof-automation investment, so external evidence can redirect
 that investment rather than merely evaluate it afterward.

 **Amended 2026-07-31: the authoring consumer is a language model, and the
 primary trial is R-0466's, not this one.** Concrete's code is written by
 models, so the ergonomic cost this trial was designed to price is largely not
 charged: a model does not mind `defer x.drop()` or `with(Console)`, because
 keystrokes are the cheapest thing it spends. Explicitness is what it cannot
 substitute for — it cannot fall back on guessing, so any rule not stated in the
 source, and any rejection that does not name its own repair, is a defect rather
 than friction. That reorders this task's own contents: the diagnostics bar below
 stops being an adoption prerequisite and becomes the primary correctness
 surface, because a diagnostic is the model's only feedback channel.

 This does **not** retire the three human sessions or the alpha bar's one
 completed non-author workflow; a diagnostic that defeats a model defeats a
 newcomer too, so R-0466 front-runs the human trial rather than replacing it,
 and it is cheap enough to run per commit where recruiting is not.

 This task owns the minimum diagnostics bar rather than depending on an
 unowned notion of “usable.” For the first-session high-friction cases—an
 unconsumed linear value (E0208), a missing capability declaration (E0240), an
 ignored `Result`, and one proof/evidence failure—the diagnostic must preserve
 the rule explanation and add source-located next actions using facts the
 compiler already knows (for example consume/return/destroy sites or the exact
 `with(Capability)` addition). A suggestion is offered only when its edit is
 semantically valid; otherwise present ranked alternatives and the command
 that explains the obligation. Gate the checked message structure and spans,
 not one brittle prose sentence. This is the bounded adoption prerequisite;
 general IDE quick-fixes remain later tooling work.

 **Measured 2026-07-31, so the bar has a starting number rather than a
 judgement.** Over `tests/invalid_programs/`, 12 of 15 rejected programs emit an
 EMPTY `hint` in `--diagnostics-json`; roughly 25 `hint :=` sites exist against
 215 `E0NNN` codes. The field is already wired end-to-end and reaches the JSON,
 so this is population work, not plumbing — and the facts are in hand at every
 empty site measured: `E0100 undeclared variable 'b'` prints no candidates
 although the resolver holds the in-scope set, and the field/variant cases
 likewise know the legal set they are choosing from. Two findings from the same
 pass belong to this bar: bug 064 (a `--query` rejection that lists the rejected
 kind as known — a message from which no repair is derivable at all, the limiting
 case of a missing next action), and `tests/invalid_programs/missing_variant.con`,
 which exercises the DELETED `#` enum qualifier and gets a generic
 `E0001 expected ;, got #` — the migration case a model trained on older material
 will hit most, with no statement that the separator was retired or that
 `Option::<i32>::Some` replaces it. That fixture has also drifted from its own
 name. Fix the class, not the instance: a gate asserting every code carries a
 non-empty next action, and a corpus grep for fixtures testing retired syntax.

 R-0445's focused non-author resource-report session counts as one recorded
 attempt here. It is an early pressure test, not a substitute for recruiting
 three non-authors or for the alpha requirement that one non-author complete a
 useful workflow.
 If the external-validation trial finds manual proof authoring too costly,
 this artifact must also run the exact narrow Task R-0167 synthesis probe
 selected by the gate, preserve its transcript and review-cost measurements,
 and record the final GO/NO decision consumed by the incremental-driver area trigger. It
 may not silently broaden that probe into the general synthesis roadmap.

 Candidate workloads for the dashboard, each admitted with the claim it
 would falsify on arrival (the envcfg→039 / httpget→045 precedent):
 a capability-firewall proxy — "no path from `main` reaches `Network`
 except through `proxy_send`" (R-0443's first real certificate customer);
 a kv store with a write-ahead log — abort-only failure semantics, where
 `defer` not running on abort forces correctness on every log prefix;
 a naive-vs-bounded regex matcher pair — the `complexity_guarded` gate
 must reject the first and accept the second, the diff being the demo;
 an editor buffer core (gap buffer + owned undo stack) — drop-glue and
 linearity composition under heavy nested non-Copy mutation (the bug 052
 shape); and a job runner / mini-shell — `with(Process)`, argv/env, the
 external-user ergonomics probe. The DNS parser is R-0244's
 pointer-chasing-bound target over R-0444's combinator core, not a separate
 entry here. Workloads are admitted one at a time, when their named claim is
 the thing that needs testing.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0098** (merged): duplicate of R-0137: merge profile-aware workload matrix and owned exclusions. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0098)
- **R-0111** (superseded): superseded by R-0511/R-0137: merge reusable exits; retire required QBE showcase. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0111)

### Task R-0120

**Phase:** G

**Pull trigger:** The new compiler runs secure_update_verifier as a public flagship with an explicit durable anti-rollback model.

**Reconciliation (2026-10-19):** Keep one bounded, precisely scoped theorem first. The theorem states its durable anti-rollback model: which version is committed, when, how it binds the accepted payload, and the outcome after interruption or two concurrent updates — with crash/interleaving controls — or it excludes persistent anti-rollback enforcement from its claim as an offline verifier.

**Objective:** Add a stretch theorem for `secure_update_verifier`: manifest-to-payload binding, version monotonicity / rollback prevention, or trusted-key rotation invariant. This is the flagship theorem that demonstrates fail-closed evidence for a real systems-security workflow.

### Task R-0121

**Phase:** G

**Pull trigger:** The new compiler runs protocol_state_machine and its state and totality contracts can express the transition theorem.

**Reconciliation (2026-10-19):** Keep after state/totality prerequisites.

**Objective:** Add a stretch theorem for `protocol_state_machine`: success-path / failure-completeness or invalid-transition refusal theorem once proof ergonomics support it.

### Task R-0122

**Phase:** research

**Pull trigger:** A public claim implemented with the new compiler requires a cryptographic primitive beyond HMAC.

**Reconciliation (2026-10-19):** Keep explicit forcing-claim gate.

**Objective:** Audit the next stronger real-crypto candidate only if it forces a new public claim:
Ed25519 verification subset, AEAD, or a post-quantum primitive. It extends or replaces HMAC's
portfolio role; it does not automatically create another public flagship.

### Task R-0123

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The body proposes ProofCore extensions, but its disposition retains admitted Core features and proof lemmas. surviving requirement: New-Core proof support must cover candidate-required shifts, bit operations, fixed-width loops, rotations, byte packing and multi-round invariants. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0123: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by R-0495/R-0512: retire new legacy-model semantics; add admitted Core features/proof lemmas instead.

**Objective:** Add only the ProofCore surface that candidate forces: shifts, bitand, u32 compound loops, rotations, byte-to-word packing, and multi-round invariants.

### Task R-0127

**Phase:** G

**Restructured (2026-11-02):** retired with the legacy compiler: The old Core-to-VC machinery retires while the flagship claim and exact loop-binding requirement survive. surviving requirement: A new-compiler security or protocol flagship must expose soundly discharged bounds, division and overflow obligations for its bounded parser or validator. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0127: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep with R-0460 soundness and exact loop binding.

**Objective:** Satisfy the runtime-error-obligation claim through the `secure_update_verifier` or
`protocol_state_machine` flagship: demonstrate that no OOB/div-zero/overflow obligation remains
undischarged on its bounded parser/validator, with the evidence visible in the bundle.

### Task R-0131

**Phase:** G

**Pull trigger:** R-0131: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0119: merge as portfolio acceptance criterion.

**Objective:** Keep the three-program portfolio collectively balanced across parser/protocol,
bounded state, crypto/security, authority, FFI/trust and ownership-heavy behavior. An axis may have
multiple fixtures or workloads, but the public flagship count remains three.

### Task R-0136

**Phase:** research

**Pull trigger:** A new-compiler security workload needs enforced secret-flow restrictions beyond a reported constant-time source shape.

**Reconciliation (2026-10-19):** Keep separate specification; no machine constant-time claim or premature claim surface.

**Objective:** Upgrade the `constant_time_tag` evidence workload from `reported` to `enforced` with a secret-dependent-flow checker. Today it reports a constant-time source shape and leaves machine timing `assumed`; add a source/IR information-flow pass that rejects secret-tagged values reaching branch conditions, loop bounds, or array indices, so the discipline becomes a compiler-enforced structural property, not a reported shape. This needs no hardware/timing model and must not claim machine-level timing: it produces `enforced` for the source-flow property only, with machine timing still named `assumed`. Mark secrets with an explicit annotation (e.g. `#[secret]`); the

 checker reports `enforced` or a counterexample flow path back to source. Add a clean positive
 fixture in the owning test corpus and `tests/adversarial/secret_flow/` negatives for a
 secret-dependent branch, a secret-dependent index, and a secret-dependent
 loop bound. Wire `scripts/tests/check_secret_flow.sh`; the gate must reject
 every negative and must never present source-flow enforcement as timing
 proof.

**Scope controls:** include checked-overflow/trap paths, early exits and calls whose observable
behavior may depend on secrets in the profile's design and fixtures. Unsupported call summaries
must not silently pass a broader leakage claim. Variable-latency operations require the target/model
analysis in R-0527; a structural source-flow pass alone cannot certify their timing. Preserve the
existing distinction between an enforced source restriction and a proved leakage property.

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

### Task R-0138

**Phase:** G

**Pull trigger:** The new compiler begins query reuse after the external-validation GO and a measured workload exposes revision-sensitive inputs.

**Reconciliation (2026-10-19):** Keep; align artifact binding with R-0497.

**Objective:** Freeze every build revision into an immutable `ProjectInputSnapshot` before evaluating queries.

The snapshot owns canonical project/file/module discovery and exact file
bytes; manifest/lock/policy/profile/target; stdlib and dependency roots;
configured cwd/path normalization, symlink and case policy; SDK/sysroot and
runtime/startup inputs; allowlisted environment variables; and compiler,
checker, solver, clang/LLVM, linker, and other tool identities. Watch events
only trigger resnapshot/content hashing. Query evaluation may perform no
ambient filesystem/environment/tool discovery outside declared input queries,
and an edit during a build belongs to the next revision rather than mutating
the active snapshot.

Give source-bearing artifacts two identities where justified: an exact-source
revision digest for bytes/spans/diagnostics/provenance, and a canonical
semantic AST/interface digest for semantics-preserving reuse. Reusing a
semantic result across whitespace/comment/span edits requires a checked
semantic-hash equivalence rule plus a refreshed revision/span map; span-bearing
output always depends on the exact revision. Gate changed environment/std
path, SDK/sysroot/tool binary, symlink/case alias, add/delete/rename, and
edit-during-build snapshots.

### Task R-0139

**Phase:** G

**Pull trigger:** R-0139: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The query correctness contract survives while ProofCore and the prescribed old frontend and SSA stage families retire. surviving requirement: When incremental compilation is pulled, use one typed session and query contract with declared input effects, canonical payloads, orthogonal statuses and an eager oracle. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; replace old Check→Elab/SSA-only stage list with current architecture.

**Objective:** Define one typed query/artifact contract and one `CompilerSession` driver.

Add a closed `QueryKey` family and an explicit typed `CompilerDB` fact/edge
store owned by the session. Split every result into (a) a canonical hashed
payload + semantic dependency manifest and (b) non-hashed operational/view
metadata. Only normalized semantic fields, schema, subject identity, and
declared dependency digests enter `output_digest`/the dependency root.
Timings, hit/miss state, cache path/location, absolute/local paths, rendered
replay commands, redaction/display choices, and other operational metadata do
not. Exact-revision diagnostics/provenance use their own revision-bound
digest.

Keep orthogonal status dimensions: the existing proof/evidence class;
`pipeline_validation_status` (`unvalidated | compiler_validated`);
independent `certificate_status`; and `replay_status`. Cache/integrity/
certificate states may not become new proof-evidence classes. The result
envelope also carries dependency keys/digests, producer validation-record id,
fact/diagnostic ids, provenance roots, cacheability, and replay-plan identity.
Required initial families:

- project input: snapshot, loaded-file/module graph, manifest/dependencies;
- file: source/lex/parse/interface-summary/body;
- module: resolve, desugar, check, elaborate, Core canonicalize, CoreCheck;
- function or explicit SCC: calls, ownership/capabilities/effects, ProofCore,
  obligations, discharge inputs, and typed report facts;
- monomorphized instance: stable definition id + canonical type arguments,
  monomorphization and post-Mono verification;
- codegen unit: lower, raw-SSA verify, SSA cleanup, post-cleanup verify, LLVM
  emission/object from `ValidatedSSAUnit` in V1;
- project: module graph, reachability, policy aggregation, link plan, and
  release/bundle view.

If several production stages remain one composite query, its contract must
name every enclosed stage and verifier so `runFrontend`, desugar,
canonicalization, cleanup, or post-pass verification cannot disappear from
the incremental path. The backend-contracts area schema-bumps codegen inputs from validated SSA
to `ValidatedBackendIR`; the incremental-driver area does not depend on future BackendIR.

Expose `--incremental=off|on|verify` (or equivalent internal modes). Strict,
tolerant, audit, proof, test, report, and codegen operations are demand sets
over the same stage functions, never alternate pipelines. Formatting,
redaction, proof-tool, target, profile, and policy inputs invalidate only
query families that declare them. The eager `off` mode remains the oracle
and debugging path.

Add a query-effect firewall: filesystem reads/discovery, environment
variables, locale/timezone/time, tool/solver discovery and identity, target
data, policy, and external process results enter computation only through
declared input/query APIs. Dynamic dependency recording cannot see ambient
inputs read behind the query engine, so bypassing the firewall is a cache
correctness bug and fails a mutation gate.

### Task R-0140

**Phase:** G

**Pull trigger:** Profiling a new-compiler workload identifies repeated scans or builders that dominate compilation time.

**Reconciliation (2026-10-19):** Merge common indexing with R-0115; retain cache-specific checks.

**Objective:** Add deterministic indexed compiler data and linear-time builders before persisting today's hot whole-program scans.

Introduce measured `ProgramIndex`/`OrderedIndex`-style structures for modules,
symbols, types, functions, layouts, obligations, stable-id lookup, reverse
imports/calls, and monomorphized users. Maps/intern tables may serve lookup;
canonically ordered arrays/lists own iteration, serialization, and output so
hash-table order never leaks. Use dense arrays where stable ids permit them
and builders/reverse accumulation for large strings and collections.
Index-heavy compiler artifacts must carry table/arena identity in the type
or validated artifact schema (`FunctionId`, `TypeId`, `BlockId`,
`ObligationId`, not raw integers). Serialized artifact bytes are hostile
until decoded and validated: cross-table IDs, stale generation IDs where
slots can be reused, and out-of-range IDs must be rejected before they become
semantic facts or cache hits.

This is profiler-driven, not a blanket ban on `List`: replace hot repeated
`find?`/`contains`, `acc ++ [x]`, growing-string append, and repeated global
scans selected by old phase 6C telemetry and bug 027's corpus. Add duplicate-key
and index-finalization verifiers, plus mutations that reintroduce a
quadratic builder and that swap two incompatible typed IDs; both must fail
their gates.

### Task R-0141

**Phase:** G

**Pull trigger:** R-0141: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The reuse invariants survive while repairing the old Unit-returning checker and separate elaborator does not. surviving requirement: Reusable new-compiler frontend artifacts must retain checked facts, prove interface dependency completeness and prevent partial artifacts from satisfying strict consumers. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep reuse contract; checker-return-artifact prerequisite belongs to R-0514.

**Objective:** Make reusable frontend artifact boundaries operational.

Add or validate durable `ParsedFile`, split `InterfaceSummary` and body
summary, `ResolvedModule`, checked module/program artifact, elaborated module,
and validated Core-module boundaries. Check may no longer return only `Unit`
if downstream reuse needs its ownership/capability/value-flow decisions; it
must produce the durable facts or typed artifact that Elab and later queries
consume. Initially keep imported body digests in the conservative dependency
envelope anywhere resolution, trait/method lookup, Check, or Elab can still
inspect bodies. A module may depend on imported **interface-only** digests—and
therefore survive a dependency-private body edit—only after the interface
completeness gate proves every consumer uses the split artifact rather than
a body side channel. Public API identity and implementation-evidence identity
remain separate. Even then, the unchanged interface permits reuse only for
importer frontend/interface queries: referenced generic bodies,
call/effect/proof facts, monomorphized instances, implementation evidence,
and codegen still depend on the applicable body digest. Gate both an unused
nongeneric private-body edit whose importer frontend remains a hit and a
referenced generic-body edit that invalidates the affected mono/object path.

Strict and tolerant/partial artifacts use different result types. A partial,
recovered, cancelled, or internal-error artifact cannot satisfy strict
codegen, proof, policy, package, or release queries. Introduce only boundaries
demonstrated useful by the workload; `CheckedModule` is a candidate name,
not permission to add a redundant typed tree.

### Task R-0142

**Phase:** G

**Pull trigger:** The new compiler's typed query contracts are ready for the first serial reuse implementation.

**Objective:** Implement the serial in-memory query engine and conservative reverse-dependency invalidation.

Record every actual dependency read while evaluating a query and maintain
reverse edges. Initial recomputation units are file, module, function/SCC,
mono instance, codegen unit, and project as listed above; do not create a
query per expression merely because an id exists. Call/import/recursive
cycles are explicit SCC or fixed-point query families, not accidental query
recursion.

V1 uses a **complete conservative dependency envelope**. A result may be
reused only when that whole envelope is unchanged. A digest authenticates
declared inputs; it cannot detect an omitted semantic dependency. Missing a
reuse opportunity is acceptable; stale reuse is not. Narrow an envelope only
after a gate or theorem demonstrates completeness for that query family.

### Task R-0143

**Phase:** G

**Pull trigger:** The first new-compiler artifact family is selected for persistent caching.

**Objective:** Define deterministic internal codecs before persisting an artifact family.

 Each cacheable type needs a compiler-versioned canonical encoder/decoder,
 explicit ordering and normalization, `decode(encode(x))` semantic equality,
 canonical re-encoding, malformed-input/size-limit fuzzing, and a round-trip
 boundary verifier. Summary hashes from old phase 6C are not a serialization
 format. Internal codecs may invalidate wholesale across compiler versions;
 the packages area separately owns stable public package encodings.

### Task R-0144

**Phase:** G

**Pull trigger:** R-0144: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The cache integrity contract survives while the old Core and SSA token reconstruction prescriptions retire. surviving requirement: A new-compiler local cache must validate untrusted decoded artifacts, reconstruct semantic boundaries and preserve producer provenance without inventing proof or certification. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; integrate payload-bound CheckedCore/CheckedMonoCore validation.

**Objective:** Add an opaque compiler-versioned local content-addressed store under `.build/concrete-cache/` (or an equivalent project-local path).

Separate the memo/action table from the content store:
`QueryKey + canonical input/dependency fingerprint -> ResultManifest +
output digest`, then `output digest -> canonical artifact bytes`. Keys include
compiler build/internal schema, semantic input/dependency digests,
invariant-set version, target/data layout, build/test profile, policy only
where consumed, and relevant proof/backend/tool identity. Add
atomic writes, locking, checksums, interrupted-write recovery, size/GC policy,
`concrete cache status --json`, and `concrete clean --cache --dry-run`.

Serialized cache entries are untrusted bytes for decoding/integrity purposes.
Loading must follow an explicit `UntrustedCached* ->
UntrustedDecodedArtifact` API after schema/digest/dependency/subject checks;
those checks yield only `cache_integrity_checked` bytes bound to the
**original** producer validation record; they cannot reconstruct
`ValidatedCore`, `ValidatedSSA`, a proof result, or a release fact. Before
The soundness-bridge and backend-contracts areas, rerun the current
semantic boundary verifier over every loaded strict Core/SSA artifact before
constructing its in-process validated token and fresh producer record. If no verifier can reconstruct
a boundary, that artifact is not safely cacheable for strict codegen, proof,
policy, package, or release use. This still trusts the compiler's verifier—it
is not independent certification.

Treat the project-local store as inside the same user's integrity boundary:
hashes/checksums detect accidental corruption and substitution relative to a
trusted action mapping, not a malicious same-user rewrite of both mapping and
result. Unknown schema/compiler/invariant versions, truncated content,
conflicting ids, or mismatched dependencies are quarantined and treated as
ordinary cache misses whenever source inputs allow recomputation. Fail only
in an explicit artifact-only/offline operation where recomputation is
impossible. A coordinated malicious rewrite of action mapping, result,
manifest, and internal hashes is outside the local-cache threat model until
an authenticated expected action/root exists; tests must not misdescribe
internal consistency checks as detecting it.

### Task R-0145

**Phase:** F

**Pull trigger:** R-0145: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The anti-upgrade and replay controls survive while ProofCore extraction queries retire. surviving requirement: Proof reuse must cache canonical new-Core inputs and dependency-bound artifacts, preserve evidence classes and require kernel replay unless policy explicitly admits an authenticated receipt. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; migrate ProofCore references and preserve R-0155 ownership.

**Objective:** Turn proof, obligation, and report work into queries rather than cached rendered strings.

Cache per-function ProofCore extraction, call/SCC classification,
stable normalized obligations, typed proof inputs, and typed evidence/report
rows whose schemas already exist. Project
reports are deterministic views/joins over those typed facts. A cache hit
preserves the original proof/evidence class and replay provenance; pipeline
validation remains a separate dimension. It never turns `tested`,
`solver_trusted`, `assumed`, or `trusted` into proof. Lean, ProofKit, solver,
synthesis-model, toolchain, spec, theorem, dependency-fact, and policy
identities affect only the query families that consume them.

Do not restore `proved_by_lean`, `proved_by_kernel_decision`, or
`kernel_replayed` from an untrusted cached verdict alone. Cache proof inputs,
normalized obligations, generated artifacts, exact proof-workspace/import
dependency digests, and replay plans; replay the
kernel before emitting the current strong status unless a separately
authenticated replay receipt is explicitly admitted by policy and named in
the TCB. Solver/LLM results retain their weaker class and replay provenance.

The incremental-driver area supplies the query namespace and store; Task R-0155 owns the
shared proof-artifact schema and activation of reusable Lean/solver verdicts,
rather than creating a second `.build/concrete-proof-cache/` database. Cache
only successful strict non-verdict artifacts first. Negative/tolerant
diagnostics, filesystem discovery, environment-dependent operations, solver
results, and cancelled work need explicit cache contracts before reuse.

### Task R-0146

**Phase:** G

**Pull trigger:** R-0146: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The object-cache controls survive while partitioning the old Mono, SSA and emitter pipeline does not. surviving requirement: New-compiler object reuse must bind exact toolchain inputs and deterministic codegen ownership, preserve native-image evidence bindings and test substitution and invalidation. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep LLVM increment; native reuse must retain image/certificate bindings.

**Objective:** Add stable codegen units, an object cache, and deterministic relinking.

First define `ToolchainIdentityV1` / `BackendInvocationV1`: exact clang/linker
executable digests (not version strings alone), SDK/sysroot, allowlisted
environment, target/data layout and ABI, optimization/debug/sanitizer flags,
runtime/startup objects, and every effective tool flag. These inputs come
from `ProjectInputSnapshot` and participate in action keys.

Then add a deterministic `ProgramCodegenPlan` / global ABI-symbol index that
owns reachability, symbol naming/collisions, whole-program signature/layout
facts, builtin/helper/type universe, global/string ownership, externs,
monomorphized-instance ownership, unit imports/exports, and common-versus-test
partitioning. The current whole-program Mono/SSA/emitter decisions must become
explicit plan inputs before a module can emit safely in isolation. V1 unit
input is `ValidatedSSAUnit`; the backend-contracts area migrates it to validated BackendIR.
Start with one unit per source module plus stable runtime/test-harness units
only if the partitionability gate passes. Later subdivision uses deterministic
stable-id buckets or explicit mono SCCs, never traversal-order chunks.

The object action key includes validated-SSA unit + codegen-plan digest,
backend schema, and `BackendInvocationV1`. The link action key includes
canonically ordered object digests plus linker/startup/runtime identities.
Cache/materialize the final linked artifact by link key if a retained-output
warm no-op is expected to skip the linker; otherwise a missing final artifact
may relink while still reusing every object.

Gate isolated unit emission, duplicate/orphan mono ownership, missing/
colliding symbols, common/test partitioning, target/toolchain invalidation,
corrupt local objects/action mappings, and requested-output materialization.
A leaf edit recompiles only its measured affected units plus link; a retained
warm no-op invokes neither clang nor linker. The backend-contracts area later adds independent
BackendIR relation checking. Before a native/object validator, an object hit
remains backend/compiler-trusted and its hash proves identity only relative
to the trusted local action mapping—not that LLVM produced correct code.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0104** (merged): merged — deterministic emission and toolchain identity join R-0146; the LCB version and upstream lock are pinned by R-0517.
- **R-0109** (superseded): superseded by R-0503/R-0146: merge; remove assumption helpers must be C.

### Task R-0147

**Phase:** G

**Pull trigger:** The new compiler's serial query engine passes reuse equivalence and cancellation checks.

**Reconciliation (2026-10-19):** Keep editor prerequisite; parallel scheduling follows serial correctness.

**Objective:** Add a long-lived service/watch surface over the same session, then deterministic independent-query scheduling.

The initial engine is serial. Once serial reuse is correct, allow independent
file/module/codegen-unit queries to execute in parallel with cancellation and
bounded resources. Serial and parallel **Concrete-owned canonical**
artifacts/facts must be byte- and schema-equivalent; external LLVM/object
nondeterminism follows the explicit the backend-contracts area policy and must not change
compiler facts or native behavior. Cancellation or a worker crash must not
commit partial cache state. Canonically sort diagnostics/events whose order
is otherwise scheduling-dependent. The internal service exists to preserve a
session across CLI or scripted edits; the editor area's LSP must wrap this exact
session rather than a batch `runFrontend` or editor-only cache.

### Task R-0148

**Phase:** G

**Pull trigger:** The new compiler's first reusable query family is ready for shadow deployment.

**Objective:** Add cache/query observability and a conservative rollout.

Extend pipeline events with `executed | memory_hit | disk_hit | invalidated |
uncacheable`, reason, dependency ids, bytes, time, validation-record id, and
the separate evidence/pipeline-validation/certificate/replay statuses. Roll
out `shadow -> opt-in -> CI verify/dual-run -> default-on`
only after each stage's corpus is green. `verify` recomputes selected hits and
compares canonical results; any false hit disables that query family and
persists a minimized regression. Audit/why/diff output may explain reuse but
may not cite reuse as correctness evidence.

### Task R-0149

**Phase:** G

**Pull trigger:** R-0149: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The gate-integrity duties survive while legacy Core, SSA and LLVM byte-comparison artifacts are replaced. surviving requirement: The new incremental driver must pass an edit, corruption, substitution and ambient-input matrix against eager execution, with bounded recomputation and honest cache threat-model claims. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; move validation program to fixture/workload home.

**Objective:** Add the incremental-driver area validation artifact.

 Create `examples/incremental_driver/`,
 `tests/perf/compiler_pipeline/incremental_driver/edits.json`,
 `scripts/tests/check_incremental_driver.sh`,
 `scripts/tests/check_codegen_object_cache.sh`,
 `scripts/tests/check_incremental_failure_recovery.sh`, and the umbrella
 `scripts/tests/check_phase8_5_incremental.sh`.

 Cover no-op; whitespace/comment; private leaf body; public
 signature/type/capability/contract; dependency private body versus
 interface; generic body/new instantiation; proof link/theorem/spec; policy;
 target/profile; file add/delete/rename; syntax/type error then repair;
 cancellation; cache truncation/corruption; schema/compiler/checker/tool
 drift; coordinated action-map/artifact/dependency rewrite (recorded as
 outside the unauthenticated local-cache threat model, never falsely
 "detected"); object
 substitution; ambient-input firewall bypass; and serial/parallel modes. Compare
 incremental-off/on/verify facts, diagnostics, obligations, reports,
 Concrete-owned canonical Core/SSA/LLVM and manifests byte-for-byte; compare
 normalized external-object metadata and native behavior unless the pinned
 toolchain is separately proved reproducible. Assert query execution sets,
 named unaffected sentinel modules/functions/objects that must remain hits,
 affected outputs that must change, and maximum query-family execution counts
 per edit—not only wall time or the tautology "declared dependencies rebuilt."
 On the flagships area large workload, a warm no-op must execute zero **producer
 recomputation** beyond required decode/integrity/boundary-validation queries,
 and a leaf edit must rebuild only declared reverse
 dependencies within the stated count bound. Publish observed/baseline-derived
 no-op and leaf-edit speedups (20x/5x are investigation targets, not phase
 gates or public claims); the release-bar area owns platform-specific release performance
 budgets.

## Area: Proof Authoring And Automation

Goal: make flagship proofs a repeatable engineering workflow, not a collection
of one-off `simp` scripts.

Done when: new flagship proofs can start from useful generated stubs, standard
lemmas, and actionable failure diagnostics.

This phase owns the proof-cost probe from the roadmap spine: before assuming the
proof discipline scales, measure manual proof effort on real flagships, then
measure review effort for the LLM-guided synthesis loop if manual authoring is
too expensive.

### Task R-0150

**Phase:** E

**Pull trigger:** R-0150: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; start measurements before automation.

**Objective:** Instrument the flagships with PROOF-EFFORT AND COVERAGE TELEMETRY
before investing in automation, so the external-validation gate's “was the
proof discipline worth the cost?” question has data instead of anecdotes.

**First shared consumer (2026-10-04): R-0486.** Begin measurements with its first
bounded component, before proof scaffolding or automation is expanded. Retain source
annotation size, manually authored proof/bridge work, time to actionable feedback,
repair time after each controlled change, and compiler-author interventions. Record
the revision, toolchain and measurement conditions; establish targets from the observed
baseline rather than inventing speedup claims. The existing HMAC and coverage series
below remain useful comparisons, not prerequisites to recording this workload.

Measure both sides of the investment:

- **cost:** per proved function, Lean proof lines, tactic depth,
  solver/`bv_decide` time, replay time, and the source complexity covered
  (loops, branches, width casts); and
- **coverage:** the fraction of the existing eligibility report's pure-Core
  denominator closed without a hand-written Lean bridge, classified by
  obligation class, discharge route (`structural`, `omega`, `bv_decide`,
  linked Lean, or unsupported), and stable blocker reason, including the share
  blocked by absent operational or fixed-width semantics.

Never invent a telemetry-only definition of “eligible”; use the compiler's
canonical eligibility facts so the denominator cannot be improved by excluding
hard cases. Seed the coverage side from R-0004's honest post-migration baseline.
Publish a small table for the HMAC flagship and the `constant_time_tag` evidence workload, refresh
it per relevant portfolio release, and retain comparable history rather than replacing the previous
number. R-0438's generated claim records eventually own the durable trend; this
task owns the measurement definition and initial series. The result may justify
changing the later R-0169/R-0170 investment, but it must not move automation
ahead of proof-subject identity, exact semantic coverage, or the R-0137
external-user trial. Cheap to collect now; impossible to reconstruct later.

**Feedback-cost attribution (2026-10-25; planned):** extend existing pipeline
telemetry on this task's shared workload rather than creating another timing
system. Distinguish obligation generation, Lean elaboration/checking,
retained-evidence replay, emission and native compilation from the existing
front-end stages. Identify overlapping or nested measurements so they are not
added twice. Mark unavailable phases explicitly rather than reporting zero.
Record cold/warm cache conditions, exact revision/toolchain, workload and evidence
scope with the observations. Re-running proof search is not retained-evidence
replay. These measurements guide investigation of small explicit obligations and
reusable lemmas; they impose no invented speed target or compiler-rewrite decision.

### Task R-0151

**Phase:** E

**Pull trigger:** R-0151: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The ownership rule survives while moving legacy proof modules and retaining registered PExprs does not. surviving requirement: New-compiler example proofs, fixtures and replay metadata must have one owner beside their source, with reusable lemmas in ProofKit and generated workspaces following that layout. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; remove dependence on a future PExpr layering split.

**Objective:** Move example Lean proofs physically next to their Concrete examples. Target layout:

`examples/<name>/src/main.con`, `examples/<name>/proofs/Proofs.lean`,
`examples/<name>/snapshot/...`. Configure Lake/module discovery so these
files are importable by the proof checker (for example as
`Examples.<Name>.Proofs` or an equivalent stable namespace), update
`Concrete.lean`/proof umbrellas so `check-proofs` can see them, retarget
source `#[proof_by]` / `#[ensures_proof]` links, and update
`concrete prove --emit-lean` / `--workspace` to prefer the
`examples/<name>/proofs/` destination. Update the proof-namespace guard to
reject new example proof modules under `Concrete/Examples/`. Pilot on one
small example, then migrate all current example proof modules. Keep registered
spec PExprs in `Concrete.Proof` until the later `ProofCore` /
`SpecRegistry` split. This executes R-0447's proof-ownership rule: after each
example migrates, delete its old top-level `proofs/Examples` copy and make the
layout manifest reject a second owner. Reusable lemmas stay in ProofKit;
example-specific theorem code, fixtures, snapshots, and replay metadata stay
with the example.

### Task R-0153

**Phase:** E

**Pull trigger:** R-0153: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; names remain display conventions, not identity.

**Objective:** Define and document stable theorem naming conventions in tool output:

 `<fn>_refines_spec`, `<fn>_<obligation>_proved`,
 `<fn>_loop_<name>_preserves`, and
 `<fn>_call_<callee>_discharges_requires`. `concrete prove` should suggest
 these names instead of leaving agents to invent them.

### Task R-0154

**Phase:** E

**Pull trigger:** R-0154: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep, incrementally with each authoring surface.

**Objective:** Add CI gates for the agent-facing proof surfaces: snapshot representative `--json` output, validate schema versioning, ensure generated Lean stubs parse/check up to the intended placeholder boundary, assert replay JSON reports the same statuses as human replay, and assert proof-check JSON maps a failing Lean proof back to the intended obligation id.

### Task R-0155

**Phase:** F

**Pull trigger:** R-0155: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; derive from canonical task/evidence objects.

**Objective:** Define one proof artifact schema shared by obligations, minimization, `--why`, generated stubs, synthesis attempts, repair plans, stale-proof reports, proof-cache entries, and replay bundles.


 Deliverable: `docs/PROOF_ARTIFACT_SCHEMA.md` plus a schema-versioned JSON
 fixture directory. Required common fields: `schema_version`,
 `obligation_id`, source fingerprint, source span, policy id, evidence
 class, replay command, tool versions, assumptions/trust deltas, and final
 status.

 Done when `--emit-lean`, `--minimize`, `--why`, `--synthesize`,
 `--repair-plan`, proof-cache status, and stale-proof reports all emit this
 shared envelope, and a gate fails if any command invents a private JSON
 dialect.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0241** (merged): duplicate of R-0155/R-0178: merge obligation payload into those canonical schemas.

### Task R-0156

**Phase:** E

**Pull trigger:** R-0156: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add human docs only after the binary path exists:

 `docs/AGENT_PROOF_AUTHORING.md` and an optional repo-root `AGENTS.md`
 should summarize the binary workflow and point to the ProofKit guide, but
 they must not be the source of truth for agents using only an installed
 binary.

### Task R-0157

**Phase:** E

**Pull trigger:** R-0157: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep as continuing acceptance requirement.

**Objective:** Keep AI/agent assurance guidance aligned with the implemented proof surface.

 `docs/verification/SPARK_CLASS_ASSURANCE.md` is the current design-target guide for
 agents: it tells Claude/Codex-style tools which assurance annotations are
 implemented today, which are future-only, and which replay commands must
 validate a claim. When loop invariants, frame/dependency contracts,
 ghost/spec code, or package evidence land, update this guide in the same
 commit as the feature and add at least one agent-facing example of the
 intended suggestion pattern.

### Task R-0160

**Phase:** E

**Pull trigger:** R-0160: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The loop-proof requirement survives while dependence on the old while_step representation does not. surviving requirement: Provide reusable loop-state lemmas over the new Core's clock-indexed semantics. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; target Core loops and clock semantics.

**Objective:** Build reusable lemmas for loop-carried state and `while_step`.

### Task R-0161

**Phase:** E

**Pull trigger:** R-0161: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep, driven by admitted widths/operators.

**Objective:** Build reusable lemmas for BitVec operations used by flagships.

### Task R-0163

**Phase:** E

**Pull trigger:** R-0163: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The useful stub shapes survive while PExpr bodies and FnTable skeletons retire. surviving requirement: Generate source-bound new-Core proof stubs for realistic data, loops, contracts and composition through one authoring path and schema. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; replace PExpr/FnTable output with exact Core statements.

**Objective:** Upgrade generated proof stubs for real shapes: arrays, structs, enums, fixed buffers, Result/Option, loops, source contracts, and refinement composition. Stubs should emit spec target, `PExpr` body, FnTable skeleton, expected theorem statement, common imports/tactics, and TODO blocks for loop invariants. These items enrich what `--emit-lean` produces; they do not introduce a second stub generator. Provide a friendly alias such as `concrete prove <file> <fn> --stub` only if it is the same artifact path and schema as `--emit-lean`, not a second proof surface.

### Task R-0164

**Phase:** E

**Pull trigger:** R-0164: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: Composition support survives while manually assembled FnTable scaffolding retires. surviving requirement: Generate call-composition lemmas, exact callee dependencies and composed theorem scaffolds for new-Core functional models. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; replace manually assembled FnTables.

**Objective:** Add generated composition scaffolds: FnTable entries, call lemmas, callee refinement dependencies, and composed theorem skeletons.

### Task R-0166

**Phase:** E

**Pull trigger:** R-0166: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: Actionable diagnostics survive while old extraction and table-entry failure machinery retires. surviving requirement: New-compiler proof failures must explain exact obligations and dependencies, distinguish timeout from refutation and preserve runnable witnesses with replay provenance. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; share persistence with R-0191.

**Objective:** Improve failed-proof diagnostics after `--json`, failed artifacts, and `--minimize` exist: classify common failures into actionable categories such as missing callee theorem, stale source link, missing table entry, failed arithmetic bridge, insufficient frame fact, and spec/extraction mismatch. Add `concrete prove <file> <fn> --why <obligation_id>` (or an equivalent `--show-obligation --why` form) to explain why the obligation exists, which source span generated it, which facts are in scope, what evidence classes are allowed, and why automation did not close it.

 Diagnostics should point to the already-generated artifact or next action
 instead of introducing another parallel proof surface. Where a failed proof,
 failed contract, or solver counterexample has concrete inputs or a finite
 witness, emit a runnable `.con` counterexample fixture plus a replay command.
 This is the Dafny/SPARK/F* lesson in Concrete's terms: proof failure should
 feel like debugging a minimized program, not reading a wall of theorem-state
 text. The fixture is evidence of failure only; it must never upgrade a claim
 to proof.

**Bounded proof-cost diagnostics:** when R-0150's measurements on R-0486 expose expensive or
unstable automation, isolate existing obligations without adding proof-language syntax. Report
failure to establish a goal, timeout, resource exhaustion and unsupported fragments distinctly,
using the existing artifact schema and R-0494 verdict authority rather than new verdict spellings.
A timeout or failed proof search is neither proof nor a refutation. Retain per-obligation time
and, where the solver supplies it, resource counts with the solver version/configuration; do not
claim cross-solver or cross-version determinism. Isolated runs retain the same checked hypotheses
and dependency closure. Reference: [Dafny verification optimization](https://dafny.org/latest/VerificationOptimization/VerificationOptimization).

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0206** (merged): duplicate of R-0166: merge into R-0166.
- **R-0253** (merged): duplicate of R-0166: merge into R-0166.

### Task R-0171

**Phase:** F

**Pull trigger:** R-0171: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The universal firewall and attack controls survive the old discharge implementation. surviving requirement: Every new proof-automation route must preserve trust classifications and pass route-specific attempts to upgrade weaker evidence into proof. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0494: merge into R-0494; preserve route-specific attacks.

**Objective:** Add an automation trust-upgrade firewall for every proof automation path.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0171).

### Task R-0177

**Phase:** E

**Pull trigger:** R-0177: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep as cumulative checkpoints, not one late capstone.

**Objective:** Add the proof-authoring area validation artifact: a proof-authoring project that exercises `--json`, `--show-obligation`, `--emit-lean`, `--emit-artifacts`, `--workspace`, `--check`, `--nearest-lemmas`, `--minimize`, and source-linked proof attachment across straight-line, array update, loop copy, fold, composition, bounded quantified specs, ghost, stale, missing, partial, and repair cases. The gate must typecheck generated stubs, reject any `proof-registry.json`, and verify that failing Lean proofs map back to stable obligation ids. If the incremental-driver area completed, it must also exercise proof-query memory/disk hits, stale dependency invalidation, proof-tool/policy drift,

corrupt-entry recovery, and cache-off/on/verify equivalence without treating
a hit as proof evidence. If the incremental-driver area was rejected/deferred, the validation
artifact runs eagerly and does not create a private proof cache.

### Task R-0152

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The ProofCore artifact format retires while reproducer minimization and its regression controls survive. surviving requirement: Proof minimization must emit a replayable minimal source, new-Core claim and Lean context preserving the failed obligation's identity and status across the specified failure fixtures. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0152: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; use checked Core and canonical proposition artifacts.

**Objective:** Add proof minimization: `concrete prove --minimize <obligation_id>` emits the smallest source / ProofCore / Lean slice needed to reproduce a failed obligation. Output directory:

 `.build/prove/<function>/<obligation_id>/minimized/` with `source.con`,
 `proofcore.lean`, `replay.lean`, `context.json`, and `README.md`. Wire
 `scripts/tests/check_prove_minimize.sh` with one loop VC, one failed
 postcondition, one stale proof, and one SMT counterexample. The minimized
 artifact must reproduce the same status and stable id without unrelated
 functions.

### Task R-0158

**Phase:** after-1.0

**Pull trigger:** An agent integration requests MCP access after the new compiler's CLI, JSON and proof workspace interfaces stabilize.

**Reconciliation (2026-10-19):** Keep; no prerequisite for compiler proofs.

**Objective:** Add MCP only after the CLI/JSON/stub/workspace surfaces are stable. The MCP server should wrap the binary rather than duplicate logic, exposing resources such as `concrete://prove/<fn>/obligations`, `concrete://proofkit/lemmas`, and `concrete://examples/evidence-classes`, plus tools for `prove_json`, `show_obligation`, `emit_lean`, `check`, `replay`, and `check_proofs`.

### Task R-0159

**Phase:** C

**Pull trigger:** R-0159: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; replace “OOB stuck” with defined bounds-trap semantics. Out-of-bounds access is a defined bounds trap in Core, not stuck execution; lemmas target the trap semantics.

**Objective:** Build reusable proof lemmas for arrays: lookup, update, length, in-bounds, OOB stuck behavior.

### Task R-0162

**Phase:** C

**Pull trigger:** R-0162: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Build reusable lemmas for structs, fields, enum construction, match, Result, Option, and bounded-buffer invariants.

### Task R-0165

**Phase:** E

**Pull trigger:** A second new-Core loop proof repeats a counter, copy, fold, multi-store, offset or block invariant shape.

**Objective:** Add generated loop-invariant templates for common proof shapes:

 counter loop over array writes, copy loop, fold loop, multi-store loop,
 offset loop, and block-processing loop.

### Task R-0167

**Phase:** after-1.0

**Pull trigger:** Users request invariant or lemma synthesis after new-Core obligation artifacts and offline replay can validate generated candidates.

**Reconciliation (2026-10-19):** Keep, conditional on R-0150 economics and exact Core claims.

**Objective:** Add **LLM-guided proof synthesis, kernel-verified** as a first-class proof-authoring workflow, not as prose-only AI help. Command shape:

 `concrete prove --synthesize <module.fn>` (or an equivalent subcommand)
 runs a closed loop: Concrete emits exact obligations; an LLM proposes loop
 invariants, helper lemmas, or Lean proof scripts; Lean's kernel /
 kernel-backed decision procedures check the artifact; failed attempts are
 minimized and fed back into the next attempt; and only replayed
 kernel-checked artifacts may upgrade evidence. LLM output is never
 evidence by itself. Reports must record model/tool identity, attempt count,
 generated assumptions, rejected assumptions, introduced trusted boundaries,
 final evidence class, replay command, and whether the final artifact works
 without the LLM. Design target:
 `docs/verification/PROOF_SYNTHESIS.md`.

 Product model: the human reviews the spec, assumptions, and evidence class;
 the kernel checks the generated proof artifact. Do not design this as
 "trust the generated proof" or "the LLM decides correctness." If the spec
 is weak, vacuous, or wrong, synthesis has only made the wrong theorem cheap;
 the audit-commands and proof-status areas spec-adequacy, vacuity, and mutation gates are therefore part of
 this feature's safety story.

 Deliverable: a replay bundle, not an LLM transcript. The bundle must contain
 the obligation, generated candidate artifacts, accepted proof/invariant/
 lemma artifacts, rejected attempts with reasons, policy decisions, and a
 `concrete replay` command that validates the final evidence without network
 access or model access.

 Gates: synthesize a loop invariant for a small loop proof; synthesize a
 missing helper lemma; repair a stale proof after source drift; reject a
 plausible but false proof; reject any attempt that introduces a new
 assumption, trusted fact, extern/Unsafe dependency, or solver-trusted fact
 without policy approval; and produce a replay bundle that verifies without
 the LLM or network. Add red-team fixtures where the agent proposes a proof
 of the wrong theorem, a vacuous spec, an unapproved assumption, and a proof
 script that typechecks only after weakening the claim.

 Agent discoverability is part of the feature, not documentation garnish.
 The installed binary must expose a machine-readable feature/command catalog
 (for example `concrete agent features --json` or `concrete help --json`,
 extending the old phase 6E command catalog rather than inventing a second one)
 that tells LLM tools which proof commands exist, what artifacts they emit,
 what evidence classes mean, which replay command validates a result, and
 which actions are forbidden without policy approval. The same facts should
 be reachable through any later MCP server, but the CLI/JSON catalog is the
 source of truth so agents can discover the workflow without reading the
 repository docs.

### Task R-0168

**Phase:** after-1.0

**Pull trigger:** Measured new-compiler proof replay time becomes a bottleneck after stable fingerprints and a shared incremental store exist.

**Reconciliation (2026-10-19):** Keep performance-pulled, as R:220 requires.

**Objective:** Add proof-result caching once proof artifacts and fingerprints are stable.

 This item is active only when the incremental-driver area completed after a GO verdict; if
 the incremental-driver area was rejected/deferred, proof replay remains uncached rather than
 growing a private workaround. Implement caching as the incremental-driver area query families in the one
 `.build/concrete-cache/` content-addressed store, not as a separate proof
 database. Cache key: compiler/query schema, toolchain version, source and
 typed-Core fingerprints, spec/proof link, obligation id, dependency-fact
 fingerprints, ProofKit version, proof/solver/synthesis engine version, and
 policy mode where it affects the verdict. Stale or dependency-mismatched
 entries become `needs_recheck`, never green. An LLM-synthesized proof is not
 cacheable as evidence until Lean replays it, and a hit preserves the
 recorded evidence class rather than upgrading it.
 Expose
 `concrete prove --cache-status --json`. Wire
 `scripts/tests/check_proof_cache.sh`; the gate must prove cache hits do not
 mask stale source, stale theorem names, changed policies, or changed solver
 trust settings, and that `--incremental=off|on|verify` produces identical
 proof statuses and replay artifacts.

### Task R-0169

**Phase:** E

**Pull trigger:** A new-compiler external trial measures structural obligations as a dominant proof-authoring cost.

**Reconciliation (2026-10-19):** Keep; choose coverage from measured workloads.

**Objective:** Add simple auto-discharge for structural obligations that do not need human proof search. V1 shapes: reflexive field projection, tuple/struct constructor-destructor round trips, enum tag preservation, fixed-array literal length, direct call wrapper, and source-contract metadata erasure.

 Command surface: `concrete prove --auto <function> --json`, reporting
 `auto_closed`, `needs_lean`, or `not_supported` per obligation. Wire
 `scripts/tests/check_structural_auto_discharge.sh`; auto-discharge may only
 emit `proved_by_kernel_decision` or a linked Lean theorem when the kernel
 actually checks the generated proof. This task follows the R-0137 external
 trial by design: use R-0150's measured structural shapes and blocker
 distribution to choose V1 coverage instead of treating “the boring 95%” as an
 assumption. It consumes R-0442 callable identity and R-0004 replay receipts;
 it does not replace kernel replay of existing hand-written artifacts.

### Task R-0170

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The bespoke ProofCore symbolic executor retires while arithmetic normalization, guard splitting and soundness controls survive. surviving requirement: Operational proof automation must use exact new-Core functional models and Lean verification tools, preserve fixed-width trap semantics and report unsupported leaves without upgrading evidence. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0170: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; retarget symbolic execution to normative Core.

**Objective:** Add operational VC auto-discharge as the next automation tier after structural auto-discharge. Today `linear` and `bitvector` obligations route to `omega` / `bv_decide`, while `operational` and `refinement` obligations route to Lean proof links; that leaves ordinary `#[ensures]` bodies and loop operational-preservation steps dependent on hand-written bridge theorems.


 The completed forcing probe and 4/6 result are recorded in
 [CHANGELOG.md](../../CHANGELOG.md). The active V1 gaps are generated
 `Int`/`Nat`/`BitVec` shift-amount normalization before `bv_decide` and
 automatic guard splitting; keep one positive fixture and one expected
 boundary failure until the implementation lands.

 V1 core: symbolically execute a narrow Core/ProofCore fragment, unfold the
 generated eval and named spec, split conjunctions and guards mechanically,
 normalize `Int`/`Nat`/`BitVec` casts, route arithmetic leaves to `omega`,
 route bitvector leaves to `bv_decide`, and report surviving leaves as
 `needs_lean` / `not_supported` rather than false green. Defer whole SHA
 compression rounds, message schedules, heap/alias-heavy code, effectful code,
 and induction-heavy specs until separate evidence shows they fit. Design
 note: `research/proof-evidence/operational-vc-auto-discharge.md`.

 Loop-bound and `complexity_guarded` arithmetic obligations (R-0244) are a
 named discharge class for this tier: a symbolic iteration bound that
 survives to `needs_lean` is a signal about the obligation, not homework
 for the user.

 Soundness boundary: automation may close only an operation whose ProofCore
 semantics matches the typed runtime operation at that width. Checked
 arithmetic needs a discharged no-overflow/no-trap obligation; explicit
 modular arithmetic needs the corresponding fixed-width operator; an
 unbounded-`Int` theorem may not silently stand for fixed-width trapping
 execution. Missing div/mod, shift-cast, saturating, width, callable-target, or
 semantic-scope support yields `needs_lean`/`not_supported`, never a broader
 green claim. Record the exact extraction and operation-semantics versions in
 the replay artifact so R-0222 can later broaden arithmetic-site coverage
 without retroactively upgrading these results.

 The first reuse-economics customer is R-0444's parser core: close the seven
 combinators plus fixed-width assembly fold once, with ordinary parsers
 inheriting their obligations through composition instead of requiring a
 bridge theorem per parser. This is a gate on the automation tier, not a reason
 to hand-author those proofs before the tier exists. Preserve the honesty
 boundary that only parsers built from the checked combinators inherit the
 result.

### Task R-0172

**Phase:** E

**Pull trigger:** R-0172: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; distinguish library models from compiler semantics.

**Objective:** Add a small verified/spec-checked standard proof library for common predicates and formal stdlib models: sorted, bounded, no-duplicates, fixed-length, prefix, checksum, constant-time source shape, `formal_vec`, `formal_map`, `formal_set`, spec-only `bigint`, and the first reusable lemma families over those models. Treat these as **formal shadow models** for real containers: user proofs reason about the mathematical model, while collection implementations prove refinement facts from `Vec`, `HashMap`, `OrderedMap`, and `HashSet` to the corresponding formal model.


 Deliverable: one end-to-end refinement slice, for example
 `HashMap<K,V>` operation facts lowering to `formal_map`, with replayable
 Lean artifacts and a user proof that consumes only the formal model facts.

 Done when the user proof does not mention buckets/tombstones/capacity, the
 collection implementation proves the bridge facts, the report names the
 refinement evidence class, and a deliberately false refinement is rejected by
 the checker/kernel. This is the tractability unlock for proving real
 programs that use containers.

### Task R-0173

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The ProofCore and TermIR lowering retires while bounded quantifier semantics and anti-erasure gates survive. surviving requirement: Bounded collection specifications must retain explicit finite domains, source identities and checked theorem meanings, with rejection of unsupported or unbounded claims. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0173: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; lower through typed contracts/TermIR.

**Objective:** Add bounded quantified specs for collections, not arbitrary open-ended logic. V1 syntax should cover only finite, source-visible domains:

 `forall i in 0..n { P(i) }`, `exists i in 0..n { P(i) }`, and library
 predicates that lower to those bounded forms (`all_bytes_valid`, `bounded`,
 `sorted`, `no_duplicates`, `prefix`, `fixed_length`). These specs must
 lower into ProofCore with explicit bounds, source spans, and generated
 theorem shapes; they may be proved by Lean, by kernel decision procedures
 only for decidable finite fragments, or reported `needs_lean` / `blocked`
 when the fragment is outside automation. Do not add general quantifier
 syntax until a bounded collection fragment has examples and gates. Add
 `examples/quantified_specs/` with `bounded_array`, `all_bytes_valid`,
 `sorted_prefix`, and one rejected unbounded quantifier. Wire
 `scripts/tests/check_quantified_specs.sh`; the gate must prove quantified
 claims are never erased into a vague postcondition and never reported as
 proved unless the generated finite theorem actually checks.

### Task R-0174

**Phase:** after-1.0

**Pull trigger:** Stale new-Core proofs become a recurring authoring cost after replay and diagnostic artifacts stabilize.

**Reconciliation (2026-10-19):** Keep; share R-0166 diagnostics and R-0167 artifacts.

**Objective:** Add AI-assisted proof repair only after artifacts, statuses, and replay are stable enough to validate suggestions mechanically. The binary surface is `concrete prove --repair-plan <obligation_id> --json`; it emits no edited source, only candidate next actions with required checks. Required JSON fields: `obligation_id`, `failure_class`, `minimal_artifact`, `suggested_lemma`, `suggested_imports`, `candidate_tactic`, `validation_command`, and `risk`. Wire `scripts/tests/check_proof_repair_plan.sh` with missing theorem, stale proof, missing frame fact, arithmetic bridge failure, and spec mismatch.

 No repair suggestion may change a proof status until `--check` or
 `check-proofs` verifies it. A convenience spelling such as
 `concrete prove <file> <fn> --repair` may exist only as an alias for the
 same repair-plan artifact; it must not edit source by default and must never
 upgrade evidence without replay.

### Task R-0175

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The proposed ProofCore inference pass retires while the workload-gated frame requirement survives. surviving requirement: Frame reasoning over new-Core functional models must provide checked preservation facts when a second update pattern demonstrates that implicit framing no longer suffices. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0175: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; foundational memory/frame lemmas still start in Phase 2.

**Objective:** **Frame inference (the proof-scaling cliff).** Every loop/state proof must establish not just what an iteration *changes* but what it *preserves* — the frame problem (Smallfoot 2006; later Infer; separation logic's frame rule:

"a proof mentioning only its footprint preserves everything else"). Today
this is handled *cheaply* and *implicitly*: mutation is functional
(`List.set` / `Env.bind`), loop invariants are total index-predicates
(`fun j => if j < m then word j else 0`), and the frame is discharged ONCE as
the generic `set_in_counter_map` lemma and applied O(1)/iteration via
`eval_while_count` — so `block_to_words` / `schedule` / `compress` pay no
per-cell frame cost. This holds only while updates stay single-cell and
arrays stay non-aliasing. It will NOT scale to scattered/multi-cell updates
per iteration, multiple aliasing mutable arrays, invariants that are not
index-predicates, or a future flat mutable-heap / pointer model. Before any
of those land, design a frame-like annotation or inference pass into
ProofCore (separation-logic-style footprints, or a `#[frame]`/`modifies`
clause that auto-derives preservation) so frame conditions never become the
majority of proof work. Gate: do not build it until a second update shape
actually forces it (per the operating rules) — the current functional-list
model gets framing for free.

   Reference points beyond Smallfoot/Infer (2026-07-25): CN (Cambridge) —
   separation-logic refinement types for C, used to verify pKVM's buddy
   allocator; its resource inference for iterated separating conjunction,
   with a syntactic restriction on ghost variables, gives predictable,
   mostly-automatic ownership reasoning (eager unfold, lazy fold) — the
   closest match for the inference mechanism. Verus — SMT-first
   verification for a Rust-like linear language; the closest match for
   the discharge posture (the boring majority closes without manual
   proof). Aeneas — translates Rust to Lean 4 (also F*/Coq/HOL4) via pure
   functional models, used on real crypto code; the closest existing
   pipeline for source→Lean extraction faithfulness. CN's own admitted
   gaps (Owned↔byte-representation conversion, concurrency) sit exactly
   where Concrete's ByteView work does — import the inference ideas, not
   the byte model.

### Task R-0176

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The proposed PExpr, PVal and ProofCore layering retires, but the disposition explicitly retains specification ownership goals. surviving requirement: New-Core proof authoring must keep example specifications independently owned and register their identities without cyclic semantic dependencies. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0176: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by R-0495/R-0512: retire proposed legacy architecture; retain ownership goals in R-0151.

**Objective:** Deferred architecture refactor: split the current `Concrete.Proof` layering so registered example specs can move without a cycle, but do not let this block the active frontier unless spec ownership or proof authoring starts depending on it. Target shape: `Concrete.ProofCore` owns `PExpr`, `PVal`, evaluation, `FnTable`, and source-independent semantics; `Concrete.SpecRegistry` owns the spec-drift table and imports whichever example spec modules it registers;

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0176).

## Area: Audit Commands And Review Artifacts

Goal: let a reviewer answer "what can this program do, what is proved, what is
assumed, and what changed?" without reading compiler internals.

Done when: `concrete audit`, semantic diff, and an artifact viewer cover the
three graduated flagships and one package-scale workload.

### Task R-0180

**Phase:** F

**Pull trigger:** R-0180: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add `concrete explain <function>`: capabilities, proof status,
assumptions, obligations, trusted callees, direct and transitively reachable
runtime-trap kinds, evidence dimensions, and why each status is what it is.
Trap explanations reuse R-0178’s site/edge facts and show the path or
completeness boundary rather than deriving reachability again.

### Task R-0181

**Phase:** D

**Pull trigger:** R-0181: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; use shared conservative call facts.

**Objective:** Add `concrete why <capability>`: explain why a function needs `File`, `Network`, `Alloc`, `Unsafe`, etc., including transitive call chains.

### Task R-0184

**Phase:** D

**Pull trigger:** R-0184: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; separate policy acceptance from compiler verification.

**Objective:** Add early **capability budget files** before full package facts exist.

 Deliverable: a project-local policy file (for example
 `concrete.policy.toml`) with a minimal V1 surface:
 `forbidden_caps = ["Network", "Unsafe"]`, `allowed_caps = ["Alloc"]`, and
 optional `forbid_trusted = true`.

 Done when `concrete check` / `concrete audit` fails a fixture that widens
 beyond the budget, reports the exact function/import that caused the
 widening, and reuses the same fact vocabulary as the packages area import fact
 constraints so the policy graduates cleanly to dependency/package
 boundaries.

The budget verdict must retain a canonical reachability witness: stable
function identities, direct edges, conservative possible-target sets for
indirect calls, the entry roots, the path introducing each reachable
capability, and every trusted/extern boundary where completeness stops.
Compiler-produced witnesses remain `compiler_validated`; they do not become
proof merely because the same compiler generated both the graph and verdict.
R-0443 later makes the narrow whole-program authority property independently
checkable from the canonical artifact.

### Task R-0441

**Phase:** D

**Pull trigger:** R-0441: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The pending alias-policy choice is superseded by its removal, but authority distinctions and regression duties survive. surviving requirement: Reject the removed Std capability alias and distinguish declared capability ceilings from reachable authority, retaining widening fixtures and mutation controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; documentation repair is already complete.

**Objective:** Decide how `with(Std)` behaves under an authority budget. The
canonical-list documentation half is complete.

**Decision status:** pending. Current profiles continue to accept `with(Std)`;
the recommendation below is not a shipped restriction. Ratification must choose
between rejecting the alias in high-integrity profiles and requiring an exact
transitive-use budget before any gate changes program acceptance.

`with(Std)` expands at parse time to every standard capability except `Unsafe`
(`Concrete/Frontend/Parser.lean` "Expand \"Std\" to the full set";
`docs/verification/SAFETY.md:61`). It is used in `docs/verification/SAFETY.md` and
several `adversarial_cap_*` fixtures. The canonical identity docs now list
`Std` beside the nine capabilities and accurately mark it as an alias over the
eight non-`Unsafe` capabilities. That documentation repair does not decide the
policy below.

The audit consequence is specific and it is why this sits beside the budget task.
Adding `Network` or `Process` use inside a `with(Std)` function produces **no
declaration widening** — the ceiling was already maximal — so a budget checked
against declared capabilities measures something that cannot change. It can still
appear in a *reachable-authority* diff, since that is a call-graph fact. So the
precise statement is: declaration-ceiling budgets over `Std` are meaningless,
reachable-authority budgets remain meaningful, and the two must not be conflated
in reports.

Decide one rule and gate it: either audited profiles reject broad aliases
outright, or a `Std` declaration must carry an exact transitive-use budget that
the compiler checks against reachable authority, so the alias stays a
convenience for scripts while audited code states what it actually reaches.
Related and worth stating in the same decision: a declared capability is an
authority *ceiling*, not evidence of use, so reports should distinguish declared
from reachable authority regardless of how the ceiling was spelled.

The list repair is complete; the task remains open for the audited-profile
policy and its gate.

Gate: a fixture where `Std`-declared code gains `Network` use, proving both
surfaces at once — declared ceiling unchanged, reachable authority changed — and
failing under the chosen rule; the same code under an explicit cap set (must
pass); a declared-vs-reachable report fixture; and a mutation removing the alias
leg from whichever check is chosen. For high-integrity profiles, rejecting `Std`
outright is likely the cleanest rule and should be the default proposal.

### Task R-0189

**Phase:** F

**Pull trigger:** R-0189: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Make `tested_by_oracle` evidence structured and diffable:

 - add an oracle manifest naming reference, seeds, case count, input model,
   comparison mode, and coverage kind;
 - split cases into boundary, known-vector, random, adversarial, and
   regression buckets;
 - report case counts, seeds, reference identity, comparison mode, and
   `not_proof` evidence level in audit output;
 - save failing cases as reproducible fixtures;
 - optionally cross-check reference, interpreter, and compiled binary;
 - support metamorphic tests where no complete reference exists;
 - flag oracle evidence weakening in `concrete diff` when cases, seeds,
   reference, comparison mode, or boundary coverage shrink.

### Task R-0193

**Phase:** F

**Pull trigger:** R-0193: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add spec provenance and adequacy facts to audit/release bundles: spec name, source standard or paper, independent reference if any, test-vector set, reviewer, review date, assumptions, and evidence class (`spec_trusted`, `spec_reviewed`, `tested_by_oracle`, or future `spec_refines_standard`). Do not let a source-to-spec proof imply the spec itself is adequate.

### Task R-0194

**Phase:** F

**Pull trigger:** R-0194: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0209: merge into R-0209.

**Objective:** Add evidence-level monotonicity checks to audit/diff output.

### Task R-0198

**Phase:** F

**Pull trigger:** R-0198: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0178: merge schema work into R-0178; verdict enforcement into R-0494.

**Objective:** Keep audit, contracts, obligations, assumptions, policies, manifests, and proof-status output on one shared vocabulary. Do not let each artifact grow its own mini-language for the same evidence classes.

### Task R-0199

**Phase:** G

**Pull trigger:** R-0199: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep as release acceptance.

**Objective:** Keep public-facing docs and website copy grounded in the same evidence vocabulary. Use the "Why Concrete" section of `docs/project/PROJECT.md` as the source for a C/Rust-oriented "why this exists" page: small systems code, explicit authority, visible evidence classes, spec-drift-tied proofs, named trust boundaries, and what Concrete deliberately avoids. The website should show the end goal and the current honest status, not catchy slogans or one-badge proof claims.

### Task R-0178

**Phase:** F

**Pull trigger:** R-0178: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; minimal authoritative dimensions needed in Phase 2.

**Objective:** Stabilize machine-readable fact schemas for proof status, obligations, effects, capabilities, assumptions, policies, snapshots, showcase metadata, runtime traps, synthesis attempts, stdlib evidence, and package evidence.

Absorb and version R-0445's early resource-fact schema for source steps,
call depth, source stack, allocation count/bytes, completeness, assumptions,
and blocker reasons. This phase may generalize the schema, but it must migrate
the existing fact identities and meanings rather than recompute resources with
a second analyzer.

Runtime-trap facts include one derived per-function view: direct trap sites,
transitively reachable trap kinds, the call/indirect-target path that makes each
kind reachable, and any trusted/extern boundary where completeness stops. This
is a projection of the shared site and call-edge facts, not a separately
maintained truth source. “No reachable trap” may be emitted only when the
conservative target closure is complete for the named scope.

Keep one shared evidence-class enum and one shared fact vocabulary across
reports; no report kind may invent private status strings for `proved`,
`reported`, `trusted`, `assumed`, `runtime_checked`, `tested_by_oracle`,
`observed_only`, or `stale`. This is the phase where the global **no second
truth source** rule becomes mechanical: README/site docs, LSP, MCP, agent
JSON, release bundles, artifact viewers, and package reports must wrap these
compiler facts or generated snapshots, not restate evidence status by hand.
Add a drift fixture proving a stale hand-written claim is caught or marked
explicitly non-authoritative.
Keep pipeline validation, independent-certificate, and kernel-replay status
in separate shared dimensions from that evidence-class enum. In particular,
`compiler_validated`, `certificate_structurally_checked`, a named checked
relation, and `kernel_replayed` answer different questions and may not be
collapsed into or used to upgrade a proof/evidence class.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0198** (merged): duplicate of R-0178: merge schema work into R-0178; verdict enforcement into R-0494.
- **R-0241** (merged): duplicate of R-0155/R-0178: merge obligation payload into those canonical schemas.
- **R-0249** (merged): duplicate of R-0178/R-0179: merge into R-0178/R-0179.

### Task R-0179

**Phase:** F

**Pull trigger:** R-0179: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; render R-0494 verdicts.

**Objective:** Add `concrete audit`: one human-readable plus machine-readable bundle covering authority, trust, allocation, proof status, obligations, assumptions, policy, snapshots, backend/target assumptions, replay, and the proof-story matrix specialized to the audited program.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0185** (merged): duplicate of R-0179: merge into R-0179.
- **R-0249** (merged): duplicate of R-0178/R-0179: merge into R-0178/R-0179.

### Task R-0182

**Phase:** F

**Pull trigger:** R-0182: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; retain bounded R-0486 increment.

**Objective:** Add `concrete diff old new`: authority/proof/trust/runtime-obligation diff.

This is also the first **proof/capability diff for code review** surface.

**First increment (2026-10-04):** serve R-0486 with exactly three change families:
added/widened authority, added/widened assumptions, and invalidated contract/proof
evidence. Derive the diff from existing canonical identities and facts; introduce no
parallel ledger. Produce human and JSON views of the same rows, including affected
subjects, old/new facts, evidence class and available provenance. Unsupported dimensions
and incomplete dependency coverage are explicit. The first increment exits with the
R-0486 change/repair cases and an unchanged-contract control; it need not implement
every later dimension or editor integration before being useful. The full task exit
below remains broader.

Output must include human text and JSON rows for: added/removed capability,
capability widening/narrowing, new `trusted`/`Unsafe`/extern boundary,
stale/missing/downgraded proof, new runtime trap site, allocation authority
change, changed assumption, changed stdlib evidence class, and a changed
per-function reachable-trap set even when the changed trap site is in a callee.

Done when the same JSON drives CI, a GitHub-comment/golden fixture, editor
display, and agent consumption. This is Concrete's review differentiator:
evidence changes live where code review happens, not in a separate proof
report nobody opens. A PR artifact should look like a compact evidence
changelog: `+ File capability`, `+ trusted function`, `- Unsafe removed`,
`verify_packet proof became stale`, `bounds obligation discharged`,
`dependency evidence downgraded`. It must preserve separate evidence classes
and must not collapse the review into one green badge.

### Task R-0183

**Phase:** F

**Pull trigger:** R-0183: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; separate later equivalence proving.

**Objective:** Add semantic trust diff gates: capability widening, allocation change, trusted boundary addition, stale proof, weakened/missing obligation, assumption widening, runtime-obligation change, and stdlib evidence-class drift. Add a red-team fixture proving the diff cannot emit a false-clean summary when a capability/trust/proof fact changed.

**First increment:** gate R-0182's three-family diff using R-0486. Each controlled
change must name the affected claim and produce the expected policy/evidence result;
repair restores acceptance only after required rechecking. A valid unchanged-contract
case remains accepted. Dropping a relevant fact or dependency edge must yield a
detected failure or incomplete coverage, never an unchanged/clean verdict. Keep the
later allocation, runtime and stdlib dimensions behind their existing fact producers.

**Selected-function equivalence follow-on (planned 2026-10-18):** use the
[Sparkle analysis](../../research/compiler/sparkle-emitted-code-validation.md) as a
reference for refactor checks. State the exact old/new relation, domain, failures
and assumptions, retaining each revision's own dependency and target context.
Re-elaborating an old body against current imports cannot establish equivalence
to the old program. Keep this behind the typed contract/model and R-0482 identity
prerequisites; a model-level proof does not supply an unchecked codegen edge.

### Task R-0185

**Phase:** F

**Pull trigger:** R-0185: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0179: merge into R-0179.

**Objective:** Add `concrete audit --json`: machine-readable audit output for CI, dashboards, editor tooling, and release bundles.

### Task R-0186

**Phase:** F

**Pull trigger:** R-0186: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; rename to avoid collision with compiler `verified` profile. Name the policy so it does not collide with the compiler's `verified` profile: a policy-clean LLVM program is not compiled through the verified native pipeline.

**Objective:** Add a **verified profile** command/policy surface that makes "formally verifiable code" operational without overclaiming. Candidate spellings:

 `concrete check --profile verified` and/or
 `concrete audit --profile verified`. The profile rejects or fails the
 policy when a selected target contains stale proofs, unresolved proof
 obligations, unapproved `assumed` facts, unapproved `solver_trusted` facts,
 unreviewed `trusted`/extern/`Unsafe` boundaries, unchecked runtime-safety
 obligations, open known holes, or missing replay commands. It must preserve
 evidence classes rather than upgrading them: `tested_by_oracle` remains
 testing, `solver_trusted` remains trusted unless separately replayed in
 Lean, and runtime checks remain runtime checks. Add a gate with one clean
 verified fixture and negatives for stale proof, assumption, SMT-only claim,
 unchecked unsafe, and unresolved runtime obligation.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0250** (merged): duplicate of R-0186: merge into R-0186.

### Task R-0187

**Phase:** F

**Pull trigger:** Reviewers need to browse the new compiler's release bundles and diffs beyond existing human and JSON audit output.

**Objective:** Add an artifact viewer CLI/TUI over facts, obligations, proofs, assumptions, release bundles, and diffs. It may show a compact dashboard, but it must never collapse different evidence classes into one fake green badge: `proved`, `runtime_checked`, `tested_by_oracle`, `assumed`, and `trusted` stay distinct on screen and in JSON.

### Task R-0188

**Phase:** F

**Pull trigger:** R-0188: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; minimum slice replay belongs in Phase 2.

**Objective:** Ensure every release bundle includes an evidence replay command.

Deliverable: `concrete replay <bundle>` rebuilds and rechecks the selected
tests, gates, proofs, doc snippets, report snapshots, and release facts. It
emits a summary of memory-safety enforcement, runtime checks, capabilities,
proofs, trusted boundaries, assumptions, and unsupported facts. It must work
without network access, without an LLM, and without private source paths.

Done when the replay gate covers: clean bundle replays green, stale proof
fails, missing gate fails, changed source fails, schema mismatch fails, and
an LLM-generated proof bundle replays using only checked artifacts.

### Task R-0190

**Phase:** E

**Pull trigger:** R-0190: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; requires typed contract semantics.

**Objective:** Add property-based contract testing as a cheap counterexample finder, not proof. Command surface: `concrete test --contracts --property --json` generates inputs satisfying `#[requires]`, executes the function, checks `#[ensures]`, runtime obligations, and selected `assert` facts, then shrinks failures to a minimal source-level witness. Evidence class:

 `tested_by_property`. It is counterexample-search evidence, not proof. Policy
 may prefer a proof or solver discharge only when the subject, property,
 assumptions, and semantic scope are comparable; unlike a ProofCore theorem, a
 native property run may cover compiled/runtime behavior. Required report
 fields: function, contract id, seed, generator profile, case count,
 shrunk witness, failing postcondition/obligation id, replay command, and
 whether the witness was persisted as a regression. Add
 `examples/property_contracts/` with `clamp`, `bounded_index`,
 `checksum_range`, one precondition-filtered generator, and one deliberately
 false postcondition. Wire `scripts/tests/check_property_contracts.sh`; the
 gate must prove property testing finds and shrinks the false claim without
 ever producing a `proved_*` status.

### Task R-0192

**Phase:** after-1.0

**Pull trigger:** New-compiler pilots with typed contracts need help deriving initial specification candidates from tests.

**Objective:** Add **observed contract inference from tests** as an explicit non-proof on-ramp to specs.


  Deliverable: `concrete infer-contracts <target> --from-tests --json`
  proposes candidate preconditions, postconditions, or invariants from
  passing tests/property cases. Every output fact is marked
  `observed_only` / `suggested_contract`, never `proved`.

  Reports must show sample size, generator profile, counterexamples tried,
  surviving mutants if any, and the command needed to promote the suggestion
  into a checked contract/proof obligation. Gates: infer a useful range
  postcondition from tests; reject or mark weak a vacuous suggestion; prove a
  mutated implementation can invalidate the observed claim; and ensure no
  inferred contract upgrades evidence without a separate proof or
  policy-approved assumption.

### Task R-0195

**Phase:** G

**Pull trigger:** The new compiler's audit bundle and fact schemas are stable enough for an agent audit demonstration.

**Reconciliation (2026-10-19):** Keep as workflow fixture.

**Objective:** Add one AI-audit demo where an agent answers authority/proof/trust questions using compiler facts rather than source guesses.

### Task R-0196

**Phase:** F

**Pull trigger:** R-0196: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add review checklists generated from facts: what changed, what widened, what became trusted, what lost proof, what gained assumptions, and which obligations remain open.

### Task R-0197

**Phase:** F

**Pull trigger:** R-0197: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; preserve replayability and identity.

**Objective:** Add artifact redaction/stability rules so release bundles can be shared publicly without leaking local paths, secrets, or machine-specific noise.

### Task R-0200

**Phase:** F

**Restructured (2026-11-02):** retired with the legacy compiler: The old CompilerLedger and ObligationCore renderer retires while the self-audit and provenance requirements survive. surviving requirement: The new compiler's self-audit must expose its own proof and pipeline facts through the shared audit surface without a private model or treating cache hits as validation. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0200: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; “historical origin” does not mean completed.

**Objective:** [historical origin: closed phase 4 item 42] Add compiler self-audit: `concrete audit

 --compiler` renders the `CompilerLedger` / `ObligationCore` as a reviewable
 bundle (ledger-from-ledger), proving the compiler's own facts are
 audit-visible through the same surface user programs use. When the incremental-driver area
 completed, it must consume its typed query/fact results and show cache/query
 provenance separately from evidence. Otherwise it consumes the eager shared
 pipeline facts. Neither branch may recompute a private audit-only compiler
 model, create a private cache, or present a cache hit as validation.

### Task R-0201

**Phase:** F

**Pull trigger:** R-0201: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep, staged with available producers.

**Objective:** Add the audit-commands area validation artifact: one package-scale audit bundle fixture with human and JSON output, semantic diff before/after a change, artifact viewer smoke test, oracle manifest, property-test manifest, persisted counterexample regression, spec-provenance facts, redaction check, replay command, and a README showing how a reviewer answers authority, proof, trust, assumption, and runtime-obligation questions without reading compiler internals. When the incremental-driver area exists, cold, warm, and cache-off audit/diff/why output must be identical after normalizing timing/cache-observability fields, and corrupt/stale query artifacts must recompute or fail closed; otherwise

 the same fixture runs eagerly with no private caching.

## Area: Proof Status And Trust Gates

Goal: make every green proof/evidence status precise, traceable, and hard to
misread.

Done when: all existing production proof specs are directly and transitively
FnTable-complete, proof dependencies and provenance are visible, assumptions
and trust boundaries have lifecycle reports, and weaker evidence cannot appear
under a stronger badge.

### Task R-0202

**Phase:** E

**Pull trigger:** R-0202: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The dependency-closure requirement survives while registered-spec FnTable traversal and legacy maintenance retire. surviving requirement: New-Core proof subjects must have complete transitive callee dependencies with missing dependencies rejected or explicitly reported. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0495/R-0512: merge dependency-closure requirement into Core migration; maintain legacy checks until retirement.

**Objective:** Add transitive FnTable completeness: walk registered spec call graphs, not only direct call sites, and fail or flag missing callees before theorem authors hit confusing `none` evaluations.

### Task R-0204

**Phase:** F

**Pull trigger:** R-0204: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by R-0494: merge into R-0494.

**Objective:** Add per-obligation proof/evidence status. Function-level status is only a summary; padding, block fold, digest serialization, final composition, contract clauses, runtime obligations, oracle checks, and assumptions each carry their own evidence class.

### Task R-0206

**Phase:** E

**Pull trigger:** R-0206: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The debugging requirement survives while registered extraction fingerprints and legacy callee tables retire. surviving requirement: Failed or stale new-Core proofs must expose current subject identity, expected statements, missing dependencies and actionable lemma guidance. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** duplicate of R-0166: merge into R-0166.

**Objective:** Add proof debugging output for failed/stale proofs: extracted spec, current fingerprint, registered fingerprint, expected theorem shape, missing callee facts, likely missing lemma class.

### Task R-0208

**Phase:** F

**Pull trigger:** R-0208: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The accepted advisory and replay limits survive while legacy evidence inventories and closed wiring are not migrated. surviving requirement: New-compiler evidence must track proof-tool drift and checker advisories, require actual replay and fail closed for affected or unknown checkers while retaining substitution controls. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** done — closed 2026-10-13 at `2b570c5a`; the body is retained for its accepted limits, now owned by R-0532.

**Objective:** Add tool-version drift checks: proof/evidence facts record the Lean version, Concrete compiler commit, ProofKit hash, extraction version, decision procedure version, and solver version where relevant. A toolchain bump marks affected evidence `needs_recheck` until replayed; old green badges are never silently reused across a proof-tool upgrade.

This task also owns a machine-readable checker-advisory register. Key entries by checker/kernel
implementation, exact version or commit, rule-set digest and upstream advisory/reproducer id; do
not rely on an unverified CVE label or a prose version range. Each entry records
`accepted | affected | revoked | unknown`, the affected evidence classes and fragments, replacement
versions, replay requirements and claim indexes. Release policy must refuse or downgrade evidence
whose checker state is affected, revoked or unknown. A changed advisory register is evidence input
and therefore changes the policy result even when the proof artifact is byte-identical.

**CLOSED 2026-10-13 at `2b570c5a`.** Push CI
[38327442096](https://github.com/unbalancedparentheses/concrete2/actions/runs/38327442096) green;
campaign [38327460557](https://github.com/unbalancedparentheses/concrete2/actions/runs/38327460557):
116 discovered = killed, 0 invalid, 0 survived, 0 could-not-apply, `completed=1`, `integrity_ok=1`,
`qualified=1`, the local aggregator re-run identical to CI. The Linux artifact `checker-acceptance-Linux`
from that push run is the self-contained evidence for the registered build `3eb21141…` on the closure
commit: register `registered`, identity 15/0, kernel re-check 10/0 and policy 24/0 with logs tied to
that identity, the accepted-build consumer re-check leg (5 of 5 only after re-check), axiom inventory
7/0, claim inventory IDENTICAL; the Lean #14576 reproducer is rejected there too. The earlier artifact
and control logs stay as supporting evidence. Criteria → evidence: `docs/verification/R-0208-closure.md`.
Still open by decision: native-trust receipts (withheld), limited rollback protection, correlated
not authenticated checker self-report.

Fragment-scoped narrowing is tested only after the broad fail-closed path. Surface VIR lacking the
affected construct is not sufficient because attached metaprograms can add unrelated hostile
declarations; an accepted narrowing needs an independently checked retained-environment footprint.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0208).

### Task R-0209

**Phase:** F

**Pull trigger:** R-0209: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by R-0494: merge into R-0494; retain audit/diff tests.

**Objective:** Add evidence monotonicity checks: a refactor cannot silently present a weaker claim as if it were still stronger (`proved` cannot degrade to `reported` while retaining the same badge/summary).

### Task R-0213

**Phase:** E

**Pull trigger:** R-0213: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; specify decidable scope and unknown outcomes. Vacuity results form an explicit algebra (`nonvacuous` / `vacuous` / `unknown`) with timeout behaviour, policy treatment and witness scope; the admitted decision fragment is stated. A valid implication and a useful specification are different judgments.

**Objective:** Add vacuity gates to proof status: `proved` summaries must be downgraded or blocked when the proof depends on an unsatisfiable precondition, contradictory assumptions, unreachable code path, or invariant `false`.

### Task R-0215

**Phase:** E

**Pull trigger:** R-0215: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The substitution and frozen-requirement controls survive while PExpr and table-entry mutations retire. surviving requirement: New-Core evidence mutation tests must detect changed subjects, contracts, hypotheses and trust, using an externally frozen exact requirement baseline and a contract-preserving positive control. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Merge common infrastructure into R-0491/R-0498; retain workload mutations.

**Objective:** Add spec/proof mutation testing to prove evidence is load-bearing. Command surface: `concrete mutate-evidence --target <example> --json` creates controlled mutants: change a function body under a proof link, weaken or delete an `#[ensures]` clause, strengthen an impossible `#[requires]`, remove a loop invariant, alter a spec PExpr/table entry, change a theorem name, and perturb a trusted assumption. Expected outcomes must be explicit:

 stale, missing, vacuous, partial, failed proof, widened trust, or unchanged
 only when the mutation is semantically irrelevant and justified. Wire
 `scripts/tests/check_evidence_mutation.sh` over `hmac_sha256`,
 `constant_time_tag`, `proof_patterns`, and one contract-negative example.
 The gate must fail if a mutated proof/spec still reports the original green
 evidence class without an allowed explanation. This is evidence about the
 evidence: proofs must constrain the implementation, not merely decorate it.

**Frozen-requirement controls (2026-10-25; planned):** extend this mutation
corpus with an accepted requirement baseline outside the candidate's writable
scope. Bind it to the exact subject, contract/proposition and allowed assumptions
through the existing identity machinery; a theorem name or refreshed fingerprint
alone cannot authorize a changed requirement. Include deletion of a proof link,
contract changes followed by regenerated fingerprints, transitions to ineligible
or trusted code, and theorem replacement under an unchanged name. Record the
checker result, applicable policy result and baseline comparison separately.
A successful check of the candidate's new requirements must not count as satisfying
the original frozen requirement. Reject any requirement-identity change in this
experiment; do not claim to decide semantic weakening from hashes.

Retain a positive control with unchanged requirements and a contract-preserving
implementation edit whose implementation binding is revalidated. Preserve the
existing evidence dimensions for eligibility, freshness, logical validity,
correspondence and assumptions; do not add a flat status vocabulary. Start with
an already-proved example in this task's corpus after the relevant subject/spec
binding repairs are qualified. Whether an eligibility/trust transition currently
escapes a required-claim policy is a hypothesis to test, not a reproduced defect.
These controls precede the agent variant in R-0486 and add no separate queue row.

### Task R-0216

**Phase:** F

**Pull trigger:** R-0216: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; R-0208’s one upgrade does not supply the general command.

**Objective:** Add proof-corpus migration across toolchain upgrades, the active half of the §7 drift story. Detection marks evidence `needs_recheck`; migration must turn a bumped corpus green again without hand-walking every obligation. Command surface: `concrete prove --recheck-corpus [--json]` re-runs every linked proof/evidence check under the current toolchain and triages each into `still_proved`, `replayed_clean`, `broke_needs_repair`, or `unavailable_dependency`; `concrete prove --recheck <obligation_id>` does one.

 Pin the external Lean proof-library surface the corpus depends on (Lean
 stdlib / Batteries / Mathlib lemmas and tactics actually cited) in a checked
 `proofs/lean-deps.lock` with versions, so a renamed or relocated upstream
 lemma is reported as `unavailable_dependency`, never a silent break. Wire
 `scripts/tests/check_proof_corpus_migration.sh`: it must prove the flagship
 corpus (`hmac_sha256`, `constant_time_tag`) re-greens under a simulated
 toolchain bump, that a removed/renamed pinned lemma surfaces as
 `unavailable_dependency`, and that no `needs_recheck` obligation can reach a
 green badge without an actual kernel re-check.

### Task R-0218

**Phase:** F

**Pull trigger:** R-0218: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The single-authority requirement survives while converting the old ObligationCore surfaces retires. surviving requirement: Proof-status reports and authoring commands must be views of canonical new-Core obligation facts rather than independent verdict recomputation. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0494: merge into R-0494.

**Objective:** [historical origin: closed phase 3 items 15/16] Convert `--report proof-status` and `concrete prove`'s obligation facts from consistency-gated recompute to literal `ObligationCore`-ledger views — the last old phase 3 surfaces that recompute (sound today because consistency-gated). They are the proof-status / trust surfaces, so they belong in this phase.

### Task R-0219

**Phase:** F

**Pull trigger:** R-0219: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep as cumulative gate suite.

**Objective:** Add the proof-status area validation artifact: a trust-gate pressure project that includes transitive proof dependencies, stale dependency propagation, tool-version drift, proof-corpus migration across a simulated toolchain bump, assumption widening, spec-adequacy policy, vacuity downgrade, solver portfolio / disagreement handling, evidence mutation testing, axiom inventory and clean-checkout replay, weaker-evidence monotonicity, and a release gate proving each status cannot be silently presented as stronger evidence. Include cache-off/on/verify parity and stale/corrupt-cache negatives so the trust gates cover the operational path

 users actually run.

### Task R-0212

**Phase:** F

**Pull trigger:** R-0212: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add spec-adequacy gates: release policy can require reviewed spec provenance for selected claims, forbid unreviewed specs in graduated flagships, and show when a theorem is `proved_by_lean` against a `spec_trusted` or unreviewed spec.

### Task R-0214

**Phase:** research

**Pull trigger:** A new-Core claim needs another external SMT solver or an admitted query produces solver disagreement.

**Reconciliation (2026-10-19):** Keep outside the strict queue.

**Objective:** Add solver portfolio and cross-solver agreement as a strictly separate evidence class, never as kernel evidence. External SMT V1 may start with Z3 only, but the trust-gate roadmap must define how to run `z3`, `cvc5`, and `bitwuzla` where the fragment applies. Result classes:

 `solver_trusted` for one trusted solver, `solver_cross_checked` when two or
 more independent solvers agree on the same `unsat` result for the same
 normalized query, `solver_disagreement` when they differ, and
 `solver_unavailable` / `solver_timeout` / `solver_unknown` for non-proofs.
 Agreement must record solver names, versions, logic, query hash, timeout,
 and replay commands. For the same normalized query, assumptions, scope, and
 policy, two independent agreeing solvers provide more corroboration than one;
 this does not globally rank solver evidence against a Lean theorem or kernel
 decision over a different subject or semantic scope. Add
 `examples/solver_portfolio/` with one
 QF_NIA query, one bitvector query, one unsupported-fragment case, one fake
 disagreement wrapper, and one missing-solver case. Wire
 `scripts/tests/check_solver_portfolio.sh`; the gate must prove no external
 solver result can overwrite kernel evidence and that disagreement blocks
 release claims unless explicitly assumed.

### Task R-0451

**Phase:** research

**Pull trigger:** A consumer of new-Core solver evidence needs auditable DRAT replay after solver evidence classes are defined.

**Reconciliation (2026-10-19):** Keep as measured certificate-checking experiment.

**Objective:** Reduce what `solver_trusted` actually asks a reviewer to trust by
making refutation *certificates* replayable in code the project controls: port a
CDCL SAT solver to Concrete, emit and consume DRAT, and gate that a corrupted
certificate is rejected. Staged — microsat (251 lines, upstream `26985d9b`) as
the probe that the port shape works, then MiniSat (5,296 lines, `37dc6c67`) as
the real solver. Fit analysis is in
[research/workloads/port-candidates.md](../../research/workloads/port-candidates.md).

Why it belongs in this phase rather than the port ladder: its value is a change
to the trust model, not a workload lesson. R-0214 defines `solver_trusted` and
`solver_cross_checked` as classes that record *which* external binary was
believed; a Concrete checker that replays a DRAT certificate converts part of
that belief into a mechanical check whose implementation is auditable in-tree.
It executes after R-0214 because the class boundaries must exist before anything
can strengthen a result inside them.

**Definition of done.** The solver decides a pinned benchmark set with results
matching MiniSat, emits DRAT for every `unsat`, and the in-tree checker accepts
those certificates while rejecting mutated ones — certificate mutation is the
red-team guard, and a checker that accepts a corrupted proof is the failure mode
the gate exists to catch. Wire `scripts/tests/check_sat_certificate_replay.sh`,
and cross-check the checker against `drat-trim` on the same certificates so a
bug in the port cannot silently manufacture agreement.

**Claim discipline.** A Concrete SAT solver produces **no kernel evidence**. Its
own correctness rests on Concrete's compiler and Lean's kernel, so replaying a
certificate with it yields at most a new distinctly-named class (a checked
refutation certificate) that sits beside `solver_trusted` under R-0440's
orthogonal-fields model — never `proved_by_lean`, and never a silent upgrade of
an existing solver result. If the port cannot be given a class that is honestly
weaker than kernel evidence, it does not ship.

### Task R-0458

**Phase:** F

**Restructured (2026-11-02):** retired with the legacy compiler: The old report and spike fields retire while orthogonal evidence semantics and recheck controls survive. surviving requirement: New-Core attestations must separate strength, independence and portability, retain replay artifacts and validate attestation integrity, including admitted-proof and oracle controls. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0458: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; reconcile with R-0440 and current evidence architecture.

**Objective:** Split the evidence vocabulary into two independent coordinates and make
every attestation carry its own re-check procedure.

`proved_by_two_kernels` currently fuses two unrelated facts into one string: how a claim
was established, and how many independent implementations established it. Fused, it
reads stronger than it is. Replace it with `strength` (per attestation) and
`independence`, plus `statable_in` / `proved_in` for portability, and demote the composite
name to a display label over the tuple as R-0440 requires.

**Partially landed — scope this task to the remainder.** R-0448's spike already ships the
independence coordinate as `independent_of` in the VC artifact
(`Concrete/Report/Report.lean:3895`), with four fields rather than the three sketched here:
`spec_formalization`, `kernel_implementation`, `kernel_foundations`, `bridge`. Per-kernel
receipts carrying kernel, version, verdict, `lowering_agreed` and a replay command also
exist, and the composite string is already display-only in the artifact. What remains is
the `strength` coordinate, `statable_in` / `proved_in`, and removing the composite from
the *human report* line (`Main.lean:1677` still intercalates it), plus the `keep`/`attest`
additions below.

Two additions the current ledger lacks. `keep`: retain the certificate artifact, because
"coqc exited 0 at time T on machine M" is neither re-checkable nor shippable in a proof
bundle. `attest`: the emitted script must assert its own integrity rather than have the
driver infer belief from an exit code — `coqc` exits 0 on `Admitted.`, and the Isabelle
driver already demonstrates the pattern with `Thm_Deps.all_oracles`.

The re-check ladder must stay honest: where a transferable certificate exists (LRAT,
Alethe) a third party re-checks independently of our toolchain; where none exists
(`omega`, `lia`, `presburger`) the claim is `trusted_modulo_toolchain`, because `lia` has
no certificate export and a `.vo` is re-checkable only by the same Rocq version.

Normative shape in [docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md](../verification/PROVER_NEUTRAL_OBLIGATIONS.md).

## Area: Provable And Predictable Subsets

Goal: give users a named small subset they can rely on for serious
proof/evidence work.

Done when: the subset family has public names, allowed constructs, rejected
constructs, arithmetic-site policy, runtime-error policy, and compatibility
promises.

### Task R-0224

**Phase:** E

**Pull trigger:** R-0224: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; integrate S13/S15 resource relation.

**Objective:** Define source-level stack-depth versus backend/target stack
claims, including how a source estimate, ABI/backend frame growth, and a named
target budget combine before the predictable profile may call stack use
bounded. Consume R-0445's source-stack and call-depth facts as the source
layer, then define the checked bridge to backend frame facts. Never relabel the
source estimate as target stack bytes.

### Task R-0228

**Phase:** D

**Pull trigger:** R-0228: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by R-0499/S8 and V6: merge enforcement into R-0499; raw-operation extensions into V6.

**Objective:** Define the **Unsafe island** rule for unchecked operations. Any operation that bypasses a safe runtime check, borrow/linearity rule, raw-pointer restriction, layout proof, or FFI ownership boundary must be exposed through a deliberately small surface requiring `with(Unsafe)` or an explicitly named `trusted` boundary. Examples include future `get_unchecked`, raw pointer place writes, unchecked casts/conversions, inline asm, and unchecked FFI wrappers. There is no global unsafe mode and no silent trusted wrapper that hides authority: audit output must show the safe wrapper, the underlying trusted/Unsafe operation, and the assumption being accepted.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0228).

### Task R-0232

**Phase:** C

**Pull trigger:** R-0232: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; use versioned V-slice/profile names.

**Objective:** Update `CLAIMS_TODAY.md`, README, showcase docs, and release bundles to use the frozen subset names consistently.

### Task R-0233

**Phase:** D

**Pull trigger:** R-0233: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The exclusion duty survives while ProofCore extraction and integer PBinOp encoding retire. surviving requirement: Float code must remain excluded from proof claims without an explicit float semantics profile, with eligibility negatives and honest audit labels. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** done — the unprofiled-float exclusion shipped (`ProofCore.lean`, `Report.lean`); profiled floats remain V7 (R-0496/R-0495).

**Objective:** Close the unprofiled-float proof hole before any float proof claim:

 float-typed params/returns/locals/literals/ops are excluded from ProofCore
 extraction unless an explicit float profile is active. Audit output must
 say `float semantics: unprofiled` and `proof eligibility: excluded` rather
 than reporting a float operation as extracted through integer `PBinOp.add`.

### Task R-0236

**Phase:** D

**Pull trigger:** R-0236: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by R-0506/R-0509: merge axiom classification and inventory.

**Objective:** Classify the first float semantics layer honestly. Until Concrete imports or proves a checked IEEE-754 semantics library, primitive float operations are `float_semantics_trusted`; proofs over profiled float code are refinements to an explicit bit-level IEEE spec under that named trusted primitive layer, not `proved_by_lean` from first principles.

### Task R-0238

**Phase:** D

**Pull trigger:** R-0238: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by R-0511/R-0512: merge into cumulative slice matrices; retain orthogonal policies.

**Objective:** Add the provable-subsets area validation artifact: a profile matrix project covering `PredictableV1`, `ProvableV1`, unprofiled-float exclusion, profiled-float admission, borrow/reference proof-class decisions, constant-time source shape, stack/runtime-failure assumptions, and negative examples for every exclusion. The gate must prove reports never call excluded code proof eligible.

### Task R-0220

**Phase:** E

**Pull trigger:** A freestanding new-compiler workload requires a predictable profile after bounded resource facts and target stack budgets are available.

**Reconciliation (2026-10-19):** Keep as orthogonal policy over verified slices.

**Objective:** Define `PredictableV1`: no allocation unless bounded, no FFI
unless trusted and assumed, no unbounded loops/recursion, an enforced
target/profile stack budget, and an explicit failure-path policy. A stack
budget may come from an explicit policy or a named target-profile default; do
not derive a universal byte threshold from the current example corpus.
Freeze and policy-enforce the relevant R-0445 resource facts; do not invent a
new predictable-only resource analyzer. `unknown` is a failed predictable
precondition unless an explicit trusted assumption names and scopes the missing
boundary.

### Task R-0225

**Phase:** research

**Pull trigger:** A new-compiler workload requests a source constant-time profile with explicit backend timing assumptions.

**Reconciliation (2026-10-19):** Keep; no native timing guarantee without separate transport.

**Objective:** Define source-level constant-time profile v1:

no secret-dependent branch, no secret-dependent memory index, fixed loop
bounds, explicit backend timing assumptions.

### Task R-0226

**Phase:** research

**Pull trigger:** The new compiler's security-profile design needs public, secret and timing-sensitive labels for a concrete secrecy claim.

**Reconciliation (2026-10-19):** Merge into R-0225’s security-profile design.

**Objective:** Define secret/data-sensitivity labels for future security work:

`public`, `secret`, `timing-sensitive`.

### Task R-0229

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The body forbids treating borrow-using code as proof-eligible through a value-only ProofCore model. surviving requirement: Classify reference-using proofs by supported borrow semantics and require explicit frame obligations for mutable references, excluding unsupported functions from proof claims. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0229: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by R-0495 and V2–V4: merge into slice admission/claim coverage.

**Objective:** Decide the proof class for references and borrows. A function using `&` or `&mut` must be classified as one of: value-only/borrow-free, proved over read-only references, proved over mutable references with explicit frame/modifies obligations, or enforced-only and outside `ProvableV1`. Do not let borrow-using code appear proof-eligible through a value-only ProofCore model.

### Task R-0234

**Phase:** E

**Pull trigger:** R-0234: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by R-0496/S7, V7: merge into V7 float-profile work.

**Objective:** Define `ProvableFloatV1` as a separate, narrow proof profile:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0234).

### Task R-0235

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The disposition retires PVal/PBinOp expansion but explicitly retains Core float semantics. surviving requirement: Admit float proofs only over an explicitly supported float profile with defined Core semantics. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0235: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by R-0496/R-0495: retire PVal/PBinOp expansion; implement Core float semantics.

**Objective:** Add ProofCore support for profiled floats only after Task R-0233 is closed:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0235).

### Task R-0237

**Phase:** E

**Pull trigger:** The new compiler can prove exact IEEE behavior for a fixed-order floating-point kernel.

**Reconciliation (2026-10-19):** Keep as fixture, not another flagship.

**Objective:** Add one small `ProvableFloatV1` proof-profile witness only after the profile exists.
It is a fixture/workload, not a fourth public flagship:

 a fixed-order `f32`/`f64` kernel such as clamp/normalize, tiny FIR/IIR, PID,
 or dot product. Prove exact IEEE behavior first; real-valued epsilon-bound
 refinement is a later layer.

### Task R-0239

**Phase:** E

**Pull trigger:** R-0239: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Define the SPARK-class contract layer in Concrete's vocabulary, not Ada's:

 frame/read/write contracts, dependency-flow contracts, ghost/spec purity,
 and proof classes for each fact. Candidate surface may include
 `#[reads(...)]`, `#[writes(...)]`, `#[modifies(...)]`, and
 `#[depends(out <- in1, in2)]`, but syntax is not frozen until a flagship or
 external workload pulls it. The design must explain how these facts differ
 from capabilities: capabilities name external authority; frame/dependency
 contracts name memory/state/data influence. Add
 `docs/verification/SPARK_CLASS_ASSURANCE.md` updates, examples with one parser/buffer
 loop and one policy/data-flow function, and a gate proving facts are
 reported as proved/enforced/reported/assumed/trusted rather than prose-only.

### Task R-0240

**Phase:** after-1.0

**Pull trigger:** A new-compiler user requests development expectations that release policy can distinguish from proof.

**Objective:** Define a development-only expectation policy, borrowing Roc's useful lightweight `expect` idea but fitting Concrete's evidence model. The goal is a clear place for "this should hold during tests/dev" that cannot be mistaken for proof, runtime-safety evidence, or a release assumption.

 Candidate surface may be an attribute, a test helper, or a restricted
 `expect` form, but the semantics must be decided before syntax: it is
 allowed in tests/examples and development profiles; it produces
 `tested`/`dev_checked` evidence only; it is excluded from `ProvableV1`
 unless translated into an explicit contract/obligation; and release bundles
 must either omit it, downgrade it, or list it as non-release evidence. Add
 `docs/EXPECTATION_POLICY.md`,
 `examples/expectation_policy/{dev_expect,test_expect,release_reject,contract_replacement}/`,
 and `scripts/tests/check_expectation_policy.sh`; the gate must prove that a
 passing expectation is never reported as `proved` and that a release/high-
 integrity profile cannot silently rely on it.

### Task R-0459

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The body mixes old arithmetic and typed-term machinery with exhaustiveness, relational-proof and hand-proof requirements. surviving requirement: Support non-arithmetic obligations with replayable kernel discharge, freshness-bound hand proofs, and separately scoped relational proofs without granting proof status to uncertified solver results. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0459: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Split exhaustiveness, relational logic and hand-proof policy. Its relational (2-safety) proposal moved to R-0527.

**Objective:** Add obligation families that are not arithmetic, so the ledger can demand
non-arithmetic proofs instead of merely accepting them.

Every family the compiler generates today is arithmetic: overflow, bounds, div,
call-site preconditions, asserts. There is no family for exhaustiveness, termination,
determinism, relational properties or refinement. That is why a 772-line parser produces
zero obligations — nothing is looking for anything it could be wrong about. The
non-arithmetic proofs that do exist are opt-in `#[proof_by]` attachments, so nothing
enumerates them and nothing reports them missing.

Two families first, chosen because neither needs new logic nor the typed obligation IR:

- **Exhaustiveness** — every reachable `(state, opcode)` pair has a defined transition.
  Finite case analysis, decidable, closable by all three hosts. `examples/vm` is the
  corpus. This is the plumbing proof that a non-arithmetic kind flows through
  obligations, ledger, reports and policy.

  Discharge it with a **kernel** (Lean `decide`, Rocq case analysis, Isabelle), not with
  SMT — and for a measured reason, not a stylistic one. cvc5 and z3 both *prove* datatype
  goals of this shape, but cvc5's Alethe output cannot *certify* any datatype-bearing
  proof: a trivial ground selector goal already fails with `DUMMY_SKOLEM`. Routing here to
  SMT would therefore yield an unreplayable `solver_trusted` verdict, violating the
  re-check invariant, in exchange for automation a kernel supplies anyway on a decidable
  finite problem. Locked by `check_multi_kernel.sh`; if Alethe gains datatype support that
  gate fails and this route should be revisited.
- **Relational (2-safety)** — constant-time and determinism via a Core-to-Core
  self-composition transform: two renamed copies, equal public inputs, assert equal
  observable trace. Not expressible as a single-execution VC at all. Bounded, so it
  unrolls to a bitvector query with a retained LRAT certificate checked by two
  independent checkers. `ct_compare_32` is the target, and today that property is only
  *enforced by a tag* — proving it is the `enforced -> proved` upgrade.

Prerequisite discovered by inspection, not assumption: `arithToBVW` handles `.add` and
`.mul` only, so the bitvector path drops xor/and/or/shift entirely. `ct_compare_32` is
xor-and-or. The bitvector sort and those operators must land as a slice of the typed
term IR (R-0454's substrate) rather than as another column in the abstraction that slice
replaces.

Also add `proved_by_hand` as a first-class strength, reusing the existing
proof-fingerprint freshness machinery — the piece that usually kills hand-proof
approaches and which already exists here.

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

### Task R-0241

**Phase:** F

**Pull trigger:** R-0241: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0155/R-0178: merge obligation payload into those canonical schemas.

**Objective:** Define stable obligation schema v1: id, kind, source span, function, expression, dependencies, evidence status, discharging theorem/check/ assumption, and replay command.

### Task R-0243

**Phase:** E

**Pull trigger:** R-0243: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Merge semantic/generator proof ownership into R-0460 and typed-operation slices.

**Objective:** Generate narrowing/invalid-cast obligations.

### Task R-0248

**Phase:** E

**Pull trigger:** R-0248: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Generate stack/recursion obligations where the profile claims
boundedness and enforce the configured target/profile stack budget rather than
only reporting it. Gate just-under/just-over programs, deep call chains with
large frames, report/gate accounting agreement, and a mutation that removes
enforcement while leaving the report. Enforce over R-0445's source facts only
after R-0224 supplies the named backend/ABI/target bridge; keep source and
target byte claims distinct in the resulting evidence.

### Task R-0251

**Phase:** C

**Pull trigger:** R-0251: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The regression corpus survives while the legacy overflow_checked release-policy expectation must be replaced. surviving requirement: Retain runtime-error regressions as new-compiler slices admit their constructs, rebasing policy expectations to decided arithmetic and failure semantics. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; integrate R-0491.

**Objective:** Add a runtime-error regression corpus: invalid cast, loop-bound violation, lossy byte/text conversion, ignored fallible result, unwrap-like failure, panic/abort profile mismatch, and release-policy rejection for missing `#[overflow_checked]` evidence where required.

### Task R-0253

**Phase:** E

**Pull trigger:** R-0253: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0166: merge into R-0166.

**Objective:** Add high-quality diagnostics for obligation failures: violated obligation, source expression, required evidence, current status, and next action.

### Task R-0254

**Phase:** E

**Pull trigger:** R-0254: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; route through R-0494 and policy.

**Objective:** Add obligation suppression only through explicit assumptions or policy waivers, never comments or hidden allowlists.

### Task R-0255

**Phase:** E

**Pull trigger:** R-0255: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0460: merge into R-0460.

**Objective:** Prove or validate obligation-generation soundness for the first obligation kinds through the compiler soundness bridge.

### Task R-0259

**Phase:** E

**Pull trigger:** R-0259: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep as cumulative corpus.

**Objective:** Add the runtime-safety area validation artifact: a runtime-safety corpus covering bounds, div/mod-zero, overflow, casts, panic/abort/assert, byte/text/path boundaries, stack/recursion, inferred invariant candidates, newtype invariant hypotheses, arithmetic-site evidence mismatches, and obligation suppression. Each case must show one of `proved`, `enforced`, `assumed`, `missing`, or `blocked`, include a negative variant, and run through policy gates plus human/JSON reports.

### Task R-0461

**Phase:** E

**Pull trigger:** R-0461: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The debt-composition semantics survive while wiring the old Evidence records and generators retires. surviving requirement: New-Core hypotheses must retain provenance and justification dependencies, cap conditional claims at assumed and preserve non-vacuous H23 and composition-removal controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Live; the H23 fix is done — merge the remaining generalization into R-0494/R-0460 and archive the H23 implementation narrative.

**Objective:** Give hypotheses provenance, and compose obligation status across the
assumption edge. **Closes H23, which is live: a guaranteed out-of-bounds access currently
reports `proved_by_multi_kernel (3: lean, rocq, isabelle)`.**

Do this before any further evidence-surface work. It is not a refinement of the
multi-kernel story; it is the defect that story is currently decorating.

**The substrate now exists.** Register C shipped 2026-07-31 as
`Concrete/Report/Evidence.lean`: `Evidence` carries an assumption set, `underHypotheses`
folds a claim together with its hypotheses' debt, and `HypOrigin` already encodes the four
sources below with `debt` total over them. C2′ and C3 are compile-time theorems, so once
this task populates `assumes` the cap applies **by construction** — R-0461 is now wiring
provenance into the generators, not designing a mechanism.

The mechanism, not just the symptom. `hyps : List Expr` erases where a fact came from,
and the three sources have entirely different justification status:

| Origin | What justifies it |
|---|---|
| control-flow guard | sound by construction (the branch was taken) |
| `#[requires]` | discharged at every call site by `callSiteObligations` |
| loop `#[invariant]` | owes O1 (init) **and** O2 (preservation) for that loop |
| explicit `#[assume]` | nothing — must downgrade the conclusion, permanently |

So hypotheses become records carrying `origin` and an optional `justifiedBy` obligation
reference, and status composes:

```
status(O)  ≤  min( status of O's own proof,
                   min over h ∈ hyps(O) of status(justifiedBy(h)) )
```

This is the same discipline already applied to proof dependencies (`staleDeps`) and to the
`trusted` boundary. Hypotheses are simply a dependency edge that was never modelled — which
is why the fix is a record change plus a fold, not new proof machinery.

**Reporting: DECIDED 2026-07-31, do not re-open when implementing.** The capped status is
the EXISTING `assumed`, plus a structured `conditions : [{ref, status}]` naming what is
outstanding. Not a new conditional badge, and specifically **no status string containing
`proved` may be emitted for a claim with an undischarged condition** — that rule is the
gate. Rationale in [docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md](../verification/PROVER_NEUTRAL_OBLIGATIONS.md):
a conditional badge still containing `proved` reproduces H23 for every consumer that
pattern-matches the status, which is how H23 fooled the policy gate and the report reader
alike. The precedent exists twice already (`assumed` for `#[assume]`, `assumed_at_entry`
for `#[requires]`), and `assumed` is already gate-forbiddable via
`ProjectPolicy.forbidAssume`, so the cap makes H23 catchable by machinery that is already
wired. Independence is orthogonal and unchanged: the receipts still record that three
kernels closed the goal.

Gate over `examples/unsound_hypothesis/src/main.con`: the bounds obligation must NOT read
proved while O2 is unproven, the fixture must stay non-vacuous (assert O2 really is
undischarged, so the gate cannot pass by the invariant becoming provable), and a mutation
must show the gate detects removal of the composition.

Widen beyond loops once the mechanism exists: any obligation resting on an `#[assume]`,
and any call-site precondition whose caller-side discharge is itself conditional.

### Task R-0462

**Phase:** E

**Pull trigger:** R-0462: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Live; the initial gate is done — broader coverage merges into R-0491; the "missing" claim is historical.

**Objective:** Fuzz the *compiled binary* against the safety claims — the empirical shadow
of Register A.

Register A asserts *if the obligation holds, the runtime property holds*. That is a
proof obligation, and R-0460 discharges it row by row over years. There is a cheap
continuous check of the same statement available now, and it is missing: **for every
function whose runtime-safety obligations all read `proved`, generate inputs satisfying
its `#[requires]`, run the COMPILED binary, and assert it does not trap.** Any trap is a
counterexample to Register A, to the obligation generator, or to the lowering — and it
is found in seconds rather than argued about.

Distinct from `--report bridge-check`, which fuzzes concrete inputs against the
*obligation* as evaluated by the interpreter. That tests the obligation against a model.
This tests the *claim* against the artifact that ships, and so also crosses the
surface→Core→SSA→LLVM lowering that no register row covers today.

H23 is the existence proof: a program whose bounds obligation reads
`proved_by_multi_kernel` aborts on the first run. Any fuzzer pointed at the binary would
have caught it immediately; every kernel-side surface reported success.

This is the highest fault-finding value per unit of work available, and it is the
project's own doctrine applied consistently — differential against the real artifact
outranks agreement between checkers of a model.

### Task R-0465

**Phase:** F

**Pull trigger:** R-0465: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The universal assumption-axis firewall survives while the enumerated legacy adapter wiring retires. surviving requirement: Every discharge route, including multi-kernel routes, must obey one verdict authority that preserves assumption debt. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** done for its enumerated wiring; the remaining universal-verdict-authority requirement merges into R-0494.

**Objective:** Extend the discharge-adapter firewall to the assumption axis, and bring the
multi-kernel path inside it.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0465).

### Task R-0460

**Phase:** E

**Pull trigger:** R-0460: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The sufficiency theorem survives while the old flat-goal lowering register and project VC generator retire. surviving requirement: Prove that each admitted new-Core safety obligation suffices for its runtime property, retaining named coverage rows and independent semantic differential controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; reconcile register with exact checked Core. The register count is five lowering rows, none fully discharged and four half discharged (stale "four rows" text corrected).

**Objective:** Discharge the obligation-sufficiency register — *if the flat goal holds,
the runtime property holds* — rule by rule.

[docs/verification/VC_BRIDGE_REGISTER.md](../verification/VC_BRIDGE_REGISTER.md) enumerates the `Core ->
obligation` lowering as four rows with the theorem each owes. Zero are discharged. This
is the ceiling on every runtime-safety claim and every multi-kernel badge: adding kernels
cannot detect a fault here, because all of them check the *same* lowered proposition, so
a mis-lowering yields unanimous agreement on the wrong formula.

Distinct from R-0455. That register asserts *transformed goal implies input goal*; this
one asserts *flat goal implies runtime property*. A transformation register cannot
discharge a row here, and conflating them loses the distinction the architecture rests
on.

Choose the first row on lemma shape rather than conclusion shape. `d /= 0` is the
simplest conclusion to state but its lemma must relate the flat fact to the interpreter's
division semantics, where truncation-versus-flooring and trap-on-zero live. Bounds has a
duller conclusion but a cleaner definedness step, and far more real instances — though it
carries an extra premise about `arraySizeMap` soundness, which is a fact about name
resolution rather than semantics. Sketch both before committing.

Exit criterion names rows, not a count: a bare "two rows discharged" is satisfiable by
two rewriting arguments while the non-arithmetic ceiling does not move at all.

**Priority argument, from the only fault this arc has actually caught.** R-0448 added two
independent kernels and, to date, kernel agreement has surfaced zero defects — every
disagreement observed was synthetic, injected by the gate's own mutation. The one real
fault the work found was `Z.div` vs `Z.quot` disagreeing at `(-7)/2`, and it was found by
`--report core-semantics-diff`: a *differential* test against an independent evaluator, not
by a second kernel. That is the expected result rather than bad luck. Adding kernels buys
independence on kernel implementation and kernel foundations — the axes where failure was
already least likely — and buys nothing on the bridge, where our own code is the thing
that can be wrong. So the differential surfaces (`core-semantics-diff`, `bridge-check`,
`lowering-agreement`) and the rows in this register are where fault-finding actually lives,
and they should outrank prover count N+1 when the two compete for the same week. Kernel
diversity is for the auditor who does not trust Lean; it is not a bug-finding strategy.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0255** (merged): duplicate of R-0460: merge into R-0460.
- **R-0261** (merged): duplicate of R-0496/R-0460: merge; already explicitly reconciled.
- **R-0464** (superseded): superseded by R-0493/R-0495/R-0460: merge; archive completed H24 fix and retire SSA-first prescription. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0464)

### Task R-0244

**Phase:** E

**Pull trigger:** R-0244: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; no separate resource analyzer.

**Objective:** Generate loop bound and variant obligations for bounded loops.
Where the obligation can expose a symbolic iteration bound, retain it as a
`complexity_guarded` fact for the same subject rather than reducing it to a
Boolean “bounded” label. This is a source-level complexity/DoS-resistance
claim, not wall-clock timing.

   Reuse R-0445's resource subjects, symbolic expressions, assumptions,
   completeness, and blocker codes. This task upgrades inferred bounds into
   obligations with replayable discharge; it does not fork their identifiers
   or introduce a parallel source cost semantics.

   Bounds are obligations and must discharge like them: loop-bound arithmetic
   routes to `omega`/`bv_decide` through R-0170, and cost-bound discharge is
   a named auto-discharge gate case — bounds that require hand-written Lean
   will not be written. Bounds compose over the known call graph: a caller's
   bound folds in its callees' bounds, and an indirect call folds the union
   of its possible target set (with recursion excluded by the predictable
   gate, composition is a DAG fold). Gate a bound that is too small and must
   be rejected with a witness, composition through direct and indirect
   calls, and a mutation that drops the bound check.

   Use a DNS message parser built from R-0444's combinator core as the forcing
   non-index-loop case. Compression pointers can jump backward and form cycles,
   so its `complexity_guarded` certificate must account for pointer-chasing and
   prove the accepted bound in terms of input length; an index-only argument or
   a watchdog is not discharge.

### Task R-0245

**Phase:** G

**Pull trigger:** R-0245: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; runtime checked arithmetic remains unconditional.

**Objective:** Define policy gates for `#[overflow_checked]`: release profiles may require overflow obligations for selected functions/packages, while ordinary examples remain quiet unless they opt in. Reports must distinguish `overflow_checked`, `overflow checking not requested`, and explicit wrapping or saturating arithmetic.

### Task R-0246

**Phase:** E

**Pull trigger:** R-0246: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; distinguish deliberate permitted traps from no-trap claims.

**Objective:** Generate obligations for panic/abort/assert-as-denial-of-service risks:

unchecked indexing, unwrap-like operations, explicit abort paths, failed
assertions, and profile-dependent panic behavior.

### Task R-0247

**Phase:** D

**Pull trigger:** R-0247: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; separate unrelated enum-discriminant bullet.

**Objective:** Generate byte/text/path boundary obligations: invalid UTF-8, lossy conversion, OS-string conversion, path normalization assumptions, and rejected implicit conversions.

- 7a. Implement explicit enum discriminant values for FFI/protocol enums
  (`enum Op { Get = 0x01, Set = 0x02 }`). These are currently REJECTED at
  parse time (E0001, "explicit enum discriminant values are not supported
  yet") because the parser previously parsed and silently DISCARDED them,
  assigning positional tags regardless — a semantically dark construct that
  would corrupt any FFI/serialization enum. Implementing means: honor the
  written value in the variant's tag/repr, reject duplicate discriminant
  values (a collision is a bug, not a silent alias — Rust rejects it),
  surface the chosen tag in the layout/ABI report, and tie it to the
  `repr(C)` boundary so a C consumer and a round-trip decoder agree. Until
  then the rejection stands (regression-locked by
  `tests/programs/error_enum_explicit_discriminant.con`). Add
  `examples/enum_discriminants/{protocol_ops,duplicate_rejected}/` and a
  gate proving values are honored at the repr boundary and duplicates are
  rejected.

### Task R-0249

**Phase:** F

**Pull trigger:** R-0249: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0178/R-0179: merge into R-0178/R-0179.

**Objective:** Report runtime-error obligations in human and JSON forms.

### Task R-0250

**Phase:** G

**Pull trigger:** R-0250: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0186: merge into R-0186.

**Objective:** Add policy gates that can require selected runtime-error obligations to be proved/enforced before graduation.

### Task R-0252

**Phase:** G

**Pull trigger:** R-0252: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add a runtime-error-obligation portfolio requirement: `secure_update_verifier` or
`protocol_state_machine` must demonstrate no OOB/div-zero/overflow under a named profile.

### Task R-0256

**Phase:** E

**Pull trigger:** New-compiler verification users demonstrate that annotations for supported arithmetic obligations block practical use.

**Objective:** Add automatic invariant inference / abstract interpretation as an annotation-reduction pass, not as trusted proof. V1 analysis is finite and auditable: interval facts, simple relational facts (`i <= n`, `i < len`, `0 <= i`), monotone loop counters, constant loop bounds, simple affine equalities/inequalities, and fixed-array length facts. It may synthesize candidate loop invariants and scoped facts for bounds, div/mod-zero, overflow, cast, and loop-variant obligations. Every inferred fact must be emitted in the obligation ledger with source span, analysis name, abstract domain, dependencies, and a replay command, then independently discharged

 by `omega`, `bv_decide`, or Lean before receiving
 `proved_by_kernel_decision` / `proved_by_lean`. Unchecked inference is not
 evidence. Add status detail `inferred_candidate` for facts proposed by the
 analysis but not yet checked. Add `examples/inferred_invariants/` with
 `array_sum_no_oob`, `copy_loop_bounds`, `ring_index_mod`, `overflow_counter`,
 and negative cases for non-affine updates, alias-sensitive updates, and
 widened bounds. Wire `scripts/tests/check_invariant_inference.sh`; the gate
 must prove inferred facts reduce required user annotations without creating
 false green obligations.

### Task R-0257

**Phase:** E

**Pull trigger:** R-0257: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Connect runtime-safety obligations to loop, frame, and dependency facts.

 Bounds/cast/overflow obligations should be dischargeable from explicit loop
 invariants, generated invariant candidates, and future `reads`/`writes`/
 `modifies` facts without duplicating the ledger. The report must show which
 invariant or frame fact discharged each obligation, and missing facts must
 produce actionable diagnostics rather than generic "unproven" output.

### Task R-0258

**Phase:** E

**Pull trigger:** R-0258: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; include mutation/re-establishment rules. Invariants must be re-established after writable access, moves through representations and foreign mutation, not only at construction; open/close rules and negative fixtures (with S8).

**Objective:** Add newtype/type invariants as scoped obligation hypotheses after they are checked at construction boundaries. This connects validated wrappers to the proof/runtime-safety pipeline: a type such as `#[invariant(self.0 > 0 && self.0 < 65536)] newtype Port = u16` must have the invariant proved/enforced at every constructor or `try_new` admission point, then injected as a named hypothesis for obligations in any function receiving a `Port`. No invariant may enter scope from a raw cast, trusted constructor, FFI boundary, stale proof, or unchecked assumption without a ledger entry saying so. Add `docs/NEWTYPE_INVARIANT_OBLIGATIONS.md`,

 `examples/newtype_invariants/{port,nonzero_len,bounded_index,ffi_trusted}/`,
 and `scripts/tests/check_newtype_invariant_obligations.sh`; the gate must
 prove constructor checks create reusable hypotheses, invalid constructors
 are rejected or return `Result`, raw/trusted paths do not silently inject
 facts, and obligation reports name the type invariant source.

### Task R-0452

**Phase:** E

**Pull trigger:** The new compiler discharges window-index obligations using frame facts and checked newtype hypotheses sufficiently to measure a zlib inflate port.

**Reconciliation (2026-10-19):** Keep as workload, with measurable acceptance.

**Objective:** Prove the obligation machinery survives obligation *density* on
code that was written for speed, by porting zlib's inflate path (2,365 lines:
`inflate.c`, `inftrees.c`, `inffast.c` plus headers, upstream `e3dc0a85`).
Deflate (3,687 lines) is a second stage, admitted only if inflate's obligation
story lands. Measurements and fit analysis are in
[research/workloads/port-candidates.md](../../research/workloads/port-candidates.md).

Why this phase and this program: a bit reader plus a 32 KB sliding window is
almost nothing but bounds, shift, and cast obligations, over the historically
most CVE-dense shape in systems code, with a bit-exact oracle available for any
corpus. The existing parser/security examples show that obligations *can* be
reported; this shows what the ledger and the discharge path do when a single
function carries dozens of them and the code was not written to be provable. It
executes after the invariant/frame connection (R-0257) and newtype hypotheses
(R-0258) because those are the facts the window and table indices need.

**Definition of done.** The port round-trips a pinned corpus bit-exactly against
zlib, as a regression gate, with malformed/truncated/adversarial streams as
negative fixtures and fuzz counterexamples persisted. Every bounds, cast, and
overflow obligation on the bit reader and window indexing carries a status from
R-0241's schema, and the phase report names how many reached
`proved_by_kernel_decision` versus `runtime_checked` — the ratio is the result
this task is measuring, and a corpus that only shows `runtime_checked` is a
finding, not a pass.

**Claim discipline.** `inffast.c` exists because of pointer-arithmetic fast
paths. Re-expressing them as index arithmetic under checked obligations is the
point of the exercise; admitting them as a trusted island passes the gate while
defeating its purpose, so any trusted island must be named in the audit bundle
with the reason it could not be indexed, and performance evidence for the port
belongs to the performance area's machinery rather than to a hand-timed claim here.

### Task R-0463

**Phase:** research

**Pull trigger:** A new-Core independent consumer cannot replay linear-arithmetic claims without the original prover installation.

**Reconciliation (2026-10-19):** Keep with explicit measurement/kill criteria.

**Objective:** Make the linear-arithmetic tier certificate-bearing, and reconsider whether
a second kernel is the right spend at that tier at all.

The tier that produces most claims — linear integer arithmetic via `omega` / `lia` /
`presburger` — is currently `trusted_modulo_toolchain`: no transferable certificate, a
`.vo` re-checkable only by the same Rocq version. That is also, not coincidentally, the
tier where multi-kernel evidence is deployed.

But linear arithmetic is the tier where certificates are *most* feasible. Rocq's
`micromega` already builds a Positivstellensatz/Farkas witness and validates it by
reflection against a checker with a soundness theorem; the witness is a first-class object,
not a byproduct. If it can be extracted and checked by a small independent checker, the
majority of claims move from `trusted_modulo_toolchain` to `replayed_certificate` — a far
larger trust gain than a fourth prover, and the thing that makes the auditor story real:
they run a few hundred lines of checker, not a multi-gigabyte Isabelle.

The strategic point this raises, which belongs in the record even if the extraction proves
impractical: **a certificate may change whether a second kernel is the best spend at that tier.**
That is a policy and measurement decision, not an automatic consequence: the certificate checker,
exporter, canonicalizer, and integer semantics can also be wrong, and independent reproduction may
still justify a second family. Multi-kernel evidence is currently concentrated where certificates
may serve better, and it is the
non-arithmetic tiers — where certification is measured to be impossible today (Alethe rejects
any datatype-bearing proof) — that actually need independent kernels. The allocation is
inverted. Probe the extraction before committing; kill criterion is a measured "the witness
cannot be got out of micromega in a stable form".

The probe binds the exact canonical VIR proposition and distinguishes integer
reasoning from rational/polynomial witness semantics. It records eligible
coverage, stable export, checker size, resource limits, and failure taxonomy
before any "majority of claims" or "small checker" estimate becomes a roadmap
fact.

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

### Task R-0261

**Phase:** E

**Pull trigger:** R-0261: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** duplicate of R-0496/R-0460: merge; already explicitly reconciled.

**Objective:** Discharge the soundness-bridge area semantic obligations through R-0496's normative Core semantics; do not introduce a separate subset interpreter. (Superseded objective, 2026-10-18: Build source semantics for the provable subset as needed by rule discharge, not as a speculative full-language semantics.)

### Task R-0263

**Phase:** C

**Pull trigger:** R-0263: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The dependency-order requirement survives while legacy V1–V7 migration and while_step-specific ordering do not. surviving requirement: Prove new-compiler construct semantics in dependency order, beginning with the constructs needed by file-copy and continuing through the decided language. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0511/R-0512: merge into V1–V7 slice ordering.

**Objective:** Prioritize the hard eval/source rules in dependency order: direct calls, structs/fields, enums/match, arrays, casts, arraySet, flat bounded `while`, and `while_step`.

### Task R-0264

**Phase:** F

**Pull trigger:** R-0264: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The objective requires report-state agreement, while the old report subsystem is deleted. surviving requirement: Prove that new-Core evidence verdicts agree with the actual proof and compiler state. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0494/R-0509: merge implementation into R-0494, soundness proof into S16.

**Objective:** Prove selected proof-report facts agree with compiler state: `proved`, `stale`, `blocked`, `missing`, `ineligible`, `trusted`.

### Task R-0265

**Phase:** F

**Pull trigger:** R-0265: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The trust-gate objective survives, but its FnTable machinery belongs to the old proof pipeline. surviving requirement: Validate deterministic subject binding, complete dependency accounting, contract freshness, proof attachment and eligibility on the new Core. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0495/R-0509: merge into statement/binding/aggregation soundness.

**Objective:** Prove or mechanically validate trust-gate correctness: body fingerprint determinism, spec-drift completeness, proof attachment lookup, FnTable completeness, eligibility classification.

### Task R-0267

**Phase:** E

**Pull trigger:** R-0267: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The body requires eligibility agreement, but names the old checker and report implementation. surviving requirement: Prove that proof eligibility reflects the new Core's authority and purity judgments without conflating capability freedom with purity. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0499/R-0507/R-0495: merge; capabilities and purity remain distinct.

**Objective:** Prove selected checker/report agreement for authority and purity facts used by proof eligibility, so a function cannot be called proof-eligible while secretly requiring capabilities.

### Task R-0268

**Phase:** -

**Pull trigger:** R-0268: no implementation pull; retained as the retirement record for legacy-compiler-final.

**Disposition:** retired with `legacy-compiler-final` on 2026-11-02 — The body asks whether the old source-proof pipeline needs a normalized Core layer, now superseded by the approved new-Core architecture.

**Objective:** Decide whether deeper source-semantics proofs require a normalized Core layer. Add the layer only if the direct rule proofs show repeated semantic duplication across at least two forcing examples.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0268).

### Task R-0269

**Phase:** E

**Pull trigger:** R-0269: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The body explicitly retains composition needs while retiring PExpr and FnTable architecture. surviving requirement: Generate model and contract dependencies in dependency order with complete call accounting so proof authors need no manual definition relocation. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0495: merge composition needs into R-0164; retire PExpr table architecture.

**Objective:** Automate dependency-ordered spec/table generation. Function specs and PExpr bodies should be collected/generated in dependency order together with FnTable completeness and call dependencies, so proof authors do not hand-relocate definitions above tables like `shaFns`.

### Task R-0270

**Phase:** E

**Pull trigger:** R-0270: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; distinguish user-spec totality from Phase 2 compiler totality.

**Objective:** Prove or mechanically validate spec/ghost totality reporting: every contract-referenced `spec fn` or ghost computation is either backed by Lean termination, accepted by a Concrete totality check, or rejected with a `totality_obligation_missing` status. A contract may not depend on a partial or non-terminating spec expression silently.

### Task R-0275

**Phase:** F

**Pull trigger:** R-0275: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The body specifies valuable independent-consumer duties alongside old Core and ProofCore boundaries. surviving requirement: Build an independent new-Core certificate consumer with a dependency firewall, precise predicate and trust statuses, and all binding, substitution, malformed-input and common-mode mutation controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; align with checked-Core derivations and R-0353.

**Objective:** Add an independent Core-certificate checker for a narrow, explicit validated-Core predicate.


 Define a versioned canonical `CoreCertificateV1` and a small separate
 `concrete-cert` library/executable target. Enforce a dependency firewall: it
 may import the canonical artifact/schema decoder, hash/id datatypes,
 **independent declarative specifications and shared data types**, and checker
 theorems; it may
 not import Parser, Check, Elab, Mono, Lower, Report, CLI, project loader, the incremental-driver area scheduler/cache, producer executable judgments such as
 `TypeJudgment`/Layout/fingerprint helpers, or producer verifier
 implementations. Publish its
 source/binary hash, import inventory, `partial`/unsafe/axiom inventory,
 rule-set version, and size so "small checker" remains measurable.

 V1 independently derives its verdict from the canonical Core artifact and
 small specifications. It checks at least: canonical/schema validity;
 artifact, subject, predecessor, and dependency-root binding; unique stable
 identities and referenced-node/provenance existence; constructor-local type
 consistency; direct-call arity and argument/result agreement; absence of
 unresolved placeholders/type variables where the Core boundary forbids
 them; direct-call capability containment; proof-target/body-fingerprint and
 obligation dependency binding; and the old phase 6B validation-chain shape.
 Proof-target/fingerprint/obligation/provenance checks establish identity and
 binding only; they do not establish extraction correctness, theorem/spec
 adequacy, or source correspondence. It
 must not merely compare a producer-emitted fact table with a second
 producer-emitted summary.

 Ownership/borrow/value-flow claims enter V1 only where the artifact carries
 enough independently checkable decisions and path edges to validate them.
 Checking that an ownership field exists is not ownership-checker soundness.
 Extend the predicate one rule family at a time with a mutation fixture. Give
 every predicate an explicit checker-proof status:
 `checker_trusted` for executable checking without a soundness theorem,
 `checker_soundness_kernel_proved` only when Lean proves a theorem such as
 `checkCoreCertificate a c = true -> CoreCertificateV1.Valid a c`, and
 `kernel_replayed` only for the artifact-specific theorem. An unproved checker
 may not appear under the same badge as a proved checker.

 Emit `certificate_structurally_checked`, `certificate_mismatch`, or
 `certificate_unsupported`; never `certified` or `proved_by_lean` merely
 because the structural checker passed. An artifact-specific preservation
 theorem actually replayed by Lean may separately emit `kernel_replayed`.
 State the remaining TCB precisely: V1 does **not** prove parsing,
 resolution, Check/source-to-Core correspondence, ownership/capability
 soundness beyond named local rules, spec adequacy, ProofCore extraction
 beyond discharged soundness-bridge-area rules, native-code behavior, or dependency
 completeness beyond the conservative the incremental-driver area envelope. Its own TCB still
 includes adequacy of the named predicate, canonical decoder, hash algorithm
 and collision assumption, checker implementation unless covered by the
 stated soundness theorem, Lean compiler/runtime and host execution when
 running a compiled checker, and every imported trusted/axiomatic primitive.
 `kernel_replayed` is reserved for an artifact-specific theorem actually
 checked by the kernel; a theorem about the checker does not make execution
 of the compiled checker kernel-only.

 Bind the checker to the incremental-driver area query DAG: an **independent consumer** may
 distrust cached canonical bytes and the producing compiler for the named V1
 predicate only after that consumer reruns the checker. A receipt rendered by
 the producer remains producer output until external replay. A verification
 receipt names artifact digest,
 dependency root, checker/rule-set version, checked and unsupported
 predicates, theorem/replay identity where any, and exact trust boundary.

 Gate altered expression types, wrong call signatures/results, missing
 capabilities, unresolved type variables, duplicate ids, missing provenance,
 stale dependency roots, artifact/certificate swapping, fabricated proof
 links, mismatched obligation subjects, malformed/noncanonical encodings,
 unsupported constructs mislabeled checked, and a producer record that
 agrees with itself about a false artifact fact. Break a producer judgment/
 layout/fingerprint helper and prove the checker still rejects the bad
 artifact without importing that helper. Wire
 `scripts/tests/check_core_certificates.sh` and a checker import-firewall gate.

### Task R-0276

**Phase:** -

**Pull trigger:** R-0276: no implementation pull; retained as the retirement record for legacy-compiler-final.

**Disposition:** retired with `legacy-compiler-final` on 2026-11-02 — The objective only reduces opacity in legacy ProofCore definitions and migration wrappers.

**Objective:** Reduce ProofCore partial-def opacity only where proof preservation needs it.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0276).

### Task R-0277

**Phase:** F

**Pull trigger:** R-0277: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The dashboard and verdict regressions survive, while shipped ProofCore constructs and legacy migration do not. surviving requirement: Publish cumulative new-Core soundness witnesses, rule statuses, replay identities, independent receipts, mutation results, cache parity and explicit remaining trust boundaries. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** superseded by R-0511/R-0512: merge into each slice’s cumulative acceptance.

**Objective:** Add the soundness-bridge area validation artifact: a compiler-soundness dashboard with one witness program per shipped ProofCore construct, one status per R-rule, replay commands for proved/mechanically-validated facts, and regressions proving report facts (`proved`, `stale`, `blocked`, `missing`, `ineligible`, `trusted`) agree with compiler state. Include the `CoreCertificateV1` predicate/rule-set version, checker binary/source hash, soundness theorem names, independent receipt per artifact, mutation corpus, cache-off/on receipt parity, and a machine-readable list of every boundary V1 still leaves producer/compiler-trusted.

### Task R-0454

**Phase:** F

**Pull trigger:** R-0454: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The body requires durable subject identity but bases its implementation and migration argument on legacy PExpr and SSA. surviving requirement: Define versioned canonical proof subjects with alpha-invariant identity, distinct call namespaces, exact attestation binding and stale-on-mismatch controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Merge implementation ownership into R-0450; retain identity invariants. Alpha-normalization alone does not make a proposition digest migration-neutral across SSA conversion; same-proposition criteria, version changes, compatibility mapping and negative controls are explicit (with R-0450/R-0512).

**Objective:** Encode obligations in a neutral term form and compute
`ProofSubjectDigest` over that neutral term rather than over any host AST.

**This belongs immediately after R-0004's remaining slices, and before the prover-neutral
evidence work merges.** The reason is a closing window, not importance: digests are
*stored*. Once artifacts carry a digest computed over a Lean AST, changing the basis
invalidates every stored claim simultaneously and forces migrating every fingerprint in
`examples/`. No `subjectDigest` field exists in `Concrete/` yet, so the migration cost is
currently zero and only grows.

It is R-0004's decoupling and is valuable even if no second prover ever ships: freshness
becomes host-independent by construction, so every prover agrees on staleness without any
of them seeing another's AST. It also closes a live defect — the multi-kernel fold matches
kernels on the obligation *id*, not on a subject digest, which is precisely the gap the
graduation criteria named.

Scope: the neutral term encoding (`PExpr`/`PVal`/`PMatchPat` are already prover-agnostic;
serialize versioned), the `NeutralObligation` and `HostAttestation` records, and
`checkedAgainstDigest` such that a mismatch reads `stale`. Three encoding invariants must
survive serialization: `.call` and `.applyVar` stay distinct (the two-namespace
resolution that closed bug 061); `displayName` is excluded from the digest because
identity is `CallableId`; and — **added 2026-07-31 from the R-0464 cross-check** —
**variable naming must not affect the digest**, so the term is alpha-normalized (de Bruijn
or a canonical renaming) before hashing.

That third invariant is this task's own closing-window argument turned on itself. R-0464
moves obligation generation from the surface AST to SSA, where the same obligation is
spelled over `i₁` instead of `i`. If names are in the digest basis, R-0464 invalidates
every stored digest — precisely the mass migration this task exists to avoid, arriving one
task later. Alpha-normalizing now makes the AST→SSA move digest-neutral and costs nothing
today. Getting this wrong is not detectable until R-0464 lands, which is exactly when it
is expensive.

Do not widen R-0004 to absorb this. That task is mid-flight; this is its successor.

Normative rule and record shapes in
[docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md](../verification/PROVER_NEUTRAL_OBLIGATIONS.md).

### Task R-0455

**Phase:** E

**Pull trigger:** R-0455: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The body retains Register B and rendering controls while motivating them through deleted report printers. surviving requirement: Prove typed goal transformations sound and retain non-vacuous per-obligation rendering validation with explicit separation of print fidelity from answer trust. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Merge typed-IR implementation into R-0450; keep Register B obligations. The paragraph proposing to delete the validation layer is historical and void; rendering validation remains required.

**Objective:** Replace per-prover string printing with a transformation pipeline, and
give each transformation a soundness register row.

The current lowering fuses three jobs into string templates plus an exit code: how to say
the obligation, how to try to prove it, and how to believe the answer. The defects follow
from that fusion.

`div`/`mod` sub-terms are dropped from the prover lowering (`exprToProver`,
`Concrete/Report/ReportObligations.lean:930`, "`div`/`mod` dropped as in `toLeanProp`" —
the comment states the fact without a reason). The operator table is infix-only, which is
why a prefix form has nowhere to go; note this is a limit of the *table*, not of the
targets, since `CoreExtract.lean:96` renders the same operators as prefix `Z.quot`/`Z.rem`
in the extraction path. The divisor-nonzero *obligation* is unaffected and does flow
through — only div/mod appearing inside a larger expression is lost.

Spec-function calls are dropped because there are no uninterpreted symbols.

The tactic is hardcoded **per driver**, so reaching a different one costs a cloned driver.
This branch measured that cost rather than predicting it: `rocqNiaLowering`
(`ReportObligations.lean:1277`) exists solely to reach `nia`, duplicating the whole
`ProverLowering` record to change one word in a proof script, and
`isabelleSmtReplayLowering` (`:1344`) is a third clone. Four drivers now differ mostly in
a binop column and a tactic name. That is the argument for `tactics` (ordered, budgeted)
being a driver *field* instead of a template literal.

Adopt the Why3 shape: a typed term IR (sorts including bitvectors, operators carrying
arity *and* fixity, binders, uninterpreted symbols) plus goal-to-goal transformations
selected by declarative per-prover drivers. A driver states what a target *cannot*
express so the pipeline transforms instead of silently dropping. Backends become
`print` / `tactics` (ordered, budgeted) / `attest` / `keep`, with a batch interface so a
session-building prover amortizes startup instead of paying it per goal.

Concrete's improvement over Why3, and the reason this is a register and not just a
refactor: Why3's transformations are trusted. Each pass here owes *transformed goal
implies input goal*, recorded per row with a fingerprint of the transformation it
justifies so a discharged row fails loudly rather than going vacuous when its subject
changes.

Difficulty is skewed and the register must record it: `eliminate_div_mod` is a rewriting
argument, while `eliminate_algebraic` requires the axiomatization be conservative over the
datatype theory, which is model-theoretic.

**REGISTER B COVERS TRANSFORMATIONS, NOT THE PRINT STEP — and that distinction is the
point of this task.** An earlier version of this section claimed a printer soundness row
"deletes the validation layer" and ranked four options linearly. Both were wrong, corrected
2026-08-03, and the correction changes what this task should promise.

Proving "this printer emits syntax denoting the same proposition" requires a formal
semantics of the TARGET syntax. There is none for SMT-LIB or Coq's parser, and neither is
ours; formalising them means trusting that our formalisation matches the real parser — the
same trust relocated. Why3 does not prove its printers either. So the honest split is:

| step | ours? | status |
|---|---|---|
| obligation → transformed goal | both sides | **provable** — this is Register B's real scope |
| transformed goal → target text | target is not | **validated, plausibly forever** |

That inverts how to treat the validation cost. It is not a temporary embarrassment to be
proved away; it is a permanent cost whose only lever is having ONE printer instead of N. A
standard interchange format is therefore not a convenience but the architecture: SMT-LIB
reaches z3, cvc5, veriT, Alt-Ergo and Yices from a single printer, so there is one thing to
validate rather than one per target.

**And the earlier ranking conflated two orthogonal axes.** Print fidelity and answer trust
remove different things from the trusted path — a verified printer feeding a trusted solver
still trusts the solver; a replayed certificate from a mis-rendered goal still proves the
wrong thing. The best position is both, not one ranked above the other. See the corrected
boundary model in
[docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md](../verification/PROVER_NEUTRAL_OBLIGATIONS.md).

What this task should therefore deliver, in order of value:

1. **A typed IR and Register B rows over the TRANSFORMATIONS** — genuinely provable,
   because both sides are ours, and far more tractable than Register A: syntactic claims
   relating two representations of the same term over a small fragment, not semantic claims
   about program execution.
2. **Collapse N printers to one standard-format printer** where the fragment allows,
   because the print step's validation cost is per-printer and permanent. `exprToSmt`
   already is that printer for the arithmetic tier, and its rendering is validated as of
   2026-08-03 (`smtAgreementGoals`, 46/46 on the families fixture, mutation-verified).
3. **Keep per-obligation print validation and make it non-vacuous**, rather than treating
   it as debt. Gate the instance count; a `0/0` "all agree" is the failure mode.

The consequence for kernel diversity, which belongs here because this is where the cost
lives: a native driver per kernel is another printer to validate forever. Kernels address
answer trust only, and only where certificates are impossible, while multiplying print
validation. That is the case against adding a fifth prover, stated as a cost rather than a
preference.

The spike built option 4 because it was reachable without an IR, which was the right call
for a spike and is not the destination. Recording the ranking here so the printer row is
scheduled as *deleting the validation layer* rather than as bookkeeping — and so a future
reader does not add a fifth prover under option 4 believing the marginal cost is a driver,
when it is a driver plus a validator plus a prover run per obligation.

### Task R-0262

**Phase:** -

**Pull trigger:** R-0262: no implementation pull; retained as the retirement record for legacy-compiler-final.

**Disposition:** retired with `legacy-compiler-final` on 2026-11-02 — The disposition retains only migration-pulled bridges for the old extraction pipeline, and legacy evidence is not migrated.

**Objective:** Upgrade extraction-only rules to three-view preservation where practical:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0262).

### Task R-0271

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The body merges implementation into the old S8a plan but expressly preserves instance-versus-generic proof meaning and law evidence. surviving requirement: Preserve proofs through monomorphization and capability instantiation while distinguishing instance-specific evidence from generic theorems and retaining explicit law-evidence acceptance. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0271: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by R-0510/S8a, V5: merge into R-0510, retaining explicit law-evidence acceptance.

**Objective:** Define proof preservation across monomorphization and capability-polymorphic instantiation. The compiler must report whether a theorem proves a specific generated instance (`proved_for_instance`) or a generic body (`proved_generic`), and it must prevent one instance proof from being presented as proof for every future instantiation.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0271).

### Task R-0443

**Phase:** F

**Pull trigger:** A new-compiler user requests an independently checked whole-program forbidden-capability or gateway policy.

**Reconciliation (2026-10-19):** Keep; separate structural graph proof from semantic containment.

**Objective:** Add independently checkable whole-program authority-property
certificates over a conservative closed-world call graph.

V1 proves a deliberately structural property, not effectful program semantics:
for named entry roots, no call path reaches a forbidden capability, or every
path that reaches it passes through an explicit allowlist of gateway functions.
The first forcing fixture is the shape “no path from `main` reaches `Network`
except through `send_report`.” This extends R-0184’s authority-budget verdict;
it does not replace functional ProofCore theorems and does not claim native
behavior, path-value properties such as “files only under `/etc`,” termination,
or correctness of an external effect.

The independently checked subject is the canonical validated/monomorphized Core
artifact plus the policy. The checker reconstructs stable function identities,
entry roots, direct edges, capability declarations, and conservative indirect
target sets instead of trusting a producer-emitted graph summary. V1 may use the
closed, auditable over-approximation “every address-taken function compatible
with the callable type” and refine it later; a false positive is acceptable,
an omitted possible target is not. An unresolved callable origin,
trusted/extern callback, dynamic ingress, unsupported identity, or incomplete
artifact becomes `certificate_unsupported` or an explicit trusted boundary,
never a clean theorem.

Bind the certificate to the artifact digest, dependency root, policy digest,
entry roots, target-set rule version, checked property, gateway identities, and
remaining boundaries. Before independent replay the producer may report only
`compiler_validated`. Reusing R-0275’s independent checker may emit
`certificate_structurally_checked` for the named graph predicate; use
`kernel_replayed` only if Lean actually checks an artifact-specific theorem.
Neither status proves source-to-Core correspondence or backend behavior.

Gate direct and indirect calls; runtime selection among two callable values; an
unreachable `Network` function; a permitted gateway; a second path bypassing the
gateway; nested and cross-module calls; monomorphized callees; `with(Std)` as a
declared ceiling distinct from reachable use; trusted/extern incompleteness; a
forged graph/certificate; and mutations that delete a direct edge, one indirect
target, a capability declaration, the artifact binding, or the gateway-bypass
path. The checker must reject each false-clean mutation without importing the
compiler’s authority-report implementation.

   Falsification probe first: run the algorithm by hand on kvstore and
   httpget. If the conservative address-taken approximation is so coarse
   that everything reaches everything, V1's design is wrong — refine it
   before building the certificate machinery.

### Task R-0456

**Phase:** research

**Restructured (2026-11-02):** retired with the legacy compiler: The disposition retires the PExpr port while explicitly allowing a measured Core-metatheory proposal. surviving requirement: Reconsider a second-host evaluator only through a measured Core-metatheory proposal with adversarial conformance vectors and explicit cross-host trust boundaries. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0456: when phase research becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** superseded by R-0496 for proposed PExpr port: retire PExpr-port design; reconsider only a measured Core-metatheory proposal.

**Reconciliation (2026-10-19):** The "no single kernel can bridge two kernels" claim conflates formal correspondence with trust in external executions; any renewed proposal targets Core.

**Objective:** Port `eval` to a second host and relate the two semantics by an
adversarial conformance vector suite.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0456).

## Area: Backend, Target, And Stdlib Contracts

Goal: make backend/toolchain/stdlib assumptions explicit, and state exactly
where source-level proof stops.

Done when: SSA, target/toolchain, optimization, ABI/layout, stdlib evidence,
and the incremental-driver area incremental build contracts are explicit enough for release
evidence; the supported SSA-to-BackendIR translation slice is independently
replayable; and every boundary after that slice remains explicitly trusted.

### Task R-0281

**Phase:** D

**Pull trigger:** The new compiler runs a paused application whose runtime trap cannot be diagnosed from source locations.

**Reconciliation (2026-10-19):** Keep; split V1b debugging from workload-triggered production features.

**Objective:** Add native compiled-program debugging and production-crash
diagnosis: DWARF/source-map emission, source-mapped backtraces for runtime
failures, debug-vs-release behavior, optimized-code caveats, and diagnostics
that distinguish source-level runtime checks from trusted backend/OS crashes.
Runtime traps inserted by Concrete (bounds, arithmetic, cast/profile checks,
failed runtime obligations) should carry enough source span and check-class
information that a real application can debug them without reverse-engineering
the abort site. This is not proof evidence; it is usability for
enforced/runtime-checked facts.

Trigger the production half when the first flagships-area flagship or external user
needs to diagnose a deployed binary. Add deterministic symbolication from a
release binary plus its exact debug/evidence bundle, stable build IDs, stripped
binary plus separate-symbol support, stack traces through Concrete/runtime/FFI
frames, core-dump or platform crash-report instructions, and a privacy-aware
redaction mode. A crash report must identify target/toolchain/backend/profile,
binary and source roots, trap/signal class, and whether each frame is exact,
inlined/optimized, runtime helper, foreign, or unavailable.

Gate a source-level trap, foreign crash, stripped release binary, mismatched
symbol bundle, optimized/inlined frame, and missing source checkout. The
mismatched bundle must fail loudly rather than symbolize against the wrong
program; no stack trace upgrades a trusted crash into checked or proved
evidence.

### Task R-0282

**Phase:** D

**Pull trigger:** R-0282: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The equivalence requirement survives, while its named backend artifacts must follow the new compiler. surviving requirement: Require clean and incremental builds to agree on diagnostics, evidence, native artifacts and behavior across target, profile and toolchain changes. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; replace old artifact names with actual IR chain.

**Objective:** Extend the incremental-driver area clean-build versus incremental-build equivalence across backend variants: facts, obligations, diagnostics, reports, BackendIR, codegen units/objects, link manifests, and native behavior must agree under target, ABI/layout, optimization, sanitizer, debug, and toolchain changes.

### Task R-0283

**Phase:** C

**Pull trigger:** R-0283: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; testing supports interop, proofs enter V6.

**Objective:** Add ABI/layout round-trip checks: C headers/stubs, offsets, size, alignment and
calling conventions, including the C-compatible adapters generated for Rust and Zig packages.
Execute native/Concrete round trips on every advertised target; type layout calculations alone
do not qualify a target or an aggregate calling shape.

**Semantics decisions (2026-10-21):** with R-0285, this task owns the foreign-name rule of
[FFI.md, "Symbol Names"](../platform/FFI.md) (every Concrete-defined function mangled into a
namespace disjoint from C; `#[link_name]` on `extern fn`) and the ownership-on-failure
classification for foreign-boundary APIs ([ERROR_CONVENTIONS](../stdlib/ERROR_CONVENTIONS.md)).

### Task R-0285

**Phase:** D

**Pull trigger:** The new compiler supports RocksDB open/read/write/close with linear handles, typed errors and explicit buffer cleanup.

**Reconciliation (2026-10-19):** Keep four-adapter exit and incremental delivery. Foreign builds run under an execution policy: filesystem, network, inherited environment/secrets, subprocess authority and undeclared generated inputs are controlled, with a hostile-build test (with R-0364). Dependency hashes establish identity, not safe execution.

**Objective:** Make C, Rust and Zig libraries usable as normal Concrete dependencies, with generated
bindings and one build command, before the bulk ethlambda port.

**Semantics decisions (2026-10-21):** every RocksDB slice-1 handle API states what happens to the
handle when it fails (returned, consumed or retained;
[ERROR_CONVENTIONS](../stdlib/ERROR_CONVENTIONS.md)), and slice 1 binds C symbols through
`extern fn` (with `#[link_name]` where the C name is not a Concrete identifier) under the
foreign-name rule of [FFI.md](../platform/FFI.md), owned with R-0283.

**Scheduling (2026-10-17; amended 2026-10-20):** promoted into the current execution queue after the
active soundness and R-0482 work. Its first deliverable is **RocksDB slice 1**, defined with its
acceptance and its prerequisite checklist under [feature prerequisites](../archive/ROADMAP_LEGACY_PLAN.md#feature-prerequisites): its
implementation and experiments proceed now, and it is accepted, and its safety advertised, when Phase 1
exits 0b, 2i and 2q hold, not when all of row 2 closes. This task owns the user workflow. R-0279 owns target/runtime facts;
R-0283/R-0287 own ABI checks, R-0286 owns exports/library artifacts and R-0289 owns sanitizer
validation. The later full BackendIR, independent verifier and functional-proof milestones are
not prerequisites for a tested foreign-library integration. Changes to evidence formats still
obey their existing owners and compatibility rules.

The outcome is a developer declaring a foreign dependency, using a normal Concrete import and
running the normal build/test commands. Binding selection and generation are automatic for the
supported interface; applications do not write bridge declarations or configure linker flags.
Tooling invokes Cargo, the C build/compiler or Zig's build/compiler, generates bindings, links
native artifacts and reuses unchanged outputs. Record exact dependency revisions/lockfiles,
selected features, bridge inputs, compiler/linker flags, target and artifact
identities. A changed header, bridge, feature or target invalidates the affected output. Failed
generation, unsupported ABI shapes and failed foreign builds produce source-located diagnostics;
they never reuse an older artifact as if the new build succeeded. A public registry is not
required: local binding packages with pinned dependencies establish the first workflow.

**First executable slice and feedback (2026-10-17; planned):**

1. Pin the RocksDB source, target/toolchain and selected C API. Retain the exact
   application/binding manifests, expected lockfile, ordinary imports and commands
   with R-0364/R-0365. Decide the exact-pin, override and native-link conflict
   behavior needed here. Draft workspace and Rust/Zig binding examples alongside
   it; those examples do not require implementing every adapter before this slice.
2. Implement the compiler-owned path from manifest parsing and dependency
   resolution through reviewed binding generation, foreign build/link and the
   runnable Concrete consumer. Exercise open/read/write/close and library-owned
   buffer cleanup through normal build/run/test. Keep the application executable
   throughout; include clean, unchanged warm and populated-cache offline rebuilds.
3. Retain controls for missing toolchains, changed archives, failed generation or
   foreign builds, unsupported targets and incompatible upgrades. Each failure
   names the relevant dependency/input and a valid next action; old generated or
   linked output cannot stand in for the failed new build.
4. Obtain a non-author trial as soon as this slice works. Record setup and manual
   intervention, maintained binding code, build/cache and copy/allocation costs,
   diagnostic quality and concrete friction. Turn each blocker into a minimal
   reproducer, owned fix and regression; use those results to adjust the shared
   implementation before adding Rust crypto, the selected upstream Zig operation
   and the bounded libp2p runtime adapter. Retain the same feedback for each slice.

Automate a fresh-consumer CI case outside the compiler checkout as soon as the
first slice runs. Fetch the exact candidate origin/revision and toolchain,
including fork-PR origins; use only declared dependencies and the installed
workflow. Compare actual results and expected failures, including resource
cleanup, rather than accepting a build or output marker alone. Record setup,
build and run timings on each claimed platform, with clean/warm/offline checks
as applicable. R-0352/R-0354 reuse this consumer for distribution acceptance.

Report the scope as each slice becomes usable. The first consumer supplies
engineering feedback; it does not close the four-adapter checkpoint or prove
foreign behavior. Broader package work grows from the observed requirements.

**Companion FFI and resource examples (2026-10-20; planned).** Four examples exercise linear handles
and exit 2i's rules (no reference stored in an aggregate, no block `let` shadowing a visible name) at
the foreign boundary, each with its own leg in `scripts/tests/check_ffi_companions.sh` (new, in the full
gate set):

- `examples/ffi_file_handle`: a linear file handle wrapping the C `open`/`read`/`write`/`close` calls,
  consumed by a `close` that returns its `Result`; a written file is read back, and a missing path
  returns a typed error.
- `examples/ffi_callback_context`: a small vendored C library that stores a callback and its context
  pointer and calls them later; the registration is a linear handle whose release unregisters it, and
  the context outlives the registration (the binding contract's callback retention).
- `examples/ffi_resource_pool`: a fixed-capacity pool of foreign handles checked out and returned as
  linear values; exhaustion returns a typed error, and shutdown releases every handle exactly once.
- `examples/ffi_error_release`: a multi-step acquisition (two foreign handles and a foreign buffer)
  in which every step can fail on request; every failure path releases exactly what was acquired.

Each leg compares the native output with a checked-in expected result, and the interpreter's where it
supports the foreign calls (otherwise the leg records that it is native-only); the foreign side counts
acquisitions and releases, and the leg asserts one release per handle on every path. Compile-fail
fixtures per example stay rejected: a handle not released on one error path, a handle used after
release, a reference stored in a handle aggregate, a block `let` shadowing a live handle, and a
context destroyed while its callback registration remains active. Their failure paths also run under
R-0030's [shared fault-injection checkpoint](#task-r-0030) (2026-10-21). The examples use today's explicit foreign
declarations and move to generated bindings when this task's workflow covers them. They are
companions accepted with or after RocksDB slice 1, not prerequisites of it; their 2i rejection
fixtures join when exit 2i lands.

**Failure and resource contract (2026-10-18; planned):** for each selected
fallible operation, state what success and each recoverable error mean for
ownership, partial progress, observable state and retry. An owned input is
returned with a stated resource state, retained by a named owner, or released;
ownership never disappears behind an error code. Borrowed mutable state has a
stated preservation or partial-update rule. Use
ordinary `Result`, records/enums, scoped borrows and explicit cleanup first.
An error does not imply rollback, and an external operation whose outcome is
unknown must say so rather than authorize a blind retry. Retain upstream
guarantees and reviewed assumptions with their provenance; generated wrappers
cannot manufacture atomicity or recovery guarantees absent from the foreign API.

In the first RocksDB slice, exercise a real recoverable error and the exact
ownership/release behavior of its results and handles. Where the selected API
supports partial progress or retry, retain an independent observable control;
otherwise state that limit and use the first applicable writer/runtime adapter
for that case. Gate successful use, controlled failure, exactly-once cleanup,
and safe resource recovery or explicit consumption. R-0488 reuses the contract
for ordinary stdlib operations. More expressive `Result`-case, `old` and frame
proofs belong to R-0473/R-0474 and the queued two-state contract work; they are
not entrance requirements for the tested consumer. Fatal abort, OOM and foreign
unwinding retain their existing separate policies. No automatic rollback,
scope-end destruction or general exception machinery is introduced.

**Annotation reuse and generated checks (2026-10-18; planned):**

- Reuse supported upstream bounds, nullability, no-escape and lifetime annotations
  before requesting new maintainer facts. Keep bounds and lifetime facts separate;
  map them only to relationships Concrete can express and check today. Upstream
  assertions retain their source and assumption status rather than becoming proofs.
- Store missing facts in the reusable binding package's external metadata, without
  editing upstream sources. Bind each reviewed fact to its selected declaration,
  signature, source revision, target/configuration and relevant input hashes.
  Conflicting annotations or overlays require an explicit maintainer resolution;
  a declaration name alone never identifies a reusable ownership contract.
- Generate only the selected public API and its required type/header closure.
  Feed extracted annotations and reviewed facts into the existing shared boundary
  description; derive native adapters, Concrete wrappers, docs and conformance
  harnesses together. Avoid importing an entire dependency's unsupported internals.
- Validate generated ABI claims against the actual foreign compiler and native
  calls, alongside Concrete callers. Retain independent behavior/error vectors and
  ownership rejection cases; generated tests do not prove the metadata they consume.
- Diagnostics identify the exact missing fact, such as whether an argument is
  retained or which function releases a returned buffer. Save the reviewed answer;
  ordinary application builds remain noninteractive. An upgrade requests review
  for affected facts and reports API/ownership/effect changes, while a stable
  signature alone does not establish unchanged foreign behavior.

Implement the metadata subset needed by the first RocksDB slice, then expand it
using that slice's feedback. A complete foreign-annotation vocabulary is not an
entrance gate. [FFI_GLUE.md](../platform/FFI_GLUE.md) records the Swift/D importer,
CXX/UniFFI generation and bindgen API-selection precedents and acceptance controls.

**Ease-of-use acceptance:** the application workflow is dependency -> ordinary import -> build.
Prefer a reviewed binding package automatically when one is available. For an uncovered library,
discover its C headers, supported Rust package/API metadata or selected Zig module metadata and
generate the supported native adapter, Concrete wrappers and boundary documentation together.
Use one versioned internal boundary description for generation, checks, docs and harnesses;
retain whether each fact was compiler-derived, checked or reviewed as an assumption. Validate
Rust/Zig adapters against the actual upstream package with its pinned compiler. An explicit
bridge interface remains an advanced escape hatch, not a mandatory handwritten step for every
application. Report the supported/unsupported API surface
and retain original documentation/source links for completion and diagnostics.

Ownership, effects or lifetime facts absent from an upstream interface are binding-maintainer
questions. Infer only established facts; generate specific requests or a draft description for
the missing ones, then retain the reviewed description for every downstream application. Name
patterns alone cannot establish pointer ownership or authorize a capability-free declaration.
Already reviewed facts are not requested again per application or build. Upgrade review is
limited to affected interfaces and facts; a changed declaration cannot silently inherit an old
ownership assumption. Never silently fall back to unsafe raw calls or serialize/copy every
buffer to make generation appear successful;
record required conversions and copies and support call-scoped borrowing where its contract is
known.

The non-author trial starts from a clean application and exercises initial setup, normal imports,
build/run/test, warm and populated-cache offline rebuilds, and a dependency upgrade. For the four
acceptance dependencies, require zero handwritten application FFI/bridge code, zero application
build scripts, zero edits to generated files and zero manual compiler/linker flags. Keep any
maintained custom adapter code counted in its binding package and in the port inventory.
Cached binding/native artifacts must match their pinned inputs;
an API/ABI change produces an actionable source-located error before it becomes a runtime call.

**Binding contract:**

- C headers supply declarations and target-specific layout facts. An explicit binding description
  supplies allocation/free pairs, owned versus call-scoped borrowed values, buffer lengths,
  retained pointers, errors, blocking/threading requirements and external authority. Unknown
  ownership or effects do not become safe, capability-free declarations through generation.
  The binding package records the supported upstream build recipe once when C package metadata
  is insufficient; applications do not maintain native build instructions.
- A narrow typed Rust bridge selects concrete functions/types and generates Rust C-ABI shims
  plus Concrete declarations from one interface. Check the shim against the real Rust API.
  Begin with scalars, records/enums, `Option`/`Result`, bytes/UTF-8 and opaque objects through
  explicitly defined ABI representations; arbitrary Rust ABI, generic APIs and trait objects
  do not cross the boundary directly. Unsupported forms refuse with an adapter diagnostic.
- Zig packages reuse existing C-compatible exports or generate exported adapters around
  selected public operations. Use supported compiler/comptime metadata with concrete
  instantiations; compile against the pinned module and resolve its `build.zig`/`build.zig.zon`
  graph. Lower slices, error unions, optional values and allocator-backed objects to explicit
  ABI representations; keep native allocator and internal layout details inside Zig. A public
  declaration alone does not establish safe ownership. No manual Zig glue is required in an
  application using a supported binding package.
- Generated safe APIs use Concrete's existing ownership and capability model:
  explicit consuming close/drop, scoped borrows, matching library deallocation and capability-
  bearing handles. Fallible cleanup retains an explicit result. Generation does not introduce
  implicit scope-end destruction, returned references or undeclared authority. Foreign behavior
  and ownership assertions remain named assumptions unless independently checked.
- Start the shared ABI subset with scalars, pointer/length buffers, opaque handles and
  status/out-parameter calls. Advertise aggregate forms only after target-specific round trips.
  Record Rust panic policy and Zig panic/trap/OOM behavior; ordinary errors become `Result`,
  while process aborts and unsupported unwinding do not become recoverable errors by generation.
- Networking adapters may keep their runtime in Rust or Zig and expose typed events and bounded
  queues with poll/wait, cancellation and shutdown. Specify full-queue behavior, thread affinity,
  callback retention and completion/cancellation lifetimes. This bounded bridge does not require
  or establish a general Concrete concurrency memory model.
- Generated headers, imported declarations, host stubs, symbol names, calling conventions,
  ownership rules, capability labels and trust assumptions round-trip through C/Rust/Zig harnesses
  and Concrete callers. Check ABI size/alignment/offsets and observable calls on each claimed
  target. Keep generated output deterministic and source-linked, with its reviewed inputs and
  native dependencies visible in build/audit metadata. Reference decision:
  [FFI glue](../platform/FFI_GLUE.md).
- Integrate transitive libraries and runtime/deployment requirements into the normal build graph.
  Cache the dependency/header closure, toolchains/sysroot, target/CPU, features, build mode,
  generator version, reviewed description and declared build inputs. Unchanged warm builds do
  no binding regeneration or foreign recompilation. Missing inputs cannot justify a reusable
  artifact; failed builds never fall back to stale linked output. Show upgrade diffs for APIs,
  effects, reviewed facts and copy/allocation behavior, with source-located errors.

**Acceptance workload:** use RocksDB's C API for open/read/write/close and buffer ownership; a real
operation from ethlambda's Rust crypto stack for typed input/results and errors; the smallest
libp2p adapter needed to exchange messages between a Concrete caller and a Rust peer; and one
pinned upstream Zig library for a real compression or hashing operation plus allocator-backed
output/cleanup. Select and record the Zig upstream/revision before claiming its gate; a local
arithmetic shim does not qualify. Retain clean, warm and offline builds, ABI/ownership and
malformed-input controls, queue saturation, cancellation and
shutdown controls, and applicable sanitizers. A non-author must build and run these through the
normal project workflow without hand-editing generated glue or invoking native build tools manually.
Measure setup steps, binding/adapter code maintained by hand, build cost and actionable diagnostic
quality. Register mutations for the authority, ownership and build invalidation rules the gates claim to
enforce. Include wrong-deallocator, use-after-close, retained-borrow, missing-capability,
native-build failure, stale-cache and incompatible-upgrade controls. Retain the selected boundary
format and supported API/target matrix with the acceptance record. This bounded exit unlocks
R-0490; it does not require wrapping every dependency in advance. Implementation remains pending.

**Clean-machine package workflow (2026-10-21; graduation criterion).** On a clean machine (a fresh
runner or container with only the documented installer), one scripted exercise installs the toolchain,
resolves the pinned dependencies, builds the native libraries the manifest declares (the `[native]`
section of `Concrete.toml`, in progress on branch `native-libs`), runs the tests, upgrades one
dependency, and rolls the upgrade back to the previous lock, with the build and tests passing at both
ends and the rolled-back lock reproducing the original artifacts. RocksDB slice 1's acceptance already
covers the install-to-test half (a fresh clone, normal commands, no manual flags); the upgrade and
roll-back half is required before the four-adapter checkpoint closes, and R-0365's lockfile exit
reuses the same exercise.

**Dependency build authority (2026-10-25; planned):** alongside each adapter's
existing build inputs and artifact identity, record which transitive dependency
steps execute code and their access to filesystem paths, environment variables,
credentials, subprocesses and network resources. Record names/scopes and access
policy, never secret values. Distinguish enforced restrictions from accepted
host/toolchain trust; declarative metadata and content hashes do not establish
confinement. Revisit this record when an upgrade changes the executable recipe.

Retain a controlled build-step fixture that would leave an observable marker if
executed. Semantic inspection must not fetch or build dependencies or trigger that
marker; explicit builds identify the executing dependency and its declared trust
boundary. Test any claimed restriction with a denied-access case and an allowed
control; otherwise report the access as trusted, not sandboxed. This is part of
the existing adapter records and first-slice feedback, not a new general-purpose
cross-platform sandbox entrance requirement. Stronger isolation remains an
explicit owner decision under the package/security work.

### Task R-0490

**Phase:** D

**Pull trigger:** The new compiler completes the bounded four-adapter interop checkpoint needed by ethlambda's first replaceable component.

**Reconciliation (2026-10-19):** Keep; distinguish bounded feedback from full-port completion. Functional proofs of serialization, state transitions, fork choice and signing are bounded later increments with their own owners and exits; the Silver shared-state workload is a separate deferred record under R-0415.

**Objective:** Port the complete client-owned ethlambda implementation to Concrete, get it running
and tested, and use concrete blockers to improve the language before proving client components.

**Status (2026-10-17; amended 2026-10-21): planned; follows the bounded R-0285 interop checkpoint.**
Until V1b's composed native exit holds, this task runs bounded component workloads inside the feature
track's single slot ([capacity allocation](../archive/ROADMAP_LEGACY_PLAN.md#capacity-allocation)); the bulk port starts after V1b, or
earlier only by a dated decision recorded when V1b is blocked on LCB's external milestones. The exit
below is unchanged. The initial
reference is [ethlambda at afc491b2](https://github.com/lambdaclass/ethlambda/tree/afc491b28d12de8c161fb5cd140b160ebf6d965d).
Pin its protocol/specification and fixture revisions before measuring parity. Advancing a pin is
an explicit compatibility change, with differences retained rather than absorbed into new goldens.

The port covers client-owned protocol and application logic: types/serialization, consensus state
transitions, fork choice, storage orchestration, networking orchestration, validator duties and
operational build/run/test tooling. Existing crypto, storage and transport/runtime dependencies may
remain behind R-0285 bindings. Inventory every retained native dependency and handwritten shim;
client consensus logic left in a foreign helper is unported work, not a completed Concrete component.
Reimplementing dependency internals is not part of initial parity.

Keep the node executable through successive replacements. Start with a Concrete component called
from the Rust client through R-0286, then move consensus processing and surrounding application
logic into Concrete. The existing Rust client supplies differential executions while pinned
protocol fixtures supply independent expected outcomes. For every completed slice retain accepted
and rejected inputs, state/root and error behavior, resource/ownership checks and appropriate
latency/allocation measurements. State precisely what happens to owned state after an error or
cancellation. Test bounded work and malformed peer inputs rather than accepting process aborts as
ordinary protocol rejection.

Each language/library gap found in the port gets a minimal reproducer, an owned fix and a permanent
regression. Preserve awkward-but-working cases in the friction record, including annotations,
callbacks/context passing, cleanup, diagnostics, compilation and debugging. R-0485/R-0488 own
related ergonomics and parameter-mutation work; the port supplies their forcing cases. Add SSZ,
bounded collections, narrow const generics or other language support only as actual cases require.
General concurrency remains research-gated; bounded foreign-runtime adapters have their own
explicit contracts. Record protocol parity, resource costs and open defects rather than treating
translated lines or a successful compilation as completion.

**Exit:** a Concrete node clean-builds and runs, syncs from peers, proposes, attests and finalizes
in a retained mixed-client devnet against the pinned reference. Preserve conformance/differential
results, restart and bounded-queue/failure scenarios, native dependency inventory, operational
instructions and measured resource costs. All remaining client-owned Rust logic and unsupported
scenarios are accounted for before calling the full port complete.

Full functional proofs are a subsequent increment, starting with serialization, state transitions,
fork-choice invariants and signing constraints once their models are supported. During the port,
ownership/capability enforcement, compiler regressions, protocol tests and honest evidence labels
remain mandatory. Existing verification release criteria and the three verified public flagships
are unchanged. Silver is a later, separate systems pressure test; its shared-state and thread
architecture does not expand this task's exit.

### Task R-0286

**Phase:** D

**Pull trigger:** The new compiler can compile an ethlambda component selected for invocation from its Rust host.

**Reconciliation (2026-10-19):** Keep; distinguish task “V1” from verified slice V1.

**Objective:** Add outbound FFI / embeddability as a first-class backend/ABI contract.

**Semantics decisions (2026-10-21):** the export attribute is the only way a Concrete-defined
function gets an unmangled C name ([FFI.md, "Symbol Names"](../platform/FFI.md)); every other
Concrete function is mangled.

 Concrete's most likely early adoption path is not replacing a whole
 process; it is exporting a small verified parser, protocol checker,
 constant-time helper, or evidence-bearing subsystem into an existing
 C/C++/Rust/Zig/Go host. Today the FFI story is mostly inbound
 (Concrete calling libc or trusted host functions). The roadmap must also
 cover Concrete code callable from outside.

 V1 deliverables:
 - `#[export]` or equivalent stable export annotation with explicit symbol
   naming / no-mangle policy;
 - `staticlib` and/or `sharedlib` output mode, with the supported target
   triples named;
 - generated C header for the exported ABI subset;
 - exported-function ABI classification: scalar, pointer, small/large
   aggregate, enum/result, string/bytes/path, ownership transfer, and
   unsupported forms;
 - generated boundary docs that expose capabilities, allocation authority,
   failure mode, trusted/Unsafe dependencies, and evidence class for each
   export;
 - evidence bundle next to the library artifact, so a host build can audit
   what the embedded Concrete component claims.

 Hard rule: exported functions must not hide Concrete's thesis at the C
 boundary. If an export allocates, needs a capability, can fail, depends on a
 trusted wrapper, or has only tested/runtime-checked evidence, the generated
 header/docs/report must say so. Gate: one C harness calls a pure exported
 parser/checker, one harness calls an exported fallible API, and one negative
 fixture proves an unsupported export shape fails with a diagnostic rather
 than silently choosing an ABI.

### Task R-0287

**Phase:** C

**Pull trigger:** R-0287: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep as R-0283’s concrete classification suite.

**Objective:** Add a C ABI classification suite, inspired by QBE's explicit ABI contract:

scalar parameters and returns, small and large structs, arrays, nested
aggregates, alignment/padding, by-value versus by-reference lowering,
varargs policy, calling convention labels, symbol names, and ownership/
capability/trust annotations. Wire `scripts/tests/check_c_abi_matrix.sh`
with generated C callers and Concrete callers, plus Rust/Zig adapters and callers for the
package interop subset. Keep target-specific positive and unsupported-shape controls.

### Task R-0289

**Phase:** C

**Pull trigger:** R-0289: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add sanitizer-backed generated-code validation for trusted/FFI/layout/ pointer-heavy examples, plus the stdlib sanitizer/runtime hooks from the proof-status area. Sanitizer findings are `runtime_checked` / `tested` evidence, not proof.

### Task R-0291

**Phase:** D

**Pull trigger:** The new compiler exposes a persistent compiler session used by editor or watch workflows.

**Reconciliation (2026-10-19):** Keep; distinct from generated-program performance.

**Objective:** Add compiler self-leak/resource soak harness for long-running workflows.

### Task R-0292

**Phase:** C

**Pull trigger:** R-0292: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; link R-0358 policy and R-0509 verification/audits.

**Objective:** Harden stdlib stability and evidence policy from the stdlib area: which stdlib functions are trusted, proved, enforced, allocation-free, capability-free, or assumption carriers.

### Task R-0293

**Phase:** C

**Pull trigger:** R-0293: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep practical contracts early; richer specifications later.

**Objective:** Define stdlib contracts for allocators, I/O handles, directory/file/path handles, byte/text/path conversion APIs, and fallible return discipline.

 Each public stdlib function must state allocation behavior, OS authority,
 failure mode, trusted platform assumptions, and evidence class. This item
 also owns the release-facing unsafe/trusted boundary UX: a safe wrapper may
 narrow authority, but reports must still show the underlying trusted,
 extern, raw-pointer, or `with(Unsafe)` operation and the assumption it
 depends on.

### Task R-0294

**Phase:** C

**Pull trigger:** R-0294: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add stdlib evidence gates so core helpers cannot silently widen authority, allocation, proof assumptions, or runtime-error obligations.

### Task R-0295

**Phase:** -

**Pull trigger:** R-0295: no implementation pull; retained as the retirement record for legacy-compiler-final.

**Disposition:** retired with `legacy-compiler-final` on 2026-11-02 — The body only reopens a legacy mid-level-IR architecture decision already superseded by the new MIR chain.

**Objective:** Evaluate a normalized mid-level IR only when traceability/backend-contract reports expose a concrete gap.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0295).

### Task R-0298

**Phase:** E

**Pull trigger:** R-0298: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; make assurance-profile selection explicit. Core claim validity is backend-independent; binary-transport assurance necessarily depends on the profile/backend.

**Objective:** Define backend-default policy explicitly if multiple production backends graduate: defaults are a versioned `(target, build_mode) -> backend` table, visible in help/audit/build receipts and overridable by an explicit flag. Changing a default requires clean-build/incremental equivalence, behavior and optimization matrices, performance data, a rollback switch, and release notes. Backend availability must never change source-language semantics, proof status, diagnostics before codegen, or package resolution.

### Task R-0299

**Phase:** E

**Pull trigger:** R-0299: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The emitted-byte and trust-boundary requirements survive, while legacy SSA and BackendIR probes do not. surviving requirement: Validate named new-compiler lowering and emitted-LLVM relations with independent semantics, actual helper closure, explicit incomplete outcomes and corruption controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; retarget old SSA/BackendIR language to S12.

**Pulled forward 2026-10-18:** this task joins R-0504 in stage S12 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), validating the LLVM profile; its ordering there takes precedence over the backend-contracts area placement below.

**Objective:** Add translation validation for codegen as the path out of a fully trusted backend. V1 should validate a narrow `ValidatedSSA -> BackendIR -> ValidatedBackendIR` subset per compile:

 integer arithmetic, fixed arrays, structs, direct calls, branches, bounded
 loops, runtime checks, capability calls, and source-map annotations. The
 validator compares checked Core / typed IR / SSA facts against BackendIR
 facts and reports one of: `translation_compiler_validated_v1`, `translation_trusted`,
 `translation_blocked`, or `translation_mismatch`. This is not a promise to
 prove the whole LLVM/native stack; it is a per-artifact check that the
 Concrete lowering into the backend contract preserves the facts Concrete
 claims. Add `examples/translation_validation/` with straight-line,
 branch/loop, struct/array, runtime-check, and deliberate mismatch fixtures.
 This is the roadmap's early backend-soundness probe: start with the small
 subset as soon as SSA/backend facts are stable enough, rather than waiting
 for a full native-backend story before learning whether Concrete's evidence
 can cross the backend boundary.
 Prior art: CompCert for proved compilation/pass-correctness discipline and
 Alive2 for SMT-checked LLVM IR equivalence/optimization validation. The
 concrete threat model here is the class differential fuzzing has already
 exposed: source/Core facts can be true while a backend lowering silently
 emits a different native artifact unless that boundary has its own check.
 Wire `scripts/tests/check_translation_validation.sh`; the gate must prove a
 Lean-proved/Core-level claim cannot be presented as native-code evidence
 unless the backend artifact is either translation-validated or explicitly
 backend-trusted in the audit bundle.

**Actual emitted LLVM increment (planned 2026-10-18):** extend validation beyond
the existing `ValidatedBackendIR` boundary with a separate, explicitly scoped
relation to the emitted `.ll` bytes. Follow the
[analysis and acceptance experiment](../../research/compiler/sparkle-emitted-code-validation.md#bounded-emitted-llvm-experiment):
retain checked SSA, generate LLVM normally, parse the actual output with a
separate reader and compare independent source/LLVM semantics for every input
in the admitted domain. The first case is checked `u8` increment: `10 -> 11`,
`254 -> 255`, and `255 -> required overflow failure`. A silent wrap to zero must
refuse. Start with integer/boolean functions and acyclic branches, and include
the real checked-arithmetic helper bodies and their required call closure;
checking helper names or the producer's pre-render AST alone is insufficient.

Retain controls for a corrupted helper, wrong width/signedness, changed branch
or return, missing failure edge, new LLVM poison/undefined behavior, malformed
output and unsupported instructions. Unsupported shapes, timeout and checker
errors remain distinct incomplete results and cannot grant a validated verdict.
Keep finite-domain checking, solver trust and kernel-checked proof separate.
Parser/reflection soundness and model-to-source correspondence remain explicit
assurance obligations. A direct probe of the current SSA emitter is permitted;
the full BackendIR migration is not a prerequisite for learning from it.
LLVM optimization and LLVM-to-machine-code correctness are separate edges.

### Task R-0300

**Phase:** F

**Pull trigger:** R-0300: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The body requires independent replay and precise edge statuses but binds them to the old artifact chain. surviving requirement: Independently replay versioned new-compiler translation relations with exact input/output binding, whole-function admission, call closure and substitution controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep distinct relation versions and call-closure accounting.

**Pulled forward 2026-10-18:** this task joins R-0504 in stage S12 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md), validating the LLVM profile; its ordering there takes precedence over the backend-contracts area placement below.

**Objective:** Make the backend-contracts area translation-validation slice independently replayable.

 Extend the soundness-bridge area `concrete-cert` target with a versioned canonical
 `BackendTranslationCertificateV1` over hash-bound
 `ValidatedSSA -> BackendIR -> ValidatedBackendIR` artifacts. The checker
 validates the explicit input/output relation; it does not trust
 producer-emitted `preserved` flags or compare two summaries produced by the
 same lowering pass. Its operator, trap, intrinsic/helper, ABI-layout, and
 target specifications must be independent declarative definitions; it may
 not reuse Lower/Emit/Layout decision helpers whose common-mode bug it is
 intended to catch.

 V1 covers integer width/signedness and operator selection, bools, direct
 calls and signatures, returns, branches, fixed-layout structs/arrays,
 required arithmetic/bounds/runtime traps, layout/target constants,
 source-map identities, helper/intrinsic calls, and capability/trust labels
 for the subset admitted by Task R-0299. V1 admission is whole-function: if an
 unsupported operation can influence control flow, memory, calls/arguments,
 or the return value, that entire function is `translation_trusted` or
 `translation_blocked`. Region-level validation is forbidden until explicit
 compositional boundaries and a composition theorem exist. Track call-closure
 separately: a caller may satisfy the local V1 lowering relation while a
 reachable callee remains backend-trusted, so that is not end-to-end validated
 function behavior.

 Bind the incremental-driver area codegen-unit manifests and object/link identities into the
 dependency root with explicit edge status. The independent V1 **relation**
 check stops at `ValidatedBackendIR`: `BackendIR -> object` and
 `object -> binary` are `identity_bound_only` / `backend_trusted`, not
 semantically checked. LLVM optimization, LLVM text-to-object
 translation, object writer, linker, ABI/runtime behavior, and native
 execution remain trusted/tested unless a later validator explicitly covers
 them. Object and binary hashes prove identity, not semantics.

 Mutate signed versus unsigned division, integer width, required overflow or
 bounds trap, branch target, field offset/layout, direct-call target/signature,
 capability/trust label, source-map id, target/profile, codegen-unit owner,
 object/link manifest, and certificate/build binding. Include an unsupported
 instruction mislabeled as validated. Independent replay must reject each
 checked-relation mutation without calling the producer backend. Also combine
 BackendIR A with object B and consistently rehash a fresh internal graph: in
 the absence of an externally expected root the checker must report an
 unvalidated backend edge, not claim semantic mismatch detection. Wire
 `scripts/tests/check_backend_translation_certificates.sh`.

**Emitted-LLVM replay increment (planned 2026-10-18):** R-0299's emitted-text
relation is distinct from the V1 relation above and must not reinterpret an old
certificate as covering output it never checked. Use the existing canonical
task/evidence objects with a separately versioned relation/profile. Bind the
exact checked input, emitted bytes, selected function and actual helper/callee
closure, target/arithmetic semantics, domain, checker/rules, proof artifacts and
assumptions. Independent replay parses the retained output and checks the
relation without calling producer Emit/Lower helpers. Gate a valid bundle,
wrong/stale output, wrong target/profile, a modified helper and a consistently
rehashed but semantically wrong bundle. Incomplete parsing or checking cannot
be hidden by a matching hash or a partial set of obligations.

Report the independently checked edge precisely: checked SSA/BackendIR to
emitted LLVM, followed separately by LLVM optimization, machine-code emission,
object/linking and runtime/ABI boundaries. LLVM transformation checking such as
Alive2 keeps its supported scope and solver trust; it does not become a Lean
proof or a Concrete-to-LLVM proof by relabeling. Object/binary identities remain
identity-only until their own semantic edge is checked. Extend hostile schema
fixtures and the R-0353 consumer when this profile graduates; do not freeze
producer-only bytes or mint stronger receipts during the development probe.

### Task R-0301

**Phase:** C

**Pull trigger:** R-0301: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The body defines bounded cumulative checkpoints rather than requiring the obsolete backend migration. surviving requirement: Retain cumulative ABI, sanitizer, debug, incremental-equivalence, substitution and stdlib controls, adding independent translation replay as its new-Core profiles graduate. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep as bounded checkpoints, not a monolithic phase gate.

**Objective:** Add the backend-contracts area validation artifact: a backend/std-lib contract project with ABI/layout C round trips, C/ABI glue generation/import checks, the C ABI classification matrix, alignment-fact fixtures if Task R-0284 is pulled into implementation, backend-IR emission/verifier checks, producer translation-validation checks, independent `BackendTranslationCertificateV1` replay and mutations, sanitizer runs, compiled-oracle differential tests, native debug/source-map smoke tests, clean-vs-incremental fact equivalence, cached-object substitution negatives, and stdlib

 authority/allocation/evidence gates. The artifact must show the exact point
 at which independent checking stops and backend/toolchain trust begins.
 Include R-0299/R-0300's actual emitted-LLVM fixture and independent replay once
 that profile graduates, with overflow-helper and output-substitution negatives;
 keep its coverage separate from the BackendIR V1 and native-code edges.

### Task R-0304

**Phase:** C

**Pull trigger:** R-0304: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep as shared acceptance constraint.

**Objective:** Keep inbound FFI, outbound FFI, and unsafe-profile work connected but distinct. Inbound FFI says how Concrete safely calls host code; outbound FFI says how host code safely calls Concrete exports; the unsafe/trusted-profile area says how the trusted/raw-pointer implementation islands remain auditable. Release-facing docs must not collapse these into a vague "FFI supported" claim. Each side needs its own examples, ABI restrictions, ownership-transfer rules, capability/evidence report, and red-team negatives.

### Task R-0284

**Phase:** after-1.0

**Pull trigger:** A new-compiler SIMD, DMA or C-ABI workload requires source-visible alignment beyond foundational layout alignment.

**Reconciliation (2026-10-19):** Keep surface extension gated; foundational alignment belongs to R-0502 now.

**Objective:** Design and, if pulled by a workload, implement first-class alignment facts.

Start from `research/language/ALIGNMENT_FACTS.md`: compare Zig, Rust, Odin, C, C++, and
SPARK/Ada; distinguish type/layout alignment from object/place alignment and
pointer/reference/slice alignment facts; decide whether Concrete uses
declaration attributes, fact/contract syntax, type-like pointer qualifiers, or
a staged mix. No implementation should start until a forcing workload exists
(SIMD/autovectorized slices, C ABI over-aligned buffers, freestanding DMA/MMIO,
allocator contracts, or an optimization/audit gap). If implemented, gates must
prove stronger alignment satisfies weaker requirements, offset/field/slice
projections conservatively weaken facts, runtime checked refinement records the
fact, unchecked assumptions are `unsafe`/trusted and reported, and backend IR
alignment metadata is emitted only from Concrete facts.

### Task R-0296

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The body retires Concrete's QBE graduation but explicitly transfers its acceptance requirements to LCB-native. surviving requirement: Graduate the LCB-native profile only with validated input, source maps, ABI and target matrices, incremental artifacts, and explicit validation-versus-backend-trust labels. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0296: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Ledger:** retired — owners R-0517, R-0505; see the [reconciliation record](ROADMAP_RECONCILIATION.md).

**Disposition (2026-10-19): retired — superseded by R-0517/R-0505.** There is no Concrete QBE backend to
graduate; the native profile is LCB-backed. Its graduation requirements (validated backend input, source maps,
ABI/target matrices, incremental artifacts, honest translation-validated versus backend-trusted labels) apply
to the LCB-native profile under R-0517/R-0505. Wasm, Cranelift and further backends stay deferred.

**Objective (historical):** Graduate the QBE backend-validation area QBE backend from experimental to release-supported only after evidence attachment, optimization policy, backend-IR verification, and backend trust boundaries are trustworthy. The QBE backend-validation area deliberately emits directly from validated/cleaned SSA to maximize independence and find bugs early; that is the initial usable backend architecture, not yet the release architecture.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0296).

### Task R-0297

**Phase:** E

**Restructured (2026-11-02):** retired with the legacy compiler: The body delegates machine IR and encoding to LCB while retaining Concrete's native-profile integration and assurance obligations. surviving requirement: Consume LCB machine-code generation through explicit input, instruction-semantics, calling-convention, relocation, debug-location and proof or validation contracts. This line governs over older implementation directions for the legacy compiler.

**Pull trigger:** R-0297: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Ledger:** delegated — owners R-0517, R-0505; see the [reconciliation record](ROADMAP_RECONCILIATION.md).

**Disposition (2026-10-19): delegated to the Lean Compiler Backend.** LCB owns machine IR, instruction
selection, register allocation and encoding with their proofs; Concrete consumes it under R-0517 and R-0505.
The Concrete-hosted comparison stays with R-0413. The text below is retained as historical rationale; the
"small owned backend investigation" it describes became the separate LCB project.

**Objective (historical):** Supply target-specific Machine IR for R-0505's native AArch64 profile. QBE is independent and is not a prerequisite; R-0505 owns scheduling and native proof/validation exits. The staged shape is `ValidatedBackendIR -> <target> MIR -> encoded object fragment`, with MIR owning instruction selection, physical-register constraints, calling- convention realization, relocations, and debug locations. The archived continuation records the historical reuse requirements.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0297).

### Task R-0303

**Phase:** research

**Pull trigger:** A compiler-in-Concrete workload identifies a specific missing tail-call, bit-layout, arena or typed-index feature.

**Reconciliation (2026-10-19):** Keep; repair incorrect “no proof interaction” assertion. Tail-call conversion interacts with deferred cleanup, live borrows, frame/resource relations and observations; 21a (TCO), 21b (bit-level layout) and SOA/`std.bigint` become tasks only when pulled.

**Objective:** Compiler-writing / verified-self-host language prerequisites (pull-gated research). Features a future proof-preserving self-host (CakeML-style, the soundness-bridge area) or a compiler-in-Concrete workload would need, surfaced by what makes ML/OCaml and Zig good compiler hosts. None is scheduled; each is pulled by a real workload and must preserve the linear / no-GC / visible-layout thesis — the OCaml GC path is deliberately not taken; this task's non-GC arena and typed-index subitems plus the layout path are.

 - 21a. Guaranteed tail-call optimization: a tail call lowers to a loop, so
   recursive tree-walks (the dominant compiler shape) run in constant stack. Today
   recursion is a stack-overflow risk the predictable profile avoids; guaranteed
   TCO makes tail recursion predictable-profile-safe rather than banned. Pure
   codegen guarantee — no ownership/capability/proof interaction. Gate: a deep
   tail-form fixture runs in O(1) stack; non-tail recursion is diagnosed, not
   silently stack-unsafe.
 - 21b. Bit-level data layout: arbitrary-width integers (`uN`/`iN`) and packed
   structs/enums with defined bit layout — compact IR nodes, discriminant/flag
   packing. Fits the visible-systems-layout thesis directly. Gate: a packed
   struct's size and field offsets equal the declared bit layout; an over-wide
   store into a `uN` field is rejected or wraps per the declared width, not
   silently. (SOA containers and `std.bigint` are stdlib-level, pulled the same
   way; nested patterns is recorded in `docs/spec/patterns.md`, "No nested patterns".)

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

### Task R-0313

**Phase:** C

**Pull trigger:** R-0313: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; obey prohibition on returned/stored safe references. Safe wrappers use scoped/downward construction or owned wrappers; no recipe returns or stores a safe reference.

**Objective:** Add safe-wrapper recipes for the common unsafe shapes: checked slice from raw pointer + length, bounds-checked indexed access, non-null construction, alignment-refined pointer construction, FFI-owned handle wrappers, allocator-backed buffers, and raw-place writes. Each recipe must show the public safe wrapper, the private trusted/Unsafe operation, the accepted assumption, and the report line that exposes it.

### Task R-0314

**Phase:** D

**Pull trigger:** The new compiler runs allocator-backed collection tests that can exercise double-free, leak and canary controls.

**Objective:** Add a debug allocator / checked allocator profile inspired by Zig's development allocators: allocation/free stack identity where available, double-free detection, use-after-free-like sentinel checks where applicable, leak reporting for tests, red-zone or canary checks where practical, and allocator-name reporting in audit output. This is `runtime_checked`, not proof evidence. It must work with Concrete's allocator-as-value direction:

`with(Alloc)` grants authority; allocator values name strategy/identity.
Include the Odin tracking-allocator lesson, but reject Odin's implicit
`context` as hidden authority: Concrete allocator identity is an explicit
value or report fact, and allocation permission remains visible as
`with(Alloc)`.

### Task R-0317

**Phase:** D

**Pull trigger:** The new compiler supports allocator values and explicit cleanup for arena and fixed-buffer examples.

**Objective:** Add allocator-strategy examples: global allocator, arena/bump allocator, fixed-buffer allocator, debug allocator, and test allocator. Every example must show allocation authority, allocator identity, cleanup path, and failure behavior. Do not add ambient allocation.

### Task R-0319

**Phase:** C

**Pull trigger:** R-0319: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; bind results to R-0498’s enforcement manifest.

**Objective:** Add FFI/trusted-boundary red-team tests: wrong length, stale pointer, null pointer, alignment mismatch, double free, missing capability, hidden allocation, and wrapper that forgets to report the underlying trusted call.

Safe code must fail to reach the operation; unsafe code must appear in
`concrete audit --trust-boundaries`.

### Task R-0315

**Phase:** D

**Pull trigger:** The new compiler admits a raw-pointer or trusted-slice workload needing checked runtime instrumentation.

**Reconciliation (2026-10-19):** Keep; cannot erase safe-Core checks without certificates. Opt-outs apply only to explicitly unsafe/trusted instrumentation; safe-Core checks are never erased by reporting alone (operation-specific certificates are required, S12).

**Objective:** Add pointer/slice runtime-instrumentation hooks for unsafe code: null checks, bounds checks for trusted slice views, alignment checks, provenance/trust labels, lifetime/owner debugging where the runtime can track it, and explicit opt-out for trusted performance paths. A checked profile may trap; a release profile may erase checks only when reports still say which checks became assumptions.

Include sanitizer opt-out attributes only as named unsafe/trusted facts
(`no_sanitize_address`-style behavior must appear in audit output), never as
comments or unreported compiler flags.

### Task R-0318

**Phase:** D

**Pull trigger:** The new compiler supports raw pointers, slices and allocation needed by a safe-versus-unsafe VM pressure test.

**Reconciliation (2026-10-19):** Keep as workload, not additional flagship or closure feature.

**Objective:** Add VM/interpreter-style pressure test as the validation workload:

a small bytecode stack, value array, call-frame/upvalue-like structure, or
mark/sweep-shaped toy heap that deliberately needs raw pointers/slices. The
workload should compare the safe-indexed version and the unsafe-pointer
version, then prove the unsafe version's assumptions are visible in reports
and checked in debug/profile gates.
The pressure test should deliberately exercise the cases from the Zig/Rust
unsafe-code debate: stack-top pointer movement, array/slice access, closure
or upvalue-like indirection, allocator-triggered collection/checking, and a
path where a safe-index/handle version is simpler but measurably different
from the unsafe pointer version. The output is not a performance claim; it is
an ergonomics/evidence stress test.

### Task R-0320

**Phase:** after-1.0

**Pull trigger:** New-compiler unsafe-profile gates exist and users measure avoidable difficulty writing or auditing pointer operations.

**Reconciliation (2026-10-19):** Keep gated by taxonomy and auditability.

**Objective:** Add syntax/ergonomics only after the taxonomy and gates exist. Candidate sugar may include pointer-field access or checked slice construction helpers, but it must not reintroduce removed `->` syntax as a general language form and must not blur raw pointers with safe references. Ergonomics serve audit, not the other way around.

Candidate ergonomics must be rejected if they make a raw pointer look like a
safe reference, hide a bounds/null/alignment assumption, or make an unchecked
operation harder to grep.

### Task R-0321

**Phase:** D

**Pull trigger:** The new compiler runs the unsafe VM workload in both normal and checked profiles.

**Objective:** Add the unsafe/trusted-profile area validation artifact:

 `examples/unsafe_concrete_vm/` plus
 `scripts/tests/check_unsafe_concrete.sh`. The gate must run the workload in
 normal and checked profiles, compare interpreter-vs-compiled behavior where
 applicable, prove audit output lists every trusted/Unsafe/allocator/FFI
 assumption, and include at least one mutation per unsafe class that the
 checked profile or audit gate catches.

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

### Task R-0417

**Phase:** C

**Pull trigger:** R-0417: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; source-cost bounds remain distinct.

**Objective:** Define performance evidence classes and claim grammar.

Use `measured_replayable`, `measured_nonreplayable`, `estimated`,
`complexity_guarded`, and `not_claimed`; never reuse `proved`, `enforced`, or
`tested_by_oracle` as a speed claim. A measurement states machine/OS, CPU
governor and load controls, toolchains, target, backend, optimization/profile,
input, repetitions, estimator, dispersion, outlier policy, and comparison
fairness. Complexity theorems or O(n²) ratchets describe growth, not wall-clock
performance. Add `docs/PERFORMANCE_EVIDENCE.md` and machine-readable schema.

`complexity_guarded` may carry compositional symbolic bounds on steps,
allocated bytes, or stack bytes produced by R-0244/R-0248 and the allocation
budget work. Such a claim names its source-level cost model and inputs and
never implies cycles or elapsed time. Start with the std container bounds
already forced by bug 048 plus one workload budget; do not build a parallel
cost-semantics task: R-0445 establishes the early schema and source model,
R-0244/R-0248 add discharge and target-budget enforcement, and this task
generalizes their evidence grammar without changing their meaning.

If a timing interpretation of `complexity_guarded` is ever entertained, run
the correlation experiment first: cost-model step counts vs. measured time on
the R-0416–R-0419 corpus (base64_cli, png_chunks, wordfreq). Poor correlation
is expected on memory-bound workloads and invalidates nothing about the
complexity claims; only a strong result licenses even speaking about time.

### Task R-0418

**Phase:** D

**Pull trigger:** The new compiler runs a stable benchmark corpus with pinned reference implementations and checked outputs.

**Objective:** Turn the flagships area corpus into a reproducible benchmark harness and
committed baseline ledger.

Add `concrete bench --manifest ... --json` or an equally stable script surface;
pin inputs and reference implementations; verify output/status before timing;
retain raw samples; separate compilation, link, startup, steady-state runtime,
and end-to-end measurements; and record cold/warm/cache state. Baselines are
reviewed artifacts with provenance, not an uncommitted `tests/perf` file. A
baseline update requires an explanatory diff and replay transcript.

### Task R-0419

**Phase:** D

**Pull trigger:** The new compiler's benchmark ledger contains repeated per-host control samples sufficient to calibrate noise.

**Objective:** Add statistically honest generated-code regression gates.

Calibrate per-host noise from repeated controls; use minimum effect sizes plus
confidence/dispersion rules rather than one fixed percentage; rerun suspected
regressions on an isolated runner; distinguish flaky infrastructure from a
confirmed regression; and retain both sample sets. Per-change CI runs a small
deterministic sentinel set, scheduled CI runs the full corpus, and release CI
replays the declared budgets. Inject CPU work, allocation, and code-size
mutations to prove the corresponding gates are load-bearing.

### Task R-0420

**Phase:** D

**Pull trigger:** The new compiler's runnable workload corpus identifies a hot path needing instruction, allocation or check-site accounting.

**Objective:** Measure codegen quality directly, not only elapsed time.

For selected hot paths record dynamic/static instruction counts where stable,
branches, calls, stack usage, allocation count/bytes, bounds/runtime-check
sites, helper calls, vectorization/inlining facts where observable, emitted IR
and object/binary size. Add structural budgets for accidental allocation,
duplicate checks, missed constant folding, helper-call explosions, and loop-body
growth. These are target/backend-specific measured or reported facts, never
source-semantics proof.

### Task R-0423

**Phase:** D

**Pull trigger:** The new compiler has comparable benchmark rows with pinned algorithms, checked outputs and retained raw samples.

**Reconciliation (2026-10-19):** Keep under R-0515/R-0355 checkpoint.

**Objective:** Publish fair comparative performance reports without turning
benchmarks into marketing badges.

Compare identical algorithms, validation, integer semantics, allocation/I/O
boundaries, optimization intent, and outputs; disclose handwritten/reference
advantages, excluded workloads, confidence intervals, and backend/toolchain
trust. Report wins, losses, and inconclusive results. Ban geometric-mean or
headline claims that cannot be reconstructed from published rows, and ban
cross-language comparisons that silently omit safety/runtime checks present in
only one implementation.

### Task R-0424

**Phase:** E

**Pull trigger:** The new compiler's LCB-native profile runs the same mixed performance corpus as its LLVM profile.

**Reconciliation (2026-10-19):** Keep; compares the LLVM and LCB-native (`verified`) profiles; no QBE rows.

**Objective:** Add the performance area validation artifact and graduate its facts
into release budgets.

Ship a pinned mixed corpus containing compute, allocation, parsing, collection,
I/O, and binary-size workloads; replay LLVM and LCB-native (`verified` profile) rows on named
hosts; demonstrate one caught runtime, allocation, code-size, and scaling
regression; emit the performance evidence ledger and raw samples; and feed the
same schema into Task R-0355 rather than creating a release-only benchmark
database. The capstone states explicitly which performance claims are measured,
estimated, complexity-only, or not made.

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

### Task R-0333

**Phase:** C

**Pull trigger:** R-0333: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; repair release-map inconsistencies.

**Objective:** Define first public release criteria: supported subset, required examples, required diagnostics, proof workflow, stdlib/project UX, evidence/policy/ tooling story.

Own the [release map](../../ROADMAP.md#release-map) and one bounded checklist/candidate record per
release scope. Assign existing task increments with explicit acceptance artifacts
and deferred remainders; do not create a second queue or require completion of all
phases. Record separate source/std/evidence compatibility and patch-release rules.
The systems preview does not satisfy or replace the verification alpha bar.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0110** (merged): merged — profile naming and evidence attribution join R-0075/R-0333; the profiles are `llvm` and the LCB-native `verified` profile. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0110)

### Task R-0334

**Phase:** C

**Pull trigger:** R-0334: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; share one tutorial with R-0346/R-0347.

**Objective:** Add the first-user teaching path as a release-bar artifact, not marketing copy. Deliverable: `docs/LEARN_CONCRETE.md` plus a checked tutorial transcript that covers install/build/run/test, `Copy` vs linear values, consume/destroy/handoff, capabilities in function headers, `Result` / ignored-result diagnostics, stdlib basics, one runtime trap, one audit report, and the five evidence classes. The tutorial must use runnable snippets covered by the doc-snippet gate and must not claim proof where the compiler reports only testing, runtime checking, assumption, or trust.

### Task R-0335

**Phase:** C

**Pull trigger:** R-0335: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; Phase 1 settles rules, each release publishes its subset.

**Objective:** Publish a versioned authoritative language reference before any public release claim. The tutorial/book is not the reference. The current normative material is scattered across grammar, value-flow, execution, invariants, and evidence docs; release users need one versioned document whose sections define the language surface, not merely explain it.


 Deliverable: `docs/LANGUAGE_REFERENCE_V0.md` (or generated equivalent)
 covering syntax, lexical forms, type/value model, Copy vs linear values,
 explicit destroy/defer, capabilities, references and borrow blocks,
 statements/expressions, modules/imports/visibility, generics/monomorphization,
 trusted/unsafe/raw pointers, runtime checks/traps, main/exit semantics,
 stdlib boundary expectations, and evidence taxonomy. Every section links to
 its machine-checked gate, diagnostic, or "not yet supported" rejection.
 Gate: `scripts/tests/check_language_reference.sh` verifies every construct
 named in the grammar has a reference section, every anti-feature/rejected
 construct has a diagnostic or policy citation, and README/site/book links
 point to this reference as the normative source.

### Task R-0336

**Phase:** C

**Pull trigger:** R-0336: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Publish a public claim matrix: what Concrete proves, enforces, reports, assumes, and trusts.

### Task R-0337

**Phase:** C

**Pull trigger:** R-0337: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add release claim freeze: README, `CLAIMS_TODAY.md`, roadmap, showcase manifest, and release bundles must agree.

### Task R-0338

**Phase:** F

**Pull trigger:** R-0338: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; minimal V1 versions precede final schema freeze.

**Objective:** Add compatibility policy for proof artifacts and fact schemas.

Own queue row 11’s receipt-v1 freeze decision: producer and independent consumer agree on canonical fixtures, the permanent schema attacks pass, and incompatible changes require a version bump.

### Task R-0339

**Phase:** E

**Pull trigger:** R-0339: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; distinguish display stability from semantic identity.

**Objective:** Add compatibility policy for generated contract/VC obligation IDs:

obligation IDs should stay stable across harmless formatting and local
refactors, and any unavoidable churn should be reported as artifact churn,
not hidden under ordinary proof drift.

### Task R-0340

**Phase:** C

**Pull trigger:** R-0340: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add public API compatibility checking before release:

`concrete api-diff old/ new/ --json` compares exported modules, function
signatures, capabilities, allocation behavior, evidence classes, stdlib API,
package interface artifacts, and generated docs. This is the Swift
API-digester lesson adapted to Concrete: API drift is a reportable fact,
not a release-note afterthought.

### Task R-0341

**Phase:** C

**Pull trigger:** R-0341: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; use typed policy rather than scalar evidence ranking.

**Objective:** Define release/showcase evidence policy by class:

`proved_by_lean` and `proved_by_kernel_decision` are strong evidence;
`tested_by_oracle` is supporting evidence; `proved_by_smt` /
`solver_trusted` require explicit policy approval; `open` and unreviewed
`assumed` are forbidden for release claims. The policy must be executable by
The audit-commands area verified-profile command, not just written in prose.

### Task R-0342

**Phase:** C

**Pull trigger:** R-0342: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add public examples policy: public-facing examples, website copy, README snippets,
paper examples and showcase manifests must not outclaim their proof status. The public flagship
portfolio is exactly HMAC-SHA256, `secure_update_verifier` and `protocol_state_machine`; active
candidates can be shown as active work, but cannot be presented as proved or graduated until their
bars land. Other fixtures/workloads may be documented without being marketed as additional
flagships.

### Task R-0343

**Phase:** C

**Pull trigger:** R-0343: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; current validity and historical replay must remain separate. Revocation rejects current policy acceptance while preserving accurately labelled historical replay (R-0430).

**Objective:** Add public security/soundness disclosure policy: compiler/proof pipeline bugs are security-relevant.

- 9a. Publish `SECURITY.md`: how to report a compiler/proof-system bug
  privately, response-time commitment, advisory format, and severity
  classes — a soundness bug that lets a false claim go green
  (checker hole, extraction bug, stale-evidence upgrade) is treated as
  highest severity even when nothing crashes. The policy must name scope:
  compiler crashes on valid input, silent miscompiles, verifier/proof
  unsoundness, stale-proof upgrades, hidden capability/allocation/trusted
  boundaries, package-evidence forgery, and stdlib/runtime safety holes are
  all security-relevant for Concrete. Advisories enumerate affected claims by
  reading the obligation ledger of affected releases, not by prose
  recollection. Gate: `scripts/tests/check_security_policy.sh` verifies
  `SECURITY.md` exists, is linked from README/site/release bundle docs, and
  names the supported versions/reporting channel before any public release
  target can pass.
- 9b. Define the proof-revocation procedure BEFORE the first release that
  carries proof claims: when a shipped theorem/evidence class is
  invalidated (proof bug, axiom-inventory violation, solver unsoundness,
  spec error), there must be a mechanical path that (1) republishes the
  release manifest with the affected facts downgraded to their honest
  class (`open` / `needs_recheck` / `trusted`), (2) emits an advisory
  listing every downgraded claim id, and (3) makes the old bundle
  fail verification loudly rather than silently coexist. Red-team gate:
  `scripts/tests/check_proof_revocation.sh` simulates revoking one
  flagship theorem (e.g. remove it from the axiom-inventory allowlist or
  break its fingerprint) and must show the release bundle flips to
  failing with the downgraded claim named — no false green on a revoked
  proof. Task R-0353's independent verifier must reject the old evidence
  root and any cached receipt that still names the revoked theorem.
- 9c. Treat upstream checker/kernel soundness advisories as Concrete security inputs. Attached
  `#[proof_by]` / `#[ensures_proof]` modules, generated proof objects and serialized checker input
  are adversarial even when they use an ordinary checked API and report no axioms. Axiom inventory
  does not detect an axiom-free kernel exploit. For each advisory, retain the exact reproducer,
  enumerate affected receipts/claims, exercise R-0208's version downgrade and R-0353's independent
  replay, and publish which evidence dimensions remain intact. The threat model must say plainly
  that Concrete cannot detect a kernel implementation bug from inside that same kernel; it can
  contain the consequence through version binding, revocation, replay and genuinely different
  checker families.

### Task R-0345

**Phase:** C

**Pull trigger:** R-0345: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep before relevant public claims.

**Objective:** Publish `THREAT_MODEL.md` and keep it linked from README, release bundles, showcase manifests, and assumptions docs.

### Task R-0346

**Phase:** C

**Pull trigger:** R-0346: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; reuse R-0285 consumer.

**Objective:** Add first-user workflow CI: install compiler, create/run one example, inspect one audit bundle without repo-local assumptions.

### Task R-0347

**Phase:** C

**Pull trigger:** R-0347: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep as tutorial content, not another onboarding project.

**Objective:** Improve onboarding, tutorial, and docs around `proved` / `enforced` / `reported` / `assumed` / `trusted`.

### Task R-0348

**Phase:** D

**Pull trigger:** R-0348: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** merged — the formatting decision is settled; residual documentation cleanup moves to R-0349.

**Objective:** Resolve string interpolation / formatting as an explicit release-facing decision, not an orphaned gap. V1 rejects string interpolation syntax, format macros, and trait-object-style `Display` because they hide allocation, dispatch, and formatting authority. The supported path is `std.fmt` over `std.io.Writer`, with any string-returning helper carrying `with(Alloc)` and any future convenience helper pulled by workloads.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0348).

### Task R-0349

**Phase:** C

**Pull trigger:** R-0349: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Refresh or archive stale gap/backlog documents before release. Documents such as `docs/language/LANGUAGE_GAPS.md`, root-level idea piles, and old stdlib reviews may be useful historically, but they cannot sit next to the roadmap making claims that contradict closed known holes or current semantics. Gate: `scripts/tests/check_gap_docs_current.sh` rejects stale "true blocker" / "largest gap" claims unless they map to an open roadmap item or known-hole id; otherwise the material moves to CHANGELOG/archive with a date and status.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0348** (merged): merged — the formatting decision is settled; residual documentation cleanup moves to R-0349. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0348)

### Task R-0351

**Phase:** G

**Pull trigger:** The new compiler has an external adopter selecting C, Rust or Zig components for migration.

**Objective:** Add migration/adoption playbook: what C/Rust/Zig code moves first, how to wrap libraries honestly, what stays outside Concrete.

### Task R-0352

**Phase:** C

**Pull trigger:** R-0352: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add release/install distribution matrix: host triples, checksums/signing, install paths, supported/deferred channels.

### Task R-0353

**Phase:** F

**Pull trigger:** R-0353: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The independent consumer contract survives, while legacy evidence migrations and old report-derived artifacts do not. surviving requirement: Build the independent hostile-bundle consumer on new-Core canonical tasks and receipts, retaining exact-artifact replay, authentication, revocation, exploit regressions and all substitution and gate-bypass controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; fix freeze ordering and profile-specific native edges.

**Linked 2026-10-18:** this task is part of stage S16 of the [verified-compiler pipeline](../compiler/VERIFIED_PIPELINE.md): it consumes R-0494's `ClaimJudgment` encoding and R-0495's generated statements without trusting stored verdicts, does not by itself establish source-to-model or native-code correspondence, and pins every 079 – 095 fixture's expected result vector. Codec and hostile-input prototypes may proceed beside queue rows 3 – 8. R-0353's complete cross-profile consumer remains at row 10, after row 9 and R-0004's prerequisites; a minimal independently checked retained-artifact consumer is required for every publicly authoritative V1 claim.

**Objective:** Define the canonical release-evidence DAG and offline independent bundle verifier before packaging or benchmarking it.

**Strategic role and dependency.** This is the standalone consumer for R-0004's stabilized
subjects, exact dependency roots and replay receipts. It is a separate product boundary, not an
extra R-0004 slice: R-0004 makes evidence production honest; R-0353 lets a stranger check the
result without trusting the Concrete compiler or its report pipeline. Codec, hostile-decoder and
root-recomputation prototypes may start earlier in shadow. R-0353's complete consumer becomes
authoritative only after R-0004's replay, migration and adversarial boundaries close. The minimal V1
consumer has its own explicitly qualified subset and cannot claim authority over unsupported legacy or
cross-profile evidence.

**Trust promise.** Given hostile bundle bytes, an externally supplied expected root or an accepted
signing key, and a verification policy, independently decide whether the bundle is internally
complete, dependency-closed, replay-bound, authenticated and policy-compliant. Do not compile
Concrete source, regenerate proofs or trust stored `proved` strings. The compiler proposes the
evidence graph; the verifier recomputes whether it is closed and properly bound.

**Small-verifier architecture.** Keep the deterministic core dependency-light and auditable:

- `concrete-cert-core`: strict versioned decoding, canonical encoding, domain-separated digest
  recomputation, identity uniqueness, graph closure/root recomputation, receipt validation,
  trust/assumption propagation and policy evaluation. No filesystem, network, subprocesses,
  compiler imports or ambient environment reads;
- `concrete-cert`: hostile file loading, CLI, expected-root/key inputs, resource limits, structured
  and human output, and kernel-adapter orchestration;
- kernel adapters: separately versioned Lean, Rocq, Isabelle and certificate-checker replay paths.
  Adapter success is evidence input, never authority to bypass core graph/policy checks.

The governing rule is **check retained producer outputs; never rerun a producer derivation and call
that independent verification**. Re-elaboration, VC generation, extraction, SAT solving or tactic
search may be useful diagnostics, but they do not substitute for checking the exact artifact that
minted the receipt. The verifier has three explicit layers:

1. **Envelope/accounting:** authenticate the expected root/signature; strictly decode canonical
   bytes; recompute identities, dependency closure and roots; validate receipts; propagate trust,
   assumptions and advisory state; apply policy. This proves internal binding and completeness only
   relative to the declared graph. It cannot discover an arbitrary omitted semantic dependency
   unless the bundle also carries an independently checkable completeness relation or an externally
   pinned expected root.
2. **Certificate profiles:** check the exact retained certificate against the exact retained
   canonical proposition. Every profile defines its supported fragment, proposition encoding,
   certificate encoding, checker implementation/version/rule set, checking relation, resource
   limits, deterministic failure taxonomy and evidence class. A checker must not invoke the solver
   or tactic that generated its certificate.
3. **Translation assurance:** validate and report source/Core-to-VIR correspondence evidence without
   claiming that the small verifier generally proves this semantic arrow. It may check future narrow
   translation certificates or finite-fragment relations, but otherwise reports the producer's
   named assurance (`producer_validated`, `sampled`, `exhaustive`, `formally_justified`, or
   `missing`). Layer 1 truth about a VIR proposition does not imply that the proposition faithfully
   describes the source program.

**Initial certificate profiles and honest probes:**

- Linear arithmetic follows R-0463: first determine whether stable micromega/Farkas or
  Positivstellensatz witnesses can be exported with enough semantic detail for an independent
  checker. Measure eligible obligations and distinguish integer reasoning from rational/polynomial
  witness semantics before estimating coverage or checker size. Failure to export a stable witness
  is the probe's named kill condition, not a reason to reconstruct the proof by rerunning tactics.
- Boolean/bit-vector evidence follows R-0451, but the authoritative chain is exact
  `VIR proposition -> CNF bytes/digest -> original retained DRAT/LRAT certificate -> checker result`.
  Re-solving captured CNF to manufacture a second certificate is corroboration, not replay of the
  certificate that supported the original claim.
- Nonlinear evidence remains `solver_trusted` while H21/R-0451 lacks a replayable proof object. The
  verifier names that boundary rather than converting solver success into certificate evidence.

Certificate replay does not automatically make a second kernel/checker worthless. Whether one
small checked certificate is sufficient is an explicit policy decision informed by checker
maturity, independent reproduction and adversarial results. Record independence along separate
axes: implementation/code lineage, algorithm and proof-system family, shared exporter/serializer,
shared libraries and shared specification. Different executable names are not independent evidence.

**Build sequence:** finish R-0004's exact correspondence and replay-receipt schema; freeze canonical
receipt/bundle bytes; ship Layer 0 accounting with hostile fixtures; run the R-0463 extraction probe;
bind exact CNF and retained certificates before the DRAT/LRAT checker; add kernel proof-object replay
profiles; mint R-0004 slice-7 receipts only for defined profiles; shadow-compare the verifier with
Concrete; complete R-0004/R-0353 adversarial exercises; then make the external verifier
authoritative behind expected-root/signature authentication and revocation policy.

From the first checker commit, each profile needs a valid positive certificate, malformed and
truncated inputs, a corrupted certificate, a valid certificate paired with the wrong goal, empty
material, duplicate/surplus material, resource-exhaustion limits, deterministic replay, and a
production-consumer mutation proving that bypassing the checker cannot retain a friendly verdict.

The hashed representation is a canonical binary format (deterministic CBOR or a deliberately
smaller specified codec); JSON is display/debug output only. Unknown critical fields and unknown
schema/rule-set versions refuse. Canonical decoding must reject duplicate keys, alternate integer
encodings, ambiguous ordering, trailing data and resource-exhaustion shapes.

**Lean kernel incident requirement (2026-07, issue #14576).** Treat proof kernels as versioned,
revocable dependencies rather than timeless oracles. The incident allowed the ordinary checked
Lean `addDecl` path to admit a malformed nested-inductive declaration and derive axiom-free
`False`; `--trust=0` and axiom printing did not detect it. The original artifact also passed an
outdated independent checker because that checker had a separate projection bug. Therefore:

- every replay receipt binds exact kernel implementation, commit/release, rule-set digest,
  exporter/serializer version and proof-artifact digest;
- policy maintains explicit accepted, vulnerable and revoked checker-version sets; a receipt from
  a newly revoked kernel becomes `needs_recheck` or rejected, never remains `proved` because it was
  green when minted;
- advisory status remains external validation-time input. A replay receipt binds immutable replay
  facts; a retained policy-decision attestation binds the advisory-registry and policy identities
  it evaluated. Neither artifact pretends a future advisory was known at minting;
- high-assurance policy requires current independent replay through structurally different
  implementations, not merely two invocations or two wrappers around shared kernel code;
- checker independence is recorded by implementation lineage and shared code/theory, not inferred
  from different executable names. A port of the affected kernel logic does not count as a fully
  independent defense against that defect class;
- the #14576 exploit, the independent-checker projection exploit, and every future upstream kernel
  soundness reproducer enter a permanent checker-arena corpus. Each accepted checker version must
  reject every applicable historical exploit before it can satisfy policy;
- successful replay is re-executed from the exact retained proof artifact in an isolated process.
  A receipt that merely states replay succeeded is insufficient;
- revocation enumerates every affected Concrete claim through receipt indexes and invalidates
  cached roots/receipts mechanically. Public advisories must name the downgraded claims.

Fragment-scoped revocation is an optimization, not the default. An affected checker version is
downgraded wholesale unless an independently checked feature footprint proves the vulnerable
mechanism absent from the retained environment. Surface VIR lacking the construct is insufficient:
an attached metaprogram may introduce a hostile declaration unrelated to the visible proposition,
and a footprint self-reported only by the affected checker cannot clear itself.

Do not respond by banning Lean metaprogramming. The elaborator is untrusted by design, and hostile
proof terms or serialized artifacts can bypass it. Defense belongs at the kernel/checker boundary,
in independent replay, version binding, exploit regression, revocation and multidimensional
policy.


 Move the common public/verifier codecs here: versioned canonical encoders
 for source/lock inputs, typed-Core artifacts and the soundness-bridge area receipts,
 obligations/proof bundles, SSA/BackendIR artifacts and the backend-contracts area receipts,
 codegen-unit/object/link manifests, binary identity, policy/profile,
 assumptions/trust facts, checker/toolchain identities, and proof replay
 payloads. Task R-0381 later wraps these in package interface/body formats;
 it does not first define encoders required by this release gate.

 Every edge declares its relation and status (`identity_bound_only`,
 `producer_validated`, or a named independently checked relation), and the
 graph schema names mandatory edges. The bundle exposes one computed evidence
 root, but self-consistency is not authenticity: verification requires an
 externally supplied pinned expected root or a policy-accepted signature/
 builder attestation over that root. A root/signature authenticates identity
 and provenance, not semantic correctness.

 Define `concrete-cert verify-bundle <bundle> --expected-root <root>` plus a
 `concrete verify-bundle` alias only if it invokes the same checker. It runs
 without repository/network state, treats bundle bytes as hostile, recomputes
 canonical nodes/edges/root, reruns the soundness-bridge and backend-contracts areas certificate predicates, and
 validates checker/rule-set/toolchain/target/profile/policy binding. Output
 separate dimensions: `internally_consistent`, `root_matches_expected`,
 `provenance_authenticated`, `core_certificate`, `backend_translation`,
 `proof_replay`, `policy`, `assumptions`, and `remaining_trust`—never one
 ambiguous `verified` badge.

 The minimum result vector includes `internally_consistent`, `root_matches_expected`,
 `signature_accepted`, `provenance_authenticated`, `dependencies_complete`, `kernel_replay_valid`,
 `checker_set_current`, `policy_satisfied`, `assumptions`, `trusted_boundaries` and
 `remaining_trust`. A bundle can be internally consistent yet describe the wrong project or rely
 on a revoked checker; those states must remain independently visible.

 The schema can reject a missing mandatory edge; the Merkle graph cannot
 discover an arbitrary omitted semantic dependency unless completeness is
 separately proved. Proof/Core and SSA/BackendIR substitutions may fail their
 independently checked relations. Object/binary substitutions fail only the
 expected-root/provenance binding; `BackendIR -> object -> binary` remains
 backend-trusted. A fresh internally consistent root over different native
 bytes is not a semantic mismatch.

 Specify a self-contained offline proof-replay payload and runtime: theorem
 sources or checked proof objects, generated workspace/manifest, exact Lean/
 ProofKit dependencies, kernel/tool versions and hashes, and licenses. If a
 platform bundle cannot provide that replay surface, report
 `proof_replay=unavailable`; it may not satisfy the beta/release bar for a
 claim advertised as offline kernel-replayable.

 Gate expected-root mismatch, accepted/bad signature, proof from build A with
 Core from B, SSA/BackendIR substitution, object/binary substitution,
 schema-required edge deletion, declared dependency-root mismatch, revoked
 proof, unsupported relation mislabeled checked, malformed cycles/duplicate
 ids, and missing checker/theorem identity. A byte mutation must produce a
 different root and fail against the pinned expected root (or lose accepted
 authentication), not necessarily fail internal-consistency parsing. Wire
 `scripts/tests/check_verify_bundle.sh`.

 Also gate omitted/duplicated/surplus dependencies; swapped subject/receipt/table material; stale
 theorem, implementation and checker identities; trust/assumption deletion; replay success without
 retained replay material; revoked checker versions; one current plus one vulnerable checker;
 purportedly independent adapters sharing the same implementation lineage; the Lean #14576
 reproducer; the historical independent-checker projection reproducer; canonical-encoding
 ambiguity; unknown critical fields; cyclic, oversized and deeply nested hostile bundles; and
 deterministic verification across clean machines and checkout paths.

**Authority progression:** `designed -> prototype -> negatively controlled -> mutation-killed ->
shadow-compared with Concrete -> independently reproduced -> authoritative consumer ->
adversarially closed`. No stage may be skipped, and internal consistency alone never satisfies the
release policy.

### Task R-0354

**Phase:** C

**Pull trigger:** R-0354: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; scope preview separately from replay-bearing releases.

**Objective:** Add concrete distribution UX:

 `concrete --version --json`, `scripts/release/build_dist.sh`,
 `dist/concrete-<version>-<target>.tar.gz`, `dist/SHA256SUMS`,
 `dist/SIGNATURES` if signing is enabled, `INSTALL.md`,
 `RELEASE_NOTES.md`, and uninstall/upgrade notes. Nix/Homebrew or similar
 channels may be added only if they can be kept reproducible and
 version-pinned. The archive must ship the independently built
 `concrete-cert` verifier, expose `concrete-cert --version --json`, record its
 source/binary/rule-set hashes separately from the producer compiler, and let
 `verify-bundle` run without repository state or network access. It must also
 ship Task R-0353's pinned proof-replay runtime/payload dependencies—or report
 `proof_replay=unavailable` and fail any release profile that requires offline
 kernel replay. Include every shipped Lean/ProofKit/runtime component in
 checksums, the supply-chain lock, and `THIRD_PARTY.md`.

### Task R-0356

**Phase:** C

**Pull trigger:** R-0356: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The body requires reproducible hashes but includes obsolete IR identities and legacy-format migration assumptions. surviving requirement: Produce reproducible new-compiler release manifests binding declared inputs, toolchains, artifacts and available evidence, preserving legacy replay solely from the frozen tag. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; preserve historical replay instead of blanket old-format rejection. Current issuance may reject old formats; retained historical verification stays supported through its versioned path (R-0512).

**Objective:** Add reproducible release artifact hashes:

 `concrete release --manifest --json` must record source tree hash,
 compiler commit/version, schema versions, target triple, build profile,
 dependency lock hash, stdlib hash, canonical query/artifact dependency root,
 the soundness-bridge and backend-contracts areas checker and rule-set identities, typed-Core/BackendIR/codegen-unit
 roots, emitted object/binary identity, release-bundle evidence root, and
 replay command. V1 does not support old release manifests; schema
 mismatches fail loudly with a regeneration command. Wire
 `scripts/tests/check_reproducible_release_hash.sh`; the gate must build the
 same release artifact twice from a clean tree and compare all hashes.

### Task R-0357

**Phase:** C

**Pull trigger:** R-0357: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add a toolchain/dependency supply-chain lock and license inventory before any release claim:

 - The Lean toolchain (`lean-toolchain`), every Lake dependency
   (`lake-manifest.json` pinned revisions), the SMT solver binary
   (name + version + hash, per the SMT replay metadata), and clang/LLVM
   versions are part of the evidence chain — a claim is only as
   trustworthy as the unpinned link. Task R-0356's release manifest must
   record all of them; drift between the lock and the build environment
   fails the release gate, mirroring the Task R-0216
   `proofs/lean-deps.lock` recheck trigger.
 - Publish `docs/THIRD_PARTY.md`: every shipped dependency (Lean
   toolchain, Lake packages, solver, runtime libs linked into emitted
   binaries) with license and role; release claims are blocked while any
   shipped dependency's license is unreviewed or incompatible.
 - Red-team gate: `scripts/tests/check_supply_chain_lock.sh` must fail
   when (a) `lake-manifest.json` and the recorded release manifest
   disagree on any pinned revision, (b) a dependency is present in the
   build but absent from `THIRD_PARTY.md`, or (c) the solver version
   recorded in replay metadata differs from the one in the lock.

### Task R-0358

**Phase:** C

**Pull trigger:** R-0358: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep before first public release.

**Objective:** Add the language/stdlib/artifact deprecation policy (moved up from the packages area — users of a released language need the policy BEFORE the release, not after): what stability the first release promises for syntax, stdlib API, proof/fact schemas, and obligation ids; how deprecations are announced (diagnostic with a replacement, minimum deprecation window measured in releases); and what may never break silently (anything that would flip an evidence class without `needs_recheck`). The editor area migration tooling (`concrete migrate`) implements this policy; the policy itself is a release-bar document. Gate: Task R-0340's `concrete api-diff` must classify every

 public-surface change as `compatible`, `deprecated(window)`, or
 `breaking`, and the release gate fails on `breaking` without a recorded
 policy exception.

### Task R-0359

**Phase:** C

**Pull trigger:** R-0359: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; bound checklist to implemented gates. Gates every implemented or activated requirement, with explicit dispositions for future tasks.

**Objective:** Add the external-contributor and public-platform surface before release.

 Write `CONTRIBUTING.md` as an operational checklist, not generic etiquette:
 how to add a language feature, which compiler modules usually change, which
 gates to run, which docs must be updated, how to add positive/negative/
 adversarial fixtures, and how to update
 `scripts/tests/example_manifest.txt` in the same commit as any new
 `examples/` `.con` file (this broke CI twice on 2026-06-09/10). Add
 `scripts/tests/check_contributing_contract.sh`; the gate must prove every
 example project has a manifest entry, every roadmap-required gate is named
 in either `Makefile` or CI, and contributor docs mention source changes,
 tests, docs, roadmap/changelog, claims, and release-bundle impacts. Publish
 a public platform-support statement (`docs/PLATFORM_SUPPORT.md`) naming
 Tier 1/Tier 2/unsupported hosts and targets, including Linux/macOS
 x86_64/aarch64 and Windows status. Windows must not remain ambient: either
 it is a named supported target with CI and hosted-capability semantics, or
 it is explicitly "not v1" with a pull trigger (external adopter, workload,
 or release requirement) and a list of blocked surfaces: paths, process,
 env, console, file locking, line endings, and toolchain/LLVM availability.
 Add performance-claim discipline: public copy may not imply "fast systems
 language" unless `concrete bench` and release performance gates publish
 replayable numbers; otherwise docs must say performance claims are not made
 yet.
**Governance (added 2026-10-19 via R-0515):** publish a lightweight governance document naming
language-design and release decision owners, the proposal/comment/decision process, how disagreements
and conflicts of interest are handled, and how maintainer authority is delegated or transferred. Retain
a public decision record for the first externally proposed change; record rejected proposals and their
rationale as well as accepted ones.

### Task R-0360

**Phase:** C

**Pull trigger:** R-0360: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; replace “all above” with scoped R-0333 candidate criteria. Ships against R-0333's bounded candidate checklist, not "all of the above".

**Objective:** Ship the first narrow public release only after the above are green.

### Task R-0361

**Phase:** C

**Pull trigger:** R-0361: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; historical origin does not make remainder complete.

**Objective:** [relocated from closed phase 4] Artifact and docs stability hardening:

 schema-version rejection gates (refuse to silently misread an artifact whose
 schema version differs from the compiler's), source-location privacy /
 redaction modes for emitted diagnostics and artifacts (historical phase 4 item 38), and
 docs-drift SEMANTIC checks beyond the artifact-existence gate
 (`check_docs_drift.sh`) — e.g. `Status:`/`Verified:` metadata and stale-claim
 marker detection. (Found not mechanically robust as a default during historical item 44
 work; they belong here, gated for the release bar.)

### Task R-0362

**Phase:** C

**Pull trigger:** R-0362: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The release gate survives, but its old artifact-chain checks must be rebuilt and scoped to supported claims. surviving requirement: Validate the scoped new-compiler release from clean installed inputs, retaining policy, replay, revocation and substitution checks whenever the candidate activates their evidence profiles. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; branch native versus LLVM assurance.

**Objective:** Add the release-bar area validation artifact:

 `scripts/tests/check_release_candidate.sh` installs the dist archive into a
 clean temp prefix on every supported host, runs `concrete --version --json`,
 builds one example, runs one proof/audit workflow, verifies checksums and
 signatures if enabled, checks `release/perf-baseline.json`, runs
 `concrete api-diff` against the previous public interface snapshot, and
 confirms the bundle contains the claim matrix, threat model, public
 examples policy, replay commands, schemas, assumptions/trust reports,
 Task R-0343's `SECURITY.md`, the supply-chain lock and `THIRD_PARTY.md` from
 Task R-0357, the deprecation policy and clean `api-diff` classification from
 Task R-0358, and
 the tutorial transcript from someone who did not build the compiler. It
 must also run Task R-0343's proof-revocation drill against the candidate
 bundle, verify the evidence DAG offline through Task R-0353 from the installed
 independent checker, and run tamper cases for source/Core/proof/BackendIR/
 object/binary/dependency-edge substitution. The report must distinguish
 Core/proof or SSA/BackendIR relation failures from object/binary failures
 against the expected root/authentication; native semantic edges remain
 explicitly backend-trusted. Release replay begins with a
 clean cache or `--incremental=off`; a local cache cannot be an undeclared
 release input.

### Task R-0344

**Phase:** G

**Pull trigger:** A public comparison of the new compiler with SPARK is proposed after checked frame and flow facts exist.

**Reconciliation (2026-10-19):** Keep conditional on claims and available frame/flow facts.

**Objective:** Add a certification-style assurance bundle profile before any SPARK-class comparison claim. The bundle must include the claim matrix, authority/capability report, obligation ledger, proof status, runtime-safety status, assumptions, trusted boundaries, package/dependency evidence, toolchain versions, replay commands, and any SPARK-class flow/frame facts.

 It must also include an agent-readable summary naming which annotations were
 checked and which were only suggested or future-only. This is a release
 artifact over existing facts, not a second evidence system.

### Task R-0350

**Phase:** G

**Pull trigger:** R-0350: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; derive claims from current matrix.

**Objective:** Add positioning page against Rust, Zig, Lean, SPARK/Ada, Austral, Hylo, Cogent, Dafny, F*, Why3.

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

### Task R-0426

**Phase:** C

**Pull trigger:** R-0426: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep readiness before 0.1; operate from 0.1.

**Objective:** Define the supported-version, maintenance, and end-of-life
policy.

Name stable/preview channels, support windows, which release lines receive
security versus correctness fixes, toolchain/platform support, evidence/schema
compatibility, and the notice required before end of life. Publish a
machine-readable support matrix consumed by installers, `doctor`, release
bundles, package tooling, and documentation. Unsupported must mean no claim, not
silent best effort.

### Task R-0427

**Phase:** C

**Pull trigger:** R-0427: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; rehearse before first supported release.

**Objective:** Make release rebuild, hotfix, rollback, and revocation procedures
replayable.

From the source tag, locks, builder/toolchain identities, and evidence root,
reproduce every supported artifact and compare declared hashes. Define signed
hotfix provenance, rollback selection, mirror/CDN correction, compromised-key
rotation, and how installers refuse a revoked release. Drill a bad binary, bad
metadata, failed rollout, and lost build host; retain timings and manual steps.

### Task R-0428

**Phase:** C

**Pull trigger:** R-0428: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; share policy with R-0343.

**Objective:** Operate a security and soundness response process.

Publish reporting channels, acknowledgement/triage/remediation targets,
embargo and coordinated-disclosure rules, severity criteria for compiler wrong
code, proof/evidence overclaim, stdlib memory safety, package compromise, and
toolchain/supply-chain defects, plus advisory/CVE policy where applicable. A
fix includes containment, minimized reproducer, affected-version matrix,
revocation/recheck consequences, class-level gate, disclosure, and backport or
explicit non-backport decision.

### Task R-0429

**Phase:** C

**Pull trigger:** R-0429: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; seed fixtures before second supported version.

**Objective:** Gate cross-version language, stdlib, package, and evidence
compatibility across the current and previous supported releases.

Test new compiler/old source, old compiler/newly declared source rejection,
old/new package interfaces, lockfiles, proof subjects, certificates, audit
bundles, diagnostics/API snapshots, and migration tooling according to the
published compatibility policy. Unknown future schemas fail loudly; migrations
preserve provenance and downgrade to `needs_recheck` whenever evidence cannot
be retained honestly.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0391** (merged): Duplicate of R-0429: merge into R-0429; intake through R-0431.

### Task R-0431

**Phase:** C

**Pull trigger:** R-0431: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep from first pilot, not only after release.

**Objective:** Turn every qualifying external failure into durable evidence.

Provide an intake/reduction template that records affected version/target,
minimal source/project, expected and observed behavior, failure boundary,
replay command, privacy/license permission, and disposition. Wrong code, crash,
hang, misleading evidence, compatibility break, diagnostic failure, and severe
performance regression each enter the appropriate permanent corpus or a
documented no-fixture state. Closure requires a class-level prevention gate and
links the advisory/issue, fix, regression, and released versions.

### Task R-0432

**Phase:** G

**Pull trigger:** The new compiler's first external pilot has consented installation, workflow and issue data to assess.

**Reconciliation (2026-10-19):** Keep; reuse pilot evidence and disclose sampling limits.

**Objective:** Measure post-release ecosystem health without hidden telemetry.

Use opt-in diagnostics bundles, explicit surveys, public issue data, CI from
consenting ecosystem projects, reproducible installation probes, and published
compatibility runs. Track install success, first-project completion, upgrade
friction, diagnostic usefulness, real-project compatibility, proof replay,
performance regressions, and time-to-resolution with collection bias and sample
size disclosed. The compiler sends nothing by default; absence of telemetry is
not evidence of success.

### Task R-0433

**Phase:** C

**Pull trigger:** R-0433: when phase C becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep bounded rehearsal before support promises.

**Objective:** Add the post-release area operational validation drill.

Rebuild a release from clean inputs; detect a substituted artifact; issue and
consume a signed revocation; produce a hotfix and rollback; migrate a previous-
version project/evidence bundle; process a simulated wrong-code or false-proof
report through containment, regression, advisory, backport, and release; test
offline behavior; and publish the postmortem with response-time and manual-step
budgets. The drill must distinguish practiced automation from policy that has
never been exercised.

### Task R-0430

**Phase:** F

**Pull trigger:** R-0430: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; make this distinction authoritative for R-0343.

**Objective:** Define package, dependency, proof, certificate, and claim
revocation operations.

Support package yanks without erasing history, compromised dependency notices,
evidence-root/proof/certificate revocation, replacement releases, offline
revocation snapshots, and audit output showing affected transitive dependents.
Replaying an old bundle must report the historical result and current revocation
status separately. Gate withheld-network/offline behavior, stale snapshots,
key compromise, malicious replacement, and recovery to a clean dependency.

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

### Task R-0363

**Phase:** G

**Pull trigger:** R-0363: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The package-consumer requirements survive, but old soundness-bridge artifacts and receipt identities must be rebuilt. surviving requirement: Define package artifacts over new-Core canonical authority bytes with dependency and evidence identities, explicit trust summaries and consumer tests excluding source-private side channels. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; JSON summaries must not replace canonical authority bytes.

**Objective:** Expand package artifacts only after reports, policies, assumptions, interface artifacts, and CI gates prove what packages must carry. The first package artifact refactor must define exact files:

`Concrete.package.json` (manifest summary), `Concrete.lock`,
`.concrete/interfaces/<module>.json`, `.concrete/facts/<module>.json`,
`.concrete/evidence/<module>.json`, `.concrete/certificates/<module>.json`,
optional `.concrete/verifier-inputs/<module>.car`, and
`.concrete/docs/<module>.json`.
Each artifact must name compiler version, schema version, package id,
source hash, interface/body hash, dependency/evidence root, the soundness-bridge and backend-contracts areas
checker/rule-set identities and receipts where applicable, authority budget,
assumptions, proof/evidence summaries, and replay commands. Add
`scripts/tests/check_package_artifacts.sh`; the gate must prove package
consumers read these artifacts rather than source-private side channels.
This phase is the main 20k+ LOC scale boundary: large Concrete programs need
package/workspace boundaries, import hygiene, visibility/API stability,
versioning, generated docs, and evidence summaries before the language can
claim to support multi-team codebases rather than only single-repo examples.

### Task R-0364

**Phase:** G

**Pull trigger:** R-0364: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep minimal exact-pin schema first. Build-time authority is enforced by R-0285's foreign-build execution policy, not only declared.

**Objective:** Design and parse package manifest.

Use one compiler-owned model for Concrete source packages and R-0285's reusable
C/Rust/Zig binding packages. Record upstream selections and locks, reviewed
boundary inputs, toolchain/sysroot, target/CPU/profile, generated binding and
native artifact identities, ordered transitive link inputs and runtime/system
libraries. Applications declare dependencies; the integrated driver selects
bindings and invokes supported foreign build tools without application scripts
or manual compiler/linker flags. Keep normal, test-only and build-tool edges
distinct; build tools target the host while linked libraries target the program.
Build-time tool authority and linked-library runtime authority are separate facts.
Imports resolve only declared dependencies against this same graph.

**Proof-authoring edges (planned 2026-10-18):** record heavy proof/tactic
dependencies separately from runtime, host build-tool and test edges within this
same model, following the [Sparkle sidecar lesson](../../research/compiler/sparkle-emitted-code-validation.md#additional-workflow-lessons).
Ordinary builds need not fetch or build unused proof-authoring packages. A build
requiring evidence must still validate its retained bundle or replay with the
required dependencies; absence cannot excuse missing/stale proof evidence.
Keep proof dependencies/toolchains in evidence roots even when they do not enter
the executable link graph. Implement this schema increment when a proof-bearing
package needs it; it does not delay the first exact-pin native dependency slice.

R-0285's first slice supplies exact application/binding manifest examples and
expected lock output; include workspace and C/Rust/Zig package cases. Label draft
fields until implemented, then keep the examples in executable fixtures so docs
and parser behavior move together. Expand the schema when a working slice needs
the next field, preserving the shared graph and identity contracts.

### Task R-0365

**Phase:** G

**Pull trigger:** R-0365: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; ranges follow specified conflict behavior. Before automatic fetching is supported: path traversal, symlink/hardlink escapes, decompression limits, interrupted acquisition and atomic installation into the dependency store each have a hostile fixture.

**Objective:** Add version constraints, dependency resolution, and lockfile.

Specify deterministic version selection and permitted duplicate versions before
admitting ranges. Cover diamond graphs, cycles, incompatible requirements,
root-only overrides and native link-identity conflicts; explain the introducing
dependency path and remedy. Lock exact source/header/binding/native input
identities, std/toolchain, target/profile and transitive link requirements.
Paths alone are insufficient: replacing an archive at the same path rejects a
locked build or requires an explicit refresh and dependent revalidation.

Planned `--locked` forbids lock changes, `--offline` forbids network access and
`--frozen` requires both. Explicit add/remove/update/lock commands own resolution
changes; upgrades preview R-0182's API, authority, assumptions and evidence facts.
Fetch and verify locked content before an offline build. A changed native input
invalidates dependent implementation evidence even when its public header stays
unchanged; identity/integrity never supplies behavioral trust or proof validity.
Use existing package/evidence roots and invalidation rules throughout.

Define how one workspace lock represents multiple host/target configurations and
conditional dependencies, so switching targets preserves unrelated selections.
Retain expected outcomes for duplicate-version/native-library conflicts and
root-only overrides. The first R-0285 slice can use exact pins; introduce ranges
only after their selection/compatibility rules and conflict fixtures are decided.

**Graduation criterion (2026-10-21):** R-0285's clean-machine package workflow exercise (install,
resolve pinned dependencies, build native libraries, test, upgrade one dependency, roll back to the
previous lock and reproduce the original artifacts) passes with this task's lockfile and commands.

### Task R-0366

**Phase:** G

**Pull trigger:** R-0366: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add workspace and multi-package support.

Use one root, shared lockfile, dependency graph and compiler session. Root-only
overrides and policy apply consistently to every member and command. Gate member
selection, local path edits and host/target separation with matching package
facts and native link inputs under build, test, check and report.

### Task R-0367

**Phase:** G

**Pull trigger:** R-0367: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add package-aware test selection.

### Task R-0368

**Phase:** G

**Pull trigger:** R-0368: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The public artifact contract survives promotion from the old incremental driver's internal split. surviving requirement: Public interface and private body artifacts must preserve dependency identities and expose private proof or assumption dependencies of public claims. This line governs over older implementation directions for the legacy compiler.

**Objective:** Split interface artifacts from body artifacts at package/workspace scale.

Promote the incremental-driver area internal split and stable ids into versioned public
package artifacts; do not expose or rename the opaque internal cache format
as a package protocol.
Interface artifacts expose public names, types, capabilities, contracts,
allocation/effect summaries, deprecation/version facts, and evidence classes.
Body artifacts contain implementation fingerprints, private obligations,
proof links, emitted IR hashes, and private diagnostics. A dependent package
may compile against interface artifacts without seeing private bodies, but
audit/release bundles must still show when a public claim depends on a
private body proof, trusted boundary, or assumption.

### Task R-0377

**Phase:** G

**Pull trigger:** R-0377: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; local docs before hosting.

**Objective:** Add API docs publishing for packages and stdlib:

 `concrete doc --format json`, `concrete doc --format html`,
 `docs/api/std/<version>/`, and package docs under
 `docs/api/packages/<name>/<version>/`. Generated docs must carry version,
 module path, capabilities, allocation behavior, evidence class,
 deprecation status, and source links where available. Published docs must
 be reproducible from the package artifact.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0378** (merged): Duplicate of R-0377 in export scope: merge export requirements into R-0377; retain hosting as deferred increment.

### Task R-0383

**Phase:** G

**Pull trigger:** R-0383: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep separate local-foundation and evidence-policy checkpoints.

**Objective:** Add the packages area validation artifact: a multi-package workspace project with dependency resolution, lockfile, package-aware tests, interface/body artifact split, dependency trust policy, assumption inheritance, authority budgets, provenance, independently checked dependency certificates, evidence-typed imports, published docs, downstream reuse after a dependency private-body edit, invalidation after a public-interface edit, and release-bundle evidence for every dependency. Include remote-cache hostile input/offline fallback if Task R-0382 is implemented. Wire it as `examples/package_workspace/` plus

 `scripts/tests/check_phase18_packages.sh`.

### Task R-0369

**Phase:** G

**Pull trigger:** R-0369: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep before package facts become authoritative.

**Objective:** Define the public package codecs and `PackageInterfaceCertificateV1` before consuming certificates.

 Canonically encode the package manifest, public interface/API payload,
 body/evidence root, optional verifier input, and certificate/receipt
 envelope. Keep `api_identity` separate from `implementation_evidence_root`:
 a private body edit may preserve exported names/types while invalidating
 capabilities/effects inferred from bodies, proof/trust/allocation/runtime
 summaries, mono/codegen facts, or other implementation evidence.

 `PackageInterfaceCertificateV1` checks package/schema/root binding and,
 where a canonical checked-Core/body verifier input is shipped, derives the
 supported exported facts from that hash-bound artifact plus the soundness-bridge and backend-contracts areas
 receipts. It may not validate a summary against a self-authored summary.
 When private verifier input is withheld, public API shape can remain
 structurally checked, but body-derived capability/trust/allocation/evidence
 summaries stay `compiler_validated` with the producer trusted unless backed
 by a separately replayable proof. The package report must expose that split.

 Gate canonical round trips, old schema, interface/body-root swapping,
 private-body edit with stable API identity but changed evidence root, and a
 coordinated tamper that changes interface summary + matching certificate
 while retaining the old body root. Wire
 `scripts/tests/check_package_interface_certificate_schema.sh`.

### Task R-0370

**Phase:** G

**Pull trigger:** R-0370: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; consume R-0494 judgments rather than new verdict logic.

**Objective:** Add proof-aware package artifacts: facts, obligations, proof status, trusted assumptions, policy declarations, package-boundary evidence summaries.

Required statuses: `proved_by_lean`, `proved_by_kernel_decision`,
`solver_trusted`, `tested_by_oracle`, `enforced`, `assumed`, `trusted`,
`partial`, `stale`, `vacuous`, `missing`, and `ineligible`. The artifact
must record whether evidence is package-local, inherited from a dependency,
or trusted through a boundary.

### Task R-0371

**Phase:** G

**Pull trigger:** R-0371: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Verify dependency interface certificates before consuming package facts or reusable artifacts.

 A content hash establishes identity, not semantic validity. Run Task R-0369's
 dedicated package-interface checker and, only over embedded canonical
 verifier inputs of the right type, the soundness-bridge area Core / applicable backend-contracts-area
 translation checker. Cache the independent receipt by package root +
 checker/rule-set version and bind it into the importing project's the incremental-driver area
 dependency graph. Invalid, stale, unsupported-schema, or unsupported-
 predicate certificates trigger a source rebuild when source and policy
 permit it, or a loud policy failure; they never silently become trusted
 facts. Source rebuild first produces `compiler_validated` facts and becomes
 independently checked only after the applicable checker reruns.

 Import authority/evidence constraints may consume independently checked
 interface facts or facts explicitly labeled `compiler_validated` with the
 producing compiler still trusted; they may not erase that distinction. Gate
 a valid package, tampered interface fact, certificate from another
 package/build, interface+certificate consistently changed against an old
 body root, stale private body with unchanged public API but changed evidence
 root, changed public interface, old checker version, withheld verifier input,
 unsupported predicate, proof revocation, and source-rebuild fallback. Wire
 `scripts/tests/check_package_certificates.sh`.

### Task R-0372

**Phase:** G

**Pull trigger:** R-0372: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; align with R-0440 typed policy.

**Objective:** Add module/package authority budgets after package graphs are real, and make imports fact-checked boundaries. Imports do not grant capabilities; they declare and constrain facts about the imported interface. The first concrete fact class is authority: support source-level or manifest-level constraints such as `import std.parse requires(no File, no Network, no Unsafe)` and package-wide budgets such as `allowed = ["Alloc"]`. A dependency capability widening must be a build/audit diff and, when constrained, a build failure until explicitly accepted. The design must leave room for the same import mechanism to constrain allocation (`no Alloc` / bounded allocation), trust

(`no trusted`, `no extern`, `no Unsafe`), runtime-failure profile, platform
(`hosted` / `freestanding` / `posix`), arithmetic-site policy, determinism,
constant-time / secret-flow claims, and supply-chain facts such as source
verification or license.
This is the package/dependency analogue of Elm ports and Elm package
discipline, adapted to Concrete rather than web apps: code may import a
dependency's values, but it does not inherit ambient authority, hidden host
effects, or private assumptions. The imported interface artifact is the
boundary contract. Roc's platform/host separation is also a useful warning:
host authority must be explicit at the boundary, never a package-local
convenience that disappears from reports.
Security threat model: if a dependency is compromised or upgraded to do more
than it used to do, the importer must be able to fail closed. Examples:
`import json as j requires(no File, no Network, no Unsafe, no trusted)`;
`import hmac.compute requires(proved_by_lean, no Unsafe)`; package-wide
`allowed_caps = ["Alloc"]`; or `requires(freestanding, deterministic,
license = "MIT OR Apache-2.0")`. The checked fact set—read through item
Task R-0371's independently-checked-or-explicitly-trusted boundary—must include at
least:
public capability set, allocation authority/profile, `trusted`/extern/FFI/
`Unsafe` use, assumption set, evidence class floor, proof staleness/vacuity,
runtime-failure policy, arithmetic-site classification, hosted/freestanding/platform facts,
dependency provenance/source hash, license status, and supply-chain lock
identity. A change in any constrained fact is not a warning hidden in prose:
it is either a build failure or an explicit audit diff requiring acceptance.
Write
`research/packages-tooling/import-fact-constraints.md`, then add
`docs/IMPORT_FACT_CONSTRAINTS.md`,
`examples/package_authority_imports/{rejects_network_widening,accepts_file_without_granting_file,rejects_unsafe_dependency,rejects_alloc_widening,rejects_hosted_dependency,rejects_trusted_widening,rejects_evidence_downgrade,rejects_new_assumption,rejects_license_drift}/`,
and `scripts/tests/check_import_fact_constraints.sh`; the gate must prove
imports read summaries from interface artifacts, do not grant authority to the
importer, compose with package-level budgets, and reject capability/allocation/
trust/platform/evidence/assumption/supply-chain fact drift until explicitly
accepted.
The first report view should be deliberately small and agent-readable:
`concrete audit dependency <pkg>` or the package-audit bundle must show
`public capabilities`, `allocation`, `trusted/Unsafe/extern`, `assumptions`,
`minimum evidence`, `platform`, `license`, and `source hash` in a stable
table. This report is the thing an AI assistant, reviewer, or CI policy reads
before accepting a dependency update.

### Task R-0373

**Phase:** G

**Pull trigger:** R-0373: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep as R-0440/R-0372 policy dimension.

**Objective:** Add dependency trust policy: trust widening across boundaries, review and inheritance.

### Task R-0374

**Phase:** G

**Pull trigger:** R-0374: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add package-level assumption inheritance: dependency assumptions must be visible to dependents and release bundles.

### Task R-0375

**Phase:** G

**Pull trigger:** R-0375: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; split local provenance from hosted publication.

**Objective:** Add package provenance and publishing model.

### Task R-0376

**Phase:** after-1.0

**Pull trigger:** Packages built by the new compiler are published externally and require a registry beyond the established provenance model.

**Reconciliation (2026-10-19):** Keep deferred; not beta prerequisite.

**Objective:** Add package registry server protocol and trust model.

### Task R-0378

**Phase:** after-1.0

**Pull trigger:** The new compiler has stable package artifacts and reproducible concrete doc output ready for hosted publication.

**Disposition (2026-10-19):** Duplicate of R-0377 in export scope: merge export requirements into R-0377; retain hosting as deferred increment.

**Objective:** Add package documentation hosting/export format only after `concrete doc` and package artifacts are stable: static HTML, JSON docs, versioned stdlib docs, package docs, release-note links, and `scripts/tests/check_docs_publish.sh` to prove generated docs are reproducible.

### Task R-0379

**Phase:** G

**Pull trigger:** R-0379: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep as R-0372’s evidence dimension under typed policy.

**Objective:** Design evidence-typed imports before the package fact schema freezes. A dependent package should be able to demand an evidence floor at the import boundary, for example `import hmac.compute requires(proved_by_lean)` or a manifest-level equivalent for all imports from a dependency. The compiler must check the requirement against the dependency's interface/evidence artifacts and fail the import if the evidence is missing, stale, vacuous, downgraded, inherited only through an unaccepted assumption, or weaker than the importing package's policy. This is a package-boundary trust-chain feature, not a local proof feature: it must define how evidence classes are

 ordered or deliberately non-ordered, how `solver_trusted`/assumed/trusted
 dependencies are named, how proof revocation invalidates dependents, and
 how release bundles explain the imported requirement. Write
 `research/packages/evidence-typed-imports.md` before implementing the
 surface, and keep it consistent with import authority constraints from
 Task R-0372 (e.g. `import hmac.compute requires(proved_by_lean, no Unsafe)`).
 Then add `docs/EVIDENCE_TYPED_IMPORTS.md`,
 `examples/package_evidence_imports/{requires_lean,allows_solver_trusted,rejects_stale,rejects_vacuous}/`,
 and `scripts/tests/check_evidence_typed_imports.sh`; the gate must prove
 dependency evidence is read from package artifacts, not source-private side
 channels, and that an evidence downgrade breaks the importing package.

### Task R-0380

**Phase:** G

**Pull trigger:** R-0380: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep after underlying facts exist.

**Objective:** Add package-level SPARK-class assurance summaries once frame/dependency contracts exist. Interface artifacts should expose public contract facts, read/write/modifies summaries, dependency-flow summaries, ghost/spec assumptions, capability requirements, trusted boundaries, and evidence class per public function. Package consumers and agents must be able to ask "what may this dependency read, write, depend on, assume, trust, or prove?" without inspecting private bodies.

### Task R-0381

**Phase:** G

**Pull trigger:** R-0381: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; reuse common encoders.

**Objective:** Extend content-addressing beyond proof fingerprints for package/evidence artifacts.


 Extend the incremental-driver area's opaque internal artifact roots into stable, versioned
 **public package** encodings rather than implementing a second hashing/cache
 system. Reuse the canonical typed-Core, obligation/proof, BackendIR,
 certificate, and release-node encoders already required by the proof-authoring, soundness-bridge and backend-contracts areas
 and Task R-0353. This item owns package interface/body/evidence/docs
 wrappers, public schema compatibility, and package-root composition—not the
 first encoding of common verifier inputs.

 The hard part is not hashing; it is deterministic canonical serialization.
 Before package hashes are authoritative, each package wrapper must define a
 stable, versioned, pointer-free canonical encoding: no map iteration order,
 temp paths, generated-name nondeterminism, host-dependent formatting, or
 schema-ambiguous fields. Borrow the Zig InternPool lesson here: artifacts
 that need stable identity must serialize from explicit IDs and normalized
 structure, not from incidental in-memory shape.

 Done when package interface/body/evidence/docs wrappers have versioned
 canonical encoders over the earlier common nodes; identical semantic package
 content hashes identically across repeated runs; a deliberately
 nondeterministic wrapper is caught by a gate; and package cache/dependency
 keys use these roots instead of source paths or human names.

### Task R-0382

**Phase:** after-1.0

**Pull trigger:** New-compiler local artifact reuse and independent package checking are mature and measured builds need remote reuse.

**Reconciliation (2026-10-19):** Keep; update native validation assumptions per profile.

**Objective:** Add optional shared/remote artifact reuse only after the local incremental-driver-area store and independent package verification are mature.

 Define an authenticated transport/protocol for content-addressed package,
 proof, and codegen artifacts; the server is an untrusted blob store, not a
 compiler authority. V1 permits only fetch-by-digest where that expected
 digest is already authorized by a lockfile, pinned release root, or
 policy-trusted builder attestation. An untrusted remote action-key-to-digest
 mapping is never an authority for semantic reuse. Every download must pass
 size/schema/digest and dependency-root checks, the applicable the soundness-bridge and backend-contracts areas
 certificate checker, target/profile/toolchain binding, and package policy
 before entering the local store.

 Remote object, link, and binary outputs may not enter a strict build merely
 because their hashes are internally consistent: the soundness-bridge and backend-contracts areas independent
 relation stops at BackendIR. Regenerate native outputs locally. A separately
 opt-in trusted-builder mode may accept signed native outputs only as
 `builder_attested` / `backend_trusted`; its signature authenticates producer
 provenance, not object semantics.
 Upload only deterministic canonical artifacts with explicit privacy/
 redaction policy; never upload source, diagnostics, proof attempts, or
 credentials implicitly. A signature authenticates provenance, not
 correctness.

 This item is optional for the first package release and must not complicate
 local builds. Gate hostile/corrupt server responses, replayed old roots,
 cross-target objects, revoked proofs, concurrent upload, offline fallback,
 and local recomputation parity. Include coordinated poisoning where a
 server rewrites the action mapping, artifact, manifest, and every internal
 hash consistently; it must still fail without an independently authorized
 expected digest. Record CI/team speedups, but keep correctness independent
 of network/cache availability.

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

### Task R-0384

**Phase:** F

**Pull trigger:** R-0384: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add artifact viewer integration for proof/evidence facts.

### Task R-0385

**Phase:** G

**Pull trigger:** R-0385: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The named CompilerSession and runFrontend machinery changes while the editor behavior and shared-session requirement survive. surviving requirement: The editor service must use the new compiler's shared incremental session for diagnostics, completion, signatures and references, with incomplete-source and stale-fact controls. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep V1a checkpoint.

**Objective:** Add compiler-as-service / LSP entrypoints after diagnostics and facts are structured. The service must host the incremental-driver area `CompilerSession`; it may not wrap batch `runFrontend` as its normal edit path.

The basic editor checkpoint also provides scope/type-aware name and member completion,
call signature help, and find-references for resolved symbols. These operations consume
the shared compiler session and respect visibility and current project revision.
Scripted incomplete-source and edit/repair cases verify useful results without stale
facts or changes to proof authority.

### Task R-0386

**Phase:** G

**Pull trigger:** R-0386: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Reuse the incremental query/certificate graph as the only editor fact engine.

 LSP/editor requests use the same typed query keys, dependency edges, local
 store, invalidation rules, validation records, independently checked package
 receipts, and evidence views as CLI/CI. Tag each response with project
 revision, subject/dependency root, completeness (`strict | partial`),
 freshness, checker/rule-set version where applicable, and cache/query
 provenance kept separate from evidence status. Cancelled or tolerant
 partial computations cannot enter complete caches or answer proof/release
 queries.

 Gate a scripted edit sequence covering comment/span-only, private body,
 public signature/capability/contract, target/profile, policy, proof, and
 dependency-certificate changes. Assert the bounded the incremental-driver area query execution
 set as well as CLI/LSP fact equality; an editor-only database, stale hover,
 or batch whole-project rebuild for a private leaf edit fails the gate.

### Task R-0387

**Phase:** G

**Pull trigger:** R-0387: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add hover/type info for capability status, proof status, predictable status, assumptions, obligations, and trusted boundaries.

### Task R-0388

**Phase:** G

**Pull trigger:** R-0388: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add obligation navigation: jump from source contract/index/mod/loop to the generated obligation and discharging theorem.

### Task R-0391

**Phase:** G

**Pull trigger:** R-0391: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Disposition (2026-10-19):** Duplicate of R-0429: merge into R-0429; intake through R-0431.

**Objective:** Add backwards-compatibility regression corpus once public users exist.

### Task R-0392

**Phase:** G

**Pull trigger:** R-0392: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep minimal diagnostics for early breaking rules; broader automation later.

**Objective:** Add migration/deprecation tooling after the policy exists (Task R-0358):

`concrete migrate --check`, `concrete migrate --apply`, diagnostics for
deprecated syntax/APIs, suggested replacements, edition/version notes where
needed, and mechanical rewrites only for transformations that preserve
evidence facts or explicitly mark them `needs_recheck`. The tool must read
`concrete api-diff --json`, `concrete doc --format json`, and the final
obligation ledger rather than scraping text.

### Task R-0393

**Phase:** G

**Pull trigger:** R-0393: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add API-docs/editor integration: LSP go-to-docs for stdlib/package APIs, evidence/capability/deprecation badges in hover, and diagnostic links to `concrete doc` output. The LSP payload must reuse the same doc/evidence JSON as the CLI.

### Task R-0395

**Phase:** F

**Pull trigger:** R-0395: when phase F becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The old Diagnostic and ObligationCore integration is replaced while machine-readable consumer requirements survive. surviving requirement: Structured diagnostics must expose obligation, proof, policy and reference-execution failures to editor and CI consumers using shared new-Core facts. This line governs over older implementation directions for the legacy compiler.

**Reconciliation (2026-10-19):** Keep; V1 editor is now the forcing consumer.

**Objective:** [historical origin: closed phase 4 item 11 tail] Route obligation / proof / policy facts into the structured `Diagnostic` record / `--diagnostics-json` channel so LSP/editor and CI-JSON consumers see them (array-bounds, solver-policy, vacuous-contract, stale-proof, …) — without duplicating the `ObligationCore`/report model. Includes interpreter structured diagnostics (historical phase 4 item 18a). Pull when a real consumer (LSP / CI JSON parser) needs machine-readable obligation diagnostics; see LANGUAGE_GAPS for the frontend-vs-obligation diagnostic split.

### Task R-0397

**Phase:** G

**Pull trigger:** R-0397: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Objective:** Add installed-binary feature discovery for agents and editor tooling.

  Deliverable: extend the old phase 6E `concrete help --json` / command catalog
  with `concrete agent features --json` only if agent-specific fields need a
  separate view. The catalog must describe supported
  commands, accepted inputs, emitted artifacts, schema versions, evidence
  classes, replay commands, policy gates, forbidden actions, and examples of
  valid next steps.

  Rule: this catalog is the source of truth that MCP, LSP, docs, and LLM
  prompts wrap; they must not duplicate a stale list of Concrete features.
  Done when golden JSON snapshots exist and one agent workflow discovers
  proof synthesis, replay, capability diffs, and counterexample saving from
  the installed binary alone.

### Task R-0398

**Phase:** G

**Pull trigger:** R-0398: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep minimal V1 checkpoint; defer advanced-feature exit.

**Objective:** Add the editor area validation artifact:

`scripts/tests/check_phase19_editor.sh` runs a scripted LSP/editor session
or golden transcript over one real project, proving hover, diagnostics,
obligation navigation, proof/evidence facts, dependency audit UI, refactor
behavior, docs integration, deprecation diagnostics, and playground output
match CLI facts rather than inventing a second truth source. It also checks
revision/freshness tags, cancellation safety, minimal incremental
recomputation, independently checked dependency receipt changes, and
cache-off/on equivalence.

### Task R-0389

**Phase:** after-1.0

**Pull trigger:** Users of the new compiler's minimal editor integration request refactors that preserve or update proofs.

**Reconciliation (2026-10-19):** Keep after minimal editor path.

**Objective:** Add refactor support that preserves or updates facts/proofs where possible.

### Task R-0390

**Phase:** G

**Pull trigger:** R-0390: when phase G becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep after package facts.

**Objective:** Add dependency audit UI for capability, allocation, FFI, trust, evidence, predictability, proof-obligation drift.

The UI must expose the same boundary facts as Task R-0372, not a prose
summary: public capabilities, allocation authority, trusted/Unsafe/extern
use, assumptions, evidence floor, platform, license, and source hash. This
is explicitly for humans and AI agents reviewing imports: an editor hover or
command palette action should answer "what can this dependency do, what does
it assume, and what evidence does it carry?" without reading private bodies.

### Task R-0394

**Phase:** after-1.0

**Pull trigger:** Onboarding evidence for the new compiler's stable release subset shows a browser or local web runner would reduce first-use friction.

**Reconciliation (2026-10-19):** Keep optional.

**Objective:** Add a playground or local web runner only after the release subset is stable: `concrete playground --local` or a static hosted runner with preloaded examples, no hidden claims, visible evidence class, audit output, and clear sandbox/timeout/resource assumptions. This is a teaching surface, not a second compiler pipeline.

### Task R-0396

**Phase:** E

**Pull trigger:** R-0396: when phase E becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep after facts exist.

**Objective:** Add editor and agent diagnostics for SPARK-class assurance facts after the facts exist: failed loop invariants, weak variants, missing frame facts, over-broad `writes`, unsatisfied `depends`, ghost/spec partiality, package evidence downgrades, and runtime-safety obligations that need a frame or invariant. The LSP/JSON payload must reuse the obligation/evidence ledger and point agents to the validation command; no editor-only proof status.

 Also surface development-only expectations from Task R-0240 as
 `dev_checked` / `tested` facts, never as proof. Editor and agent prompts
 should suggest "promote this to a contract/obligation" when release or
 high-integrity policy requires stronger evidence.

## Area: Concurrency And Research-Gated Extensions

Goal: keep speculative ideas gated until Concrete's proof/evidence foundation
can contain them honestly.

Done when: each research idea is either pulled into an earlier phase by a
forcing example, explicitly deferred, or rejected.

### Task R-0408

**Phase:** research

**Pull trigger:** The new Core evaluator encounters a workload whose undefined-behavior diagnosis requires instrumentation beyond its normative semantics.

**Disposition (2026-10-19):** Superseded in core scope by R-0496/S7: merge required instrumentation into R-0496; separately specify any extra research.

**Objective:** Broaden the proof-relevant interpreter toward Miri-style UB checking only if the proof-subset interpreter proves valuable.

### Task R-0409

**Phase:** research

**Pull trigger:** R-0409: when phase research becomes active or next, review the surviving requirement and its acceptance artifacts.

**Restructured (2026-11-02):** retired with the legacy compiler: The disposition explicitly retires legacy ProofCore redesign but retains demonstrated proof debt. surviving requirement: Demonstrated recurring evaluator proof debt must inform evaluation and proof engineering on the new Core without committing to a sized evaluator. This line governs over older implementation directions for the legacy compiler.

**Disposition (2026-10-19):** Superseded by R-0496/R-0512: retire legacy redesign; carry demonstrated proof debt into Core proof engineering.

**Objective:** Investigate a sized/indexed ProofCore evaluator only if the current fuel-indexed evaluator remains repeated proof debt after HMAC and at least one other substantial loop/composition proof. This is ProofCore v2 research, not a migration commitment; see [research/proof-evidence/SIZED_EVALUATOR_INVESTIGATION.md](../../research/proof-evidence/SIZED_EVALUATOR_INVESTIGATION.md).

### Task R-0487

**Phase:** D

**Pull trigger:** R-0487: when phase D becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep reporting analysis separate from mandatory capability containment.

**Objective:** Infer the authority each function's body MAY use, and use that to check and
explain capability declarations — never to replace them. Every inferred fact is a potential
effect, not guaranteed execution: a reachable network call means "may use `Network`", not
"uses the network".

**Status (2026-10-04): rewritten around R-0484's accepted semantics; planned, not scheduled.**
R-0484's partial checkpoint (`a7c9cf1c`) made the header authoritative: `with(...)` is the
complete list of a function's external authority, handles carry their capability in their
type (`Writer<C>`) and using one requires `with(C)`, `trusted` absorbs only `Unsafe`, and
every foreign binding declares its effects as an audited assumption. The task's original
framing (2026-09-26: split `with(...)` into separate `requires`/`carries`/`performs` facts,
with authority travelling unannounced inside handle values) is superseded on both counts:
`carries` is now expressed in types, and the declared header is the primary fact, not an
inferred one. What survives is inference as a CHECK over declarations and as the basis for
negative answers.

1. **Potential authority per function.** Compute the capabilities a body may use through
   direct calls, handle use at each handle's instantiated capability, function-pointer
   values with a resolved target set, and foreign bindings' declared effects. Potential
   use beyond the declaration is already a checker error. A declaration broader than the
   potential set is reported as an over-declaration — an advisory least-authority hint,
   issued only when the analysis is complete (no unresolved edge), never an error:
   conservative declarations remain valid, for instance to keep an interface stable.
2. **Instantiation-level reports.** A capability-generic helper reports its variable
   (`print_bytes<cap C>` may use `C`), and each call site reports the instantiation
   (`Console` where `base64_cli` calls it), so a reviewer sees concrete authority where it
   is chosen.
3. **Negative questions only under completeness.** "Does this touch the network?" is an
   absence, so it is answered only when the potential set has no unresolved edge — a
   function-pointer value whose targets are unknown, or a dependency summarised without
   capability facts. An unresolved edge yields `unknown`, naming the edge. An answer that
   passes through a foreign binding is conditional on that binding's declared effects and
   names the assumption (R-0484 R10). See `docs/project/ABSENCE_IS_NOT_A_FACT.md`.
4. **Out of scope:** what is mutated through arguments, termination, and which inputs
   influence which outputs. Those are separate contracts (R-0488's parameter-mutation audit;
   R-0473/R-0474 typed contracts), and `with(...)` does not claim them.

**Exit:** `print_bytes` reports "may use `C`" generically and "may use `Console`" at its
`base64_cli` call site; a function declaring `with(Network)` whose completely-analysed body
may use nothing network-capable gets an advisory over-declaration note, and the same
function with an unresolved edge gets none; a negative claim is refused where the potential
set has an unresolved function-pointer edge, naming it; a negative claim that depends on a
foreign binding names the assumption. Gate: a fixture pair in which the same helper is called through a resolved
and through an unresolved function pointer yields `none` and `unknown` respectively, plus a
positive control where a `Writer<{}>` helper may use no external authority.


---

## Open Research Questions: Bend And Roc

These questions remain undecided: they add no implementation tasks, priorities,
release gates or adoption decisions. The narrow agreed clarifications belong to
R-0215, R-0285, R-0150, R-0486 and R-0416 above. The
[research record](../../research/meta/bend-roc-lessons.md) retains the source links,
video discussion, evidence limits and [complete consideration register](../../research/meta/bend-roc-lessons.md#11-complete-consideration-register).
The current execution queue is unchanged.

### Requirements, repair and developer guidance

- Does a declared required-claim set add protection beyond existing inventories,
  mutation controls and policy, particularly when eligibility or trust changes?
- After the frozen-baseline controls work, would a bounded agent repair experiment
  improve authoring cost while retaining every failed, refused or unsupported case?
- Would version-matched installed guidance, one discoverable verification entry,
  graded tasks with hidden solutions and correct-refusal cases, and a rejected-bug
  demonstration improve the existing workflow without duplicating its machinery?

Context: [R-0215](#task-r-0215), [R-0486](#task-r-0486),
[proof synthesis](../verification/PROOF_SYNTHESIS.md), research C01–C07/C10.

### Checking and replay costs

- Which measured phases dominate cold, warm and incremental feedback, and where
  could smaller generated obligations or reusable lemmas reduce cost without
  changing the statement, trust boundary or semantic coverage?
- What complexity discipline and missed-optimization diagnostics would keep the
  compiler and its guidance understandable without arbitrary token or size caps?

Context: [R-0150](#task-r-0150), [pipeline telemetry](../../scripts/tests/check_pipeline_telemetry.sh), research C08–C10.

### Dependency and build-time authority

- Beyond the adapter authority inventory, which transitive native/trusted additions
  should application policy reject, and what explicit foundation can it accept?
- What isolation is warranted for executable dependency recipes and upgrades, and
  how should enforced restrictions and accepted host authority be presented?
- How can tooling communicate authority, caller-visible mutation, determinism,
  termination/resource bounds and correctness without inferring one from another?

Context: [R-0285](#task-r-0285), [R-0440](ROADMAP_TASKS.md#task-r-0440),
[FFI integration](../platform/FFI_GLUE.md), research C11–C13.

### Ownership and allocation

- Which real workloads justify consuming/mutating transforms, capacity reuse,
  preallocation or explicit arenas, after existing mutation is measured?
- Can scoped callable specialization and sound alias information remove overhead
  while preserving non-escape rules, capability effects and the exact backend contract?
- What allocation, copying, growth and peak-memory regressions should workload
  evidence expose, including cleanup cascades and arena lifetime tradeoffs?

Context: [R-0416](#task-r-0416), [value model](../archive/design-records/VALUE_MODEL.md),
[allocator research owner](#task-r-0404), research C14–C16/C23.

### Representation and optimization

- Where do profiles justify check hoisting, bounded compile-time tables, intentional
  wrapping arithmetic, unboxing or tail-call optimization with pending cleanup accounted for?
- When do immutable snapshots save more downstream work than they cost to retain or copy?
- Would compact indexed representations or direct-load caches improve measured hot
  paths while preserving owner/bounds checks, schema/target validation and invalidation?

Context: [R-0416](#task-r-0416), [compiler research](../../research/compiler/COMPILER_PIPELINE_RESEARCH.md), research C17–C19/C22.

### Proof-guided optimization and artifacts

- Can optimization against fixed requirements improve a measured workload when
  replay, external conformance, differential results, runtime and memory are evaluated together?
- Which emitted-code validation would establish the missing semantic edge beyond
  safe compiler implementation or a theorem about a source model?
- How should development feedback and optimized deployment be measured separately,
  with reproducible target/toolchain/artifact identities and honest adverse-case comparisons?

Context: [R-0416](#task-r-0416), [R-0299](#task-r-0299), [R-0300](#task-r-0300),
[emitted-code research](../../research/compiler/sparkle-emitted-code-validation.md), research C20–C22/C26–C27.

### GPU feasibility, behind concurrency prerequisites

- After [R-0399's design-only and formal-model prerequisites](#task-r-0399)
  are met, is there a workload justifying one restricted array backend with explicit
  buffers, transfers, synchronization, address spaces and partial-failure semantics?
- How would transitive device eligibility, indirect targets, runtime/ABI limits,
  toolchain/artifact identity and CPU/device differential evidence be checked?
- Would end-to-end gains survive compilation, transfer, launch, synchronization,
  irregular work and memory costs, with explicit unavailable-device/fallback behavior?

Synchronous host completion does not remove device-side concurrency. No automatic
offload, managed shared heap or general recursive device runtime follows from these
questions. Context: [execution model](../platform/EXECUTION_MODEL.md), research C24–C26.

Any later adoption needs a forcing workload, bounded evidence and an explicit
owner decision under the [design policy](PROJECT.md#design-policy) and
[evidence architecture](../verification/EVIDENCE_ARCHITECTURE.md).


## Open Research Questions: Goose

These questions remain undecided. They are not planned features, implementation
tasks, priorities, release requirements, or changes to the current execution queue.
The [Goose research survey](../../research/compiler/goose-design-survey.md) records the
pinned sources, compatibility concerns, and possible experiments.

- Could residual bounds-check reports and optional enforcement fit explicit
  contracts and declaration-level summaries without whole-program inference?
- Could validated indices preserve bounds, container identity, and stale-slot
  safety while reducing repeated traversal checks?
- Could explicit compact storage and validated binary images help a real workload
  within the current layout contract, with visible allocation and failure behavior?
- Does construction directly into a destination eliminate measurable copies
  without changing evaluation order, aliasing, or linear cleanup?
- Which benchmark controls and share-nothing concurrency constraints are useful
  to compare without importing Goose's implicit authority or lifetime model?
- Could the test runner compile many fixtures per compiler process, could a
  differential fuzzer guard any pass that removes safety checks, and would an
  analysis inventory of Check, Elab and Lower locate structural bug owners?
- What evidence would support or rule out each idea under Concrete's
  [design policy](PROJECT.md#design-policy)?

A residual-check inventory and a bounded validated-index graph are possible
experiments, not selected work. Record counterexamples, costs, contract/report
shape, and evidence scope before recommending adoption, deferral, or rejection.
Existing arena/index, binary-validation, concurrency, resource-fact, and
performance-evidence owners remain unchanged. Listing Goose here neither adopts
its nonlocal returns, automatic reclamation, or stale-slot semantics nor reopens
standing language decisions. Any adoption requires a separate decision.


## Open Research Questions: Recent Languages

These questions remain undecided. They are not planned features, implementation
tasks, priorities, release requirements, or changes to the current execution queue.
The [recent-languages survey](../../research/meta/recent-languages-2026.md) records
the 16 projects reviewed (repositories created 2026-04-24 to 2026-10-24), their
pinned commits, the admission filter, and what is not imported. Every entry was
reviewed at README level only; each question below starts with a source review
at the pinned commit that confirms or corrects the claim.

### Evidence and verified toolchains

- Solidus: how does a single end-to-end `compile_correct` statement, with target
  semantics kept in a separate pinned repository, compare with the S15 refinement
  and transport theorems in [VERIFIED_PIPELINE.md](../compiler/VERIFIED_PIPELINE.md)?
- LeanCIL: do receipts require that regeneration from the recorded identity
  reproduce the same bytes, and is its trusted-computing-base presentation clearer?
- qedsvm: could `Concrete.Semantics.TermIR` join the interpreter and compiled
  binary in differential fuzzing, so proof-model/emitted-code disagreement is
  found automatically? Owner: [emitted-code research](../../research/compiler/sparkle-emitted-code-validation.md).
- Phil: does its source-verification/artifact-certification split name a
  distinction our evidence classes lack?

### Authority and review

- Jacquard: how does its completeness guarantee for effect lists compare with
  capability checking for FFI, indirect calls and trusted code, and could the
  capability set of `main` be recorded in the built artifact for release-to-release
  comparison without a runtime grant layer?
- Fai, Blorp and FSL: which diagnostics and machine-readable outputs help
  generate-check-repair loops, independent of their memory models?

### Ownership contrast

- Valen: which workloads does group borrowing accept that second-class
  references reject, and does Concrete have an explicit answer for each?

### Tooling and backends

- zerolang: could semantic-query results carry a content hash that edit tooling
  must match, with text remaining the source?
- Dawn: what cross-backend equality gate should exist once a second backend does?
- machin and bendc: is byte-identical emitted output across hosts and runs
  already gated by the [determinism](../compiler/DETERMINISM.md) checks?
- Rux and Ink: what can bounded explicit optimization passes and proved rewrite
  laws contribute to the optimization-policy and proof-guided optimization questions?
- Hale: does channel-only communication between single-owner components inform
  R-0399 once its prerequisites are met?

### Language and stdlib proposals for the owner

- A function-level "no remaining bounds trap" claim, a grow-only std collection,
  guaranteed construction into the destination, stored validated indices,
  owned-only thread transfer with contained worker failure, and validated
  offset-linked data loading are recorded in the survey as proposals. Each
  goes to the owner with options before any design or encoding work.

### Periodic refresh

- Every six months, repeat the repository search, append new projects to the
  survey with pinned commits, and add questions here only for entries that pass
  the admission filter.

Any adoption needs a forcing workload, bounded evidence and an explicit owner
decision under the [design policy](PROJECT.md#design-policy) and
[evidence architecture](../verification/EVIDENCE_ARCHITECTURE.md).

### Task R-0399

**Phase:** research

**Pull trigger:** A workload needs concurrency after the new compiler's v1 surface is frozen.

**Reconciliation (2026-10-19):** Keep dated decision/deferral under R-0515.

**Objective:** Keep concurrency design-only until the v1 surface is frozen:

capability lattice, scopes, spawn/join, linear handles, bounded channels,
result flow, ownership transfer, rejected forms, and report schema. This
includes the memory-model question Concrete must not answer accidentally:
atomics, synchronization, shared mutable state, data-race freedom,
capability-gated thread authority, and proof/evidence classes for concurrent
code all remain research-gated until a formal model and pressure tests exist.
Async syntax remains rejected for v1 per `docs/project/ANTI_FEATURES.md`; the
positive research direction lives here and in `docs/platform/EXECUTION_MODEL.md`:
explicit concurrency primitives, visible effects, linear handles, and
bounded scheduling/failure evidence rather than hidden async lowering or a
second control-flow semantics.

   Reference points for the design survey (2026-07-25): Microsoft
   Research's Project Verona — Behaviour-Oriented Concurrency: shared
   state lives in isolated cowns (concurrent owners); a behaviour
   acquires its whole cown set atomically, giving data-race freedom and
   deadlock freedom by construction. A cown is a linear handle scheduled
   against an executor — close to Concrete's linear-handle vocabulary,
   but the honest mapping is typestate/protocol rather than a
   `with(...)` capability, and the executor (scheduling, fairness, the
   freestanding story) is the real design problem, not the spelling.
   Pony's reference capabilities are the shipped, production-tested
   ancestor of the same model. Verona itself is research-grade and
   mid-refactor: steal the atomic multi-acquisition semantics, not the
   runtime.

### Task R-0400

**Phase:** research

**Pull trigger:** The new compiler's concurrency design is ready to test through concrete sketches and expected reports.

**Objective:** Build concurrency pressure-test sketches and expected reports before implementation.

### Task R-0401

**Phase:** research

**Pull trigger:** New-compiler concurrency sketches expose a stable candidate model whose safety claims need mechanization.

**Reconciliation (2026-10-19):** Keep; disambiguate “v1 concurrency” from language 1.0.

**Objective:** Mechanize the v1 concurrency formal model before claiming safety.

### Task R-0402

**Phase:** after-1.0

**Pull trigger:** The new compiler's concurrency model is mechanized and its pressure-test reports are stable.

**Objective:** Implement OS threads/scopes/channels only after the model and reports are stable.

### Task R-0403

**Phase:** research

**Pull trigger:** A new-compiler protocol workload cannot express its required typestate with explicit state values.

**Objective:** Research typestate only if a current state-machine/protocol example needs it.

### Task R-0404

**Phase:** research

**Pull trigger:** A new-compiler bounded-capacity workload demonstrates a lifetime or region gap that ordinary allocator values cannot solve.

**Reconciliation (2026-10-19):** Keep only unresolved lifetime/region design; ordinary allocator examples are R-0317.

**Objective:** Research arena allocation after bounded-capacity/allocation-profile work exposes a concrete gap.

### Task R-0405

**Phase:** research

**Pull trigger:** A new-compiler freestanding user needs WCET annotations for a named deterministic hardware model.

**Reconciliation (2026-10-19):** Keep after freestanding hardware model.

**Objective:** Research exact WCET/cache/pipeline behavior only with a
target/hardware model. Posture: be a strong WCET annotation frontend, not
another WCET analyzer—Concrete can export its closed callable set, explicit
indirect edges, and R-0445/R-0244 resource facts instead of making a
source-level cycle claim. Requires deterministic hardware (freestanding
Cortex-M class), so realistically after the freestanding profile (R-0322).
Source-level step/space bounds stay with R-0445, R-0244/R-0248, and R-0417's
`complexity_guarded` evidence; none of them imply hardware timing. A future
WCET bridge consumes those exported facts under a named hardware model and
must not silently reinterpret or overwrite their source scope.

### Task R-0449

**Phase:** research

**Pull trigger:** An audited consumer of new-Core multi-kernel evidence requires bridge-level trust reduction beyond prover agreement.

**Reconciliation (2026-10-19):** Keep workload gate; align bridge with Core/VC semantics.

**Objective:** Research Why3-style realization for the prover backends — proving, in Rocq and Isabelle themselves, that the obligation IR's built-in theories are sound in that prover's model — only after R-0448 has a real multi-kernel user who needs bridge-level trust rather than agreement-level trust. Each realization proof converts a slice of the trusted Core→obligation bridge into kernel-checked evidence; without it, "three kernels" means three syntaxes over one trusted bridge. Pulled by an audited consumer requiring it, not by symmetry.

### Task R-0406

**Phase:** research

**Pull trigger:** New-compiler packet or ELF ports still repeat parser boilerplate after the checked combinator proof path works.

**Reconciliation (2026-10-19):** Keep only generation remainder.

**Objective:** Research binary-format DSLs only if packet/ELF examples show repeated parser boilerplate. The trigger fired (2026-07-25 audit: seven combinator shapes spelled ~45–50 times; elf_header the least-protected parser in the tree); the combinator core and the numeric/PNG/ELF ports are pulled forward as R-0444. What remains gated here is the generation layer proper — EverParse-style spec→Concrete source — gated on R-0170's checked combinator proof leg existing and on DECISIONS' external-generation rule (generated output is audited as ordinary source).

### Task R-0407

**Phase:** research

**Pull trigger:** A hardware target needs the new compiler's stable source capabilities and package policies mapped to hardware enforcement.

**Objective:** Research hardware capability mapping after source-level capabilities and package policies are stable.

### Task R-0410

**Phase:** research

**Pull trigger:** A new-compiler semantic-diff or proof workload repeatedly recomputes equalities despite mature backend contracts and evidence support.

**Reconciliation (2026-10-19):** Keep with explicit workload trigger.

**Objective:** Research persistent equality/rewrite state after backend contracts, semantic diff, and proof/evidence pipeline are stronger.

### Task R-0411

**Phase:** research

**Pull trigger:** New-compiler stdlib and concurrency pressure tests repeatedly demonstrate that explicit capabilities block real programs.

**Reconciliation (2026-10-19):** Keep deferral, not implementation backlog.

**Objective:** Do not adopt row effects for v1. The default design stays object-capability and audit-visible: authority should be obvious in source, not hidden behind abstract effect inference. Revisit only as a research note if explicit capabilities create a proven, repeated blocker in real programs after the stdlib and concurrency pressure tests exist.

### Task R-0412

**Phase:** research

**Pull trigger:** A new-compiler callback or observer workload proves static linearity and second-class references inadequate through a pressure test and decision record.

**Reconciliation (2026-10-19):** Keep strict pressure-test and evidence-class conditions.

**Objective:** Research a generational-reference-style dynamic fallback only if a forcing workload proves static linearity plus second-class references are too restrictive. The target use case is not ordinary ownership; it is a narrow escape for liveness/provenance facts around observers, back-references, callback-heavy object graphs, or host/FFI handles that Concrete cannot prove statically without unacceptable surface complexity. If adopted, the runtime check discharges an obligation as `checked_dynamically`, never as `proved`, and audit/report output must distinguish it from static ownership proof.


 Do not pull this forward without a pressure test and a decision record.
 The research pass must compare Vale-style generational references and
 region/frozen-scope ideas, explicitly reject any vague "zero-cost safety"
 claim, and prove the fallback does not become a hidden borrow checker,
 hidden GC, implicit Drop, or a way to weaken the linear default.

### Task R-0413

**Phase:** research

**Pull trigger:** The new compiler's published TCB inventory identifies a narrow pass whose Concrete implementation could measurably reduce trust.

**Reconciliation (2026-10-19):** Keep distinct from proving existing Lean compiler.

**Objective:** Research dogfooding Concrete's evidence model onto the compiler's own TCB.

 This is the logical endpoint of "evidence-carrying artifact applied to the
 compiler": rewrite or mirror selected compiler passes/checkers in Concrete,
 attach contracts, and use Concrete's own linearity/capability/proof pipeline
 to reduce the trusted base of the toolchain itself. This is NOT a near-term
 migration commitment and must not block the stdlib through editor areas work; it is a research
 bet to keep visible so the project does not stop at verifying user programs
 while leaving the compiler as an opaque trust-me artifact. This rewrite-in-Concrete route is complementary to the soundness-bridge area verified-compiler endgame; the primary route there is proving the existing Lean-hosted passes semantics-preserving, not re-implementing them.

 This is distinct from the soundness-bridge and backend-contracts areas independent certificate checker. That
 checker re-derives narrow predicates over compiler artifacts in a small Lean
 target; this research item asks whether selected compiler passes should be
 implemented or mirrored in Concrete itself. Do not delay the independent
 checker while waiting for self-hosting evidence.

 First pressure-test only: choose one narrow pass or checker helper with a
 small state space (for example a value-flow checker slice, a type-policy
 predicate, a report fact transformer, or a simple SSA cleanup rule), specify
 its input/output contract, and compare the Concrete implementation against
 the Lean/compiler implementation on a generated corpus. Done when the
 decision record states whether the approach actually shrinks the TCB, merely
 duplicates it, or creates a second source of truth. Pull forward only if a
 real pass can be checked without weakening the no-second-truth-source rule.

**Native backend comparison (planned 2026-10-18; updated 2026-10-19):** the Lean Compiler Backend
(R-0517) is the Lean 4 reference; R-0297 is delegated to it. This task owns evaluating a Concrete implementation
alongside a Lean 4 reference. Recommend Lean 4 for the first assurance prototype,
with explicit instruction semantics and preservation/validation obligations.
Writing the generator in Lean does not prove it correct; writing it in Concrete
also requires accounting for how Concrete compiled the generator. Keep bootstrap,
host-runtime and target-runtime trust distinct. The compiler's Lean runtime and
reference counting are not automatically linked into programs whose machine code
it emits. Compare maintained code, proof/annotation burden, independent bug
detection, reproducibility and actual TCB reduction. See
[QBE/native-backend research](../../research/compiler/qbe-in-concrete.md#lean-4-alternative-and-generated-runtime-2026-10-18).

### Task R-0414

**Phase:** research

**Pull trigger:** The new compiler's incremental database has a concrete relation family that typed functions, maps and joins cannot express cleanly.

**Reconciliation (2026-10-19):** Keep; do not reopen incremental-driver architecture.

**Objective:** Research a Datalog-style / stratified relational **rule layer** only when the shipped the incremental-driver area `CompilerDB` has real relational consumers that typed queries/maps cannot express cleanly.


 `CompilerDB` facts — pass-agreement edges, borrow conflicts, E0293
 container exclusions, provenance edges, dependency/invalidation, package
 evidence, and proof/evidence queries — are naturally relational. A
 Datalog-like or stratified-facts model is a good design reference for that
 layer: derived facts are explicit, dependency edges are queryable, cycles
 and invalidation have a discipline, and audit/report views can ask
 relational questions without reparsing prose.

 the incremental-driver area already owns the typed acyclic query/cache driver, scheduling, and
 invalidation; do not reopen or replace it here. What remains deferred is a
 general Datalog/stratified derivation layer or user-visible query language.
 The trigger is a real `CompilerDB` relation family whose rules are awkward
 as ordinary typed Lean functions/maps/joins. The first investigation should
 compare a small stratified internal rule layer against those ordinary typed
 functions, reuse the incremental-driver area dependency/certificate machinery, and keep the
 user language unchanged: no user-facing logic language, no implicit effects,
 and no hidden second truth source.

 Done when a design note demonstrates one concrete relation family
 (for example provenance invalidation or package evidence queries), includes
 an acyclicity/stratification story, shows replayable derived-fact output,
 and rejects a cyclic or stale derived fact. Until then, this remains a
 future rule-layer reference, not unfinished incremental-compiler work.

### Task R-0415

**Phase:** research

**Pull trigger:** R-0415: when phase research becomes active or next, review the surviving requirement and its acceptance artifacts.

**Reconciliation (2026-10-19):** Keep; add omitted R-0449 to enumerated coverage. Records the deferred Silver shared-state systems workload (outside ethlambda completion), and R-0449 belongs to its enumerated coverage.

**Objective:** Add the research-extensions area validation artifact: one pressure-test sketch, expected report, and decision record for every research-gated extension (concurrency, atomics/memory model, typestate, arena allocation, WCET, binary-format DSLs, hardware capability mapping, Miri-style interpreter, sized evaluator, persistent rewrite state, row effects, generational dynamic fallback, compiler self-verification, and Datalog-style relational facts). No research item graduates unless its forcing example, report shape, evidence class, and rejection or pull-forward criteria are recorded.

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

### Task R-0305

**Phase:** after-1.0

**Pull trigger:** A new-compiler deployment requires Wasm32 sandboxing and validated BIR is available.

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Target Wasm32 with a pinned WebAssembly toolchain and one documented WASI profile. Browser integration, the component model, Wasm GC, Wasm64, threads, SIMD, and exception handling are explicitly later work unless a forcing workload promotes one of them.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0100** (superseded): superseded — Wasm/Cranelift no longer wait for QBE; the WebAssembly area entrance criteria (R-0305) consume validated BIR. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0100)

### Task R-0306

**Phase:** after-1.0

**Pull trigger:** The new compiler's selected Wasm32 profile needs its first independently lowered validated-BIR module.

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Lower validated BIR (R-0503) into a small typed Concrete WebAssembly AST, run a pure Concrete verifier, emit deterministic `.wasm`, and validate it with the pinned external validator. Do not emit ad-hoc WAT from CLI code and do not route WebAssembly through LLVM.

### Task R-0307

**Phase:** after-1.0

**Pull trigger:** The new compiler's Wasm lowering begins consuming shared layout, trap and capability facts.

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Reuse canonical semantic, layout, builtin, trap, source-identity, and capability facts, while keeping WebAssembly instruction selection and control-flow restructuring independent from LLVM and LCB lowering.

### Task R-0308

**Phase:** after-1.0

**Pull trigger:** The new compiler's Wasm AST and emitter can execute the initial main-returning-42 fixture.

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Deliver thin vertical slices in this order: `main -> 42`; direct calls and returns; branches/loops; integer operations and checked traps; linear-memory loads/stores; basic aggregates; runtime imports. Each slice ends in an executable differential fixture.

### Task R-0309

**Phase:** after-1.0

**Pull trigger:** A new-compiler Wasm fixture first needs an authority-bearing runtime import.

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Map external authority deliberately onto an explicit WASI import manifest.

Missing capabilities and unsupported operations fail before partial code
generation with stable `wasm_pending` diagnostics; there is no LLVM/LCB or
per-function fallback.

### Task R-0310

**Phase:** after-1.0

**Pull trigger:** A supported new-compiler Wasm slice must run through ordinary project build, run and test commands.

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Integrate `--backend wasm`, `--emit-wasm`, projects, retained artifacts, debug bundles, replay commands, caching, `doctor`, feature JSON, audit output, and ordinary `run`/`test` workflows through the backend-neutral driver.

### Task R-0311

**Phase:** after-1.0

**Pull trigger:** The new compiler executes a Wasm slice that also runs through ConcreteSpec, LLVM and LCB-native.

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Compare Core-semantics, LLVM, LCB-native and WebAssembly observations for every eligible fixture: stdout, stderr, exit status, and normalized trap/failure class. Add Wasm-specific cases for structured control flow, linear-memory bounds, alignment, integer-width conversion, imports, and deterministic module composition.


Done when: the declared Wasm32/WASI subset builds and runs at least one the flagships area-class workload through normal user workflows; its capability matrix is
complete and fail-closed; emitted modules and manifests are deterministic and
independently validated; the four-way differential suite covers the supported
subset; and audit/evidence output labels WebAssembly results honestly as
`tested` or `backend_trusted`, never as proof that native or source semantics
are preserved.

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

### Task R-0322

**Phase:** after-1.0

**Pull trigger:** A new-compiler workload requires no libc and its own startup or embedded runtime.

**Objective:** Define `hosted` versus `freestanding` target profiles: libc,
startup, allocator, terminal abort behavior, stack assumptions, I/O
availability, floating-point assumptions, and supported capabilities. Hosted
abort terminates the process; freestanding abort transfers to a declared
non-returning target handler. Neither profile promises cleanup.

### Task R-0323

**Phase:** after-1.0

**Pull trigger:** A selected new-compiler freestanding profile cannot use std without separating OS and allocation dependencies.

**Objective:** Split stdlib modules by target profile: core no-alloc/no-OS modules, allocator-backed modules, hosted OS modules, and explicitly unavailable modules.

### Task R-0324

**Phase:** after-1.0

**Pull trigger:** A new-compiler freestanding build needs validation that unavailable hosted services cannot enter through target grants.

**Objective:** Define freestanding capability policy: no ambient `File`, `Network`, `Console`, or `Process`; target-specific capabilities must be declared and audited.

### Task R-0325

**Phase:** after-1.0

**Pull trigger:** A new-compiler device-driver workload needs MMIO, volatile access, inline assembly or interrupt entry.

**Reconciliation (2026-10-19):** Keep; no implicit extension of sequential native theorem.

**Objective:** Define hardware-access primitives and evidence classes before device-driver examples: `volatile`/MMIO operations require explicit `with(Mmio)` or `with(Device)` capability, inline assembly is `trusted` and requires `with(Unsafe)`, and interrupt handlers are an effectful/trusted boundary with explicit target assumptions.

### Task R-0326

**Phase:** after-1.0

**Pull trigger:** A new-compiler freestanding program first needs allocation and a target-specific allocation-failure contract.

**Objective:** Add explicit allocator/runtime hooks for freestanding builds, including ownership of allocation failure behavior and cleanup expectations.

### Task R-0327

**Phase:** after-1.0

**Pull trigger:** A new-compiler freestanding binary requires a custom entry symbol, linker script or section layout.

**Objective:** Add linker/startup configuration: entry symbol, no-main mode, target triple, data layout, linker script hooks, and section/layout assumptions.

### Task R-0328

**Phase:** after-1.0

**Pull trigger:** A new-compiler freestanding build could accidentally reach libc, a hosted API or hidden allocation.

**Objective:** Add freestanding diagnostics: reject hosted APIs, hidden allocation, libc calls, unsupported target features, and unavailable capabilities.

### Task R-0329

**Phase:** after-1.0

**Pull trigger:** The new compiler's freestanding profile, std partition, startup and diagnostics support a complete no-allocation program.

**Objective:** Add one freestanding example: bounded parser, small checksum/hash kernel, or fixed-capacity state machine with no allocation and no hosted I/O.

### Task R-0330

**Phase:** after-1.0

**Pull trigger:** The first new-compiler freestanding example needs an audit of its remaining target assumptions.

**Objective:** Add one embedded-style audit bundle naming all remaining target assumptions:

stack, interrupt model if any, allocator/runtime hooks, endian/layout, and
backend/toolchain boundary.

### Task R-0331

**Phase:** after-1.0

**Pull trigger:** A new-compiler freestanding workload demonstrates that supported LLVM and LCB-native targets are insufficient.

**Reconciliation (2026-10-19):** Keep; baseline updated to the LLVM and LCB-native paths (R-0517).

**Objective:** Keep production freestanding Wasm and additional backend targets deferred until freestanding profiles prove the LLVM and LCB-native paths are not enough. Any LCB freestanding target, runtime/startup contract, or release claim graduates here and through R-0505/R-0517's profile contract; LCB's own target breadth (its P10) is an external dependency, not Concrete work. (Updated 2026-10-19: the QBE backend-validation area's QBE differential oracle is retired in favour of the LCB-native leg of R-0491's differential harness.)

### Task R-0332

**Phase:** after-1.0

**Pull trigger:** The new compiler has a freestanding example and target-assumption bundle ready for an end-to-end validation gate.

**Objective:** Add the freestanding area validation artifact: one freestanding demo project plus an MMIO/device-profile mock audit bundle. The demo must build with no hosted APIs, name allocator/startup/linker assumptions, reject hidden libc or allocation, and report `with(Device)`/`with(Mmio)`/`with(Unsafe)` evidence classes without pretending hardware behavior is proved.

### Task R-0453

**Phase:** after-1.0

**Pull trigger:** The new compiler supports a freestanding block-device profile sufficient to run littlefs power-loss injection tests.

**Reconciliation (2026-10-19):** Keep after profiles and forcing workload.

**Objective:** Extend the freestanding story from "no libc" to "no libc, plus a
device and a crash model" by porting littlefs, the power-loss-resilient flash
filesystem: 7,633 lines (`lfs.c` 6,558 plus headers, upstream `6cb4e865`), zero
dynamic allocation, fixed buffers, and an explicit `lfs_t` context over a
block-device interface. Fit analysis is in
[research/workloads/port-candidates.md](../../research/workloads/port-candidates.md).

Why it belongs after R-0448 rather than beside it: MM0's verifier discharges this
phase's trigger with a syscall surface and a single-bit output, which is the
cheapest honest freestanding claim. littlefs adds the two things that claim does
not reach — a block-device capability (the `with(Device)`/MMIO evidence decision
this phase already owns) and correctness that spans *interrupted* runs. It should
not start before the freestanding profiles, profile-split stdlib, and
freestanding diagnostics exist, for the same reason R-0448 does not.

**Definition of done.** The freestanding build passes the upstream test runner,
and the red-team guard is `lfs_emubd` power-loss injection: for a recorded set of
interruption points across mount/write/rename/remove, the filesystem must remain
mountable with no lost committed operation, run as a regression gate rather than
a transcript. The audit bundle reports the zero-allocation claim (no
`with(Alloc)`), the device capability, the block-device trust boundary, and every
remaining trusted island.

**Claim discipline.** Crash consistency is a relational property across runs —
every interrupted prefix leaves a mountable filesystem — and `#[requires]` /
`#[ensures]` cannot state it today. The honest class is `tested_by_oracle` under
adversarial power-loss injection, with the interruption-point coverage reported
as a number rather than implied by a green gate. Do not describe the port as a
verified or crash-safe filesystem; describe what the injection harness covered.
