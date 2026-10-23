# Concrete Roadmap Backlog

This file holds the roadmap tasks that no active or next phase schedules. [ROADMAP.md](../../ROADMAP.md)
keeps the phases, the execution queue and every task a queue row names or whose **Phase:** line includes
an active or next phase; every other `### Task R-NNNN` lives here, with its full text, stable ID and
**Phase:** line unchanged. A task in this file is not abandoned and not lower quality: it waits for its
phase or its trigger. Task IDs remain immutable across the move, so a reference such as `R-0305` means
the same task in either file.

The tasks were split out of ROADMAP.md on 2026-10-19. They are filed under the same topical
**area** headings as in ROADMAP.md (an area's prose and its scheduled tasks stay there; an area with
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

1. **Pull triggers.** A task moves into ROADMAP.md when any of these happens:
   - a workload, bug report, pilot or user need hits it (its **Pull trigger:** names which ones);
   - a phase that depends on it becomes active or next, or a queue row needs it; or
   - a release checkpoint in the [release section](../../ROADMAP.md#releases) needs a bounded
     increment of it.

   Pulling moves the whole task, unchanged, into its area in ROADMAP.md, in the same change that
   updates its **Phase:** line or the queue row that needs it. The **Pull trigger:** line may be dropped
   once the task lives in ROADMAP.md.
2. **Phase entry checkpoint.** When a phase becomes active (the **Active phases:** line in
   ROADMAP.md changes), every task in this file is reviewed. Each is either pulled into ROADMAP.md,
   kept here with a reason, or retired to the archive with a reason recorded in the reconciliation
   record. A task kept here although its **Phase:** line names an active or next phase carries a
   **Kept in backlog:** line giving the reason. The review is recorded in the
   [review log](#review-log).
3. **Per-release review.** Before each release candidate (R-0333's candidate record), the backlog is
   reviewed the same way, with attention to tasks whose triggers the release's pilots, workloads or
   bug reports have hit. The review is recorded in the review log.
4. **Placement is gated.** `scripts/tests/check_roadmap_coverage.sh` checks that every task lives in
   exactly one of ROADMAP.md and this file; that every task here has a **Phase:** or disposition
   line and a **Pull trigger:** line, with no two live tasks sharing the same trigger text; that
   ROADMAP.md names its **Active phases:** and **Next phases:**; that no task here is scheduled in
   one of those phases without a **Kept in backlog:** reason; that no task in ROADMAP.md is scheduled
   only in later phases unless a queue row names it; and that the latest review-log row names the
   current active phases.

### Review log

| date | occasion | active phases | outcome |
|---|---|---|---|
| 2026-10-19 | split from ROADMAP.md during Phase 1 | 1 | 154 tasks moved here: each is scheduled only in Phases 4 – 7 or after 1.0/research (or, for a retired stub, in no phase); none is named by a queue row |

## Area: Verified-Compiler Pipeline, Stabilization And Usability

### Task R-0522

**Phase:** 5

**Pull trigger:** Phase 5 entry, or earlier when an Isabelle batch-vs-single-goal divergence or a change that has to be made twice in the builder is reported.

**Objective:** Collapse the Isabelle builder's duplicated batch and single-goal paths into one
implementation with both entry points.

**Status (2026-10-19): planned; bounded cleanup recorded twice in prose without an owner.**

**Exit:** one builder path; batch and single runs produce identical theories for a shared fixture
set; a mutation in the shared path fails both entry points.

### Task R-0527

**Phase:** after-1.0/research

**Pull trigger:** a workload that needs a noninterference, constant-time or relational claim beyond R-0531's bounded Core-level revision equivalence.

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

**Phase:** 5

**Pull trigger:** Phase 5 entry, or earlier when an interrupted or concurrent evidence-store write is observed to leave a partial record.

**Objective:** Atomic evidence persistence: crash-safe writes, permissions and concurrent readers for
the evidence store. Distinct from R-0144's compiler cache.

**Status (2026-10-19): planned.**

**Exit:** an interrupted write leaves either the previous or the new record, never a partial one
(fault-injection control); concurrent readers never observe a partial record; store permissions are
checked on open.

### Task R-0531

**Phase:** 6

**Pull trigger:** Phase 6 entry once R-0473/R-0474's typed contracts land, or a reviewer who needs a revision-equivalence claim beyond R-0183's diff gate.

**Objective:** Selected-function equivalence across revisions: compare a function's two revisions
with their original dependencies and semantics, as the follow-on to R-0183's diff gate.

**Status (2026-10-19): planned; after typed contracts.**

Before implementation, define the admitted Core fragment, input relation, observation relation, treatment of termination and traps, and versioned dependency environments. This task owns bounded Core-level equivalence across revisions; it makes no native-transport, constant-time or noninterference claim.

**Exit:** an equivalence claim between two revisions of one function is generated against Core and
replayed; a behaviour-changing edit is refused; a dependency change is reported as a changed
premise, not hidden.

### Task R-0532

**Phase:** 5

**Pull trigger:** Phase 5 entry; earlier if a release candidate needs native-trust receipts, an advisory-rollback guard or fragment-scoped advisory narrowing instead of the published limitation.

**Objective:** R-0208's residual limits: native-trust receipt issuance, advisory-policy rollback
protection and fragment-scoped advisory narrowing — each either implemented or kept as an explicit,
published limitation.

**Status (2026-10-19): planned.** R-0208 is closed; these are its recorded accepted limits. Native-
evaluation receipts remain withheld until issued with distinct trust labels (R-0506/R-0440).
Reverting both policy and minimum defeats the local rollback guard; that limitation stays published
until a threat-model-specific guard exists. Narrowing an advisory to a fragment requires an
independently checked environment footprint; whole-version refusal is the safe fallback.

**Exit:** each of the three has an implemented control or a published limitation with its threat
model and owner.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0208** (done): done — closed 2026-10-13 at `2b570c5a`; the body is retained for its accepted limits, now owned by R-0532. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0208)

### Task R-0533

**Phase:** after-1.0/research

**Pull trigger:** users who repeatedly hand-shrink trusted wrappers ask for suggestions, after the trusted-boundary reports are stable.

**Objective:** Optional trusted-boundary shrinker: suggest smaller trusted wrappers without upgrading
any evidence.

**Status (2026-10-19): pull-gated.**

**Exit:** suggestions are advisory only; a suggestion never changes a verdict; a control shows an
applied suggestion still requires the ordinary checks.

### Task R-0534

**Phase:** after-1.0/research

**Pull trigger:** a workload that needs epsilon error bounds on floats, after V7's exact IEEE float profile exists.

**Objective:** Floating-point error-bound specifications (epsilon bounds via real analysis) after the
exact IEEE float profile (V7).

**Status (2026-10-19): deferred; requires V7's float semantics.**

**Exit:** a decision record on the real-analysis library and one bounded worked example, or a recorded
deferral.

### Task R-0535

**Phase:** after-1.0/research

**Pull trigger:** a pilot or release reviewer asks to record a reviewed human proof argument as evidence.

**Objective:** Decide whether reviewed human proof arguments (`proved_by_hand`) can be recorded as
freshness-tracked reviewed evidence without ever presenting as machine-checked `proved`.

**Status (2026-10-19): research.** R-0461 forbids `proved` for unresolved conditions; a human
argument is reviewed evidence, not a kernel-checked proposition.

**Exit:** a policy decision; if adopted, the evidence class is distinct on every surface and a control
shows it never satisfies a kernel-only policy.

### Task R-0537

**Phase:** 4, 7

**Pull trigger:** Phase 4 entry for the callable-set compatibility rules; earlier when a library change alters a dependent's reachable callable set without invalidation.

**Objective:** Closed-world library evolution: callable-set compatibility and escape hatches when a
library's set of callables changes, respecting open imported callbacks.

**Status (2026-10-19): planned.**

**Exit:** additions, removals and changes to a dependent’s reachable callable set trigger dependency invalidation and compatibility checking. Previously closed-world evidence is retained only after rechecking the enlarged or changed set against its contracts, capabilities and proof obligations. Adding an unreachable export may remain compatible. Controls cover both cases; open imported callbacks are never treated as closed.

### Task R-0538

**Phase:** after-1.0/research

**Pull trigger:** a release claim or user needs `bv_decide` evidence without native-evaluation trust (H20), or a certificate-replay or kernel-reducible checking route becomes cheap enough to measure.

**Objective:** Reduce the native `bv_decide` checking trust (H20): bitvector proofs currently rely on
native evaluation in the checker.

**Status (2026-10-19): research.** R-0506 owns honest classification of the native-evaluation axiom
now; this task owns eliminating the trust, which was previously "unowned".

**Exit:** a decision record on certificate replay or kernel-reducible checking for the admitted
bitvector fragment, with measured cost; or a recorded deferral.

## Area: Standard Library And Core APIs

### Task R-0448

**Phase:** after-1.0/research

**Pull trigger:** a real user who needs to replay claims with a kernel other than Lean, after R-0506's axiom policy and R-0440's typed evidence land.

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

**Phase:** 4

**Pull trigger:** none of its own — a duplicate; its remaining obligations are pulled with R-0061, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** duplicate of R-0061: merge.

**Objective:** Extend the shipped pure-core proof arc over actual `Option`/`Result`, `Bytes`/slice, numeric checked helpers, and checked text/path conversions against their documented contracts.

### Task R-0028

**Phase:** 6

**Pull trigger:** none of its own — a duplicate; its remaining obligations are pulled with R-0060, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** duplicate of R-0060: merge.

**Objective:** Proof-facing formal stdlib models (`formal_vec`, `formal_map`, `formal_set`, `bigint`, lemma helpers) once contracts need them.

### Task R-0029

**Phase:** after-1.0/research

**Pull trigger:** none of its own — a duplicate; its remaining obligations are pulled with R-0065, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** duplicate of R-0065: merge deferral policy; retain narrow Phase 3 module owners.

**Objective:** Broad compression/crypto/networking/threading only after workload demand.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0029).

### Task R-0061

**Phase:** 4

**Pull trigger:** Phase 4 entry for stdlib APIs that workloads have shown are small, pure, central and stable; earlier when a flagship proof needs a shipped stdlib fact.

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

**Phase:** after-1.0/research

**Pull trigger:** a workload that needs one of the listed deferred families (compression, broader crypto, PEM/ASN.1/X509, full HTTP, dynamic libraries, debug formats and the rest); the forced family alone is pulled.

**Reconciliation (2026-10-19):** Keep explicit deferral ledger, not a phase-completion blocker.

**Objective:** Record deliberately deferred stdlib families so they do not disappear from planning: compression/archive formats, broad crypto beyond the narrow hash/HMAC/constant-time story, PEM/ASN.1/X509, MIME helpers beyond the first workload, full HTTP client/server, DNS/Unix-socket breadth, dynamic libraries, OS debug formats (`ELF`, DWARF, image parsing), atomics, threads, SIMD, target/ABI databases, and platform-specific C/POSIX wrappers. Each stays package-later, backend-later, freestanding-later, or research-later until a workload forces it.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0029** (merged): duplicate of R-0065: merge deferral policy; retain narrow Phase 3 module owners. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0029)

## Area: Usable QBE Backend And Independent Validation

### Task R-0072

**Phase:** none — retired; see the disposition below

**Pull trigger:** none of its own — retired; its remaining obligations are pulled with R-0517, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** retired — LCB pins QBE 1.3 in its own upstream lock; Concrete pins the LCB version under R-0517.

**Objective:** Provision QBE as an explicit toolchain dependency. Pin a supported release and checksum in Nix/CI, verify the installed binary rather than accepting an arbitrary `PATH` match, document source/bootstrap and system-package paths, and make local discovery available through `concrete doctor` / agent feature JSON. Tool download is never an implicit compilation side effect.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0072).

### Task R-0083

**Phase:** none — retired; see the disposition below

**Pull trigger:** none of its own — retired; its remaining obligations are pulled with R-0511, R-0517, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** retired — there is no Concrete QBE backend to select; the no-silent-fallback rule moves to R-0511/R-0517.

**Objective:** Add an explicit backend selection surface:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0083).

### Task R-0084

**Phase:** none — delegated; see the disposition below

**Pull trigger:** none of its own — delegated; its remaining obligations are pulled with R-0517, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** delegated — a typed QBE-style IR and printer are LCB's input layer; Concrete's adapter constructs LCB-IR under R-0517.

**Objective:** Add `Concrete/Backend/QBE.lean` for a small typed QBE IL AST, `EmitQBE.lean` for the pure text printer, and an SSA-to-QBE translation module separate from `EmitSSA.lean`. Do not build QBE text through ad-hoc concatenation inside CLI code. Pin the supported QBE release/tool identity in CI and record it in debug/audit output.

### Task R-0087

**Phase:** 4

**Pull trigger:** none of its own — superseded; its remaining obligations are pulled with R-0512, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** superseded by VP V7/R-0512: merge with total float model and profile-specific trust labels.

**Objective:** Give floating point its own executable slice rather than treating the `f32`/`f64` type mapping as coverage. Add arithmetic; ordered/unordered comparisons; NaN, infinity, signed-zero, subnormal, and precision-change vectors; int-to-float conversions; and checked float-to-int conversions.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0087).

### Task R-0088

**Phase:** 4

**Pull trigger:** none of its own — superseded; its remaining obligations are pulled with R-0502, R-0505, R-0517, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** superseded — aggregate/memory ABI realization moves to R-0502 (layout) and the adapter boundary (R-0505/R-0517) across V2–V6.

**Objective:** Add memory and aggregate support: fixed arrays, structs, packed/explicitly aligned layouts, enums, canonical `Option`/`Result`, strings, vectors, nested places, byte-union payloads, aggregate copies, and small/large aggregate calls/returns. QBE aggregates are memory/ABI objects rather than general SSA temporaries, so make materialization explicit and test tag/payload offsets, padding, alignment, and partially initialized payload storage.

### Task R-0089

**Phase:** 4

**Pull trigger:** none of its own — superseded; its remaining obligations are pulled with R-0505, R-0517, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** superseded — call/FFI/ABI matrices apply at the LCB adapter boundary (R-0505/R-0517); LCB's own ABI coverage (its P07) is an external dependency.

**Objective:** Add calls and platform boundaries: indirect calls, externs, variadics used by the runtime, argc/argv accessors, symbol visibility, linker aliases, C ABI scalar/aggregate classification, and Linux/macOS target selection for the architectures QBE and Concrete both declare supported. Unsupported target, calling convention, TLS, or ABI shapes must fail before emission with a stable diagnostic.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0089).

### Task R-0099

**Phase:** none — retired; see the disposition below

**Pull trigger:** none of its own — retired; its remaining obligations are pulled with R-0517, R-0505, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** retired — graduation of the native path is R-0517/R-0505's LCB-native profile; no QBE graduation exists.

**Objective:** Graduation to a supported release backend occurs only in the backend-contracts area: migrate QBE from the direct validation path onto `ValidatedBackendIR`, pass the full target/toolchain and C ABI matrices, integrate debug/source maps and incremental artifacts, attach translation-validation status, define optimization/tool-version policy, and pass release/platform soak gates.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0099).

### Task R-0100

**Phase:** after-1.0/research

**Pull trigger:** none of its own — superseded; its remaining obligations are pulled with R-0305, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** superseded — Wasm/Cranelift no longer wait for QBE; the WebAssembly area entrance criteria (R-0305) consume validated BIR.

**Objective:** After the QBE backend is stable, evaluate a complementary target rather than starting one in parallel. WebAssembly is the provisional next backend, governed by the WebAssembly area's entrance criteria and narrow Wasm32/WASI scope.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0100).

### Task R-0101

**Phase:** none — delegated; see the disposition below

**Pull trigger:** none of its own — delegated; its remaining obligations are pulled with R-0517, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** delegated — legality verification of the QBE-style IR is LCB's (its F04/V02); Concrete validates adapter output under R-0517.

**Objective:** Define and gate QBE-IL legality independently of execution. Add a pure verifier for Concrete's typed QBE AST (definition-before-use where QBE requires it, unique temporaries/blocks/symbols, legal base/extended type positions, phi predecessor agreement, terminator completeness, entry-block restrictions, call/return ABI agreement, aggregate definitions before use, alignment validity, and escaped identifier/string syntax), then run the pinned `qbe` parser as a second validation layer. An emitter failure must be a structured Concrete diagnostic with the `.qbe` artifact retained; raw QBE/assembler/linker errors must not be the first user-facing diagnosis for a compiler-owned invalid artifact.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0101).

## Area: Flagship Depth And Examples

### Task R-0120

**Phase:** 7

**Pull trigger:** Phase 7 entry, when `secure_update_verifier` graduates as a public flagship (R-0119) and its anti-rollback state model can be stated.

**Reconciliation (2026-10-19):** Keep one bounded, precisely scoped theorem first. The theorem states its durable anti-rollback model: which version is committed, when, how it binds the accepted payload, and the outcome after interruption or two concurrent updates — with crash/interleaving controls — or it excludes persistent anti-rollback enforcement from its claim as an offline verifier.

**Objective:** Add a stretch theorem for `secure_update_verifier`: manifest-to-payload binding, version monotonicity / rollback prevention, or trusted-key rotation invariant. This is the flagship theorem that demonstrates fail-closed evidence for a real systems-security workflow.

### Task R-0121

**Phase:** 7

**Pull trigger:** Phase 7 entry, once Phase 6's state and totality prerequisites (R-0524, R-0526) let `protocol_state_machine` state its transition theorem.

**Reconciliation (2026-10-19):** Keep after state/totality prerequisites.

**Objective:** Add a stretch theorem for `protocol_state_machine`: success-path / failure-completeness or invalid-transition refusal theorem once proof ergonomics support it.

### Task R-0122

**Phase:** after-1.0/research

**Pull trigger:** a stronger real-crypto candidate (Ed25519 verification, AEAD or a post-quantum primitive) is needed for a new public claim that HMAC cannot carry.

**Reconciliation (2026-10-19):** Keep explicit forcing-claim gate.

**Objective:** Audit the next stronger real-crypto candidate only if it forces a new public claim:
Ed25519 verification subset, AEAD, or a post-quantum primitive. It extends or replaces HMAC's
portfolio role; it does not automatically create another public flagship.

### Task R-0123

**Phase:** 4

**Pull trigger:** none of its own — superseded; its remaining obligations are pulled with R-0495, R-0512, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** superseded by R-0495/R-0512: retire new legacy-model semantics; add admitted Core features/proof lemmas instead.

**Objective:** Add only the ProofCore surface that candidate forces: shifts, bitand, u32 compound loops, rotations, byte-to-word packing, and multi-round invariants.

### Task R-0127

**Phase:** 4, 5, 6, 7

**Pull trigger:** Phase 4 entry, when the admitted slice covers the flagship's bounded parser or validator and R-0460's Core-to-VC soundness covers its OOB, division and overflow obligations.

**Reconciliation (2026-10-19):** Keep with R-0460 soundness and exact loop binding.

**Objective:** Satisfy the runtime-error-obligation claim through the `secure_update_verifier` or
`protocol_state_machine` flagship: demonstrate that no OOB/div-zero/overflow obligation remains
undischarged on its bounded parser/validator, with the evidence visible in the bundle.

### Task R-0131

**Phase:** 7

**Pull trigger:** none of its own — a duplicate; its remaining obligations are pulled with R-0119, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** duplicate of R-0119: merge as portfolio acceptance criterion.

**Objective:** Keep the three-program portfolio collectively balanced across parser/protocol,
bounded state, crypto/security, authority, FFI/trust and ownership-heavy behavior. An axis may have
multiple fixtures or workloads, but the public flagship count remains three.

### Task R-0136

**Phase:** after-1.0/research

**Pull trigger:** a user or flagship needs an enforced (not reported) constant-time claim; pulled together with R-0225's constant-time profile.

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

## Area: Proof Authoring And Automation

### Task R-0152

**Phase:** 4

**Pull trigger:** Phase 4 entry, or earlier when diagnosing a failed obligation requires hand-cutting a reproducer from a large program.

**Reconciliation (2026-10-19):** Keep; use checked Core and canonical proposition artifacts.

**Objective:** Add proof minimization: `concrete prove --minimize <obligation_id>` emits the smallest source / ProofCore / Lean slice needed to reproduce a failed obligation. Output directory:

 `.build/prove/<function>/<obligation_id>/minimized/` with `source.con`,
 `proofcore.lean`, `replay.lean`, `context.json`, and `README.md`. Wire
 `scripts/tests/check_prove_minimize.sh` with one loop VC, one failed
 postcondition, one stale proof, and one SMT counterexample. The minimized
 artifact must reproduce the same status and stable id without unrelated
 functions.
### Task R-0158

**Phase:** after-1.0/research

**Pull trigger:** the CLI/JSON/stub/workspace proof surfaces are stable and an agent integration asks for MCP resources rather than CLI calls.

**Reconciliation (2026-10-19):** Keep; no prerequisite for compiler proofs.

**Objective:** Add MCP only after the CLI/JSON/stub/workspace surfaces are stable. The MCP server should wrap the binary rather than duplicate logic, exposing resources such as `concrete://prove/<fn>/obligations`, `concrete://proofkit/lemmas`, and `concrete://examples/evidence-classes`, plus tools for `prove_json`, `show_obligation`, `emit_lean`, `check`, `replay`, and `check_proofs`.

### Task R-0159

**Phase:** 4

**Pull trigger:** Phase 4 entry as V2 admits arrays; earlier when a second array proof re-proves lookup/update/length facts by hand.

**Reconciliation (2026-10-19):** Keep; replace “OOB stuck” with defined bounds-trap semantics. Out-of-bounds access is a defined bounds trap in Core, not stuck execution; lemmas target the trap semantics.

**Objective:** Build reusable proof lemmas for arrays: lookup, update, length, in-bounds, OOB stuck behavior.

### Task R-0162

**Phase:** 4

**Pull trigger:** Phase 4 entry as V2-V3 admit structs, enums, `Option` and `Result`; earlier when a second proof re-derives the same field or match facts.

**Objective:** Build reusable lemmas for structs, fields, enum construction, match, Result, Option, and bounded-buffer invariants.

### Task R-0165

**Phase:** 4

**Pull trigger:** Phase 4 entry, when loop proofs over the admitted slices repeat one of the listed shapes (counter, copy, fold, multi-store, offset, block) a second time.

**Objective:** Add generated loop-invariant templates for common proof shapes:

 counter loop over array writes, copy loop, fold loop, multi-store loop,
 offset loop, and block-processing loop.
### Task R-0167

**Phase:** after-1.0/research

**Pull trigger:** proof artifacts, statuses and replay are stable enough that a synthesized proof can be validated mechanically, and users ask for invariant or lemma synthesis.

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

**Phase:** after-1.0/research

**Pull trigger:** proof checking time becomes a measured bottleneck once proof artifacts and fingerprints are stable (the `ProofCache` performance pull).

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

**Phase:** 4

**Pull trigger:** Phase 4 entry, or earlier when structural obligations (field projection, constructor round trips, tag preservation, literal lengths) dominate hand-written proof links.

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

**Phase:** 4

**Pull trigger:** Phase 4 entry, after R-0169's structural tier, when ordinary `#[ensures]` bodies and loop-preservation steps still need hand-written proofs.

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

**Phase:** 6

**Pull trigger:** Phase 6 entry with R-0526's specification library; earlier when a collection proof would otherwise mention buckets, tombstones or capacity.

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

**Phase:** 6

**Pull trigger:** Phase 6 entry, when a collection contract needs a bounded quantifier and a bounded fragment has examples and gates.

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

**Phase:** after-1.0/research

**Pull trigger:** proof artifacts, statuses and replay are stable enough to validate repair suggestions mechanically, and stale proofs after source drift become a recurring cost.

**Reconciliation (2026-10-19):** Keep; share R-0166 diagnostics and R-0167 artifacts.

**Objective:** Add AI-assisted proof repair only after artifacts, statuses, and replay are stable enough to validate suggestions mechanically. The binary surface is `concrete prove --repair-plan <obligation_id> --json`; it emits no edited source, only candidate next actions with required checks. Required JSON fields: `obligation_id`, `failure_class`, `minimal_artifact`, `suggested_lemma`, `suggested_imports`, `candidate_tactic`, `validation_command`, and `risk`. Wire `scripts/tests/check_proof_repair_plan.sh` with missing theorem, stale proof, missing frame fact, arithmetic bridge failure, and spec mismatch.

 No repair suggestion may change a proof status until `--check` or
 `check-proofs` verifies it. A convenience spelling such as
 `concrete prove <file> <fn> --repair` may exist only as an alias for the
 same repair-plan artifact; it must not edit source by default and must never
 upgrade evidence without replay.
### Task R-0175

**Phase:** 6

**Pull trigger:** Phase 6 entry with R-0524's frames, and only when a second update shape forces frame reasoning that the functional-list model does not give for free.

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

**Phase:** 4

**Pull trigger:** none of its own — superseded; its remaining obligations are pulled with R-0495, R-0512, R-0151, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** superseded by R-0495/R-0512: retire proposed legacy architecture; retain ownership goals in R-0151.

**Objective:** Deferred architecture refactor: split the current `Concrete.Proof` layering so registered example specs can move without a cycle, but do not let this block the active frontier unless spec ownership or proof authoring starts depending on it. Target shape: `Concrete.ProofCore` owns `PExpr`, `PVal`, evaluation, `FnTable`, and source-independent semantics; `Concrete.SpecRegistry` owns the spec-drift table and imports whichever example spec modules it registers;

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0176).

## Area: Audit Commands And Review Artifacts

### Task R-0178

**Phase:** 5

**Pull trigger:** Phase 5 entry with receipt v1 (R-0338); earlier when an external consumer depends on one of the listed fact schemas.

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

**Phase:** 5

**Pull trigger:** Phase 5 entry, or earlier when a reviewer must assemble authority, trust, proof and assumption facts by hand for one program.

**Reconciliation (2026-10-19):** Keep; render R-0494 verdicts.

**Objective:** Add `concrete audit`: one human-readable plus machine-readable bundle covering authority, trust, allocation, proof status, obligations, assumptions, policy, snapshots, backend/target assumptions, replay, and the proof-story matrix specialized to the audited program.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0185** (merged): duplicate of R-0179: merge into R-0179.
- **R-0249** (merged): duplicate of R-0178/R-0179: merge into R-0178/R-0179.

### Task R-0182

**Phase:** 5

**Pull trigger:** Phase 5 entry; the three-family increment is pulled earlier by the verified-component alpha's change-and-repair scenario (R-0486).

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

**Phase:** 5

**Pull trigger:** Phase 5 entry; the alpha increment is pulled earlier with R-0182 by the verified-component alpha (R-0486).

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

**Phase:** 5

**Pull trigger:** none of its own — a duplicate; its remaining obligations are pulled with R-0179, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** duplicate of R-0179: merge into R-0179.

**Objective:** Add `concrete audit --json`: machine-readable audit output for CI, dashboards, editor tooling, and release bundles.

### Task R-0186

**Phase:** 5, 7

**Pull trigger:** Phase 5 entry, when the evidence product (R-0521, R-0353) can back a `verified` profile policy without overclaiming.

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

**Phase:** 5, 7

**Pull trigger:** Phase 5 entry, when users have release bundles and diffs worth browsing outside JSON.

**Objective:** Add an artifact viewer CLI/TUI over facts, obligations, proofs, assumptions, release bundles, and diffs. It may show a compact dashboard, but it must never collapse different evidence classes into one fake green badge: `proved`, `runtime_checked`, `tested_by_oracle`, `assumed`, and `trusted` stay distinct on screen and in JSON.

### Task R-0188

**Phase:** 5, 7

**Pull trigger:** Phase 5 entry with receipt v1, or the first release bundle that ships evidence.

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

**Phase:** 6

**Pull trigger:** Phase 6 entry with typed contracts (R-0473), or earlier when contract authors ask for counterexamples before proof.

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

**Phase:** after-1.0/research

**Pull trigger:** Phase 6 contracts exist and pilots show users need help writing their first specs from tests.

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

**Phase:** 7

**Pull trigger:** Phase 7 entry, when the audit bundle (R-0179) and fact schemas (R-0178) are stable enough for an agent to answer from them.

**Reconciliation (2026-10-19):** Keep as workflow fixture.

**Objective:** Add one AI-audit demo where an agent answers authority/proof/trust questions using compiler facts rather than source guesses.

### Task R-0196

**Phase:** 5, 7

**Pull trigger:** Phase 5 entry with R-0182's diff, or when reviewers of a dependency or source change ask for a generated checklist.

**Objective:** Add review checklists generated from facts: what changed, what widened, what became trusted, what lost proof, what gained assumptions, and which obligations remain open.

### Task R-0197

**Phase:** 5

**Pull trigger:** Phase 5 entry, or before the first release bundle that is shared publicly.

**Reconciliation (2026-10-19):** Keep; preserve replayability and identity.

**Objective:** Add artifact redaction/stability rules so release bundles can be shared publicly without leaking local paths, secrets, or machine-specific noise.

### Task R-0200

**Phase:** 5

**Pull trigger:** Phase 5 entry, when `concrete audit` (R-0179) exists to render the compiler's own ledger.

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

**Phase:** 5, 7

**Pull trigger:** Phase 5 entry, when R-0179, R-0182 and R-0187 exist to be combined into one package-scale validation artifact.

**Reconciliation (2026-10-19):** Keep, staged with available producers.

**Objective:** Add the audit-commands area validation artifact: one package-scale audit bundle fixture with human and JSON output, semantic diff before/after a change, artifact viewer smoke test, oracle manifest, property-test manifest, persisted counterexample regression, spec-provenance facts, redaction check, replay command, and a README showing how a reviewer answers authority, proof, trust, assumption, and runtime-obligation questions without reading compiler internals. When the incremental-driver area exists, cold, warm, and cache-off audit/diff/why output must be identical after normalizing timing/cache-observability fields, and corrupt/stale query artifacts must recompute or fail closed; otherwise

 the same fixture runs eagerly with no private caching.

## Area: Proof Status And Trust Gates

### Task R-0212

**Phase:** 5, 7

**Pull trigger:** Phase 5 entry, or when a graduated flagship (R-0119) or release policy must refuse an unreviewed spec.

**Objective:** Add spec-adequacy gates: release policy can require reviewed spec provenance for selected claims, forbid unreviewed specs in graduated flagships, and show when a theorem is `proved_by_lean` against a `spec_trusted` or unreviewed spec.

### Task R-0214

**Phase:** after-1.0/research

**Pull trigger:** a user needs claims that rest on an external SMT solver beyond Z3, or a solver disagreement is observed on an admitted fragment.

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

**Phase:** after-1.0/research

**Pull trigger:** R-0214's evidence classes exist and a reviewer needs `solver_trusted` reduced to a replayable certificate.

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

**Phase:** 5; after-1.0/research

**Pull trigger:** Phase 5 entry with receipt v1 and R-0440's typed evidence; earlier when a surface reports `proved_by_two_kernels` as stronger than its evidence.

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

### Task R-0220

**Phase:** 4, 6

**Pull trigger:** Phase 4 entry, when R-0445's resource facts are frozen for the admitted slices, or a freestanding or embedded user needs a predictable profile.

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

**Phase:** after-1.0/research

**Pull trigger:** a workload or flagship (for example R-0136) needs a constant-time claim; native timing still requires separate transport.

**Reconciliation (2026-10-19):** Keep; no native timing guarantee without separate transport.

**Objective:** Define source-level constant-time profile v1:

no secret-dependent branch, no secret-dependent memory index, fixed loop
bounds, explicit backend timing assumptions.
### Task R-0226

**Phase:** after-1.0/research

**Pull trigger:** pulled together with R-0225's security-profile design when a constant-time or secrecy claim is needed.

**Reconciliation (2026-10-19):** Merge into R-0225’s security-profile design.

**Objective:** Define secret/data-sensitivity labels for future security work:

`public`, `secret`, `timing-sensitive`.
### Task R-0229

**Phase:** 4

**Pull trigger:** none of its own — superseded; its remaining obligations are pulled with R-0495, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** superseded by R-0495 and V2–V4: merge into slice admission/claim coverage.

**Objective:** Decide the proof class for references and borrows. A function using `&` or `&mut` must be classified as one of: value-only/borrow-free, proved over read-only references, proved over mutable references with explicit frame/modifies obligations, or enforced-only and outside `ProvableV1`. Do not let borrow-using code appear proof-eligible through a value-only ProofCore model.

### Task R-0234

**Phase:** 4

**Pull trigger:** none of its own — superseded; its remaining obligations are pulled with R-0496, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** superseded by R-0496/S7, V7: merge into V7 float-profile work.

**Objective:** Define `ProvableFloatV1` as a separate, narrow proof profile:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0234).

### Task R-0235

**Phase:** 4

**Pull trigger:** none of its own — superseded; its remaining obligations are pulled with R-0496, R-0495, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** superseded by R-0496/R-0495: retire PVal/PBinOp expansion; implement Core float semantics.

**Objective:** Add ProofCore support for profiled floats only after Task R-0233 is closed:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0235).

### Task R-0237

**Phase:** 4

**Pull trigger:** Phase 4 entry, after V7's float profile (`ProvableFloatV1`) exists.

**Reconciliation (2026-10-19):** Keep as fixture, not another flagship.

**Objective:** Add one small `ProvableFloatV1` proof-profile witness only after the profile exists.
It is a fixture/workload, not a fourth public flagship:

 a fixed-order `f32`/`f64` kernel such as clamp/normalize, tiny FIR/IIR, PID,
 or dot product. Prove exact IEEE behavior first; real-valued epsilon-bound
 refinement is a later layer.
### Task R-0239

**Phase:** 6

**Pull trigger:** Phase 6 entry with R-0524's frames; its syntax freezes only when a flagship or external workload pulls it.

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

**Phase:** after-1.0/research

**Pull trigger:** users ask for a test-time `expect` that is visibly not proof, runtime safety or a release assumption.

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

**Phase:** 4, 6; after-1.0/research

**Pull trigger:** Phase 4 entry for non-arithmetic families the admitted slices support (exhaustiveness first); a parser workload that yields zero obligations shows the gap.

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

### Task R-0244

**Phase:** 6

**Pull trigger:** Phase 6 entry with R-0526's `#[decreases]`/totality work, or a predictable-profile user (R-0220) who needs loop bounds.

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

**Phase:** 5, 7

**Pull trigger:** Phase 5 entry, when release policy (R-0186) can require overflow obligations for selected packages.

**Reconciliation (2026-10-19):** Keep; runtime checked arithmetic remains unconditional.

**Objective:** Define policy gates for `#[overflow_checked]`: release profiles may require overflow obligations for selected functions/packages, while ordinary examples remain quiet unless they opt in. Reports must distinguish `overflow_checked`, `overflow checking not requested`, and explicit wrapping or saturating arithmetic.

### Task R-0246

**Phase:** 4, 6

**Pull trigger:** Phase 4 entry, when the admitted slices reach unwrap-like operations and abort paths, or a user needs a no-panic denial-of-service claim.

**Reconciliation (2026-10-19):** Keep; distinguish deliberate permitted traps from no-trap claims.

**Objective:** Generate obligations for panic/abort/assert-as-denial-of-service risks:

unchecked indexing, unwrap-like operations, explicit abort paths, failed
assertions, and profile-dependent panic behavior.
### Task R-0247

**Phase:** 4, 6

**Pull trigger:** Phase 4 entry, when V3-V4 admit byte, text and path conversions, or a workload hits a lossy or invalid UTF-8 conversion.

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

**Phase:** 5

**Pull trigger:** none of its own — a duplicate; its remaining obligations are pulled with R-0178, R-0179, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** duplicate of R-0178/R-0179: merge into R-0178/R-0179.

**Objective:** Report runtime-error obligations in human and JSON forms.

### Task R-0250

**Phase:** 5, 7

**Pull trigger:** none of its own — a duplicate; its remaining obligations are pulled with R-0186, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** duplicate of R-0186: merge into R-0186.

**Objective:** Add policy gates that can require selected runtime-error obligations to be proved/enforced before graduation.

### Task R-0252

**Phase:** 7

**Pull trigger:** Phase 7 entry, when the flagships graduate (R-0119) and R-0127's runtime-error claim exists to require.

**Objective:** Add a runtime-error-obligation portfolio requirement: `secure_update_verifier` or
`protocol_state_machine` must demonstrate no OOB/div-zero/overflow under a named profile.

### Task R-0256

**Phase:** 4, 6

**Pull trigger:** Phase 4 entry, when annotation burden on the admitted slices is measured to block users; its facts never become trusted proof.

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

**Phase:** 4, 6

**Pull trigger:** Phase 4 entry, once loop, frame and dependency facts exist on the admitted slices.

**Objective:** Connect runtime-safety obligations to loop, frame, and dependency facts.

 Bounds/cast/overflow obligations should be dischargeable from explicit loop
 invariants, generated invariant candidates, and future `reads`/`writes`/
 `modifies` facts without duplicating the ledger. The report must show which
 invariant or frame fact discharged each obligation, and missing facts must
 produce actionable diagnostics rather than generic "unproven" output.
### Task R-0258

**Phase:** 6

**Pull trigger:** Phase 6 entry with typed contracts, or a validated-wrapper workload whose invariant must be a proof hypothesis.

**Reconciliation (2026-10-19):** Keep; include mutation/re-establishment rules. Invariants must be re-established after writable access, moves through representations and foreign mutation, not only at construction; open/close rules and negative fixtures (with S8).

**Objective:** Add newtype/type invariants as scoped obligation hypotheses after they are checked at construction boundaries. This connects validated wrappers to the proof/runtime-safety pipeline: a type such as `#[invariant(self.0 > 0 && self.0 < 65536)] newtype Port = u16` must have the invariant proved/enforced at every constructor or `try_new` admission point, then injected as a named hypothesis for obligations in any function receiving a `Port`. No invariant may enter scope from a raw cast, trusted constructor, FFI boundary, stale proof, or unchecked assumption without a ledger entry saying so. Add `docs/NEWTYPE_INVARIANT_OBLIGATIONS.md`,

 `examples/newtype_invariants/{port,nonzero_len,bounded_index,ffi_trusted}/`,
 and `scripts/tests/check_newtype_invariant_obligations.sh`; the gate must
 prove constructor checks create reusable hypotheses, invalid constructors
 are rejected or return `Result`, raw/trusted paths do not silently inject
 facts, and obligation reports name the type invariant source.

### Task R-0452

**Phase:** 6; after-1.0/research

**Pull trigger:** Phase 6 entry for the zlib-inflate density workload, once obligation discharge on the admitted slices is stable enough to measure density.

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

**Phase:** after-1.0/research

**Pull trigger:** the linear-arithmetic tier's lack of transferable certificates blocks an independent consumer (R-0353) or a release claim.

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

### Task R-0262

**Phase:** 4

**Pull trigger:** none of its own — superseded; its remaining obligations are pulled with R-0495, R-0512, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** superseded by R-0495/R-0512: retire blanket legacy bridge; retain only migration-pulled bridges.

**Objective:** Upgrade extraction-only rules to three-view preservation where practical:

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0262).

### Task R-0271

**Phase:** 4, 6

**Pull trigger:** none of its own — superseded; its remaining obligations are pulled with R-0510, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** superseded by R-0510/S8a, V5: merge into R-0510, retaining explicit law-evidence acceptance.

**Objective:** Define proof preservation across monomorphization and capability-polymorphic instantiation. The compiler must report whether a theorem proves a specific generated instance (`proved_for_instance`) or a generic body (`proved_generic`), and it must prevent one instance proof from being presented as proof for every future instantiation.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0271).

### Task R-0443

**Phase:** 4, 7

**Pull trigger:** Phase 4 entry, when V5 admits capabilities and a user needs an independently checkable whole-program authority property.

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

**Phase:** after-1.0/research

**Pull trigger:** none of its own — superseded; its remaining obligations are pulled with R-0496, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** superseded by R-0496 for proposed PExpr port: retire PExpr-port design; reconsider only a measured Core-metatheory proposal.

**Reconciliation (2026-10-19):** The "no single kernel can bridge two kernels" claim conflates formal correspondence with trust in external executions; any renewed proposal targets Core.

**Objective:** Port `eval` to a second host and relate the two semantics by an
adversarial conformance vector suite.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0456).

## Area: Backend, Target, And Stdlib Contracts

### Task R-0284

**Phase:** after-1.0/research

**Pull trigger:** a workload that needs source-visible alignment facts beyond R-0502's foundational layout alignment.

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

**Phase:** none — retired; see the disposition below

**Pull trigger:** none of its own — retired; its remaining obligations are pulled with R-0517, R-0505, and it reopens only if that disposition is reversed.

**Ledger:** retired — owners R-0517, R-0505; see the [reconciliation record](ROADMAP_RECONCILIATION.md).

**Disposition (2026-10-19): retired — superseded by R-0517/R-0505.** There is no Concrete QBE backend to
graduate; the native profile is LCB-backed. Its graduation requirements (validated backend input, source maps,
ABI/target matrices, incremental artifacts, honest translation-validated versus backend-trusted labels) apply
to the LCB-native profile under R-0517/R-0505. Wasm, Cranelift and further backends stay deferred.

**Objective (historical):** Graduate the QBE backend-validation area QBE backend from experimental to release-supported only after evidence attachment, optimization policy, backend-IR verification, and backend trust boundaries are trustworthy. The QBE backend-validation area deliberately emits directly from validated/cleaned SSA to maximize independence and find bugs early; that is the initial usable backend architecture, not yet the release architecture.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0296).

### Task R-0297

**Phase:** none — delegated; see the disposition below

**Pull trigger:** none of its own — delegated; its remaining obligations are pulled with R-0517, R-0505, and it reopens only if that disposition is reversed.

**Ledger:** delegated — owners R-0517, R-0505; see the [reconciliation record](ROADMAP_RECONCILIATION.md).

**Disposition (2026-10-19): delegated to the Lean Compiler Backend.** LCB owns machine IR, instruction
selection, register allocation and encoding with their proofs; Concrete consumes it under R-0517 and R-0505.
The Concrete-hosted comparison stays with R-0413. The text below is retained as historical rationale; the
"small owned backend investigation" it describes became the separate LCB project.

**Objective (historical):** Supply target-specific Machine IR for R-0505's native AArch64 profile. QBE is independent and is not a prerequisite; R-0505 owns scheduling and native proof/validation exits. The staged shape is `ValidatedBackendIR -> <target> MIR -> encoded object fragment`, with MIR owning instruction selection, physical-register constraints, calling- convention realization, relocations, and debug locations. The archived continuation records the historical reuse requirements.

The rest of this task's original body is archived in [ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md#r-0297).

### Task R-0303

**Phase:** after-1.0/research

**Pull trigger:** a compiler-in-Concrete or verified self-host workload that is blocked by one of the listed language features; only that feature is pulled.

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
   way; nested patterns is tracked in `docs/language/NESTED_PATTERNS.md`.)

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

**Phase:** after-1.0/research

**Pull trigger:** a user or workload needs a sandboxed or Wasm32 target, after validated BIR (R-0503) exists.

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Target Wasm32 with a pinned WebAssembly toolchain and one documented WASI profile. Browser integration, the component model, Wasm GC, Wasm64, threads, SIMD, and exception handling are explicitly later work unless a forcing workload promotes one of them.

**Inherited obligations (2026-10-19 reconciliation; binding):** the acceptance criteria,
regression cases and controls of these tasks transfer here unless the reason retires them.

- **R-0100** (superseded): superseded — Wasm/Cranelift no longer wait for QBE; the WebAssembly area entrance criteria (R-0305) consume validated BIR. — [archived body](../archive/ROADMAP_RETIRED_TASKS.md#r-0100)

### Task R-0306

**Phase:** after-1.0/research

**Pull trigger:** R-0305 is pulled and validated BIR (R-0503) exists.

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Lower validated BIR (R-0503) into a small typed Concrete WebAssembly AST, run a pure Concrete verifier, emit deterministic `.wasm`, and validate it with the pinned external validator. Do not emit ad-hoc WAT from CLI code and do not route WebAssembly through LLVM.

### Task R-0307

**Phase:** after-1.0/research

**Pull trigger:** pulled with R-0306, when the WebAssembly lowering starts.

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Reuse canonical semantic, layout, builtin, trap, source-identity, and capability facts, while keeping WebAssembly instruction selection and control-flow restructuring independent from LLVM and LCB lowering.

### Task R-0308

**Phase:** after-1.0/research

**Pull trigger:** pulled with R-0306; each slice is taken in the listed order.

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Deliver thin vertical slices in this order: `main -> 42`; direct calls and returns; branches/loops; integer operations and checked traps; linear-memory loads/stores; basic aggregates; runtime imports. Each slice ends in an executable differential fixture.

### Task R-0309

**Phase:** after-1.0/research

**Pull trigger:** the first WebAssembly program needs external authority (R-0308's runtime-imports slice).

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Map external authority deliberately onto an explicit WASI import manifest.

Missing capabilities and unsupported operations fail before partial code
generation with stable `wasm_pending` diagnostics; there is no LLVM/LCB or
per-function fallback.
### Task R-0310

**Phase:** after-1.0/research

**Pull trigger:** a WebAssembly slice must run through ordinary `build`/`run`/`test` workflows.

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Integrate `--backend wasm`, `--emit-wasm`, projects, retained artifacts, debug bundles, replay commands, caching, `doctor`, feature JSON, audit output, and ordinary `run`/`test` workflows through the backend-neutral driver.

### Task R-0311

**Phase:** after-1.0/research

**Pull trigger:** the first WebAssembly slice exists to compare against Core semantics, LLVM and LCB-native.

**Reconciliation (2026-10-19):** Keep; no QBE prerequisite (R-0113); consumes validated BIR (R-0503); compares against LLVM and LCB-native.

**Objective:** Compare Core-semantics, LLVM, LCB-native and WebAssembly observations for every eligible fixture: stdout, stderr, exit status, and normalized trap/failure class. Add Wasm-specific cases for structured control flow, linear-memory bounds, alignment, integer-width conversion, imports, and deterministic module composition.


Done when: the declared Wasm32/WASI subset builds and runs at least one the flagships area-class workload through normal user workflows; its capability matrix is
complete and fail-closed; emitted modules and manifests are deterministic and
independently validated; the four-way differential suite covers the supported
subset; and audit/evidence output labels WebAssembly results honestly as
`tested` or `backend_trusted`, never as proof that native or source semantics
are preserved.

## Area: Unsafe Concrete And Trusted Systems Profile

### Task R-0315

**Phase:** 4

**Pull trigger:** Phase 4 entry with V6 (raw pointers, `Unsafe`, FFI), or a trusted slice or pointer bug that the checked profile would have caught.

**Reconciliation (2026-10-19):** Keep; cannot erase safe-Core checks without certificates. Opt-outs apply only to explicitly unsafe/trusted instrumentation; safe-Core checks are never erased by reporting alone (operation-specific certificates are required, S12).

**Objective:** Add pointer/slice runtime-instrumentation hooks for unsafe code: null checks, bounds checks for trusted slice views, alignment checks, provenance/trust labels, lifetime/owner debugging where the runtime can track it, and explicit opt-out for trusted performance paths. A checked profile may trap; a release profile may erase checks only when reports still say which checks became assumptions.

Include sanitizer opt-out attributes only as named unsafe/trusted facts
(`no_sanitize_address`-style behavior must appear in audit output), never as
comments or unreported compiler flags.
### Task R-0318

**Phase:** 4

**Pull trigger:** Phase 4 entry with V6, as the pressure workload for raw pointers and slices.

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

**Phase:** after-1.0/research

**Pull trigger:** R-0315 and R-0321's taxonomy and gates exist and unsafe code is measured to be needlessly hard to write or audit.

**Reconciliation (2026-10-19):** Keep gated by taxonomy and auditability.

**Objective:** Add syntax/ergonomics only after the taxonomy and gates exist. Candidate sugar may include pointer-field access or checked slice construction helpers, but it must not reintroduce removed `->` syntax as a general language form and must not blur raw pointers with safe references. Ergonomics serve audit, not the other way around.

Candidate ergonomics must be rejected if they make a raw pointer look like a
safe reference, hide a bounds/null/alignment assumption, or make an unchecked
operation harder to grep.
### Task R-0321

**Phase:** 4

**Pull trigger:** Phase 4 entry, as V6's validation artifact, once R-0318's workload exists.

**Objective:** Add the unsafe/trusted-profile area validation artifact:

 `examples/unsafe_concrete_vm/` plus
 `scripts/tests/check_unsafe_concrete.sh`. The gate must run the workload in
 normal and checked profiles, compare interpreter-vs-compiled behavior where
 applicable, prove audit output lists every trusted/Unsafe/allocator/FFI
 assumption, and include at least one mutation per unsafe class that the
 checked profile or audit gate catches.

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

**Phase:** after-1.0/research

**Pull trigger:** a freestanding or embedded user or workload (no libc, own startup); all freestanding tasks start from this profile definition.

**Objective:** Define `hosted` versus `freestanding` target profiles: libc,
startup, allocator, terminal abort behavior, stack assumptions, I/O
availability, floating-point assumptions, and supported capabilities. Hosted
abort terminates the process; freestanding abort transfers to a declared
non-returning target handler. Neither profile promises cleanup.

### Task R-0323

**Phase:** after-1.0/research

**Pull trigger:** R-0322 is pulled and a freestanding build needs the std modules split by profile.

**Objective:** Split stdlib modules by target profile: core no-alloc/no-OS modules, allocator-backed modules, hosted OS modules, and explicitly unavailable modules.

### Task R-0324

**Phase:** after-1.0/research

**Pull trigger:** R-0322 is pulled; freestanding builds must not see ambient `File`, `Network`, `Console` or `Process`.

**Objective:** Define freestanding capability policy: no ambient `File`, `Network`, `Console`, or `Process`; target-specific capabilities must be declared and audited.

### Task R-0325

**Phase:** after-1.0/research

**Pull trigger:** a device-driver or MMIO workload, after R-0322's profiles exist.

**Reconciliation (2026-10-19):** Keep; no implicit extension of sequential native theorem.

**Objective:** Define hardware-access primitives and evidence classes before device-driver examples: `volatile`/MMIO operations require explicit `with(Mmio)` or `with(Device)` capability, inline assembly is `trusted` and requires `with(Unsafe)`, and interrupt handlers are an effectful/trusted boundary with explicit target assumptions.

### Task R-0326

**Phase:** after-1.0/research

**Pull trigger:** R-0322 is pulled and a freestanding build needs allocation or a defined allocation-failure behaviour.

**Objective:** Add explicit allocator/runtime hooks for freestanding builds, including ownership of allocation failure behavior and cleanup expectations.

### Task R-0327

**Phase:** after-1.0/research

**Pull trigger:** the first freestanding binary needs its own entry symbol, linker script or section layout.

**Objective:** Add linker/startup configuration: entry symbol, no-main mode, target triple, data layout, linker script hooks, and section/layout assumptions.

### Task R-0328

**Phase:** after-1.0/research

**Pull trigger:** the first freestanding build that could silently reach a hosted API, libc call or hidden allocation.

**Objective:** Add freestanding diagnostics: reject hosted APIs, hidden allocation, libc calls, unsupported target features, and unavailable capabilities.

### Task R-0329

**Phase:** after-1.0/research

**Pull trigger:** R-0322-R-0328 support a first freestanding program end to end.

**Objective:** Add one freestanding example: bounded parser, small checksum/hash kernel, or fixed-capacity state machine with no allocation and no hosted I/O.

### Task R-0330

**Phase:** after-1.0/research

**Pull trigger:** R-0329's example exists and its remaining target assumptions must be named for review.

**Objective:** Add one embedded-style audit bundle naming all remaining target assumptions:

stack, interrupt model if any, allocator/runtime hooks, endian/layout, and
backend/toolchain boundary.
### Task R-0331

**Phase:** after-1.0/research

**Pull trigger:** a freestanding profile proves the LLVM and LCB-native paths are insufficient for a target someone needs.

**Reconciliation (2026-10-19):** Keep; baseline updated to the LLVM and LCB-native paths (R-0517).

**Objective:** Keep production freestanding Wasm and additional backend targets deferred until freestanding profiles prove the LLVM and LCB-native paths are not enough. Any LCB freestanding target, runtime/startup contract, or release claim graduates here and through R-0505/R-0517's profile contract; LCB's own target breadth (its P10) is an external dependency, not Concrete work. (Updated 2026-10-19: the QBE backend-validation area's QBE differential oracle is retired in favour of the LCB-native leg of R-0491's differential harness.)

### Task R-0332

**Phase:** after-1.0/research

**Pull trigger:** R-0329 and R-0330 exist and the freestanding area needs its validation artifact.

**Objective:** Add the freestanding area validation artifact: one freestanding demo project plus an MMIO/device-profile mock audit bundle. The demo must build with no hosted APIs, name allocator/startup/linker assumptions, reject hidden libc or allocation, and report `with(Device)`/`with(Mmio)`/`with(Unsafe)` evidence classes without pretending hardware behavior is proved.


### Task R-0453

**Phase:** after-1.0/research

**Pull trigger:** the freestanding profile (R-0322) exists and a device-plus-crash-model workload is wanted; littlefs is the named candidate.

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

## Area: Public Release Bar

### Task R-0344

**Phase:** 6, 7

**Pull trigger:** Phase 6 entry with R-0239's SPARK-class contracts, or before any public comparison claim against SPARK.

**Reconciliation (2026-10-19):** Keep conditional on claims and available frame/flow facts.

**Objective:** Add a certification-style assurance bundle profile before any SPARK-class comparison claim. The bundle must include the claim matrix, authority/capability report, obligation ledger, proof status, runtime-safety status, assumptions, trusted boundaries, package/dependency evidence, toolchain versions, replay commands, and any SPARK-class flow/frame facts.

 It must also include an agent-readable summary naming which annotations were
 checked and which were only suggested or future-only. This is a release
 artifact over existing facts, not a second evidence system.
### Task R-0350

**Phase:** 7

**Pull trigger:** Phase 7 entry, before the project beta publishes comparison material.

**Reconciliation (2026-10-19):** Keep; derive claims from current matrix.

**Objective:** Add positioning page against Rust, Zig, Lean, SPARK/Ada, Austral, Hylo, Cogent, Dafny, F*, Why3.

## Area: Post-Release Operations And Stewardship

### Task R-0430

**Phase:** 5, 7

**Pull trigger:** Phase 5 entry with receipt v1 (evidence revocation); package yanks are pulled with Phase 7's package work (R-0440), and any earlier published release that must revoke a compromised dependency or proof pulls it at once.

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

### Task R-0369

**Phase:** 5, 7

**Pull trigger:** Phase 5 entry with receipt v1, before any package certificate is consumed.

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

**Phase:** 7

**Pull trigger:** Phase 7 entry with R-0440's package evidence.

**Reconciliation (2026-10-19):** Keep; consume R-0494 judgments rather than new verdict logic.

**Objective:** Add proof-aware package artifacts: facts, obligations, proof status, trusted assumptions, policy declarations, package-boundary evidence summaries.

Required statuses: `proved_by_lean`, `proved_by_kernel_decision`,
`solver_trusted`, `tested_by_oracle`, `enforced`, `assumed`, `trusted`,
`partial`, `stale`, `vacuous`, `missing`, and `ineligible`. The artifact
must record whether evidence is package-local, inherited from a dependency,
or trusted through a boundary.
### Task R-0371

**Phase:** 7

**Pull trigger:** Phase 7 entry, when package facts or reusable artifacts from a dependency are first consumed.

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

**Phase:** 7

**Pull trigger:** Phase 7 entry, once package graphs are real; earlier if a dependency's capability widening goes unnoticed in a workload.

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

**Phase:** 7

**Pull trigger:** Phase 7 entry with R-0440's typed policy.

**Reconciliation (2026-10-19):** Keep as R-0440/R-0372 policy dimension.

**Objective:** Add dependency trust policy: trust widening across boundaries, review and inheritance.

### Task R-0374

**Phase:** 7

**Pull trigger:** Phase 7 entry, when a dependent's release bundle must show its dependencies' assumptions.

**Objective:** Add package-level assumption inheritance: dependency assumptions must be visible to dependents and release bundles.

### Task R-0375

**Phase:** 7; after-1.0/research

**Pull trigger:** Phase 7 entry for local provenance; the publishing model is pulled when packages are first published outside the repository.

**Reconciliation (2026-10-19):** Keep; split local provenance from hosted publication.

**Objective:** Add package provenance and publishing model.

### Task R-0376

**Phase:** after-1.0/research

**Pull trigger:** packages are published outside the repository and a registry is needed; after R-0375's provenance model.

**Reconciliation (2026-10-19):** Keep deferred; not beta prerequisite.

**Objective:** Add package registry server protocol and trust model.

### Task R-0378

**Phase:** after-1.0/research

**Pull trigger:** none of its own — a duplicate; its remaining obligations are pulled with R-0377, and it reopens only if that disposition is reversed.

**Disposition (2026-10-19):** Duplicate of R-0377 in export scope: merge export requirements into R-0377; retain hosting as deferred increment.

**Objective:** Add package documentation hosting/export format only after `concrete doc` and package artifacts are stable: static HTML, JSON docs, versioned stdlib docs, package docs, release-note links, and `scripts/tests/check_docs_publish.sh` to prove generated docs are reproducible.

### Task R-0379

**Phase:** 7

**Pull trigger:** Phase 7 entry, before the package fact schema freezes.

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

**Phase:** 6, 7

**Pull trigger:** Phase 6 entry with R-0524's frame and dependency contracts, then Phase 7 for package interfaces.

**Reconciliation (2026-10-19):** Keep after underlying facts exist.

**Objective:** Add package-level SPARK-class assurance summaries once frame/dependency contracts exist. Interface artifacts should expose public contract facts, read/write/modifies summaries, dependency-flow summaries, ghost/spec assumptions, capability requirements, trusted boundaries, and evidence class per public function. Package consumers and agents must be able to ask "what may this dependency read, write, depend on, assume, trust, or prove?" without inspecting private bodies.

### Task R-0381

**Phase:** 5, 7

**Pull trigger:** Phase 5 entry with receipt v1's canonical encoders, or a nondeterministic package/evidence hash.

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

**Phase:** after-1.0/research

**Pull trigger:** the local incremental store (incremental-driver area) and independent package verification are mature, and builds are measured to need remote reuse.

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

### Task R-0389

**Phase:** after-1.0/research

**Pull trigger:** the minimal editor path (R-0385-R-0388) exists and users ask for refactors that keep facts and proofs.

**Reconciliation (2026-10-19):** Keep after minimal editor path.

**Objective:** Add refactor support that preserves or updates facts/proofs where possible.

### Task R-0390

**Phase:** 7

**Pull trigger:** Phase 7 entry, when package facts (R-0370) exist to show in the editor.

**Reconciliation (2026-10-19):** Keep after package facts.

**Objective:** Add dependency audit UI for capability, allocation, FFI, trust, evidence, predictability, proof-obligation drift.

The UI must expose the same boundary facts as Task R-0372, not a prose
summary: public capabilities, allocation authority, trusted/Unsafe/extern
use, assumptions, evidence floor, platform, license, and source hash. This
is explicitly for humans and AI agents reviewing imports: an editor hover or
command palette action should answer "what can this dependency do, what does
it assume, and what evidence does it carry?" without reading private bodies.
### Task R-0394

**Phase:** after-1.0/research

**Pull trigger:** the release subset is stable and onboarding (R-0515) shows a browser runner would lower the first-use barrier.

**Reconciliation (2026-10-19):** Keep optional.

**Objective:** Add a playground or local web runner only after the release subset is stable: `concrete playground --local` or a static hosted runner with preloaded examples, no hidden claims, visible evidence class, audit output, and clear sandbox/timeout/resource assumptions. This is a teaching surface, not a second compiler pipeline.

### Task R-0396

**Phase:** 6, 7

**Pull trigger:** Phase 6 entry, when frame, invariant and variant facts exist to diagnose.

**Reconciliation (2026-10-19):** Keep after facts exist.

**Objective:** Add editor and agent diagnostics for SPARK-class assurance facts after the facts exist: failed loop invariants, weak variants, missing frame facts, over-broad `writes`, unsatisfied `depends`, ghost/spec partiality, package evidence downgrades, and runtime-safety obligations that need a frame or invariant. The LSP/JSON payload must reuse the obligation/evidence ledger and point agents to the validation command; no editor-only proof status.

 Also surface development-only expectations from Task R-0240 as
 `dev_checked` / `tested` facts, never as proof. Editor and agent prompts
 should suggest "promote this to a contract/obligation" when release or
 high-integrity policy requires stronger evidence.

## Area: Concurrency And Research-Gated Extensions

### Task R-0399

**Phase:** after-1.0/research

**Pull trigger:** the v1 language surface is frozen and a workload needs concurrency.

**Reconciliation (2026-10-19):** Keep dated decision/deferral under R-0515.

**Objective:** Keep concurrency design-only until the v1 surface is frozen:

capability lattice, scopes, spawn/join, linear handles, bounded channels,
result flow, ownership transfer, rejected forms, and report schema. This
includes the memory-model question Concrete must not answer accidentally:
atomics, synchronization, shared mutable state, data-race freedom,
capability-gated thread authority, and proof/evidence classes for concurrent
code all remain research-gated until a formal model and pressure tests exist.
Async syntax remains rejected for v1 per `docs/language/ANTI_FEATURES.md`; the
positive research direction lives here and in `docs/language/EXECUTION_MODEL.md`:
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

**Phase:** after-1.0/research

**Pull trigger:** R-0399's design exists; sketches and expected reports precede any implementation.

**Objective:** Build concurrency pressure-test sketches and expected reports before implementation.

### Task R-0401

**Phase:** after-1.0/research

**Pull trigger:** R-0399 and R-0400 exist; the model is mechanized before any concurrency safety claim.

**Reconciliation (2026-10-19):** Keep; disambiguate “v1 concurrency” from language 1.0.

**Objective:** Mechanize the v1 concurrency formal model before claiming safety.

### Task R-0402

**Phase:** after-1.0/research

**Pull trigger:** R-0401's model and reports are stable.

**Objective:** Implement OS threads/scopes/channels only after the model and reports are stable.

### Task R-0403

**Phase:** after-1.0/research

**Pull trigger:** a state-machine or protocol example needs typestate that explicit state values cannot express.

**Objective:** Research typestate only if a current state-machine/protocol example needs it.

### Task R-0404

**Phase:** after-1.0/research

**Pull trigger:** bounded-capacity or allocation-profile work exposes a concrete gap that arenas would close.

**Reconciliation (2026-10-19):** Keep only unresolved lifetime/region design; ordinary allocator examples are R-0317.

**Objective:** Research arena allocation after bounded-capacity/allocation-profile work exposes a concrete gap.

### Task R-0405

**Phase:** after-1.0/research

**Pull trigger:** the freestanding profile (R-0322) and a deterministic hardware model exist, and a user needs WCET annotations.

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

**Phase:** after-1.0/research

**Pull trigger:** R-0448 has a real multi-kernel user who needs bridge-level rather than agreement-level trust.

**Reconciliation (2026-10-19):** Keep workload gate; align bridge with Core/VC semantics.

**Objective:** Research Why3-style realization for the prover backends — proving, in Rocq and Isabelle themselves, that the obligation IR's built-in theories are sound in that prover's model — only after R-0448 has a real multi-kernel user who needs bridge-level trust rather than agreement-level trust. Each realization proof converts a slice of the trusted Core→obligation bridge into kernel-checked evidence; without it, "three kernels" means three syntaxes over one trusted bridge. Pulled by an audited consumer requiring it, not by symmetry.

### Task R-0406

**Phase:** after-1.0/research

**Pull trigger:** the generation layer is pulled only if R-0444's combinator core still leaves repeated parser boilerplate in packet or ELF examples.

**Reconciliation (2026-10-19):** Keep only generation remainder.

**Objective:** Research binary-format DSLs only if packet/ELF examples show repeated parser boilerplate. The trigger fired (2026-07-25 audit: seven combinator shapes spelled ~45–50 times; elf_header the least-protected parser in the tree); the combinator core and the numeric/PNG/ELF ports are pulled forward as R-0444. What remains gated here is the generation layer proper — EverParse-style spec→Concrete source — gated on R-0170's checked combinator proof leg existing and on DECISIONS' external-generation rule (generated output is audited as ordinary source).

### Task R-0407

**Phase:** after-1.0/research

**Pull trigger:** source-level capabilities and package policies are stable and a hardware target needs capabilities mapped to it.

**Objective:** Research hardware capability mapping after source-level capabilities and package policies are stable.

### Task R-0410

**Phase:** after-1.0/research

**Pull trigger:** a workload shows semantic diff or proof evidence repeatedly recomputing the same equalities, after backend contracts and the evidence pipeline mature.

**Reconciliation (2026-10-19):** Keep with explicit workload trigger.

**Objective:** Research persistent equality/rewrite state after backend contracts, semantic diff, and proof/evidence pipeline are stronger.

### Task R-0411

**Phase:** after-1.0/research

**Pull trigger:** explicit capabilities are shown to be a proven, repeated blocker in real programs after the stdlib and concurrency pressure tests.

**Reconciliation (2026-10-19):** Keep deferral, not implementation backlog.

**Objective:** Do not adopt row effects for v1. The default design stays object-capability and audit-visible: authority should be obvious in source, not hidden behind abstract effect inference. Revisit only as a research note if explicit capabilities create a proven, repeated blocker in real programs after the stdlib and concurrency pressure tests exist.

### Task R-0412

**Phase:** after-1.0/research

**Pull trigger:** a forcing workload with a pressure test and decision record shows static linearity plus second-class references are too restrictive.

**Reconciliation (2026-10-19):** Keep strict pressure-test and evidence-class conditions.

**Objective:** Research a generational-reference-style dynamic fallback only if a forcing workload proves static linearity plus second-class references are too restrictive. The target use case is not ordinary ownership; it is a narrow escape for liveness/provenance facts around observers, back-references, callback-heavy object graphs, or host/FFI handles that Concrete cannot prove statically without unacceptable surface complexity. If adopted, the runtime check discharges an obligation as `checked_dynamically`, never as `proved`, and audit/report output must distinguish it from static ownership proof.


 Do not pull this forward without a pressure test and a decision record.
 The research pass must compare Vale-style generational references and
 region/frozen-scope ideas, explicitly reject any vague "zero-cost safety"
 claim, and prove the fallback does not become a hidden borrow checker,
 hidden GC, implicit Drop, or a way to weaken the linear default.
### Task R-0413

**Phase:** after-1.0/research

**Pull trigger:** the verified pipeline's compiler-TCB inventory is published and a measured proposal shows evidence dogfooding would shrink it.

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

**Phase:** after-1.0/research

**Pull trigger:** the incremental driver's `CompilerDB` has real relational consumers that typed queries and maps cannot express cleanly.

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

**Phase:** after-1.0/research

**Pull trigger:** the first research-gated extension above is pulled; each pulled extension adds its sketch, expected report and decision record.

**Reconciliation (2026-10-19):** Keep; add omitted R-0449 to enumerated coverage. Records the deferred Silver shared-state systems workload (outside ethlambda completion), and R-0449 belongs to its enumerated coverage.

**Objective:** Add the research-extensions area validation artifact: one pressure-test sketch, expected report, and decision record for every research-gated extension (concurrency, atomics/memory model, typestate, arena allocation, WCET, binary-format DSLs, hardware capability mapping, Miri-style interpreter, sized evaluator, persistent rewrite state, row effects, generational dynamic fallback, compiler self-verification, and Datalog-style relational facts). No research item graduates unless its forcing example, report shape, evidence class, and rejection or pull-forward criteria are recorded.
