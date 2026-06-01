# Concrete Roadmap

This document is the active execution plan. It answers one question:
**what should happen next, in what order?**

The roadmap is linear. The first unfinished item in the earliest unfinished
phase is next. Completed work moves to [CHANGELOG.md](CHANGELOG.md). Deferred
or conditional work moves later. There are no `NEXT` tags.

North star: **systems code with explicit authority, bounded behavior, small
trusted boundaries, and Lean-backed evidence tied to real source code, while
keeping compiler, backend, toolchain, runtime, and target assumptions honest.**

Governing frame: **no semantically dark constructs.** Every language
construct is `proved`, `enforced`, `reported`, `assumed`, or `trusted` —
never a vague middle. "Provable language" is not "everything in
`ProvableV1`"; it is "every construct has a defined proof story across the
five claim classes." `ProvableV1` is the current `proved`-via-value-semantics
cell. Other cells are populated by capability typing, runtime obligations,
assumption files, and declared trust boundaries.

## Current State

Concrete already has a real Lean 4 compiler pipeline:

`Parse -> Resolve -> Check -> Elab -> CoreCheck -> Mono -> Lower -> EmitSSA -> LLVM IR`

Concrete has four graduated showcase flagships:

1. `parse_validate` — parser / packet validation.
2. `crypto_verify` — toy authentication proof scaffolding, explicitly not real crypto.
3. `fixed_capacity` — bounded mutable systems state.
4. `constant_time_tag` — narrow real-crypto-adjacent tag comparison.

The proof/evidence pipeline is operational:

- Lean-checked user-code theorems exist.
- Body fingerprints catch source drift.
- Spec-drift checks catch hand-written spec drift.
- FnTable completeness checks catch missing proof callees.
- Assumptions, policies, snapshots, oracles, catches, release bundles, and
  showcase manifests exist for graduated examples.
- Compiler-soundness work has started: the flagship-used ProofCore extraction
  surface is no longer blocked by `partial def` opacity, and R-01 through R-28
  now have extraction/preservation coverage at least at the shape level. The
  remaining work is deeper source-semantics agreement and trust-gate soundness,
  not basic extractor access.
- A reusable proof layer (the "proof ladder") is shipped and kernel-verified:
  array update lemmas, `while_` unfolding, evaluator fuel monotonicity
  (`eval_fuel_succ`/`eval_fuel_le`), and bounded counter-loop induction
  (`eval_while_count`) — so loop/array proofs are systematic, not one-off
  scripts. `bv_decide` (kernel-checked BitVec automation, in-toolchain) now
  backs a committed proof with zero added trust. See
  [docs/PROOF_LADDER.md](docs/PROOF_LADDER.md).
- The first eval ↔ spec **refinements** ship: an independent `BitVec`-valued
  SHA-256 spec (`Concrete/Sha256Spec.lean`) and proofs that the extracted
  Boolean round functions refine it for ALL inputs — `ch_refines` /
  `maj_refines` (`Concrete/Sha256Refine.lean`), word-level core discharged by
  `bv_decide` (`proved_by_kernel_decision`). This is "the source computes the
  function the spec names," not "evaluates to these bytes."
- The first **loop** refinement ships: `block_to_words_refines_spec` proves the
  extracted 16-iteration big-endian word-packing loop refines
  `Sha256Spec.blockToWords` for ALL 64 input bytes — `eval_while_count` drives
  the loop, a `List.set`/`getElem` array invariant the per-iteration write, and
  `bv_decide` the per-word packing. The project has moved from straight-line
  helper refinement to looping crypto-state refinement. The
  rotation/sigma/schedule/compression refinements and the `hash`/`hmac`
  composition (HMAC bar #2) reuse this machinery and are next.
- A fifth flagship, `hmac_sha256` (first real cryptographic primitive), is
  implemented and runtime-verified against FIPS 180-4 + RFC 4231; ProvableV1
  conformance `full`; two kernel theorems attached; graduation gated on bar #2
  (a Lean-checked composition/refinement theorem).
- `ProvableV1` is named, documented, and wired into the flagship corpus.
  `docs/PROVABLE_V1.md` is the public contract; every graduated flagship
  declares conformance in its README; `tests/showcase/manifest.toml` carries a
  `provable_v1_conformance` field per flagship (all four currently `full`).
  The "Direction" section in `docs/PROVABLE_V1.md` records the long-term
  commitment: the subset is additive, and the broader frame is "no
  semantically dark constructs."

The remaining question is no longer "can Concrete compile programs?" It is:

**Can Concrete become a top-tier assurance-oriented systems language, with a
stable provable subset, systematic obligations, strong audit UX, and a shrinking
trusted compiler/proof bridge?**

## First-Release Success Bar

Do not call the language releasable until these are true:

- A small supported subset is frozen and documented.
- At least one proof-bearing example can be built, audited, and understood by a
  new user in under ten minutes.
- Claims are clearly separated into `proved`, `enforced`, `reported`,
  `assumed`, and `trusted`.
- The public threat model names what Concrete is trying to prevent, what it
  intentionally does not prevent, and which assumptions a user must review.
- Bad changes are caught by normal tools: authority widening, allocation/FFI
  changes, predictable-profile breaks, proof/spec/body drift, missing
  obligations, and evidence weakening.
- At least one parser/protocol example, one bounded-state example, and one
  crypto/security example carry honest evidence bundles.
- The release does not claim a fully verified compiler. It claims selected
  user-code properties under explicit assumptions, with a visible plan for
  reducing trusted extraction/compiler gaps.

## Operating Rules

- Keep the list linear. If an item depends on another item, it appears later.
- Move completed items to [CHANGELOG.md](CHANGELOG.md), not to a completed
  section in this file.
- Every proof/evidence claim must identify whether it is proved, enforced,
  reported, assumed, or trusted.
- No semantically dark constructs: every language construct has a row in the
  proof-story matrix, or its absence is treated as a documentation bug.
- Contracts, when they land, create obligations. They are never promises by
  themselves.
- Keep parser changes LL(1). Attribute-style contracts are allowed; ambiguous
  trailing contract syntax is not.
- SMT can assist obligation discharge, but it must never silently replace Lean
  or human-readable evidence. Solver results are classified as `proved_by_smt`,
  `smt_unknown`, `smt_counterexample`, or `solver_trusted`, and audit/release
  artifacts must show that classification.
- Add proof/evidence infrastructure only when it tightens a real claim, unlocks
  a flagship, or reduces a named trust gap.
- New flagship candidates enter through an audit first, then code, then proof,
  then oracle/catches/bundle/manifest.
- Do not add a named IR layer until at least two forcing examples expose the
  same duplication and direct extraction is no longer coherent.
- Do not start packages, editor-first tooling, concurrency implementation, or a
  second backend before the proof/evidence foundation is stable enough to carry
  their claims.

---

## Phase 1: Proof Status And Trust Gates

Goal: make every green proof/evidence status precise, traceable, and hard to
misread.

Done when: all existing production proof specs are directly and transitively
FnTable-complete, proof dependencies and provenance are visible, assumptions
and trust boundaries have lifecycle reports, and weaker evidence cannot appear
under a stronger badge.

1. Add transitive FnTable completeness: walk registered spec call graphs, not
   only direct call sites, and fail or flag missing callees before theorem
   authors hit confusing `none` evaluations.
2. Add proof dependency tracking: if proof/spec for `f` depends on `g`, drift in
   `g` must affect `f`'s proof/evidence status or surface an explicit
   dependency warning.
3. Add proof debugging output for failed/stale proofs: extracted spec, current
   fingerprint, registered fingerprint, expected theorem shape, missing callee
   facts, likely missing lemma class.
4. Add evidence provenance to proof/evidence facts: source file/span, compiler
   commit, theorem name, spec name, policy file, assumption file, tool version,
   and replay command where available.
5. Add evidence monotonicity checks: a refactor cannot silently present a weaker
   claim as if it were still stronger (`proved` cannot degrade to `reported`
   while retaining the same badge/summary).
6. Add assumption lifecycle checks: every assumption has an owner, scope,
   rationale, review date, affected claims, and a diff gate when it widens.
7. Add a trust-boundary inventory report: all `trusted`, `Unsafe`, extern,
   backend, runtime, and target assumptions in one machine-readable list.

## Phase 2: Provable And Predictable Subsets

Goal: give users a named small subset they can rely on for serious
proof/evidence work.

Done when: the subset family has public names, allowed constructs, rejected
constructs, arithmetic profiles, runtime-error policy, and compatibility
promises.

1. Define `PredictableV1`: no allocation unless bounded, no FFI unless trusted
   and assumed, no unbounded loops/recursion, explicit failure-path policy.
2. Freeze the first arithmetic profiles:
   wrapping, checked, and proved/no-overflow.
3. Carry arithmetic profile choices into diagnostics, reports, assumptions,
   proof obligations, and release bundles.
4. Define a first runtime failure model: abort, assertion failure, OOM, stack
   overflow, `defer`/cleanup, impossible branches, and what each does to
   proof/resource claims.
5. Define source-level stack-depth versus backend/target stack claims.
6. Define source-level constant-time profile v1:
   no secret-dependent branch, no secret-dependent memory index, fixed loop
   bounds, explicit backend timing assumptions.
7. Define secret/data-sensitivity labels for future security work:
   `public`, `secret`, `timing-sensitive`.
8. Define source-level memory-safety claims precisely: what linearity, borrows,
   cleanup, trusted code, raw pointers, and FFI do and do not guarantee.
9. Define the v1 threat model: adversary, trusted base, proof scope, backend
   scope, side-channel scope, dependency scope, and what remains out of model.
10. Add negative examples for every `ProvableV1` and `PredictableV1` exclusion.
11. Update `CLAIMS_TODAY.md`, README, showcase docs, and release bundles to use
    the frozen subset names consistently.

## Phase 3: Runtime Safety Obligations

Goal: generate SPARK-like obligations for boring runtime failures instead of
relying only on examples and prose.

Done when: parser/security examples can show obligations for bounds, div/mod
zero, overflow profile, casts, and loop bounds with statuses
`proved`, `enforced`, `assumed`, `missing`, or `blocked`.

1. Define stable obligation schema v1: id, kind, source span, function,
   expression, dependencies, evidence status, discharging theorem/check/
   assumption, and replay command.
2. Generate array index bounds obligations.
3. Generate division/modulo nonzero obligations.
4. Generate overflow obligations under checked/proved arithmetic profiles.
5. Generate narrowing/invalid-cast obligations.
6. Generate loop bound and variant obligations for bounded loops.
7. Generate stack/recursion obligations where the profile claims boundedness.
8. Report runtime-error obligations in human and JSON forms.
9. Add policy gates that can require selected runtime-error obligations to be
   proved/enforced before graduation.
10. Add a runtime-error regression corpus: OOB, div/mod zero, overflow-profile
    violation, invalid cast, loop-bound violation.
11. Add a runtime-error-obligation flagship requirement: one graduated example
    must demonstrate no OOB/div-zero/overflow under a named profile.
12. Add high-quality diagnostics for obligation failures: violated obligation,
    source expression, required evidence, current status, and next action.
13. Add obligation suppression only through explicit assumptions or policy
    waivers, never comments or hidden allowlists.
14. Prove or validate obligation-generation soundness for the first obligation
    kinds through the compiler soundness bridge.

## Phase 4: Source Contracts

Goal: let proof-relevant properties live in source code without breaking LL(1),
and make every contract generate obligations instead of becoming decorative
prose.

Design reference: [docs/CONTRACTS_AND_VCS.md](docs/CONTRACTS_AND_VCS.md), and
[docs/PROOF_LADDER.md](docs/PROOF_LADDER.md) for the build order.

**Ordering (let the proof teach the syntax):** do not freeze contract syntax or
VC shapes before a real refinement proof exists. The spec layer, the `bv_decide`
tier, the refinement pattern (`ch_refines` / `maj_refines`), and the first loop
refinement (`block_to_words_refines_spec`, Phase 8) have all **shipped** — so
this phase now designs against obligation shapes that have actually been
discharged, including a real `eval_while_count` loop obligation, rather than
imagined ones. See the build order in `docs/PROOF_LADDER.md`.

Done when: one flagship uses source contracts as the primary proof surface, and
each contract is classified as proved, enforced, assumed, missing, blocked, or
solver-assisted.

1. Add LL(1)-safe function attributes:
   `#[requires(...)]` and `#[ensures(...)]`.
2. Restrict v1 contract expressions to proof-friendly expressions:
   parameters, `result`, literals, comparisons, boolean operators, simple
   arithmetic, fixed array lengths, and named pure predicates where explicitly
   supported.
3. Add a contract design fixture corpus before implementation:
   straight-line `ch`, bounds-only `get16`, loop-shaped `ct_compare`, and an
   HMAC-style `ensures result == spec(...)` example. These fixtures define the
   expected parse, Core, VC, and audit shapes before the parser/compiler work
   hardens them.
4. Store source contracts through Parse/Resolve/Check/Core and report them with
   source spans.
5. Generate verification-condition seeds for each source contract: caller-side
   preconditions, callee-side postconditions, and assumptions needed by the
   expression language.
6. Add `--report contracts` with evidence statuses and links to generated
   obligations and VC ids.
7. Connect contracts to the proof registry: a theorem can discharge a specific
   source contract id.
8. Map existing proof-registry entries to generated source-contract obligations
   where possible, so current Lean theorems migrate forward instead of being
   discarded when contracts become the primary proof surface.
9. Add contract negative examples: unmet precondition at call site, missing
   postcondition proof, weakened postcondition, invalid contract expression.
10. Add proof-only source forms:
   `ghost let`, `assert`, and `assume`. `ghost` erases before codegen;
   `assert` creates an obligation; `assume` is a tainted, audit-loud
   assumption, never a proof.
11. Add loop attributes:
   `#[invariant(...)]` and `#[variant(...)]`.
12. Generate loop-invariant obligations: initialization, preservation, variant
   decrease, and exit-implies-postcondition.
13. Add source contract soundness work to the compiler soundness bridge: parsing
   preserves meaning, generated obligations correspond to contract semantics,
   discharged obligations imply the advertised contract claim.
14. Add contract diagnostics that explain whether the failure is caller-side
    precondition, callee-side postcondition, loop invariant initialization,
    invariant preservation, or variant decrease.
15. Add contract stability rules: weakening a precondition, strengthening a
    postcondition, or changing a public invariant is a semantic API change.
16. Add one contract-bearing flagship retrofit after the machinery is real:
    `constant_time_tag` first, then `hmac_sha256` once its refinement theorem
    has graduated.

## Phase 5: Verification Conditions And SMT Assistance

Goal: get closer to SPARK-style automation without hiding solver trust or
replacing Lean-checked theorem claims.

Design reference: [docs/CONTRACTS_AND_VCS.md](docs/CONTRACTS_AND_VCS.md).

Done when: contracts, bounds checks, arithmetic side conditions, and simple loop
invariants generate machine-readable VCs; a solver can discharge the easy ones;
counterexamples are reported clearly; and audit output distinguishes Lean,
SMT, tests, enforcement, assumptions, and trusted solver claims.

1. Define VC schema v1: id, source span, kind, hypotheses, conclusion,
   originating contract/obligation, dependencies, arithmetic profile, and
   expected discharge mode.
2. Generate VCs for pure no-loop contracts first: preconditions at call sites
   and postconditions at returns.
3. Generate VCs for runtime safety obligations from Phase 3: array bounds,
   div/mod nonzero, checked/proved overflow, casts, and loop bounds.
4. Generate VCs for loop invariants: initialization, preservation,
   variant decrease, and exit-implies-postcondition.
5. **Kernel-checked automation first (`bv_decide`).** Before any external
   solver, route BitVec / bounded-arithmetic VCs to Lean's `bv_decide`
   (in-toolchain; bit-blasts to SAT and replays a kernel-checked certificate —
   **no TCB growth**). Already validated against the HMAC helper facts. Its
   results are classified `proved_by_kernel_decision`, a kernel-checked class
   distinct from `proved_by_smt`. See [docs/PROOF_LADDER.md](docs/PROOF_LADDER.md).
6. Add an *external* SMT backend behind an explicit flag or policy gate, reached
   for only when `bv_decide` cannot (e.g. nonlinear). Start with one solver
   adapter and a stable SMT-LIB output path before adding more solvers. Its
   results are `solver_trusted` (solver enters the TCB) unless a certificate is
   replayed — never collapsed into a kernel-checked class.
7. Classify solver results in reports and artifacts:
   `proved_by_kernel_decision` (kernel-checked), `proved_by_smt` /
   `solver_trusted` (external), `unknown`, `counterexample`, `timeout`,
   `solver_error`.
8. Surface counterexamples in source terms where possible: function inputs,
   loop variables, failing index, failing arithmetic side condition, and the
   contract/obligation that failed.
9. Add CI gates for solver determinism and replay: same VC, same solver
   configuration, same result class, with timeouts treated as non-proofs.
10. Add Lean replay for the simplest SMT-discharged fragments where practical:
   propositional/linear integer facts, bounds arithmetic, and trivial BitVec
   identities. Results without replay remain explicitly solver-trusted.
11. Add policy controls: projects can require `proved_by_lean`, allow
    `proved_by_smt`, or permit `solver_trusted` only under named assumptions.
12. Add SMT negative examples: false postcondition, missing invariant,
    overflow counterexample, OOB counterexample, div-zero counterexample,
    solver timeout, and unsupported theory.
13. Retrofit one flagship with source contracts whose easy VCs discharge
    automatically, while the meaningful theorem remains Lean-checked.
14. Update audit/release bundles so VC results appear beside proof registry,
    assumptions, runtime obligations, and proof coverage classification.
15. Add soundness documentation for the SMT path: trusted solver binary,
    encoding assumptions, unsupported theories, replayed fragments, and how a
    solver bug affects each claim class.

## Phase 6: Proof Authoring And Automation

Goal: make flagship proofs a repeatable engineering workflow, not a collection
of one-off `simp` scripts.

Done when: new flagship proofs can start from useful generated stubs, standard
lemmas, and actionable failure diagnostics.

1. Build reusable proof lemmas for arrays: lookup, update, length, in-bounds,
   OOB stuck behavior.
2. Build reusable lemmas for loop-carried state and `while_step`.
3. Build reusable lemmas for BitVec operations used by flagships.
4. Build reusable lemmas for structs, fields, enum construction, match, Result,
   Option, and bounded-buffer invariants.
5. Upgrade generated proof stubs for real shapes: arrays, structs, enums,
   fixed buffers, Result/Option, loops, and source contracts.
6. Add `concrete prove <function>`: generate the proof stub, list the source
   contracts and VCs, show the registry/spec target, and print the current
   failing obligation with replay commands.
7. Add proof minimization/debugging UX: show the smallest extracted expression
   or lemma surface related to a failed proof.
8. Add proof replay/caching once proof artifacts and fingerprints are stable.
9. Add simple auto-discharge for structural obligations that do not need human
   proof search.
10. Add a small verified/spec-checked standard proof library for common
   predicates: sorted, bounded, no-duplicates, fixed-length, prefix, checksum,
   constant-time source shape.
11. Add AI-assisted proof repair only after artifacts, statuses, and replay are
   stable enough to validate suggestions mechanically.

## Phase 7: Audit Commands And Review Artifacts

Goal: let a reviewer answer "what can this program do, what is proved, what is
assumed, and what changed?" without reading compiler internals.

Done when: `concrete audit`, semantic diff, and an artifact viewer cover the
four graduated flagships and one package-scale example.

1. Stabilize machine-readable fact schemas for proof status, obligations,
   effects, capabilities, assumptions, policies, snapshots, and showcase
   metadata.
2. Add `concrete audit`: one human-readable plus machine-readable bundle
   covering authority, trust, allocation, proof status, obligations,
   assumptions, policy, snapshots, backend/target assumptions, replay, and the
   proof-story matrix specialized to the audited program.
3. Add `concrete explain <function>`: capabilities, proof status, assumptions,
   obligations, trusted callees, evidence level, and why each status is what it
   is.
4. Add `concrete why <capability>`: explain why a function needs `File`,
   `Network`, `Alloc`, `Unsafe`, etc., including transitive call chains.
5. Add `concrete diff old new`: authority/proof/trust/runtime-obligation diff.
6. Add semantic trust diff gates: capability widening, allocation change,
   trusted boundary addition, stale proof, weakened/missing obligation,
   assumption widening.
7. Add `concrete audit --json`: machine-readable audit output for CI,
   dashboards, editor tooling, and release bundles.
8. Add an artifact viewer CLI/TUI over facts, obligations, proofs,
   assumptions, release bundles, and diffs.
9. Ensure every release bundle includes an evidence replay command.
10. Add evidence-level monotonicity checks to audit/diff output.
11. Add one AI-audit demo where an agent answers authority/proof/trust
    questions using compiler facts rather than source guesses.
12. Add review checklists generated from facts: what changed, what widened,
    what became trusted, what lost proof, what gained assumptions, and which
    obligations remain open.
13. Add artifact redaction/stability rules so release bundles can be shared
    publicly without leaking local paths, secrets, or machine-specific noise.

## Phase 8: Flagship Depth And Examples

Goal: produce examples that outside systems engineers find impressive, not only
internally coherent.

Done when: the showcase set includes a serious security/crypto or protocol
example with proof/evidence strong enough to anchor the public pitch.

Immediate HMAC rule: finish `hmac_sha256` before building the source-contract
language. Contracts/VCs should be designed against a refinement proof that has
actually been discharged, not against an imagined obligation shape. After HMAC
graduates, resume Phase 4 through Phase 7 in order: source contracts, VC
generation, discharge classification, a flagship retrofit, `concrete prove`,
optional external SMT, and editor/LSP surfaces.

1. Maintain the four graduated flagships and keep their evidence bundles green:
   `parse_validate`, `crypto_verify`, `fixed_capacity`, `constant_time_tag`.
2. Add stretch theorem for `constant_time_tag`: full iff if tractable, or a
   clearly named stronger negative-direction theorem.
3. Add stretch theorem for `fixed_capacity`: multi-iteration ring invariant or
   stronger push/search property.
4. Add stretch theorem for `parse_validate`: success-path / failure-completeness
   theorem once proof ergonomics support it.
5. Audit the next stronger real-crypto candidate: HMAC-SHA256 verification or
   Ed25519 verification subset.
6. Add only the ProofCore surface that candidate forces: shifts, bitand, u32
   compound loops, rotations, byte-to-word packing, and multi-round invariants.
7. For `hmac_sha256`, complete the remaining refinement chain in order:
   `sha256_schedule_refines_spec`, `sha256_compress_refines_spec`,
   `sha256_hash_refines_spec`, and `hmac_sha256_refines_spec`.
8. Graduate `hmac_sha256` only after bar #2 is closed: update manifest,
   README/status text, release bundle, proof-status table, changelog, and
   roadmap notes.
9. After `hmac_sha256` graduates, update the paper and website to use HMAC as
   the running example for source, spec, proof status, assumptions, oracle,
   limits, and trusted boundaries. Do not make HMAC the public center before
   the refinement claim is real.
10. Graduate one runtime-error-obligation flagship: parser/protocol example with
   no OOB/div-zero/overflow obligations discharged.
11. Graduate one authority/capability flagship: a privilege-separated tool whose
   trusted core cannot touch files/network/processes except through named
   wrappers.
12. Graduate one FFI-wrapper flagship: trusted C boundary, safe pure core,
    explicit assumptions, layout/ABI evidence.
13. Graduate one ownership-heavy resource flagship: explicit cleanup,
    borrow-heavy APIs, no leaks/double-use, and evidence explaining why.
14. Keep the curated showcase balanced: parser/protocol, bounded state,
    crypto/security, authority, FFI/trust, ownership-heavy.

## Phase 9: Compiler Soundness Bridge

Goal: move the flagship-used `Core -> ProofCore` rules from "extracts to the
expected ProofCore shape" toward source-semantics agreement and checked
trust-gate correctness.

Done when: each flagship-used ProofCore construct is classified as
shape-preserved, eval/source-semantics-preserved, or still trusted; proof-report
facts agree with compiler state; and the remaining trusted base is
machine-readable.

1. Add a compiler-soundness rule-status dashboard over R-01..R-21:
   `shape-preserved`, `eval-preserved`, `source-preserved`, `trusted`, or
   `blocked`, with theorem names and source links for each status.
2. Build source semantics for the provable subset as needed by rule discharge,
   not as a speculative full-language semantics.
3. Upgrade extraction-only rules to three-view preservation where practical:
   source Core evaluation, extracted PExpr evaluation, and extraction theorem
   agree.
4. Prioritize the hard eval/source rules in dependency order: direct calls,
   structs/fields, enums/match, arrays, casts, arraySet, flat bounded `while`,
   and `while_step`.
5. Prove selected proof-report facts agree with compiler state: `proved`,
   `stale`, `blocked`, `missing`, `ineligible`, `trusted`.
6. Prove or mechanically validate trust-gate correctness: body fingerprint
   determinism, spec-drift completeness, proof attachment lookup, FnTable
   completeness, eligibility classification.
7. Record a machine-readable trusted computing base for proof/evidence claims:
   Lean kernel, compiler modules, backend/toolchain, runtime/OS/hardware,
   trusted/extern code.
8. Prove selected checker/report agreement for authority and purity facts used
   by proof eligibility, so a function cannot be called proof-eligible while
   secretly requiring capabilities.
9. Decide whether deeper source-semantics proofs require a normalized Core
   layer. Add the layer only if the direct rule proofs show repeated semantic
   duplication across at least two forcing examples.

## Phase 10: Backend, Target, And Stdlib Contracts

Goal: make backend/toolchain/stdlib assumptions explicit, and state exactly
where source-level proof stops.

Done when: SSA, target/toolchain, optimization, ABI/layout, stdlib evidence,
and incremental build contracts are explicit enough for release evidence.

1. Stabilize SSA as the only backend contract.
2. Document target/toolchain model: triple, data layout, linker, runtime/startup,
   libc expectation, clang/llc boundary, sanitizer/coverage hooks.
3. Define optimization policy: allowed optimizations, evidence preservation,
   debug/release behavior, report/codegen validation.
4. Add clean-build versus incremental-build equivalence checks: facts,
   obligations, diagnostics, reports, and codegen must agree.
5. Add ABI/layout round-trip checks: C headers/stubs, offsets, size, alignment,
   calling conventions.
6. Add sanitizer-backed generated-code validation for trusted/FFI/layout/
   pointer-heavy examples.
7. Add backend/codegen differential validation where executable oracles exist.
8. Add compiler self-leak/resource soak harness for long-running workflows.
9. Define stdlib stability and evidence policy: which stdlib functions are
   trusted, proved, enforced, allocation-free, capability-free, or assumption
   carriers.
10. Add stdlib evidence gates so core helpers cannot silently widen authority,
    allocation, proof assumptions, or runtime-error obligations.
11. Evaluate a normalized mid-level IR only when traceability/backend-contract
   reports expose a concrete gap.
12. Keep QBE/WASM/second backend deferred until evidence attachment,
    optimization policy, and backend trust boundaries are trustworthy.

## Phase 11: Language Usability And Daily Workflow

Goal: make Concrete usable as a normal experimental language, independent of
whether a user is writing proofs.

Done when: a new user can format, build, run, test, diagnose, inspect, and
debug small Concrete programs with predictable commands and useful errors.

1. Add `concrete fmt`: stable formatting for source files, examples, docs
   snippets, and generated fixtures. Formatting must not churn semantic
   fingerprints.
2. Improve diagnostics for parser, resolver, type checker, ownership, linearity,
   capability, unsupported-construct, and codegen/interpreter mismatch errors:
   every diagnostic has a source span, reason, and next action.
3. Add basic LSP/editor diagnostics early: parse/type errors, capability
   summaries, hover for inferred types, and jump-to-definition. Deeper
   proof/evidence LSP features remain in the later editor phase.
4. Stabilize modules and imports before packages grow: module names, file
   layout, visibility, import resolution, cycle diagnostics, and generated
   interface summaries.
5. Decide the v1 iteration protocol before broad stdlib work. Evaluate and
   document the replacement for closures/trait-object iterators:
   index-based `for i in 0..len { xs[i] }`, explicit cursor/iterator structs
   with `next() -> Option<T>`, and monomorphized `for_each`-style helpers. The
   decision must cover `Vec`, slices, maps, parser cursors, and interpreter
   workloads, and must explain how authority and allocation remain visible.
6. Decide capability polymorphism for higher-order stdlib functions before
   adding `map`/`fold`/`for_each` families or structured concurrency. The
   design must avoid a combinatorial split like `map`, `map_file`,
   `map_alloc`; the expected shape is explicit capability-set polymorphism such
   as `fn map<T, U, C>(xs, f: fn(T) with(C) -> U) with(C) -> ...`, grounded in
   `research/language/capability-polymorphism.md`.
7. Define stdlib v1 for daily programs: fixed arrays/slices, bytes/string
   basics, `Result`/`Option`, numeric helpers, and capability-scoped Console,
   File, Network, and Alloc APIs. Each stdlib item must declare its evidence
   class (`trusted`, `enforced`, `proved`, `reported`, or `assumed`).
8. Design user-facing testing framework UX before `std.test` hardens:
   test discovery (`#[test]` versus naming convention), expected failures,
   capability-scoped fixtures, temp files without ambient authority, oracle
   tests, interpreter-vs-compiled tests, proof-status interaction, and how test
   failures appear in `concrete audit`.
9. Add `concrete test`: discover and run user tests, example tests,
   expected-failure tests, interpreter-vs-compiled differential tests,
   snapshot tests, oracle tests, and policy/assumption gates through one
   command.
10. Add debug/trace mode: `concrete run --trace`, interpreter step traces, Core /
   lowered-IR dumps, source spans in runtime errors, and stable replay commands
   for report/debug failures.
11. Add a minimal project model before full packages: `Concrete.toml` fields for
   name, entry points, tests, policies, assumptions, source roots, and build
   profiles.
12. Normalize the CLI around predictable verbs:
   `concrete build`, `concrete run`, `concrete test`, `concrete fmt`,
   `concrete audit`, `concrete prove`, `concrete doc`, and `concrete clean`.
13. Add `concrete doc`: generate basic API/reference docs from source,
    capabilities, modules, and public comments without depending on proof
    infrastructure.
14. Add a first-user tutorial path that does not start with proofs: install,
    hello world, ownership, capabilities, arrays, modules, tests, audit, then
    proof-bearing examples.
15. Add useful non-proof examples: a small CLI tool, a protocol decoder, a
    bounded cache, and a capability-scoped file/console program.
16. Add basic benchmarking UX: run small benchmarks, compare interpreter versus
    compiled performance, and detect obvious generated-code regressions.
17. Document the memory model for ordinary users: move/copy/drop behavior,
    cleanup, borrows, linear values, trusted/Unsafe escape hatches, and what is
    rejected.
18. Add cross-platform build sanity for the supported host set: macOS and Linux
    first, with CI coverage, reproducible commands, and documented toolchain
    expectations.

## Phase 12: Freestanding And Embedded Target

Goal: make Concrete's hosted-vs-freestanding boundary explicit enough for
embedded, kernel, and audit-critical targets without destabilizing the hosted
language.

Prerequisite: the hosted stdlib/runtime boundary, allocator story, target model,
and daily workflow in Phase 10 and Phase 11 must be stable. Freestanding is not
a second backend escape hatch; it is a target profile with fewer assumptions.

Trigger: start this phase when at least one serious workload needs no libc,
explicit allocator/runtime setup, or embedded/kernel-style startup, and the
hosted evidence pipeline can already explain what will be removed.

Done when: a freestanding Concrete program can build under a named target
profile, with no ambient hosted stdlib assumptions, explicit allocator/startup
choices, and an audit report naming every remaining target/runtime assumption.

1. Define `hosted` versus `freestanding` target profiles: libc, startup,
   allocator, panic/abort behavior, stack assumptions, I/O availability,
   floating-point assumptions, and supported capabilities.
2. Split stdlib modules by target profile: core no-alloc/no-OS modules,
   allocator-backed modules, hosted OS modules, and explicitly unavailable
   modules.
3. Define freestanding capability policy: no ambient `File`, `Network`,
   `Console`, or `Process`; target-specific capabilities must be declared and
   audited.
4. Add explicit allocator/runtime hooks for freestanding builds, including
   ownership of allocation failure behavior and cleanup expectations.
5. Add linker/startup configuration: entry symbol, no-main mode, target triple,
   data layout, linker script hooks, and section/layout assumptions.
6. Add freestanding diagnostics: reject hosted APIs, hidden allocation, libc
   calls, unsupported target features, and unavailable capabilities.
7. Add one freestanding example: bounded parser, small checksum/hash kernel, or
   fixed-capacity state machine with no allocation and no hosted I/O.
8. Add one embedded-style audit bundle naming all remaining target assumptions:
   stack, interrupt model if any, allocator/runtime hooks, endian/layout, and
   backend/toolchain boundary.
9. Keep WASM, QBE, and additional backends deferred until freestanding target
   profiles prove the current LLVM path is not enough.

## Phase 13: Public Release Bar

Goal: make Concrete understandable and usable by someone who did not build the
compiler.

Done when: a fresh user can install Concrete, run a proof-bearing example,
inspect its audit bundle, and understand the claim matrix in under ten minutes.

1. Define first public release criteria: supported subset, required examples,
   required diagnostics, proof workflow, stdlib/project UX, evidence/policy/
   tooling story.
2. Publish a public claim matrix: what Concrete proves, enforces, reports,
   assumes, and trusts.
3. Add release claim freeze: README, `CLAIMS_TODAY.md`, roadmap, showcase
   manifest, and release bundles must agree.
4. Add compatibility policy for proof artifacts and fact schemas.
5. Add compatibility policy for generated contract/VC obligation IDs:
   obligation IDs should stay stable across harmless formatting and local
   refactors, and any unavoidable churn should be reported as artifact churn,
   not hidden under ordinary proof drift.
6. Define release/showcase evidence policy by class:
   `proved_by_lean` and `proved_by_kernel_decision` are strong evidence;
   `tested_by_oracle` is supporting evidence; `proved_by_smt` /
   `solver_trusted` require explicit policy approval; `open` and unreviewed
   `assumed` are forbidden for release claims.
7. Add public security/soundness disclosure policy: compiler/proof pipeline
   bugs are security-relevant.
8. Publish `THREAT_MODEL.md` and keep it linked from README, release bundles,
   showcase manifests, and assumptions docs.
9. Add first-user workflow CI: install compiler, create/run one example,
   inspect one audit bundle without repo-local assumptions.
10. Improve onboarding, tutorial, and docs around `proved` / `enforced` /
   `reported` / `assumed` / `trusted`.
11. Add positioning page against Rust, Zig, Lean, SPARK/Ada, Austral, Dafny,
   F*, Why3.
12. Add migration/adoption playbook: what C/Rust/Zig code moves first, how to
   wrap libraries honestly, what stays outside Concrete.
13. Add release/install distribution matrix: host triples, checksums/signing,
    install paths, supported/deferred channels.
14. Ship the first narrow public release only after the above are green.

## Phase 14: Packages And Dependency Evidence

Goal: let package users inspect proof, trust, capability, and assumption facts
before adopting a dependency.

Done when: packages have manifests, lockfiles, package-aware facts, trust
policies, provenance, and registry protocol.

1. Expand package artifacts only after reports, policies, assumptions,
   interface artifacts, and CI gates prove what packages must carry.
2. Design and parse package manifest.
3. Add version constraints, dependency resolution, and lockfile.
4. Add workspace and multi-package support.
5. Add package-aware test selection.
6. Split interface artifacts from body artifacts at package/workspace scale.
7. Add proof-aware package artifacts: facts, obligations, proof status, trusted
   assumptions, policy declarations, package-boundary evidence summaries.
8. Add module/package authority budgets after package graphs are real.
9. Add dependency trust policy: trust widening across boundaries, review and
   inheritance.
10. Add package-level assumption inheritance: dependency assumptions must be
    visible to dependents and release bundles.
11. Add package provenance and publishing model.
12. Add package registry server protocol and trust model.

## Phase 15: Editor And Human Tooling

Goal: make evidence visible where developers work.

Done when: editor/LSP/tooling exposes the same facts as CI and command-line
reports without inventing a second truth source.

1. Add artifact viewer integration for proof/evidence facts.
2. Add compiler-as-service / LSP entrypoints after diagnostics and facts are
   structured.
3. Add hover/type info for capability status, proof status, predictable status,
   assumptions, obligations, and trusted boundaries.
4. Add obligation navigation: jump from source contract/index/mod/loop to the
   generated obligation and discharging theorem.
5. Add refactor support that preserves or updates facts/proofs where possible.
6. Add dependency audit UI for capability, allocation, FFI, trust, evidence,
   predictability, proof-obligation drift.
7. Add backwards-compatibility regression corpus once public users exist.
8. Add language/versioning/deprecation policy across syntax, stdlib, proof/fact
   artifacts.

## Phase 16: Concurrency And Research-Gated Extensions

Goal: keep speculative ideas gated until Concrete's proof/evidence foundation
can contain them honestly.

Done when: each research idea is either pulled into an earlier phase by a
forcing example, explicitly deferred, or rejected.

1. Keep concurrency design-only until the v1 surface is frozen:
   capability lattice, scopes, spawn/join, linear handles, bounded channels,
   result flow, ownership transfer, rejected forms, and report schema.
2. Build concurrency pressure-test sketches and expected reports before
   implementation.
3. Mechanize the v1 concurrency formal model before claiming safety.
4. Implement OS threads/scopes/channels only after the model and reports are
   stable.
5. Research typestate only if a current state-machine/protocol example needs
   it.
6. Research arena allocation after bounded-capacity/allocation-profile work
   exposes a concrete gap.
7. Research exact WCET/cache/pipeline behavior only with a target/hardware
   model.
8. Research binary-format DSLs only if packet/ELF examples show repeated
   parser boilerplate.
9. Research hardware capability mapping after source-level capabilities and
   package policies are stable.
10. Broaden the proof-relevant interpreter toward Miri-style UB checking only
    if the proof-subset interpreter proves valuable.
11. Investigate a sized/indexed ProofCore evaluator only if the current
    fuel-indexed evaluator remains repeated proof debt after HMAC and at least
    one other substantial loop/composition proof. This is ProofCore v2
    research, not a migration commitment; see
    [docs/SIZED_EVALUATOR_INVESTIGATION.md](docs/SIZED_EVALUATOR_INVESTIGATION.md).
12. Research persistent equality/rewrite state after backend contracts,
    semantic diff, and proof/evidence pipeline are stronger.
