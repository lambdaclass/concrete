# Concrete Roadmap

This document is the active execution plan. It answers one question:
**what should happen next, in what order?**

The roadmap is linear. The first unfinished item in the earliest unfinished
phase is next. Completed work moves to [CHANGELOG.md](CHANGELOG.md). Deferred
or conditional work moves later. There are no `NEXT` tags.

North star: **systems code with explicit authority, bounded behavior, small
trusted boundaries, and Lean-backed evidence tied to real source code, while
keeping compiler, backend, toolchain, runtime, and target assumptions honest.**

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
- Phase 12 has started: literal extraction preservation is proved against the
  public extractor wrapper.

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
- Contracts, when they land, create obligations. They are never promises by
  themselves.
- Keep parser changes LL(1). Attribute-style contracts are allowed; ambiguous
  trailing contract syntax is not.
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

## Phase 1: Stabilize The Proof Trust Core

Expected outcome: the existing proof/evidence pipeline is no longer vulnerable
to practical attachment footguns, and every current green proof status has a
clear provenance trail.

Phase closes when: all existing production proof specs are directly and
transitively FnTable-complete, proof coverage is classified, and the proof
report tells reviewers exactly what kind of theorem each green check represents.

1. Add transitive FnTable completeness: walk registered spec call graphs, not
   only direct call sites, and fail or flag missing callees before theorem
   authors hit confusing `none` evaluations.
2. Add proof dependency tracking: if proof/spec for `f` depends on `g`, drift in
   `g` must affect `f`'s proof/evidence status or surface an explicit
   dependency warning.
3. Add proof coverage classification to proof registry and reports:
   point proof, one-direction theorem, iff theorem, invariant theorem,
   runtime-error proof, full contract proof.
4. Classify existing flagship proofs under that vocabulary so a zero-case proof
   and an iff theorem cannot both appear as an undifferentiated green check.
5. Add proof debugging output for failed/stale proofs: extracted spec, current
   fingerprint, registered fingerprint, expected theorem shape, missing callee
   facts, likely missing lemma class.
6. Add evidence provenance to proof/evidence facts: source file/span, compiler
   commit, theorem name, spec name, policy file, assumption file, tool version,
   and replay command where available.
7. Add evidence monotonicity checks: a refactor cannot silently present a weaker
   claim as if it were still stronger (`proved` cannot degrade to `reported`
   while retaining the same badge/summary).
8. Add "no hidden green" discipline: every green badge in showcase/release
   material links to the exact command, fact, theorem, or bundle that justifies
   it.

## Phase 2: Build The Phase 12 Source-To-Proof Bridge

Expected outcome: the flagship-used `Core -> ProofCore` extraction rules start
moving from trusted compiler behavior to Lean-proved preservation claims.

Phase closes when: the ProofCore constructs used by the four graduated
flagships have source semantics, extraction theorems, and a clear list of
remaining trusted assumptions.

1. Extend Phase 12 from R-01/R-02 literals to R-03 identifiers: source
   environment lookup, `cExprToPExpr (.ident name ty)`, and PExpr `.var`
   evaluation agree.
2. Discharge R-04 for simple width-agnostic binops (`add`, `sub`, `mul`) over
   the current proof value model.
3. Discharge R-05 comparisons and boolean result preservation.
4. Discharge R-06 let-binding preservation.
5. Discharge R-07 if/early-return preservation for the validator shape used by
   `parse_validate`.
6. Decide whether the mutual extraction block needs a structural-recursion lift.
   If later rules remain blocked by `partial`, replace `mapM` recursion with
   explicit structural helpers.
7. Build source semantics for the provable subset as needed by the discharged
   rules, not as a speculative full-language semantics.
8. Discharge calls/FnTable preservation for direct calls with complete proof
   tables.
9. Discharge structs and field access.
10. Discharge enum literals and match expressions.
11. Discharge arrays: literals, index reads, and in-bounds/OOB behavior.
12. Discharge casts under the current widening-only proof restriction.
13. Discharge width-tagged BitVec operations used by flagships: `mod`, `bitxor`,
   `bitor`, signed/unsigned result interpretation.
14. Discharge arraySet / functional update preservation.
15. Discharge flat bounded `while` preservation.
16. Discharge `while_step` / `LoopStep.Cont` / `LoopStep.Break` preservation.
17. Prove selected proof-report facts agree with compiler state: `proved`,
   `stale`, `blocked`, `missing`, `ineligible`, `trusted`.
18. Prove or mechanically validate trust-gate correctness: body fingerprint
   determinism, spec-drift completeness, proof attachment lookup, FnTable
   completeness, eligibility classification.
19. Record a machine-readable trusted computing base for proof/evidence claims:
   Lean kernel, compiler modules, backend/toolchain, runtime/OS/hardware,
   trusted/extern code.

## Phase 3: Freeze A Provable Systems Subset

Expected outcome: Concrete has a named small subset that users can rely on for
serious proof/evidence work.

Phase closes when: the subset has a public name, allowed constructs, rejected
constructs, arithmetic profile, runtime-error policy, and compatibility promise.

1. Define `ProvableV1`: allowed types, expressions, statements, effects, loops,
   trusted boundaries, allocation rules, and proof attachment requirements.
2. Define `PredictableV1`: no allocation unless bounded, no FFI unless trusted
   and assumed, no unbounded loops/recursion, explicit failure-path policy.
3. Freeze the first arithmetic profiles:
   wrapping, checked, and proved/no-overflow.
4. Carry arithmetic profile choices into diagnostics, reports, assumptions,
   proof obligations, and release bundles.
5. Define a first runtime failure model: abort, assertion failure, OOM, stack
   overflow, `defer`/cleanup, impossible branches, and what each does to
   proof/resource claims.
6. Define source-level stack-depth versus backend/target stack claims.
7. Define source-level constant-time profile v1:
   no secret-dependent branch, no secret-dependent memory index, fixed loop
   bounds, explicit backend timing assumptions.
8. Define secret/data-sensitivity labels for future security work:
   `public`, `secret`, `timing-sensitive`.
9. Add negative examples for every `ProvableV1` and `PredictableV1` exclusion.
10. Update `CLAIMS_TODAY.md`, README, showcase docs, and release bundles to use
    the frozen subset names consistently.

## Phase 4: Runtime-Error Obligation Generation

Expected outcome: Concrete starts generating SPARK-like obligations for boring
runtime failures instead of relying only on examples and prose.

Phase closes when: parser/security examples can show obligations for bounds,
div/mod zero, overflow profile, casts, and loop bounds with statuses
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
12. Prove or validate obligation-generation soundness for the first obligation
    kinds in Phase 12.

## Phase 5: Source-Level Contracts

Expected outcome: proof-relevant properties can live in source code without
breaking LL(1), and every contract generates obligations instead of becoming
decorative prose.

Phase closes when: one flagship uses source contracts as the primary proof
surface, and each contract is classified as proved, enforced, assumed, missing,
or blocked.

1. Add LL(1)-safe function attributes:
   `#[requires(...)]` and `#[ensures(...)]`.
2. Restrict v1 contract expressions to proof-friendly expressions:
   parameters, `result`, literals, comparisons, boolean operators, simple
   arithmetic, fixed array lengths, and named pure predicates where explicitly
   supported.
3. Store source contracts through Parse/Resolve/Check/Core and report them with
   source spans.
4. Add `--report contracts` with evidence statuses and links to generated
   obligations.
5. Connect contracts to the proof registry: a theorem can discharge a specific
   source contract id.
6. Add contract negative examples: unmet precondition at call site, missing
   postcondition proof, weakened postcondition, invalid contract expression.
7. Add loop attributes:
   `#[invariant(...)]` and `#[variant(...)]`.
8. Generate loop-invariant obligations: initialization, preservation, variant
   decrease, and exit-implies-postcondition.
9. Add source contract soundness work to Phase 12: parsing preserves meaning,
   generated obligations correspond to contract semantics, discharged
   obligations imply the advertised contract claim.
10. Add one contract-bearing flagship retrofit after the machinery is real.

## Phase 6: Proof Ergonomics And Automation

Expected outcome: proving flagship properties becomes a repeatable engineering
workflow, not a collection of one-off `simp` scripts.

Phase closes when: new flagship proofs can start from useful generated stubs,
standard lemmas, and actionable failure diagnostics.

1. Build reusable proof lemmas for arrays: lookup, update, length, in-bounds,
   OOB stuck behavior.
2. Build reusable lemmas for loop-carried state and `while_step`.
3. Build reusable lemmas for BitVec operations used by flagships.
4. Build reusable lemmas for structs, fields, enum construction, match, Result,
   Option, and bounded-buffer invariants.
5. Upgrade generated proof stubs for real shapes: arrays, structs, enums,
   fixed buffers, Result/Option, loops, and source contracts.
6. Add proof minimization/debugging UX: show the smallest extracted expression
   or lemma surface related to a failed proof.
7. Add proof replay/caching once proof artifacts and fingerprints are stable.
8. Add simple auto-discharge for structural obligations that do not need human
   proof search.
9. Add AI-assisted proof repair only after artifacts, statuses, and replay are
   stable enough to validate suggestions mechanically.

## Phase 7: Audit Artifacts And Review UX

Expected outcome: a reviewer can answer "what can this program do, what is
proved, what is assumed, and what changed?" without reading compiler internals.

Phase closes when: `concrete audit`, semantic diff, and an artifact viewer cover
the four graduated flagships and one package-scale example.

1. Stabilize machine-readable fact schemas for proof status, obligations,
   effects, capabilities, assumptions, policies, snapshots, and showcase
   metadata.
2. Add `concrete audit`: one human-readable plus machine-readable bundle
   covering authority, trust, allocation, proof status, obligations,
   assumptions, policy, snapshots, backend/target assumptions, and replay.
3. Add `concrete explain <function>`: capabilities, proof status, assumptions,
   obligations, trusted callees, evidence level, and why each status is what it
   is.
4. Add `concrete why <capability>`: explain why a function needs `File`,
   `Network`, `Alloc`, `Unsafe`, etc., including transitive call chains.
5. Add `concrete diff old new`: authority/proof/trust/runtime-obligation diff.
6. Add semantic trust diff gates: capability widening, allocation change,
   trusted boundary addition, stale proof, weakened/missing obligation,
   assumption widening.
7. Add an artifact viewer CLI/TUI over facts, obligations, proofs,
   assumptions, release bundles, and diffs.
8. Ensure every release bundle includes an evidence replay command.
9. Add evidence-level monotonicity checks to audit/diff output.
10. Add one AI-audit demo where an agent answers authority/proof/trust
    questions using compiler facts rather than source guesses.

## Phase 8: Flagship Depth

Expected outcome: Concrete has one example that outside systems engineers find
impressive, not only internally coherent.

Phase closes when: the showcase set includes a serious security/crypto or
protocol example with proof/evidence strong enough to anchor the public pitch.

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
7. Graduate one stronger real-crypto candidate with honest assumptions and an
   oracle.
8. Graduate one runtime-error-obligation flagship: parser/protocol example with
   no OOB/div-zero/overflow obligations discharged.
9. Graduate one authority/capability flagship: a privilege-separated tool whose
   trusted core cannot touch files/network/processes except through named
   wrappers.
10. Graduate one FFI-wrapper flagship: trusted C boundary, safe pure core,
    explicit assumptions, layout/ABI evidence.
11. Keep the curated showcase balanced: parser/protocol, bounded state,
    crypto/security, authority, FFI/trust, ownership-heavy.

## Phase 9: Backend, Target, And Compiler Contracts

Expected outcome: backend/toolchain assumptions are no longer vague, and
evidence claims state exactly where source-level proof stops.

Phase closes when: SSA, target/toolchain, optimization, ABI/layout, and
incremental build contracts are explicit enough for release evidence.

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
9. Evaluate a normalized mid-level IR only when traceability/backend-contract
   reports expose a concrete gap.
10. Keep QBE/WASM/second backend deferred until evidence attachment,
    optimization policy, and backend trust boundaries are trustworthy.

## Phase 10: Release Product Bar

Expected outcome: Concrete is understandable and usable by someone who did not
build the compiler.

Phase closes when: a fresh user can install Concrete, run a proof-bearing
example, inspect its audit bundle, and understand the claim matrix in under ten
minutes.

1. Define first public release criteria: supported subset, required examples,
   required diagnostics, proof workflow, stdlib/project UX, evidence/policy/
   tooling story.
2. Publish a public claim matrix: what Concrete proves, enforces, reports,
   assumes, and trusts.
3. Add release claim freeze: README, `CLAIMS_TODAY.md`, roadmap, showcase
   manifest, and release bundles must agree.
4. Add compatibility policy for proof artifacts and fact schemas.
5. Add public security/soundness disclosure policy: compiler/proof pipeline
   bugs are security-relevant.
6. Add first-user workflow CI: install compiler, create/run one example,
   inspect one audit bundle without repo-local assumptions.
7. Improve onboarding, tutorial, and docs around `proved` / `enforced` /
   `reported` / `assumed` / `trusted`.
8. Add positioning page against Rust, Zig, Lean, SPARK/Ada, Austral, Dafny,
   F*, Why3.
9. Add migration/adoption playbook: what C/Rust/Zig code moves first, how to
   wrap libraries honestly, what stays outside Concrete.
10. Add release/install distribution matrix: host triples, checksums/signing,
    install paths, supported/deferred channels.
11. Ship the first narrow public release only after the above are green.

## Phase 11: Packages And Ecosystem Evidence

Expected outcome: package users can inspect proof, trust, capability, and
assumption facts before adopting a dependency.

Phase closes when: packages have manifests, lockfiles, package-aware facts,
trust policies, provenance, and registry protocol.

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
10. Add package provenance and publishing model.
11. Add package registry server protocol and trust model.

## Phase 12: Editor And Human Tooling

Expected outcome: evidence is visible where developers work.

Phase closes when: editor/LSP/tooling exposes the same facts as CI and command
line reports without inventing a second truth source.

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

## Phase 13: Concurrency And Research-Gated Extensions

Expected outcome: speculative ideas stay gated until Concrete's proof/evidence
foundation can contain them honestly.

Phase closes when: each research idea is either pulled into an earlier phase by
a forcing example, explicitly deferred, or rejected.

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
11. Research persistent equality/rewrite state after backend contracts,
    semantic diff, and proof/evidence pipeline are stronger.

