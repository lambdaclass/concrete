# Concrete Roadmap

This document is the active execution plan. It answers one question:
**what should happen next, in what order?**

The roadmap is linear. Phases are ordered, and items inside a phase are ordered
unless explicitly marked as a constraint or a deferred research note. Read the
document as one queue:

1. harden source contracts: negative cases, vacuity, spec/ghost totality,
   trapdoor discipline, diagnostics, and soundness obligations;
2. finish VC generation, discharge examples, and external-SMT policy without
   hiding solver trust;
3. finish remaining proof-authoring cleanup: colocate example Lean proofs with
   their Concrete examples, keep generated proof workspaces source-linked and
   replayable, and leave the deeper `ProofCore` / spec-registry split deferred
   unless it becomes necessary;
4. harden audit / proof-status / trust gates around source contracts,
   spec provenance, evidence classes, tool-version drift, and oracle evidence;
5. close the release-blocking predictable/provable/runtime-safety gaps,
   starting with casts, loop-derived bounds, runtime-safety policy, and the
   remaining profile story after array bounds, div/mod-zero, and
   opt-in overflow obligations;
6. only then broaden the ordinary language surface (patterns, bytes/text/path,
   collections, iteration, capability polymorphism, tests);
7. run external validation before the large ecosystem/release/editor build-out;
8. keep later research items later unless a prior gate forces them.

Completed work moves to [CHANGELOG.md](CHANGELOG.md). Deferred or conditional
work moves later in the same linear queue. There are no parallel tracks. Inline
`NEXT` notes are allowed only as scoped follow-ups inside a numbered item; they
do not create a second queue.

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

Design bias: **explicitness over cleverness.** Authority, allocation, trust,
runtime failure, byte/text/path boundaries, and proof class must remain visible
in source or audit output. Do not hide them behind inference-heavy abstractions,
implicit conversions, ambient lookup, or broad metaprogramming.

## Cross-cutting decisions and checkpoints (recorded 2026-06-03)

These are decisions/checkpoints, not yet implementation. Each names the
evidence class so no construct stays semantically dark.

### Floats: usable now, provable only under an explicit profile

**Decision.** `f32`/`f64` are allowed as runtime types but are **outside
ProvableV1** unless a function opts into a future float profile. Unprofiled
float arithmetic is excluded from proof eligibility and audit-loud; no proof or
release claim may be made over float arithmetic until a profile exists.

**Provable Float V1 (future, narrow profile — see Phase 6).** A function opts
into a named profile, e.g. `#[float_profile(ieee754_binary64_nearest_even)]`:
- IEEE-754 binary32/binary64; round-to-nearest-even only at first.
- No fast-math; no reassociation; no FMA contraction unless the source names
  `fma`; no ambient rounding-mode mutation.
- Explicit NaN / infinity / subnormal / signed-zero policy.
- **Bit-level semantics first**: model floats as `BitVec 32/64`; add
  `PVal.float32/64` and float `PBinOp` cases (`fadd`/`fsub`/`fmul`/`fdiv`/`feq`/
  `flt`/`fle`) carrying width + rounding; interpreter agreement.
- **Honest trust label.** Lean's `Float` is opaque/native, not bit-level
  reasoning. The primitive IEEE ops start as a **trusted axiom layer**
  (`float_semantics_trusted`) or an imported checked library (Flocq-style)
  until derived. So v1 = "extracted body refines an explicit IEEE-754 bit-spec
  whose primitive ops are named trusted" — not `proved_by_lean` from first
  principles. Backend must prove/report `fast_math: forbidden`,
  `rounding: nearest_even`.
- First flagship: a small deterministic numeric kernel (clamp/normalize, a
  fixed-order dot product, a tiny IIR/FIR or PID), proved exact-IEEE — not
  scientific computing. Real-number ε-bound refinement is a later layer.

### Embedded hardware access: evidence classes (see Phase 12)

Name the evidence class before implementing freestanding/embedded targets:
- **inline asm** — `trusted`, requires `with(Unsafe)`.
- **volatile / MMIO** — explicit capability, e.g. `with(Device)` / `with(Mmio)`;
  reads/writes are audit-visible effects, never silently elided.
- **interrupt handlers** — a separate trusted/effectful boundary.

### Native debug info (see Phase 11)

Once Concrete emits real binaries, source-mapped DWARF / crash traces matter for
auditability. Tooling/backend item, not first-release proof-critical.

### Target-conditional code without hidden metaprogramming

Concrete needs target-specific code for hosted/freestanding builds, OS-specific
FFI, and stdlib splitting, but broad in-source conditional compilation would
pull the language toward macro-like configuration. Prefer module/file selection
through `Concrete.toml` target profiles. Add only narrow, audit-visible
`cfg`-style attributes if file/module selection proves insufficient.

Audit output must explain target selection:
- selected source roots/modules,
- excluded source roots/modules,
- target profile and platform reason,
- enabled build profile,
- and any `cfg` attributes if they are ever admitted.

### Contract-VC stability tiers (dependency edge into Phase 8)

The risk this names: Phase 9 flagships are what exercise contracts, and they
will keep hitting an un-frozen Phase 8 surface — so any VC/contract IR designed
in Phases 1-2 against that surface gets reworked when collections and the
iteration protocol land. The fix is **not** to reorder (that is circular: the
flagships are what stress-test Phase 8) but to tag each contract/VC construct by
stability tier and refuse to freeze syntax over the provisional tier. This is
the same discipline as "let the proof teach the syntax" (Phase 1 preamble),
made into an explicit dependency.

- **Frozen-safe** — obligations over integers, booleans, `BitVec`, and
  fixed-size arrays. This is the slab every shipped proof already stands on
  (HMAC, `ct_compare`, the loop VC). Contract/VC syntax here may be stabilized.
- **Provisional** — any obligation quantifying over collections, iterators,
  strings/text, bytes, paths, or capability-polymorphic callees. These depend
  on the **Phase 8 core slab** (modules/imports, minimal project model,
  `concrete test`, core diagnostics, bytes/text/path, and collections). They
  also remain provisional over later iteration and capability-polymorphism
  work. Do not freeze contract syntax or VC shape for those constructs until
  the relevant Phase 8 item has landed. Treat any such construct as "will be
  reworked," and do not let a flagship bake an iterator/collection assumption
  into the VC shape.

### Spec and contract trust (no dark specs)

Concrete proves source against specs, but that is not enough by itself. The
spec/contract layer must also have evidence classes, or the system can produce
a beautiful proof of the wrong theorem. Apply the same rule one layer higher:
no semantically dark specs, no hidden vacuity, no partial ghost/spec functions,
and no pretending a monomorphic proof covers every future generic
instantiation.

- Specs need provenance and review status: external standard, independent
  reference, test-vector set, reviewer, remaining assumptions, and trust class.
- Contracts need satisfiability/vacuity checks: an impossible precondition or
  unreachable path must not show up as an ordinary proof.
- `spec fn` and ghost computations used in contracts must be pure, erased, and
  total, with a named obligation when totality is not inherited from Lean.
- Generic and capability-polymorphic proofs must state their scope:
  per-instantiation, generic-once, or generic contract with instance-level proof
  artifacts.
- Proof evidence is relative to the toolchain that checked it. Lean,
  `bv_decide`, `omega`, ProofKit, extraction, and compiler-version changes need
  drift detection and recheck status, not silent reuse of old green badges.
- References and borrows need an explicit proof class: value-only/borrow-free,
  read-only-reference proof, mutable-reference proof with frame obligations, or
  enforced-only and outside `ProvableV1`.

### External-validation gate (go / no-go before the back half of Phases 8-15)

This is a **gate, not a note** — promoted out of Phase 13 because validation
that sits downstream of the build-out it is meant to justify is no validation
at all. The central bet of the whole project is "evidence-carrying source is
worth the discipline." Today the only person who has found it worth the cost is
the person who built it, yet Phases 8-15 (packages, editor, freestanding,
release) are a large investment fully predicated on that bet. The research under
`research/` and `thesis-validation/` tests the thesis but is currently orphaned
from the execution plan; this gate wires it in.

It cannot be "before any Phase 8" — there is a chicken-and-egg floor: an outside
user needs *some* slab to write anything real. So the gate is:

1. **The minimum slab is the Phase 8 core slab**: modules/imports, minimal
   project model, `concrete test`, core diagnostics, bytes/text/path, and
   collections.
2. **Build exactly that** — not the full back half.
3. **Run the trial and treat the result as an explicit go / no-go on the rest
   of Phases 8-15.**

The trial should be implemented as the first external-user workload in the
Phase 9 real-workload ladder, not as a separate validation artifact.

**Pass criterion:** at least one person who is **not** the compiler author
writes, proves, or contract-annotates a useful Concrete program and reports that
the proof discipline (ProofKit + contracts + `concrete prove`) was worth the
cost. Until this passes, the back half of Phases 8-15 is flagged **at-risk**,
not green.

---

## Phase 1: Source Contracts

Goal: let proof-relevant properties live in source code without breaking LL(1),
and make every contract generate obligations instead of becoming decorative
prose.

Design reference: [docs/CONTRACTS_AND_VCS.md](docs/CONTRACTS_AND_VCS.md), and
[docs/PROOF_LADDER.md](docs/PROOF_LADDER.md) for the build order.
Usage reference (what works today): [docs/CONTRACTS_GUIDE.md](docs/CONTRACTS_GUIDE.md).

**Ordering (let the proof teach the syntax):** do not freeze contract syntax or
VC shapes before a real refinement proof exists. The spec layer, the `bv_decide`
tier, the refinement pattern (`ch_refines` / `maj_refines`), and the first loop
refinement (`block_to_words_refines_spec`) have all shipped and are recorded in
the changelog, so
this phase now designs against obligation shapes that have actually been
discharged, including a real `eval_while_count` loop obligation, rather than
imagined ones. See the build order in `docs/PROOF_LADDER.md`.

Done when: the source-contract path is hardened beyond the first flagship:
negative cases are covered, diagnostics are actionable, source-contract
soundness obligations are named in the compiler-soundness bridge, and HMAC
retrofit is explicitly queued behind proof-link migration.

1. Finish the source-contract hardening gate before Phase 2. The remaining
   tasks in this phase are the contract cases that can make a green proof
   misleading: negative examples, vacuity, spec/ghost totality, trapdoor
   discipline, diagnostics, API-stability rules, and soundness obligations.
2. ~~Add vacuity and satisfiability checks for contracts.~~ **MOSTLY DONE.**
   Report-side detection (keeps the file compilable): `#[requires(false)]` and
   other constant-false preconditions (folder), contradictory assumptions like
   `x>0 && x<0` (omega refutes `∀vars, ¬(conjunction)`, `Report.vacuityGoals`),
   and `#[invariant(false)]` are reported `vacuous` / `invalid/vacuous`, never
   `proved` (`--report contracts` flags `⚠ VACUOUS` + marks each `#[ensures]`
   `vacuous`). The release/policy gate rejects vacuous contracts by default
   (E0613, `Policy.enforceNoVacuous` + `Main.computeVacuousQuals`). Fixtures:
   `examples/contract_negatives/vacuous_contract/` (the three report cases) +
   `tests/programs/adversarial_policy_vacuous/` (policy rejection). Gate
   `check_contract_negatives.sh` 15/0; zero false positives across the flagships.
   REMAINING (deeper, path-sensitive): unreachable returns and postconditions
   proved only because a path is impossible.
3. ~~Add `spec fn` / ghost totality rules.~~ **DONE (purity) / N/A (spec-fn totality).**
   `spec fn` is body-less and Lean-backed, so its totality is inherited from Lean
   — there is no Concrete-level spec-fn body to reject. The checkable Concrete-side
   rule is that the spec/ghost language is PURE and TOTAL: a contract calling a
   capability-requiring (effectful) function is rejected report-side
   (`Report.impureFnNames` + `contractImpureCalls`, wired into requires/ensures/
   invariants/variants) as `invalid_contract_expression: impure call '<fn>' …`.
   `examples/contract_negatives/spec_ghost_totality/` covers the negative (an
   effectful call) and a positive control (pure helper + `spec fn`, not
   over-rejected); zero false positives across the flagships. (Deeper ghost-value
   totality — e.g. a partial op inside a `ghost let` value — remains a possible
   follow-up.)
4. **[done]** `assert` / `assume` trapdoor discipline. First-class `Stmt`
   constructs (`assert(e);` / `assume(e);`), type-checked to `bool` (E0220),
   erased before Core (they may read ghost state in proof context). `assert(e)`
   generates an omega obligation folding the function's `#[requires]`:
   `proved_by_kernel_decision` when discharged, `unproven` when not (never
   silently accepted), `VIOLATION` when always-false. `assume(e)` is audit-loud:
   evidence class `assumed` (never upgraded to proof) and the function is marked
   `⚠ TAINTED`; the `forbid-assume` release profile rejects it (E0614). Reported
   in a `=== assert / assume ===` section (`Report.assertGoals` /
   `renderAssertAssume`). `examples/contract_negatives/assert_obligation/`
   (closed-by-omega / unproven / false) and `assume_taint/` (taint + clean
   sibling + policy rejection) are pinned by `check_contract_negatives.sh`
   (24/0, +8).
5. **[done]** Contract diagnostics taxonomy + positive resolver fixture. Every
   failure class is now self-labelled and placed: caller-side precondition lives
   in `=== Call-site obligations ===` (`unproven_at_callsite (caller does not
   establish …)` / `failed_at_callsite`); callee-side postcondition is the O*
   `ensures` status (`missing` / `proved_by_lean` / `partial — one direction
   proved_by_lean, converse outstanding`); loop obligations are O1 init / O2
   preservation / O4–O5 variant; vacuity is `⚠ VACUOUS`; bad/stale proof links
   surface in `--report proof-status`; non-total spec/ghost is
   `invalid_contract_expression: impure call …`. The companion positive fixture
   `examples/contract_positive/valid_complex_contract_scope/` mentions every
   legal name a contract can (params, `result`, module constant, pure helper
   call, `spec fn` call, loop counter, `ghost let`, ordinary local) and is pinned
   to produce ZERO false positives by `check_contract_negatives.sh` (31/0, +7) —
   over-eager scope-checking is as dishonest as a missed obligation.
6. Add contract stability rules: weakening a precondition, strengthening a
   postcondition, or changing a public invariant is a semantic API change.
7. Add source contract soundness work to the compiler soundness bridge: parsing
   preserves meaning, generated obligations correspond to contract semantics,
   discharged obligations imply the advertised contract claim, and source proof
   links imply the same claim class as their generated registry entry. Include
   satisfiability/vacuity and spec/ghost totality in this soundness story.
8. Add `hmac_sha256` source-contract retrofit only after proof-link migration
   and `concrete prove` examples make the small proof path routine. Do not
   start by moving the HMAC chain; use it as the late regression anchor for the
   mature source-link path.
9. Add the Phase 1 validation artifact: the combined contract-positive,
   contract-negative, vacuity, totality, assert/assume, and stability fixtures
   must run under one gate, produce one report snapshot per contract failure
   class, and have a README explaining what each fixture proves, reports, or
   deliberately refuses to prove.

## Phase 2: Verification Conditions And SMT Assistance

Goal: get closer to SPARK-style automation without hiding solver trust or
replacing Lean-checked theorem claims.

Design reference: [docs/CONTRACTS_AND_VCS.md](docs/CONTRACTS_AND_VCS.md).

Done when: contracts, bounds checks, arithmetic side conditions, and simple loop
invariants generate machine-readable VCs; a solver can discharge the easy ones;
counterexamples are reported clearly; and audit output distinguishes Lean,
SMT, tests, enforcement, assumptions, and trusted solver claims.

1. Finish the VC/discharge gate before Phase 3. The remaining tasks in this
   phase are the VC schema, generation coverage, kernel-checked decision path,
   arithmetic bridge library, explicit external-SMT trust model, examples,
   counterexamples, replay/determinism gates, and audit integration.
2. Define VC schema v1: id, source span, kind, hypotheses, conclusion,
   originating contract/obligation, dependencies, arithmetic profile, and
   expected discharge mode.
3. Generate VCs for pure no-loop contracts first: preconditions at call sites
   and postconditions at returns.
4. Generate VCs for the runtime-safety obligations that already exist today:
   array bounds, div/mod nonzero, and `#[overflow_checked]` no-overflow claims.
   Later Phase 7 work extends this same VC path to casts, panic/abort,
   byte/text/path boundaries, stack/recursion, and any new runtime-error
   obligation kinds.
5. Generate VCs for loop invariants: initialization, preservation,
   variant decrease, and exit-implies-postcondition.
6. **Kernel-checked automation first (`bv_decide`).** Before any external
   solver, route BitVec / bounded-arithmetic VCs to Lean's `bv_decide`
   (in-toolchain; bit-blasts to SAT and replays a kernel-checked certificate —
   **no TCB growth**). Already validated against the HMAC helper facts. Its
   results are classified `proved_by_kernel_decision`, a kernel-checked class
   distinct from `proved_by_smt`. See [docs/PROOF_LADDER.md](docs/PROOF_LADDER.md).
7. Centralize arithmetic bridge lemmas before adding more crypto/protocol VCs:
   `Int` / `Nat` / `BitVec` round trips, division/modulo, shift index
   arithmetic, byte/word packing, and symbolic bridge cases like the
   `sdiv`/`Nat.div` proof from HMAC padding. These are proof-library
   primitives, not one-off flagship lemmas.
8. Add an *external* SMT backend behind an explicit flag or policy gate, reached
   for only when `bv_decide` cannot (e.g. nonlinear). Start with one solver
   adapter and a stable SMT-LIB output path before adding more solvers. Its
   results are `solver_trusted` (solver enters the TCB) unless a certificate is
   replayed — never collapsed into a kernel-checked class.
9. Classify solver results in reports and artifacts:
   `proved_by_kernel_decision` (kernel-checked), `proved_by_smt` /
   `solver_trusted` (external), `unknown`, `counterexample`, `timeout`,
   `solver_error`.
10. Surface counterexamples in source terms where possible: function inputs,
   loop variables, failing index, failing arithmetic side condition, and the
   contract/obligation that failed.
11. Add CI gates for solver determinism and replay: same VC, same solver
   configuration, same result class, with timeouts treated as non-proofs.
12. Add Lean replay for the simplest SMT-discharged fragments where practical:
   propositional/linear integer facts, bounds arithmetic, and trivial BitVec
   identities. Results without replay remain explicitly solver-trusted.
13. Add policy controls: projects can require `proved_by_lean`, allow
    `proved_by_smt`, or permit `solver_trusted` only under named assumptions.
14. Add SMT negative examples: false postcondition, missing invariant,
    overflow counterexample, OOB counterexample, div-zero counterexample,
    solver timeout, and unsupported theory.
15. Add a compact VC/discharge example suite before external SMT:
    - `proved_by_lean`: straight-line refinement (`ch`), operational loop
      preservation, and one call-composition theorem that cannot be closed by a
      decision procedure.
    - `proved_by_kernel_decision (omega)`: linear integer facts, loop
      `invariant_init`, `variant_nonnegative`, `variant_decreases`, and
      `exit_implies_post`.
    - `proved_by_kernel_decision (bv_decide)`: `rotr`, byte packing, xor/or bit
      facts, fixed-width wrapping arithmetic, and call-site constant bounds.
    - `partial`: one-direction postcondition proved, converse outstanding.
    - `stale`: source-linked proof whose body drifted from the extracted spec.
    - `missing`: proof-eligible function with no proof link yet.
    - `assumed`: precondition assumed at entry and a named timing assumption.
    - `trusted`: FFI/backend/unsafe wrapper that is intentionally outside the
      proof path but audit-visible.
    - `runtime_checked`: deliberately unproved runtime-error obligation checked
      dynamically, with audit showing it is not proof.
    - `tested_by_oracle`: small function checked against a reference
      implementation, with audit showing it is regression evidence, not proof.
    These examples are release-facing documentation fixtures: every evidence
    class should have one small program and one report snapshot.
16. Add a clear external-SMT example suite only after the backend exists and
    only behind an explicit policy flag. These examples must teach when SMT is
    useful, when it is trusted, and when Concrete should prefer Lean/kernel
    decision procedures. Put them under a dedicated example group such as
    `examples/smt/`, with one small program per case, a README, report
    snapshots, and a gate that asserts the expected evidence class. Use this
    sequence:
    - `range_block_count`: HMAC-shaped symbolic block-count arithmetic, e.g.
      prove `(len + 9 + 63) / 64 <= max_blocks` under a length bound. This is
      the "SMT is useful for arithmetic summaries" example. Report
      `proved_by_smt` / `solver_trusted` unless Lean replay closes it.
    - `nonlinear_overflow`: a product/range obligation such as
      `sample * gain` under interval bounds where `omega` is not enough. This
      must clearly say why `omega` does not own it and whether the result is
      solver-trusted or replayed.
    - `path_feasibility`: several guarded branches imply a postcondition, e.g.
      a clamp/classifier where each branch has different arithmetic facts.
      This shows SMT over path conditions, not ordinary array bounds.
    - `false_postcondition_counterexample`: deliberately false `#[ensures]`
      with a source-level model: concrete inputs, failing branch, and failing
      postcondition. Status is `counterexample`, never proof.
    - `overflow_counterexample`: an opt-in `#[overflow_checked]` function with
      insufficient bounds. Report concrete inputs that overflow or a symbolic
      counterexample if concrete reconstruction is unavailable.
    - `div_zero_counterexample`: a missing nonzero divisor proof. Report the
      path/inputs where divisor can be zero.
    - `unknown_or_timeout`: a deliberately hard quantified/nonlinear case with
      a small timeout. Status is `unknown` or `timeout`, treated as non-proof
      and blocked from release unless policy allows it.
    - `unsupported_theory`: a construct outside the SMT encoding. It must fail
      with an explicit diagnostic naming the unsupported theory rather than
      silently dropping the obligation.
    - `kernel_preferred`: a near-duplicate fact already closed by `omega` or
      `bv_decide`, proving the tool does **not** route ordinary fixed-array
      bounds, linear integer facts, or BitVec identities through external SMT.
      This is the anti-example that protects the trust boundary.
    Every SMT example must print solver name/version, timeout, encoding hash or
    SMT-LIB path, replay status, and trust class. Do not use external SMT for
    facts already enforced by Concrete or closed by `omega` / `bv_decide`, such
    as ordinary fixed-array bounds.
17. Update audit/release bundles so VC results appear beside proof registry,
    assumptions, runtime obligations, and proof coverage classification.
18. Add soundness documentation for the SMT path: trusted solver binary,
    encoding assumptions, unsupported theories, replayed fragments, and how a
    solver bug affects each claim class.
19. Add the Phase 2 validation artifact: a VC/discharge project that runs the
    compact VC suite plus the external-SMT suite when enabled, checks every
    solver/kernel evidence class, pins counterexample output, records solver
    name/version/encoding hash, and proves ordinary linear/bv obligations stay
    on `omega`/`bv_decide` rather than drifting into external SMT.

## Phase 3: Proof Authoring And Automation

Goal: make flagship proofs a repeatable engineering workflow, not a collection
of one-off `simp` scripts.

Done when: new flagship proofs can start from useful generated stubs, standard
lemmas, and actionable failure diagnostics.

1. Move example Lean proofs physically next to their Concrete examples. Target layout:
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
   `SpecRegistry` split.
2. Add proof minimization: `concrete prove --minimize <obligation_id>` emits
    the smallest source / ProofCore / Lean slice needed to reproduce a failed
    obligation. This should be built after JSON and failed-artifact formats are
    stable, not before.
3. Define and document stable theorem naming conventions in tool output:
    `<fn>_refines_spec`, `<fn>_<obligation>_proved`,
    `<fn>_loop_<name>_preserves`, and
    `<fn>_call_<callee>_discharges_requires`. `concrete prove` should suggest
    these names instead of leaving agents to invent them.
4. Add CI gates for the agent-facing proof surfaces: snapshot representative
    `--json` output, validate schema versioning, ensure generated Lean stubs
    parse/check up to the intended placeholder boundary, assert replay JSON
    reports the same statuses as human replay, and assert proof-check JSON maps
    a failing Lean proof back to the intended obligation id.
5. Add human docs only after the binary path exists:
    `docs/AGENT_PROOF_AUTHORING.md` and an optional repo-root `AGENTS.md`
    should summarize the binary workflow and point to the ProofKit guide, but
    they must not be the source of truth for agents using only an installed
    binary.
6. Add MCP only after the CLI/JSON/stub/workspace surfaces are stable. The MCP
    server should wrap the binary rather than duplicate logic, exposing resources such
    as `concrete://prove/<fn>/obligations`, `concrete://proofkit/lemmas`, and
    `concrete://examples/evidence-classes`, plus tools for `prove_json`,
    `show_obligation`, `emit_lean`, `check`, `replay`, and `check_proofs`.
7. Build reusable proof lemmas for arrays: lookup, update, length, in-bounds,
    OOB stuck behavior.
8. Build reusable lemmas for loop-carried state and `while_step`.
9. Build reusable lemmas for BitVec operations used by flagships.
10. Build reusable lemmas for structs, fields, enum construction, match, Result,
    Option, and bounded-buffer invariants.
11. Upgrade generated proof stubs for real shapes: arrays, structs, enums,
    fixed buffers, Result/Option, loops, source contracts, and refinement
    composition. Stubs should emit spec target, `PExpr` body, FnTable skeleton,
    expected theorem statement, common imports/tactics, and TODO blocks for
    loop invariants. These items enrich what `--emit-lean` produces; they do
    not introduce a second stub generator.
12. Add generated composition scaffolds: FnTable entries, call lemmas, callee
    refinement dependencies, and composed theorem skeletons.
13. Add generated loop-invariant templates for common proof shapes:
    counter loop over array writes, copy loop, fold loop, multi-store loop,
    offset loop, and block-processing loop.
14. Improve failed-proof diagnostics after `--json`, failed artifacts, and
    `--minimize` exist: classify common failures into actionable categories
    such as missing callee theorem, stale source link, missing table entry,
    failed arithmetic bridge, insufficient frame fact, and spec/extraction
    mismatch. Diagnostics should point to the already-generated artifact or
    next action instead of introducing another parallel proof surface.
15. Add proof-result caching once proof artifacts and fingerprints are stable.
16. Add simple auto-discharge for structural obligations that do not need human
    proof search.
17. Add a small verified/spec-checked standard proof library for common
    predicates: sorted, bounded, no-duplicates, fixed-length, prefix, checksum,
    constant-time source shape.
18. Add AI-assisted proof repair only after artifacts, statuses, and replay are
    stable enough to validate suggestions mechanically.
19. **Frame inference (the proof-scaling cliff).** Every loop/state proof must
   establish not just what an iteration *changes* but what it *preserves* — the
   frame problem (Smallfoot 2006; later Infer; separation logic's frame rule:
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
20. Deferred architecture refactor: split the current `Concrete.Proof` layering
   so registered example specs can move without a cycle, but do not let this
   block Phase 8 unless spec ownership or proof authoring starts depending on
   it. Target shape: `Concrete.ProofCore` owns `PExpr`, `PVal`, evaluation,
   `FnTable`, and source-independent semantics; `Concrete.SpecRegistry` owns
   the spec-drift table and imports whichever example spec modules it registers;
   `Concrete.Proof` becomes the generic proof-theorem / compatibility umbrella.
   Only after this split should registered example SPEC PExprs move from
   `Concrete.Proof.*Expr` into `Concrete.Examples.<Ex>.Proofs` or sibling
   `Specs` modules. Preserve the spec-drift tie throughout.
21. Add the Phase 3 validation artifact: a proof-authoring project that
   exercises `--json`, `--show-obligation`, `--emit-lean`, `--emit-artifacts`,
   `--workspace`, `--check`, `--nearest-lemmas`, `--minimize`, and source-linked
   proof attachment across straight-line, array update, loop copy, fold,
   composition, ghost, stale, missing, partial, and repair cases. The gate must
   typecheck generated stubs, reject any `proof-registry.json`, and verify that
   failing Lean proofs map back to stable obligation ids.

## Phase 4: Audit Commands And Review Artifacts

Goal: let a reviewer answer "what can this program do, what is proved, what is
assumed, and what changed?" without reading compiler internals.

Done when: `concrete audit`, semantic diff, and an artifact viewer cover the
five graduated flagships and one package-scale example.

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
10. Make `tested_by_oracle` evidence structured and diffable:
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
11. Add spec provenance and adequacy facts to audit/release bundles: spec name,
    source standard or paper, independent reference if any, test-vector set,
    reviewer, review date, assumptions, and evidence class
    (`spec_trusted`, `spec_reviewed`, `tested_by_oracle`, or future
    `spec_refines_standard`). Do not let a source-to-spec proof imply the spec
    itself is adequate.
12. Add evidence-level monotonicity checks to audit/diff output.
13. Add one AI-audit demo where an agent answers authority/proof/trust
    questions using compiler facts rather than source guesses.
14. Add review checklists generated from facts: what changed, what widened,
    what became trusted, what lost proof, what gained assumptions, and which
    obligations remain open.
15. Add artifact redaction/stability rules so release bundles can be shared
    publicly without leaking local paths, secrets, or machine-specific noise.
16. Keep audit, contracts, obligations, assumptions, policies, manifests, and
    proof-status output on one shared vocabulary. Do not let each artifact grow
    its own mini-language for the same evidence classes.
17. Keep public-facing docs and website copy grounded in the same evidence
    vocabulary. Use `docs/WHY_CONCRETE.md` as the source for a C/Rust-oriented
    "why this exists" page: small systems code, explicit authority, visible
    evidence classes, spec-drift-tied proofs, named trust boundaries, and what
    Concrete deliberately avoids. The website should show the end goal and the
    current honest status, not catchy slogans or one-badge proof claims.
18. Add the Phase 4 validation artifact: one package-scale audit bundle fixture
    with human and JSON output, semantic diff before/after a change, artifact
    viewer smoke test, oracle manifest, spec-provenance facts, redaction check,
    replay command, and a README showing how a reviewer answers authority,
    proof, trust, assumption, and runtime-obligation questions without reading
    compiler internals.

## Phase 5: Proof Status And Trust Gates

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
3. Add per-obligation proof/evidence status. Function-level status is only a
   summary; padding, block fold, digest serialization, final composition,
   contract clauses, runtime obligations, oracle checks, and assumptions each
   carry their own evidence class.
4. Keep oracle-tested evidence separate from Lean/spec refinement. Oracles are
   implementation sanity and regression evidence, not proof completion.
5. Add proof debugging output for failed/stale proofs: extracted spec, current
   fingerprint, registered fingerprint, expected theorem shape, missing callee
   facts, likely missing lemma class.
6. Add evidence provenance to proof/evidence facts: source file/span, compiler
   commit, theorem name, spec name, policy file, assumption file, tool version,
   and replay command where available.
7. Add tool-version drift checks: proof/evidence facts record the Lean version,
   Concrete compiler commit, ProofKit hash, extraction version, decision
   procedure version, and solver version where relevant. A toolchain bump marks
   affected evidence `needs_recheck` until replayed; old green badges are never
   silently reused across a proof-tool upgrade.
8. Add evidence monotonicity checks: a refactor cannot silently present a weaker
   claim as if it were still stronger (`proved` cannot degrade to `reported`
   while retaining the same badge/summary).
9. Add assumption lifecycle checks: every assumption has an owner, scope,
   rationale, review date, affected claims, and a diff gate when it widens.
10. Add a trust-boundary inventory report: all `trusted`, `Unsafe`, extern,
   backend, runtime, and target assumptions in one machine-readable list.
11. Add spec-adequacy gates: release policy can require reviewed spec
    provenance for selected claims, forbid unreviewed specs in graduated
    flagships, and show when a theorem is `proved_by_lean` against a
    `spec_trusted` or unreviewed spec.
12. Add vacuity gates to proof status: `proved` summaries must be downgraded or
    blocked when the proof depends on an unsatisfiable precondition,
    contradictory assumptions, unreachable code path, or invariant `false`.
13. Add the Phase 5 validation artifact: a trust-gate pressure project that
    includes transitive proof dependencies, stale dependency propagation,
    tool-version drift, assumption widening, spec-adequacy policy, vacuity
    downgrade, weaker-evidence monotonicity, and a release gate proving each
    status cannot be silently presented as stronger evidence.

## Phase 6: Provable And Predictable Subsets

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
9. Decide the proof class for references and borrows. A function using `&` or
   `&mut` must be classified as one of: value-only/borrow-free,
   proved over read-only references, proved over mutable references with
   explicit frame/modifies obligations, or enforced-only and outside
   `ProvableV1`. Do not let borrow-using code appear proof-eligible through a
   value-only ProofCore model.
10. Define the v1 threat model: adversary, trusted base, proof scope, backend
   scope, side-channel scope, dependency scope, and what remains out of model.
11. Add negative examples for every `ProvableV1` and `PredictableV1` exclusion.
12. Update `CLAIMS_TODAY.md`, README, showcase docs, and release bundles to use
    the frozen subset names consistently.
13. Close the unprofiled-float proof hole before any float proof claim:
    float-typed params/returns/locals/literals/ops are excluded from ProofCore
    extraction unless an explicit float profile is active. Audit output must
    say `float semantics: unprofiled` and `proof eligibility: excluded` rather
    than reporting a float operation as extracted through integer `PBinOp.add`.
14. Define `ProvableFloatV1` as a separate, narrow proof profile:
    IEEE-754 binary32/binary64, round-to-nearest-even, no fast-math, no
    reassociation, no implicit FMA contraction, no ambient rounding-mode
    mutation, and explicit NaN/infinity/subnormal/signed-zero policy.
15. Add ProofCore support for profiled floats only after item 13 is closed:
    `PVal.float32/64`, float `PBinOp` cases carrying width and rounding
    (`fadd`/`fsub`/`fmul`/`fdiv`/`feq`/`flt`/`fle`), interpreter agreement,
    and backend/audit checks that prove/report `fast_math: forbidden`.
16. Classify the first float semantics layer honestly. Until Concrete imports
    or proves a checked IEEE-754 semantics library, primitive float operations
    are `float_semantics_trusted`; proofs over profiled float code are
    refinements to an explicit bit-level IEEE spec under that named trusted
    primitive layer, not `proved_by_lean` from first principles.
17. Add one small `ProvableFloatV1` flagship only after the profile exists:
    a fixed-order `f32`/`f64` kernel such as clamp/normalize, tiny FIR/IIR, PID,
    or dot product. Prove exact IEEE behavior first; real-valued epsilon-bound
    refinement is a later layer.
18. Add the Phase 6 validation artifact: a profile matrix project covering
    `PredictableV1`, `ProvableV1`, unprofiled-float exclusion, profiled-float
    admission, borrow/reference proof-class decisions, constant-time source
    shape, stack/runtime-failure assumptions, and negative examples for every
    exclusion. The gate must prove reports never call excluded code proof
    eligible.

## Phase 7: Runtime Safety Obligations

Goal: generate SPARK-like obligations for boring runtime failures instead of
relying only on examples and prose.

Done when: parser/security examples can show obligations for bounds, div/mod
zero, overflow profile, casts, and loop bounds with statuses
`proved`, `enforced`, `assumed`, `missing`, or `blocked`.

1. Define stable obligation schema v1: id, kind, source span, function,
   expression, dependencies, evidence status, discharging theorem/check/
   assumption, and replay command.
2. Define the user-level error model: `Result`, `Option`, assertion failure,
   abort/panic, recoverable errors, test failures, and how error flow interacts
   with capabilities, proofs, runtime obligations, and audit output.
3. Generate narrowing/invalid-cast obligations.
4. Generate loop bound and variant obligations for bounded loops.
5. Define policy gates for `#[overflow_checked]`: release profiles may require
   overflow obligations for selected functions/packages, while ordinary
   examples remain quiet unless they opt in. Reports must distinguish
   `overflow_checked`, `overflow checking not requested`, and explicit wrapping
   or saturating arithmetic.
6. Generate obligations for panic/abort/assert-as-denial-of-service risks:
   unchecked indexing, unwrap-like operations, explicit abort paths, failed
   assertions, and profile-dependent panic behavior.
7. Generate byte/text/path boundary obligations: invalid UTF-8, lossy
   conversion, OS-string conversion, path normalization assumptions, and
   rejected implicit conversions.
8. Generate stack/recursion obligations where the profile claims boundedness.
9. Report runtime-error obligations in human and JSON forms.
10. Add policy gates that can require selected runtime-error obligations to be
   proved/enforced before graduation.
11. Add a runtime-error regression corpus: invalid cast, loop-bound violation,
    lossy byte/text conversion, ignored fallible result, unwrap-like failure,
    panic/abort profile mismatch, and release-policy rejection for missing
    `#[overflow_checked]` evidence where required.
12. Add a runtime-error-obligation flagship requirement: one graduated example
    must demonstrate no OOB/div-zero/overflow under a named profile.
13. Add high-quality diagnostics for obligation failures: violated obligation,
    source expression, required evidence, current status, and next action.
14. Add obligation suppression only through explicit assumptions or policy
    waivers, never comments or hidden allowlists.
15. Prove or validate obligation-generation soundness for the first obligation
    kinds through the compiler soundness bridge.
16. Add the Phase 7 validation artifact: a runtime-safety corpus covering
    bounds, div/mod-zero, overflow, casts, panic/abort/assert, byte/text/path
    boundaries, stack/recursion, and obligation suppression. Each case must show
    one of `proved`, `enforced`, `assumed`, `missing`, or `blocked`, include a
    negative variant, and run through policy gates plus human/JSON reports.

## Phase 8: Language Usability And Daily Workflow

Goal: make Concrete usable as a normal experimental language, independent of
whether a user is writing proofs.

Done when: a new user can format, build, run, test, diagnose, inspect, and
debug small Concrete programs with predictable commands and useful errors.

The first six items are the **Phase 8 core slab**. Build them before the
external-validation trial, medium real-workload examples, or any contract/VC
syntax that depends on collections, bytes/text/path, or project layout. The
rest of Phase 8 stays after that slab in the same linear queue.

1. Stabilize modules and imports before packages grow: module names, file
   layout, visibility, import resolution, cycle diagnostics, and generated
   interface summaries.
2. Add a minimal project model before full packages: `Concrete.toml` fields for
   name, entry points, tests, policies, assumptions, source roots, build
   profiles, target profiles, oracle manifests, and evidence gates. The file
   must make authority, assumptions, runtime-check policy, and proof policy
   visible; it must not become an ambient hidden configuration channel.
3. Add `concrete test`: discover and run user tests, example tests,
   expected-failure tests, interpreter-vs-compiled differential tests,
   snapshot tests, oracle tests, and policy/assumption gates through one
   command.
4. Improve diagnostics for parser, resolver, type checker, ownership, linearity,
   capability, unsupported-construct, and codegen/interpreter mismatch errors:
   every diagnostic has a source span, reason, and next action.
5. Define strings, bytes, paths, and OS strings: `Bytes` for raw data, `Text`
   for validated UTF-8, and `Path`/`OsString` for OS-native boundaries. Specify
   literals, ownership, slicing, indexing, formatting, conversions, parser/JSON
   interaction, diagnostics, and test output. No implicit lossy conversion.
6. Define the collections story: fixed arrays, slices, dynamic `Vec`, maps,
   buffers, parser cursors, and which collections require `Alloc` or other
   capabilities.
7. Add `concrete fmt`: stable formatting for source files, examples, docs
   snippets, and generated fixtures. Formatting must not churn semantic
   fingerprints.
8. Add `docs/GRAMMAR.md`: LL(1) grammar, reserved keywords, attribute syntax,
   contract syntax, `ghost`/`assert`/`assume`, iteration syntax, and negative
   parser fixtures. This is a syntax reference, not a language-design
   committee.
9. Add plain type aliases before the larger stdlib/examples slab:
   `type Digest = [u8; 32]`, `type Tag = [u8; 16]`, etc. Aliases must be
   transparent to layout, extraction, and proof unless explicitly declared as a
   future opaque/newtype form. This is an ordinary readability feature, not a
   proof abstraction.
10. Decide user-facing loop control before broad parser/service examples:
   `break`, `continue`, and whether labeled loops exist. The decision must
   state how each interacts with bounded-loop analysis, cleanup/defer,
   contracts, and runtime-safety obligations. If deferred, examples should use
   explicit state flags or early returns instead of hidden control flow.
11. Close the match/pattern ergonomics gap before broad `Result`/`Option` and
   protocol-decoder work. This is one compound usability block: algebraic data
   types are already in the language, so the pattern language must be
   expressive enough to use them without stacks of boilerplate matches.

   **Current baseline:** `_` wildcard *match arms*
   (`tests/programs/wildcard_pattern.con`); integer/value patterns (`litArm`)
   and variable patterns (`varArm`); `let`-destructuring including `let … else`
   (`letDestructure` with `elseBody`); struct destructuring
   (`let Struct { fields } = expr;`, `letStructDestructure`).

   **Still open (each: implement, or explicitly defer with examples):**
   - match guards — `Pattern if condition => …`
   - OR patterns — `A | B => …`
   - `if let`
   - `while let`
   - nested patterns
   - tuple types, or a deliberate no-tuples decision (record which)
   - struct update syntax — `Struct { field: x, ..base }`
   - `_` wildcard *inside destructuring bindings* (distinct from the `_` match
     arm, which is done) — still deferred.

   **Suggested order:** `if let` / `let … else` documentation and examples
   first (they exercise the existing destructuring with the least new
   machinery), then OR patterns or struct update — whichever hurts more in
   practice (parser/service code tends to want OR patterns; SHA-style state
   updates tend to want `..base`).
12. Define numeric literal and cast rules: suffixes, inference/default integer
   type, signed/unsigned comparisons, narrowing, widening, checked/proved/
   wrapping overflow profiles, and diagnostics for ambiguous or lossy casts.
13. Define resource cleanup semantics: `defer`, drop/cleanup ordering,
    early-return cleanup, failure during cleanup, move-after-defer behavior, and
    linear-value interaction.
14. Define the FFI language surface: `extern` syntax, layout restrictions,
    ABI/calling convention annotations, ownership crossing the boundary,
    capability/trust requirements, and what cannot be expressed safely.
15. Define language-visible build profiles: debug/release, overflow checks,
    assertions, runtime checks, optimization assumptions, and proof/audit
    compatibility.
16. State the macro/metaprogramming stance for v1: no unrestricted macro
    system. Allow only controlled, audited compile-time generation /
    derive-like helpers for boring repeated artifacts such as equality,
    debug/display, serializers/parsers, proof stubs, contract boilerplate, and
    small table generation. Generated code must preserve source spans and
    evidence/audit traceability.
17. Define handle-relative filesystem APIs as the preferred capability shape:
    directory/file handles are capabilities; privileged code should operate
    relative to opened handles rather than repeated ambient path lookup. The
    design must address TOCTOU risks, path normalization, symlinks, temp files,
    and byte-preserving OS boundary behavior.
18. Add ignored-result diagnostics for fallible APIs: discarding `Result`,
    `Option`, or runtime-check results is a warning/error unless explicitly
    acknowledged with `_ = ...`, `ignore(...)`, or a policy-approved pattern.
19. Track accumulating error sets for `Result`-heavy code, without adopting row
    effects. Protocol parsers and service pipelines repeatedly want "this
    function may return exactly these error variants" without hand-writing one
    giant wrapper enum for every stage. First implementation should be a
    compiler report / audit fact over existing enum-returning functions; only
    later consider surface syntax if it stays obvious, e.g. a named error-set
    alias or a restricted union of enum variants. Do **not** introduce general
    row polymorphism or implicit effect rows.
20. Evaluate units-of-measure / dimensional annotations for common systems
    mistakes: bytes vs bits, milliseconds vs seconds, block counts vs byte
    offsets, protocol lengths, and memory sizes. Start as optional annotations
    and diagnostics over integer-like values, not full dependent types. Any
    proof story must be contract/VC-based and audit-visible; unit erasure must
    not hide conversions or allocation.
21. Add source style guidance alongside `concrete fmt`: idiomatic layout for
    functions, modules, contracts, matches, error handling, examples, and
    proof-bearing code.
22. Decide the v1 iteration protocol before broad stdlib work. Evaluate and
    document the replacement for closures/trait-object iterators:
    index-based `for i in 0..len { xs[i] }`, explicit cursor/iterator structs
    with `next() -> Option<T>`, and monomorphized `for_each`-style helpers. The
    decision must cover `Vec`, slices, maps, parser cursors, and interpreter
    workloads, and must explain how authority and allocation remain visible.
23. Decide capability polymorphism for higher-order stdlib functions before
    adding `map`/`fold`/`for_each` families or structured concurrency. The
    design must avoid a combinatorial split like `map`, `map_file`,
    `map_alloc`; the expected shape is explicit capability-set polymorphism
    such as `fn map<T, U, C>(xs, f: fn(T) with(C) -> U) with(C) -> ...`,
    grounded in `research/language/capability-polymorphism.md`. The decision
    must also specify proof scope: prove each monomorphized instance, prove the
    generic once, or allow generic contracts with instance-level proof
    artifacts. Audit output must distinguish `proved_for_instance` from any
    future `proved_generic` class.
24. Define stdlib v1 for daily programs: fixed arrays/slices, bytes/string
    basics, `Result`/`Option`, numeric helpers, and capability-scoped Console,
    File, Network, and Alloc APIs. Each stdlib item must declare its evidence
    class (`trusted`, `enforced`, `proved`, `reported`, or `assumed`).
25. Design user-facing testing framework UX before `std.test` hardens:
    test discovery (`#[test]` versus naming convention), expected failures,
    capability-scoped fixtures, temp files without ambient authority, oracle
    tests, interpreter-vs-compiled tests, proof-status interaction, and how test
    failures appear in `concrete audit`.
26. Add debug/trace mode: `concrete run --trace`, interpreter step traces, Core /
    lowered-IR dumps, source spans in runtime errors, and stable replay commands
    for report/debug failures.
27. Add interactive evidence commands for low-ceremony feedback without a live
    mutable REPL: evaluate a function with concrete inputs, inspect Core and
    ProofCore for one function, show the current generated obligation, and
    replay a failing proof/debug report. Target commands include
    `concrete eval`, `concrete inspect --core`, `concrete inspect --proofcore`,
    `concrete prove --show-obligation`, and `concrete run --trace`.
28. Add basic LSP/editor diagnostics early: parse/type errors, capability
    summaries, hover for inferred types, and jump-to-definition. Deeper
    proof/evidence LSP features remain in the later editor phase.
29. Decide target-conditional code selection before freestanding and
    cross-platform stdlib work harden. Prefer profile-selected source roots and
    modules in `Concrete.toml`; if narrow `cfg` attributes are added later, they
    must be LL(1)-safe, small, target/profile-only, and reported in audit.
30. Normalize the CLI around predictable verbs:
    `concrete build`, `concrete run`, `concrete test`, `concrete fmt`,
    `concrete audit`, `concrete prove`, `concrete eval`, `concrete inspect`,
    `concrete doc`, and `concrete clean`.
31. Add `concrete doc`: generate basic API/reference docs from source,
    capabilities, modules, and public comments without depending on proof
    infrastructure.
32. Add a first-user tutorial path for C/Rust developers that does not start
    with proofs: install, hello world, values and fixed arrays, ownership,
    borrows, capabilities, explicit errors, tests, compiled debugging, audit,
    then proof-bearing examples. The tone should be "ordinary systems code with
    visible evidence," not proof-assistant ceremony.
33. Add useful non-proof examples: a small CLI tool, a protocol decoder, a
    bounded cache, and a capability-scoped file/console program.
34. Add basic benchmarking UX: run small benchmarks, compare interpreter versus
    compiled performance, and detect obvious generated-code regressions.
35. Document the memory model for ordinary users: move/copy/drop behavior,
    cleanup, borrows, linear values, trusted/Unsafe escape hatches, definite
    assignment, and what is rejected. State the invariant explicitly: safe
    Concrete has no uninitialized reads by construction; trusted/FFI memory may
    carry explicit assumptions.
36. Add cross-platform build sanity for the supported host set: macOS and Linux
    first, with CI coverage, reproducible commands, and documented toolchain
    expectations.
37. Add the Phase 8 validation project: a small C/Rust-style CLI using the core
    slab plus daily workflow (`Concrete.toml`, modules/imports,
    `concrete test`, bytes/text/path, collections, diagnostics, formatting,
    docs, and trace/debug commands). CI must build, run, test, format-check,
    audit, and compare interpreter-vs-compiled behavior on macOS and Linux.

## Phase 9: Flagship Depth And Examples

Goal: produce examples that outside systems engineers find impressive, not only
internally coherent.

Done when: the showcase set includes a serious security/crypto or protocol
example with proof/evidence strong enough to anchor the public pitch.

HMAC and `constant_time_tag` are complete flagship baselines recorded in the
changelog. This phase maintains the graduated showcase, deepens theorem
coverage where it strengthens public claims, and adds new examples only when
they force a named surface or public claim.

1. Maintain the five graduated flagships and keep their evidence bundles green:
   `parse_validate`, `crypto_verify`, `fixed_capacity`, `constant_time_tag`,
   and `hmac_sha256`.
2. Add stretch theorem for `fixed_capacity`: multi-iteration ring invariant or
   stronger push/search property.
3. Add stretch theorem for `parse_validate`: success-path / failure-completeness
   theorem once proof ergonomics support it.
4. Audit the next stronger real-crypto candidate only if it forces a new public
   claim: Ed25519 verification subset, AEAD, or a post-quantum primitive.
5. Add only the ProofCore surface that candidate forces: shifts, bitand, u32
   compound loops, rotations, byte-to-word packing, and multi-round invariants.
6. Keep `hmac_sha256` as the regression anchor for exact-extraction,
   spec-drift-tied refinement: source perturbations must make the registered
   proof stale, and ProofKit refactors must keep the 11 proof checks green.
7. Keep the paper, website, README, and showcase manifest aligned with HMAC's
   actual claim: exact extracted source refines an independent SHA-256/HMAC
   spec under named assumptions and trusted backend boundaries.
8. Use HMAC-derived proof patterns only after they move into ProofKit or an
   explicit example guide; do not let future flagships copy private
   `Sha256Refine` scaffolding as hidden infrastructure.
9. Graduate one runtime-error-obligation flagship: parser/protocol example with
   no OOB/div-zero/overflow obligations discharged.
10. Graduate one authority/capability flagship: a privilege-separated tool whose
   trusted core cannot touch files/network/processes except through named
   wrappers.
11. Graduate one FFI-wrapper flagship: trusted C boundary, safe pure core,
    explicit assumptions, layout/ABI evidence.
12. Graduate one ownership-heavy resource flagship: explicit cleanup,
    borrow-heavy APIs, no leaks/double-use, and evidence explaining why.
13. Keep the curated showcase balanced: parser/protocol, bounded state,
    crypto/security, authority, FFI/trust, ownership-heavy.
14. Add a Unix-tool/protocol compatibility flagship that demonstrates bugs
    memory safety alone does not catch: byte-preserving I/O, path/OS-string
    handling, handle-relative filesystem authority, exit-code compatibility,
    error behavior compatibility, ignored-result diagnostics, and oracle tests
    against a reference implementation.
15. Add a graduated real-workload ladder. The goal is to make sure Concrete
    builds real things that can be checked against references, not only tiny
    proof demos. Each workload must name the surface or public claim it forces;
    otherwise it does not belong in this phase. Do not jump straight to multiple
    10k-line ports before the Phase 8 core slab and daily workflow can support
    them; that would mostly test missing ergonomics. Sequence:
    - **Main compiler repo:** keep tiny proof patterns
      (`examples/proof_patterns/`), evidence-class examples, small real programs
      that gate the compiler, and showcase flagships here. These protect
      compiler/proof correctness and should stay close to the tests.
    - **Medium in-repo real programs after the Phase 8 core slab:** 500-2,000 line
      examples, each chosen for a named pressure point: INI/TOML or tiny JSON
      parser (bytes/text/path, diagnostics, runtime obligations), bounded HTTP
      header parser (ignored-result diagnostics, byte-preserving parsing),
      small tar/zip reader (path/OS-string boundaries, archive offsets,
      overflow/cast obligations), bytecode interpreter (modules, dispatch,
      bounded loops), fixed-capacity LRU cache (collections, ownership, frame
      facts), or CLI checksum tool (project model, `concrete test`, oracle
      comparison). Tier exits only when at least two medium programs build, run,
      pass interpreter-vs-compiled checks, carry full evidence/trust
      classification, and are covered by runtime-obligation audit.
    - **Separate workload repo later** (`concrete-workloads`,
      `concrete-corpus`, or similar): use it for larger ports, 2k-10k line
      programs, compatibility suites, external-user programs, benchmark
      workloads, and reference/oracle data that would bloat the compiler repo.
      Do not create this repo until modules/imports, `Concrete.toml`,
      `concrete test`, bytes/text/path, collections, and basic diagnostics are
      usable. The first external-user workload in this repo is the
      external-validation-gate trial from the cross-cutting checkpoint. The repo
      must pin a Concrete compiler/toolchain version and be validated by release
      CI so it does not silently rot as the language changes.
    - **Ported compatibility examples after the daily workflow is stable:**
      `wc`/`cat`/`sha256sum`-style tools, QOI image decoder, base64 library,
      INI parser, tiny glob/regex matcher, arena allocator demo, MMIO-mock
      driver, or protocol codec. Annotate forcing surfaces: glob/regex must
      stay bounded or explicitly outside `PredictableV1`; arena allocator demo
      waits for allocation-profile work that needs it; MMIO-mock driver waits
      for the Phase 12 `with(Device)`/MMIO evidence decision or explicitly
      pulls that decision forward.
    - **10k-line stress ports** only after daily workflow is stable enough that
      the port tests Concrete rather than fighting missing basics. Tier exits
      only when at least one large port has a pinned reference/oracle suite,
      interpreter-vs-compiled differential coverage, runtime-obligation audit,
      and release-CI replay.
    Each workload must have a check story: oracle/reference comparison,
    interpreter-vs-compiled differential tests, runtime-obligation audit, and
    explicit evidence/trust classification for what is proved, tested, assumed,
    or trusted.
16. Do not run broad examples cleanup/polish sweeps. Clean examples
    opportunistically when a roadmap task touches them. Improve examples only
    when they serve proof-link migration, `concrete prove` authoring,
    external validation, or a release-facing tutorial.
17. Add the Phase 9 validation artifact: a showcase/workload dashboard that
    proves every flagship and graduated workload has a check story, evidence
    bundle, oracle or reference when appropriate, interpreter-vs-compiled
    coverage, runtime-obligation audit, trust/assumption classification, and
    release-CI replay. The first external-user workload in this dashboard is
    the external-validation-gate trial.

## Phase 10: Compiler Soundness Bridge

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
10. Automate dependency-ordered spec/table generation. Function specs and
    PExpr bodies should be collected/generated in dependency order together
    with FnTable completeness and call dependencies, so proof authors do not
    hand-relocate definitions above tables like `shaFns`.
11. Prove or mechanically validate spec/ghost totality reporting: every
    contract-referenced `spec fn` or ghost computation is either backed by
    Lean termination, accepted by a Concrete totality check, or rejected with a
    `totality_obligation_missing` status. A contract may not depend on a
    partial or non-terminating spec expression silently.
12. Define proof preservation across monomorphization and
    capability-polymorphic instantiation. The compiler must report whether a
    theorem proves a specific generated instance (`proved_for_instance`) or a
    generic body (`proved_generic`), and it must prevent one instance proof from
    being presented as proof for every future instantiation.
13. Add the Phase 10 validation artifact: a compiler-soundness dashboard with
    one witness program per shipped ProofCore construct, one status per
    R-rule, replay commands for proved/mechanically-validated facts, and
    regressions proving report facts (`proved`, `stale`, `blocked`, `missing`,
    `ineligible`, `trusted`) agree with compiler state.

## Phase 11: Backend, Target, And Stdlib Contracts

Goal: make backend/toolchain/stdlib assumptions explicit, and state exactly
where source-level proof stops.

Done when: SSA, target/toolchain, optimization, ABI/layout, stdlib evidence,
and incremental build contracts are explicit enough for release evidence.

1. Stabilize SSA as the only backend contract.
2. Document target/toolchain model: triple, data layout, linker, runtime/startup,
   libc expectation, clang/llc boundary, sanitizer/coverage hooks.
3. Define optimization policy: allowed optimizations, evidence preservation,
   debug/release behavior, report/codegen validation.
4. Add native compiled-program debugging support: DWARF/source-map emission,
   source-mapped backtraces for runtime failures, debug-vs-release behavior,
   optimized-code caveats, and diagnostics that distinguish source-level
   runtime checks from trusted backend/OS crashes.
5. Add clean-build versus incremental-build equivalence checks: facts,
   obligations, diagnostics, reports, and codegen must agree.
6. Add ABI/layout round-trip checks: C headers/stubs, offsets, size, alignment,
   calling conventions.
7. Add sanitizer-backed generated-code validation for trusted/FFI/layout/
   pointer-heavy examples.
8. Add backend/codegen differential validation where executable oracles exist.
9. Add compiler self-leak/resource soak harness for long-running workflows.
10. Define stdlib stability and evidence policy: which stdlib functions are
   trusted, proved, enforced, allocation-free, capability-free, or assumption
   carriers.
11. Define stdlib contracts for allocators, I/O handles, directory/file/path
    handles, byte/text/path conversion APIs, and fallible return discipline.
    Each public stdlib function must state allocation behavior, OS authority,
    failure mode, trusted platform assumptions, and evidence class.
12. Add stdlib evidence gates so core helpers cannot silently widen authority,
    allocation, proof assumptions, or runtime-error obligations.
13. Evaluate a normalized mid-level IR only when traceability/backend-contract
    reports expose a concrete gap.
14. Keep QBE/WASM/second backend deferred until evidence attachment,
    optimization policy, and backend trust boundaries are trustworthy.
15. Add the Phase 11 validation artifact: a backend/std-lib contract project
    with ABI/layout C round trips, sanitizer runs, compiled-oracle
    differential tests, native debug/source-map smoke tests, clean-vs-incremental
    fact equivalence, and stdlib authority/allocation/evidence gates.

## Phase 12: Freestanding And Embedded Target

Goal: make Concrete's hosted-vs-freestanding boundary explicit enough for
embedded, kernel, and audit-critical targets without destabilizing the hosted
language.

Prerequisite: the hosted stdlib/runtime boundary, allocator story, target model,
and daily workflow in Phase 8 and Phase 11 must be stable. Freestanding is not
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
4. Define hardware-access primitives and evidence classes before device-driver
   examples: `volatile`/MMIO operations require explicit `with(Mmio)` or
   `with(Device)` capability, inline assembly is `trusted` and requires
   `with(Unsafe)`, and interrupt handlers are an effectful/trusted boundary
   with explicit target assumptions.
5. Add explicit allocator/runtime hooks for freestanding builds, including
   ownership of allocation failure behavior and cleanup expectations.
6. Add linker/startup configuration: entry symbol, no-main mode, target triple,
   data layout, linker script hooks, and section/layout assumptions.
7. Add freestanding diagnostics: reject hosted APIs, hidden allocation, libc
   calls, unsupported target features, and unavailable capabilities.
8. Add one freestanding example: bounded parser, small checksum/hash kernel, or
   fixed-capacity state machine with no allocation and no hosted I/O.
9. Add one embedded-style audit bundle naming all remaining target assumptions:
   stack, interrupt model if any, allocator/runtime hooks, endian/layout, and
   backend/toolchain boundary.
10. Keep WASM, QBE, and additional backends deferred until freestanding target
    profiles prove the current LLVM path is not enough.
11. Add the Phase 12 validation artifact: one freestanding demo project plus an
    MMIO/device-profile mock audit bundle. The demo must build with no hosted
    APIs, name allocator/startup/linker assumptions, reject hidden libc or
    allocation, and report `with(Device)`/`with(Mmio)`/`with(Unsafe)` evidence
    classes without pretending hardware behavior is proved.

## Phase 13: Public Release Bar

Goal: make Concrete understandable and usable by someone who did not build the
compiler.

Done when: a fresh user can install Concrete, run a proof-bearing example,
inspect its audit bundle, and understand the claim matrix in under ten minutes.

**Language graduation is a gate, not a date.** Concrete can graduate individual
examples before the language graduates. The language reaches **alpha** only
after the source-contract/proof-link path is usable outside flagship hero work;
it reaches **beta/release** only after ordinary project workflow and external
validation are in place.

**Alpha bar (language can be presented as a usable experimental language):**
- At least one non-author writes, proves, or contract-annotates a useful
  Concrete program and reports that ProofKit + contracts + `concrete prove`
  were worth the discipline.
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
- In-source proof links are the only proof path; `proof-registry.json` support
  must remain absent.
- Release bundles have stable schemas, replay commands, assumption/trust
  reports, and proof-link provenance.
- The stdlib/runtime boundary is stable enough for daily examples.
- At least one external user completes the first-user tutorial and a useful
  audit/proof workflow without compiler-author intervention.

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
7. Add public examples policy: public-facing examples, website copy, README
   snippets, paper examples, and showcase manifests must not outclaim their
   proof status. Active candidates can be shown as active work, but cannot be
   presented as proved or graduated until their bars land.
8. Add public security/soundness disclosure policy: compiler/proof pipeline
   bugs are security-relevant.
9. Publish `THREAT_MODEL.md` and keep it linked from README, release bundles,
   showcase manifests, and assumptions docs.
10. Add first-user workflow CI: install compiler, create/run one example,
   inspect one audit bundle without repo-local assumptions.
11. Improve onboarding, tutorial, and docs around `proved` / `enforced` /
   `reported` / `assumed` / `trusted`.
12. Add positioning page against Rust, Zig, Lean, SPARK/Ada, Austral, Dafny,
   F*, Why3.
13. Add migration/adoption playbook: what C/Rust/Zig code moves first, how to
   wrap libraries honestly, what stays outside Concrete.
14. Add release/install distribution matrix: host triples, checksums/signing,
    install paths, supported/deferred channels.
15. Ship the first narrow public release only after the above are green.
16. Add the Phase 13 validation artifact: a release-candidate bundle installed
    from scratch on supported hosts, containing the claim matrix, threat model,
    first-user workflow, public examples policy, replay commands, schemas,
    assumptions/trust reports, checksums/signing metadata, and a tutorial run by
    someone who did not build the compiler.

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
13. Add the Phase 14 validation artifact: a multi-package workspace project
    with dependency resolution, lockfile, package-aware tests, interface/body
    artifact split, dependency trust policy, assumption inheritance, authority
    budgets, provenance, and release-bundle evidence for every dependency.

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
9. Add the Phase 15 validation artifact: a scripted editor/LSP session or
   golden transcript over one real project, proving hover, diagnostics,
   obligation navigation, proof/evidence facts, dependency audit UI, and
   refactor behavior match CLI facts rather than inventing a second truth
   source.

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
13. Do not adopt row effects for v1. The default design stays object-capability
    and audit-visible: authority should be obvious in source, not hidden behind
    abstract effect inference. Revisit only as a research note if explicit
    capabilities create a proven, repeated blocker in real programs after the
    stdlib and concurrency pressure tests exist.
14. Add the Phase 16 validation artifact: one pressure-test sketch, expected
    report, and decision record for every research-gated extension
    (concurrency, typestate, arena allocation, WCET, binary-format DSLs,
    hardware capability mapping, Miri-style interpreter, sized evaluator,
    persistent rewrite state, and row effects). No research item graduates
    unless its forcing example, report shape, evidence class, and rejection or
    pull-forward criteria are recorded.
