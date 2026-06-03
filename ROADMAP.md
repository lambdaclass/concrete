# Concrete Roadmap

This document is the active execution plan. It answers one question:
**what should happen next, in what order?**

The roadmap is linear after the explicit active HMAC exception below. Read the
order as:

1. finish the active HMAC proof track;
2. resume Phase 1 through Phase 4 to make source-level evidence usable;
3. close Phase 5 through Phase 7 hardening gaps when they block those phases or
   before release;
4. continue Phase 8 onward.

Completed work moves to [CHANGELOG.md](CHANGELOG.md). Deferred or conditional
work moves later. There are no `NEXT` tags.

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

## Phase 0: Finish HMAC Refinement — DONE (2026-06-02)

`hmac_sha256` graduated as the fifth flagship with bar #2 closed the rigorous
way: the entire composition chain is kernel-verified AND tied to the extracted
source through the spec-drift gate. `hmac_sha256_refines_spec` proves the
extracted body computes exactly `Sha256Spec.hmac` for all inputs in the
documented bounds; the nine chain refinements (`block_to_words(_at)`,
`sha256_schedule`, `sha256_round`, `sha256_compress(_at)`, `state_to_bytes`,
`sha256_hash`, `hmac_sha256`) plus the two bar-#1 point proofs are all
registered with the spec equal to the source-extracted PExpr
(`Concrete.Proof.specs`), so a source edit turns the proof stale. The thesis
realized in full: *source extracts to this ProofCore body, this body refines
the spec, and drift breaks the claim.* `--report check-proofs` = 11 verified,
0 failed; spec-drift gate = 0 drift, 0 stale.

Next: continue the post-HMAC build-out. The first source-contract slices have
landed (attributes, `spec fn`, `requires`/`ensures`, call-site obligations,
`bv_decide` discharge, loop contracts, and one kernel-checked loop VC), and
ProofKit has been harvested into reusable modules. The remaining Phase 1-4
work is to make those pieces less hand-linked: generated VCs, ghost state,
proof stubs, audit UX, flagship retrofit, and release-grade review artifacts.
Phase 5 through Phase 7 remain hardening gates; pull their items forward when
they block the Phase 1-4 work or before release.

---

## Cross-cutting decisions and checkpoints (recorded 2026-06-03)

These are decisions/checkpoints, not yet implementation. Each names the
evidence class so no construct stays semantically dark.

### Floats: usable now, provable only under an explicit profile

**Decision.** `f32`/`f64` are allowed as runtime types but are **outside
ProvableV1**. No proof or release claim may be made over float arithmetic until
a float profile exists.

**Soundness gap — CLOSED (2026-06-03).** Previously a float-arithmetic function
with no float *literal* — e.g. `fn fadd(x: f64, y: f64) -> f64 { x + y }` —
extracted to `(x + y)` and reported `status: extracted`, `eligible`: the float
`+` was silently modeled as the *integer* `.add`. That was a soundness/honesty
bug (it violated "no semantically dark constructs"). The first slice of the
float arc now closes it: `assessEligibility` flags any float-typed param,
return, or body op (detected from the still-present Core type via `isFloatTy`)
as a profile miss, so the function is **excluded** from eligibility and never
extracted. `identifyUnsupportedExpr` likewise reports float binops/idents/casts/
literals as `unsupported_for_proof`. Audit-loud, the dedicated framing reads:
`float semantics: unprofiled / proof eligibility: excluded /
reason: floating-point arithmetic has no active proof profile`. Regression
fixture: `examples/float_unprofiled/` with `eligibility` + `extraction`
snapshots (`0 extracted`). `ProvableFloatV1` below remains the future opt-in.

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
  on **Phase 8 items 4 (module/import stability), 7 (bytes/text/path), 9
  (collections), 17 (iteration protocol), and 18 (capability polymorphism)**
  and MUST NOT have their contract syntax or VC shape frozen until those items
  land. Treat any such construct as "will be reworked," and do not let a
  flagship bake an iterator/collection assumption into the VC shape.

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

1. **Define the minimum slab** an external user needs to write and prove one
   useful program — likely Phase 8 items 4 (module stability), 7 (bytes/text),
   9 (collections), plus a package/build path.
2. **Build exactly that** — not the full back half.
3. **Run the trial and treat the result as an explicit go / no-go on the rest
   of Phases 8-15.**

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
refinement (`block_to_words_refines_spec`, Phase 0) have all **shipped** — so
this phase now designs against obligation shapes that have actually been
discharged, including a real `eval_while_count` loop obligation, rather than
imagined ones. See the build order in `docs/PROOF_LADDER.md`.

Done when: one flagship uses source contracts as the primary proof surface, and
each contract is classified as proved, enforced, assumed, missing, blocked, or
solver-assisted.

**Status (2026-06-03).** The contract surface is no longer aspirational:
`spec fn`, `#[ensures]`, `#[requires]`, call-site obligations, `bv_decide`
classification, `#[invariant]`/`#[variant]`, and one kernel-checked
`invariant_preservation` obligation have shipped. The remaining work in this
phase is to replace hand-linked examples with generated obligations, add
`ghost`/`assert`/`assume`, and retrofit a flagship so source contracts become
the primary proof surface rather than a reported overlay.

1. Add LL(1)-safe function attributes:
   `#[requires(...)]` and `#[ensures(...)]`.
2. Restrict v1 contract expressions to proof-friendly expressions:
   parameters, `result`, literals, comparisons, boolean operators, simple
   arithmetic, fixed array lengths, and named pure predicates where explicitly
   supported.
3. Keep the contract surface deliberately boring: attributes, ghost values,
   `assert`, and `assume` only. No tactics in `.con`, no embedded theorem
   language, no dependent-programming sublanguage, and no clever contract DSL
   that makes ordinary code feel abstract before it is auditable.
4. Add a contract design fixture corpus before implementation:
   straight-line `ch`, bounds-only `get16`, loop-shaped `ct_compare`, and an
   HMAC-style `ensures result == spec(...)` example. These fixtures define the
   expected parse, Core, VC, and audit shapes before the parser/compiler work
   hardens them.
5. Store source contracts through Parse/Resolve/Check/Core and report them with
   source spans.
6. Generate verification-condition seeds for each source contract: caller-side
   preconditions, callee-side postconditions, and assumptions needed by the
   expression language.
7. Add `--report contracts` with evidence statuses and links to generated
   obligations and VC ids.
8. Connect contracts to the proof registry: a theorem can discharge a specific
   source contract id.
9. Map existing proof-registry entries to generated source-contract obligations
   where possible, so current Lean theorems migrate forward instead of being
   discarded when contracts become the primary proof surface. (The registry's
   eventual retirement is a Phase 3 proof-authoring item, gated on
   `concrete prove`.)
10. Add contract negative examples: unmet precondition at call site, missing
   postcondition proof, weakened postcondition, invalid contract expression.
11. Add proof-only source forms:
   `ghost let`, `assert`, and `assume`. `ghost` erases before codegen;
   `assert` creates an obligation; `assume` is a tainted, audit-loud
   assumption, never a proof.
12. Add loop attributes:
   `#[invariant(...)]` and `#[variant(...)]`.
13. Generate loop-invariant obligations: initialization, preservation, variant
   decrease, and exit-implies-postcondition.
14. Add source contract soundness work to the compiler soundness bridge: parsing
   preserves meaning, generated obligations correspond to contract semantics,
   discharged obligations imply the advertised contract claim.
15. Add contract diagnostics that explain whether the failure is caller-side
    precondition, callee-side postcondition, loop invariant initialization,
    invariant preservation, or variant decrease.
16. Add contract stability rules: weakening a precondition, strengthening a
    postcondition, or changing a public invariant is a semantic API change.
17. Add one contract-bearing flagship retrofit after the machinery is real:
    `constant_time_tag` first, then `hmac_sha256` once its refinement theorem
    has graduated.

## Phase 2: Verification Conditions And SMT Assistance

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
3. Generate VCs for runtime safety obligations from Phase 7: array bounds,
   div/mod nonzero, checked/proved overflow, casts, and loop bounds.
4. Generate VCs for loop invariants: initialization, preservation,
   variant decrease, and exit-implies-postcondition.
5. **Kernel-checked automation first (`bv_decide`).** Before any external
   solver, route BitVec / bounded-arithmetic VCs to Lean's `bv_decide`
   (in-toolchain; bit-blasts to SAT and replays a kernel-checked certificate —
   **no TCB growth**). Already validated against the HMAC helper facts. Its
   results are classified `proved_by_kernel_decision`, a kernel-checked class
   distinct from `proved_by_smt`. See [docs/PROOF_LADDER.md](docs/PROOF_LADDER.md).
6. Centralize arithmetic bridge lemmas before adding more crypto/protocol VCs:
   `Int` / `Nat` / `BitVec` round trips, division/modulo, shift index
   arithmetic, byte/word packing, and symbolic bridge cases like the
   `sdiv`/`Nat.div` proof from HMAC padding. These are proof-library
   primitives, not one-off flagship lemmas.
7. Add an *external* SMT backend behind an explicit flag or policy gate, reached
   for only when `bv_decide` cannot (e.g. nonlinear). Start with one solver
   adapter and a stable SMT-LIB output path before adding more solvers. Its
   results are `solver_trusted` (solver enters the TCB) unless a certificate is
   replayed — never collapsed into a kernel-checked class.
8. Classify solver results in reports and artifacts:
   `proved_by_kernel_decision` (kernel-checked), `proved_by_smt` /
   `solver_trusted` (external), `unknown`, `counterexample`, `timeout`,
   `solver_error`.
9. Surface counterexamples in source terms where possible: function inputs,
   loop variables, failing index, failing arithmetic side condition, and the
   contract/obligation that failed.
10. Add CI gates for solver determinism and replay: same VC, same solver
   configuration, same result class, with timeouts treated as non-proofs.
11. Add Lean replay for the simplest SMT-discharged fragments where practical:
   propositional/linear integer facts, bounds arithmetic, and trivial BitVec
   identities. Results without replay remain explicitly solver-trusted.
12. Add policy controls: projects can require `proved_by_lean`, allow
    `proved_by_smt`, or permit `solver_trusted` only under named assumptions.
13. Add SMT negative examples: false postcondition, missing invariant,
    overflow counterexample, OOB counterexample, div-zero counterexample,
    solver timeout, and unsupported theory.
14. Retrofit one flagship with source contracts whose easy VCs discharge
    automatically, while the meaningful theorem remains Lean-checked.
15. Update audit/release bundles so VC results appear beside proof registry,
    assumptions, runtime obligations, and proof coverage classification.
16. Add soundness documentation for the SMT path: trusted solver binary,
    encoding assumptions, unsupported theories, replayed fragments, and how a
    solver bug affects each claim class.

## Phase 3: Proof Authoring And Automation

Goal: make flagship proofs a repeatable engineering workflow, not a collection
of one-off `simp` scripts.

Done when: new flagship proofs can start from useful generated stubs, standard
lemmas, and actionable failure diagnostics.

**Status (2026-06-03).** ProofKit v1.1 has shipped as reusable, `fns`-generic
modules (`Eval`, `BitVec`, `Array`, `Loops`, `Calls`, `Refinement`) with an
umbrella import and a proof guide. The remaining work is proof authoring UX:
generated stubs, generated composition scaffolds, failed-obligation debugging,
and `concrete prove <function>`.

1. [DONE] Package the reusable HMAC proof infrastructure as **Concrete Proof Kit v1**:
   spec/refinement pattern, loop proof kit, array/state proof kit, BitVec
   automation kit, function-table/call kit, generated proof stubs,
   failed-obligation/debug UX, and tutorial proof shapes.
2. [DONE] After HMAC is tied to the exact extracted source and graduated, split generic
   proof infrastructure out of `Concrete/Sha256Refine.lean` into dedicated
   modules:
   `Concrete/ProofKit/Eval.lean` for fuel and `while_` lemmas,
   `Concrete/ProofKit/Array.lean` for lookup/set/length/frame lemmas,
   `Concrete/ProofKit/BitVec.lean` for `Int`/`Nat`/`BitVec` bridges, packing,
   and `sdiv` patterns,
   `Concrete/ProofKit/Loops.lean` for copy/xor/multi-store loop templates,
   `Concrete/ProofKit/Calls.lean` for FnTable and call-wrapper helpers, and
   `Concrete/ProofKit/Refinement.lean` for generic `PExpr`-refines-spec
   theorem patterns. `Concrete/Sha256Refine.lean` should then import the kit
   and contain only SHA/HMAC-specific proofs and buffer/spec shapes.
3. [DONE] Make the spec/refinement pattern public: independent Lean spec module,
   extracted `PExpr` refinement theorem shape, registry/fingerprint wiring,
   and examples such as `ch_refines`, `block_to_words_refines_spec`,
   `sha256_compress_refines_spec`, and `sha256_hash_refines_spec`.
4. [DONE] Expose the loop proof kit:
   `eval_fuel_succ`, `eval_fuel_le`, `eval_while_false`,
   `eval_while_true`, `eval_while_count`, and templates for counter-loop
   invariants.
5. [DONE] Expose the array/state proof kit:
   `lookupIndex_set_self`, `lookupIndex_set_ne`, `length_set`,
   `set_in_counter_map`, the multi-store frame pattern from `state_to_bytes`,
   and the buffer-update model from the padding proof.
6. [DONE] Expose the BitVec automation kit:
   `bv_decide` examples, `Int`/`Nat`/`BitVec` bridge lemmas, u8/u32 packing
   helpers, wrapping add/shift/rotate helper facts, and symbolic index bridge
   examples.
7. [DONE] Expose the function-table/call kit:
   FnTable completeness helpers, call-wrapper refinement pattern, callee
   refinement dependencies, and dependency-ordered table-entry discipline for
   composed functions.
8. Build reusable proof lemmas for arrays: lookup, update, length, in-bounds,
   OOB stuck behavior.
9. Build reusable lemmas for loop-carried state and `while_step`.
10. Build reusable lemmas for BitVec operations used by flagships.
11. Build reusable lemmas for structs, fields, enum construction, match, Result,
   Option, and bounded-buffer invariants.
12. Upgrade generated proof stubs for real shapes: arrays, structs, enums,
    fixed buffers, Result/Option, loops, source contracts, and refinement
    composition. Stubs should emit spec target, `PExpr` body, FnTable skeleton,
    expected theorem statement, common imports/tactics, and TODO blocks for
    loop invariants.
13. Add generated composition scaffolds: FnTable entries, call lemmas, callee
    refinement dependencies, and composed theorem skeletons.
14. Add generated loop-invariant templates for common proof shapes:
    counter loop over array writes, copy loop, fold loop, multi-store loop,
    offset loop, and block-processing loop.
15. Add `concrete prove <function>`: generate the proof stub, list the source
   contracts and VCs, show the registry/spec target, and print the current
   failing obligation with replay commands.
16. Add proof minimization/debugging UX: show the smallest extracted expression
    or lemma surface related to a failed proof, including messages like
    "failed to prove index expression equals spec offset under len <= 375" for
    symbolic arithmetic glue.
17. Add proof replay/caching once proof artifacts and fingerprints are stable.
18. Add simple auto-discharge for structural obligations that do not need human
    proof search.
19. Add a small verified/spec-checked standard proof library for common
    predicates: sorted, bounded, no-duplicates, fixed-length, prefix, checksum,
    constant-time source shape.
20. Add AI-assisted proof repair only after artifacts, statuses, and replay are
    stable enough to validate suggestions mechanically.
21. **Frame inference (the proof-scaling cliff).** Every loop/state proof must
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
22. **Retire `proof-registry.json` (transitional, not wrong).** The JSON
   registry was the right first mechanism; the trajectory is to dissolve it as
   contracts become the primary proof surface, tied to `concrete prove`
   (item 15) which teaches the replacement link shape.
   - **Today:** `proof-registry.json` is the bridge from source functions to
     their Lean specs/proofs (`proof`, `spec`, `coverage`, `ensures_proof`,
     `body_fingerprint`).
   - **Near term:** auto-discharge removes many entries because obligations
     close from the in-source contract alone — O1/O3/O4/O5 and O2's arithmetic
     half already need no registry entry (omega / `bv_decide`).
   - **Next:** `concrete prove <function>` teaches the remaining link shape (the
     residual hand-written-Lean proofs: HMAC chain, point proofs, O2 operational
     half).
   - **Later:** in-source proof attributes — e.g. `#[proof_by(thm)]` /
     `#[spec(name)]` — replace the JSON theorem/spec links, versioned with the
     code and surfaced in the same audit output as the contract.
   - **End state:** `body_fingerprint` is computed from extraction at build
     time (re-extraction already runs), not hand-stored in JSON and rot-prone.
   - **Rule:** do **not** migrate the registry to a new format before
     `concrete prove` exists — let the tool teach the syntax (same discipline as
     the contract-VC stability tiers).

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
10. Add evidence-level monotonicity checks to audit/diff output.
11. Add one AI-audit demo where an agent answers authority/proof/trust
    questions using compiler facts rather than source guesses.
12. Add review checklists generated from facts: what changed, what widened,
    what became trusted, what lost proof, what gained assumptions, and which
    obligations remain open.
13. Add artifact redaction/stability rules so release bundles can be shared
    publicly without leaking local paths, secrets, or machine-specific noise.
14. Keep audit, contracts, obligations, assumptions, policies, manifests, and
    proof-status output on one shared vocabulary. Do not let each artifact grow
    its own mini-language for the same evidence classes.

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
7. Add evidence monotonicity checks: a refactor cannot silently present a weaker
   claim as if it were still stronger (`proved` cannot degrade to `reported`
   while retaining the same badge/summary).
8. Add assumption lifecycle checks: every assumption has an owner, scope,
   rationale, review date, affected claims, and a diff gate when it widens.
9. Add a trust-boundary inventory report: all `trusted`, `Unsafe`, extern,
   backend, runtime, and target assumptions in one machine-readable list.

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
9. Define the v1 threat model: adversary, trusted base, proof scope, backend
   scope, side-channel scope, dependency scope, and what remains out of model.
10. Add negative examples for every `ProvableV1` and `PredictableV1` exclusion.
11. Update `CLAIMS_TODAY.md`, README, showcase docs, and release bundles to use
    the frozen subset names consistently.
12. Close the unprofiled-float proof hole before any float proof claim:
    float-typed params/returns/locals/literals/ops are excluded from ProofCore
    extraction unless an explicit float profile is active. Audit output must
    say `float semantics: unprofiled` and `proof eligibility: excluded` rather
    than reporting a float operation as extracted through integer `PBinOp.add`.
13. Define `ProvableFloatV1` as a separate, narrow proof profile:
    IEEE-754 binary32/binary64, round-to-nearest-even, no fast-math, no
    reassociation, no implicit FMA contraction, no ambient rounding-mode
    mutation, and explicit NaN/infinity/subnormal/signed-zero policy.
14. Add ProofCore support for profiled floats only after item 12 is closed:
    `PVal.float32/64`, float `PBinOp` cases carrying width and rounding
    (`fadd`/`fsub`/`fmul`/`fdiv`/`feq`/`flt`/`fle`), interpreter agreement,
    and backend/audit checks that prove/report `fast_math: forbidden`.
15. Classify the first float semantics layer honestly. Until Concrete imports
    or proves a checked IEEE-754 semantics library, primitive float operations
    are `float_semantics_trusted`; proofs over profiled float code are
    refinements to an explicit bit-level IEEE spec under that named trusted
    primitive layer, not `proved_by_lean` from first principles.
16. Add one small `ProvableFloatV1` flagship only after the profile exists:
    a fixed-order `f32`/`f64` kernel such as clamp/normalize, tiny FIR/IIR, PID,
    or dot product. Prove exact IEEE behavior first; real-valued epsilon-bound
    refinement is a later layer.

## Phase 7: Runtime Safety Obligations

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
5. Define the user-level error model: `Result`, `Option`, assertion failure,
   abort/panic, recoverable errors, test failures, and how error flow interacts
   with capabilities, proofs, runtime obligations, and audit output.
6. Generate narrowing/invalid-cast obligations.
7. Generate loop bound and variant obligations for bounded loops.
8. Generate obligations for panic/abort/assert-as-denial-of-service risks:
   unchecked indexing, unwrap-like operations, explicit abort paths, failed
   assertions, and profile-dependent panic behavior.
9. Generate byte/text/path boundary obligations: invalid UTF-8, lossy
   conversion, OS-string conversion, path normalization assumptions, and
   rejected implicit conversions.
10. Generate stack/recursion obligations where the profile claims boundedness.
11. Report runtime-error obligations in human and JSON forms.
12. Add policy gates that can require selected runtime-error obligations to be
   proved/enforced before graduation.
13. Add a runtime-error regression corpus: OOB, div/mod zero, overflow-profile
    violation, invalid cast, loop-bound violation, lossy byte/text conversion,
    ignored fallible result, unwrap-like failure, and panic/abort profile
    mismatch.
14. Add a runtime-error-obligation flagship requirement: one graduated example
    must demonstrate no OOB/div-zero/overflow under a named profile.
15. Add high-quality diagnostics for obligation failures: violated obligation,
    source expression, required evidence, current status, and next action.
16. Add obligation suppression only through explicit assumptions or policy
    waivers, never comments or hidden allowlists.
17. Prove or validate obligation-generation soundness for the first obligation
    kinds through the compiler soundness bridge.

## Phase 8: Language Usability And Daily Workflow

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
5. Add `docs/GRAMMAR.md`: LL(1) grammar, reserved keywords, attribute syntax,
   contract syntax, `ghost`/`assert`/`assume`, iteration syntax, and negative
   parser fixtures. This is a syntax reference, not a language-design
   committee.
6. Close the match/pattern ergonomics gap before broad `Result`/`Option` and
   protocol-decoder work. This is one compound usability block: algebraic data
   types are already in the language, so the pattern language must be
   expressive enough to use them without stacks of boilerplate matches.

   **Done (first chunk):** `_` wildcard *match arms*
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
7. Define strings, bytes, paths, and OS strings: `Bytes` for raw data, `Text`
   for validated UTF-8, and `Path`/`OsString` for OS-native boundaries. Specify
   literals, ownership, slicing, indexing, formatting, conversions, parser/JSON
   interaction, diagnostics, and test output. No implicit lossy conversion.
8. Define numeric literal and cast rules: suffixes, inference/default integer
   type, signed/unsigned comparisons, narrowing, widening, checked/proved/
   wrapping overflow profiles, and diagnostics for ambiguous or lossy casts.
9. Define the collections story: fixed arrays, slices, dynamic `Vec`, maps,
   buffers, parser cursors, and which collections require `Alloc` or other
   capabilities.
10. Define resource cleanup semantics: `defer`, drop/cleanup ordering,
    early-return cleanup, failure during cleanup, move-after-defer behavior, and
    linear-value interaction.
11. Define the FFI language surface: `extern` syntax, layout restrictions,
    ABI/calling convention annotations, ownership crossing the boundary,
    capability/trust requirements, and what cannot be expressed safely.
12. Define language-visible build profiles: debug/release, overflow checks,
    assertions, runtime checks, optimization assumptions, and proof/audit
    compatibility.
13. State the macro/metaprogramming stance for v1: no unrestricted macro
    system. Allow only controlled, audited compile-time generation /
    derive-like helpers for boring repeated artifacts such as equality,
    debug/display, serializers/parsers, proof stubs, contract boilerplate, and
    small table generation. Generated code must preserve source spans and
    evidence/audit traceability.
14. Define handle-relative filesystem APIs as the preferred capability shape:
    directory/file handles are capabilities; privileged code should operate
    relative to opened handles rather than repeated ambient path lookup. The
    design must address TOCTOU risks, path normalization, symlinks, temp files,
    and byte-preserving OS boundary behavior.
15. Add ignored-result diagnostics for fallible APIs: discarding `Result`,
    `Option`, or runtime-check results is a warning/error unless explicitly
    acknowledged with `_ = ...`, `ignore(...)`, or a policy-approved pattern.
16. Add source style guidance alongside `concrete fmt`: idiomatic layout for
    functions, modules, contracts, matches, error handling, examples, and
    proof-bearing code.
17. Decide the v1 iteration protocol before broad stdlib work. Evaluate and
    document the replacement for closures/trait-object iterators:
    index-based `for i in 0..len { xs[i] }`, explicit cursor/iterator structs
    with `next() -> Option<T>`, and monomorphized `for_each`-style helpers. The
    decision must cover `Vec`, slices, maps, parser cursors, and interpreter
    workloads, and must explain how authority and allocation remain visible.
18. Decide capability polymorphism for higher-order stdlib functions before
    adding `map`/`fold`/`for_each` families or structured concurrency. The
    design must avoid a combinatorial split like `map`, `map_file`,
    `map_alloc`; the expected shape is explicit capability-set polymorphism
    such as `fn map<T, U, C>(xs, f: fn(T) with(C) -> U) with(C) -> ...`,
    grounded in `research/language/capability-polymorphism.md`.
19. Define stdlib v1 for daily programs: fixed arrays/slices, bytes/string
    basics, `Result`/`Option`, numeric helpers, and capability-scoped Console,
    File, Network, and Alloc APIs. Each stdlib item must declare its evidence
    class (`trusted`, `enforced`, `proved`, `reported`, or `assumed`).
20. Design user-facing testing framework UX before `std.test` hardens:
    test discovery (`#[test]` versus naming convention), expected failures,
    capability-scoped fixtures, temp files without ambient authority, oracle
    tests, interpreter-vs-compiled tests, proof-status interaction, and how test
    failures appear in `concrete audit`.
21. Add `concrete test`: discover and run user tests, example tests,
    expected-failure tests, interpreter-vs-compiled differential tests,
    snapshot tests, oracle tests, and policy/assumption gates through one
    command.
22. Add debug/trace mode: `concrete run --trace`, interpreter step traces, Core /
    lowered-IR dumps, source spans in runtime errors, and stable replay commands
    for report/debug failures.
23. Add interactive evidence commands for low-ceremony feedback without a live
    mutable REPL: evaluate a function with concrete inputs, inspect Core and
    ProofCore for one function, show the current generated obligation, and
    replay a failing proof/debug report. Target commands include
    `concrete eval`, `concrete inspect --core`, `concrete inspect --proofcore`,
    `concrete prove --show-obligation`, and `concrete run --trace`.
24. Add a minimal project model before full packages: `Concrete.toml` fields for
    name, entry points, tests, policies, assumptions, source roots, and build
    profiles.
25. Normalize the CLI around predictable verbs:
    `concrete build`, `concrete run`, `concrete test`, `concrete fmt`,
    `concrete audit`, `concrete prove`, `concrete eval`, `concrete inspect`,
    `concrete doc`, and `concrete clean`.
26. Add `concrete doc`: generate basic API/reference docs from source,
    capabilities, modules, and public comments without depending on proof
    infrastructure.
27. Add a first-user tutorial path that does not start with proofs: install,
    hello world, ownership, capabilities, arrays, modules, tests, audit, then
    proof-bearing examples.
28. Add useful non-proof examples: a small CLI tool, a protocol decoder, a
    bounded cache, and a capability-scoped file/console program.
29. Add basic benchmarking UX: run small benchmarks, compare interpreter versus
    compiled performance, and detect obvious generated-code regressions.
30. Document the memory model for ordinary users: move/copy/drop behavior,
    cleanup, borrows, linear values, trusted/Unsafe escape hatches, and what is
    rejected.
31. Add cross-platform build sanity for the supported host set: macOS and Linux
    first, with CI coverage, reproducible commands, and documented toolchain
    expectations.

## Phase 9: Flagship Depth And Examples

Goal: produce examples that outside systems engineers find impressive, not only
internally coherent.

Done when: the showcase set includes a serious security/crypto or protocol
example with proof/evidence strong enough to anchor the public pitch.

HMAC is complete and tracked in Phase 0 as the forcing example for
source-contract and ProofKit obligation shape. This phase now maintains the
graduated showcase, deepens theorem coverage where it strengthens public
claims, and adds new examples only when they force a named surface or public
claim.

1. Maintain the five graduated flagships and keep their evidence bundles green:
   `parse_validate`, `crypto_verify`, `fixed_capacity`, `constant_time_tag`,
   and `hmac_sha256`.
2. Add stretch theorem for `constant_time_tag`: full iff if tractable, or a
   clearly named stronger negative-direction theorem.
3. Add stretch theorem for `fixed_capacity`: multi-iteration ring invariant or
   stronger push/search property.
4. Add stretch theorem for `parse_validate`: success-path / failure-completeness
   theorem once proof ergonomics support it.
5. Audit the next stronger real-crypto candidate only if it forces a new public
   claim: Ed25519 verification subset, AEAD, or a post-quantum primitive.
6. Add only the ProofCore surface that candidate forces: shifts, bitand, u32
   compound loops, rotations, byte-to-word packing, and multi-round invariants.
7. Keep `hmac_sha256` as the regression anchor for exact-extraction,
   spec-drift-tied refinement: source perturbations must make the registered
   proof stale, and ProofKit refactors must keep the 11 proof checks green.
8. Keep the paper, website, README, and showcase manifest aligned with HMAC's
   actual claim: exact extracted source refines an independent SHA-256/HMAC
   spec under named assumptions and trusted backend boundaries.
9. Use HMAC-derived proof patterns only after they move into ProofKit or an
   explicit example guide; do not let future flagships copy private
   `Sha256Refine` scaffolding as hidden infrastructure.
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
15. Add a Unix-tool/protocol compatibility flagship that demonstrates bugs
    memory safety alone does not catch: byte-preserving I/O, path/OS-string
    handling, handle-relative filesystem authority, exit-code compatibility,
    error behavior compatibility, ignored-result diagnostics, and oracle tests
    against a reference implementation.

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
10. Define stdlib contracts for allocators, I/O handles, directory/file/path
    handles, byte/text/path conversion APIs, and fallible return discipline.
    Each public stdlib function must state allocation behavior, OS authority,
    failure mode, trusted platform assumptions, and evidence class.
11. Add stdlib evidence gates so core helpers cannot silently widen authority,
    allocation, proof assumptions, or runtime-error obligations.
12. Evaluate a normalized mid-level IR only when traceability/backend-contract
    reports expose a concrete gap.
13. Keep QBE/WASM/second backend deferred until evidence attachment,
    optimization policy, and backend trust boundaries are trustworthy.

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
13. Do not adopt row effects for v1. The default design stays object-capability
    and audit-visible: authority should be obvious in source, not hidden behind
    abstract effect inference. Revisit only as a research note if explicit
    capabilities create a proven, repeated blocker in real programs after the
    stdlib and concurrency pressure tests exist.
