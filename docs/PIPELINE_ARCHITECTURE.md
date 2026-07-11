# Pipeline Architecture: One Meaning, Committed Once

This is the north-star note for the compiler pipeline. It does not add work;
it names the single principle that the Phase 6.5 refactor items are all
instances of, so they read as one architectural bet instead of two dozen
unrelated cleanups.

**Thesis: each program has exactly one meaning, that meaning is committed once
by the stage that owns it, and every later stage may only *read* it — never
*re-derive* it.**

## The disease

Every front-end bug in the 2026-07 sweep was the same shape: two stages holding
different opinions about the same program. Check typed a literal from its hint
while Elab typed it from the sibling operand. `typesCompatible` was lenient
where SSA-verify was strict. The interpreter, the constant-folder, and the
backend each re-encoded integer overflow/trap semantics by hand and were kept in
agreement by a fuzzer. The duplication survey found the same fingerprint
everywhere: types implemented three times, arithmetic three times, tree
traversal fifteen times, type substitution four times, capability rendering
twice, the diagnostic-code catalog twice.

These are not independent defects. They are one structural fact: **the IR is
syntax that carries no committed meaning, so every pass re-interprets it, and
two of those re-interpretations (the interpreter and the backend) are parallel
implementations of the language semantics kept in sync by luck.**

The interpreter-as-oracle premise that the entire differential-testing and
proof-soundness story rests on is, today, guaranteed by `fuzz_differential.py`
rather than by construction. That is the weakness worth an architectural fix:
the thing Concrete trusts most is the thing least structurally guaranteed.

## The two principles

### Principle 1 — One reference semantics

The language's operational meaning (integer arithmetic, casts, evaluation rules)
is defined in exactly one place. The **interpreter is that definition** — the
oracle by construction, not by coincidence. Every other consumer is derived from
or checked against it:

- the constant-folder proves each fold preserves the reference semantics;
- the backend derives checked-helper selection from the same facts;
- differential testing stops being a bug hunt and becomes a stated relation:
  `interpret == fold-then-interpret == compiled`.

In a Lean codebase this relation is uniquely available: it can be *proved* over
the reference module, not merely sampled. Divergence between interp, optimizer,
and codegen becomes a theorem to state, not a bug to catch.

Phase 6.5 item #1 (`Concrete/Semantics/IntArith.lean`) is the beachhead of this
principle on the arithmetic axis.

## Judgment modules

The reusable shape is a **judgment module**: one pure compiler module that owns a
semantic decision and returns the whole decision record. It is not a helper that
answers only "yes/no" or "what type?" while other passes re-derive the rest.

Current and planned judgment modules:

- `IntArith` owns integer width, signedness, ranges, checked/wrapping/
  saturating behavior, trap predicates, and foldability.
- `TypeJudgment` owns source-expression type decisions that Check and Elab both
  need; Elab stamps the resulting type into already-typed Core.
- `CapabilityJudgment` should own required capabilities, callback propagation,
  purity, trusted/Unsafe/package-boundary reasons, and the report/audit payload
  for those decisions.
- `CopyJudgment` / `InstantiationJudgment` should own conditional `Copy`,
  trait-bound satisfaction, substitution, turbofish/caller type params,
  specialization demotion, ownership/linearity consequences, and the
  diagnostic/report evidence for generic instantiations. Its gate is
  consistency under refinement: the pre-mono conditional answer, instantiated
  with concrete type arguments, must equal the post-mono concrete answer.

Each judgment module must satisfy the same contract:

- pure and deterministic;
- returns a decision record, not a bare `Bool`, `Ty`, or `CapSet`;
- is the only implementation of that semantic decision;
- is consumed by every relevant stage instead of being re-derived downstream;
- drives diagnostics, reports, audit rows, and tooling facts from the same
  record;
- names its owning stage, downstream consumers, and intentional non-scope;
- is consumed by or checked against the interpreter/oracle when the fact affects
  runtime behavior;
- ships with a red-team agreement gate proving historical divergent consumers
  cannot disagree;
- proves completeness when replacing an old implementation, so the new judgment
  covers the old behavior matrix instead of only the cases where consumers
  already agreed;
- obeys the fact-home rule: node-local structural facts are typed-IR fields;
  relational, cross-node, cross-stage, provenance, evidence, and query facts
  live in `CompilerDB` when that DB is pulled.

The practical purity rule belongs to this same pattern. Once capability
judgment can prove an expression is pure, trap-free/total, and returns
non-`Unit`, silently discarding it as `expr;` should be rejected with an explicit
acknowledgement escape: this is ordinary discard hygiene, not a user-facing
row-effect system. Pure expressions that may trap are excluded until the trap
judgment proves they are total, because their traps are observable behavior.

### Principle 2 — Facts committed once, read-only downstream

Every semantic fact about a node — its type, capability set, ownership state,
arithmetic policy, evidence class, resolved identity — is attached **once**, by
the stage that owns that decision, and is **read-only** for every stage after.

The crucial escalation this note asks for: move from *gates that detect drift*
to *types that forbid it*. Shared predicates and pass-agreement gates (items
#2, #3, #5, #6, #10) DETECT disagreement after the fact. That is necessary but
it still leaves the architecture *inviting* the bug — a new pass can always
re-infer and disagree, and the gate only fires if someone wrote the fixture.

The structural fix is **one shared type judgment feeding the already-typed Core,
with `CompilerDB` reserved for relational facts when those facts are pulled**.
Core `CExpr` already
carries `ty : Ty` on every expression node; Elab stamps that field today. The
drift bug is that Check and Elab compute source types independently. Phase 6.5
therefore extracts a `TypeJudgment` module that both Check and Elab call: Check
uses it for type-dependent checks, Elab uses it to stamp `CExpr.ty`, and
downstream stages consume typed Core. Some facts are inherently relational
(call-site↔parameter pass agreement, borrow conflicts, container/context
exclusions), so `CompilerDB` is the relational/evidence/provenance/query
substrate when those facts become load-bearing. `ValidatedCore`/
`ValidatedSSA`/`ValidatedBackendIR` remain boundary tokens; future provenance is
native **edges** in the DB (`lowersTo`/`emitsAs`/`trapSource`), not a
hand-maintained join.

This pattern is not greenfield. `Concrete/Pipeline/Pipeline.lean` already applies
it at **stage granularity**: `ValidatedCore` is constructible only by
`Pipeline.coreCheck`, `ResolvedProgram` only by Resolve, and so on — a
downstream stage cannot fabricate "this ran" without the token. The escalation
this note asks for is from *stage* tokens ("this pass executed") to *node facts*
("this type/capability is committed here, read-only"): carry the fact inside the
IR, not just a phantom proof that a pass ran over the whole program.

This is the same bet Concrete makes about user code — *evidence carried in the
artifact, not reconstructed by inspection* — applied to the compiler's own IR.
Concrete demands evidence-carrying source from its users while its own pipeline
is an ordinary trust-me compiler; closing that gap is dogfooding the thesis onto
the toolchain's own trusted base.

## How the Phase 6.5 items ladder under the principles

| Principle | Phase 6.5 items |
| --- | --- |
| **1 — one reference semantics** | #1 (arithmetic), #4 (no hidden second pipeline), #14 (feature-matrix interp-vs-compiled), #21 (counterexample-first) |
| **2 — facts committed once** | shared predicates, shared `TypeJudgment`, stage contracts, typed Core, `CompilerDB` relational facts, capability fact source, walker coverage, CoreCheck boundary, resolved names, builtin registry, source-span preservation |
| **Evidence the refactor is load-bearing** | #22 (mutation testing), #23 (pass-output replay), #24 (validation artifact) |

Read the phase this way: items under Principle 1 make the oracle true by
construction; items under Principle 2 progressively convert re-derivation from
"caught by a gate" to "impossible by type." The remaining items are the standard
Concrete evidence discipline proving the refactor did not silently weaken
anything.

## Staging (no big-bang)

The roadmap is correctly allergic to rewrites that stall the frontier. This is a
staged conversion, each stage independently shippable and each *retiring* debt
rather than adding it:

1. Land `Concrete/Semantics/IntArith.lean` (item #1) — Principle 1's beachhead,
   and it already has a real bug to its name (the seed-20260703 fold that
   dropped a trap).
2. Introduce the **type-axis single source**: shared `TypeJudgment` feeds Check's
   type-dependent checks and Elab's `CExpr.ty` stamping. Core is already the
   typed carrier. Prove the pattern on literal types before widening.
3. Widen the fact set one axis at a time — capability, arithmetic policy,
   evidence class, resolved identity — each landing with the differential fuzzer
   as the safety net, exactly as the checked-arithmetic flip was staged.
4. Insert `BackendIR` after `ValidatedSSA` when backend/audit facts need a
   structured boundary: `ValidatedSSA -> BackendIR -> ValidatedBackendIR ->
   EmitLLVM`. This is valuable even if LLVM remains the only emitter because it
   gives reports, translation validation, and source maps a structured backend
   contract instead of raw LLVM text.

## Relationship to Phase 14

Phase 6.5, not Phase 14, decides the representation: Core `CExpr` is the typed
carrier, a shared `TypeJudgment` feeds Check and Elab, and future `CompilerDB`
carries relational/evidence/provenance facts when those facts are pulled. Phase
14 #13b then proves and preserves that choice: Elab's re-inference is deleted,
Check/Elab type agreement is structural through the shared judgment, and
preservation proofs show each pass keeps the committed Core meaning. 6.5 makes
the architecture stop inviting
drift; 14 proves the passes honor it.

## Non-goals

- **Not a full verified compiler now.** The aim is that meaning is committed
  once and the reference semantics is single-sourced — not that every pass ships
  a Lean preservation proof today. Preservation proofs are Phase 14, pulled by
  need.
- **Not the incremental/demand-driven rearchitecture.** Turning the batch
  pipeline into a memoized query engine (salsa / rust-analyzer style) is a real
  200x lever for *tooling* — it would make `concrete why`, `concrete diff`,
  `explain`, and the Phase 19 LSP surface instant — but it is the wrong bet
  while the language surface and IR are still moving. It is a Phase 19
  architectural note, not Phase 6.5 work.

## Evidence

The phase is not done because the code is prettier. It is done when the two
principles are checkable:

- **Principle 1:** the `interpret == fold-then-interpret == compiled` gate is
  green over the integer-operation matrix, and a fold that erases a documented
  trap fails a red-team fixture (item #1); every constructor has an
  interp-vs-compiled coverage row (item #14).
- **Principle 2:** for each committed fact, a downstream stage cannot construct
  a program that re-derives it to a different answer — enforced by the
  certificate types where they exist and by pass-agreement gates elsewhere
  (items #2, #3, #5, #9); mutation testing proves each gate is load-bearing
  (item #22).
