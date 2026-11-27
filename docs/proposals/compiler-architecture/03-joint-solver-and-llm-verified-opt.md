**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27); decides no language rule. Maps to provisional task [R-0732](../../../ROADMAP.md#r-0732) (extending `R-0299`, `R-0583`, and `R-0727`).**

# 03 — Kernel-Verified Refinement Validators, Untrusted Joint Backend Solvers, and Verified LLM Optimization

## Problem Statement

Two of the hardest problems in compiler engineering are actually **the exact same architectural problem**:

1. **The Backend Phase-Ordering Wall:** Instruction selection, instruction scheduling, register allocation, and spilling are jointly NP-hard and interfere destructively when run as separate sequential passes. LLVM hides this behind millions of lines of brittle heuristics. Verified compilers (CompCert, CakeML) prove modest sequential passes directly, which makes matching LLVM's native speed enormously expensive.
2. **The LLM-Era Optimization Opportunity:** [docs/PHILOSOPHY.md](../../PHILOSOPHY.md#what-concrete-is-for) explicitly expects most Concrete code to be written by language models and checked by humans and machines. An LLM (or an offline solver like CP-SAT / Unison / MAP-Elites under `R-0727`) can synthesize remarkably fast domain-specific implementations (bit-twiddling, cache blocking, SIMD schedules, algebraic simplifications)—**if and only if** the compiler can soundly reject any subtly buggy candidate without expanding the trusted computing base.

Both problems collapse cleanly once the compiler separates **untrusted search** from **kernel-checked validation**.

---

## 1. Core Principle: Zero-TCB Untrusted Search + Tiny Verified Validator

Concrete's pipeline contract ([Concrete/Contracts.lean](../../../Concrete/Contracts.lean)) already distinguishes:
- `EvidenceKind.directProof` — for stable structural passes (lexing, elaboration, lowering, BIR→LCB adaptation).
- `EvidenceKind.provedValidator` — for search-heavy, heuristic, or externally proposed transformations.

For any search-heavy step—whether it is an e-graph, a CP-SAT joint instruction-selector/register-allocator, a MAP-Elites search driver (`R-0727`), or an LLM proposing a faster body—**the proposer does not need a single line of Lean proof**. Only the validator needs a soundness proof:

```lean
theorem bir_validate_sound (b₁ b₂ : BIR.Program) (w : Witness) :
    b₁.WF → validateBIR b₁ b₂ w = true →
    b₂.WF ∧ Refines birSemantics b₁ b₂
```

Once `bir_validate_sound` is proved **once** in Lean:
- You can swap in a better heuristic, a new SAT/ILP solver, or a frontier LLM tomorrow.
- If the proposer produces a valid optimization + witness, Lean checks it in milliseconds and admits it under the exact same end-to-end `ImageTheorem.compile_correct`.
- If the proposer hallucinates or has a subtle edge-case bug (e.g. off-by-one overflow, wrong trap site, swapped live range), `validateBIR` returns `false` and the compiler rejects it cleanly with a pinpointed mismatch.

---

## <a id="2-the-proposal--validator-boundary"></a>2. The Proposal ↔ Validator Boundary (`OptimizationProposal` / `ObligationBundle`)

To let external tools (e-graph engines, CP-SAT solvers, MAP-Elites, LLMs) participate without entering the TCB or scraping prose logs, define a small canonical wire protocol reusing [Concrete/Artifact/Encoding.lean](../../../Concrete/Artifact/Encoding.lean):

### Outbound: `ObligationBundle` (Compiler → Untrusted Tool)
For a selected function (or hot loop/block), the compiler exports a self-contained canonical payload:
- Typed entity identity & canonical digest (`Artifact/Sha256.lean`),
- The current IR (`Core` or `BIR`) plus its `FactTable` ([doc 01](01-validated-passes-and-ub-free-opt.md#3-first-class-facttable-carrier-through-bir)),
- Target profile & cost metrics (`frameSlots`, instruction count, spill count, step bound, or target ISA constraints for LCB),
- For repair workflows (`R-0593`/`R-0599`): any unproved obligation proposition + concrete failing witness inputs.

### Inbound: `OptimizationProposal` (Untrusted Tool → Compiler)
The external tool returns one of three typed proposals:
1. **IR rewrite + replay witness (`birRewrite`):** Candidate `BIR` + a compact proof/replay witness (symbolic-execution alignment map, e-graph rewrite trace, or register-liveness mapping).
2. **Source-level candidate (`sourceCandidate`):** A candidate `.con` function body proposed by an LLM or human against a reference `.con` implementation (`concrete verify-opt ref.con opt.con`).
3. **Layout or backend schedule (`layoutPlan` / `lcbSchedule`):** A candidate `LayoutPlan` ([doc 02](02-layout-plan-synthesis.md)) or LCB instruction/register schedule.

### Gate: Kernel-Checked Validator
- **Never trusts the proposer.** Re-parses/re-checks `WF`, runs the proved validator on `(original, candidate, witness)`, checks optional cost non-regression ([doc 04](04-cost-preserving-and-compositional.md#1-cost-preserving-compilation)), and binds the accepted witness into the compilation certificate (`R-0552`, `R-0582`).

---

## 3. Three Tiers of Validators (Growing Cumulatively Across Slices)

### Tier A — BIR ↔ BIR Symbolic-Execution & Witness Validator (Start in `B0b` / Slice 1)
How a tiny (~150–300 line) Lean validator verifies a wide class of mid-end optimizations without SMT in the TCB:
1. **Acyclic / basic-block symbolic equivalence:** Symbolically evaluate each block of `b₁` and `b₂` into hash-consed expression DAGs (normalized using a small library of kernel-proved bit-vector algebraic lemmas: commutativity, associativity, identity, strength reduction, constant folding, dead-binding elimination). Verify that every live output register, branch condition, exit word, and trap site `(kind, SiteId)` matches under the witness's block/register bijection.
2. **Loop & CFG alignment (Slices 3–5):** The untrusted witness supplies a **lockstep or cut-point correspondence** between CFG cut-points (entry, loop headers, exits) plus a register-relation map (CompCert / Necula translation-validation style). The validator symbolically checks every cut-point-to-cut-point path—linear in the number of blocks, zero fixpoint guessing inside the validator!
3. **Proof-producing e-graph replay (Slice 3+ / Phase E, `R-0299`):** An external e-graph engine (`egg`-style, untrusted) runs equality saturation and outputs a linear sequence of `(ruleId, site, substitution)` rewrite steps; Lean replays only the chosen steps against kernel-proved rewrite rules.

### Tier B — `concrete verify-opt <ref.con> <opt.con>` (Verified LLM-as-Optimizer, Slice 3+)
Turns any LLM into a **100%-sound domain optimizer**:
1. Author writes (or keeps) a crystal-clear, obviously-correct reference function `ref.con`.
2. An LLM (or MAP-Elites search under `R-0727`) generates an aggressively optimized candidate `opt.con` (unrolled, bit-packed, branchless, SIMD-scheduled).
3. Both compile through `CheckedCore` to `BIR` (checking identical signature, capability set, and no-hidden-traps admission).
4. Concrete runs either:
   - The fast **Tier A BIR refinement validator** (for structural/algebraic/unrolling rewrites), or
   - **Lean functional-model equivalence** via `concrete extract-lean` (`R-0495` at Slice 10, where the LLM also supplies a Lean proof script checked by Lean's kernel + `nanoda`/`con-leche`).
5. Only if the equivalence theorem kernel-checks does `opt.con` replace `ref.con` in the build, carrying a certificate that `opt` refines `ref`!

### Tier C — Joint Backend Solver + Native LCB Validator (Post-`LP0` / Phase E)
Directly solves the classical **phase-ordering problem** in LCB (`R-0517`):
- **Untrusted joint solver:** Export hot BIR/LCB functions to a constraint solver (CP-SAT / ILP, in the style of Unison) that **jointly** solves instruction selection, register allocation, coalescing, spilling, and scheduling for AArch64 / x86-64 under exact latency/register-pressure models.
- **Tiny proved LCB validator:** Instead of proving the CP-SAT solver in Lean, verify the resulting LCB machine function against BIR/LCB-IR using a **CompCert-style symbolic live-range & value-transfer validator** over LCB's imported ISA semantics (`LeanCompilerBackend.Verified`).
- **Payoff:** Unison-grade / LLVM-beating codegen on hot loops with a **~500-line validator proof** instead of a 50,000-line heuristic-backend proof.

---

## Why This Will Work

- **Proven precedent:** CompCert's register-allocation validator (Rideau & Leroy), Necula's translation validation, `egg`/Cranelift e-graphs, and Unison's CP-SAT joint backend are each individually battle-tested. Concrete's advantage is **unifying all of them inside one Lean 4 kernel-checked pipeline** where BIR has zero UB and LCB already has formal Lean semantics (`LeanCompilerBackend.Verified.Source`).
- **Tiny TCB:** External solvers, e-graphs, MAP-Elites, and LLMs have **zero** trust privilege—every candidate passes through a kernel-checked Lean validator before entering the artifact.
- **Reproducible builds (`R-0577`):** Nondeterministic search runs offline or produces a committed/cached witness file; release builds replay only the deterministic validator, keeping builds 100% bit-for-bit reproducible.

## Slice Staging & Acceptance Gates

| Increment | Slice | Deliverable | Gate |
|---|---|---|---|
| **1. BIR refinement validator v1** | `B0b` / Slice 1 | `Concrete/Validator/BIR.lean` proving `validateBIR_sound` for acyclic BIR (const-fold, copy-prop, DCE, algebraic identities) + planted-bug refusal tests | Kernel-checked soundness (only standard 3 axioms), planted wrong-constant and wrong-trap-site candidates rejected |
| **2. CFG/cut-point validator + `concrete verify-opt` v1** | Slices 3–5 | Cut-point symbolic alignment for branches/loops/calls + `concrete verify-opt ref.con opt.con` CLI + canonical `OptimizationProposal` wire schema | Positive equivalence controls + planted off-by-one/wrong-branch-condition mutants rejected + end-to-end `ImageTheorem` integration |
| **3. Joint backend solver + LCB validator** | Post-`LP0` / Phase E | External CP-SAT/e-graph solver adapter + proved LCB symbolic regalloc/schedule validator feeding `R-0517` & `R-0727` | Proved LCB validator soundness, cross-backend differential parity, benchmark speedup evidence (`R-0424`) |

**Spec impact:** None. Purely inside `Concrete/` and tooling. No owner decision required to implement `R-0732`.
