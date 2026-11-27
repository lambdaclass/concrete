**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27); decides no language rule. Maps to provisional task [R-0730](../../../ROADMAP.md#r-0730) (extending `R-0583` and `R-0711`).**

# 01 — Pluggable `ValidatedPass` Slots, `FactTable` Carrier, and UB-Free Mid-End Optimization

## Problem Statement

Modern optimizing compilers (LLVM, GCC) achieve high code quality by leaning on an intricate tower of **undefined behavior** (`undef`, `poison`, `freeze`, TBAA, pointer-provenance heuristics, speculative `nuw`/`nsw` assumptions). Formal verification tools such as Alive2 continue to uncover miscompilations in LLVM because those rules interact inconsistently across passes. Verified compilers (CompCert, CakeML) avoid UB miscompilation by refusing aggressive assumptions, historically leaving substantial performance on the table.

Concrete does not need to accept this tradeoff. Every optimization that C/C++/LLVM justifies via UB or brittle global alias analysis is justified **soundly and structurally** in Concrete:

1. **Exact, O(1) structural non-aliasing by construction:** References are strictly second-class (`&T`, `&mut T` cannot be stored in aggregates or returned from functions) and linear ownership guarantees exclusive ownership of non-`Copy` values. Disjoint roots and disjoint scoped borrows never alias—without heuristic points-to analysis or TBAA.
2. **Checked arithmetic and bounds without UB:** Accepted safe code has no hidden traps ([no hidden traps](../../decisions.md#decided-project-no-hidden-traps)). Every arithmetic operation, shift, division, and array index either carries a **discharged evidence token** proving safety or has an explicit source-level branch.
3. **Explicit effect boundaries:** Every external effect requires a `with(...)` permission and a linear grant value. Pure/read-only/terminating/non-aborting functions are proved facts in `CheckedCore`, not speculative guesses.

To exploit these invariants *inside Concrete's own mid-end* (instead of throwing them away at lowering time and hoping LLVM recovers them), three concrete architectural upgrades are needed in `Concrete/`.

---

## 1. Pluggable `ValidatedPass` Slots in `Interfaces/Pipeline`

### Current Architecture on `main`
In [Concrete/Interfaces/Pipeline.lean](../../../Concrete/Interfaces/Pipeline.lean), `PassFunctions` hardcodes a fixed 10-stage linear funnel:

```lean
structure PassFunctions ... where
  lex : Source → Except LexRefusal TokenStream
  elaborate : ...
  checkCore : ...
  lowerCore : ...
  lowerUnchecked : ...
  lowerMemory : ...
  buildSSA : ...
  buildBIR : ...
  emitLLVM : ...
  emitLCB : ...
```

Meanwhile, [Concrete/Contracts.lean](../../../Concrete/Contracts.lean) *already* defines `EvidenceKind.provedValidator` and transitive `Refines`, and `Pipeline` in [Concrete/Interfaces/Contracts.lean](../../../Concrete/Interfaces/Contracts.lean) already composes heterogeneous `Stage` entries. What is missing is a first-class combinator for inserting **optional, endomorphic (`IR → IR`) optimization passes** into the registered pipeline without altering `Theorem.lean` each time a pass is added.

### Proposed Architecture
Introduce a generic `ValidatedPass` combinator over any IR with a `Semantics`:

```lean
structure ValidatedPass (IR Failure : Type) {E : Type} (sem : Semantics IR E) where
  name : String
  wf : IR → Prop
  /-- Untrusted proposal: hand-written pass, e-graph rewrite, external solver, or LLM rewrite. -/
  propose : IR → Except Failure IR
  /-- Small, total, kernel-checked equivalence/refinement validator. -/
  validate : IR → IR → Bool
  /-- One-time soundness proof of the validator (never needs re-proving when `propose` changes). -/
  validate_sound : ∀ a b, wf a → validate a b = true → wf b ∧ Refines sem a b
```

From any `ValidatedPass`, a single generic constructor builds a `Stage IR Failure IR`:
- Execute `b ← propose a`.
- Run `validate a b`. If `false`, either fail closed (for mandatory/strict validation mode) or fall back to returning `a` unchanged (guaranteed identity refinement `Refines sem a a`, never miscompiling!).
- Record `kind := .provedValidator`, `outputInvariant := wf`, and `success a b := Refines sem a b`.

**Pipeline integration:** Extend `PassFunctions` with ordered pass lists (`optMIR : List (Stage MIR ...)`, `optMemory`, `optSSA`, `optBIR`). Because `Refines` is reflexive and transitive, the end-to-end theorem in [Concrete/Theorem.lean](../../../Concrete/Theorem.lean) composes an arbitrary chain of validated passes with **zero changes to the top-level theorem statement**.

---

## 2. Fixing `PreservesOrigins` Before Optimizations Land

### Current Architectural Trap on `main`
In [Concrete/Interfaces/Contracts.lean](../../../Concrete/Interfaces/Contracts.lean), origin preservation is currently defined as **exact set equality**:

```lean
def PreservesOrigins (source target : List Origin) : Prop :=
  (∀ o ∈ target, o ∈ source) ∧ (∀ o ∈ source, o ∈ target)
```

This works for Slice 0's non-optimizing lowering, where every source operation becomes an emitted instruction. However, the **moment any real optimization runs** (constant folding, dead-code elimination, common subexpression elimination, redundant-check elimination, algebraic simplification), `target` legitimately **drops** eliminated operations—violating `∀ o ∈ source, o ∈ target`!

### Proposed Fix: `OriginProvenance` + `EliminationLedger`
Split the single `PreservesOrigins` predicate into two distinct concepts:

1. **Diagnostic Provenance Soundness (`OriginSubset`, mandatory for all passes):**
   ```lean
   def OriginSubset (source target : List Origin) : Prop :=
     ∀ o ∈ target, o ∈ source
   ```
   Every surviving instruction, trap site, or runtime event in `target` traces back to a real source `Origin` in `source` (no fabricated spans, no lost trap locations). This holds for **all** optimizing passes, including DCE, CSE, constant folding, inlining, and loop unrolling (which duplicates origins from `source`, still preserving `target ⊆ source`).
2. **Retention / Elimination Accounting (`OriginLedger`, optional metadata for facts & coverage):**
   - Non-optimizing structural lowering passes (`Core → MIR`, `MIR → Memory`) may retain `PreservesOrigins` (or record discharged sites explicitly).
   - Optimizing passes record eliminated origins in a side ledger (`eliminated : List (Origin × EliminationReason)`) so `concrete facts` and diagnostic coverage still know *why* a source operation disappeared (e.g. `folded_constant`, `dead_binding`, `discharged_check`) without preventing optimization.

---

## 3. First-Class `FactTable` Carrier Through BIR

### Current Limitation on `main`
In [Concrete/Stages.lean](../../../Concrete/Stages.lean), `Admission.check` produces `evidence : List Admission.Token`, which [Concrete/MIR.lean](../../../Concrete/MIR.lean) immediately projects down to `discharged : Option Int` on `MIR.Op.add`. All richer static facts established by `CheckedCore` (ownership exclusivity, disjoint borrow roots, value intervals/octagons from Slice 3, bitmask/modulo/shift facts from `R-0712`, loop iteration bounds from Slice 4, purity/no-writes/termination/non-abort facts from `R-0575`) are discarded before `Memory`, `SSA`, and `BIR`.

### Proposed Architecture: Pass-Preserved `FactTable`
Define a typed `FactTable` carried alongside `Origin` across `Core → MIR → Memory → SSA → BIR`:

```lean
inductive Fact where
  | constVal (v : Int)
  | intRange (lo hi : Int)              -- from octagon checker (Slice 3)
  | bitMask (knownZero knownOne : Nat)  -- from R-0712 (Slice 7)
  | nonAliasing (p₁ p₂ : PlaceId)       -- structural from C5 second-class borrows (Slice 7)
  | uniqueOwner (p : PlaceId)           -- structural from linear ownership (Slice 1/6)
  | pureEffect (reads writes aborts : Bool) (terminates : Bool) -- from Facts (Slice 3/5/11)
  | loopBound (maxIters : Nat)          -- from #[variant] / counted for (Slice 4)
```

Each fact entry is keyed by stable entity/site identity (`SiteId` / `PlaceId` / `RegId`) and backed by a **soundness predicate** `FactTable.Holds sem program facts` stating that every dynamic state reachable in `sem` satisfies the recorded facts.

- **Admission (`CheckedCore`)** populates the initial `FactTable` with a proof `admission_facts_hold`.
- **Each lowering pass** maps keys forward and proves `FactTable.Holds` is preserved (or drops any fact it cannot transport—dropping a fact is always sound!).
- **Every mid-end optimization pass and backend (`R-0711` LLVM hints + LCB native optimizations)** reads from this one authoritative `FactTable` ([Rule 5: every fact once](../../PHILOSOPHY.md#the-rules)).

---

## 4. UB-Free, Ownership-Driven Mid-End Optimizations

With `ValidatedPass`, `OriginSubset`, and `FactTable` in place, Concrete's mid-end (`MIR` / `BIR`) can execute aggressive optimizations with **zero `undef` / `poison` semantics**:

1. **Evidence-backed arithmetic simplification (Slice 0–3):**
   - Constant folding, algebraic identities (`x + 0 → x`, `x * 2 → x << 1`), and width narrowing strictly gated by `intRange` / `constVal` / `bitMask` entries in `FactTable`.
   - Zero overflow UB: wrapping vs. non-overflowing rewrites only trigger when `FactTable` proves the result is in-range.
2. **Zero-alias load/store forwarding, GVN, and LICM (Slices 3–7):**
   - Because `nonAliasing` and `uniqueOwner` come directly from Concrete's type/ownership checker, load-store forwarding, redundant load elimination, and loop-invariant hoisting across `&T`/`&mut T` references are **O(1) table lookups**, never fragile pointer-escape heuristics.
   - Speculative hoisting and dead-call elimination are gated strictly on `pureEffect` (`reads = false, writes = false, aborts = false, terminates = true`), matching `R-0711`'s soundness controls.
3. **Static linear reuse & copy elision (`Perceus`-style without refcounting, Slices 6–12):**
   - Because ownership is strictly linear (values are consumed at a unique point), MIR knows statically when an owned aggregate's storage is dead immediately before constructing a same-layout aggregate—enabling **proved in-place mutation** without dynamic reference counting.
4. **Vectorization & branch elimination (Slices 4, 7, 18):**
   - Counted `for` loops with `nonAliasing` slice views and `discharged` bounds checks satisfy the exact preconditions for safe SIMD vectorization without runtime alias-check versioning loops.

---

## Why This Will Work

- **Zero TCB bloat:** Untrusted optimization heuristics live in `propose`; only the small `validate` function and its soundness proof sit in the proof obligation.
- **No `poison` soundness traps:** Every BIR value is a concrete bit-vector or defined trap—never `undef`/`poison`. Soundness of every rewrite rule is a direct bit-vector/state theorem in Lean 4 (`omega`, `bv_decide`, or structural induction).
- **Composable with `R-0711` and `R-0299`:** The same `FactTable` drives both Concrete's own UB-free mid-end passes and backend metadata emission (`R-0711`) and proof-producing e-graphs (`R-0299`).

## Slice Staging & Acceptance Gates

| Increment | Slice | Deliverable | Gate |
|---|---|---|---|
| **1. Pipeline & origin prep** | `B0b` / Slice 1 | `ValidatedPass` combinator in `Interfaces/Pipeline.lean`, `OriginSubset` split in `Interfaces/Contracts.lean`, minimal Slice-0 `FactTable` carrying `constVal` + `discharged` | `Stage.Validated` proof for `ValidatedPass`, Slice-0 end-to-end theorem unchanged, negative control rejecting an unsound `propose` |
| **2. UB-free mid-end v1** | Slice 3 | Octagon-backed `intRange` + `pureEffect` in `FactTable`; proved constant folding, copy/DCE, redundant-branch pruning over MIR/BIR | Kernel-checked validator soundness, differential observation match (opt vs. unopt on LLVM & LCB), negative control refusing hoisting of `may-abort` or `&mut`-writing calls |
| **3. Ownership/alias opts** | Slices 4–7 | `loopBound`, `bitMask`, `nonAliasing`, `uniqueOwner` in `FactTable`; load/store forwarding, LICM, linear in-place reuse | Refinement proofs + adversarial interaction workloads (`R-0573`) + zero-UB differential suite |

**Spec impact:** None. Purely inside `Concrete/`. No owner decision required to implement `R-0730`.
