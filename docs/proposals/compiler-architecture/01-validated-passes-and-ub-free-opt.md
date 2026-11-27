**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27); decides no language rule. Maps to provisional task [R-0730](../../../ROADMAP.md#r-0730) (extending `R-0583` and `R-0711`).**

# 01 — Pluggable `ValidatedPass` Slots, `FactTable` Carrier, and UB-Free Mid-End Optimization

## Problem Statement

Modern optimizing compilers (LLVM, GCC) achieve high code quality by leaning on an intricate tower of **undefined behavior** (`undef`, `poison`, `freeze`, TBAA, pointer-provenance heuristics, speculative `nuw`/`nsw` assumptions). Formal verification tools such as Alive2 continue to uncover miscompilations in LLVM because those rules interact inconsistently across passes. Verified compilers (CompCert, CakeML) avoid UB miscompilation by refusing aggressive assumptions, historically leaving substantial performance on the table.

Concrete does not need to accept this tradeoff. Several of the strongest guarantees that C/C++/LLVM approximate via UB or heuristic alias analysis can be established **soundly** in Concrete—provided each guarantee is backed by an explicit proof obligation:

1. **Scoped exclusivity without global points-to heuristics (requires C5-to-Memory/BIR bridge proof):** References are strictly second-class (`&T`, `&mut T` cannot be stored in aggregates or returned from functions) and linear ownership gives each non-`Copy` value a single owner. **Important qualification:** shared borrows (`&T`, `&T`) **can overlap**, so Concrete does *not* have unconditional O(1) non-aliasing across all borrows. Rather, `&mut` exclusivity and distinct owned roots yield non-aliasing *only* after we prove the explicit **C5 borrow-exclusivity-to-Memory/BIR bridge theorem** connecting `ConcreteSpec`'s loan invariant (`R-0611`) to physical memory disjointness in [Concrete/Interfaces/Carriers.lean](../../../Concrete/Interfaces/Carriers.lean)'s `Memory` / `BIR` carriers. That bridge proof does not exist yet and is an explicit prerequisite before any pass consumes `nonAliasing` facts.
2. **Checked arithmetic and bounds without UB:** Accepted safe code has no hidden traps ([no hidden traps](../../decisions.md#decided-project-no-hidden-traps)). Every arithmetic operation, shift, division, and array index either carries a **discharged evidence token** proving safety or has an explicit source-level branch.
3. **Explicit effect boundaries:** Every external effect requires a `with(...)` permission and a linear grant value. Pure/read-only/terminating/non-aborting functions are proved facts in `CheckedCore`, not speculative guesses.

To exploit these invariants *inside Concrete's own mid-end* (instead of throwing them away at lowering time and hoping LLVM recovers them), three concrete architectural upgrades are needed in `Concrete/` (two tiny ones in the stage-interfaces cutover, and the `FactTable` carrier once Slice 3 introduces octagon/purity facts).

---

## 1. Pluggable `ValidatedPass` Slots in `Interfaces/Pipeline`

### Current Architecture on `main`
In [Concrete/Interfaces/Pipeline.lean](../../../Concrete/Interfaces/Pipeline.lean), `PassFunctions` hardcodes a fixed 10-stage linear funnel over the slice-indexed carriers in [Concrete/Interfaces/Carriers.lean](../../../Concrete/Interfaces/Carriers.lean) (`MIR lang profile`, `Memory lang profile`, `SSA lang profile`, `BIR lang profile`):

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

Meanwhile, [Concrete/Contracts.lean](../../../Concrete/Contracts.lean) *already* defines `EvidenceKind.provedValidator` and `Refines`, and `Pipeline` in [Concrete/Interfaces/Contracts.lean](../../../Concrete/Interfaces/Contracts.lean) already composes heterogeneous `Stage` entries. What belongs in the **cutover's single interface change** is a **closed pass slot at each IR level** (`MIR`, `Memory`, `SSA`, `BIR`) where proved passes can be registered later, with **Slice 0's pass lists empty**.

### Proposed Architecture (Separating Distinct Validation Obligations)
Introduce a `ValidatedPass` combinator over any IR carrier with a `Semantics`. Crucially, **one undifferentiated "validated" label is not sufficient**, because an untrusted proposer can cheat naive checks in three separate ways:
- **Swapping two existing trap sites** still passes a bare origin-subset check (`target ⊆ source`).
- **Replacing an uncalled helper function** still passes a closed whole-program (`main`-only) refinement check.
- **Returning the right answer while blowing stack or allocation budgets** still passes plain functional refinement.

Therefore every `ValidatedPass` carries **explicit, separate obligations**:

```lean
structure ValidatedPass (IR Failure : Type) {E : Type} (sem : Semantics IR E) where
  name : String
  wf : IR → Prop
  /-- Safety & progress of well-formed inputs (required even for identity fallback!). -/
  wf_refines_self : ∀ a, wf a → Refines sem a a
  /-- Deterministic work budget (never wall-clock timeouts; extends R-0528). -/
  workLimit : Nat
  /-- Untrusted proposal: hand-written pass, e-graph rewrite, external solver, or LLM rewrite. -/
  propose : IR → Except Failure IR
  /-- Separate kernel-checked validators: functional refinement (per-function when open),
      exact trap-site correspondence + elimination accounting, and resource non-regression. -/
  validateFunctional : IR → IR → Bool
  validateProvenance : IR → IR → EliminationLedger → Bool
  validateResources  : IR → IR → Bool
  validate_sound : ∀ a b ledger, wf a →
    validateFunctional a b = true →
    validateProvenance a b ledger = true →
    validateResources a b = true →
    wf b ∧ Refines sem a b ∧ OptProvenance a b ledger ∧ ResourceNonRegressing a b
```

From any `ValidatedPass`, a single generic constructor builds a `Stage IR Failure IR`:
- Execute `b ← propose a` (bounded by `workLimit`).
- Run **all three** checks (`validateFunctional`, `validateProvenance`, `validateResources`).
- If any check returns `false`, either fail closed (mandatory validation mode) or fall back to returning `a` unchanged—**which is sound ONLY because `wf_refines_self` proves `wf a → Refines sem a a`!** (Note: `Refines` in [Concrete/Contracts.lean](../../../Concrete/Contracts.lean) includes `PrefixSafety` and `¬ stuck`, so identity refinement is *not* unconditionally reflexive; it requires safety and progress of `a`.)
- Record `kind := .provedValidator`, `outputInvariant := wf`, and the conjoined postcondition.

**Cutover integration:** Add closed pass-list slots (`optMIR`, `optMemory`, `optSSA`, `optBIR`) to `PassFunctions` in [Concrete/Interfaces/Pipeline.lean](../../../Concrete/Interfaces/Pipeline.lean), kept **empty (`[]`) in Slice 0**. Because `Refines` and `OptProvenance` compose transitively, later slices register proved passes in those slots without reshaping the pipeline or top-level theorem.

---

## 2. Fixing `PreservesOrigins` Before Optimizations Land

### Current Architectural Trap on `main`
In [Concrete/Interfaces/Contracts.lean](../../../Concrete/Interfaces/Contracts.lean), origin preservation is currently defined as **exact set equality**:

```lean
def PreservesOrigins (source target : List Origin) : Prop :=
  (∀ o ∈ target, o ∈ source) ∧ (∀ o ∈ source, o ∈ target)
```

Exact coverage is right for **plain, non-optimizing lowering**, where every source operation maps directly to emitted IR. However, any real optimization (constant folding, dead-code elimination, CSE, redundant-check elimination) removes operations, violating `source ⊆ target`. Conversely, relaxing it to bare `target ⊆ source` is **too weak**: a cheating optimizer could swap two existing trap locations `site₁ ↔ site₂` (both in `source`) and still satisfy `target ⊆ source`!

### Proposed Split (Part of the Cutover's Single Interface Change)
Split provenance into **two distinct stage rules**:

1. **Plain Lowering Rule (`PreservesOrigins`, unchanged):**
   - Non-optimizing structural lowering passes (`Core → MIR`, `MIR → Memory`, `Memory → SSA`, `SSA → BIR`) keep exact origin coverage (`PreservesOrigins`).
2. **Optimizing Pass Rule (`OptProvenance`):**
   - **Exact trap-site correspondence per execution:** Every trapping execution of `target` must trap with the **exact same `(TrapKind, Origin)`** as the corresponding execution of `source` (already enforced dynamically by `Observation.trapped` in `Refines`, and statically checked on every surviving trap site by `validateProvenance`—closing the trap-site-swapping loophole).
   - **Accounting for every removed value (`EliminationLedger`):** Every origin present in `source` but absent from `target` must be accounted for in `ledger : List (Origin × EliminationReason)` (e.g. `folded_constant`, `dead_binding`, `discharged_check`, `cse_merged`), and every origin in `target` must come from `source` (`origins(target) ∪ ledger.origins = origins(source)`). No origin vanishes silently and no origin is fabricated.

---

## <a id="3-first-class-facttable-carrier-through-bir"></a>3. First-Class `FactTable` Carrier Through BIR (Slice 3+)

### Current Limitation on `main`
In the Slice-0 prototype ([Concrete/Stages.lean](../../../Concrete/Stages.lean) and [Concrete/MIR.lean](../../../Concrete/MIR.lean)), `Admission.check` produces `evidence : List Admission.Token`, which `MIR.lower` projects down to `discharged : Option Int` on `MIR.Op.add`; in the newer stage-interface carriers ([Concrete/Interfaces/Carriers.lean](../../../Concrete/Interfaces/Carriers.lean)), `OperationCheck` records `CheckDisposition` per operation, but richer static facts (octagon intervals from Slice 3, bitmasks from `R-0712`, loop iteration bounds from Slice 4, purity/no-writes/termination/non-abort facts from `R-0575`, and C5-proved mutable exclusivity from Slice 7) are not yet carried as a unified side-channel across `MIR → Memory → SSA → BIR`.

### Proposed Architecture: Pass-Preserved `FactTable`
Define a typed `FactTable` carried alongside `Origin` across `Core → MIR → Memory → SSA → BIR` (starting at Slice 3, not blocking the cutover):

```lean
inductive Fact where
  | constVal (v : Int)
  | intRange (lo hi : Int)              -- from octagon checker (Slice 3)
  | bitMask (knownZero knownOne : Nat)  -- from R-0712 (Slice 7)
  | exclusiveDisjoint (p₁ p₂ : PlaceId) -- ONLY from proved C5 &mut/owned-root exclusivity bridge (Slice 7; NEVER shared &T/&T)
  | uniqueOwner (p : PlaceId)           -- structural from linear ownership (Slice 1/6)
  | pureEffect (reads writes aborts : Bool) (terminates : Bool) -- from Facts (Slice 3/5/11)
  | loopBound (maxIters : Nat)          -- from #[variant] / counted for (Slice 4)
```

Each fact entry is keyed by stable entity/site identity (`SiteId` / `PlaceId` / `SSAValue`) and backed by a **soundness predicate** `FactTable.Holds sem program facts` stating that every dynamic state reachable in `sem` satisfies the recorded facts.

- **Admission (`CheckedCore`)** populates the initial `FactTable` with a proof `admission_facts_hold` (and `exclusiveDisjoint` is populated **only** once the C5 borrow-exclusivity bridge theorem is proved).
- **Each lowering pass** maps keys forward and proves `FactTable.Holds` is preserved (or drops any fact it cannot transport—dropping a fact is always sound!).
- **Every mid-end optimization pass and backend (`R-0711` LLVM hints + LCB native optimizations)** reads from this one authoritative `FactTable` ([Rule 5: every fact once](../../PHILOSOPHY.md#the-rules)).

---

## 4. UB-Free, Ownership-Driven Mid-End Optimizations

With closed pass slots, `OptProvenance`, and `FactTable` in place, Concrete's mid-end (`MIR` / `BIR`) can execute aggressive optimizations with **zero `undef` / `poison` semantics**:

1. **Evidence-backed arithmetic simplification (Slice 3):**
   - Constant folding, algebraic identities (`x + 0 → x`, `x * 2 → x << 1`), and width narrowing strictly gated by `intRange` / `constVal` / `bitMask` entries in `FactTable`.
   - Zero overflow UB: wrapping vs. non-overflowing rewrites only trigger when `FactTable` proves the result is in-range.
2. **Exclusivity-backed load/store forwarding, GVN, and LICM (Slices 3–7):**
   - Shared `&T` borrows may alias, so no optimization assumes `&T`/`&T` disjointness. Once the C5-to-Memory/BIR exclusivity bridge proof lands at Slice 7, `exclusiveDisjoint` (`&mut` vs. any other live borrow, or distinct owned roots) enables sound load-store forwarding, redundant load elimination, and LICM without fragile global pointer-escape heuristics.
   - Speculative hoisting and dead-call elimination are gated strictly on `pureEffect` (`reads = false, writes = false, aborts = false, terminates = true`), matching `R-0711`'s soundness controls.
3. **Static linear reuse & copy elision (`Perceus`-style without refcounting, Slices 6–12):**
   - Because ownership is strictly linear (values are consumed at a unique point), MIR knows statically when an owned aggregate's storage is dead immediately before constructing a same-layout aggregate—enabling **proved in-place mutation** without dynamic reference counting.
4. **Vectorization & branch elimination (Slices 4, 7, 18):**
   - Counted `for` loops with C5-proved `exclusiveDisjoint` slice views and `discharged` bounds checks satisfy the exact preconditions for safe SIMD vectorization without runtime alias-check versioning loops.

---

## Why This Will Work

- **Zero TCB bloat:** Untrusted optimization heuristics live in `propose`; only the separate small validators (`validateFunctional`, `validateProvenance`, `validateResources`) and their soundness proofs sit in the proof obligation.
- **No `poison` soundness traps:** Every `Interfaces.BIR` value is a concrete bit-vector or defined trap—never `undef`/`poison`. Soundness of every rewrite rule is a direct bit-vector/state theorem in Lean 4 (`omega`, `bv_decide`, or structural induction).
- **Composable with `R-0711` and `R-0299`:** The same `FactTable` drives both Concrete's own UB-free mid-end passes and backend metadata emission (`R-0711`) and proof-producing e-graphs (`R-0299`).

## Slice Staging & Acceptance Gates

| Increment | Slice | Deliverable | Gate |
|---|---|---|---|
| **1. Cutover interface change** | Cutover (`B0b` / Slice 0) | Closed pass slots (`optMIR`/`optMemory`/`optSSA`/`optBIR`, empty in Slice 0) in `Interfaces/Pipeline.lean` + split `PreservesOrigins` (plain lowering) vs. `OptProvenance` (optimizing passes: exact trap-site preservation + elimination ledger) in `Interfaces/Contracts.lean` | Slice-0 end-to-end theorem unchanged with empty pass lists; negative controls rejecting (a) a swapped trap site and (b) an unaccounted dropped origin |
| **2. UB-free mid-end v1** | Slice 3 | `FactTable` (`constVal`, `intRange`, `pureEffect`) + deterministic work budgets (`R-0528`) + proved constant folding, copy/DCE, redundant-branch pruning over `Interfaces.MIR`/`BIR` with `wf_refines_self` proved | Separate checks for per-function refinement, trap-site provenance, and resource non-regression; planted unsound proposer & stack-regressing proposer refused; opt vs. unopt differential match on LLVM & LCB |
| **3. Exclusivity-backed alias opts** | Slices 4–7 | Prove C5 borrow-exclusivity-to-Memory/BIR bridge theorem; populate `exclusiveDisjoint`, `loopBound`, `bitMask`, `uniqueOwner`; load/store forwarding, LICM, linear in-place reuse | C5 exclusivity bridge proof + negative control refusing assumed non-aliasing of two shared `&T` borrows + adversarial interaction workloads (`R-0573`) |

**Spec impact:** None. Purely inside `Concrete/`. No owner decision required to implement `R-0730`.
