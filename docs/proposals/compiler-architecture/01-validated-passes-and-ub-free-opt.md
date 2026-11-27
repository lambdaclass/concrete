**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27); decides no language rule. Maps to provisional task [R-0730](../../../ROADMAP.md#r-0730) (extending `R-0583` and `R-0711`).**

# 01 — Pluggable `ValidatedPass` Slots, `FactTable` Carrier, and UB-Free Mid-End Optimization

## Problem Statement

Modern optimizing compilers (LLVM, GCC) achieve high code quality by leaning on an intricate tower of **undefined behavior** (`undef`, `poison`, `freeze`, TBAA, pointer-provenance heuristics, speculative `nuw`/`nsw` assumptions). Formal verification tools such as Alive2 continue to uncover miscompilations in LLVM because those rules interact inconsistently across passes. Verified compilers (CompCert, CakeML) avoid UB miscompilation by refusing aggressive assumptions, historically leaving substantial performance on the table.

Concrete does not need to accept this tradeoff. Several of the strongest guarantees that C/C++/LLVM approximate via UB or heuristic alias analysis can be established **soundly** in Concrete—provided each guarantee is backed by an explicit proof obligation:

1. **Scoped exclusivity without global points-to heuristics (requires C5-to-Memory/BIR bridge proof):** References are strictly second-class (`&T`, `&mut T` cannot be stored in aggregates or returned from functions) and linear ownership gives each non-`Copy` value a single owner. **Important qualification:** shared borrows (`&T`, `&T`) **can overlap**, so Concrete does *not* have unconditional O(1) non-aliasing across all borrows. Rather, `&mut` exclusivity and distinct owned roots yield non-aliasing *only* after we prove the explicit **C5 borrow-exclusivity-to-Memory/BIR bridge theorem** connecting `ConcreteSpec`'s loan invariant (`R-0611`) to physical memory disjointness in [Concrete/Interfaces/Carriers.lean](../../../Concrete/Interfaces/Carriers.lean)'s `Memory` / `BIR` carriers. That bridge proof does not exist yet and is an explicit prerequisite before any pass consumes `nonAliasing` facts.
2. **Checked arithmetic and bounds:** Slice 0 admission discharges known-safe additions. Later operations need evidence appropriate to their slice. [No hidden traps](../../decisions.md#decided-project-no-hidden-traps).
3. **Explicit effect boundaries:** Later slices may derive purity, write, termination and abort facts for use by validated passes.

To exploit these invariants *inside Concrete's own mid-end* (instead of throwing them away at lowering time and hoping LLVM recovers them), three concrete architectural upgrades are needed in `Concrete/` (two tiny ones in the stage-interfaces cutover, and the `FactTable` carrier once Slice 3 introduces octagon/purity facts).

---

## 1. Pluggable `ValidatedPass` Slots in `Interfaces/Pipeline`

### Current Architecture on `main`
`PassFunctions` collects ten executable fields used by the registry to build checked and unchecked routes with a selected emitter. The thirteen stage entries operate on slice-indexed carriers. [Registry](../../../Concrete/Interfaces/Pipeline.lean) · [Carriers](../../../Concrete/Interfaces/Carriers.lean).

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

The proposed registry fixes each IR’s semantics, input and output invariants, callable-function relation, provenance rule, resource metric and refusal rule. A proposer supplies a candidate and witness. Acceptance requires a soundness proof for each of the three work-bounded checks.

- **Callable refinement is an open obligation.** The indexed `BehaviorRefinement` starts with `ProgramSafe source input →` and compares the results selected by its two result predicates. With `sem.result`, those are process observations. Two functions can write different values to a caller's cell, emit no events and both return zero. The caller distinguishes them on its next read. The hosted projection also discards non-`u8` returns. [Indexed relation](../../../Concrete/Interfaces/Behavior.lean) · [Process observations](../../../Concrete/Contracts.lean) · [Hosted projection](../../../ConcreteSpec/StageInterface.lean).
- **Required callable contract.** For every legal function entry and caller continuation, run the original and rewritten program with that caller, the same arguments, and corresponding initial store, loans and grants. If the source is safe in that context, require target safety. Every terminating target run must match a terminating source run with the same observation and trace. Every divergent target run must match a divergent source run with the same finite visible trace prefixes. On return the continuation receives the typed value and resumes with the state left by the call, including all caller-accessible storage and capabilities. A checker using a relation on return values and post-call states must prove that relation suffices for every such continuation. Establish this contract and its connection to the registered semantics before enabling a nonidentity pass on the supported domain.
- **Identity and composition.** Prove reflexivity and transitivity for the callable contract, including the safety premise needed by the next pass. For the stronger scalar relation, even `Refines sem sem a a` requires reachable safety, permitted events and progress. Syntactic IR well-formedness alone cannot establish these. Identity fallback also needs identity provenance with an empty ledger and resource equality. [Scalar relation](../../../Concrete/Contracts.lean).
- **Handoff.** Check the project, entry, declarations, signatures, mutable locals, layouts and profile's BIR frame-slot limit. From Slice 3, require `FactTable.Holds` for facts used by a pass and transport or re-establish each fact it passes on.
- **Cutover integration.** Keep the four lists private to the registry and empty in Slice 0. `PassFunctions` supplies only executable proposers for MIR, Memory, SSA and BIR. `Registry` wraps them with repository-owned validators and contracts. Mandatory checks return the specified refusal on failure or exhaustion. Optional passes fall back to their validated input. Prove the wrapper’s success and refusal cases, compatibility with the next stage, and empty-list and composition laws.

---

## 2. Fixing `PreservesOrigins` Before Optimizations Land

### Current Architectural Trap on `main`
In [Concrete/Interfaces/Contracts.lean](../../../Concrete/Interfaces/Contracts.lean), origin preservation is currently defined as **exact set equality**:

```lean
def PreservesOrigins (source target : List Origin) : Prop :=
  (∀ o ∈ target, o ∈ source) ∧ (∀ o ∈ source, o ∈ target)
```

Structural lowering retains exact origin coverage, including origins recorded at block boundaries (`Block.boundaryOrigins`). Removing an instruction can preserve that coverage. Even exact set equality allows two trap slots to be swapped.

### Proposed Split (Part of the Cutover's Single Interface Change)
Split provenance into **two distinct stage rules**:

1. **Plain lowering.** Keep `PreservesOrigins` on the structural lowering edges.
2. **Optimizing passes.** For an optimizing pass, relate every surviving occurrence to the source occurrences it implements and check a semantic reason for every eliminated occurrence. For matching trapping executions, resolve the actual trap slot and require equality of kind and full origin, including operation, lexical span and evidence key. Preserve the projected diagnostic and its exact bytes. Compose these witnesses across copies, fusions and eliminations, keeping required admission discharges on their exactly-once path. Two occurrences can share an origin, and two different spans or evidence keys can print the same location. The ledger and the trap-slot check must distinguish them.

---

## <a id="3-first-class-facttable-carrier-through-bir"></a>3. First-Class `FactTable` Carrier Through BIR (Slice 3+)

### Current Limitation on `main`
In the scalar prototype, `Admission.Output.tokens : List Admission.Token` records the discharges. `Stages.Output.evidence` retains the list. `MIR.lower` projects the matching token to `discharged : Option Int` on an addition. The indexed carriers use `OperationCheck` and `CheckDisposition`. A unified table for ranges, bitmasks, loops, effects and C5 exclusivity is proposed for later slices.

### Proposed Architecture: Pass-Preserved `FactTable`
Define a typed `FactTable` carried alongside `Origin` across `Core → MIR → Memory → SSA → BIR` (starting at Slice 3, not blocking the cutover):

```lean
inductive Fact where
  | constVal (v : Int)
  | intRange (lo hi : Int)              -- from octagon checker (Slice 3)
  | bitMask (knownZero knownOne : Nat)  -- from R-0712 (Slice 7)
  | exclusiveDisjoint (p₁ p₂ : PlaceId) -- ONLY from proved C5 &mut/owned-root exclusivity bridge (Slice 7; NEVER shared &T/&T)
  | uniqueOwner (p : PlaceId)           -- requires a proved exclusive footprint for this use
  | pureEffect (reads writes aborts : Bool) (terminates : Bool) -- from Facts (Slice 3/5/11)
  | loopBound (maxIters : Nat)          -- from #[variant] / counted for (Slice 4)
```

An optimization fact applies to a particular artifact, function, program point and value or place version under stated path and lifetime conditions. In MIR/Memory include the block, instruction index and place version. In SSA/BIR bind the dominating SSA definition. `FactTable.Holds` quantifies over reachable states at that point. Intersect at joins and re-establish facts across writes, moves, calls and back edges, or drop optional facts. Elaboration site numbers alone cannot identify values across artifacts.

Shared borrows may overlap. Use exclusive-footprint facts only where the C5 bridge proves them, including the conditions on raw or foreign access. Withhold `exclusiveDisjoint` and `uniqueOwner` for materialized roots reachable through raw pointers, trusted/Unsafe code or extern calls. Admission must establish the initial facts and every lowering must justify the keys and facts it transports.
- **Every mid-end optimization pass and backend (`R-0711` LLVM hints + LCB native optimizations)** reads from this one authoritative `FactTable` ([Rule 5: every fact once](../../PHILOSOPHY.md#the-rules)).

---

## 4. UB-Free, Ownership-Driven Mid-End Optimizations

With closed pass slots, `OptProvenance`, and `FactTable` in place, Concrete's mid-end (`MIR` / `BIR`) can execute aggressive optimizations with **zero `undef` / `poison` semantics**:

1. **Evidence-backed arithmetic simplification (Slice 3):**
   - Constant folding, algebraic identities (`x + 0 → x`, `x * 2 → x << 1`), and width narrowing strictly gated by `intRange` / `constVal` / `bitMask` entries in `FactTable`.
   - Zero overflow UB: wrapping vs. non-overflowing rewrites only trigger when `FactTable` proves the result is in-range.
2. **Exclusivity-backed load/store forwarding, GVN, and LICM (Slices 3–7):**
   - Require the C5 bridge for the actual footprints and preserve the values read on each path. Hoisting additionally requires safe execution on newly reached paths, with effects, termination and first-trap order preserved.
3. **Static linear reuse & copy elision (Slices 6–12):**
   - Reuse a destination only after proving the previous value and all its loans are dead across the write and any calls.
4. **Vectorization & branch elimination (Slices 4, 7, 18):**
   - Vectorization additionally requires dependence, tail, bounds, effect and first-trap proofs, including loop-carried dependences within a single mutable slice.

---

## Why This Will Work

- **Zero TCB bloat:** Untrusted optimization heuristics live in `propose`; only the separate small validators (`validateFunctional`, `validateProvenance`, `validateResources`) and their soundness proofs sit in the proof obligation.
- **Defined values.** Scalar BIR pairs an integer type with a Lean `Int`. Register lookup returns the stored value even for 256 tagged as `u8`. A rewrite that relies on integer range must prove that the relevant values in reachable states of a well-formed program lie within their type’s range. Indexed literals carry bytes, so a rewrite using an integer literal must also establish that decoding succeeds at its declared type. Rewrites must preserve the defined integer, aggregate and trap semantics at the supported slice. [Scalar operands](../../../Concrete/BIR.lean) · [Indexed literals](../../../Concrete/Interfaces/Carriers.lean) · [Literal decoder](../../../ConcreteSpec/StageInterface.lean).
- **Composable with `R-0711` and `R-0299`:** The same `FactTable` drives both Concrete's own UB-free mid-end passes and backend metadata emission (`R-0711`) and proof-producing e-graphs (`R-0299`).

## Slice Staging & Acceptance Gates

| Increment | Slice | Deliverable | Gate |
|---|---|---|---|
| **1. Cutover interface change** | Cutover (`B0b` / Slice 0) | Closed pass slots (`optMIR`/`optMemory`/`optSSA`/`optBIR`, empty in Slice 0) in `Interfaces/Pipeline.lean` + split `PreservesOrigins` (plain lowering) vs. `OptProvenance` (optimizing passes: exact trap-site preservation + elimination ledger) in `Interfaces/Contracts.lean` | Slice-0 end-to-end theorem unchanged with empty pass lists; negative controls rejecting (a) a swapped trap site and (b) an unaccounted dropped origin |
| **2. UB-free mid-end v1** | Slice 3 | Sound point-scoped facts and budgeted constant folding, copy/DCE and branch pruning | Prove callable refinement, occurrence-level provenance and the per-function resource metric. Use a fixture on which the optimizer applies an accepted nonidentity rewrite. Run it with optimization enabled and disabled on each backend (LLVM and LCB). Require identical exit status and exact stderr bytes across all four native runs. Reject equal returns with different caller-visible writes, unequal typed returns with the same hosted projection, and growth beyond the resource bound. |
| **3. Exclusivity-backed alias opts** | Slices 4–7 | Prove C5 borrow-exclusivity-to-Memory/BIR bridge theorem; populate `exclusiveDisjoint`, `loopBound`, `bitMask`, `uniqueOwner`; load/store forwarding, LICM, linear in-place reuse | C5 exclusivity bridge proof + negative control refusing assumed non-aliasing of two shared `&T` borrows + adversarial interaction workloads (`R-0573`) |

**Spec impact:** None. Purely inside `Concrete/`. No owner decision required to implement `R-0730`.
