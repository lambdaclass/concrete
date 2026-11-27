**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27); decides no language rule. Maps to provisional tasks [R-0733](../../../ROADMAP.md#r-0733) (cost-preserving compilation) and [R-0734](../../../ROADMAP.md#r-0734) (compositional verified compilation & linking, extending `R-0705`).**

# 04 — Cost-Preserving Compilation and Compositional Verified Linking

Two deep open problems in verified compilation live right beside [Concrete/Contracts.lean](../../../Concrete/Contracts.lean) and [Concrete/Interfaces/Contracts.lean](../../../Concrete/Interfaces/Contracts.lean):
1. **Cost-preserving compilation:** Proving not only *what* a compiled program returns, but also *how much* stack, heap, and execution work it can consume on the target machine.
2. **Compositional verified compilation & linking:** Proving compilation of **safe open modules** separately so that linking separately compiled modules composes their correctness theorems without re-proving the whole program.

**Timing note (Codex cutover review):** Neither of these belongs in the Stage-0 stage-interfaces cutover. A cost parameter that defaults to "no cost" (`Unit`) bounds nothing, and adding an open-environment parameter during the cutover would undo the closed-semantics work the cutover is stabilizing. Both land cleanly at their natural slices: cost preservation at **Slices 4–5 + `LP0`**, and the safe open-module boundary at **Slice 8** (before `A2c`).

---

## <a id="1-cost-preserving-compilation"></a>1. Cost-Preserving Compilation (`R-0733`)

### Problem Statement
Functional refinement alone does not bound resource use. CakeML has a verified space-cost semantics. Concrete proposes cost bounds for matching execution prefixes under the same inputs and environmental choices. [CakeML space-cost theorem](https://cakeml.org/oopsla20.pdf).

The proposed cost model draws on these parts of the specification and roadmap:
- No hidden runtime, no GC, no hidden allocations ([PHILOSOPHY.md](../../PHILOSOPHY.md#the-rules), Rule 8).
- Explicit allocators and per-lineage byte accounting (`C7`, `R-0613`, `R-0595`).
- Termination evidence and a proposed abstract step model for loops and calls at Slices 4–5. [Vertical slices](../../../ROADMAP.md#vertical-slices).
- Final-binary stack accounting via LCB (`R-0575`, `R-0505`, `R-0704`).
- Policy/budget enforcement strictly in `Concrete.toml` ratchets (never source cost syntax, per [2026-11-22 decision](../../decisions.md#decided-2026-11-22-llm-inference-showcase-and-float-order)).

### Current Architecture on `main`
`Semantics` supplies transitions and observations. The indexed `CheckpointSimulation` relates reachable checkpoints. Quantitative transfer is a separate proof obligation for the slices that have a cost model. [Contracts](../../../Concrete/Interfaces/Contracts.lean).

### Proposed Architecture (Landing at Slices 4–5 + `LP0`, Non-Vacuous)
Distinguish IR frame slots, live native stack bytes, cumulative allocation requests and peak live bytes per lineage. Prove monotone transfer bounds, including bounded stuttering, active caller frames and call fan-out where applicable. Derive native upper bounds from validated machine artifacts.

- **Core → MIR → Memory → SSA → BIR (Slices 4–5).** Compare matched finite prefixes, including prefixes of divergent executions. Count abstract steps at the named level and BIR slots per function. For each lowering, prove `target.steps ≤ c₁ * source.steps + c₀` on every matched prefix, with fixed natural constants independent of the input and run length. Also bound target steps between checkpoints and prove the stuttering rank decreases.
- **Allocation (C7).** Track requested, live, peak and function-peak bytes per lineage. Requests accumulate. Compose peaks using the entry live balance and the greatest increase during each prefix. [Accounting](../../../ConcreteSpec/Spec/Accounting.lean).
- **BIR → LCB → native (R-0572, R-0505).** Bound live bytes across the active call stack and validate instruction expansion, spills and frames in the machine artifact. A decreasing variant can establish termination. Exact dynamic instruction counts and elapsed worst-case time would require their own models and proofs.
- **Optimizing passes (R-0730, R-0732).** Check resource non-regression for each declared metric under the same inputs and choices. Monotonicity lets a consumer compose successive upper bounds.

### Why This Will Work
- **No source syntax creep:** Cost limits stay strictly in `Concrete.toml` policies/ratchets (`R-0576`, `R-0201`), obeying the 2026-11-22 owner decision.

---

## <a id="2-compositional-verified-compilation-and-verified-linking"></a>2. Compositional Verified Compilation and Verified Linking (`R-0734`)

### Problem Statement
Closed whole-program compilation proofs (`main → exit status`) break the moment a real system compiles modules separately, links libraries compiled by different compiler invocations/versions, or calls an `extern fn` C/assembly boundary. Research systems like CompCertO and DimSum solved parts of this for C, but at massive proof complexity—almost entirely because **arbitrary shared mutable pointers and re-entrant callbacks** let an unknown callee mutate almost any byte in memory.

### Concrete's Structural Advantage (Safe Concrete Modules vs. FFI)
In Concrete, **safe Concrete module boundaries** are dramatically narrower than C or Rust boundaries:
1. **No globals (`OD8`):** No hidden mutable global cells shared across modules.
2. **Second-class references.** Scoped loans restrict how long a callee may retain access. Their footprints can include dynamically sized heap objects and slices.
3. **Explicit authority.** The call contract must relate permitted effects and grants, including grants transferred back on return.
4. **Already planned interfaces (`R-0556`, `R-0582`, `R-0705`):** Slice 8 already introduces bodyless generated module interfaces (`R-0556`) and per-module certificates (`R-0582`), and Phase E already schedules open-module correctness (`R-0705`, proof-order item 5).

**Explicit FFI boundary.** Seek the owner's approval for safe Concrete modules at Slice 8, before `A2c`. Re-entrant or state-retaining FFI needs a separate decision.

### Proposed Architecture: Safe `InteractionSemantics` + Horizontal `link` Composition (Slice 8+, Not in Cutover)

Without disturbing the cutover's closed `Semantics A E` in [Concrete/Contracts.lean](../../../Concrete/Contracts.lean), layer an **open-component `InteractionSemantics`** for safe Concrete modules at Slice 8 (parameterized by an interface signature `Sig` of exported functions, imported functions, and allowed external event kinds):

Model incoming and outgoing calls and matching returns, including nested call identity, permitted effects, transferred ownership, live loans and grants. The protocol needs `callIn`, `callOut`, `retIn` and `retOut`, internal steps, external events and terminal trap/abort outcomes. A stack of suspended call frames pairs returns with the correct invocation during re-entry.

1. **Horizontal composition.** Quantify each module's guarantee over legal contexts satisfying its import contract. Check both endpoints' signatures, permissions, ownership, loans, grants and dynamic footprints. Each endpoint must preserve the unpassed state across a call.
2. **Closing.** Prove agreement of closing the module graph with the existing closed semantics for the same launcher and environment.
3. **Linking.** Prove horizontal refinement from compatible endpoint guarantees without assuming the composite theorem. Native linking additionally checks ABI, object identity and applicable premises, including the calling convention and layouts.

At Slice 8 seek owner approval for the safe boundary. R-0705 owns later verified linking and any separately approved FFI extension. A simulated environment under R-0717 could implement the same permitted interface, with its own specified responses.

---

## Slice Staging, Spec Impact & Acceptance Gates

| Increment | Slice | Deliverable | Spec Impact | Gate |
|---|---|---|---|---|
| **1. Loop/call cost preservation (R-0733)** | Slices 4–5 | Source-to-IR prefix bounds and per-function slots | Uses the slice step model | Prove the linear step bound on matched prefixes. Check monotone composition, bounded stuttering and nested calls. Reject a cost-increasing rewrite |
| **2. Native-bound composition** | After the source-to-IR increment (R-0572, R-0505) | Validated machine frame and instruction bounds | Compiler and artifact validation | Compare matched prefixes with active caller frames and reject an understated bound from the artifact |
| **3. Safe open-module boundary (`R-0734`)** | Slice 8 / before `A2c` (Slice 11) | Define safe Concrete `InteractionSemantics` & `Footprint` boundary in `ConcreteSpec` (`StageInterface`/`C6`/`C10`) and `Concrete/Contracts.lean` without altering the cutover's closed `Semantics` | **Yes** (safe open-module call-boundary relation before `A2c`) | Owner-approved safe protocol covers both endpoints and specifies closing agreement |
| **4. Separate-module verified linking & separately approved FFI (`R-0705`)** | Slice 17 / Phase E | Compositional module certificates (`R-0582`) + verified linker step (`R-0505`); separate owner approval before any re-entrant or state-retaining FFI contract | Owner-approved `R-0705` subset & separate FFI approval | Separately compiled 2-module native binary holds composed theorem; incompatible-interface/substituted-object linking refused |

### Owner Questions Required Before `R-0734` Enters `ConcreteSpec`
1. **Safe Concrete module boundary before `A2c` (Slice 8):** Approve the call-boundary contract for **safe Concrete modules** (owned values + lexically scoped borrows + explicit linear grants, zero globals) so Core freezes cleanly at Slice 11 (`A2c`) without undoing the cutover's closed `Semantics`.
2. **Re-entrant or retaining FFI (separate approval, Slice 17 / Phase E):** Keep re-entrant callbacks and pointer/state-retaining foreign C functions explicitly excluded from the safe-module contract and subject to a separate owner decision under `R-0705`.
