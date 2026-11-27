**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27); decides no language rule. Maps to provisional tasks [R-0733](../../../ROADMAP.md#r-0733) (cost-preserving compilation) and [R-0734](../../../ROADMAP.md#r-0734) (compositional verified compilation & linking, extending `R-0705`).**

# 04 — Cost-Preserving Compilation and Compositional Verified Linking

Two deep open problems in verified compilation live right beside [Concrete/Contracts.lean](../../../Concrete/Contracts.lean) and [Concrete/Interfaces/Contracts.lean](../../../Concrete/Interfaces/Contracts.lean):
1. **Cost-preserving compilation:** Proving not only *what* a compiled program returns, but also *how much* stack, heap, and execution work it can consume on the target machine.
2. **Compositional verified compilation & linking:** Proving compilation of **safe open modules** separately so that linking separately compiled modules composes their correctness theorems without re-proving the whole program.

**Timing note (Codex cutover review):** Neither of these belongs in the Stage-0 stage-interfaces cutover. A cost parameter that defaults to "no cost" (`Unit`) bounds nothing, and adding an open-environment parameter during the cutover would undo the closed-semantics work the cutover is stabilizing. Both land cleanly at their natural slices: cost preservation at **Slices 4–5 + `LP0`**, and the safe open-module boundary at **Slice 8** (before `A2c`).

---

## <a id="1-cost-preserving-compilation"></a>1. Cost-Preserving Compilation (`R-0733`)

### Problem Statement
CompCert, CakeML, and almost all production compilers prove (at best) **functional equivalence**: target behavior refines source behavior. They make **zero** formal guarantee that lowering or optimization preserves stack depth, heap footprint, or worst-case execution steps (WCET). Real-time, kernel, crypto, and aerospace engineers therefore have to run separate, unverified binary-level static analyzers after compilation and pray that codegen didn't blow a budget.

Concrete *already* builds half the machinery needed to close this gap:
- No hidden runtime, no GC, no hidden allocations ([PHILOSOPHY.md](../../PHILOSOPHY.md#the-rules), Rule 8).
- Explicit allocators and per-lineage byte accounting (`C7`, `R-0613`, `R-0595`).
- Mandatory `#[variant]` loop/recursion bounds and abstract step-counting model at Slices 4–5 ([ROADMAP.md](../../../ROADMAP.md#vertical-slices)).
- Final-binary stack accounting via LCB (`R-0575`, `R-0505`, `R-0704`).
- Policy/budget enforcement strictly in `Concrete.toml` ratchets (never source cost syntax, per [2026-11-22 decision](../../decisions.md#decided-2026-11-22-llm-inference-showcase-and-float-order)).

### Current Architecture on `main`
In [Concrete/Contracts.lean](../../../Concrete/Contracts.lean), `Semantics A E` tracks `State`, `step`, `result`, `safe`, and `trace`, and `CheckpointSimulation` in [Concrete/Interfaces/Contracts.lean](../../../Concrete/Interfaces/Contracts.lean) proves trace/observation refinement. Neither carries resource cost yet—and we intentionally keep them focused on functional refinement during Slice 0 rather than introducing a vacuous `Unit`-defaulted parameter.

### Proposed Architecture (Landing at Slices 4–5 + `LP0`, Non-Vacuous)
Once Slices 4–5 introduce loops, calls, and the spec's step-counting model, introduce an **explicit, non-vacuous `ResourceCost` contract** layered beside functional `Refines` (never defaulting to "no cost"):

```lean
/-- Concrete's quantitative resource vector (never optional/Unit on cost-checked stages). -/
structure ResourceCost where
  steps : Nat       -- abstract steps (Core) or instructions (Interfaces.BIR / LCB)
  peakStack : Nat   -- frame slots (`Interfaces.BIR.frameSlots`) or exact stack bytes (LCB / binary validator)
  allocBytes : Nat  -- peak requested bytes per lineage (from C7 accounting)
```

Define a separate **`CostPreserving bound s t a b`** judgment alongside `Refines s t a b` (matching [Doc 01](01-validated-passes-and-ub-free-opt.md)'s rule that functional refinement and resource bounds are separate, independently checked obligations):

- **Core → MIR → Memory → SSA → BIR (Slices 4–5):** Prove `target.allocBytes ≤ source.allocBytes` (zero hidden allocations!), `target.steps ≤ c₁ * source.steps + c₀` (linear step preservation), and `target.frameSlots ≤ bound(source)` over the `Interfaces` carriers in [Concrete/Interfaces/Carriers.lean](../../../Concrete/Interfaces/Carriers.lean).
- **BIR → LCB → Native (`LP0` + Phase E, `R-0572`, `R-0505`):** Compose BIR's step/slot bounds with LCB's per-instruction machine expansion factor and proved Mach-O/ELF frame-byte decoding (`R-0505`), yielding an **end-to-end theorem from source `#[variant]` down to exact native instruction count and stack bytes**!
- **Optimizing passes (`R-0730`, `R-0732`):** Resource non-regression (`validateResources`) is checked as its own explicit obligation, preventing a functionally equivalent rewrite from silently regressing stack slots, allocations, or step bounds.

### Why This Will Work
- **CerCo / CompCert-Cost precedent, minus the C pain:** Academic cost-preserving C compilers struggled primarily because C has hidden compiler spills, unstructured `goto`/recursion, and opaque allocators. Concrete's structured loops with `#[variant]`, explicit allocators, and direct LCB control make the cost-transfer bound linear and exact.
- **No source syntax creep:** Cost limits stay strictly in `Concrete.toml` policies/ratchets (`R-0576`, `R-0201`), obeying the 2026-11-22 owner decision.

---

## <a id="2-compositional-verified-compilation-and-verified-linking"></a>2. Compositional Verified Compilation and Verified Linking (`R-0734`)

### Problem Statement
Closed whole-program compilation proofs (`main → exit status`) break the moment a real system compiles modules separately, links libraries compiled by different compiler invocations/versions, or calls an `extern fn` C/assembly boundary. Research systems like CompCertO and DimSum solved parts of this for C, but at massive proof complexity—almost entirely because **arbitrary shared mutable pointers and re-entrant callbacks** let an unknown callee mutate almost any byte in memory.

### Concrete's Structural Advantage (Safe Concrete Modules vs. FFI)
In Concrete, **safe Concrete module boundaries** are dramatically narrower than C or Rust boundaries:
1. **No globals (`OD8`):** No hidden mutable global cells shared across modules.
2. **Second-class references (`REF-1`/`REF-3`):** A safe Concrete callee receives only owned values and scoped borrows (`&T`, `&mut T`) whose memory footprints are lexically bounded at the call site. No safe callee can stash a borrowed reference to mutate after return.
3. **Explicit dual authority (`with(...)` + linear grants):** If a callee lacks `with(Fs)` or `with(Unsafe)` and receives no grant value, it **structurally cannot** perform I/O or touch unpassed memory.
4. **Already planned interfaces (`R-0556`, `R-0582`, `R-0705`):** Slice 8 already introduces bodyless generated module interfaces (`R-0556`) and per-module certificates (`R-0582`), and Phase E already schedules open-module correctness (`R-0705`, proof-order item 5).

**Explicit FFI boundary:** Safe Concrete modules are settled first at Slice 8 before `A2c`. **Re-entrant or state-retaining FFI** (foreign C code that retains pointers, mutates external hidden state, or calls back into Concrete) is explicitly *out of scope* for the safe-module boundary and requires separate owner approval at Slice 17 / Phase E (`R-0705`).

### Proposed Architecture: Safe `InteractionSemantics` + Horizontal `link` Composition (Slice 8+, Not in Cutover)

Without disturbing the cutover's closed `Semantics A E` in [Concrete/Contracts.lean](../../../Concrete/Contracts.lean), layer an **open-component `InteractionSemantics`** for safe Concrete modules at Slice 8 (parameterized by an interface signature `Sig` of exported functions, imported functions, and allowed external event kinds):

```lean
/-- A safe Concrete module transitions internally, invokes an imported function with
    explicit value/scoped-borrow/grant arguments, or receives an incoming call on an export. -/
inductive Interaction (CallArg CallRet ExtEvent : Type) where
  | internal
  | extEvent (e : ExtEvent)
  | callOut (callee : FnId) (args : CallArg) (footprint : Footprint)
  | retIn   (result : CallRet) (updatedFootprint : Footprint)
```

With two core operations and one central theorem:

1. **Horizontal composition (`link`):** Given two open modules `M₁` and `M₂` whose import/export signatures match (and satisfy `R-0556`'s `InterfaceSummary` contracts), `link M₁ M₂` connects `M₁`'s `callOut` to `M₂`'s entry and hides internal calls.
2. **Closing (`close`):** Connecting a complete module graph's `main` to a target launcher/environment oracle recovers the **exact** existing closed `Semantics A E` in [Concrete/Contracts.lean](../../../Concrete/Contracts.lean)—preserving all closed Slice 0–7 proofs unchanged!
3. **Horizontal refinement theorem (`link_refines`):**
   ```lean
   theorem link_refines {M₁ M₁' M₂ M₂'} :
       OpenRefines M₁ M₁' → OpenRefines M₂ M₂' →
       OpenRefines (link M₁ M₂) (link M₁' M₂')
   ```
   **Consequence:** Compile safe module `A` and safe module `B` **separately** (even with different verified optimization passes or cached certificates from `R-0582`). Linking them yields a **free, kernel-checked whole-program theorem** with zero whole-program re-verification.

### Unifying Bonus: `concrete sim` (`R-0717`) Falls Out of the Same Model
Notice that an **external environment / grant launcher** is just another component `Env` implementing the imported grant operations (`Fs`, `Entropy`, `Alloc`, `Time`, `Network`). Swapping the real OS `Env` for a deterministic fault-injecting `SimEnv` in `link Program SimEnv` gives **`concrete sim` (`R-0717`)** the exact same formal semantics as production execution—zero second semantics!

---

## Slice Staging, Spec Impact & Acceptance Gates

| Increment | Slice | Deliverable | Spec Impact | Gate |
|---|---|---|---|---|
| **1. Non-vacuous loop/call cost preservation (`R-0733`)** | Slices 4–5 | Separate `CostPreserving` judgment over real `ResourceCost` (no `Unit`-defaulted Slice-0 hook); prove linear step, zero-hidden-alloc, and `BIR.frameSlots` bounds across `MIR → Memory → SSA → BIR` | Uses planned Slice 4–5 step model (no source cost syntax) | Cost-preservation theorems kernel-checked; planted stack-blowup / hidden-alloc lowering mutant rejected |
| **2. Native cost bounds (`R-0733`)** | `LP0` + Phase E | Compose BIR cost bounds with `LP0` / `R-0505` proved Mach-O/ELF frame-byte and instruction-count bounds | None | Bounds match `concrete facts` (`R-0575`, `R-0576`) |
| **3. Safe open-module boundary (`R-0734`)** | Slice 8 / before `A2c` (Slice 11) | Define safe Concrete `InteractionSemantics` & `Footprint` boundary in `ConcreteSpec` (`StageInterface`/`C6`/`C10`) and `Concrete/Contracts.lean` without altering the cutover's closed `Semantics` | **Yes** (safe open-module call-boundary relation before `A2c`) | 2-module composition theorem `link_refines` + `close` agreement with closed `Semantics` |
| **4. Separate-module verified linking & FFI (`R-0734` + `R-0705`)** | Slice 17 / Phase E | Compositional module certificates (`R-0582`) + verified linker step (`R-0505`); separate owner approval before any re-entrant or state-retaining FFI contract | Owner-approved `R-0705` subset & separate FFI approval | Separately compiled 2-module native binary holds composed theorem; incompatible-interface/substituted-object linking refused |

### Owner Questions Required Before `R-0734` Enters `ConcreteSpec`
1. **Safe Concrete module boundary before `A2c` (Slice 8):** Approve the call-boundary contract for **safe Concrete modules** (owned values + lexically scoped borrows + explicit linear grants, zero globals) so Core freezes cleanly at Slice 11 (`A2c`) without undoing the cutover's closed `Semantics`.
2. **Re-entrant or retaining FFI (separate approval, Slice 17 / Phase E):** Keep re-entrant callbacks and pointer/state-retaining foreign C functions explicitly excluded from the safe-module contract and subject to a separate owner decision under `R-0705`.
