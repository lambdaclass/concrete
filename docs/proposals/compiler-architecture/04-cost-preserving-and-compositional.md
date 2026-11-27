**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27); decides no language rule. Maps to provisional tasks [R-0733](../../../ROADMAP.md#r-0733) (cost-preserving compilation) and [R-0734](../../../ROADMAP.md#r-0734) (compositional verified compilation & linking, extending `R-0705`).**

# 04 — Cost-Preserving Compilation and Compositional Verified Linking

Two deep open problems in verified compilation live right inside [Concrete/Contracts.lean](../../../Concrete/Contracts.lean) and [Concrete/Interfaces/Contracts.lean](../../../Concrete/Interfaces/Contracts.lean):
1. **Cost-preserving compilation:** Proving not only *what* a compiled program returns, but also *how much* stack, heap, and execution work it can consume on the target machine.
2. **Compositional verified compilation & linking:** Proving compilation of **open modules** separately so that linking separately compiled modules (and explicit FFI boundaries) composes their correctness theorems without re-proving the whole program.

Both become dramatically cleaner if we make two small, well-scoped generalizations to Concrete's semantic contracts **early**, rather than bolting them on after 11 slices.

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
In [Concrete/Contracts.lean](../../../Concrete/Contracts.lean), `Semantics A E` tracks `State`, `step`, `result`, `safe`, and `trace`, and `CheckpointSimulation` in [Concrete/Interfaces/Contracts.lean](../../../Concrete/Interfaces/Contracts.lean) proves trace/observation refinement. Neither carries resource cost.

### Proposed Architectural Generalization
Add an optional, **parametric ordered cost monoid `Cost`** (defaulting to `Unit` where cost is irrelevant, so existing proofs remain untouched):

```lean
structure CostMonoid (C : Type) where
  zero : C
  add : C → C → C
  le : C → C → Prop
  le_refl : ∀ x, le x x
  le_trans : ∀ x y z, le x y → le y z → le x z

/-- Concrete's default quantitative cost vector. -/
structure ResourceCost where
  steps : Nat       -- abstract steps (Core) or instructions (BIR / LCB)
  peakStack : Nat   -- frame slots (BIR) or exact stack bytes (LCB / binary validator)
  allocBytes : Nat  -- peak requested bytes per lineage (from C7 accounting)
```

Extend `Semantics` and `Refines` with a **cost-transfer bound `bound : C₁ → C₂`** (e.g., affine `k * steps + c`, or exact identity for `allocBytes` since lowering never inserts hidden allocations!):

- **Core → MIR → Memory → BIR (Slices 0–5):** Prove `target.allocBytes ≤ source.allocBytes` (zero hidden allocations!), `target.steps ≤ c₁ * source.steps + c₀` (linear step preservation), and `target.frameSlots ≤ bound(source)` directly inside each stage's `CheckpointSimulation`.
- **BIR → LCB → Native (`LP0` + Phase E, `R-0572`, `R-0505`):** Compose BIR's step/slot bounds with LCB's per-instruction machine expansion factor and proved Mach-O/ELF frame-byte decoding (`R-0505`), yielding an **end-to-end theorem from source `#[variant]` down to exact native instruction count and stack bytes**!
- **Optimizing passes (`R-0730`, `R-0732`):** A `ValidatedPass` can optionally require `cost(b) ≤ cost(a)` so optimizations never silently regress stack depth or allocation count.

### Why This Will Work
- **CerCo / CompCert-Cost precedent, minus the C pain:** Academic cost-preserving C compilers struggled primarily because C has hidden compiler spills, unstructured `goto`/recursion, and opaque allocators. Concrete's structured loops with `#[variant]`, explicit allocators, and direct LCB control make the cost-transfer bound linear and exact.
- **No source syntax creep:** Cost limits stay strictly in `Concrete.toml` policies/ratchets (`R-0576`, `R-0201`), obeying the 2026-11-22 owner decision.

---

## <a id="2-compositional-verified-compilation-and-verified-linking"></a>2. Compositional Verified Compilation and Verified Linking (`R-0734`)

### Problem Statement
Closed whole-program compilation proofs (`main → exit status`) break the moment a real system compiles modules separately, links libraries compiled by different compiler invocations/versions, or calls an `extern fn` C/assembly boundary. Research systems like CompCertO and DimSum solved parts of this for C, but at massive proof complexity—almost entirely because **arbitrary shared mutable pointers and re-entrant callbacks** let an unknown callee mutate almost any byte in memory.

### Concrete's Structural Advantage
In Concrete, **open-module boundaries are dramatically narrower than in C or Rust**:
1. **No globals (`OD8`):** No hidden mutable global cells shared across modules.
2. **Second-class references (`REF-1`/`REF-3`):** A callee receives only owned values and scoped borrows (`&T`, `&mut T`) whose disjoint memory footprints are **known statically at the call site**. No callee can stash a borrowed pointer to mutate later!
3. **Explicit dual authority (`with(...)` + linear grants):** If a callee lacks `with(Fs)` or `with(Unsafe)` and receives no grant value, it **structurally cannot** perform I/O or touch unpassed memory.
4. **Already planned interfaces (`R-0556`, `R-0582`, `R-0705`):** Slice 8 already introduces bodyless generated module interfaces (`R-0556`) and per-module certificates (`R-0582`), and Phase E already schedules open-module correctness (`R-0705`, proof-order item 5).

### Proposed Architecture: `InteractionSemantics` + Horizontal `link` Composition

Generalize [Concrete/Contracts.lean](../../../Concrete/Contracts.lean)'s closed `Semantics A E` into an **open-component `InteractionSemantics`** parameterized by an interface signature `Sig` (exported functions, imported functions, and allowed external event kinds):

```lean
/-- An open component transitions internally, invokes an imported function with
    explicit value/borrow/grant arguments, or receives an incoming call on an export. -/
inductive Interaction (CallArg CallRet ExtEvent : Type) where
  | internal
  | extEvent (e : ExtEvent)
  | callOut (callee : FnId) (args : CallArg) (footprint : Footprint)
  | retIn   (result : CallRet) (updatedFootprint : Footprint)
```

With two core operations and one central theorem:

1. **Horizontal composition (`link`):** Given two open components `M₁` and `M₂` whose import/export signatures match (and satisfy `R-0556`'s `InterfaceSummary` contracts), `link M₁ M₂` connects `M₁`'s `callOut` to `M₂`'s entry and hides internal calls.
2. **Closing (`close`):** Connecting a complete module graph's `main` to a target launcher/environment oracle recovers the **exact** existing closed `Semantics A E` in [Concrete/Contracts.lean](../../../Concrete/Contracts.lean)!
3. **Horizontal refinement theorem (`link_refines`):**
   ```lean
   theorem link_refines {M₁ M₁' M₂ M₂'} :
       OpenRefines M₁ M₁' → OpenRefines M₂ M₂' →
       OpenRefines (link M₁ M₂) (link M₁' M₂')
   ```
   **Consequence:** Compile module `A` and module `B` **separately** (even with different verified optimization passes or cached certificates from `R-0582`). Linking them yields a **free, kernel-checked whole-program theorem** with zero whole-program re-verification.

### Unifying Bonus: `concrete sim` (`R-0717`) Falls Out of the Same Model
Notice that an **external environment / grant launcher** is just another component `Env` implementing the imported grant operations (`Fs`, `Entropy`, `Alloc`, `Time`, `Network`). Swapping the real OS `Env` for a deterministic fault-injecting `SimEnv` in `link Program SimEnv` gives **`concrete sim` (`R-0717`)** the exact same formal semantics as production execution—zero second semantics!

---

## Slice Staging, Spec Impact & Acceptance Gates

| Increment | Slice | Deliverable | Spec Impact | Gate |
|---|---|---|---|---|
| **1. Cost monoid hook (`R-0733`)** | `B0b` / Slice 1 | Add parametric `Cost` (default `Unit`) to `Semantics` & `CheckpointSimulation`; prove step + frame-slot preservation on Slice 0 `MIR → Memory → BIR` | None | Existing Slice-0 proofs unchanged + step/slot bound theorem kernel-checked |
| **2. Loop/call & native cost preservation (`R-0733`)** | Slices 4–5 + `LP0` | Carry Slice 4–5 step bounds through BIR and compose with `LP0` / `R-0505` native stack/instruction bounds | Uses planned Slice 4–5 step model (no source cost syntax) | Planted stack-blowup / hidden-alloc lowering mutant rejected; bounds match `concrete facts` |
| **3. Open-component boundary (`R-0734`)** | Slice 8 / before `A2c` (Slice 11) | Define `InteractionSemantics` & `Footprint` boundary in `ConcreteSpec` (`StageInterface`/`C6`/`C10`) and `Concrete/Contracts.lean` before Core freeze | **Yes** (open-module call-boundary relation before `A2c`) | 2-module composition theorem `link_refines` + `close` agreement with closed `Semantics` |
| **4. Separate-module verified linking & FFI (`R-0734` + `R-0705`)** | Slice 17 / Phase E | Compositional module certificates (`R-0582`) + FFI footprint/capability contracts + verified linker step (`R-0505`) | Owner-approved `R-0705` subset & assumptions | Separately compiled 2-module native binary holds composed theorem; incompatible-interface/substituted-object linking refused |

### Owner Questions Required Before `R-0734` Enters `ConcreteSpec`
1. **Open-module subset and residual assumptions (shared with `R-0705`):** Approve the supported open-module boundary representation (values + disjoint scoped borrows + explicit grants, excluding raw-pointer cross-module aliasing until `trusted` FFI contracts in Slice 17).
2. **Pre-`A2c` timing:** Confirm landing the minimal `InteractionSemantics` / `Footprint` hook in `StageInterface`/`C6`/`C10` at Slice 8 so Core freezes cleanly at Slice 11 without breaking closed Slice 0–7 proofs.
