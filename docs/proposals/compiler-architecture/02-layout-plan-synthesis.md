**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27); decides no language rule. Maps to provisional task [R-0731](../../../ROADMAP.md#r-0731) (extending `R-0580`, `R-0502`, `R-0609`). Owner decision required before changing default struct layout.**

# 02 — Parametric `LayoutPlan` Synthesis and Proved Data-Layout Refinement

## Problem Statement

On modern CPUs and accelerators, **memory layout**—struct field ordering and padding elimination, enum niche packing, hot/cold field splitting, destination-passing copy elision, and Array-of-Structs (AoS) to Struct-of-Arrays (SoA) transformation—routinely produces **5×–50×** real-world speedups, dwarfing instruction-level peephole optimization.

Yet C, C++, and even Rust compilers almost never perform whole-aggregate or whole-collection layout synthesis automatically:
- In C/C++, `offsetof`, pointer arithmetic, `void*` casts, and escaping field pointers bake physical memory layout into program semantics.
- In Rust, although `#[repr(Rust)]` allows field reordering and niche optimization, references (`&T`, `&mut T`) can be stored in structs, passed across arbitrary lifetimes, and cast to raw pointers (`&x.f as *const _`), making whole-collection transforms (AoS↔SoA, hot/cold splitting) practically impossible without whole-program escape analysis.

Concrete has a rare structural advantage:
1. **Strictly second-class references:** No `&T` or `&mut T` can ever be stored in a struct/enum/collection or returned from a function. Field borrows exist only lexically inside scoped calls or `borrow` blocks.
2. **Explicit raw-memory boundary:** Raw pointers (`*T`, `*mut T`) exist only inside `trusted` / `with(Unsafe)` boundaries (`C4` materialization rule in `R-0610`), and foreign/wire types are explicitly marked (`#[repr(C)]`, `ByteSafe` under `R-0714`).
3. **One `Layout` authority (`Memory.lean`):** [Concrete/Memory.lean](../../../Concrete/Memory.lean) is *already* architected as the sole layout authority between `MIR` and `SSA`/`BIR` (`R-0580`), and already separates `.virtual` (non-escaping scalar) from `.materialized` storage via `storageClass` and `escapes`.

---

## 1. The Spec Boundary Question (Owner-Gated Before Slice 6 / `A2c`)

Look closely at `R-0502` and `R-0609` in [ROADMAP.md](../../../ROADMAP.md#a-core-and-theorem) on `main`:
- `R-0502` currently states: *"the default struct layout is declaration order, natural alignment defined exactly per target, size rounded up to the alignment, zero-sized fields taking no bytes, with no C ABI promise."*
- `R-0609` currently states: *"`i32` enum tags, no niches."*

**Why this matters before Slice 6 and Core freeze (`A2c` at Slice 11):**
If `ConcreteSpec` (`C3`) makes declaration-order offsets and `i32` non-niche tags **observable invariants** of ordinary internal structs/enums, then Concrete permanently gives up automatic packing, niche optimization, hot/cold splitting, and AoS↔SoA unless a later Core revision reopens `C3`.

**Proposed `LayoutPolicy` distinction (for owner decision before Slice 6):**
- **Fixed-layout types (`LayoutPolicy.fixed`):** Any type annotated `#[repr(C)]`, `#[repr(transparent)]`, `ByteSafe` (`R-0714`), exported across FFI (`extern fn`, Slice 17), or materialized to raw bytes via `trusted`/`with(Unsafe)` uses a **single deterministic, spec-fixed layout algorithm** (exact offsets, exact `rep_roundtrip`, zero surprises).
- **Compiler-chosen internal types (`LayoutPolicy.free`):** Ordinary safe Concrete structs, enums, and fixed-size arrays whose address is never exposed to raw pointers or FFI have **abstract observational layout**: safe code can observe field values, equality, and `ByteSafe`/wire conversions, **never** internal field byte offsets or padding bytes.

Under this split, safe code remains 100% deterministic and auditable (and `concrete facts` still reports the exact concrete layout chosen by the compiler for every type!), while `Memory.lean` gains full legal freedom to synthesize optimal layouts.

---

## 2. Architecture: `LayoutPolicy` + `LayoutPlan` + Parametric Lowering

### Current Architecture on `main`
In [Concrete/Memory.lean](../../../Concrete/Memory.lean), layout is a single hardcoded query (`u8LayoutQuery`), tying the lowering proof to one fixed layout algorithm.

### Proposed 3-Part Architecture in `Concrete/Memory.lean`

```
               ┌──────────────────────────────┐
               │   Untrusted Layout Synthesizer│  (field sort, niche finder,
               │   (heuristics / PGO / search) │   hot/cold split, AoS↔SoA)
               └──────────────┬───────────────┘
                              │ produces `plan : LayoutPlan`
                              ▼
               ┌──────────────────────────────┐
               │  Decidable `LayoutPlan.valid` │  (proved in Lean: respects
               │   (fixed ABI preserved, all   │   LayoutPolicy.fixed, disjoint
               │    fields in-bounds, aligned, │   field ranges, valid tags/niches,
               │    bijective representation)  │   stack/size caps)
               └──────────────┬───────────────┘
                              │ `hValid : plan.valid policy = true`
                              ▼
               ┌──────────────────────────────┐
               │  `Memory.lower (plan)`        │  ONE parametric refinement proof:
               │  `∀ plan, plan.valid →`       │  works for EVERY valid plan,
               │  `Refines MIR (Memory plan)`  │  forever!
               └──────────────────────────────┘
```

#### What `LayoutPlan` Encodes
1. **Per-struct physical field map:** `(fieldId ↦ (bankId, byteOffset, align))` — supporting:
   - **Field reordering & padding elimination** (sorting fields by alignment / grouping small fields, single bank `0`).
   - **Hot/cold field splitting** (assigning hot fields to bank `0` and cold fields to bank `1` when accessed in separate phases).
2. **Per-enum tag & niche map:** `(variantId ↦ TagEncoding)` — supporting compact 1-byte tags and niche encodings (e.g., `Option<bool>`, `Option<NonZeroU64>`, `Option<Heap<T, A>>` at zero overhead) proved injective by `LayoutPlan.valid`.
3. **Per-array / collection storage strategy (`StorageShape`):**
   - `aos` (contiguous struct elements), or
   - `soa` (parallel field arrays: `[S; N]` lowered to `(Field₀[N], Field₁[N], ...)` whenever no whole-element raw address is taken and every access is indexed field projection `arr[i].f` or whole-element value copy/move).
4. **Destination-passing / copy-elision annotations (`SlotReuse`):**
   - Marks value-returning calls and struct literals that write directly into the caller's destination slot (generalizing `&out T` under the hood for internal calls without requiring user annotations).

---

## 3. Why the Proof Stays Small and Tractable

Proving each layout optimization ad hoc in Lean would be painful. **Parameterizing `Memory.lower` by `LayoutPlan`** turns all four optimizations into **instances of one single refinement theorem**:

- **Abstract state (`MIR`):** Each logical local/aggregate is a structured tree of typed values (fields, variants, array indices).
- **Concrete state (`Memory`):** A map of byte blocks `(blockId, offset) ↦ Byte`.
- **Simulation relation (`RefinesPlan plan`):** For every live logical value at logical path `p`, decoding the bytes at `plan.locate(p)` yields `MIR`'s logical value, and `plan.valid` guarantees **non-overlapping byte ranges for distinct logical paths** (`p₁ ≠ p₂ → Disjoint (plan.range p₁) (plan.range p₂)`).
- **Frame lemma (proved once):** Writing to `plan.range p₁` preserves `decode (plan.range p₂)` for every `p₂ ≠ p₁`.
  - Whether `plan` puts `a.x` before `a.y`, packs a discriminant into a niche bit, splits hot/cold fields into two blocks, or stripes `arr[i].x` into a separate SoA array, **the exact same non-overlap proof discharges the frame lemma!**

Because `LayoutPlan.valid` is a small decidable Boolean check in Lean, **any** future synthesizer (simple alignment sort in Slice 6, profile-guided hot/cold splitter, or MAP-Elites layout search in Phase E under `R-0727`) plugs in with **zero new lowering proofs**.

---

## 4. Auditability & Transparency (Philosophy Rules 3, 5, 14)

Does compiler-chosen layout violate Concrete's transparency philosophy? **No, as long as layout is visible and explicit at boundaries:**
- Every FFI, wire-format (`ByteSafe`), or `#[repr(C)]` type is `LayoutPolicy.fixed` and deterministic across compilers.
- `concrete facts` reports the **exact realized `LayoutPlan`** (size, alignment, field offsets, bank/SoA shape, niche encoding) for every type in the program, bound to the artifact digest.
- No hidden runtime representation: `LayoutPlan` is static, deterministic per build inputs, and inspectable in the audit dossier (`R-0201`).

---

## Slice Staging, Owner Questions & Acceptance Gates

| Increment | Slice | Deliverable | Gate |
|---|---|---|---|
| **0. Owner decision** | Pre-Slice 6 (`Batch 2`) | Decide `LayoutPolicy` split (`fixed` vs `free` internal structs/enums/arrays) revising `R-0502`/`R-0609` | Decision recorded in `docs/decisions.md` |
| **1. Parametric `Memory.lean` + packing + copy elision** | Slice 6 (structs & enums) | `LayoutPlan`, decidable `LayoutPlan.valid`, parametric `Memory.lower_correct`; automatic field reordering, compact/niche enum tags, destination-passing copy elision | Parametric refinement theorem (zero `sorry`), negative control rejecting an overlapping `LayoutPlan`, ABI-fixed control proving `#[repr(C)]` is never reordered |
| **2. Proved AoS↔SoA & hot/cold splitting** | Slices 7 & 13 (arrays / `HeapArray`) | Multi-bank / SoA `StorageShape` in `LayoutPlan` + disjointness check for indexed array/slice projections | Refinement proof + cross-backend differential match (`aos` vs `soa` identical observations) + benchmark evidence (`R-0424`) |

### Owner Questions Required Before Implementation
1. **Default layout freedom (`R-0502` / `R-0609`):** May ordinary safe structs/enums (not `#[repr(C)]`, not `ByteSafe`, not address-exposed to raw pointers) use compiler-chosen `LayoutPlan` (reordering, compact/niche tags, splitting, AoS↔SoA), while `#[repr(C)]` and `ByteSafe` remain spec-fixed?
2. **Determinism rule:** Should the default `LayoutPlan` synthesizer be strictly deterministic from source + target profile (preserving reproducible builds `R-0577`), with PGO/search-driven plans requiring an explicit checked plan file in build inputs? (Recommendation: **Yes** — keeps builds reproducible and auditable.)
