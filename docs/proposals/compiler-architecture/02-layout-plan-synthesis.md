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

**Codex's recommended split (for owner decision before Slice 6):**
1. **Keep the current predictable default layouts (`R-0502`/`R-0609`):** Ordinary types keep their simple, deterministic default layout at public, ABI, `#[repr(C)]`, `ByteSafe` (`R-0714`), FFI, and raw-memory boundaries—no silent layout surprises.
2. **Allow private optimized layouts ONLY with an exact conversion/refinement proof:** Internal, non-escaping, non-ABI-exposed aggregates and arrays may adopt an optimized `LayoutPlan` inside `Memory` *only* when accompanied by a kernel-checked proof that the private representation converts back exactly to the spec's logical value model (`decode ∘ encode = id` / representation refinement), leaving all observable behavior unchanged.
3. **Decide compact / niche enum tags separately:** Compact 1-byte enum tags and niche encodings (e.g. `Option<bool>`, `Option<Heap<T, A>>`) touch `R-0609`'s explicit "`i32` enum tags, no niches" rule and must be decided by the owner separately from private struct/array layout optimization.

Under this split, predictable default layouts stay intact, `concrete facts` reports the exact realized `LayoutPlan` for every type, and private layout optimization stays strictly proof-gated (and comes at Slice 6+, **not** in the Stage-0 cutover).

---

## 2. Architecture: `LayoutPolicy` + `LayoutPlan` + Parametric Lowering

### Current Architecture on `main`
In the Slice-0 prototype ([Concrete/Memory.lean](../../../Concrete/Memory.lean)), layout is a single hardcoded query (`u8LayoutQuery`), while [Concrete/Interfaces/Carriers.lean](../../../Concrete/Interfaces/Carriers.lean) defines the slice-indexed `Memory lang profile` carrier (`MemoryPlace`, `MemoryInst`, `StorageClass`, `MemoryState`). Layout search is intentionally deferred until Slice 6 (structs/enums) rather than complicating Slice 0.

### Proposed 3-Part Architecture in `Memory` (Slice 6+)

```
               ┌──────────────────────────────┐
               │   Untrusted Layout Synthesizer│  (field sort, niche finder,
               │   (heuristics / PGO / search) │   hot/cold split, AoS↔SoA)
               └──────────────┬───────────────┘
                              │ produces `plan : LayoutPlan`
                              ▼
               ┌──────────────────────────────┐
               │  Decidable `LayoutPlan.valid` │  (proved in Lean: respects
               │   fixed/default ABI, sibling  │   LayoutPolicy, spatial/
               │   disjointness, injective     │   temporal/encoding validity,
               │   tag/niche codec, size caps) │   exact conversion to spec)
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
1. **Per-struct private field map:** `(fieldId ↦ (bankId, byteOffset, align))` — supporting:
   - **Field reordering & padding elimination** (sorting fields by alignment / grouping small fields, single bank `0`).
   - **Hot/cold field splitting** (assigning hot fields to bank `0` and cold fields to bank `1` when accessed in separate phases).
2. **Per-enum tag & niche map (separately owner-gated):** `(variantId ↦ TagEncoding)` — supporting compact tags and niche encodings only if approved under `R-0609`.
3. **Per-array / collection storage strategy (`StorageShape`):**
   - `aos` (contiguous struct elements), or
   - `soa` (parallel field arrays: `[S; N]` lowered to `(Field₀[N], Field₁[N], ...)` whenever no whole-element raw address is taken and every access is indexed field projection `arr[i].f` or whole-element value copy/move).
4. **Destination-passing / copy-elision annotations (`SlotReuse`):**
   - Marks value-returning calls and struct literals that write directly into the caller's destination slot across disjoint lifetimes.

---

## 3. Why the Proof Stays Small — Without a False "All Paths Disjoint" Lemma

A naive "all distinct paths have disjoint byte ranges" lemma (`p₁ ≠ p₂ → Disjoint (plan.range p₁) (plan.range p₂)`) is **mathematically false** for three distinct reasons:
1. **Parent/child overlap:** A struct path `p` necessarily **overlaps its own field paths** `p.f`.
2. **Tag/niche bit-sharing:** Niche-encoded enum tags **share bits/bytes with the payload** (they are disjoint only in the *value domain*, not as raw byte ranges).
3. **Temporal slot reuse:** Destination-passing and `SlotReuse` intentionally **share the exact same bytes** across sequentially disjoint lifetimes.

Therefore, `LayoutPlan.valid` and the parametric refinement theorem separate the three invariants cleanly:

1. **Spatial disjointness of incomparable sibling paths (within one live value):**
   - Only **incomparable (non-prefix)** field/element paths inside the same live aggregate (`¬ Prefix p₁ p₂ ∧ ¬ Prefix p₂ p₁`) occupy disjoint byte ranges (`Disjoint (plan.range p₁) (plan.range p₂)`). A parent path's range is the union of its children's ranges (plus any tag/padding).
   - This discharges the **spatial frame lemma** for field writes (`p.f₁ := v` leaves `p.f₂` untouched whenever `f₁ ≠ f₂`, whether laid out as reordered AoS, hot/cold banks, or SoA arrays).
2. **Exact value-representation round-trip (for tags & niches):**
   - Rather than claiming disjoint byte ranges for niche tags, each aggregate/enum `LayoutPlan` provides an explicit `encode`/`decode` pair proved injective on valid spec values (`∀ v, ValidVal v → decode (encode v) = some v`), and proves variant encodings have disjoint encoded bit-pattern images even when sharing bytes with payload fields.
3. **Temporal non-overlap (for `SlotReuse` / destination passing):**
   - Two distinct logical locals share a physical slot `(blockId, offset)` **only** when the MIR liveness/linearity analysis proves their live intervals do not overlap (the old linear owner is dead/consumed before the new write begins).

Because `LayoutPlan.valid` checks these three separate conditions decidably in Lean, **any** future layout synthesizer (simple alignment sort in Slice 6, hot/cold splitter, or MAP-Elites layout search in Phase E under `R-0727`) plugs into the same parametric `Memory.lower_correct` proof with **zero new lowering proofs**.

---

## 4. Auditability & Transparency (Philosophy Rules 3, 5, 14)

Does private optimized layout violate Concrete's transparency philosophy? **No, as long as boundaries stay explicit and predictable:**
- Predictable default layouts (`R-0502`/`R-0609`) remain unchanged, and every FFI, wire-format (`ByteSafe`), or `#[repr(C)]` type is strictly `LayoutPolicy.fixed`.
- Private optimized layouts are admitted **only** with a kernel-checked conversion/refinement proof back to the spec's value model.
- `concrete facts` reports the **exact realized `LayoutPlan`** (size, alignment, field offsets, bank/SoA shape, tag encoding) for every type in the program, bound to the artifact digest.

---

## Slice Staging, Owner Questions & Acceptance Gates

| Increment | Slice | Deliverable | Gate |
|---|---|---|---|
| **0. Owner decisions** | Pre-Slice 6 (`Batch 2`) | (a) Keep predictable defaults while allowing private optimized layouts backed by exact conversion proofs; (b) separately decide compact/niche enum tags (`R-0609`) | Decisions recorded in `docs/decisions.md` |
| **1. Parametric `Memory` + private packing + copy elision** | Slice 6 (structs & enums) | `LayoutPlan`, decidable 3-part `LayoutPlan.valid` (incomparable-sibling spatial disjointness, injective tag/niche codec, temporal slot-reuse non-overlap), parametric `Memory.lower_correct` | Parametric refinement theorem (zero `sorry`), negative controls rejecting (a) overlapping sibling fields, (b) colliding niche encodings, (c) live-overlap slot reuse, and (d) mutated `#[repr(C)]`/default-boundary layout |
| **2. Proved AoS↔SoA & hot/cold splitting** | Slices 7 & 13 (arrays / `HeapArray`) | Multi-bank / SoA `StorageShape` in `LayoutPlan` + incomparable-path disjointness for indexed array/slice projections | Refinement proof + cross-backend differential match (`aos` vs `soa` identical observations) + benchmark evidence (`R-0424`) |

### Owner Questions Required Before Implementation
1. **Private optimized layouts (`R-0502` / `R-0609`):** Keep the current predictable default layouts, and allow private optimized layouts (reordering, splitting, copy elision, AoS↔SoA) for non-ABI/non-`ByteSafe`/non-raw-exposed internal types **only** with a kernel-checked proof they convert back exactly to the spec's value model?
2. **Compact / niche enum tags (separate decision):** Independently of private struct/array layouts, should `R-0609`'s "`i32` enum tags, no niches" rule be relaxed for non-`#[repr(C)]` enums (e.g. smallest tag width and/or proved niche encodings)?
3. **Determinism rule:** Should the default `LayoutPlan` synthesizer be strictly deterministic from source + target profile (preserving reproducible builds `R-0577`), with search/PGO-driven plans requiring an explicit checked plan file in build inputs? (Recommendation: **Yes**.)
