**PROPOSED RESEARCH & DESIGN NOTE (2026-11-27); decides no language rule. Maps to provisional task [R-0731](../../../ROADMAP.md#r-0731) (extending `R-0580`, `R-0502`, `R-0609`). Owner decision required before changing default struct layout.**

# 02 — Parametric `LayoutPlan` Synthesis and Proved Data-Layout Refinement

## Problem Statement

Field order, padding, copy elision and array layout can change cache traffic and the number of bytes moved. Measure their effect on the target workload.

Yet C, C++, and even Rust compilers almost never perform whole-aggregate or whole-collection layout synthesis automatically:
- In C/C++, `offsetof`, pointer arithmetic, `void*` casts, and escaping field pointers bake physical memory layout into program semantics.
- In Rust, although `#[repr(Rust)]` allows field reordering and niche optimization, references (`&T`, `&mut T`) can be stored in structs, passed across arbitrary lifetimes, and cast to raw pointers (`&x.f as *const _`), making whole-collection transforms (AoS↔SoA, hot/cold splitting) practically impossible without whole-program escape analysis.

Concrete has a rare structural advantage:
1. **Strictly second-class references:** No `&T` or `&mut T` can ever be stored in a struct/enum/collection or returned from a function. Field borrows exist only lexically inside scoped calls or `borrow` blocks.
2. **Explicit raw-memory boundary:** Raw pointers (`*T`, `*mut T`) exist only inside `trusted` / `with(Unsafe)` boundaries (`C4` materialization rule in `R-0610`), and proposed wire markers such as `ByteSafe` remain subject to R-0714 approval.
3. **One materialized layout query.** The spec defines materialized layout and Memory consumes its query. Memory classifies virtual and materialized storage. [Indexed lowering](../../../Concrete/Memory/Lowering.lean).

---

## 1. The Spec Boundary Question (Before Slice 6, Ahead of the Slice 11 Core Freeze)

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
The scalar prototype uses `u8LayoutQuery`. Indexed Memory lowering queries `language.layout mir.declarations profile.memoryTarget ty`. `LayoutsMatch` ties the recorded layouts to that query. The handoff checks layout geometry for the physical target. `Layout.project` resolves a field path inside the root's single materialized range. The early language returns scalar layout trees, while the aggregate control exercises a materialized field at a declared offset. Multiple banks per root would require an extension. [Lowering](../../../Concrete/Memory/Lowering.lean) · [Contracts](../../../Concrete/Interfaces/Contracts.lean) · [Stage interface](../../../ConcreteSpec/StageInterface.lean) · [Aggregate layout control](../../../Concrete/Memory/AggregateLayoutControl.lean).

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
               │ Parametric LowersMemory      │
               │ for checked plan constructors│
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
4. **Destination passing (`SlotReuse`):** A separate per-function MIR/Memory frame plan proposes a destination for each eligible call or struct construction.

---

## 3. Why the Proof Stays Small — Without a False "All Paths Disjoint" Lemma

A naive "all distinct paths have disjoint byte ranges" lemma (`p₁ ≠ p₂ → Disjoint (plan.range p₁) (plan.range p₂)`) is **mathematically false** for three distinct reasons:
1. **Parent/child overlap:** A struct path `p` necessarily **overlaps its own field paths** `p.f`.
2. **Tag/niche bit-sharing:** Niche-encoded enum tags **share bits/bytes with the payload** (they are disjoint only in the *value domain*, not as raw byte ranges).
3. **Temporal slot reuse:** Destination-passing and `SlotReuse` intentionally **share the exact same bytes** across sequentially disjoint lifetimes.

Check independent, simultaneously live footprints across roots and banks, value round-trips for each active encoding, and non-overlap of reused storage across every live activation and borrow.

1. **Spatial disjointness.** Check co-active incomparable sibling field/element paths within the same struct, array or active enum variant. Distinct variants may reuse payload storage. A write must preserve all other independent live storage, including caller storage in other roots or banks.
2. **Exact representation.** Prove that each active encoding decodes to its value and that different variants have distinguishable encodings. Rematerialize canonical bytes at observable boundaries and agree on the ABI of a non-inlined `&[S]` across modules.
3. **Temporal reuse.** The per-function frame check must show that a destination aliases no argument read by the callee. Account for nested activations, active borrows and partial writes on a trap or error path.

Prove initialized, aligned, in-bounds reads, writes and moves, preservation of other live storage, allocation behavior and rematerialization of canonical bytes at observable boundaries. Model exhaustion/OOM and C7 accounting per lineage when a multi-bank plan changes the allocation count or alignment slack. Establish `LowersMemory`, including `BehaviorRefinement` and `Memory.RepresentsCheckpoint`, for each supported plan constructor. The current contract matches ordinary results and permits physical `resourceExhausted`. Strict matching of every terminal result requires a bound ruling out reachable exhaustion (`LowersMemory.refines_of_bound`). A new heuristic can reuse the proof for existing plan constructors. Each new representation or reuse constructor needs a soundness case. [Memory refinement](../../../Concrete/Memory/Refinement.lean).

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
| **1. Parametric `Memory` + private packing + copy elision** | Slice 6, subject to approval | Checked plan and per-function frame plan with parametric `LowersMemory` | Prove the access, frame, allocation and canonical-boundary obligations above. Reject a write into live caller storage, a tag collision, reuse across a live borrow or trap, and changed observable ABI bytes. |
| **2. Proved AoS↔SoA & hot/cold splitting** | Slices 7 & 13 (arrays / `HeapArray`) | Multi-bank / SoA `StorageShape` in `LayoutPlan` + incomparable-path disjointness for indexed array/slice projections | Refinement proof + cross-backend differential match (`aos` vs `soa` identical observations) + benchmark evidence (`R-0424`) |

### Owner Questions Required Before Implementation
1. **Private optimized layouts (`R-0502` / `R-0609`):** Keep the current predictable default layouts, and allow private optimized layouts (reordering, splitting, copy elision, AoS↔SoA) for non-ABI/non-`ByteSafe`/non-raw-exposed internal types **only** with a kernel-checked proof they convert back exactly to the spec's value model?
2. **Compact / niche enum tags (separate decision):** Independently of private struct/array layouts, should `R-0609`'s "`i32` enum tags, no niches" rule be relaxed for non-`#[repr(C)]` enums (e.g. smallest tag width and/or proved niche encodings)?
3. **Determinism rule:** Should the default `LayoutPlan` synthesizer be strictly deterministic from source + target profile (preserving reproducible builds `R-0577`), with search/PGO-driven plans requiring an explicit checked plan file in build inputs? (Recommendation: **Yes**.)
