# Layout and ABI Contract Surface

Status: stable reference (Phase 3 item 75 closed)

This document is the stable-direction counterpart to the exploratory note at [../research/language/layout-contract-surface.md](../research/language/layout-contract-surface.md). It settles the question posed by ROADMAP item 75:

> settle the supported `repr` forms, when layout is guaranteed versus intentionally opaque, transparent-wrapper rules, field-order promises, and what reports must surface for reviewers and package boundaries.

For implementation details, see [ABI_LAYOUT.md](ABI_LAYOUT.md) and `Concrete/Layout.lean`.
For platform support and sizes, see [ABI.md](ABI.md).
For FFI rules, see [FFI.md](FFI.md).

---

## 1. Principle

**The promised layout surface is smaller than the implemented layout surface.** The compiler knows more about layout than the language guarantees. Anything not listed as guaranteed is opaque to user code and may change without notice.

This keeps audit cost, proof cost, and backend-coupling bounded, and lets the implementation evolve without invalidating downstream claims.

---

## 2. Supported `repr` Forms (stable first-release menu)

Exactly four annotations form the stable layout surface. No other `repr` forms are part of the first-release contract.

| Form | Target | Guarantee | Capability / restriction |
|---|---|---|---|
| *(none)* | struct, enum | **Opaque.** Compiler may change field order, padding, tag size, or discriminant. | — |
| `#[repr(C)]` | struct | **Guaranteed.** C-compatible in-memory layout: declaration field order, standard padding, alignment. | No type parameters. All fields must be FFI-safe. |
| `#[repr(packed)]` | struct | **Guaranteed.** Declaration field order, no padding between fields. | Must not contain types whose alignment exceeds the field position. |
| `#[repr(align(N))]` | struct | **Guaranteed.** Minimum alignment of `N` bytes, where `N` is a power of two. May be combined with `#[repr(C)]`. | `N` ≥ natural alignment of the struct. |

Any other annotation (`#[repr(transparent)]`, `#[repr(Rust)]`, integer-tagged enums, `u8`-repr enums, etc.) is **not** part of the first-release contract and is not accepted by the checker at freeze. See section 6 for reconsideration triggers.

---

## 3. When Layout Is Guaranteed Versus Opaque

### 3.1 Guaranteed

Guaranteed layout applies only to the four forms in section 2, on types that satisfy the listed restrictions. When a struct has a guaranteed layout, these facts are stable and FFI-safe to rely on:

- total `size`
- total `align`
- each field's `offset`
- field declaration order

These facts are the same across every supported target in [ABI.md](ABI.md#platform-assumptions) unless explicitly noted.

### 3.2 Opaque

Everything else is opaque. Specifically:

- **Unannotated structs.** Field order, padding, and offsets are implementation-defined and may change between compiler versions. Unannotated structs must not cross `extern fn` boundaries.
- **All enums** (user-defined or builtin: `Option`, `Result`). Tag size, tag position, payload packing, and discriminant assignment are implementation-defined. Enums must not cross `extern fn` boundaries.
- **Generic struct instantiations.** Even if the generic has all FFI-safe type arguments, layout is not guaranteed — `#[repr(C)]` rejects type parameters precisely to avoid this ambiguity.
- **Builtin compound types** (`String`, `Vec<T>`, `HashMap<K, V>`). Sizes are documented in [ABI.md](ABI.md) as an implementation reference, not as a stability promise. Do not pattern-match on layout through FFI.
- **Function pointer layout, vtable layout, trait object layout.** None exist in the stable surface; reserved for later design.

Opaque does not mean hidden — the compiler will still emit layout facts in reports (section 5). It means the facts are not promises the language is willing to defend across versions or across targets.

---

## 4. Transparent-Wrapper Rules

`#[repr(transparent)]` is **not** part of the first-release contract.

Rationale: a transparent wrapper commits the language to two coupled promises — (a) the wrapper's layout equals its sole inner field, and (b) ABI calls pass the wrapper exactly as the inner field. That second property is non-trivial on aggregates, and the project does not yet need it.

**Workaround for zero-cost newtypes.** The opaque validated wrapper work (roadmap item 72) achieves the same ergonomic goal without exposing a layout guarantee: the wrapper type is a single-field struct, the compiler lowers it to the field, but no stable promise is made about FFI-crossing behavior. Validated wrappers are for domain clarity, not for FFI layout.

Transparent-wrappers are revisited only if a medium-workload finding (item 67) shows a concrete FFI case the opaque-wrapper path cannot cover.

---

## 5. Field-Order Promises

Field order is a stable, visible property for **guaranteed** layouts only:

- `#[repr(C)]` and `#[repr(packed)]`: declaration order is the in-memory order. Reordering fields in source is a layout-breaking change.
- Unannotated structs: declaration order is **not** promised. The compiler may reorder fields for packing. Source-level reordering is not a layout-breaking change.

Reordering discipline for library authors:

- Adding, removing, or reordering fields on a `#[repr(C)]` struct is an ABI break. Record it in the module interface artifact (section 6) and treat it as a semver-major change for the module.
- The same operations on an unannotated struct are not layout breaks but may still be observable through derived traits or pattern-match completeness; handle under the normal API evolution rules.

---

## 6. Reports and Interface Artifacts

The compiler surfaces layout facts via the fact/report system so reviewers and package boundaries do not have to re-derive them. The stable set of emitted facts is:

| Fact | Emitted for | Purpose |
|---|---|---|
| `repr` annotation | every struct/enum | distinguishes guaranteed from opaque |
| `size`, `align` | every struct/enum | sanity and cross-check |
| field `offset` | guaranteed-layout structs only | FFI review |
| `ffi_safe: bool` | every type that appears on an `extern fn` boundary | trust-boundary review |
| `target_caveats` | types whose layout is target-sensitive | cross-target auditing |
| `stability: guaranteed \| opaque` | every emitted layout fact | reviewer can tell at a glance |

These facts flow into:

- `--report layout` human-readable summaries
- module/package interface artifacts (future: roadmap item 108) — stable fingerprint of publicly layout-relevant types
- diagnostics for FFI-safety violations and repr-combination errors

Opaque types still appear in reports, clearly marked. The signal is not "the compiler knows nothing" — it is "the language is not defending these facts."

---

## 7. Freeze Close-Out Status

Phase 3 closed on the contract-definition bar, not on "every future layout report feature already exists."

- The first-release layout menu in section 2 is the frozen surface: opaque-by-default plus `#[repr(C)]`, `#[repr(packed)]`, and `#[repr(align(N))]`.
- `#[repr(transparent)]` is intentionally outside that contract. Opaque validated wrappers are the frozen first-release answer for domain wrappers; if transparent FFI wrappers ever become necessary, that is an explicit unfreeze/revision event.
- `--report layout` already exists and is regression-tested, but the richer fact-tagging described in section 6 (`ffi_safe`, `target_caveats`, explicit `guaranteed|opaque` labels everywhere, package/interface fingerprints) is now follow-up artifact/report hardening, not a Phase 3 blocker.
- The FFI-heavy examples and pressure programs are sufficient to freeze the promised surface. Additional report enrichment and stricter checker gates belong to later artifact/contract-hardening phases.

In short: the contract is frozen; remaining work in this area is implementation/report polish, not surface-definition ambiguity.

---

## 8. Reconsideration Triggers

The section-2 menu is the frozen surface. It is reconsidered only on explicit evidence:

- A medium-workload finding (item 67) that needs a layout form not in section 2 and cannot be expressed via the existing four.
- A validated-wrapper workload (item 72) that crosses FFI in a way opaque newtypes cannot express.
- A concrete cross-target portability bug that the current target caveats do not describe.

Any such finding is recorded in [STDLIB.md](STDLIB.md) gap ledger and triggers a scoped design revision. Growth is by evidence, not by precedent from other languages.
