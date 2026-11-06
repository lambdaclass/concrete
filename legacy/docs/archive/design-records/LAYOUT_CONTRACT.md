# Layout and ABI Contract Surface (design record)

Archived from docs/platform on 2026-10-30: the layout contract (the `repr` menu, guaranteed versus
opaque layout, the stride fact, `Option` and `Result` identity, the reports). **The specification
governs.** Its rules are in [docs/spec/ffi.md](../../spec/ffi.md): unspecified layout and the
`sizeof` / `alignof` window (FFI-7), what `repr(C)` and `repr(packed)` promise (FFI-8), the four
layout forms, with `transparent` and any `repr` on an enum refused (FFI-10), `repr(C)` structs by
value in extern calls (FFI-11, pending R-0502), the stride of a buffer (FFI-12), the one `Layout`
authority (FFI-2) and the well-formedness of the attributes (FFI-4). The builtin `Option` and
`Result` are TYPE-23 in [docs/spec/types.md](../../spec/types.md). Where this record and the
specification differ, the specification governs. This record keeps the reasoning, the report
surface (section 6) and the history.

Open questions it raised: whether an enum crosses the foreign boundary (OPEN-110; §3.2 and §3.3
said both no and "as integers"), misaligned fields of a packed struct (OPEN-111; §2), a user enum
named `Result` (OPEN-43; §3.4 decided it is a distinct type, main withholds the builtin from such a
module) and returned `repr(C)` structs (OPEN-122), all in
[docs/spec/decisions.md](../../spec/decisions.md).

For implementation details, see [ABI_LAYOUT.md](../../platform/ABI_LAYOUT.md) and
`Concrete/Check/Layout.lean`; for platform support, [ABI.md](../../platform/ABI.md); for FFI,
[FFI.md](../../platform/FFI.md).

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
| `#[repr(packed)]` | struct | **Guaranteed.** Declaration field order, no padding between fields. | Whether a field may sit at an offset below its alignment is open ([OPEN-111](../../spec/decisions.md#OPEN-111)); main does not check it. |
| `#[repr(align(N))]` | struct | **Guaranteed.** Minimum alignment of `N` bytes, where `N` is a power of two. May be combined with `#[repr(C)]`. Nothing about field order or offsets. | `N` a power of two (E0577). Today `N` may not exceed the struct's natural alignment (E0585, bug 037: over-aligned declarations are not yet emitted), so the attribute is accepted only where it is a no-op. |

Any other annotation (`#[repr(transparent)]`, `#[repr(Rust)]`, integer-tagged enums, `u8`-repr enums, etc.) is **not** part of the first-release contract and is not accepted by the checker at freeze. See section 6 for reconsideration triggers.

---

## 3. When Layout Is Guaranteed Versus Opaque

### 3.1 Guaranteed

Guaranteed layout applies only to the four forms in section 2, on types that satisfy the listed restrictions. When a struct has a guaranteed layout, these facts are stable and FFI-safe to rely on:

- total `size`
- total `align`
- each field's `offset`
- field declaration order

These facts are the same across every supported target in [ABI.md](../../platform/ABI.md#platform-assumptions) unless explicitly noted.

### 3.2 Opaque

Everything else is opaque. Specifically:

- **Unannotated structs.** Field order, padding, and offsets are implementation-defined and may change between compiler versions. Unannotated structs must not cross `extern fn` boundaries.
- **All enums** (user-defined or builtin: `Option`, `Result`). Tag size, tag position, payload packing, and discriminant assignment are implementation-defined. Enums must not cross `extern fn` boundaries.
- **Generic struct instantiations.** Even if the generic has all FFI-safe type arguments, layout is not guaranteed — `#[repr(C)]` rejects type parameters precisely to avoid this ambiguity.
- **Builtin compound types** (`String`, `Vec<T>`, `HashMap<K, V>`). Sizes are documented in [ABI.md](../../platform/ABI.md) as an implementation reference, not as a stability promise. Do not pattern-match on layout through FFI.
- **Function pointer layout, vtable layout, trait object layout.** None exist in the stable surface; reserved for later design.

Opaque does not mean hidden — the compiler will still emit layout facts in reports (section 5). It means the facts are not promises the language is willing to defend across versions or across targets.

### 3.3 What code may rely on, and who owns layout (decided 2026-10-21)

The language rules are FFI-7, FFI-8 and FFI-12 of [the specification's FFI chapter](../../spec/ffi.md#FFI-7).

**Rule.** Each `repr` form guarantees exactly the properties section 2 lists for it and nothing
else: `#[repr(C)]` C field order, padding and alignment; `#[repr(packed)]` declaration order with no
padding; `#[repr(align(N))]` a minimum alignment, with no promise about order or offsets. Every other
layout fact of every other type is unspecified. Safe code observes layout only through
`sizeof::<T>()` and `alignof::<T>()`. Trusted code that does pointer arithmetic over elements may
rely on `sizeof` and on one further fact, which the layout authority states and keeps: the stride of
an array, `HeapArray` or raw buffer of `T` is `sizeof::<T>()`. `#[repr(C)]` on an enum is rejected,
and whether enums cross the foreign boundary at all (as integers, or not, as section 3.2 says) is
the open owner decision [OPEN-110](../../spec/decisions.md#OPEN-110); main refuses it.

**One authority.** One `Layout` module computes every size, alignment, offset, stride and emitted
type for every consumer (the checker, `Lower`, the emitter, and every model that states a layout
fact); no consumer keeps its own copy of a layout fact (structural class 12).

**Status on main.** The `sizeof` / `alignof` surface is the only layout observation in std,
examples and tests today (std 25 lines in 3 files, examples 4 in 3, tests 23 in 13; no source
migration). One authority is **partly** true: `Concrete/Check/Layout.lean` owns sizes, alignments
and offsets, and since bug 121 the emitted enum types carry exactly its size and alignment
(`check_enum_union_layout.sh` leg 5), but the builtin `Option` / `Result` union is still computed
once program-wide by `EmitSSA.scanBuiltinEnumArgs` instead of per instantiation (bug 122, section
3.4). The stride fact is implemented but not yet stated or gated by the layout module.

**Prevents.** Bugs 121 and 122 and their class (037, 057: one fact in two places), and the enum
field-layout panics of bugs 005 and 035.

**Owner.** R-0502 (one `Layout`, queue row 4's Layout checkpoint).

### 3.4 `Option` and `Result` have no layout of their own (normative, decided 2026-10-21; staged)

**Rule.** `Option` and `Result` have ordinary enum semantics. They are laid out per instantiation,
like every generic enum since R-0001, and the compiler refers to them only by resolved identity (a
definition id or lang item), never by spelling, so a user enum named `Result` is a different type
with its own layout. Defining both in `std` source, with intrinsics that return them constructing
the resolved `std` type or wrapped by `std` functions, is the preferred implementation; limited
compiler recognition by resolved identity is the fallback. Spelling in user code does not change.

**Status on main: not enforced.** Both are compiler-builtin (`Concrete/Resolve/BuiltinEnums.lean`,
`BuiltinEnumId`); `EmitSSA.scanBuiltinEnumArgs` builds one canonical LLVM union per builtin for the
whole program, which disagrees with the per-instantiation `Layout.tySize` and gives wrong field
offsets and a stack overrun, including when a user enum is named `Result` (bug 122). Sixteen corpus
files (1 example, 15 tests) define their own `Option` or `Result` enum and must stay distinct types.

**Stages.** (1) Now, as a row-0 repair (ROADMAP exit 0b, R-0482): bug 122's per-instantiation
layout, not deferred to the full Layout migration. (2) Identity by resolved definition and the
`std` definitions ride R-0492 (`DefId`) and R-0514; the layout half is R-0502.

**Prevents.** Bug 122 (both halves) and the builtin/user join by spelled name (structural class 1).

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

Any such finding is recorded in [STDLIB.md](../../stdlib/STDLIB.md) gap ledger and triggers a scoped design revision. Growth is by evidence, not by precedent from other languages.
