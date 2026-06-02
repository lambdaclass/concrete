# Opaque Validated Wrappers and Fallible Conversions

Status: stable reference (Phase 3 item 72 closed)

This document is the stable-direction counterpart to [../research/language/opaque-validated-types.md](../research/language/opaque-validated-types.md). It settles the question posed by ROADMAP item 72:

> add opaque/newtype validated wrapper types and canonical fallible conversion conventions: zero-cost domain wrappers, smart constructors, checked narrowing/parsing, and explicit `try_from`-style paths should make validated values easy to carry without implicit coercions or ambient convention.

For the underlying layout story, see [LAYOUT_CONTRACT.md](LAYOUT_CONTRACT.md).
For the broader stdlib direction, see [STDLIB.md](STDLIB.md).

---

## 1. What Is Already Shipped

Concrete already has the language mechanism. This document's job is to settle the **convention** around it, not the syntax.

### 1.1 Newtype declaration

```concrete
newtype UserId = Int;
newtype Wrapper<T> = T;
```

- One-field wrapper; the inner type is held positionally as field `.0`.
- Generics work: `newtype Wrapper<T> = T;`.
- Inner linearity is preserved: a `newtype` over a linear type is itself linear.
- Zero-cost: lowers to the inner type in SSA; no runtime indirection.

### 1.2 Construction

```concrete
let id: UserId = UserId(42);
```

- `TypeName(value)` is the constructor form. No other construction path; no implicit coercion from the inner type.

### 1.3 Inner access

```concrete
let raw: Int = id.0;
```

- `.0` is the only inner-access path in idiomatic code.
- `unwrap(id)` exists as an intrinsic fallback but is not the recommended user-facing path; prefer `.0`.

### 1.4 Non-coercion

```concrete
fn takes_userid(id: UserId) { ... }
takes_userid(42); // ERROR: Int is not UserId
```

- Passing a raw `Int` where `UserId` is expected is a compile error. Construction must be explicit.

Tests covering the above: `tests/programs/newtype_basic.con`, `newtype_generic.con`, `newtype_linear.con`, `newtype_copy.con`, `error_newtype_no_implicit.con`, `error_newtype_wrong_inner.con`, `error_newtype_double_unwrap.con`, `adversarial_newtype_consume.con`.

---

## 2. The Convention This Document Fixes

The language feature is settled. What remains is the stable convention for **how validated wrappers are built and converted**, so that stdlib, examples, and user code do not drift.

### 2.1 Naming

| Role | Convention | Example |
|---|---|---|
| Wrapper type | `PascalCase`, domain name | `UserId`, `Port`, `PacketLen`, `NonZeroU32`, `AsciiText` |
| Smart constructor | `TypeName::try_new(inner) -> Option<Self>` | `Port::try_new(value)` |
| Total constructor (when validation is free/trivial) | `TypeName(inner)` (raw) | `UserId(42)` |
| Inner access | `.0` | `port.0` |
| Raw construction bypassing validation | `trusted` context, `TypeName(inner)` | only inside the module that owns the wrapper |

If a wrapper requires validation, the plain `TypeName(inner)` constructor should be kept private to the defining module, and `try_new` becomes the public entry point. This is the difference between a "domain tag" (no validation: `UserId = Int` — the value is fine, the tag is meaning) and a "validated wrapper" (`Port = u16` — not every u16 is a valid port in context).

### 2.2 Fallible conversions

When converting from a wider type to a narrower one, use one of these shapes — and only these:

| Conversion kind | Shape | Returns |
|---|---|---|
| Checked narrowing from primitive | `Type::try_from_<src>(x: Src) -> Option<Type>` | `Option<Type>` |
| Checked narrowing that needs an error reason | `Type::try_from_<src>(x: Src) -> Result<Type, Type::Error>` | `Result<Type, E>` |
| Parse-from-string | `Type::try_parse(s: &String) -> Result<Type, ParseError>` | `Result<Type, ParseError>` |
| Infallible widening (wrapper → inner) | `.0` | `Inner` |
| Infallible wrapping (only when the domain logically forbids a failure) | `Type::from_<src>(x: Src) -> Type` | `Type` |

Rules:

- **Never** expose an infallible `from_*` when validation is required. Use `try_new` / `try_from_*` and force the caller to handle `Option`/`Result`.
- **Never** add implicit `From`/`Into` machinery. Conversions are visible by name; there is no trait-driven coercion.
- **Every** fallible path returns `Option<T>` or `Result<T, E>`. Panicking narrowing is not part of the convention. (A `trusted` module may internally skip the check, but the public surface does not.)

### 2.3 Validation sites

Validation runs at exactly one point — the constructor:

```concrete
newtype Port = u16;

impl Port {
    pub fn try_new(value: u16) -> Option<Port> {
        if value == 0 {
            return Option::<Port>::None;
        }
        return Option::<Port>::Some { value: Port(value) };
    }

    pub fn value(&self) -> u16 {
        return self.0;
    }
}
```

After construction, the wrapper is trusted: holders can call `.0` directly without re-checking. The validation story is "validate once at the edge, trust after."

### 2.4 Error types

Validated wrappers that need a reason use a module-local error enum:

```concrete
enum PortError { Zero, OutOfRange }

impl Port {
    pub fn try_from_u32(value: u32) -> Result<Port, PortError> {
        if value == 0 {
            return Result::<Port, PortError>::Err { error: PortError::Zero };
        }
        if value > 65535 {
            return Result::<Port, PortError>::Err { error: PortError::OutOfRange };
        }
        return Result::<Port, PortError>::Ok { value: Port(value as u16) };
    }
}
```

Module-local error enums are preferred over shared vocabularies so wrappers stay composable without importing a grab-bag error type.

---

## 3. Interaction with Layout

Validated wrappers do **not** make layout guarantees. See [LAYOUT_CONTRACT.md](LAYOUT_CONTRACT.md) section 4.

- A validated wrapper is not `#[repr(transparent)]`. It is not guaranteed to cross FFI boundaries as its inner type.
- For FFI values that also need domain meaning, the recommended shape is: extern boundary uses the raw inner type, and the first thing the callee does is `Type::try_from_*`.

This keeps the validated-wrapper surface strictly about domain clarity, not ABI.

---

## 4. Canonical First-Release Wrappers

The first stdlib release ships the following validated wrappers as built-in examples of the convention:

| Wrapper | Inner | Validation |
|---|---|---|
| `NonZeroU32` | `u32` | `value != 0` |
| `NonZeroU64` | `u64` | `value != 0` |
| `AsciiText` | `String` | all bytes in `0..=127` |
| `Port` | `u16` | `value != 0` |

These live in `std.numeric` / `std.text`. They exist primarily as **proof of convention**: each demonstrates the `try_new` / `.0` / `try_from_*` shape, so user code has a working reference.

The list is small on purpose. Growth is evidence-driven — a new wrapper lands in stdlib only if two unrelated examples independently need the exact same validation story.

---

## 5. Anti-Patterns

Call-site conventions the stable surface rejects:

- **Ambient `From`/`Into` coercion.** No such trait is part of the stable surface. All conversions are named functions.
- **Panic on invalid.** A constructor that aborts on invalid input is not a validated wrapper — it is a trusted cast. Trusted casts live behind `trusted fn` and do not pretend to validate.
- **Silent re-validation.** Methods on a validated wrapper trust the invariant; they do not re-check. The compiler has no way to enforce this, but it is a discipline invariant.
- **Unwrap-as-conversion.** `.0` exists for reading the inner value. Using `.0` to move a wrapper into an API expecting raw `Inner` is valid, but it is a deliberate downgrade and should be commented or rare in reviewed code.

---

## 6. Freeze Close-Out Status

The validated-wrapper surface is accepted as freeze-ready on the current evidence:

- [x] `NonZeroU32`, `NonZeroU64`, `Port`, `AsciiText` exist in stdlib with the shapes in section 4.
- [x] Each ships tests that cover `try_new` happy path, `try_new` rejection, `.0` extraction, and absence of implicit coercion.
- [x] No stdlib validated wrapper exposes an infallible `from_*` where validation is required.
- [x] This document and [STDLIB.md](STDLIB.md) cross-reference each other on the validated-wrapper section.
- [x] The compiler/runtime contract is closed and covered by end-to-end regressions: native/SSA layout on enum-payload newtypes, cross-module newtype identity, instance-method dispatch on wrappers, and narrowed cast exemption for wrap/unwrap-only representation casts.

A medium-workload example using a stdlib validated wrapper end-to-end would still be useful follow-up evidence under item 67, but it is not a freeze blocker now that the design and compiler gaps are closed.

---

## 7. Reconsideration Triggers

The convention in section 2 is the stable direction. It is reconsidered only on explicit evidence:

- A medium-workload finding that cannot express a validation via the `try_new` / `try_from_*` shapes and requires a new conversion form.
- A concrete FFI case where `#[repr(transparent)]` would materially simplify interop and no other path works. That triggers a scoped layout-contract revision (item 75), not a validated-wrapper revision.
- A cross-wrapper composition pattern (e.g., `Port × UserId → SessionKey`) that proves recurrent. Composed wrappers are left to user code until two unrelated examples justify a stdlib helper.

Until such evidence lands, the convention here is frozen.

---

## 8. Known Implementation Gaps

No known compiler-level gaps remain against the convention. Each was tracked, fixed, and is now covered by regressions.

**Resolved (2026-04-24):** native/SSA layout on enum-payload newtypes. `Layout.tySize`/`Layout.tyAlign`/`isPassByPtr`/`tyToLLVM` previously panicked on newtypes that survived to codegen (e.g. `Option<Port>`). `Layout.Ctx` now carries a newtypes list; the size/alignment/pass-by-ptr/LLVM-type paths resolve named/generic types through `resolveNewtype` before querying primitive layout, threaded through `CModule` → `SModule` → `EmitSSA`/`Lower`/`CoreCheck`/`Report`. Native and interp agree on enum-payload newtypes.

**Resolved (2026-04-25):** cross-module newtype identity. Pub newtypes are importable as types, imported function signatures preserve newtype identity (no inner-type erasure at the boundary), and inherent impl methods come along with the import. Demos: `tests/programs/adversarial_module_newtype_across.con`, `tests/programs/summary_import_pub_newtype.con`.

**Resolved (2026-04-25):** instance-method dispatch on newtypes. `p.value()` where `p: Port` now resolves against `Port`'s inherent impl; previously the type was erased to `u16` in elaboration so dispatch landed on the inner type. `resolveTypeE` no longer erases newtypes; the newtype constructor and `.0` field access carry the wrapper name through as a representation no-op cast (Layout sees the same LLVM type either way). Demo: `tests/programs/newtype_method_dispatch.con`.

**Resolved (2026-04-25):** narrow cast exemption — guards the wrapper contract from `as`-bypass. The first cut of the dispatch fix exempted *any* cast where either side named a newtype, which made `p as bool` and `b as Port` compile and bypass the smart-constructor contract in §2. The exemption is now precise: a cast is allowed past the validity table only when one side is a newtype `N` and the other side equals `N`'s resolved inner type (after generic substitution) — i.e., exactly the wrap/unwrap representation cast that Elab inserts. The previously-overbroad `hasTypeVar` check (which treated every `.named _` as a type parameter) is also tightened to recognise concrete user-defined struct/enum/newtype names. Negative regressions: `tests/programs/error_newtype_cast_to_unrelated.con`, `tests/programs/error_newtype_cast_from_unrelated.con`.
