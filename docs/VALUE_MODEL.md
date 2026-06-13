# Value & Reference Model

Status: stable reference

Concrete uses a strict value/reference model with linear types by default.

For the full safety model (capabilities, trusted boundaries, `Unsafe`), see [SAFETY.md](SAFETY.md).
For the current safe-memory guarantee boundary, see [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md).
For FFI and trust boundaries, see [FFI.md](FFI.md).

## Value Categories

| Type | Semantics | Default | Notes |
|------|-----------|---------|-------|
| Primitives (`Int`, `i32`, `bool`, etc.) | Value, Copy | Copy | Always passed by value |
| `&T` | Shared reference | Copy | Immutable borrow |
| `&mut T` | Exclusive reference | Linear | Mutable borrow, consumed on use |
| `*mut T` / `*const T` | Raw pointer | Copy | Requires `Unsafe` capability |
| `Heap<T>` | Owned heap pointer | Linear | `->` field access, must be freed |
| Structs | Value | Linear | Opt-in `Copy` via marker |
| Enums | Value | Linear | Opt-in `Copy` via marker |
| Newtypes | Same as inner type | Inherits | `newtype UserId = Int` is Copy because `Int` is Copy |
| Function pointers | Value | Copy | No closures |

## Copy vs Linear

- **Copy types** can be used multiple times and can be reassigned freely. Primitives, `&T`, raw pointers, and function pointers are always Copy. Structs and enums opt in with a `Copy` marker.
- **Linear types** must be consumed exactly once. All structs and enums are linear by default. Branches must agree on consumption. Loops cannot consume linear variables from outer scope. **Linear variables cannot be reassigned** — one binding, one resource. Use a new binding instead.

## References are second-class: they flow down, never up through returns

**Invariant (language-level).** References (`&T`, `&mut T`) are *scoped access*,
not values you return and carry around. They may flow **downward** — into
function/method calls, into callback parameters, and into borrow blocks — but a
**safe-callable function or function *type* may not *return* a reference**,
directly or nested inside any aggregate, alias, or generic instantiation.

Concretely, the following are rejected:

- `fn id(x: &T) -> &T` and `fn bad() -> &T { return &local; }` — bare reference
  returns (a returned ref has no provenance the checker can track without
  lifetimes/regions, so the language does not allow it at all);
- `fn f(...) -> Option<&T>` / `Result<&T, E>` / any aggregate-wrapped reference
  return (this is the no-aggregate-ref ban — now a *corollary* of this rule);
- `fn(&V) -> &V` as a **function-pointer type** (so a ref-returning callback
  cannot be constructed or passed — this is what keeps scoped callbacks like
  `with_value` sound: the callback cannot return the borrowed element);
- a generic type parameter **instantiated to a reference** when that parameter
  occurs in the return type (e.g. `wrap<R>(r: R) -> Option<R>` called with
  `R = &V` — the generic backdoor to `Option<&V>`).

This is the design that closes H1 (returned-reference provenance) *by
subtraction* rather than by adding lifetimes, regions, or `from()`. To **observe**
borrowed data without returning a reference, use a **scoped callback**
(`with_value`, see `CALLABLE_VALUES_AND_CAPABILITIES.md` §5); to **store** a view,
use an **owned view** (`ByteView`, ROADMAP #5a); to **mutate**, use an operation
API (`update`, `remove`) or a scoped mutable callback; for cheap `Copy` data,
return **by value**. Low-level code may return a **raw pointer** (`*const T` /
`*mut T`, deref requires `Unsafe`, audit-visible) — never a `&T`. `from(param)`
(returned-reference provenance) stays deferred and evidence-gated (ROADMAP #8a1).

Comparison: Austral permits references as values tracked by *regions*
(`Reference[T, R]`); Concrete is deliberately stricter — it removes the
returned-reference shape from safe code entirely, so it needs no region/provenance
machinery. The trade is less borrowed-return ergonomics for a smaller, more
auditable language; the Austral-style region path stays an evidence-gated research
escape valve if real workloads ever prove the restriction too costly.

## The four-cell value model (Copy / Clone / Move / Borrow)

Duplication and ownership transfer fall into four explicit, distinct cells.
This frames the *deliberate* design of `Clone` (see ROADMAP #8a2) — and the
deliberate decision NOT to rush it as a soundness patch:

| Cell | Meaning | Status | Visibility |
|------|---------|--------|------------|
| **Copy** | Implicit bit duplication | Implemented (`Copy` marker, primitives, `&T`, raw ptrs, fn-ptrs) | Free; no effect |
| **Move** | Ownership transfer of a linear value (the default) | Implemented (linear consumption) | Default; tracked by the checker |
| **Borrow** | Scoped temporary access (`&T` / `&mut T` / borrow blocks) | Implemented; references may not escape their scope | Scoped, local |
| **Clone** | Explicit *semantic* duplication of a non-Copy value | **Not yet — deliberate future design** | Capability-visible (`with(Alloc)`) and audit-visible |

`Clone` is intentionally a separate, deliberate design item, **not** the answer
to the returned-reference hole (H1). H1 closes by API design (operation/value
APIs + owned views + scoped callbacks; ROADMAP #8a), independent of `Clone`.

When `Clone` is designed, it must be:
- **Explicit** — `x.clone()`, never implicit; the opposite of `Copy`'s
  invisibility.
- **Capability-visible** — cloning a heap-backed value allocates, so `clone`
  carries `with(Alloc)`; this stays in the signature and audit output.
- **Proof-honest** — `clone` is effectful (allocates), so a function that
  clones is not pure and is outside `ProvableV1`.
- **Library-expressible** — a stdlib `trait Clone { fn clone(&self) -> Self }`
  rides the existing trait-bound dispatch; no builtin trait is required
  (verified). It composes with `Destroy` (a cloned linear value must still be
  consumed/destroyed exactly once).

**Move-out vs copy-out.** `Clone` duplicates (copy-out). The ownership-transfer
counterpart (move-out) is the default for owned values; for collections,
`remove`/`update` already move a value out, and the one genuine gap is indexed
containers — `swap(i, new) -> V` transfers ownership out of a slot without
clone or delete, preserving the linear one-value-per-slot invariant. Like
`Clone`, build it when a workload needs it (ROADMAP #8a2), not speculatively.

## Current Guarantee Boundary

Today the language already relies on the checker to enforce the core ownership/borrow model:

- no use-after-move for linear values
- no forgotten linear values at scope exit
- no mutable-vs-shared borrow conflicts in safe code
- no borrow escape from borrow blocks
- explicit cleanup instead of implicit destructors

What is still being consolidated is the single public semantics document and theorem-like guarantee statement for the full safe-memory subset, especially around harder aliasing/reference edge cases.

## Borrowed String Literals

`&"literal"` produces a borrowed `&String` that points directly at the global constant — no heap allocation, no copy. The resulting `String` struct has `cap = 0` to signal it is not heap-owned and must not be freed.

This means string building can consume borrowed literal text without temporary ownership:

```concrete
// Zero-alloc: no temp created, nothing to drop
sum.append(&" ok, ");
print_string(&"hello\n");
```

Owned string literals (`let s: String = "hello"`) still heap-allocate a mutable copy, since the caller owns and may mutate or drop the value. The distinction is:

| Form | Allocates | Ownership | Must drop |
|------|-----------|-----------|-----------|
| `let s: String = "hello"` | Yes (heap copy) | Caller owns | Yes |
| `&"hello"` | No (points at global) | Borrowed | No |

## Newtype

```concrete
newtype UserId = Int;       // Copy (inner is Copy)
newtype OwnedBuf = String;  // Linear (inner is linear)
newtype Wrapper<T> = T;     // Inherits from T
```

- **Wrap:** `UserId(42)` — type-checked constructor
- **Unwrap:** `id.0` — extracts inner value
- **No implicit conversions:** `UserId` and `Int` are distinct types
- **Zero-cost:** Erased at compile time, no runtime representation

## Layout Attributes

### `#[repr(C)]`
C-compatible struct layout. Required for FFI.

### `#[repr(packed)]`
No padding between fields. Alignment forced to 1.
```concrete
#[repr(C, packed)]
struct Header { version: u8, flags: u16, length: u32 }
// sizeof = 7 (1 + 2 + 4, no padding)
```

### `#[repr(align(N))]`
Minimum alignment override (N must be a power of 2).
```concrete
#[repr(C, align(16))]
struct Aligned { x: i32 }
// sizeof = 16 (rounded up), alignof = 16
```

`packed` and `align(N)` cannot be combined on the same struct.

## Intrinsics

- `sizeof::<T>()` — compile-time byte size of `T` (returns `Uint`)
- `alignof::<T>()` — compile-time alignment of `T` (returns `Uint`)
