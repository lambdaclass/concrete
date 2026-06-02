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
