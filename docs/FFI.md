# FFI and Unsafe Boundary

Status: stable reference

This document describes the current foreign-function interface boundary and the role of `Unsafe` in Concrete.

For the full safety model (capabilities, trusted, Unsafe, proof boundary), see [SAFETY.md](SAFETY.md). For layout and representation rules, see [ABI.md](ABI.md). For active priorities, see [../ROADMAP.md](../ROADMAP.md).

## Core Principles

Concrete keeps the foreign boundary explicit:

- FFI functions are declared with `extern fn`
- raw pointers are explicit types (`*const T`, `*mut T`)
- unsafe operations are gated by `with(Unsafe)`
- safe code does not silently cross into foreign or pointer-sensitive behavior

## Extern Functions

Extern declarations are the language-level entry point to foreign code:

```con
extern fn puts(ptr: *const u8) -> i32;
```

Current rules:

- `extern fn` has no Concrete body
- extern calls require `Unsafe` by default
- extern parameter and return types must be FFI-safe

### Trusted Extern Functions

A `trusted extern fn` is an audited foreign binding that callers can use without `with(Unsafe)`:

```con
trusted extern fn sqrt(x: Float64) -> Float64;
```

This is the right tool for well-known, pure libc functions (math, `abs`, etc.) where requiring `Unsafe` at every call site adds noise without safety value.

Rules:

- `trusted extern fn` uses the existing `trusted` keyword — no new syntax
- callers do not need `with(Unsafe)`
- parameter and return types must still be FFI-safe
- the declaration itself is the audit boundary — it asserts the foreign function is safe to call with any valid arguments of the declared types
- `--report unsafe` shows trusted extern declarations in a separate "Trusted extern functions" section

Keep the category narrow. `trusted extern fn` is for pure, well-understood foreign functions — not a general "safe FFI" escape hatch. If a foreign function has side effects, mutates global state, or can crash on valid inputs, it should remain a regular `extern fn` under `with(Unsafe)`.

## FFI-Safe Types

FFI-safe types currently include:

- integer types
- float types
- `Bool`
- `Char`
- `()`
- raw pointers (`*const T`, `*mut T`)
- `#[repr(C)]` structs

The implementation authority for this check is `Layout.isFFISafe`.

## Unsafe Boundary

Concrete currently requires `Unsafe` for:

- calling `extern fn` (but not `trusted extern fn`)
- dereferencing raw pointers
- assigning through raw pointers
- pointer-involving casts, except reference-to-pointer casts

Safe exception:

- `&x as *const T`
- `&mut x as *mut T`

These preserve compiler-known provenance and do not invent an address.

## Intentional Design Rule

The goal is not to hide low-level operations behind “safe-feeling” library APIs.

The audit story should stay simple:

- `grep with(Unsafe)` finds the boundary
- extern declarations are visible in signatures
- raw pointers stay explicit in types

Concrete is aiming for an unsafe boundary that is:

- operationally obvious
- explicitly gated
- easier to audit than a broad ambient low-level model

## Trusted Split

Concrete now has the three-way split described in the research notes:

- **capabilities** (`with(Alloc)`, `with(File)`, etc.) = semantic effects visible to callers
- **`trusted`** = containment of internal pointer-level implementation techniques behind a safe API
- **`with(Unsafe)`** = authority to cross foreign boundaries (FFI, transmute) — always explicit, even inside trusted code

That means:

- `trusted` does **not** suppress ordinary capabilities
- `trusted fn`/`trusted impl` does **not** permit `extern fn` calls without `with(Unsafe)`
- `trusted extern fn` is a separate, narrower mechanism: it marks a specific foreign binding as safe to call, rather than granting blanket trust to a block of code
- builtin and stdlib internals are aligned to this same model instead of relying on silent exemptions

## Authority Wrapper Patterns

The recommended pattern for containing unsafe or capability-requiring code is: write a small trusted wrapper, expose a safe narrow interface above it, keep raw pointers and FFI details inside the wrapper.

```con
// Bad: callers need with(Unsafe) to use raw FFI
pub fn read_header(fd: i32) with(Unsafe) -> Int {
    // raw extern call ...
}

// Good: trusted wrapper contains the unsafety
pub trusted fn read_header(fd: i32) with(File, Unsafe) -> Int {
    // raw extern call inside trusted boundary
}
// Callers still need with(File, Unsafe); trusted contains the pointer work, not the capability
```

The stdlib demonstrates this pattern throughout:

- `trusted impl Vec<T>` wraps pointer arithmetic behind safe push/pop/get
- `trusted impl HashMap<K, V>` wraps raw allocation behind safe insert/get/remove
- `trusted impl TextFile` wraps POSIX file I/O behind open/read/write/close
- `trusted fn print/println` wraps extern console I/O behind `with(Console)`
- `trusted extern fn sqrt` marks a pure foreign function as safe to call without `with(Unsafe)`

The `--report unsafe` and `--report authority` modes make these boundaries visible for audit.

## Capability Aliases

Capability aliases reduce signature repetition without hiding authority:

```con
cap IO = File + Console;
cap Host = File + Network + Env + Process;

fn serve() with(Host) -> Int { ... }
fn log(msg: &String) with(IO) { ... }
```

An alias expands to its constituent capabilities at parse time — the rest of the compiler sees only concrete capability names. Aliases can use the `Std` macro (`cap All = Std;` expands to all standard capabilities except `Unsafe`).

Aliases are validated at definition time: all constituent names must be known capabilities. Unknown names produce a parse error.

## Future Refinement

This doc should expand if the FFI surface grows substantially, for example:

- more explicit calling-convention rules
- ABI notes for additional targets
- low-level FFI helper patterns in the stdlib
- continued hardening of builtin and stdlib internals around the implemented `trusted` boundary

For the exploratory direction behind those ideas, see [../research/language/unsafe-structure.md](../research/language/unsafe-structure.md) and [../research/language/trusted-boundary.md](../research/language/trusted-boundary.md).
