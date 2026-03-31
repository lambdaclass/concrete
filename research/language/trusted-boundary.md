# Trusted Boundary

**Status:** Implemented

This note defines the `trusted` boundary design for containing implementation-level unsafety without leaking `Unsafe` into safe public APIs.

The model applies uniformly to:

- compiler builtins
- standard-library internals
- user-written low-level code (C bindings, custom allocators, intrusive structures, runtime components)

## The Three-Way Split

These three mechanisms are distinct and compose cleanly:

- **capabilities** (`with(Alloc)`, `with(File)`, etc.) = semantic effects visible to callers
- **`trusted`** = containment of internal pointer-level implementation techniques behind a safe API
- **`with(Unsafe)`** = authority to cross foreign or semantically dangerous boundaries (FFI, transmute)

## Core Distinction

### 1. Semantic effect visible to callers

A function may allocate, perform file I/O, or open a socket. These belong in the ordinary capability signature:

```con
fn push(&mut self, value: T) with(Alloc) { ... }
```

That tells the caller something true about program behavior.

### 2. Implementation trust boundary

A function uses raw pointer arithmetic, dereferences raw pointers, or contains audited low-level code that would normally require `Unsafe`. That is not a public semantic effect. It is a statement about *how the implementation is achieved*.

That is modeled by a separate construct:

```con
trusted impl Vec<T> {
    fn push(&mut self, value: T) with(Alloc) { ... }
}
```

## Why This Is Better

Without a trusted boundary, there are two bad options:

### Bad option 1: silent exemption

The compiler does not enforce `Unsafe` rules inside selected stdlib code.

- the trust boundary is invisible
- the language appears stronger than it is
- auditing becomes harder

### Bad option 2: leak `Unsafe` to callers

```con
fn push(&mut self, value: T) with(Alloc, Unsafe) { ... }
```

- safe abstractions stop looking safe
- `Unsafe` spreads through large parts of ordinary code
- callers are forced to carry implementation details they should not need to know

The trusted-boundary design avoids both:

- `Alloc` stays visible where it matters
- raw-pointer internals are contained
- the trust boundary remains explicit and grep-able

## Surface Syntax

```con
trusted fn helper(...) { ... }
trusted impl Vec<T> { ... }
trusted extern fn sqrt(x: Float64) -> Float64;
```

Three forms:

- `trusted fn` — a function whose body may use raw pointers without leaking `Unsafe` to callers
- `trusted impl` — an impl block whose methods may use raw pointers without leaking `Unsafe` to callers
- `trusted extern fn` — a foreign binding that callers can use without `with(Unsafe)`, because the binding is audited as safe for any valid inputs of the declared types

This is better than `#[trusted]` because it is more visible, feels like a language-level boundary, and is easier to grep.

This is better than `unsafe fn` because `unsafe fn` suggests danger for the caller. Here the point is that the *implementation* is trusted, while the public API remains safe-facing.

## What `trusted` Permits

`trusted` has a precise, closed scope. It covers internal pointer-level implementation techniques. It does not cover foreign or semantic boundaries.

### Permitted inside `trusted` without leaking `Unsafe` to callers

- raw pointer dereference (`*ptr`)
- raw pointer assignment (`*ptr = value`)
- pointer arithmetic (`ptr + offset`)
- pointer casts (`ptr as *mut T`, `ptr as &T`)

### Still requires `with(Unsafe)` even inside `trusted fn`/`trusted impl`

- **`extern fn` calls** — FFI crosses a semantic boundary (calling code with unknown behavior), not just an implementation detail. Callers should know when foreign code is invoked.
- **`transmute`** — stays under `with(Unsafe)`, even inside `trusted`. Transmute can violate type safety in ways pointer operations cannot. If a narrower layout-preserving reinterpretation is ever needed, it should be a different feature — not a weakening of transmute.

### Exception: `trusted extern fn`

A `trusted extern fn` declaration asserts that a specific foreign binding is safe to call with any valid inputs. This removes the `Unsafe` requirement at the call site. Unlike `trusted fn`/`trusted impl` (which contain *blocks* of pointer-level code), `trusted extern fn` is a *per-binding* audit assertion.

This is the right tool for pure, well-understood libc functions like `sqrt`, `sin`, `floor`, etc. It is not a general escape from `Unsafe` for arbitrary FFI. The distinction: `trusted fn` trusts a body of code; `trusted extern fn` trusts a single foreign symbol.

### Why this boundary

Raw pointer operations inside a well-audited container are an implementation technique. The data stays within the language's type and memory model — the risk is a bug in the trusted code, not unbounded foreign behavior.

An `extern fn` call hands control to code outside the language entirely. The compiler cannot reason about what happens. That is a fundamentally different kind of danger and remains visible even inside trusted code.

## Rules

1. **Available to all code, controlled by audit visibility**
   Anyone writing C bindings, custom allocators, ring buffers, or intrusive data structures needs `trusted`. Restricting it to stdlib would push users to `with(Unsafe)` everywhere, breaking the model. The control mechanism is audit visibility (`--report`), not an artificial restriction.

2. **Explicit, never inferred**
   A function or impl is trusted only if marked directly.

3. **Not nestable**
   No implicit propagation, no stacking, no "trusted because it is inside trusted."

4. **Ordinary capability rules still apply**
   `trusted` does not erase `Alloc`, `File`, `Network`, etc. It only affects the internal pointer-level trust boundary. If `Vec::push` allocates, it declares `with(Alloc)`. If a network wrapper opens a socket, it declares `with(Network)`. `trusted` only removes the need to leak `Unsafe` for internal pointer work — it never suppresses semantic capability checking.

5. **FFI stays under `Unsafe` by default**
   Regular `extern fn` calls require `with(Unsafe)` even inside `trusted` code. `trusted fn`/`trusted impl` is not a backdoor to call foreign code silently. The narrow exception is `trusted extern fn`, which marks a specific foreign binding as safe — this is a per-binding audit decision, not a blanket trust grant.

6. **`transmute` stays under `Unsafe`**
   `transmute` requires `with(Unsafe)` even inside `trusted` code. If a narrower layout-preserving reinterpretation is needed later, it will be a separate feature.

7. **Must appear in audit outputs**
   `grep trusted` finds the boundary in source. Compiler reports surface it too.

## Audit Report Shape

Near term:

- `--report unsafe` includes trusted regions explicitly
- Sections: unsafe functions, trusted functions/impls, unsafe reasons, trusted reasons

Later, if it becomes noisy:

- add `--report trusted` as a focused view

The immediate decision: trusted appears in `--report unsafe`. Separate `--report trusted` is optional later.

## Examples

### Vec

```con
trusted impl Vec<T> {
    fn new() with(Alloc) -> Vec<T> { ... }

    fn push(&mut self, value: T) with(Alloc) {
        ...
    }

    fn get(&self, at: u64) -> Option<&T> {
        ...
    }
}
```

- `new` and `push` may allocate — callers see `with(Alloc)`
- `get` does not allocate — no capability needed
- raw-pointer internals stay inside the trusted impl
- callers do not need `Unsafe`

### HashMap

```con
trusted impl HashMap<K, V> {
    fn new(hash: fn(&K) -> u64, eq: fn(&K, &K) -> bool) with(Alloc) -> HashMap<K, V> {
        ...
    }

    fn insert(&mut self, key: K, value: V) with(Alloc) {
        ...
    }

    fn get(&self, key: &K) -> Option<&V> {
        ...
    }
}
```

### SocketBuffer (trusted + Unsafe coexistence)

```con
trusted impl SocketBuffer {
    // Pointer internals covered by trusted.
    // FFI still requires Unsafe — it crosses a semantic boundary.
    fn recv_into(&mut self, fd: i32) with(Unsafe, Network) {
        let buf: *mut u8 = self.ptr + self.pos;
        extern fn recv(fd: i32, buf: *mut u8, len: u64, flags: i32) -> i64;
        let n: i64 = recv(fd, buf, self.cap - self.pos, 0);
        ...
    }

    // Pure pointer work — no FFI, no Unsafe needed.
    fn get(&self, idx: u64) -> u8 {
        let p: *mut u8 = self.ptr + idx;
        return *p;
    }
}
```

## Scope: Builtins, Stdlib, and User Code

The model is coherent across all three layers.

### 1. Builtins

Compiler-intercepted operations (collection builtins, etc.) expose real semantic effects in their signatures:

- `vec_new` / `map_new` / `insert`-style growth paths carry `with(Alloc)`
- non-allocating lookup/read paths stay capability-free

The builtin layer does not get a silent exemption from capability checking.

### 2. Stdlib internals

Collection and systems-library internals migrate to:

- public signatures that expose real semantic effects (`Alloc`, `File`, `Network`, `Process`, etc.)
- `trusted fn` / `trusted impl` boundaries for pointer-level implementation techniques
- `with(Unsafe)` still required for `extern fn` calls

### 3. User code

User code uses `trusted` for C bindings, custom allocators, ring buffers, intrusive collections, and runtime code. The control mechanism is audit visibility, not an artificial privilege boundary.

## Stdlib Migration

`trusted` is now implemented in the compiler, and the first builtins/std/user-code coherence pass has landed. The next work is hardening and refinement, not invention of the model itself.

What needs `trusted`:

- container internals: `Vec`, `Bytes`, `HashMap`, `HashSet`
- low-level file/network/process wrappers that use raw pointers internally
- allocator internals and pointer/memory helpers

The migration is not ad hoc. The remaining order is:

1. keep capability checking for raw pointer ops and extern calls uniform with `trusted` as the containment mechanism
2. deepen builtin and stdlib migration coverage where any silent exemptions remain
3. improve `--report unsafe` and trusted-boundary visibility further as the audit story grows
4. add `--report trusted` later only if the combined report becomes noisy

## Rollout Order

1. keep raw-pointer / low-level checks uniform with `trusted` available as the containment mechanism
2. keep builtin and stdlib signatures honest about their semantic capabilities
3. keep trusted boundaries visible in `--report unsafe`
4. later, add `--report trusted` if the combined report becomes noisy

## Relationship To Other Work

- [unsafe-structure.md](unsafe-structure.md) — the broader `Unsafe` inspection and containment story
- [capability-sandboxing.md](capability-sandboxing.md) — capability hardening and sandboxing
- [../docs/FFI.md](../docs/FFI.md) — the foreign-function interface boundary
