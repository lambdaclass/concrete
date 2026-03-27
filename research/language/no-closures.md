# Why Concrete Has No Closures

**Status:** Decided — no closures, function pointers only
**Affects:** All phases (permanent language constraint)
**Date:** 2026-03-08

## What Are Closures?

A closure is an anonymous function that captures variables from its enclosing scope. The captured variables form a hidden environment that travels with the function pointer:

```rust
// Rust
let offset = 10;
let add_offset = |x: i32| x + offset;  // captures `offset`
add_offset(32)  // → 42
```

At the machine level, a closure is a **fat pointer**: a pair `{ fn_ptr, env_ptr }` where `fn_ptr` points to the code and `env_ptr` points to a struct containing the captured variables. The call `add_offset(32)` compiles to something like `fn_ptr(env_ptr, 32)` — an indirect call through a function pointer, with a hidden first argument the programmer did not write.

## Why Concrete Excludes Them

Three language invariants are violated:

**1. "All code paths known at compile time."** When a function takes a closure parameter `f: fn(Int) -> Int`, the compiler cannot determine which function runs when `f(x)` is called. The target is a function pointer loaded at runtime. Static analysis, formal verification, and exhaustive reasoning about program behavior require knowing the call graph — closures make the call graph dynamic.

**2. "No hidden control flow."** At the call site, `f(x)` where `f` is a closure looks identical to `foo(x)` where `foo` is a known function. The programmer cannot tell from reading the code whether this is a direct call or an indirect call through an opaque pointer. The "visibility in the type" argument (`f: fn(Int) -> Int` declares it's a function pointer) only helps at the *declaration* site, not at the *call* site where the dispatch happens.

**3. "No hidden data flow."** A closure's captured environment is invisible at the call site. When you call `f(x)`, you cannot see what other data `f` has access to. The captures are determined wherever the closure was created, which may be in a completely different part of the program. This is the same objection we have against trait objects — hidden state traveling with a value.

## Why "Visible in the Type" Is Not Enough

The previous design argued that closures are acceptable because the type `fn(Int) -> Int` explicitly says "this is a function pointer." The reasoning was: trait objects hide indirection (`x.describe()` looks like a direct call), but closures make it explicit (the type tells you).

This argument is weaker than it appears:

- **At the call site, the indirection is invisible.** `f(x)` looks the same whether `f` is a local function name or a closure parameter. The programmer must check the declaration to know.
- **The captured environment is always invisible.** No amount of type information tells you what variables a closure has captured. The type `fn(Int) -> Int` says nothing about what hidden state the closure carries.
- **For formal verification, it doesn't matter.** Whether the indirection is "visible in the type" or not, the verifier still faces a function pointer it cannot resolve. SPARK (Ada's verified subset) bans all access-to-subprogram types for exactly this reason — the control flow graph must be statically determinable.

## Precedent

| Language | Closures? | Philosophy |
|----------|-----------|------------|
| **C** | No | Function pointers + `void*` context. All critical infrastructure. |
| **Zig** | No | Explicit function pointers + comptime generics. "Closures are too magical." |
| **Forth** | No | Execution tokens (code addresses). Context passed on the stack. |
| **SPARK/Ada** | No | Access-to-subprogram banned in verified mode. Control flow must be static. |
| **Austral** | No | Linear types + capabilities (very similar to Concrete). Closures banned because they hide captured resources. |
| **Odin** | No | Procedure pointers + explicit context parameter. |
| **Rust** | Yes | `Fn`/`FnMut`/`FnOnce` traits, monomorphized or `dyn`. Complex type system to track capture behavior. |
| **Go** | Yes | GC'd closures, capture by reference. Hidden heap allocations. |
| **ATS** | Yes | `clo`/`cloptr`/`cloref` — three kinds of closure with different lifetime rules. Requires dependent types to verify. |

Every language that prioritizes verification and explicitness either bans closures entirely (C, Zig, SPARK, Austral, Odin) or requires a complex type system to make capture behavior visible (Rust, ATS). Concrete opts for the simpler path.

## What Concrete Does Instead

### 1. Function pointers (no-capture, C-style)

For single-method callbacks where no context is needed:

```
fn apply(x: Int, f: fn(Int) -> Int) -> Int {
    return f(x);
}

fn double(x: Int) -> Int {
    return x * 2;
}

let result: Int = apply(21, double);  // passes a function pointer
```

The type `fn(Int) -> Int` is a plain function pointer — a code address, 8 bytes, no environment. The function being pointed to is a named, defined function. At the call site in `apply`, `f(x)` is an indirect call, but:
- There is no hidden captured state
- The set of possible targets is the set of named functions with matching signature
- A verification tool can enumerate these

### 2. Monomorphized generics (zero-cost, statically resolved)

For higher-order patterns like map/filter/fold:

```
trait Transform {
    fn apply(&self, x: Int) -> Int;
}

struct Doubler {}
impl Transform for Doubler {
    fn apply(&self, x: Int) -> Int { return x * 2; }
}

struct Adder { offset: Int }
impl Transform for Adder {
    fn apply(&self, x: Int) -> Int { return x + self.offset; }
}

fn transform<T: Transform>(val: Int, t: &T) -> Int {
    return t.apply(val);
}

let d: Doubler = Doubler {};
let result: Int = transform(21, &d);  // monomorphized to transform_Doubler
```

The compiler generates `transform_Doubler` and `transform_Adder` — specialized copies with direct calls. No indirection, no function pointers, every call target known at compile time.

This is more verbose than `apply(21, fn(x) { x * 2 })` but:
- Every piece of data is named and visible
- The "captured" state (`offset` in `Adder`) is an explicit struct field
- The compiler resolves every call statically
- Formal verification can reason about the complete program

### 3. Struct of function pointers (manual vtable, explicit context)

For multi-method pluggable interfaces where context is needed:

```
struct Renderer {
    ctx: Heap<RenderContext>,
    draw_fn: fn(&RenderContext, Int, Int) -> Int,
    clear_fn: fn(&RenderContext) -> Int,
}
```

The context is an explicit field. The function pointers are explicit fields. At the call site, you write `r.draw_fn(&(*r.ctx), x, y)` — the indirection is visible, the context passing is visible, nothing is hidden.

This is what C does (Linux kernel VFS, GTK, OpenGL). It works for the world's most critical infrastructure.

### 4. Enums for closed dispatch

For cases where the set of behaviors is known:

```
enum Action {
    Double {},
    Add { offset: Int },
    Negate {},
}

fn apply_action(a: &Action, x: Int) -> Int {
    match a {
        Action#Double {} => { return x * 2; },
        Action#Add { offset } => { return x + offset; },
        Action#Negate {} => { return 0 - x; },
    }
}
```

Exhaustiveness checked. All code paths visible. No indirection.

## Coverage Analysis

| Use case | Without closures | Works? |
|----------|-----------------|--------|
| Callback (no context) | Function pointer | Yes |
| Callback (with context) | Function pointer + explicit context struct | Yes — verbose but visible |
| map/filter/fold | Monomorphized generics with trait bounds | Yes — zero-cost, statically resolved |
| Strategy pattern | Enum or struct of function pointers | Yes |
| Deferred cleanup | `defer destroy(x)` (known function, not a closure) | Yes |
| Event handler | Function pointer + context struct | Yes |
| Builder/configuration | Struct fields | Yes |
| `sort(list, comparator)` | Monomorphized generic: `sort<T: Ord>(list)` | Yes |
| Composing transformations | Chain of trait method calls or enum pipeline | Yes — more verbose |
| Quick inline logic | **No** — must define a named function | No shorthand |

## What You Lose

**Ergonomics for inline anonymous logic.** Instead of:

```
// Not possible in Concrete
let result = apply(21, fn(x) { x * 2 });
```

You must write:

```
fn double(x: Int) -> Int { return x * 2; }
let result: Int = apply(21, double);
```

Or with generics:

```
struct Doubler {}
impl Transform for Doubler {
    fn apply(&self, x: Int) -> Int { return x * 2; }
}
let d: Doubler = Doubler {};
let result: Int = transform(21, &d);
```

This is more verbose. The tradeoff: every function has a name, every piece of state is a visible struct field, every call target is either statically known (monomorphized generics) or explicitly a function pointer (visible in the type AND at the call site since there's no captured environment to hide).

**Capability polymorphism impact.** The `cap C` feature in Phase 1 allows:

```
fn map<T, U, cap C>(list: List<T>, f: fn(T) with(C) -> U) with(C) -> List<U>
```

This still works with function pointers — `f` is a pointer to a named function. The caller passes a function reference: `map(data, transform_with_file)`. The capability variable `C` is inferred from the referenced function's declared capabilities. No closures needed.

## The Key Insight

Closures bundle **code + hidden data** into a single opaque value. Removing closures means:
- Code is always a named function (findable with grep)
- Data is always in explicit structs or function arguments (visible at every use site)
- Indirection (function pointers) exists but carries no hidden state

This is the same principle behind removing trait objects: if you can't see what data a value carries and what code it will execute, you can't verify the program. Closures and trait objects are two sides of the same coin — both hide dispatch and state behind an opaque interface.

## Decision

No closures. Function pointers (no capture) + monomorphized generics + explicit context structs. This is a permanent language constraint. The formal verification story (Phase 9) depends on either statically resolved dispatch (monomorphization) or visible indirect dispatch through typed function pointers with no hidden environment.
