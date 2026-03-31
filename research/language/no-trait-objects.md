# Why Concrete Has No Trait Objects

**Status:** Decided — no trait objects, monomorphization only
**Affects:** All phases (permanent language constraint)
**Date:** 2026-03-08

## What Are Trait Objects?

Trait objects (`dyn Trait` in Rust, interfaces in Go, virtual methods in C++) let you store values of **different concrete types** behind a single type, with method calls dispatched at runtime through a function pointer table (vtable).

```rust
// Rust
fn print_all(items: Vec<Box<dyn Display>>) {
    for item in items {
        item.display(); // which display()? decided at runtime via vtable
    }
}

let mixed: Vec<Box<dyn Display>> = vec![
    Box::new(42),              // an integer
    Box::new("hello"),         // a string
    Box::new(Point { x: 1 }), // a struct
];
print_all(mixed);
```

The call `item.display()` compiles to something like `vtable[0](data_ptr)` — an indirect call through a function pointer loaded from a table at runtime.

## Why Concrete Excludes Them

Two language invariants are violated:

**1. "All code paths known at compile time."** With `dyn Trait`, the compiler cannot determine which concrete method runs at a given call site. The target is a function pointer loaded from a runtime table. Static analysis, formal verification, and exhaustive reasoning about program behavior become harder.

**2. "No hidden control flow."** The call `x.describe()` on a trait object looks syntactically identical to `x.describe()` on a concrete type `Point`. The indirection is invisible at the call site. The programmer cannot tell from reading the code whether this is a direct call or a runtime dispatch.

## Function Pointers Instead

Concrete does not have closures. When explicit indirect dispatch is needed, it uses plain function pointers and explicit context values passed as ordinary arguments.

## What Concrete Does Instead

Concrete uses three mechanisms that cover the vast majority of use cases:

### 1. Enums (closed dispatch)

For heterogeneous collections where the set of types is known at compile time:

```
enum Shape {
    Circle { radius: Int },
    Rect { w: Int, h: Int },
    Tri { base: Int, height: Int },
}

fn area(s: &Shape) -> Int {
    match s {
        Shape#Circle { radius } => { return radius * radius * 3; },
        Shape#Rect { w, h } => { return w * h; },
        Shape#Tri { base, height } => { return base * height / 2; },
    }
}
```

Adding a new variant forces updating every match — the compiler checks exhaustiveness. This is a feature: you cannot forget to handle a case.

### 2. Monomorphized generics (compile-time dispatch)

For writing code that works across types where the concrete type is known at each call site:

```
trait Describe {
    fn describe(&self) -> Int;
}

fn show<T: Describe>(x: T) -> Int {
    return x.describe();  // resolved to Point_describe at compile time
}

let result: Int = show(myPoint);  // generates show_for_Point
```

The compiler generates a specialized copy of `show` for each concrete type. No runtime dispatch. Every call target is a direct, known function.

### 3. Function pointers (explicit indirect dispatch)

For pluggable single-method behavior:

```
fn apply(x: Int, f: fn(Int) -> Int) -> Int {
    return f(x);  // indirect call — visible from the type
}

let doubled: Int = apply(21, double);
```

The type `fn(Int) -> Int` makes the indirection explicit. No hidden dispatch, no hidden environment.

## Coverage Analysis

| Use case | Mechanism | Works? |
|---|---|---|
| Heterogeneous collection (known types) | Enum + match | Yes — exhaustiveness checked |
| Strategy / callback (single method) | Function pointer | Yes — explicit in the type |
| AST / expression tree | Enum + Heap for recursion | Yes — natural fit |
| Multiple backends (DB, format, protocol) | Enum | Yes — known set at compile time |
| Multi-method pluggable interface | Struct of function pointers plus explicit context | Yes — verbose but explicit |
| Library extensible by downstream users | **No** | Must modify the enum |
| Runtime plugin loading | **No** | Needs dynamic linking |

## What You Lose

The one case enums cannot cover: **open extension by downstream code**.

If you write a library with a `Widget` enum, users cannot add new widget types without modifying your enum definition. With trait objects, they would just `impl Widget for TheirCustomWidget`.

This is a deliberate tradeoff. Concrete prioritizes:
- Static knowledge of all code paths (for verification)
- No hidden control flow (for readability)
- Exhaustiveness checking (for correctness)

Over:
- Runtime extensibility
- Open-world polymorphism

### The manual vtable escape hatch

If you truly need multi-method runtime dispatch, a struct of function pointers plus explicit context is a manual vtable:

```
struct Renderer {
    draw_fn: fn(&Int, &Int) -> Int,
    clear_fn: fn() -> Int,
}
```

This is what C does (Linux kernel VFS, GTK, etc.). It works, it's explicit, and no compiler magic is involved. The programmer constructs the dispatch table, passes it around, and calls through it — all visible in the code.

## Precedent

- **C** has no dynamic dispatch. Function pointer tables are the standard pattern for polymorphism (Linux kernel, SQLite, OpenGL). It works for the world's most critical infrastructure.
- **Zig** has no trait objects. Uses comptime generics and tagged unions (enums).
- **Go** has interfaces (implicit dynamic dispatch) but the Go team has acknowledged the hidden performance and complexity costs.
- **Rust** has `dyn Trait` but the community increasingly favors `impl Trait` (static dispatch) and enums over trait objects, except where truly needed.

## Decision

No trait objects. Monomorphization + enums + function pointers. This is a permanent language constraint, not a "not yet implemented" feature. The formal verification story depends on all dispatch being statically resolvable or explicitly indirect through typed function pointers.
