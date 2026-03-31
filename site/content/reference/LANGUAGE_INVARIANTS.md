+++
title = "Language Invariants"
+++

# Language Invariants

Status: stable reference

These rules apply across all phases. They come directly from the language design and must never be violated.

## 1. Pure By Default

A function without `with()` is pure. It cannot call any function that has `with()`.

## 2. True Linear Types

Every linear value must be consumed exactly once. Not zero (leak = compile error). Not twice (double-use = compile error). Forgetting a resource is rejected.

### What counts as consuming a value

- Passing it as a by-value argument to a function or method (including `destroy(x)`)
- Returning it from a function (`return x`)
- Moving it into a struct field during construction (`Point { x: val }`)
- `break val` inside a loop-as-expression
- Destructuring via `match` or `let` (the original is consumed, the fields become new bindings)
- Storing into an array element during array literal construction (`[val1, val2]`)

Each phase that adds a new consumption form must update this list.

### What does not count as consumption

- borrowing (`&x`, `&mut x`)
- taking an address for a raw pointer (`&x as *const T`)
- deferring (`defer destroy(x)` reserves but does not consume until execution)

## 3. No Hidden Control Flow

`a + b` on integers is primitive addition, not a method call. The compiler never inserts destructor calls — you write `defer destroy(x)` explicitly. If it allocates, you see `with(Alloc)`. Errors propagate only where `?` appears.

### Note on `defer`

`defer` schedules code at scope exit, but the programmer writes `defer` explicitly at the point of scheduling. The compiler emits the deferred call at the point of scope exit. This does not violate the invariant because:

- the programmer wrote the `defer` statement
- the execution point (scope exit) is deterministic and visible from the block structure
- no implicit function dispatch occurs; the exact function being called is the one written in the `defer` statement

## 4. No Variable Shadowing

Each variable name must be unique within its scope. This extends to region names in borrow blocks — `borrow x as xr in R` introduces `xr` and `R` into scope, and neither may shadow existing names.

## 5. No Uninitialized Variables

All variables must be initialized at declaration.

## 6. No Operator Overloading

Operators (`+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `&&`, `||`, `!`) on primitives (`Int`, `Uint`, `i8-i64`, `u8-u64`, `f32`, `f64`, `Bool`) are built-in. They are not trait method calls. User-defined types cannot use these operators — use named functions instead.

## 7. No Implicit Conversions

No silent coercion between types. Explicit `as` casts only.

## 8. No Null

Optional values use `Option<T>`.

## 9. No Exceptions

Errors are values (`Result<T, E>`), propagated with `?`.

## 10. No Global Mutable State

All global interactions are mediated through capabilities.

## 11. No Interior Mutability In Safe Code

All mutation flows through `&mut`. Exception: `UnsafeCell<T>` in later low-level stdlib work, gated by `Unsafe`. Before that, interior mutability does not exist in safe code.

## 12. Local-Only Type Inference

Function signatures must be fully annotated. Inside function bodies, local variable types may be inferred from the right-hand side of `let` bindings.

Inference direction:
- right-to-left only
- no cross-statement constraint solving
- no broad bidirectional inference

## 13. LL(1) Grammar

Every parsing decision must be possible with a single token of lookahead. No ambiguity, no backtracking.

## 14. Readability Is Part of Correctness

Surface syntax, stdlib APIs, and trusted boundaries should stay explicit enough that ordinary code can be read and audited predictably. A feature that makes low-level behavior harder to see increases audit cost and should be treated as a correctness concern, not merely a style issue.

## 15. New Features Must Justify Their Cost

New language features must justify:

- grammar cost
- audit cost
- proof cost

If a feature makes the language harder to parse, harder to review, or harder to prove without delivering a correspondingly large benefit, it should be rejected or delayed.

## 16. `abort()` Is Immediate Process Termination

Deferred cleanup does not run on `abort()`. Out-of-memory and stack overflow also trigger abort. This is outside the language's semantic model.

On POSIX systems, `abort()` typically produces exit code `134`, but tests should check for nonzero exit rather than a specific code.

## 17. Reproducible Builds

Same source plus same compiler should yield the same binary. No timestamps, random seeds, or environment-dependent output.

## 18. Diagnostics Stay Structured

The compiler keeps diagnostics structured through the main semantic pipeline. It may accumulate multiple errors across functions or modules, but it does not rely on broad recovery or ambiguous parse fallback to continue.
