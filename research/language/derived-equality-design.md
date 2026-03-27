# Derived Equality Design: `==` on User-Defined Types

**Status:** Open decision
**Affects:** Language Invariant 6 (no operator overloading), Phase 9 (standard library)
**Date:** 2026-03-07

## Context

Language Invariant 6 says "No operator overloading." This means `==` only works on primitives (Int, Bool, Float64, etc.). User-defined types (structs, enums) cannot use `==` — you must write a named function like `fn eq(a: &Point, b: &Point) -> Bool`.

This is painful. Comparison is one of the most common operations. Every data type needs a named function. The question: can we add `==` for user-defined types without violating "no operator overloading"?

The key distinction: **operator overloading** means the user defines arbitrary behavior for `==`. **Derived equality** means the compiler generates structural equality automatically. The user has no control over the behavior — it's always field-by-field comparison. These are fundamentally different things.

---

## How Other Languages Handle This

### Austral — No equality on user types

The compiler has a hardcoded `is_comparable` function that returns `false` for all named types (records and unions). Only primitives and pointers are comparable with `==`. There is no `Eq` typeclass and no derived equality. To compare two records, you write a function.

This is the most restrictive approach. Austral compiles to C, and C's `==` doesn't support structs, so this is also a practical constraint.

### Zig — No `==` on regular structs

Zig does not support `==` on regular structs (compile error). Packed structs can use `==` (compares backing integer). For general struct comparison, the standard library provides `std.meta.eql` which does recursive field-by-field comparison. No operator overloading.

### Go — Implicit structural equality

If all fields of a struct are comparable types, `==` is automatically available. It performs field-by-field structural comparison. No customization possible — you cannot override `==`. If a struct contains a slice, map, or function, `==` is a compile error.

This is closest to what we want. Go's approach:
- No user customization (no hidden behavior)
- Automatic for types where it makes sense
- Compile error for types where it doesn't (slices, functions)
- Predictable behavior (always structural)

### Haskell — Opt-in `deriving (Eq)`

Types can opt into structural equality with `deriving (Eq)`. The compiler generates field-by-field comparison. Users can also write custom `Eq` instances. All fields must already have `Eq` instances.

---

## Options for Concrete

### Option A: Status quo — no `==` on user types

Keep Invariant 6 as-is. `==` only works on primitives. Compare structs with named functions.

```
fn eq(a: &Point, b: &Point) -> Bool {
    return a.x == b.x && a.y == b.y;
}

if eq(&p1, &p2) { ... }
```

**For:** Simplest. No special rules. No hidden behavior at all.
**Against:** Extremely verbose. Every struct needs an `eq` function. Deep nesting (`eq(&a.inner, &b.inner)`) gets ugly. LLMs will generate these repetitive functions constantly.

### Option B: Compiler-derived `==` (like Go)

`==` is automatically available for types where all fields support `==`. The compiler generates structural equality. No user customization.

```
struct Point { x: Float64, y: Float64 }

// == is automatically available because Float64 supports ==
if p1 == p2 { ... }    // compiler-generated: p1.x == p2.x && p1.y == p2.y
```

Rules:
- `==` on a struct compares all fields with `==`, in declaration order
- All fields must support `==`. If any field doesn't (linear type, function type, `Heap<T>`), `==` on the struct is a compile error
- `==` on an enum compares variant tags first, then variant fields
- No user customization — always structural equality
- Applies to `Copy` types only (comparing linear types is problematic — does comparison consume them? It shouldn't, but linear values can't be borrowed in the current design without Phase 6)
- After Phase 6 (borrow regions): `==` takes `&T` operands (borrows, does not consume)
- `!=` is the logical negation of `==` (also compiler-generated)
- `==` is pure — no capabilities needed

**For:** Natural syntax. Predictable behavior (always structural). No hidden dispatch — the compiler generates the code, not the user. Go proves this works at scale.
**Against:** Adds a special compiler rule for `==`. Technically violates the letter of "no operator overloading" (but not the spirit — the behavior is non-customizable and predictable).

### Option C: Opt-in `struct Eq` marker (like Haskell deriving)

Require explicit opt-in, similar to `struct Copy`:

```
struct Eq Point { x: Float64, y: Float64 }

// or: struct Copy Eq Point { ... }   (both Copy and Eq)

if p1 == p2 { ... }    // only works because Point is marked Eq
```

Rules:
- `struct Eq` (or `enum Eq`) enables `==` and `!=`
- All fields must also support `==` (recursive check, same as Copy)
- Compiler generates structural equality — no user override
- Types without `Eq` marker cannot use `==` on them
- `Eq` implies nothing about `Copy` — a `Copy` type may or may not be `Eq`, and vice versa (though in practice, most `Eq` types will also be `Copy`)

**For:** Explicit opt-in. The programmer declares intent. No surprise behavior on types that shouldn't be compared.
**Against:** Another keyword/marker to learn. If almost every type wants `Eq`, the marker becomes noise. Still adds compiler-generated `==` behavior.

### Option D: `Eq` as a built-in trait with auto-derived impl

Like `Destroy`, make `Eq` a built-in trait. But instead of requiring manual implementation, the compiler auto-generates the impl for any type where all fields have `Eq`:

```
// Eq is a built-in trait (like Destroy)
trait Eq {
    fn eq(&self, other: &Self) -> Bool;
}

// Compiler auto-generates for any type where all fields are Eq
// No user impl needed, no user impl allowed

struct Point { x: Float64, y: Float64 }
// Compiler generates: impl Eq for Point { fn eq(&self, other: &Self) -> Bool { ... } }

if p1 == p2 { ... }  // sugar for Eq.eq(&p1, &p2)
```

Rules:
- `Eq` is a built-in trait — users cannot declare it or write manual impls. Error: `"'Eq' is a built-in trait and cannot be implemented manually"`
- The compiler automatically generates `impl Eq for T` whenever all fields of `T` have `Eq` impls
- `==` is syntactic sugar for the `Eq.eq` method call
- This IS a function call (violates "no implicit function calls")
- The function is always compiler-generated structural equality — no user customization

**For:** Uses existing trait machinery. Extensible to `Ord` (comparison: `<`, `>`, `<=`, `>=`) with the same pattern.
**Against:** Violates Invariant 3 ("no hidden control flow") — `p1 == p2` is now a function call, not a primitive operation. Even though the function is non-customizable, it's still a call. Also violates "no implicit function calls" because the compiler generates the impl without the user writing anything.

---

## The Invariant Question

The core tension is with Invariant 3 and Invariant 6:

> **Invariant 3:** `a + b` on integers is primitive addition, not a method call. The compiler never inserts destructor calls.
> **Invariant 6:** Operators on primitives are built-in. They are not trait method calls.

Options B and C keep `==` as a primitive operation (not a function call) — the compiler generates inline comparison code, just like it generates `==` for integers. There is no function call, no trait dispatch, no hidden behavior. The code that runs is deterministic and non-customizable.

Option D makes `==` a trait method call, which clearly violates both invariants.

**Recommendation: Option B or C.** Both preserve the invariants. Option B (like Go) is simpler — fewer concepts. Option C (opt-in marker) gives more control but adds complexity.

---

## Extending to Ordering (`<`, `>`, `<=`, `>=`)

If we add derived `==`, should we also add derived ordering?

Structural ordering (field-by-field, left to right, lexicographic) is well-defined and predictable. But it's also arbitrary — ordering `Point` by `x` first, then `y` is a choice, and the user might want the opposite. For `==`, structural equality is always correct (two values are equal iff all fields are equal). For `<`, structural ordering is often wrong.

**Recommendation:** Add derived `==` and `!=` only. Do NOT add derived `<`, `>`, `<=`, `>=`. For ordering, require named functions (`fn compare(a: &Point, b: &Point) -> Ordering`). This avoids the "arbitrary ordering" problem.

---

## The LLM Argument

For LLMs, Option B is clearly best:
- LLMs generate `p1 == p2` naturally — less token overhead than `eq(&p1, &p2)`
- LLMs reading code see `==` and know it's structural equality — no need to find the `eq` function definition
- LLMs can't accidentally implement a broken `eq` function — the compiler generates the correct one
- Less repetitive boilerplate for LLMs to generate (no `fn eq(...)` for every type)

Option A (status quo) forces LLMs to generate `eq` functions for every struct — repetitive, error-prone, and wasteful.

---

## Recommendation

**Option B (compiler-derived, like Go).** Reasons:
1. Simplest to understand — `==` always means structural equality, for all types
2. No marker needed — if all fields support `==`, it works
3. Preserves Invariant 3 — the compiler generates inline code, not a function call
4. Preserves Invariant 6 — the behavior is non-customizable, always structural
5. LLM-friendly — natural syntax, no boilerplate
6. Go proves it works at scale in production

The invariant should be updated from "no operator overloading" to "no *user-defined* operator behavior — `==` on structs/enums is compiler-generated structural equality."

Do NOT extend to `<`/`>`/`<=`/`>=` — ordering requires user intent.

---

## Open Questions

1. **What about `Copy` requirement?** Should `==` only work on `Copy` types? Or should it work on any type where `==` can borrow both operands (`&T`)? If borrowing, this depends on Phase 6.
2. **What about `Heap<T>`?** Is `Heap<T> == Heap<T>` valid? It would need to borrow both and compare the inner values. If `T` supports `==`, should `Heap<T>`?
3. **What about enums with no fields?** `enum Direction { North, South, East, West }` — should `==` compare variant tags? (Yes, obviously.)
4. **What about recursive types?** `struct Node { value: Int, next: Option<Heap<Node>> }` — `==` would need to recursively compare. Is this always desirable?
5. **Update Invariant 6** once this decision is finalized.
