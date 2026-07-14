# Iteration Protocol

Status: canonical reference — ROADMAP Phase 6 #17.

Concrete has **no `Iterator` trait, no closures, and no trait-object iterator
protocol**. Traversal is expressed with a small hierarchy of explicit forms,
each of which keeps authority (capabilities) and allocation visible in the
signature. This document is the official story for which traversal form to reach
for, and it is the contract Phase 7 stdlib authors build against (see
[STDLIB_HANDOFF.md](stdlib/STDLIB_HANDOFF.md), surface `iteration`).

The forms are gated by `scripts/tests/check_iteration_protocol.sh`.

## Why no `Iterator` trait

A trait-object `Iterator` (`dyn Iterator`) hides three things a reviewer of
Concrete code must be able to see: the **allocation** behind the iterator state,
the **dynamic dispatch** of `next`, and the **capabilities** the per-element
callback may use. Concrete's thesis is that those are visible at the call site.
So instead of a universal lazy protocol, traversal is a fixed set of concrete
forms, chosen by what you are traversing.

## The hierarchy

Reach for the **first** form that fits:

### 1. `for` / indexed loops — simple bounded traversal

For counting and walking a fixed or index-addressable range, use the language
`for` loop. No library, no callback, no allocation, fully predictable:

```concrete pseudocode
for (let mut i = 0; i < n; i = i + 1) { ... }
```

This is the predictable-profile-friendly default (the loop bound is explicit;
see [PROFILES.md](PROFILES.md)).

### 2. Cursor structs — parsers and readers

For positioned, bounds-checked consumption of a byte stream (parsers, binary
readers), use a **cursor struct** that owns a position and returns `Result` on
each step. The canonical one is `ByteCursor` (`std/src/numeric.con`): `peek_u8`,
`read_u8`, advancing reads, all bounds-checked. The cursor is a plain `Copy`
value with no hidden allocation; the caller sees every step.

### 3. Capability-polymorphic `for_each` / `fold` / `map` — collections

For the standard collections (`Vec`, `Map`, `Set`, …), traversal is a method
that takes an explicit function value whose capability set is polymorphic:

```concrete pseudocode
pub fn for_each<cap C>(&self, f: fn(&T) with(C)) with(C)
pub fn fold<A, cap C>(&self, init: A, f: fn(A, &T) with(C) -> A) with(C) -> A
pub fn map<U, cap C>(&self, f: fn(&T) with(C) -> U) with(C, Alloc) -> Vec<U>
```

The `with(C)` on the method means "this traversal needs exactly the capabilities
the callback needs" — authority is threaded, not ambient. **Allocation is
visible in the signature**: `for_each` and `fold` carry no `Alloc`; `map`
allocates a new `Vec`, so its signature carries `Alloc`. A reviewer reads the
type and knows whether traversal can allocate.

### 4. Explicit context threading — stateful callbacks

Because there are no closures, a callback that needs mutable state takes that
state as an explicit `&mut Ctx` parameter. Collections provide a `_ctx` variant:

```concrete pseudocode
pub fn for_each_ctx<Ctx, cap C>(&self, ctx: &mut Ctx, f: fn(&mut Ctx, &T) with(C)) with(C)
```

The accumulator/state is a named value the reviewer can see, not a captured
environment.

## What is deliberately excluded

- **No closures.** Callbacks are top-level/`fn` values; state is threaded via
  `_ctx`. (See [CALLABLE_VALUES_AND_CAPABILITIES.md](CALLABLE_VALUES_AND_CAPABILITIES.md).)
- **No `Iterator` trait / no `dyn` trait-object iterators.** There is no lazy
  universal protocol; pick the concrete form above.
- **No hidden allocation.** Any traversal that allocates says so in its
  capability set (`Alloc`).

These exclusions are verifiable: the gate fails if an `Iterator` trait, a `dyn`
trait object, or a closure form appears, or if an allocating traversal hides its
`Alloc`.
