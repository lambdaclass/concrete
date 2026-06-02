# Capability Polymorphism

**Status:** Open research direction
**Affects:** capability model, type system, stdlib design, higher-order functions, concurrency
**Date:** 2026-05-01

## Purpose

This note defines how higher-order code in Concrete should remain usable when callbacks have arbitrary capability requirements. Without an answer here, the standard library either bans higher-order code or duplicates every combinator across capability sets.

This is a prerequisite for the structured concurrency direction in [async-concurrency-evidence.md](../stdlib-runtime/async-concurrency-evidence.md). Scope methods like `s.spawn(f, ...)` and combinators like `iter.map(f)` need a single signature that works whether `f` is pure, does I/O, spawns tasks, or holds `with(Concurrent)`.

## Framing

Concrete's capabilities are static permission gates, not algebraic effects. A `with(File)` annotation on a function is a check at the call site that the calling context holds the `File` permission. There is no handler, no resume, no continuation capture, and no runtime semantics associated with the capability itself.

This note is therefore about **permission-set polymorphism**, not effect-row polymorphism. The mathematical structure is sets of permissions with union, subset, and lattice subsumption — closer to Rust `where` bounds, modular implicits, and capability-calculus prior art than to Koka's effect rows or Effekt's effect handlers. Languages that handle effects (Koka, Effekt, Frank, OCaml 5) face elaboration questions that do not arise here, because permissions are checked, not handled.

Keep this distinction visible throughout the design. Treating capabilities as a kind of effect imports complexity Concrete deliberately does not need.

## The Problem

A simple `map` looks like:

```con
fn map<T, U>(xs: List<T>, f: fn(T) -> U) -> List<U>
```

This signature only accepts pure callbacks. The moment a user wants to map a function that prints, allocates, or reads a file, the signature is wrong. The naive workaround is to duplicate the combinator:

```con
fn map<T, U>(xs: List<T>, f: fn(T) -> U) -> List<U>
fn map_io<T, U>(xs: List<T>, f: fn(T) with(File) -> U) with(File) -> List<U>
fn map_async<T, U>(xs: List<T>, f: fn(T) with(Async) -> U) with(Async) -> List<U>
```

This explodes combinatorially as the capability lattice grows. Every user-defined combinator has the same problem. The result is either a tiny stdlib that refuses higher-order use, or a sprawling stdlib that bakes capability assumptions into every API.

Capability polymorphism is the way out: let `map` carry whatever its callback carries, no more and no less.

## Core Idea

Add a type-parameter form for capability sets. A function can be generic in the capability set it requires.

Sketch:

```con
fn map<T, U, C>(xs: List<T>, f: fn(T) with(C) -> U) with(C) -> List<U>
```

Reading: `map` requires whatever capability set `C` its callback `f` requires. If `f` is pure, `C` is empty and `map` is pure. If `f` requires `with(File)`, `map` requires `with(File)`. If `f` requires `with(Async, File)`, `map` requires `with(Async, File)`.

The capability set `C` is a *type-level* value. Concrete already has the elaboration machinery for type parameters; this extends it to capability sets.

## Capability Set Operators

A working polymorphic system needs at least three operators on capability sets:

1. **Singleton:** `{File}` is a capability set containing one element.
2. **Union:** `C1 ∪ C2` is the smallest capability set containing both. Useful for composition: a function that calls two callbacks with capability sets `C1` and `C2` requires `C1 ∪ C2`.
3. **Subset:** `C1 ⊆ C2` is the rule that `with(C2)` may call code requiring `with(C1)`.

The capability lattice already has subsumption rules (`Concurrent` implies `Async`, `Std` implies most platform capabilities). Polymorphism builds on those: a generic signature `with(C)` is satisfiable by any caller that holds a superset of `C`.

## Subtyping Rules

The lattice has built-in implications:

- `Concurrent` implies `Async`.
- `Std` implies the platform capabilities included in `Std`.
- Resource-bounded capabilities tighten by intersection: `Heap(64K)` implies `Heap(N)` for any `N >= 64K`.

These should compose with polymorphism cleanly. A function declared `with(Async)` is callable from a `with(Concurrent)` context because `{Concurrent} ⊇ {Async}` (after lattice expansion).

The checker needs to expand capability sets through the lattice before comparing them. This is the same kind of subtype check Concrete already performs, applied to capability sets instead of types.

## Inference

Most capability-polymorphic uses should not require explicit annotations.

Inference rules:

1. If a function calls a callback with capability set `C`, the function's required capability set includes `C`.
2. If a function calls multiple callbacks, the required set is the union.
3. The function's declared capability set must be a superset of the inferred set.

For library authors, this means the polymorphic parameter `C` is usually elidable in higher-order signatures. A practical surface might allow:

```con
fn map<T, U>(xs: List<T>, f: fn(T) -> U) with(f) -> List<U>
```

Where `with(f)` reads as "with whatever capabilities `f` requires." This is a notational shortcut for the explicit `<C>` form. The mechanism is set propagation: take the union of the capability sets of all callbacks called inside the function body and require that union on the function itself.

## Bounds On Capability Sets

Some combinators want to *constrain* the capability set rather than just propagate it. Examples:

- A pure `map` that rejects callbacks with side effects: `with(C) where C ⊆ {}`.
- A spawn combinator that requires the callback to be safe to run on another task: `with(C) where C ⊆ {Async, File, Net, ...}` — explicitly excluding `Concurrent` because the spawned task might run sequentially.
- A bounded-resource combinator that rejects callbacks whose resource bounds exceed a budget.

Bounds on capability sets are the analog of trait bounds in Rust generics. The syntax should make them visible without being noisy. A possible form:

```con
fn map_pure<T, U>(xs: List<T>, f: fn(T) -> U) where caps(f) ⊆ {} -> List<U>
```

This is more research than the basic propagation case. The basic case should ship first.

## Interaction With Concurrency

The structured concurrency direction depends on capability polymorphism. Specifically:

```con
s.spawn<F, T, E, C>(f: F, args: ...) -> Handle<T, E>
    where F: fn(args) with(C) -> Result<T, E>,
          C ⊆ caps(s)
```

Reading: `s.spawn` accepts a callback with any capability set `C` that is a subset of the scope's capability set. A scope opened with `with(Async)` accepts callbacks with `with(Async)` or weaker; a scope opened with `with(Concurrent)` accepts everything `Async` accepts plus `Concurrent`-requiring callbacks.

Without this rule, `spawn` either accepts only pure functions or hard-codes a fixed capability set. Neither is acceptable for a real stdlib.

## Interaction With Linear Types

Capabilities are not linear in the value-uniqueness sense. They are static type-level annotations on functions, not runtime tokens. A function declared `with(File)` does not consume a `File` token; it requires that the calling context holds the `File` capability authority.

This matters because polymorphic capability sets compose through type checking, not through ownership transfer. A combinator can be polymorphic in capabilities without affecting the linearity of the values it manipulates.

The two systems are independent. Capabilities track *what authority* is required; linearity tracks *what values* must be consumed.

## Capabilities Are Not Effects

This is the most important framing point in the note, worth stating directly.

Concrete's capabilities are not effects. The differences:

| Property | Algebraic effects | Concrete's gated capabilities |
|---|---|---|
| Term-level reification | Effects are values; operations of an effect can be invoked | Capabilities are static markers; never appear as runtime values |
| Handlers | Effects can be handled, transformed, intercepted | Capabilities cannot be handled; they are checked or rejected |
| Continuations | Handlers may capture or resume continuations | No continuation machinery |
| Runtime cost | Possible (handler dispatch, continuation allocation) | None; capabilities are compile-time only |
| Polymorphism | Effect rows with row variables, often with row inference | Permission sets with subset/union and lattice subsumption |
| Audit surface | Larger; handler resolution depends on lexical and dynamic context | Smaller; static check at the call site |

This means polymorphism over capability sets is a strictly simpler problem than polymorphism over effect rows. The elaboration is set propagation, not row unification. Error messages can name the missing capability rather than explaining why an effect variable failed to instantiate.

The simpler model is the right fit for Concrete's evidence-bearing positioning. There is no plan to add effect handlers, and this note does not assume them as a future direction.

When this note refers to combinators like `map` or scope methods like `spawn`, the polymorphism is over the *permission set* the callback requires, full stop. Anything that would be called an "effect" in Koka or Effekt is, in Concrete, either:

1. A capability the function requires (permission, no runtime semantics), or
2. A linear value it consumes (resource, no permission semantics), or
3. Both, independently tracked by separate parts of the type system.

## What Concrete Should Ship First

A minimal polymorphism feature set:

1. Capability-set type parameters in function signatures.
2. Inference of required capability set from callback uses.
3. Capability-set elision shortcut (`with(f)` or equivalent) for the common case.
4. Subtype check on capability sets that respects the lattice.
5. Compiler error messages that explain capability mismatch in terms of the lattice (`with(File)` required by `f`, not held by caller).

This is enough to write a usable higher-order stdlib without combinatorial duplication.

What can wait:

1. Bounds on capability sets (`where C ⊆ {...}`).
2. Capability-set difference / subtraction.
3. First-class capability values at runtime.

Effect handlers are not on the list; they are not a planned future direction.

## Prior Art

The closest matches operate on permissions or aliasing capabilities, not effects:

- **Capability calculus** (Walker, Crary, Morrisett, and successors) formalizes static capabilities as permission tokens that can be required, granted, and consumed in well-typed code. The polymorphism story uses set operations rather than row machinery. This is the closest theoretical match.
- **Wyvern** (Aldrich's group at CMU) is a capability-safe object language. Its capability-passing discipline is similar to Concrete's gated capabilities, including the no-ambient-authority position.
- **Pony** has reference capabilities (`iso`, `ref`, `val`, `tag`) that gate aliasing and cross-actor transfer. The domain is different from authority capabilities, but the static-gate-with-lattice-subsumption shape is the same.
- **Modular implicits** (Carette, Kiselyov, White) and **Scala implicits** show how to elaborate generic code that requires implicit context without effect-handler machinery. This is the right kind of polymorphism inference for Concrete.
- **Rust's `where` bounds** provide the user-facing surface analog: a function declares a constraint on its type parameters; the compiler checks at the call site. Capability bounds in Concrete should feel similar.

Adjacent but different — these handle effects rather than gating permissions:

- **Effekt** (Brachthäuser et al.) — second-class capabilities with handlers and lexical scoping. Closer to Concrete than Koka, but still effect-handler-based.
- **Koka** (Daan Leijen / Microsoft Research) — row-polymorphic algebraic effects.
- **Frank** (Lindley / McBride / Hammond) — effect polymorphism via "effect ability."
- **OCaml 5** — effect handlers without static checking.
- **Roc** — abilities; exploring effect-style propagation.

These are worth reading for inference and error-message techniques, but their elaboration concerns (handler resolution, effect-row unification) do not arise in Concrete. A capability-polymorphic stdlib that produces unintelligible type errors when callers misalign is worse than one that requires explicit annotations; the lessons about diagnostics transfer even when the underlying mechanism does not.

## What Not To Add

- No first-class capability values at the term level. Capabilities should remain static signatures, not values that flow through the program. First-class capabilities are a different language with a different audit story.
- No row-polymorphic effects in the Koka style. The expressiveness gain is real but the syntax cost and elaboration complexity are also real, and the simpler set-polymorphic story is enough for stdlib use cases.
- No implicit capability widening. A `with(File)` function is not silently promoted to `with(File, Net)`. Widening is always explicit at the call site or via subsumption through the lattice.
- No capability subtraction in the user-facing surface. `C1 - C2` adds checker complexity and rarely answers a question users actually have.

## What Compiler Reports Should Show

For each higher-order signature, the compiler should be able to report:

1. The declared capability set (with type parameters expanded).
2. The inferred capability set from the function body.
3. Whether the inferred set matches the declared set (mismatch is an error).
4. For each call site, the capability set required at that site after substitution.

This connects capability polymorphism to the broader evidence story: a function's permission surface is a reportable fact, not a hidden implementation detail.

## One-Line Test

A capability-polymorphic feature is good if writing a higher-order combinator requires no more annotation than writing the same combinator in a non-polymorphic language, while the call-site capability set is still computed correctly and reported.

## Relationship To Other Notes

- [../stdlib-runtime/async-concurrency-evidence.md](../stdlib-runtime/async-concurrency-evidence.md) — depends on this for scope/spawn signatures
- [../stdlib-runtime/concurrency.md](../stdlib-runtime/concurrency.md) — near-term threads-first plan; capability polymorphism is needed before the long-term direction
- [../stdlib-runtime/stdlib-design.md](../stdlib-runtime/stdlib-design.md) — stdlib shape; this note constrains it
- [no-trait-objects.md](no-trait-objects.md) — related decision about higher-order machinery
- [../stdlib-runtime/iterators.md](../stdlib-runtime/iterators.md) — iterators are the canonical higher-order combinator surface
