# Should Concrete Have `union`?

**Status:** Open research question. Not on the main roadmap.

## Short Answer

Maybe, but only for narrow low-level use cases and only with explicit unsafe rules.

`union` is not needed for Concrete's core language design. It only becomes interesting if Concrete wants stronger C interop and more explicit representation-level programming than `struct` and `enum` can express comfortably.

## Why Consider It

A `union` gives overlapping storage. That is useful for:

- C interop with existing APIs and ABIs
- packet/header parsing
- manual tagged representations
- low-level runtime/value layouts
- memory overlays where the programmer really wants one storage slot viewed in multiple ways

These are real systems-programming cases. They are not "ergonomic sugar" cases.

## Why Be Careful

`union` interacts badly with Concrete's core values if added casually:

- the active interpretation of the bytes can become unclear
- reads can become semantically ambiguous
- layout and ABI rules become much more important
- it naturally pulls toward unsafe behavior
- it can undermine the language's "what you see is what executes" discipline if too much is implicit

Concrete should not add a high-level, magical, automatically-tracked union feature.

## What A Concrete `union` Would Need

If Concrete ever adds `union`, it should likely have these properties:

- explicit declaration syntax
- explicit layout rules
- no hidden active-field tracking
- no implicit safety story
- reads of a different field than the one last written should be `Unsafe`
- clear FFI motivation
- a precise interaction with `repr(C)`

That means a possible design is:

- writing one field is allowed
- reading the same field is allowed
- reinterpreting the bytes as another field requires `with(Unsafe)`

This keeps the model explicit.

## Why It Is Not On The Roadmap

Concrete has higher-priority work first:

- finish the compiler architecture
- make Core and SSA authoritative
- add `newtype`
- add `repr(C)` and sharpen layout rules
- make `unsafe` smaller and more precise

Only after those are solid does `union` become worth considering.

## Current Recommendation

Do not add `union` now.

Revisit it only after:

1. `repr(C)` exists
2. ABI/layout rules are clearly documented
3. the `unsafe` boundary is sharp
4. there is a concrete low-level use case that `struct`/`enum` cannot express well enough
