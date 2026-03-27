# Analyzable Concurrency

**Status:** Open

This note defines how concurrency should interact with the predictable-execution profile.

## Why This Needs A Separate Answer

Concurrency is the single largest threat to execution predictability after unbounded loops and allocation.

Even if a program has no recursion, no allocation, and bounded loops, adding unrestricted threads or shared mutable state reintroduces:

1. unbounded scheduling delays
2. priority inversion
3. deadlock
4. timing-dependent behavior that no source-level analysis can fully capture

The predictable-execution profile must take a position on concurrency, not defer it.

## First Profile Rule

The first predictable-execution profile should be **single-threaded only.**

1. thread creation is a compile error in the restricted profile
2. shared-mutable-state primitives (mutexes, atomics beyond load/store) are compile errors
3. channel operations are compile errors
4. no implicit runtime threads or background work

This is the simplest honest answer. It avoids pretending that concurrency is analyzable before the concurrency model (Phase M) even exists.

## Why Not Ravenscar Immediately

A Ravenscar-style restricted concurrency model (fixed task set, no dynamic creation, ceiling-priority locking) is a plausible second profile. But it requires:

1. a concurrency model to restrict (Phase M must land first)
2. a scheduler model that is explicit enough to reason about
3. priority semantics that are real, not aspirational
4. validation on actual bounded concurrent examples

Shipping Ravenscar-style restrictions before the base concurrency model exists would produce rules with nothing to enforce.

## Second Profile Direction

When Phase M lands structured concurrency with OS threads and message passing, the second analyzable profile should likely allow:

1. a fixed, statically known set of tasks
2. no dynamic thread creation
3. fixed-capacity channels only
4. no blocking synchronization beyond bounded channel operations
5. no shared mutable state

This resembles Ravenscar but adapted to Concrete's ownership and capability model rather than Ada's tasking model.

## What The Compiler Should Report

Even before enforcement, the compiler should report:

1. whether a function spawns threads
2. whether a function uses shared-mutable-state primitives
3. whether a function performs channel operations
4. whether the concurrency graph is statically fixed or dynamic

## Relationship To Phase M

Phase M (Concurrency) defines the language model. This note defines the restrictions the predictable-execution profile places on that model.

The two should be designed together:

1. Phase M should not introduce concurrency primitives that make analyzable restriction impossible
2. the predictable-execution profile should not assume concurrency features that Phase M hasn't committed to

The constraint flows both ways.

## Relationship To Other Notes

1. [predictable-execution.md](predictable-execution.md) — umbrella
2. [blocking-effects.md](blocking-effects.md) — blocking from synchronization overlaps here
3. [../../stdlib-runtime/concurrency.md](../stdlib-runtime/concurrency.md) — Phase M base design
4. [../../stdlib-runtime/long-term-concurrency.md](../stdlib-runtime/long-term-concurrency.md) — longer-term concurrency directions
