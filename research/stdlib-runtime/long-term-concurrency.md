# Long-Term Concurrency Direction

**Status:** Open research direction
**Affects:** Language design, stdlib design, runtime design, capability model, audit/report direction
**Date:** 2026-03-15

## Purpose

This note captures the long-term concurrency target for Concrete.

It is intentionally different from the near-term concurrency note in [concurrency.md](concurrency.md):

- `concurrency.md` focuses on the first practical model Concrete should implement
- this note focuses on the best long-term shape if Concrete wants concurrency without losing clarity

The central claim is:

Concrete should not aim for one universal concurrency abstraction.
It should aim for a layered model with one small primary contract.

## The Recommended Long-Term Shape

The best long-term answer is:

- core semantic model: structured concurrency
- base runtime primitive: OS threads plus message passing
- later specialized model: evented I/O runtime
- not the primary model: unrestricted "spawn anything anywhere" async ecosystem

This gives Concrete:

- explicit parent/child lifetimes
- clear cleanup boundaries
- a better cancellation story
- a more auditable runtime model
- a more understandable capability story
- room for high-scale I/O later without making it the default for all code

## Why Structured Concurrency Should Be The Core Model

Structured concurrency is the best long-term semantic center because it gives the language a clear answer to:

- who started this concurrent work?
- who is responsible for joining or cancelling it?
- when must cleanup happen?
- how do failures propagate?
- what can still be running when a scope exits?

This matters especially for Concrete because the language already values:

- explicit authority
- explicit trust boundaries
- visible ownership/resource flow
- compiler reports that help audits

Unstructured task spawning works against all of those.

Concrete should prefer a model where concurrent work is usually scope-owned, lifecycle-visible, and hard to detach accidentally.

## Why Threads Should Be The Base Primitive

Threads are the best first and long-term base substrate for Concrete because they are:

- simple to explain
- explicit in operational cost
- familiar in low-level systems work
- compatible with a hosted runtime starting point
- easier to audit than a hidden executor ecosystem

Threads are not the final story for every workload, but they are the right base primitive because they keep the execution model legible.

Concrete should resist the temptation to make executor-driven async the default foundation.

## Why Message Passing Should Be The Default Coordination Style

Concrete should prefer actor-style and channel-style coordination patterns at the library level because they reduce the amount of shared mutable state ordinary users need to reason about.

Message passing fits Concrete well because it aligns with:

- explicit ownership transfer
- visible subsystem boundaries
- capability-aware design
- audit/report friendliness

The default "good style" should be:

- spawn explicit concurrent work
- move values into it
- communicate through channels/messages
- use shared mutable synchronization only when it is truly needed

This does not require Concrete to become an actor language.
It just means the language/runtime should bias toward clearer coordination patterns.

## Why Evented I/O Should Be A Later Specialized Layer

Evented I/O matters for high-scale networking and some latency-sensitive systems, but it should not become the language's primary concurrency identity.

It belongs later because it introduces:

- scheduler structure
- readiness vs blocking semantics
- cancellation and wakeup subtleties
- more pressure toward runtime coupling

Concrete should support it only after the thread-based structured model is clear.

When it does arrive, it should:

- live under the same explicit runtime/capability model
- not replace the thread model
- be clearly documented as a different execution regime
- avoid forcing non-networked code into async coloring

The goal is "specialized tool for specialized workloads," not "everything becomes async."

## What Concrete Should Explicitly Avoid

Concrete should avoid centering its long-term concurrency story on:

- unrestricted detached spawning
- ambient runtime access
- hidden executors
- runtime fragmentation baked into every library surface
- one abstraction that blurs blocking, evented, and parallel work
- an ecosystem where ordinary code is forced into async signatures for unrelated reasons

This is the main trap to avoid:

do not build a Rust-style async fragmentation story with slightly cleaner syntax.

That would cost a lot of complexity while cutting directly against Concrete's auditability goals.

## The Layered Model In Practice

The long-term layering should look like this:

### Layer 1: semantic contract

Structured concurrency as the default meaning of concurrent work:

- concurrent work belongs to scopes
- parent/child lifetime structure is explicit
- cleanup and eventual cancellation follow structure

### Layer 2: base runtime primitive

OS threads plus explicit join/message passing:

- thread creation is visible
- blocking is visible
- ownership transfer is explicit

### Layer 3: controlled shared-state tools

Only after the transfer/message-passing model is solid:

- mutexes
- atomics
- small synchronization types

These should remain explicit and constrained, not the default style.

### Layer 4: specialized runtime models

Only when clearly justified:

- event loops
- non-blocking/network runtimes
- executor-style systems

These should fit one explicit contract instead of creating separate concurrency cultures.

## Why This Fits Concrete Better Than "Async Everywhere"

Concrete wants to be:

- explicit
- inspectable
- proof-oriented
- capability-aware

Structured concurrency with threads-first layering matches that.

"Async everywhere" tends to create:

- function coloring pressure
- hidden runtime assumptions
- detached lifecycle complexity
- harder-to-read business logic
- more difficult audit outputs

Concrete should optimize for clarity first and specialization second.

## The Ideal End State

In the best long-term version of Concrete:

- most ordinary concurrent programs use structured thread-and-channel patterns
- high-scale networking can use an evented runtime without infecting the entire language
- compiler reports can explain where concurrency happens, where blocking occurs, and what runtime authority is required
- high-integrity profiles can constrain concurrency deliberately
- future proofs/reports can reason about concurrency as a small explicit model, not a sprawling ecosystem accident

That is a much stronger long-term position than "support every concurrency style equally."

## Relationship To Other Research

- [concurrency.md](concurrency.md): near-term first implementation direction
- [high-integrity-profile.md](high-integrity-profile.md): how stricter profiles may restrict concurrency
- [trust-multipliers.md](trust-multipliers.md): how audit/report/evidence work could reinforce explicit runtime and concurrency boundaries
- [complete-language-system.md](complete-language-system.md): why concurrency is part of becoming a complete system rather than just a compiler

## Working Conclusion

If Concrete succeeds here, its long-term concurrency identity should be:

- structured by default
- threads-first underneath
- message-passing biased
- evented when specialized workloads justify it
- explicit in capabilities and runtime boundaries
- resistant to async fragmentation

That is probably the best concurrency direction available to Concrete.
