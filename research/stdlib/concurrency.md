# Concurrency Design

**Status:** Open research direction
**Affects:** Language design, stdlib design, runtime design, effects/capabilities
**Date:** 2026-03-09

## Purpose

This note captures the concurrency direction Concrete should explore before adding async-style language features or a larger concurrent stdlib.

The goal is not to copy Rust async. The goal is to build a concurrency model that is:

- explicit
- auditable
- ownership-friendly
- capability-tracked
- easier to reason about than today's mainstream async ecosystems

For the stronger long-term async/evented target, including the `Async` versus
`Concurrent` capability split, structured scopes, linear task handles, and
simulation-backed evidence, see [async-concurrency-evidence.md](async-concurrency-evidence.md).

## Why This Matters

Concurrency is one of the easiest places for a low-level language to become harder to read than the business logic it is supposed to express.

Rust is powerful here, but it also shows the failure mode Concrete should avoid:

- function coloring everywhere
- runtime fragmentation
- heavy trait and macro machinery
- lifetime complexity leaking into normal application code
- detached task patterns that are easy to spawn and harder to reason about later

Concrete should try to do better by keeping concurrency small, explicit, and structurally constrained.

## What Concrete Could Potentially Do Better Than Rust

Rust's async story is powerful, but it is also one of its weakest design areas:

- function coloring
- runtime fragmentation
- heavy trait and macro machinery
- awkward lifetime interactions
- code that is often harder to read than the business logic it encodes

Concrete has a real chance to do better if it stays disciplined.

What would make it better:

- one explicit concurrency model, not many overlapping ones
- explicit effects/capabilities for blocking, spawning, scheduling, time, channels, and cancellation
- no hidden allocation or runtime behavior
- explicit resource ownership across tasks
- explicit cancellation and cleanup semantics

What Concrete should avoid:

- copying Rust async directly
- building the ecosystem around macros
- making concurrency depend on huge generic abstraction towers
- hiding runtime choice everywhere

So the interesting target is not "Rust async but cleaner." The target is a smaller concurrency model that is easier to audit and explain.

## Main Direction

### Concrete's Recommended First Model

Concrete should begin with a deliberately small concurrency model:

- hosted runtime first
- OS threads as the primitive
- explicit `spawn` / `join` / channel APIs in stdlib/runtime code
- move-first ownership across thread boundaries
- shared mutable state only through explicit synchronization types
- concurrency guarded by explicit runtime/capability surfaces
- no built-in `async` / `await` in the initial model

This is the smallest model that is still useful for real systems work while staying aligned with Concrete's auditability and proof goals.

Why this is the right first target:

- it avoids hidden schedulers and hidden allocation
- it keeps runtime behavior visible in ordinary code
- it gives authority reporting something concrete to describe
- it avoids overcommitting to a large executor/cancellation model too early
- it leaves room for richer evented or async runtimes later, if they earn their complexity

The initial concurrency surface should look like library/runtime API, not new language syntax.

Examples of the intended shape:

- `thread.spawn(f, arg)`
- `thread.join(handle)`
- `chan.send(ch, value)`
- `chan.recv(ch)`

The language should avoid introducing a dedicated `async` color until the runtime model, cancellation model, and ownership transfer rules are all stable enough to justify it.

### 1. Prefer structured concurrency

Child tasks should belong to explicit scopes.

That means:

- parents know when children start
- parents cannot silently outlive child work unless this is made explicit
- cancellation and cleanup follow scope structure
- "fire and forget" should be rare and explicit

This fits Concrete much better than an unconstrained `spawn` culture.

### 2. Track concurrency through effects/capabilities

Concrete already has explicit capability/effect thinking. Concurrency should use that rather than inventing a separate hidden runtime model.

Useful concurrency-related capabilities/effects may include:

- may spawn tasks
- may block
- may wait/join
- may sleep/timer-wait
- may use networking
- may access shared synchronization primitives

The first model should gate thread creation separately from ordinary blocking or network authority. "May create concurrent work" is an important boundary in its own right.

The compiler should help distinguish these rather than collapsing them all into one generic async color.

### 3. Separate blocking, evented, and parallel work

Concrete should avoid pretending all concurrency is one thing.

At minimum, the model should distinguish:

- synchronous computation
- blocking I/O
- evented/non-blocking I/O
- parallel/threaded work

That is a better fit for auditability than one large async abstraction.

This is one of the most interesting opportunities for Concrete.

Many languages flatten these into one concurrency story even when the operational tradeoffs differ sharply:

- blocking code ties progress to threads or kernel waits
- evented code ties progress to readiness and schedulers
- parallel code ties progress to core-level execution and ownership transfer

Concrete could do better by exposing these as separate semantics rather than one generic "async" color.

The initial language/runtime stance should only standardize the threaded case. Evented I/O and richer scheduler models can come later, but they should be added as clearly different execution models rather than smuggled in under one universal abstraction.

### 4. Keep runtime access capability-based

Runtime services should not be ambient.

Code that needs:

- spawning
- timers
- networking
- sleeping
- synchronization

should require explicit capabilities or explicit runtime values.

This keeps concurrency visible in signatures and makes concurrent subsystems easier to audit.

### 5. Make cancellation explicit

Cancellation is usually where concurrency models become vague and dangerous.

Concrete should aim for:

- explicit cancellation scopes
- explicit cancellation points
- well-defined cleanup behavior under cancellation
- no hidden task abandonment

This fits the language's explicit resource-management story.

Concrete should not try to solve cancellation in the first thread/channel model. It is better to start with explicit join/termination semantics and add cancellation only after the runtime boundary and cleanup rules are fully documented.

## Recommended Staging

### Stage 1: hosted thread model

Define and implement:

- hosted-only concurrency
- OS-thread-based `spawn` / `join`
- explicit channels
- capability-gated thread creation
- move-first cross-thread ownership

Do not add yet:

- built-in `async` / `await`
- hidden executors
- cancellation semantics in the core model
- large synchronization surface area

### Stage 2: shareability and synchronization discipline

Add a small, explicit model for:

- which types may cross thread boundaries
- which types may be shared concurrently
- minimal synchronization primitives (`mutex`, perhaps atomics) only if they fit ownership cleanly

This may eventually look somewhat like `Send` / `Sync`, but it should be phrased in Concrete's own capability/ownership terms rather than copied mechanically.

### Stage 3: audit/report integration

Once concurrency exists, the compiler should be able to report:

- where threads are spawned
- where code may block or join
- which modules require concurrency/runtime capabilities
- where shared synchronization primitives appear
- whether queues or worklists are bounded or unbounded

This is one of the strongest reasons for Concrete to keep the model small and explicit.

### Stage 4: richer runtime models only if earned

Only after the thread model is well understood should Concrete evaluate:

- evented I/O runtimes
- explicit executor models
- cancellation scopes
- any eventual async syntax

Those additions should be justified by clear needs, not by imitation.

The current best candidate for that later stage is not generic async/await.
It is capability-typed structured concurrency: optional overlap through
`with(Async)`, required concurrent progress through `with(Concurrent)`, scoped
tasks, linear handles, and deterministic simulation as an evidence-producing
backend.

## High-Leverage Areas Where Concrete Could Be Better

### 1. Better structured concurrency

Rust does not force a scoped task model strongly enough.

Concrete could make task structure part of the default model rather than an optional library convention.

### 2. Better effect-tracked blocking behavior

Rust often makes it difficult to tell from a signature whether something may block, wait, or interact with a runtime.

Concrete could make this visible through capabilities/effects.

### 3. Better runtime discipline

Rust has runtime fragmentation.

Concrete could support multiple runtimes later while still requiring them to fit one explicit capability model, so libraries depend on stable concurrency contracts instead of runtime-specific conventions.

### 4. Better cancellation semantics

Most languages treat cancellation as messy or ad hoc.

Concrete could make:

- cancellation points explicit
- cancellation-safe code easier to identify
- destructor/cleanup behavior guaranteed under cancellation

### 5. Clearer ownership across tasks

Rust has `Send` and `Sync`, but the user experience is often indirect.

Concrete could make task transfer and shared access more explicit and simpler.

### 6. Better audit outputs

A Concrete compiler should eventually be able to answer questions like:

- where are tasks spawned?
- where can code block?
- which modules require concurrency/runtime capabilities?
- where are cancellation boundaries?

This is a strong fit for Concrete's compiler-as-audit-tool direction.

### 7. Runtime plurality without chaos

Concrete may eventually want multiple runtimes, but the boundary should stay explicit:

- maybe multiple runtimes exist
- but they satisfy one explicit capability model
- and libraries depend on that model, not on ad hoc runtime conventions

### 8. Safer low-level synchronization primitives

If Concrete later adds mutexes, atomics, channels, arenas, or schedulers, they should align tightly with ownership and effects instead of being thin wrappers around unsafe primitives.

### 9. Better story than "async everywhere"

A lot of systems code does not want one universal async abstraction.

Concrete should consider supporting several first-class forms cleanly:

- threads
- event loops
- callbacks with explicit context
- message passing

without forcing one abstraction over everything.

## What To Avoid

Concrete should be very skeptical of:

- copying Rust async/await directly
- making concurrency a core-language syntax story before the runtime boundary is stable
- macro-heavy concurrency APIs
- runtime-specific library conventions baked into the whole ecosystem
- implicit task detachment
- hidden executors or scheduler choice
- hidden allocation in concurrent APIs
- a single abstraction that blurs blocking, evented, and parallel work

## Useful External Models

Concrete should study other languages for constraints and structure, not for feature-copying.

### Swift

Swift's task groups are a strong example of structured concurrency with scoped child tasks, automatic waiting, and cancellation propagation.

Concrete lesson:

- task lifetimes should be nested by default
- child tasks should not silently outlive their parent scopes

Reference:

- https://developer.apple.com/documentation/swift/taskgroup

### Kotlin

Kotlin's coroutine scopes make parent/child relationships explicit and cancellation tree-shaped, even though the overall coroutine ecosystem is broader than Concrete should probably copy.

Concrete lesson:

- concurrency should be scope-owned
- cancellation and failure propagation should be structural, not ad hoc

References:

- https://kotlinlang.org/docs/coroutines-basics.html
- https://kotlinlang.org/docs/coroutines-guide.html

### Trio / nursery-style systems

The nursery idea remains one of the clearest structured-concurrency shapes available: create a scope, spawn work inside it, and do not leave the scope with children still running.

Concrete lesson:

- a small scoped spawning model may be better than a sprawling async feature set

Related references:

- https://docs.rs/async_nursery
- https://docs.rs/nursery

### libdill

libdill is useful because it demonstrates structured-concurrency ideas in a low-level setting and treats cancellation as an operational concern rather than a purely high-level abstraction.

Concrete lesson:

- scoped concurrent work can exist even in a systems-oriented setting
- cancellation should interact with blocking operations in a defined way

References:

- https://libdill.org/structured-concurrency.html
- https://libdill.org/bundle_go.html

### Erlang / OTP

Erlang/OTP is important less because of its syntax and more because of its supervision-tree discipline.

Concrete lesson:

- hierarchical supervision is a powerful systems idea
- runtime structure matters as much as task syntax

References:

- https://www.erlang.org/doc/system/design_principles.html

### Elixir

Elixir inherits the OTP supervision model and is a good reminder that task creation alone is not enough; task supervision and failure structure matter too.

Concrete lesson:

- do not focus only on spawn syntax
- include supervision and lifecycle structure in the model

### Koka / effect-oriented research languages

Effect systems and effect-polymorphic function signatures are relevant because Concrete already has explicit capability/effect thinking.

Concrete lesson:

- concurrency should be reflected in signatures where it materially changes behavior
- blocking, spawning, waiting, and runtime interaction should not be invisible

### Pony

Pony is useful because it combines actor-style concurrency with a strong aliasing/isolation story.

Concrete lesson:

- ownership and concurrency should reinforce each other
- shared-state escape hatches should not be the default

### Go

Go remains worth studying for channels and lightweight concurrency, even though Concrete should not copy its default detached goroutine style.

Concrete lesson:

- message passing is valuable
- cheap task creation alone is not enough; lifecycle structure matters

### Zig / Odin

These are useful mainly because they keep low-level costs visible.

Concrete lesson:

- do not hide runtime allocation, scheduler interaction, or ownership transfer inside concurrency helpers
- allocator and resource costs should remain visible in the API story

### Typestate / protocol-checked APIs

Several languages and libraries have shown that stateful resources are easier to use safely when their legal transitions are reflected in types.

Concrete lesson:

- channels, sockets, locks, and cancellation handles may benefit from typestate-like APIs later
- concurrency APIs should make illegal protocol steps harder to express

## What Concrete Should Probably Not Copy

### Go's default goroutine style

Go is good at lightweight concurrent execution and channels, but its default style is much less structured than what Concrete should aim for.

Concrete lesson:

- cheap spawning is not enough
- explicit structure and cancellation matter more than ease of detached spawning

### Rust's ecosystem-wide async culture

Rust has many useful pieces, but Concrete should resist:

- runtime fragmentation
- trait-heavy abstraction layers
- macro-led concurrency APIs
- hidden ecosystem conventions around executors

### Session-type-heavy concurrency as the default model

Session types are interesting and powerful, but they are probably too heavy to make Concrete's default concurrency surface.

Concrete lesson:

- protocol-checked APIs are worth remembering
- full session-type machinery should stay research until the simpler concurrency model is solid

## Candidate Building Blocks

Concrete may eventually want a combination of:

- task scopes
- explicit join handles
- explicit detached tasks
- channels/message passing
- executor/runtime capabilities
- timer capabilities
- synchronization primitives with explicit capability requirements

But these should be introduced through one coherent model, not as disconnected features.

## Relationship To Other Roadmap Work

Concurrency should not become the next major language feature until these are in better shape:

1. ABI/layout subsystem clarity
2. stronger stdlib foundations
3. explicit project/build model
4. audit-focused compiler outputs
5. more formal confidence in Core/lowering boundaries

That reduces the risk of adding a powerful but under-explained subsystem too early.

## Strongest Current Bets

If Concrete eventually grows a concurrency platform, the highest-leverage ideas look like:

1. structured concurrency
2. effect-tracked spawn/block/wait behavior
3. capability-based runtime access
4. explicit cancellation scopes and cleanup semantics
5. a first-class distinction between blocking, evented, and parallel work

Other promising ideas to remember after that:

- supervision-tree style lifecycle structure
- message passing with owned-task boundaries
- typestate-inspired APIs for stateful concurrent resources
- audit-focused compiler outputs for concurrency behavior

## Current Recommendation

The right next move is research, not implementation.

The first concrete deliverables should be:

1. define the concurrency invariants Concrete wants
2. decide what belongs in capabilities/effects
3. decide whether the base model is scoped tasks, message passing, or a hybrid
4. write down how cancellation and cleanup interact
5. only then consider syntax or stdlib APIs

## One-Line Test

A good Concrete concurrency feature should make concurrent code easier to audit than the equivalent Rust code, not merely shorter to write.

## Related Notes

- [design-filters.md](design-filters.md)
- [external-ideas.md](external-ideas.md)
- [stdlib-design.md](stdlib-design.md)
- [pre-post-conditions.md](pre-post-conditions.md)
