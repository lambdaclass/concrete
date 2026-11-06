# Async, Concurrency, and Evidence

**Status:** Open research direction
**Affects:** capability model, linearity, structured concurrency, runtime design, predictable execution, evidence reports
**Date:** 2026-05-01

## Purpose

This note captures a stronger long-term async/concurrency direction for Concrete.

It does not replace the near-term threads-first plan in [concurrency.md](concurrency.md). The current implementation order should still be:

1. document the concurrency stance
2. implement OS threads and typed channels
3. integrate reporting
4. evaluate richer async/evented runtimes later

The point of this note is to define what "later" should be aiming at if Concrete wants concurrency to strengthen the evidence story instead of becoming another runtime ecosystem split.

## Core Thesis

Concrete should not copy Rust async or generic async/await function coloring.

The best long-term shape is:

- capability-typed concurrency authority
- structured concurrency scopes
- linear task handles
- an explicit distinction between optional parallelism and required concurrent progress
- deterministic simulation as a runtime backend
- evidence reports for concurrent properties

The distinctive claim is:

Concrete should make concurrent code auditable by construction, not merely safe by convention.

## Async Versus Concurrent

Most languages collapse two different ideas:

1. two operations are independent, so either order is valid and running them in parallel is an optimization
2. two operations must make progress concurrently, or the program can deadlock

Concrete should keep these separate.

Suggested capability split:

```text
Concurrent
    |
  Async
```

`Async` means a function may start order-independent work. A backend may run that work sequentially if sequential execution is semantically valid.

`Concurrent` means a function requires real concurrent progress. A backend that cannot provide concurrent progress is not a valid instantiation for that code.

Rules:

- `Concurrent` implies `Async`.
- `Async` does not imply `Concurrent`.
- Code that only needs opportunistic overlap should use `with(Async)`.
- Code whose correctness requires simultaneous progress should use `with(Concurrent)`.

This turns a common runtime failure mode into a capability error. Code that requires real concurrency cannot be called from a context that only grants optional async authority.

## Structured Scopes

The default user-facing model should be structured concurrency, not detached futures.

Sketch:

```con
fn save_both(a: Bytes, b: Bytes) with(File, Async) -> Result<Unit, IoError> {
    scope s with(Async) {
        s.spawn(save_file, "a.txt", a);
        s.spawn(save_file, "b.txt", b);
    }
}
```

A scope owns all tasks spawned inside it.

Scope rules:

- a task cannot outlive its scope
- scope exit waits for children to terminate
- if one child errors, the scope cancels the remaining children
- spawned tasks inherit the scope's capability level
- `scope ... with(Concurrent)` requires the surrounding function to have `with(Concurrent)`

This keeps task lifetime visible in source and gives the checker a small shape to reason about.

## Linear Handles

Tasks that return values need handles.

```con
scope s with(Async) {
    let h_a = s.spawn(download, url_a);
    let h_b = s.spawn(download, url_b);

    let bytes_a = s.join(h_a)?;
    let bytes_b = s.join(h_b)?;

    process(bytes_a, bytes_b)
}
```

`Handle<T, E>` should be linear:

- it must be joined or canceled before scope close
- it cannot escape the owning scope
- race/select operations must return any still-pending loser as a fresh handle
- a handle leak is a compile error

The no-leaked-task property should be checker-enforced, not a library convention.

Spawn-without-binding should remain valid for tasks whose result is intentionally ignored:

```con
scope s with(Async) {
    s.spawn(log_event, event);
}
```

In this form, the scope owns cleanup fully. This is not detached work; it is fire-and-forget within a lexical scope.

## Cross-Task Transfer

Concrete should avoid a Rust-style `Send` contagion story.

Initial rule:

- owned linear values may move into a child task
- borrowed references may not cross task boundaries
- shared mutable state requires an explicit synchronization type and a capability such as `with(Sync)`
- channels move owned values, not borrowed references

This makes transferability a consequence of ownership, not a second structural property that infects generic signatures.

`Shared<T>` is the risk point. If it becomes too convenient, it becomes the new contagion mechanism. The design should keep shared mutable state visible, capability-gated, and uncommon.

### Borrows Across Cancellation Checkpoints

Cross-task transfer rules cover what crosses scope boundaries. A separate question is what may be held across cancellation checkpoints (`await`, blocking channel ops, `check_cancel`) within a single task.

The intended rule depends on the runtime backend:

- **Stackful coroutines.** Each task has its own stack. Borrows from the task's own stack are stable across checkpoints because the stack persists. This is the simpler model and is likely the first implementation.
- **Stackless coroutines.** The compiler transforms the task into a state machine; locals that span checkpoints become fields of a generated struct. Borrows into stack-allocated locals do not cross checkpoints in general; references must be reified or moved into the state.

The first model is the target. Stackless coroutines are out of scope for the initial implementation; if they are added later, the rules for borrows across checkpoints must be tightened to match the state-machine transform.

This is a real difference from Rust async, which uses stackless coroutines and has complex rules about what may be held across `await`. Concrete's structured-concurrency story is simpler if it commits to stackful coroutines for the evented backend.

## Race And Select

Race/select should be linear-aware from the beginning.

Pairwise sketch:

```con
scope s with(Concurrent) {
    let v4 = s.spawn(resolve_v4, name);
    let v6 = s.spawn(resolve_v6, name);

    match s.race(v4, v6) {
        First(addr, loser) => {
            s.cancel(loser)?;
            Ok(addr)
        }
        Second(addr, loser) => {
            s.cancel(loser)?;
            Ok(addr)
        }
    }
}
```

The important property is that the loser remains linear. The caller cannot observe the winner and accidentally leak the still-running task.

## Deadlines And Cancellation

Deadlines should be scope properties:

```con
scope s with(Async) deadline 30s {
    s.spawn(fetch_a, a);
    s.spawn(fetch_b, b);
}
```

Rules:

- child scopes inherit parent deadlines
- child scopes may tighten deadlines
- child scopes may not loosen deadlines
- deadline expiry requests cancellation of the scope

Cancellation should be cooperative. Preemptive cancellation cuts across linear cleanup and should not be part of safe Concrete.

Long-running CPU code that is intended to respond to cancellation should have an explicit effect such as `with(Cancellable)` or a predictable-execution obligation requiring checkpoint calls in bounded loops.

## Runtime Backends

A long-term capability implementation can have multiple backends without changing library source:

1. **Threaded**: OS threads, blocking syscalls, thread pool. This is the first implementable backend.
2. **Evented**: non-blocking I/O, readiness APIs, and stackful or stackless task suspension. This should remain later and specialized.
3. **Sim**: deterministic single-threaded simulation with virtual time, simulated I/O, and seeded scheduling.

The simulation backend is the strongest fit for Concrete's evidence story.

Simulation should make concurrency bugs reproducible:

- virtual clock instead of wall clock
- deterministic scheduler choices from a seed
- simulated network and filesystem operations where possible
- CI can run many seeds
- failures can report the seed and schedule path

This is empirical evidence, not proof, but it is still machine-checkable evidence that belongs in Concrete's report model.

## Evidence Reports

Concurrent code should extend the existing evidence vocabulary.

Candidate properties:

| Property | Possible evidence level | Source |
|---|---|---|
| no leaked tasks | compiler-enforced | linear scopes and handles |
| no data races in safe code | compiler-enforced | owned transfer, no `with(Sync)` |
| no missing-concurrency deadlock | compiler-enforced | `Async` vs `Concurrent` capability split |
| bounded task count | compiler-enforced or reported | `Tasks(N)` plus spawn analysis |
| bounded stack per task | compiler-reported or enforced | stack analysis |
| deterministic replay coverage | simulated(N) or compiler-reported | Sim backend seed runs |
| linearizable FFI region | trusted assumption | user-marked trusted boundary |

This is where Concrete can do better than mainstream async designs. The user should be able to ask "what concurrent behavior is enforced, what was simulated, and what is trusted?" and get a stable artifact answer.

## Predictable Profile Interaction

The first predictable profile should remain single-threaded. See [../predictable-execution/analyzable-concurrency.md](../predictable-execution/analyzable-concurrency.md).

After the structured model exists, a stricter analyzable concurrent profile can be considered:

- statically fixed task set
- fixed-capacity channels only
- no dynamic spawn after initialization
- no shared mutable state
- bounded queues and bounded waits
- no evented backend unless its scheduler model is part of the profile

This should be designed as a restricted profile, not the default concurrency model.

## Resource Capabilities

Concurrency should connect to bounded-resource capabilities:

```con
fn serve(req: Request) with(Concurrent, Tasks(8), Heap(64K), Time(100ms)) -> Response
```

The important design principle is that each resource claim must say what evidence backs it:

- proved
- compiler-enforced
- compiler-reported
- simulation-backed
- trusted assumption
- backend/target assumption

Do not add `Tasks(N)`, `Heap(N)`, or `Time(N)` as syntax until the checker/report pipeline can explain the source of the bound.

## No Syntactic Async Color

A practical consequence of the design: there should be no `async` keyword on functions, and no async-specific call syntax inside scopes.

A function that takes a capability parameter is just a function. A call inside a scope is a normal call:

```con
scope s with(Async) {
    s.spawn(save_file, "a.txt", data);
    process(other_data);  // ordinary call, no async marker
}
```

`save_file` itself has no `async` keyword anywhere in its definition. The same `save_file` is callable outside any scope as a sequential operation:

```con
save_file("a.txt", data);  // sequential, same function
```

The library author never chooses async or sync. The application author chooses by deciding whether to call inside a scope. This is a stronger position than Zig's design, which still has `io.async` as a syntactic marker at the call site. Concrete should avoid even that residual coloring.

The only async-specific syntax in the language is the `scope` block itself.

## Test As Capability Instantiation

The capability model enables a clean test story without runtime dependency injection. A test substitutes capability implementations at compile time:

```con
#[test]
fn test_handler() {
    let mock_clock = MockClock::at(t0);
    let mock_file = MockFile::with_contents("hello");
    handle_request(req) with mock_file, mock_clock
}
```

This subsumes:

- mocking frameworks (compile-time substitution rather than runtime patching)
- time abstraction libraries (`with(Clock)` instantiated with virtual time)
- filesystem abstraction (same mechanism)
- property-based testing of I/O (instantiate the capability with a fuzzed implementation)

Combined with the `Sim` backend, this becomes the highest-confidence test surface available in any production systems language: every external dependency is a typed capability, every capability has a test instantiation, every test has deterministic execution.

The test instantiation pipeline does not require new language machinery beyond capability polymorphism. It is a stdlib pattern enabled by the existing capability surface.

## What Not To Add

To keep this coherent:

- no detached spawn outside a scope
- no global executor
- no async function color (no `async` keyword, no async-specific call syntax)
- no actor model as the core language model
- no software transactional memory
- no broad dynamic dispatch over async functions
- no evented runtime as the first concurrency milestone

The runtime capability is the executor. The scope is the lifecycle boundary. The evidence report is the audit surface.

## One-Line Test

A Concrete concurrency feature is good if it makes concurrent code easier to audit than the equivalent Rust code, not merely shorter to write. Every primitive in this note should pass that bar; primitives that fail it should be cut.

## Implementation Order

Recommended order:

1. keep the near-term OS-thread and channel plan
2. define the formal model for scopes, spawn, join, cancel, race, and capability containment
3. add report-only concurrency facts before enforcing advanced claims
4. add structured scopes and linear handles on the threaded backend
5. add bounded channels and race/select
6. add simulation backend and seed reporting
7. add resource-bounded concurrency capabilities
8. evaluate evented I/O after stack-size and runtime-boundary work are strong enough

This sequencing preserves the current pragmatic path while giving the long-term design a sharper target.

## Relationship To Existing Notes

- [concurrency.md](concurrency.md): near-term implementation direction, still threads-first
- [long-term-concurrency.md](long-term-concurrency.md): broader structured-concurrency direction
- [channel-model.md](channel-model.md): first channel design; capacity-typed, ownership-transferring, linear handles
- [ffi-cancellation-boundary.md](ffi-cancellation-boundary.md): how cancellation interacts with FFI and trusted regions
- [../language/capability-polymorphism.md](../language/capability-polymorphism.md): polymorphism over capability sets, prerequisite for usable higher-order stdlib
- [../predictable-execution/analyzable-concurrency.md](../predictable-execution/analyzable-concurrency.md): predictable-profile restrictions
- [../predictable-execution/concurrent-stack-analysis.md](../predictable-execution/concurrent-stack-analysis.md): per-task stack bounds, prerequisite for evented backend
- [allocation-budgets.md](allocation-budgets.md): future bounded-resource capability direction
- [../proof-evidence/concurrency-formal-model.md](../proof-evidence/concurrency-formal-model.md): operational semantics, typing rules, theorems, Lean mechanization plan
- [../proof-evidence/concurrency-evidence-example.md](../proof-evidence/concurrency-evidence-example.md): worked example program with `--report concurrency` output
- [../proof-evidence/evidence-review-workflows.md](../proof-evidence/evidence-review-workflows.md): evidence artifacts and review workflow
