# Concurrent Stack Analysis

**Status:** Open research direction
**Affects:** predictable execution, structured concurrency, runtime backends, evidence reports
**Date:** 2026-05-01

## Purpose

This note extends [stack-bounds.md](stack-bounds.md) to per-task analysis. The existing note covers what Concrete can say about stack use in single-threaded code. The structured-concurrency direction in [../stdlib-runtime/async-concurrency-evidence.md](../stdlib-runtime/async-concurrency-evidence.md) introduces spawned tasks, each of which needs its own stack. The question is what the compiler can say about per-task stack bounds and how that feeds the evented runtime backend.

This is a place where Concrete is positioned to do something other languages can't yet. Zig is explicitly waiting on stack analysis as a language feature before its evented runtime can ship cleanly. Concrete already has the analysis pass for sequential code; extending it to per-task is incremental work, not a new mechanism.

## Why Per-Task Stacks Matter

A spawned task needs its own stack. The runtime has three options:

1. **One large default stack per task** (e.g., 8 MB). Wastes memory; limits the number of concurrent tasks.
2. **Allocate stack on demand and grow it.** Requires a relocating allocator or split stacks; defeats predictable-execution claims.
3. **Allocate exactly the stack size the task needs**, computed at compile time.

Option 3 is the cleanest fit for Concrete's positioning. It requires the compiler to compute a per-task stack bound. The threaded backend can use it for sizing. The evented backend depends on it for serious deployment — packing thousands of tasks into reasonable memory requires that each task's stack be small and known.

## What The Existing Analysis Provides

The current single-threaded analysis (per [stack-bounds.md](stack-bounds.md)) provides:

1. Recursion presence/absence per function.
2. Static call depth where the call graph permits it.
3. Functions with large stack-allocated locals.
4. Stack-sensitive lowering patterns.

This is enough to compute a per-function stack bound for non-recursive functions on a fixed target. The bound has known limitations: calling-convention assumptions, backend frame layout decisions, optimization-dependent inlining.

## Per-Task Bound

A task's stack bound is the bound for its entry function plus the stack required by anything that function calls, transitively, including any nested spawns it performs (which spawn into the same scope but consume separate stacks).

For the bound to be computable:

1. The task entry must be a function whose stack bound is known.
2. The function must not recurse without a bound.
3. The function must not call function values whose targets are unknown statically.
4. The function must not invoke FFI that itself uses unknown stack.

The first three are usually satisfied in well-typed Concrete code. The fourth is the boundary case; FFI calls should be marked with a stack assumption (a trusted attribute) or rejected from the analysis.

## Capability For Stack Bounds

A `with(Stack(N))` capability bounds the maximum stack a function may use, transitively.

```con
fn handle_request(req: Request) with(Stack(16K)) -> Response { ... }
```

Reading: this function uses at most 16K of stack. The compiler verifies the bound by computing the per-function bound and checking it. If the function spawns tasks, each spawned task's stack is separate; the spawning function's own stack is what the capability bounds.

For tasks spawned inside a scope:

```con
scope s with(Async, Stack(8K)) {
    s.spawn(worker, ...)  // worker's stack must be ≤ 8K
}
```

The scope's stack capability bounds each spawned task. This is different from a heap budget; each task gets its own 8K stack, not a shared 8K pool.

A separate `with(Heap(N))` capability bounds heap allocation; the two are independent.

## What The Analysis Reports

For each function:

1. Maximum stack frame size (computed from locals + spilled registers + alignment).
2. Maximum call depth in the transitive call graph.
3. Total maximum stack use along the worst path.
4. Whether the bound is known (no unbounded recursion or unknown FFI on the path).
5. The source of any unknown components (specific FFI calls, recursive cycles).

For each spawned task:

1. The entry function's bound.
2. The runtime's allocation for that task's stack (per backend).
3. Whether the runtime allocation matches or exceeds the bound.

## Backend Interactions

The bound is consumed differently by each runtime backend.

**Threaded backend.** OS threads come with default stack sizes (usually a few MB). The bound is informational unless the backend uses `pthread_attr_setstacksize` (or equivalent) to size threads precisely. The bound can also be reported for audit even when not used for sizing.

**Evented backend.** Each task's stack is an allocation in the evented runtime's memory. The bound is consumed directly: allocate exactly the bounded size (plus a small safety margin). Without the bound, the runtime must guess; guessing wastes memory or risks overflow.

**Sim backend.** Each simulated task has a virtual stack. The bound determines simulation memory use. The bound is also useful for testing: a deliberate bound violation in a simulation seed surfaces overflow bugs before real deployment.

## Recursion And Unknown Bounds

A function with unbounded recursion has no static stack bound. Such a function cannot be spawned with a `with(Stack(N))` capability.

Three options for handling recursion:

1. **Reject from analyzable profiles.** The first predictable-execution profile already excludes recursion. Recursive code can run, but its stack bound is reported as unknown.
2. **Tail-call elimination by the lowering pass.** Tail-recursive code has a constant stack bound; the compiler should recognize and report this.
3. **Bounded-recursion attribute.** The user asserts a recursion bound; the compiler accepts it as a trusted assumption and uses it to compute a stack bound.

The third option is a last resort. The first two should cover most cases.

## FFI Considerations

FFI calls have unknown stack bounds unless declared. Three approaches:

1. **Reject FFI from stack-bounded scopes.** Strict but clear. Code requiring `with(Stack(N))` cannot cross into unannotated foreign code.
2. **Require FFI declarations to include stack assumptions.** The user asserts "this foreign function uses at most M bytes of stack." The compiler propagates the assumption and marks it as trusted.
3. **Allow FFI with a configurable safety margin.** The bound is "known portion + FFI margin"; the margin is conservative.

The first option is the right starting point. The second can be added once the FFI declaration syntax is rich enough to carry assumptions.

This connects to [ffi-boundaries.md](ffi-boundaries.md) and [../stdlib-runtime/ffi-cancellation-boundary.md](../stdlib-runtime/ffi-cancellation-boundary.md): FFI is the analysis boundary in many predictable-execution questions, not just stack.

## Evidence Levels

| Property | Possible level | Source |
|---|---|---|
| recursion presence | enforced | structural |
| max stack frame size | enforced (per target) | lowering output |
| transitive call depth | enforced (when graph is closed) | static analysis |
| total stack bound | enforced (or reported with caveats) | combined analysis |
| `with(Stack(N))` honored | enforced | bound check at compile time |
| matches runtime allocation | reported | backend assumption |

The transition from reported to enforced depends on:

1. Closing the call graph (no unknown function calls).
2. Excluding or annotating FFI.
3. Pinning calling convention and frame layout per target.

The first two are user-facing; the third is a compiler responsibility tied to the target.

## What This Replaces

Without per-task stack analysis:

- Runtimes guess stack sizes, wasting memory or risking overflow.
- Embedded targets fall back to small global stacks per task and crash on edge cases.
- Audit of memory use depends on hand-counting locals and call depth.

With per-task stack analysis, the audit story has a typed bound for stack consumption, the evented runtime can size tasks precisely, and the predictable-execution profile gains a substantive memory claim alongside its existing time claims.

## What Not To Try Yet

- Exact byte bounds across compiler versions. The bound depends on lowering decisions that may change. Reports should fix the target and the compiler version; bounds are not portable across builds.
- Bounds for recursive code in the general case. Bounded recursion via attribute is acceptable; unrestricted recursion is not.
- Bounds across FFI without declarations. The trusted assumption belongs to the FFI declaration, not the analysis.
- Promoting the bound to "proved" status without a Lean correspondence. The lowering pass is not formally verified end-to-end; the bound is enforced, not proved.

## Implementation Sequence

1. Extend the existing per-function stack analysis to track frame size and call depth.
2. Add a transitive bound computation across the closed call graph.
3. Implement the `with(Stack(N))` capability and its compile-time check.
4. Report stack bounds in `--report concurrency` and `--report stack`.
5. Wire the threaded backend to optionally use the bound for thread sizing.
6. Wire the evented backend to consume the bound for task allocation.
7. Add per-target frame-layout pinning for byte-precise bounds.

Steps 1-4 are valuable independent of any concurrency runtime; they strengthen the existing predictable-execution story. Steps 5-6 are runtime-specific and should follow the structured-concurrency implementation.

## One-Line Test

The stack analysis is good if a function declared `with(Stack(N))` can be compiled, audited, and deployed with confidence that no input causes the task's stack to exceed N bytes — without depending on runtime probing or guard pages.

## Relationship To Other Notes

- [stack-bounds.md](stack-bounds.md) — single-threaded baseline; this note extends it
- [predictable-execution.md](predictable-execution.md) — umbrella for predictable-execution direction
- [analyzable-concurrency.md](analyzable-concurrency.md) — predictable concurrent profile; per-task stack bound is part of it
- [bounded-loops.md](bounded-loops.md) — sister analysis for loops; similar bound discipline
- [ffi-boundaries.md](ffi-boundaries.md) — FFI cuts across this analysis
- [../stdlib-runtime/async-concurrency-evidence.md](../stdlib-runtime/async-concurrency-evidence.md) — concurrency direction; depends on this for evented backend
- [../stdlib-runtime/allocation-budgets.md](../stdlib-runtime/allocation-budgets.md) — sibling resource-bounded capability for heap
- [../proof-evidence/concurrency-evidence-example.md](../proof-evidence/concurrency-evidence-example.md) — stack appears in worked evidence example
