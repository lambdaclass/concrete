# Runtime Execution Pressure

Status: open

Phase H programs are beginning to expose runtime and execution-model questions that are larger than isolated compiler bugs.

## Pressure Signals

- deep recursion in MAL benchmarks
- stack sensitivity under heavy recursive workloads
- performance cliffs that are not just language-surface problems

## Goal

Clarify which execution-pressure issues belong to:

- language design
- runtime implementation
- stdlib support
- tooling/benchmark harnesses
- later concurrency/runtime phases

## Questions

1. What recursion/stack behavior should Concrete treat as ordinary versus out-of-scope?
2. Which workloads justify stronger runtime support rather than algorithm redesign?
3. What benchmark harness expectations belong in language examples versus external tooling?
4. What findings from Phase H should flow into the later concurrency/runtime plurality phase?

## Constraint

Do not overreact to one interpreter benchmark by prematurely redesigning the runtime. The point is to classify execution pressure honestly before later phases absorb it.
