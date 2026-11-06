# Performance Research Packets

**Status:** Open

Concrete should eventually emit one small, agent-readable bundle that tells a human or LLM optimization agent where performance work may matter and what semantic guardrails must not regress.

This is not "let an LLM randomly optimize the compiler."

It is: give an optimizer the same checked facts that Concrete gives auditors.

## Why This Matters

Concrete's vision is not only "like Rust/Zig, but proved."

Concrete can expose facts that most systems languages do not put in one place:

- what each function can touch
- which functions allocate
- which functions block
- which functions cross FFI / trusted / backend boundaries
- which functions are predictable-profile compatible
- which functions are proved, enforced, reported, stale, or trusted-assumption-based

Those facts are useful for performance research because a change that is faster but adds allocation, authority, FFI, blocking, stale proof, or trusted code may be a bad trade.

## The Packet

A `performance research packet` should eventually contain:

1. repository / commit / build identity
2. compiler version and target triple
3. current benchmark table
4. perf-baseline delta
5. compile-time summary
6. test-suite timing summary
7. binary-size summary
8. LLVM IR / SSA size summary
9. allocation/effects report summary
10. predictable-profile summary
11. proof/evidence summary
12. top changed functions since a baseline, if available
13. known performance hypotheses from research/docs/bugs
14. known correctness / evidence guardrails
15. exact commands to reproduce benchmark and report outputs

## Useful Agent Questions

The packet should help an agent answer:

1. which examples are slow?
2. which outputs got bigger?
3. which functions allocate on the hot path?
4. which functions call through stdlib wrappers instead of lowering cleanly?
5. did a proposed optimization widen authority?
6. did it add `Alloc`, FFI, blocking, recursion, unbounded loops, or trusted code?
7. did it change a proved function's fingerprint?
8. did it improve runtime but make IR/binary/codegen obviously worse?

## Near-Term Form

Start simple:

1. update the performance baseline command so it can print one compact markdown or JSON summary
2. include command lines for `--report effects`, `--check predictable`, selected `--report alloc`, and perf tests
3. link current optimization notes and known bug reports
4. keep it generated or reproducible; do not maintain a hand-edited stale dashboard

## Non-Goals

- no autonomous benchmark-chasing without report/evidence regression checks
- no target-specific optimization oracle yet
- no claim that an LLM knows whether an optimization is semantically valid
- no performance win that hides allocation, authority, trust, or proof drift
