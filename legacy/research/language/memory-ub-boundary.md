# Memory And UB Boundary For Evidence Claims

**Status:** Open

Concrete already has an execution model. This note is narrower: for predictable-execution checks and proof-backed evidence, classify memory and failure behaviors as checked, reported, trusted, excluded, or modeled.

## Why This Matters

A function can be capability-free, no-alloc, non-recursive, and still fail a strong evidence claim if it depends on unchecked memory or target-defined behavior.

Concrete's reports should eventually make that visible instead of implying more than the compiler can justify.

## Things To Classify

For each predictable or proof-backed subset, define the status of:

1. raw pointer dereference and assignment
2. raw pointer provenance / validity assumptions
3. aliasing assumptions that matter for emitted code
4. unchecked array / pointer indexing
5. checked bounds access
6. uninitialized memory
7. integer overflow
8. narrowing casts
9. pointer-integer casts
10. abort
11. assertion failure
12. impossible / unreachable branches
13. out-of-memory paths
14. stack overflow
15. extern calls and host callbacks

## Classification Vocabulary

- `checked` — the compiler enforces the condition before the operation
- `reported` — the compiler surfaces that the operation exists
- `trusted` — evidence depends on an audited Concrete/stdlib/compiler assumption
- `excluded` — the profile/proof subset rejects the operation
- `modeled` — the proof/evidence model explicitly includes the behavior

## Near-Term Rule

The first proof-backed parser-core demo should stay in the simplest category:

1. no raw pointer operations in the proved function body
2. no allocation
3. no FFI
4. no blocking host calls
5. no trusted body
6. no OOM path
7. bounds/failure guards represented in the function's checked Core / ProofCore model

Broader claims should wait until the operation is classified.
