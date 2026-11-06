# Validation Examples

**Status:** Open

This note defines the example set that should be used to validate the thesis-level claims.

## Why Separate Example Selection Matters

The thesis-validation phase needs examples chosen for what they prove, not just for being interesting programs.

Each example should exercise one or more of:

1. visible authority boundaries
2. `NoAlloc` or bounded-allocation claims
3. predictable-execution restrictions
4. proof-backed evidence
5. trust-boundary visibility

## Candidate Examples

### 1. Fixed-buffer parser

Exercises:

1. no allocation
2. parameter-bounded loops
3. no FFI
4. no blocking

### 2. Bounded-state controller

Exercises:

1. analyzable control flow
2. no recursion
3. no allocation
4. explicit failure boundaries

### 3. Ring buffer

Exercises:

1. fixed-capacity data structure
2. no post-init allocation
3. ownership discipline
4. no blocking

### 4. Capability-separated packet decoder

Exercises:

1. visible authority
2. parser cannot reach the network itself
3. predictable-execution candidate core
4. reportable trust/authority boundaries

### 5. Small proof-carrying pure function

Exercises:

1. extraction to Lean
2. proof-backed artifact path
3. source/report/proof traceability

## What This Set Should Demonstrate

Together, the examples should let Concrete show:

1. one enforced operational restriction
2. one restricted profile
3. one report story
4. one proof story

That is enough to validate the thesis without needing a large application portfolio first.
