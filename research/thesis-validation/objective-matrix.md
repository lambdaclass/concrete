# Objective Matrix

**Status:** Open

This note turns the thesis-validation examples into an explicit matrix of what each example is supposed to prove.

## The Validation Axes

Each flagship example should exercise one or more of:

1. authority visibility
2. operational behavior / boundedness
3. trust-boundary visibility
4. evidence level (report, enforcement, proof)

## Example Matrix

### 1. Packet Decoder

Exercises:

1. authority visibility — parser core should not require network authority
2. operational behavior — no allocation, no blocking, bounded loops
3. evidence level — candidate for predictable-execution profile and proof-backed parser property

Does not primarily exercise:

1. broad trusted boundaries
2. package ecosystem concerns

### 2. Crypto Verification Core

Exercises:

1. operational behavior — no allocation, no blocking, bounded computation
2. trust boundary — constant-time/security-sensitive implementation pressure
3. evidence level — candidate for proof-backed correctness and boundedness claims

Does not primarily exercise:

1. authority visibility beyond "capability-free core"

### 3. ELF / Binary Inspector

Exercises:

1. authority visibility — parsing core separated from file access
2. trust boundary — layout-sensitive systems code, possible trusted boundary pressure
3. operational behavior — bounded binary parsing core

Does not primarily exercise:

1. concurrency

### 4. Ring Buffer / Fixed-Capacity Structure

Exercises:

1. operational behavior — bounded memory, no post-init allocation
2. trust boundary — ownership and low-level implementation discipline
3. evidence level — candidate for no-allocation and predictable-execution enforcement

Does not primarily exercise:

1. authority visibility

### 5. Privilege-Separated Tool

Exercises:

1. authority visibility — strong architectural separation
2. trust boundary — capability escalation should become a compiler error
3. evidence level — reportable authority structure

Does not primarily exercise:

1. bounded execution in its full application shell

### 6. Proof-Carrying Function

Exercises:

1. evidence level — Concrete-to-Lean pipeline, theorem attachment, proof-backed artifact
2. trust boundary — clear statement of what layer the theorem applies to

Does not primarily exercise:

1. full application architecture

## Recommended First Example

If only one example is used to drive the first thesis-validation slice, it should be the packet decoder.

Why:

1. it combines visible authority separation with bounded execution
2. it can be made no-allocation and non-blocking in the core
3. it has a realistic proof target
4. it is recognizably systems code rather than a toy

The intended first end-to-end slice is:

1. the parsing core passes the predictable-execution profile
2. the I/O shell fails for the right reasons
3. one small parser-core property is proved through the Concrete-to-Lean pipeline
