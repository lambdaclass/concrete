# Core Thesis

**Status:** Open

Concrete is trying to become a small systems language for evidence-carrying software.

It is built in Lean 4 and aims to make four things explicit enough to audit, restrict, report, and prove at the function boundary.

## The Four Axes

### 1. Authority

What resources can this function touch?

Examples:

1. file
2. network
3. process
4. randomness
5. FFI

Concrete already has the beginning of this through capability-visible signatures.

### 2. Operational Behavior

How predictably does this function behave?

Examples:

1. no allocation / bounded allocation / unbounded allocation
2. may block / does not block
3. recursion present / absent
4. loop bounds known / unknown
5. bounded or unbounded execution shape

This is the predictable-execution side of the thesis.

### 3. Trust Boundary

What trusted or external boundaries does this function cross?

Examples:

1. uses `trusted`
2. crosses FFI
3. depends on backend assumptions for stronger claims

This is the trust-story side of the thesis.

### 4. Evidence Level

What backs the claim being made about this function?

Examples:

1. report-only
2. compiler-enforced profile
3. proof-backed
4. trusted assumption

This is the proof-and-evidence side of the thesis.

## The Claim

Concrete is not trying to be "just another safe systems language."

It is also not trying to be "Lean without a garbage collector" or "Rust with proofs."

The deeper claim is:

1. authority can be visible
2. operational behavior can be surfaced and restricted
3. trust boundaries can stay explicit
4. evidence can be attached to real compiler artifacts

If this works, a reviewer should be able to ask far more precise questions about a function than most languages make practical.

## Why This Is Unusual

Many languages expose one or two of these dimensions well.

Concrete is trying to make all four work together:

1. Rust-like seriousness about compiler-enforced restrictions
2. Zig-like explicitness and low-level control
3. SPARK/Ada-like attention to specs, proof obligations, and assurance workflow
4. Lean 4-backed proof and artifact story

The point is not to beat those languages at their own center of gravity. The point is to combine:

1. explicit systems programming
2. operational auditability
3. proof-backed evidence

in one coherent model.
