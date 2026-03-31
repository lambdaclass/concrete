# Failure Semantics

**Status:** Open

This note gathers the failure model that Concrete should make explicit across language, runtime, and restricted profiles.

## Why This Matters

Concrete avoids panic/unwind machinery and prefers explicit error returns. That is good for auditability, but the overall failure story still needs to be unified.

Important questions include:

1. what abort means
2. what happens on out-of-memory
3. what happens on assertion failure
4. what happens on impossible branches or internal compiler assumptions
5. how restricted profiles treat these cases

## Current Shape

Today the execution model already implies:

1. explicit recoverable errors use `Result`
2. out-of-memory aborts the process
3. there is no stack unwinding
4. traps such as segfault or stack overflow are handled by the host environment, not the language

This is simple, but it should be treated as an intentional design, not a collection of incidental facts.

## Design Questions

### 1. Abort policy

Concrete should stay explicit about where `abort()` is part of the model and where it is not.

Open questions:

1. should all fatal runtime failures continue to be immediate process termination?
2. should restricted profiles allow abort-based failure?
3. which failures are considered programming errors versus environmental errors?

### 2. Assertions

Assertions need a clear position:

1. are they debug-only checks?
2. are they profile-forbidden in high-integrity code?
3. do they always abort on failure?

### 3. Impossible paths

Concrete should distinguish:

1. unreachable states caused by compiler/runtime invariants
2. user-visible logic errors
3. proof/profile violations

These may all terminate, but they are not the same kind of event.

### 4. OOM behavior

Concrete currently uses abort-on-OOM. That is simple and honest.

The design question is not whether abort-on-OOM is acceptable in general hosted code. It is whether:

1. restricted profiles forbid allocation entirely
2. bounded-allocation profiles allow OOM at all
3. no-alloc/fixed-capacity code can exclude OOM by construction

## Relationship To Profiles

Failure semantics matter more, not less, in restricted profiles.

Examples:

1. predictable-execution profile likely forbids hidden or environment-dependent failure paths
2. high-integrity profile may want a smaller accepted set of abort causes
3. freestanding targets may require a more explicit target-specific failure contract

## Relationship To Other Notes

1. [../../docs/EXECUTION_MODEL.md](../../docs/EXECUTION_MODEL.md)
2. [../../docs/LANGUAGE_INVARIANTS.md](../../docs/LANGUAGE_INVARIANTS.md)
3. [high-integrity-profile.md](high-integrity-profile.md)
4. [../predictable-execution/predictable-execution.md](../predictable-execution/predictable-execution.md)

## Bottom Line

Concrete should keep failure semantics simple, but it should also make them explicit enough that:

1. ordinary hosted code is unsurprising
2. restricted profiles know what is allowed
3. evidence/reporting systems can explain where failure boundaries exist
