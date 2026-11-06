# Effectful Proofs

**Status:** Open

This note defines how the proof story changes once functions are no longer purely computational.

## Why This Matters

Pure functions are the easy case.

Concrete becomes genuinely interesting only if it can say something useful about functions that involve:

1. capabilities
2. allocation
3. explicit failure
4. trusted boundaries
5. maybe later restricted classes of stateful systems code

## The Main Question

What kinds of effectful functions can still participate in a meaningful proof workflow?

Likely distinctions:

1. pure functions in the provable subset
2. functions with explicit but controlled capabilities
3. functions whose reports are proved even if their full behavior is not
4. functions excluded because they cross FFI or trusted boundaries too aggressively

## Why This Is Important

If Concrete wants to be a serious systems-proof language, it cannot stop at "prove arithmetic helpers." It needs a clear answer to how far proofs can go once the code is still recognizably systems code.
