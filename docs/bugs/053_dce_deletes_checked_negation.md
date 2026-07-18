# Bug 053: DCE deletes checked negations — documented trap silently vanishes

**Status:** Open
**Discovered:** 2026-07-18, middle-end audit (reproduced:
`discard(-x)` with `x: i8 = -128` — compiled prints "survived", exit 0;
interp aborts with `arithmetic overflow (checked negation)`; the emitted
`user_main` contains no `__cc_ssub_i8` call).

## Symptom

Per ARITHMETIC_POLICY, `-x` traps when `x` is the type's MIN (exactly like
checked `0 - x`). The compiled binary silently skips the trap whenever the
negation's result is unused.

## Root cause

`isSideEffecting` (`Concrete/IR/SSACleanup.lean:325-340`) keeps checked
BINOPS live (the fuzz-seed fix) but omits `.unaryOp .neg` — even though
`foldConstants`' own comment documents that neg must stay live to trap, and
EmitSSA lowers integer negation to the checked `ssub/usub` helper. This is
the same class as the fuzz fix, one constructor over: the side-effect
inventory is incomplete for a trapping operation whose result is discarded.

## Candidate fix

Add `.unaryOp .neg` to the side-effecting set (integer widths — float neg
does not trap), mirroring the checked-binop handling. Regression:
`discard(-x)` at `i8` MIN must abort on BOTH paths; a differential fuzzer
case over discarded checked unary ops belongs in the positions gate.
