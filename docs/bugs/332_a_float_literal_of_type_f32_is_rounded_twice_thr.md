# Bug 332 — A float literal of type f32 is rounded twice, through binary64, instead of once

**Status:** OPEN
**Found:** 2026-10-30, by the Codex and Kimi reviews of the spec-gap rules (wip/spec-s6), native against `--interp` on main.
**Severity:** silent: an `f32` literal is rounded decimal to binary64 to binary32, so a literal just above a binary32 midpoint becomes the even neighbour: the program returns 0 on both consumers where 42 is expected.

Codex confirmation 2026-10-30: CONFIRMED.

<!-- bug-ledger
summary: A float literal of type f32 is rounded twice, through binary64, instead of once
coverage: reproducer tests/regressions/bug332_a_float_literal_of_type_f32_is_rounded_twice_thr/repro.con, found by the spec-gap reviews 2026-10-30; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg) and check_spec.sh S4 (ARITH-7's pending example)
class: 13
silent: yes
exit: 0b
ruling: Violates: ARITH-7@1
-->

## Reproducer

`tests/regressions/bug332_a_float_literal_of_type_f32_is_rounded_twice_thr/repro.con`.

## Observed

an `f32` literal is rounded decimal to binary64 to binary32, so a literal just above a binary32 midpoint becomes the even neighbour: the program returns 0 on both consumers where 42 is expected.

## Expected

[ARITH-7](../spec/arithmetic.md#ARITH-7): a float literal is correctly rounded at the type it takes, once.

## Cause

`Concrete/Frontend/Lexer.lean` parses every float literal to a binary64 `Float`; `Concrete/Backend/EmitSSA.lean` (`svalToOperand`) and `Concrete/Interp/Interp.lean` round that value to binary32 for an `f32`. Bug 109's record names this residual.

## Structural cause

Class 13: a documented rule with an enforcing site that misses one form.

## Structural owner

R-0493 (one arithmetic authority), which owns the value a literal denotes.

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg, and the rule's
pending example records main's outcome.
