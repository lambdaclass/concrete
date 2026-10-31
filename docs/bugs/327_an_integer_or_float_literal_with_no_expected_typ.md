# Bug 327 — An integer or float literal with no expected type defaults to Int or Float64

<!-- bug-ledger
summary: An integer or float literal with no expected type defaults to Int or Float64, which ARITH-2 refuses as the owner decided OD31 on 2026-10-29
coverage: reproducer tests/regressions/bug327_an_integer_or_float_literal_with_no_expected_typ/repro.con, found checking main against OD31 on 2026-10-29; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 42) and check_spec.sh S4 (the pending ARITH-2 example)
class: 13
silent: yes
ruling: Violates: ARITH-2@3
-->

**Status:** OPEN
**Found:** 2026-10-29, by checking main against the owner's tightening OD31 ("strict across the board", 2026-10-29), one program per tightening, native against `--interp`, on main 0bf8350a.
**Severity:** silent under the decided rule: a program ARITH-2 refuses is accepted, and both consumers agree on its result.

Codex confirmation 2026-10-29: CONFIRMED (all ten tightenings OD23 – OD32 were confirmed in one review of main's measured behaviour; for OD31: the code site below produces it).

## Reproducer

`tests/regressions/bug327_an_integer_or_float_literal_with_no_expected_typ/repro.con`.

## Observed

On main 0bf8350a (built on OVH): `if 3 < 4` types both literals as `Int` and `if 1.5 < 2.5` both as `Float64`; both consumers return 42.

## Expected

Since ARITH-2@3 (2026-10-30) the rule covers integer literals only: the integer default to `Int`
violates it, and the float default to `Float64` is the open owner question OPEN-63.

[ARITH-2](../spec/arithmetic.md#ARITH-2) (OD31): an integer literal takes its type from its context, and one with nothing expected is refused; a float literal follows the same principle.

## Cause

`Concrete/Semantics/TypeJudgment.lean`, `intLitDecision` (the `Int` fallback) and `floatLitType` (the `Float64` fallback).

## Structural cause

Class 13: a decided rule with no enforcing site. The rule changed on 2026-10-29 and main still
implements the earlier, looser one.

## Structural owner

Exit 2q (one migration of std, examples and tests to the decided rules) with R-0499 (the Core
checker, by stable rule ID).

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg, and ARITH-2's
pending example records main's outcome. A program written to the decided rule is accepted today too.
