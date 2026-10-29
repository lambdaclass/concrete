# Bug 320 — main may return Int or bool: the native exit status is the result masked to 0-255 or 1 for true, and --interp prints it

<!-- bug-ledger
summary: main may return Int or bool: the native exit status is the result masked to 0-255 or 1 for true, and --interp prints it, which STMT-7 refuses as the owner decided OD24 on 2026-10-29
coverage: reproducer tests/regressions/bug320_main_may_return_int_or_bool_the_native_exit_stat/repro.con, found checking main against OD24 on 2026-10-29; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 42) and check_spec.sh S4 (the pending STMT-7 example)
class: 13
silent: yes
-->

**Status:** OPEN
**Found:** 2026-10-29, by checking main against the owner's tightening OD24 ("strict across the board", 2026-10-29), one program per tightening, native against `--interp`, on main 0bf8350a.
**Severity:** silent under the decided rule: a program STMT-7 refuses is accepted, and both consumers agree on its result.

Codex confirmation 2026-10-29: CONFIRMED (all ten tightenings OD23 – OD32 were confirmed in one review of main's measured behaviour; for OD24: the code site below produces it).

## Reproducer

`tests/regressions/bug320_main_may_return_int_or_bool_the_native_exit_stat/repro.con`.

## Observed

On main 0bf8350a (built on OVH): `fn main() -> Int` returning 298 exits 42 natively (masked to 0-255) and `--interp` prints 298; `fn main() -> bool` returning `true` exits 1 and `--interp` prints nothing.

## Expected

[STMT-7](../spec/statements.md#STMT-7) (OD24): `main` returns `u8` or `Unit` (exit 0); an `Int`, `bool` or other result type is refused, with no masking and no bool mapping.

## Cause

`Concrete/Backend/EmitSSA.lean`, `emitMainWrapper` (masks an integer result, maps `bool`) and `Concrete/Interp/Interp.lean`, `interpret` (prints the result); `Check.checkModule` admits any result type.

## Structural cause

Class 13: a decided rule with no enforcing site. The rule changed on 2026-10-29 and main still
implements the earlier, looser one.

## Structural owner

Exit 2q (one migration of std, examples and tests to the decided rules) with R-0499 (the Core
checker, by stable rule ID).

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg, and STMT-7's
pending example records main's outcome. A program written to the decided rule is accepted today too.
