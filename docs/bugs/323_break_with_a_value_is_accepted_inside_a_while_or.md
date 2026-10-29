# Bug 323 — break with a value is accepted inside a while or for statement and its value is dropped

<!-- bug-ledger
summary: break with a value is accepted inside a while or for statement and its value is dropped, which STMT-6 refuses as the owner decided OD27 on 2026-10-29
coverage: reproducer tests/regressions/bug323_break_with_a_value_is_accepted_inside_a_while_or/repro.con, found checking main against OD27 on 2026-10-29; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 1) and check_spec.sh S4 (the pending STMT-6 example)
class: 13
silent: yes
ruling: Violates: STMT-6@2
-->

**Status:** OPEN
**Found:** 2026-10-29, by checking main against the owner's tightening OD27 ("strict across the board", 2026-10-29), one program per tightening, native against `--interp`, on main 0bf8350a.
**Severity:** silent under the decided rule: a program STMT-6 refuses is accepted, and both consumers agree on its result.

Codex confirmation 2026-10-29: CONFIRMED (all ten tightenings OD23 – OD32 were confirmed in one review of main's measured behaviour; for OD27: the code site below produces it).

## Reproducer

`tests/regressions/bug323_break_with_a_value_is_accepted_inside_a_while_or/repro.con`.

## Observed

On main 0bf8350a (built on OVH): `break 42;` inside a `while` statement is accepted, the value is dropped, and both consumers return 1.

## Expected

[STMT-6](../spec/statements.md#STMT-6) (OD27): `break <expr>` is refused at parse (E0001).

## Cause

`Concrete/Frontend/Parser.lean`, `parseStmt` (parses a value after `break`) and `Concrete/Check/Check.lean`, `checkStmt` (accepts and discards a Copy value).

## Structural cause

Class 13: a decided rule with no enforcing site. The rule changed on 2026-10-29 and main still
implements the earlier, looser one.

## Structural owner

Exit 2q (one migration of std, examples and tests to the decided rules) with R-0499 (the Core
checker, by stable rule ID).

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg, and STMT-6's
pending example records main's outcome. A program written to the decided rule is accepted today too.
