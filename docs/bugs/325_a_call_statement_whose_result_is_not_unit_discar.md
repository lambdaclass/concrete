# Bug 325 — A call statement whose result is not Unit discards the result without discard()

<!-- bug-ledger
summary: A call statement whose result is not Unit discards the result without discard(), which OWN-7 refuses as the owner decided OD29 on 2026-10-29
coverage: reproducer tests/regressions/bug325_a_call_statement_whose_result_is_not_unit_discar/repro.con, found checking main against OD29 on 2026-10-29; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 42) and check_spec.sh S4 (the pending OWN-7 example)
class: 13
silent: yes
ruling: Violates: OWN-7@3
-->

**Status:** OPEN
**Found:** 2026-10-29, by checking main against the owner's tightening OD29 ("strict across the board", 2026-10-29), one program per tightening, native against `--interp`, on main 0bf8350a.
**Severity:** silent under the decided rule: a program OWN-7 refuses is accepted, and both consumers agree on its result.

Codex confirmation 2026-10-29: CONFIRMED (all ten tightenings OD23 – OD32 were confirmed in one review of main's measured behaviour; for OD29: the code site below produces it).

## Reproducer

`tests/regressions/bug325_a_call_statement_whose_result_is_not_unit_discar/repro.con`.

## Observed

On main 0bf8350a (built on OVH): `f();` where `f` returns `Int` is accepted and both consumers return 42.

## Expected

[OWN-7](../spec/ownership.md#OWN-7) (OD29): a statement expression has type `Unit`; any other unused result is written `discard(e)`, with no syntactic purity heuristic.

## Cause

`Concrete/Check/Check.lean`, `checkStmt` and `exprPureDiscardable`: only a syntactically pure non-Unit Copy expression is refused (E0294); a call is not judged pure, so its result is dropped silently.

## Structural cause

Class 13: a decided rule with no enforcing site. The rule changed on 2026-10-29 and main still
implements the earlier, looser one.

## Structural owner

Exit 2q (one migration of std, examples and tests to the decided rules) with R-0499 (the Core
checker, by stable rule ID).

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg, and OWN-7's
pending example records main's outcome. A program written to the decided rule is accepted today too.
