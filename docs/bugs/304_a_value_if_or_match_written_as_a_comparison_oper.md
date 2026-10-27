# Bug 304 — a value if or match written as a comparison operand took the comparison's Bool hint as its type, so CoreCheck refused a program Check accepted

<!-- bug-ledger
summary: a comparison passed its own type hint (its result type, Bool, from a `return` of a `bool` function, a `bool` binding or an `if` condition) on to the operand it types first; Elab's value `if` and `match` take a hint as their type unconditionally, so `(if c { 1 } else { 2 }) >= 2` was stamped Bool in Core, and CoreCheck refused the program Check had accepted (E0510, E0503)
fixtures: regress_304_comparison_operand_hint.con
class: 12 — the operand hint of a binary operator decided separately in Check and Elab, and the comparison's result type taken for its operands' type
silent: no
-->

**Status:** FIXED (branch `wip/bug-304`)
**Found:** 2026-10-26, by the differential fuzzer's new borrow mode (`fuzz_differential.py --mode
borrow`: 8 of 60 programs refused), hunting wrong code in Lower's handling of borrows.
**Severity:** loud. A well-typed program is refused (by `--interp` and the compiler alike).

## Reproducer

```
fn k(c: bool) -> bool {
    return (if c { 1 } else { 2 }) >= 2;
}
fn main() with(Console) { if k(true) { println(1); } }
```

## Observed

On main ea21a2e8 Check accepts it and CoreCheck refuses it: `error[core-check]: (E0510) [main]
type mismatch at the value of an if branch: declared Bool, value has type Int`, the same for the
else branch, then `(E0503) comparison operand type mismatch`. A value `match` operand
(`(match c { true => 1, false => 2 }) > 1`), an `if` condition (`if (if c { 3 } else { 4 }) == 4`)
and a `bool` binding fail the same way; binding the value `if` to an `i64` first is accepted.
`regress_304_comparison_operand_hint.con` covers the four; main refuses it.

## Expected

The program is well typed (Check accepts it, and the comparison's operands are both integers);
it compiles and runs as `--interp` would.

## Cause

`Elab/Elab.lean`, `elabExprEv`'s `.binOp` case, passed the expression's hint to the operand it
types first whatever the operator; for a comparison that hint is the comparison's own result type,
`Bool`. A flexible literal ignores a `Bool` hint, but the value `if` and `match` take any hint as
their Core result type (the arms are typed under it), so the operand was stamped `Bool` while its
arms are integers. Check's `checkExpr` passed the hint the same way but derives a value `if`'s type
from its arms, so it accepted (Codex confirmation, 2026-10-26).

## Fix

`TypeJudgment.binOpOperandHint`, shared by Check and Elab (as `binOpOperandOrder` already is),
gives the operand hint of every binary operator: an arithmetic, bitwise or logical operator's
operands have its result type, so its hint passes on; a comparison passes none. The match lists
every operator, so a new one has to be classified.

## Structural cause

Class 12: one fact kept in two places. Check and Elab each decide the hint a binary operator gives
its operands, and each types a value `if` with its own rule (from the arms in Check, from the hint
in Elab), so a hint that should not have reached the operand was harmless in one and fatal in the
other.

## Structural owner

R-0499 (S8: the Core checker as the one typing judgment, with Elab producing what it checks). Until
then the operand hint is one shared function (`binOpOperandHint`), next to the shared operand order.

Codex confirmation 2026-10-26: CONFIRMED — Comparison elaboration leaks an outer Bool hint into a value-if operand whose Core type then contradicts its numeric branches, causing CoreCheck to reject a well-typed program.
