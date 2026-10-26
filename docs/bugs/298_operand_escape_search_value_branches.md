# Bug 298 — Check searched a value if or match nested as an operand again at every level

<!-- bug-ledger
summary: bug 142's rule (no return/break/continue inside an operand, E0278) is enforced by a search of the operand's subtree at every protected expression; bug 280 handed the answer down through parentheses and operators but a value `if` or `match` dropped it, so an operand chain n deep through nested value ifs or matches was searched again at every level, time n² (8000 deep on main 1bcb7c36: check 2721 ms through ifs, 2806 ms through matches). Found probing the R-0550 shapes for Check quadratics. Class 17.
fixtures: regress_298_value_branch_operand_exits.con error_298_exit_nested_value_if.con error_298_exit_nested_value_match.con
class: 17 — the operand-escape search restarted at every value if or match
silent: no
-->

**Status:** FIXED (branch `wip/bug-298`)
**Found:** 2026-10-26, probing Check with shapes beyond the scaling gate's (R-0550 follow-up,
task 3 of the throughput queue): a value `if` nested as an operand n deep.
**Severity:** loud (performance). Compile time only; every diagnostic is unchanged.

## Reproducer

`let s: i64 = if t > 0 { 1 + if t > 0 { 1 + ... 0 } else { 0 } ... } else { 0 };` nested n deep
in `main` (and the same chain through `match t { 1 => 1 + ..., _ => 0, }`), `check` stage of
`--emit-trace-json`, dev host, main 1bcb7c36:

| depth | 2000 | 8000 | 10000 |
|---|---|---|---|
| `if` chain, check ms (heartbeats) | 165 (8.4 M) | 2721 (129.5 M) | 4507 |
| `match` chain, check ms | 165 | 2806 | |

Callgrind at 1000 → 2000: `operandEscape?` 197 M → 787 M instructions, `firstOperandEscape?`
130 M → 520 M.

## Cause

`Check/Check.lean`, `checkExpr`: a protected expression (an operator, a call, a literal, ...)
searches its operands for an exit (`firstOperandEscape?`, `CheckHelpers.lean`), and since bug 280
passes `clean` to operands its search covered, so they skip their own search. An unprotected
expression kept `clean` only for a parenthesis; a value `if` or `match` dropped it, and in
`checkExprCore` its condition or scrutinee and its branches' or arms' trailing expressions were
checked without it, so the `1 + ...` inside each level searched the rest of the chain again, and
`operandEscape?` allocated an operand list per node visited (Codex review 2026-10-26).

## Fix

A value `if` or `match` keeps `clean`, and passes it to its condition or scrutinee and to the
trailing expression of each branch or arm: an exit leaving any of them leaves the `if` or `match`
too, with no loop between, so the enclosing search already answered for them. The statements
before a trailing expression (where a loop may lie) still start their own searches, as before,
which `regress_298_value_branch_operand_exits.con` pins with a `break` bound by such a loop.
`--emit-core` and `--emit-llvm` (stdout, stderr with every diagnostic, exit status) of every `.con`
under `tests/programs` and `examples` are byte-identical with main's compiler.

## Measurements

`check` stage, ms, dev host, main 1bcb7c36 → fix:

| chain | 2000 | 8000 | 10000 |
|---|---|---|---|
| value `if` | 165 → 5 | 2721 → 21 | 4507 → 38 |
| value `match` | 165 → 6 | 2806 → 28 | |

Allocation heartbeats with the fix grow x4.0 for x4 (380 K → 1.52 M).

## Regression gate and leg

`run_tests.sh`: the run fixture (42) and the two error fixtures (E0278 at the innermost `return`
through value ifs and through value matches); each chain 8000 deep must check within 150 ms,
retried once at 300 ms.

## Structural cause

Class 17: a question about a subtree (does an exit leave this operand?) answered by a walk at every
level of the subtree; bug 280's answer-passing covered some expression forms and not these.

## Structural owner

R-0422; bug 142's rule (exit 2l, the one rule site in `checkExpr`) owns the search.

Codex review 2026-10-26: CONFIRMED — Value-if boundaries discard the inherited escape-search result, causing quadratic subtree traversal and temporary operand-list allocation in this shape.
