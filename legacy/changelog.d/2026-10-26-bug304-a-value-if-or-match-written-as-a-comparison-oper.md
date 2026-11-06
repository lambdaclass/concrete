### Bug 304: a value if or match written as a comparison operand took the comparison's Bool hint as its type, so CoreCheck refused a program Check accepted (2026-10-26)

`(if c { 1 } else { 2 }) >= 2` and the same with a value `match` now compile in a `bool` context
(a `return`, a binding, an `if` condition): a comparison no longer passes its result type to its
operands as a hint, in Check and Elab alike (`TypeJudgment.binOpOperandHint`). Regression leg:
`regress_304_comparison_operand_hint.con` in `run_tests.sh`. Structural owner: R-0499
([bug 304](docs/bugs/304_a_value_if_or_match_written_as_a_comparison_oper.md)).
