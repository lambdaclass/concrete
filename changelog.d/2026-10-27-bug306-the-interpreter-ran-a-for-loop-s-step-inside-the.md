### Bug 306: the interpreter ran a for loop's step inside the body's scope, before the body's deferred calls (2026-10-27)

`--interp` now runs a `for` loop's step after the body's deferred calls on a fall-through end of the
body, as it already did after `continue` and as the binary does on both: a deferred call in the
body reads the counter before the step. This implements the owner's decision OD2 part 5
(2026-10-29): every non-abort exit of a scope runs one exit sequence, and the step is not part of
the body's scope. Lower and the interpreter take the body's block from one function,
`CStmt.whileBodyBlock`. Regression leg: `regress_306_for_step_after_body_defers.con` in
`run_tests.sh` (binary and `--interp`). Structural owner: OD2 part 5, then R-0496
([bug 306](docs/bugs/306_the_interpreter_ran_a_for_loop_s_step_inside_the.md)).
