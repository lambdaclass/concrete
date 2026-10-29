### Bug 330: a continue inside a for loop's step: the binary never terminates and the interpreter goes to the condition (2026-10-29)

Check now refuses a `break` or `continue` inside a `for` loop's step that targets that loop (E0245):
such an exit had no defined meaning, the compiled program never terminated, and `--interp` went to
the condition. The owner decided on 2026-10-29 to remove C-style `for` (loops become `while` and
`for i from a until b`) and to refuse this until then. A `return` in the step, a nested loop's own
exits and an outer loop's labelled `continue` stay accepted. Regression legs:
`error_330_continue_in_for_step.con`, `error_330_break_in_for_step.con` and
`regress_330_for_step_exits_allowed.con` in `run_tests.sh`. Structural owner: the removal of C-style
`for`, then R-0496 ([bug 330](docs/bugs/330_a_continue_inside_a_for_loop_s_step_the_binary_n.md)).
