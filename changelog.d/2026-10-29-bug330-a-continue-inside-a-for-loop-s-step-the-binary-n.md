### Bug 330: a continue inside a for loop's step: the binary never terminates and the interpreter goes to the condition (2026-10-29)

Filed, open: a `continue` inside a `for` loop's step makes the compiled program loop forever while
`--interp` goes to the condition; what such a `continue` does is not decided. The reproducer is
pinned as accepted by `check_example_outputs.sh` (open-bug reproducers leg). Bug 306's interpreter
comment no longer claims Check refuses every such step. Structural owner: the owner's ruling, then
R-0496
([bug 330](docs/bugs/330_a_continue_inside_a_for_loop_s_step_the_binary_n.md)).
