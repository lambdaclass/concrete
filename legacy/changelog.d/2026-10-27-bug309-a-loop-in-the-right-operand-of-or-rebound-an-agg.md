### Bug 309: a loop in the right operand of && or || rebound an aggregate that the short-circuit merge did not reconcile (E0703) (2026-10-27)

A loop inside the right operand of `&&` / `||` (in a value `match` or `if` there) no longer gets
the program refused with E0703: the short-circuit merge now goes through the branch merge the
`if`, value `if` and `match` share, so an aggregate the operand rebinds is merged like any other.
Regression leg: `regress_309_shortcircuit_aggregate_merge.con` in `run_tests.sh`. Structural owner:
R-0501 ([bug 309](docs/bugs/309_a_loop_in_the_right_operand_of_or_rebound_an_agg.md)).
