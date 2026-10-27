### Bug 303: the value if did not drop the bindings its arms declare, so a later loop's phi took a value defined on one arm's path (2026-10-26)

A value `if` whose arm declares a local (a loop counter, a `let`) followed by a loop no longer
fails SSA verification (E0708): every branch merge in Lower now drops the bindings its arms
declare, in the one merge function the statement `if`, the value `if` and `match` share.
Regression leg: `regress_303_value_if_arm_scope.con` in `run_tests.sh`. Structural owner: R-0501
([bug 303](docs/bugs/303_the_value_if_did_not_drop_the_bindings_its_arms.md)).
