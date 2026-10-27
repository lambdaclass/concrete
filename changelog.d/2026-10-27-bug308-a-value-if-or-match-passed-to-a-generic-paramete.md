### Bug 308: a value if or match passed to a generic parameter left its type variable uninferred, so Check refused the call (2026-10-27)

`gid(if c { 1 } else { 2 })` and the same with a value `match` now infer the type parameter from an
arm's trailing value instead of being refused (E0220): the argument-type peek Check and Elab share
answers both forms. Regression leg: `regress_308_generic_arg_value_if.con` in `run_tests.sh`.
Structural owner: R-0499
([bug 308](docs/bugs/308_a_value_if_or_match_passed_to_a_generic_paramete.md)).
