### Bug 350: the proven-overflow arm of the compile-time safety diagnostics can never fire, so the documented refusal of a proven overflow never happens (2026-10-31)

Filed, not fixed: the proven-overflow arm of the compile-time safety diagnostics can never fire, because the constant folder needs literal-only operands and the type lookup needs a typed one, so the documented refusal of a proven overflow never happens (the run-time trap is unaffected). The reproducer is pinned by `check_example_outputs.sh` (open-bug reproducers leg); the structural owner is R-0499
([bug 350](docs/bugs/350_the_proven_overflow_arm_of_the_compile_time_safe.md)).
