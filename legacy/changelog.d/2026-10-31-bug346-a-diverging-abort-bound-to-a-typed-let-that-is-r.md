### Bug 346: a diverging abort() bound to a typed let that is returned, or returned directly, emits ret void in a function with a result and fails LLVM validation (2026-10-31)

Filed, not fixed: a diverging `abort()` bound to a typed `let` that is returned, or returned directly, passes Check and fails native LLVM validation (`ret void` in a function with a result) (PASS-12@1). The reproducer is pinned by `check_example_outputs.sh` (open-bug reproducers leg); the structural owner is R-0501
([bug 346](docs/bugs/346_a_diverging_abort_bound_to_a_typed_let_that_is_r.md)).
