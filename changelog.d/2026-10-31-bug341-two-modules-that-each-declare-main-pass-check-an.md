### Bug 341: two modules that each declare main pass Check and the native build fails in clang on a duplicate @__concrete_argc (2026-10-31)

Filed, not fixed: a program in which two modules each declare `main` passes Check and then fails to build natively inside clang on a second `@__concrete_argc` (PASS-12@1). The reproducer is pinned by `check_example_outputs.sh` (open-bug reproducers leg); the structural owner is R-0492
([bug 341](docs/bugs/341_two_modules_that_each_declare_main_pass_check_an.md)).
