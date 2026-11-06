### Bug 347: get_args has a Check signature and no implementation, so a program whose emitted code calls it fails LLVM validation on an undefined @get_args (2026-10-31)

Filed, not fixed: `get_args` has a Check signature but no implementation, so a program whose emitted code calls it, even from a function nothing calls, fails native LLVM validation on an undefined `@get_args` (PASS-12@1). The reproducer is pinned by `check_example_outputs.sh` (open-bug reproducers leg); the structural owner is R-0492/R-0514
([bug 347](docs/bugs/347_get_args_has_a_check_signature_and_no_implementa.md)).
