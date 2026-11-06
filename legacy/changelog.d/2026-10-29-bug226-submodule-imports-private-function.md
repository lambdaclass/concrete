### Bug 226: a submodule can import a private function of another module (2026-10-29)

A submodule's imports follow the same rule as a top-level module's: only `pub` declarations cross a module boundary (E0285 at check, MOD-2), and, as the owner decided OD20 on 2026-10-29, `std.libc`'s bindings are importable by std's own modules only (E0326, MOD-6). Pinned by run_tests.sh and the MOD-2 and MOD-6 spec examples; structural owner R-0492/R-0514
([bug 226](docs/bugs/226_submodule_imports_private_function.md)).
