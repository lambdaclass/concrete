### Bug 345: a newtype unwrap bound to a let of the inner type passes Check and the native build stops at the SSA verifier (2026-10-31)

Filed, not fixed: `let x: Int = unwrap(m);` with `m` a newtype over `Int` passes Check, and the native build stops at the SSA verifier (E0715) because the Core expression keeps the newtype's type, while `--interp` runs it (PASS-12@1). The reproducer is pinned by `check_example_outputs.sh` (open-bug reproducers leg); the structural owner is R-0499
([bug 345](docs/bugs/345_a_newtype_unwrap_bound_to_a_let_of_the_inner_typ.md)).
