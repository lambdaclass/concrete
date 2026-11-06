### Bug 344: The interpreter refuses the abort() intrinsic as an undefined function while the binary aborts (2026-10-31)

Filed, not fixed: `abort()` aborts the binary, but `--interp` refuses it as an undefined function (its only abort case is in the table of C functions, which the intrinsic never reaches). Loud. Pinned by `check_example_outputs.sh` (open-bug reproducers leg); the structural owner is class 7's, an interpreter intrinsic dispatch checked total against `resolveIntrinsic`
([bug 344](docs/bugs/344_the_interpreter_refuses_the_abort_intrinsic_as_a.md)).
