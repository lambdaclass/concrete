### Bug 339: An ordering comparison of two bool values compiles natively and fails at run time under --interp (2026-10-30)

Filed, not fixed: `true < false` compiles and the binary exits with the comparison's value, while
`--interp` stops with "unsupported binop on given value types" (PASS-13@1; whether bool ordering
exists is OPEN-42). The native acceptance is pinned by check_example_outputs.sh's open-bug
reproducers leg; the structural owner is R-0496
([bug 339](docs/bugs/339_an_ordering_comparison_of_two_bool_values_compil.md)).
