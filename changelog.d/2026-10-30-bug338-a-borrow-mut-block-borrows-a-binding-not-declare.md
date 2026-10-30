### Bug 338: A borrow mut block borrows a binding not declared mut, so the block writes to an immutable binding (2026-10-30)

Filed, not fixed: `borrow mut x as r in R { ... }` is accepted when `x` is not declared `mut`, and
the writes through `r` reach `x` (silent; OWN-10@2). The reproducer is pinned by
check_example_outputs.sh's open-bug reproducers leg; the structural owner is R-0499
([bug 338](docs/bugs/338_a_borrow_mut_block_borrows_a_binding_not_declare.md)).
