### Finite Size Through Generic Instantiations: Bug 205 (2026-10-23)

A struct that holds itself by value through a generic instantiation (`struct Node { w: W<Node> }`)
is now refused with E0583, as a direct cycle already was. Before, the check stopped at
instantiations: the native build failed in `llvm-as` while `--interp` ran the program. Pinned by
`error_205_recursive_through_instantiation.con` and `regress_205_instantiations_without_cycle.con`.
