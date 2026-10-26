### The Back End No Longer Makes Long Functions Quadratic (bugs 265-267, 269, 2026-10-25)

A function of n bindings took time n² to lower, and n bindings each followed by a branch took time
n³: 80000 chained `let`s took 111 s in Lower, 4000 `let`s each followed by an `if` 135 s, and the
post-mono reference-binding verifier 149 s on 80000 `let`s with an `if` each. Lower's variable
table was an association list (bug 265); every branch merge visited every binding in scope and
walked the branch bodies once per binding (bug 266); SSACleanup settled one link of a chain of
constant branches per run of its whole pass sequence (bug 267); and the verifier searched its
environment list per assignment (bug 269). The table is persistent search trees that record when
each binding was written, so a merge visits only what an arm wrote; a forward pass settles constant
branch chains in one walk (in functions without an operation whose fold depends on whether its
operands are still registers); the verifier's environment is a map. The emitted SSA and LLVM IR are
unchanged (the whole corpus and 3000 generated programs compare byte-identical). 80000 statements
of each of these shapes now lower in 1-18 s. Pinned by `regress_265_variable_table.con`,
`regress_266_branch_merges.con`, `regress_267_constant_branch_chains.con`, the `*_269_*` fixtures
and a time-bounded leg over generated long functions. Loops still give every binding in scope a
header phi ([bug 270](docs/bugs/270_loop_header_phi_per_binding.md), open).
