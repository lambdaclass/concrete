### Long Functions No Longer Make the Front End Quadratic (bugs 255-261, 2026-10-25)

A function of n statements took time n² in every front-end stage: 40000 `s = s + 1;` statements
took 9 s to parse and 9-12 s each to resolve, check and elaborate, and 40000 chained `let`s
over four minutes in all. The parser appended each statement to the list built so far; the local
binder table, the resolver's scopes, the checker's bindings, Elab's binder frames and bound locals,
ElabVerify's and CoreCheck's scopes were lists searched, filtered or rebuilt per statement, and the
checker also scanned every binding at each block, branch, loop and arm. Each is now a map or an
array (the checker's an ordered map with indexed subsets and change ticks, iterating in the old
order, so diagnostics are unchanged). 80000 statements now pass the front end in about a second.
Pinned by `regress_255_block_statement_order.con` to `regress_261_corecheck_far_reads.con`, the
`error_257_*` and `error_258_*` fixtures and a time-bounded leg over generated long functions.
