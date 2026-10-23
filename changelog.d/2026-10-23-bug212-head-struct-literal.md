### A Constant Before An `if` / `while` / `match` Block Gets An Explaining Diagnostic (bug 212, 2026-10-23)

In a control-flow head an uppercase name followed by `{` is read as the start of a struct literal,
so `if n > LIMIT { … }` with a module constant was refused with `expected }, got =`, pointing inside
the block. The E0001 now says that `LIMIT {` was read as a struct literal and names the
parenthesized form (`if (n > LIMIT) { … }`). The case rule, which was in neither the grammar nor
the specification, is written into `grammar/concrete.ebnf` and SYN-1; the accepted language is
unchanged, and whether a head should admit a struct literal is left open. Found by Phase 7
workload 9 (`examples/lzpack`). Pinned by `error_212_const_in_{if,while,match}_head.con` and
`regress_212_parenthesized_const_head.con`.
