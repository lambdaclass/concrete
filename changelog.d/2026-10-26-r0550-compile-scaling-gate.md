### Compile-Time Scaling Gate; Ten Superlinear Stages Filed (R-0550, bugs 273 – 282, 2026-10-26)

`scripts/tests/check_compile_scaling.sh` (Phase 1 exit 2o, in the full gate set) compiles generated
programs in each shape (array literals, array writes in a loop, long functions, many locals, `let`s
interleaved with `if`/`while`/`match`, many functions, structs and enums, deep nesting) at two sizes
a factor of ten apart (10^4 and 10^5 elements for the array, long-function and many-locals shapes),
times each stage, and fails a stage over its budget (measured on the CI host, recorded with its
commit in `scripts/tests/lib/compile_scaling_budgets.tsv`) or growing faster than n^1.8 in time
(where large enough to resolve) or n^1.2 in allocations; host noise is retried away. Building it
found ten more superlinear stages over many declarations or deep nesting, filed open as bugs
273 – 282 (the parser's module item lists, Resolve's enum variant fold and scope stack, Check's duplicate-declaration list and
expression re-walks, Elab's declaration analyses and evidence use lists, CoreCheck's and Lower's
function lists, Mono's type names; [bug 273](docs/bugs/273_parser_module_items_append.md) and the
records after it). Those stages, and Lower's loop header phis (bug 270, the `let`-then-`while` shape),
are named exclusions the gate reports on every run; the declaration and nesting shapes run at 1000
and 10000 until their fixes land.
