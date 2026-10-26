### Compile-Time Scaling Gate; Ten Superlinear Stages Filed (R-0550, bugs 273 – 282, 2026-10-26)

`scripts/tests/check_compile_scaling.sh` (Phase 1 exit 2o) compiles generated programs of
10^4 – 10^5 elements or statements in each shape (array literals, array writes in a loop, long
functions, many locals, `let`s interleaved with `if`/`while`/`match`, many functions, structs and
enums, deep nesting) at two sizes a factor of ten apart, times every stage, and fails a stage over
its budget (measured on the CI host, recorded with its commit in
`scripts/tests/lib/compile_scaling_budgets.tsv`) or growing faster than n^1.6; noise is retried away,
a quadratic is not. Building it found ten more superlinear stages over many declarations or deep
nesting, filed open as bugs 273 – 282 (the parser's module item lists, Resolve's enum variant
fold and scope stack, Check's duplicate-declaration list and expression re-walks, Elab's
declaration analyses and evidence use lists, CoreCheck's and Lower's function lists, Mono's type
names; [bug 273](docs/bugs/273_parser_module_items_append.md) and the records after it). The gate
names each as an exclusion on every run until it is fixed.
