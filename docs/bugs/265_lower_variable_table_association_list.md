# Bug 265 — Lower kept its variable table in an association list: a lookup or write cost the bindings in scope

<!-- bug-ledger
summary: Lower's variable table was an association list searched per read and searched and rebuilt or appended per write (quadratic in a function's bindings)
fixtures: regress_265_variable_table.con
class: 17 — Lower's per-function variable table
silent: no
-->

**Status:** FIXED (branch `wip/perf-backend`)
**Found:** 2026-10-25, measuring the back end over the long-function shapes of bugs 255-261 (the
front-end agent's report: chained `let`s took 22 s to lower at 40000 and 103 s at 80000).
**Severity:** loud (performance). A function of n bindings cost the square of n to lower: every read
of a local and every write searched a list as long as the bindings in scope, and a write to a new
name appended to it.

## Reproducer

`fn main` with n chained `let s<i>: i64 = s<i-1> + 1;` (the shape of bugs 255-261's legs). The
`lower` stage (Lower, SSA verification, cleanup; `--emit-trace-json`) took 20.9 s at 40000 and
110.9 s at 80000; fixed, 0.38 s and 0.97 s. `regress_265_variable_table.con` pins what the table
must keep; run_tests.sh's long-function leg bounds the time.

## Measurements

`lower` stage in ms, dev host (other agents loading it, one run each), on the front end of bugs
255-261 (`wip/perf-frontend-v2`, so the front end does not dominate), before and after this branch:

| n chained `let`s | before | after |
|---|---|---|
| 1000 | 5 | 3 |
| 4000 | 78 | 17 |
| 10000 | 625 | 78 |
| 20000 | 4082 | 121 |
| 40000 | 20933 | 377 |
| 80000 | 110878 | 971 |

Instruction counts (callgrind) of the after build double with n: Lower 315 M at 20000 and 667 M
at 40000; cleanup 918 M and 1836 M.

## Cause

`IR/Lower.lean`, `LowerState.vars : List (LocalKey × SVal)`: `setVar` asked `any` and then rebuilt the
list with `map`, or appended with `++ [x]`; `lookupVar` and `addrOfLocal` searched it with `lookup`;
`snapshotVars` filtered the whole list at every branch. `promotedAllocas` was a list searched by
every read and write.

## Fix

The table is a `VarMap`: persistent search trees keyed by binding identity (`LocalKey.cmp`, now in
`Elab/Core.lean`), with the old listing order kept as a position per binding (a write to a bound
name keeps its position, a new name goes last), so every traversal visits bindings in the order the
list held them. Lookups, writes and snapshots are logarithmic. The promoted-slot table is a map
from the name to its slot (no name is ever registered twice). `--emit-ssa` and `--emit-llvm` of the
whole `tests/programs` and `examples` corpus (1934 files) and of 3000 generated differential-fuzz
programs are byte-identical before and after.

## Structural cause

Class 17: pass state in association lists. Lower's table was written for small functions and never
sized for the function; three structures of the same pass (this table, the merge walks of bug 266
and the loop bookkeeping) shared the shape. Bugs 255-261 are the same measurement on the front end.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes): per-function pass state sized for the
function, with the long-function shapes as its envelope (exit 2o's scaling gate).
