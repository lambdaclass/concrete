# Bug 228 — the interpreter's loops are quadratic in their iteration count

**Status:** FIXED (branch `wip/workload-stackvm-2`)
**Found:** 2026-10-24, by Phase 7 workload 10 (`examples/stackvm`): `stackvm exec testdata/fib.sasm`
did not finish in 10 minutes under `--interp`; natively it takes a millisecond.
**Severity:** loud (performance). `--interp`, the oracle of every `@run` case and of the
differential checks, could not run a long loop: the time per iteration grew with the number of
iterations already run.

## Reproducer

`tests/programs/regress_228_interp_loop_env.con`: a `while` of 200000 iterations whose body binds
three `let`s. Observed on main `408880cb`: the binary returns 42 at once; `--interp` did not finish
in 60 s. The stackvm interpreter loop (`vm.loop_run`, four `let`s per step) on a countdown program:

| iterations | main | fixed |
|---|---|---|
| 250 | 2.3 s | 0.97 s |
| 500 | 7.3 s | 1.1 s |
| 1000 | 42.5 s | 1.6 s |
| 2000 | 222 s | 2.5 s |

## Cause

The interpreter's environment is one association list (`Interp.Env`), searched from the front.
`evalWhile` passed the environment the body ended with to the next iteration, so every `let` of
every iteration stayed in it, shadowed but present. Each lookup or assignment of a variable bound
outside the loop walked past all of them. `evalWhileExpr` (the value-bearing loop) trimmed the
environment back to its entry length, but only when the loop ended.

## Fix

`evalWhile` takes the environment's length at loop entry and trims back to it after every
iteration (and on every exit), keeping the entries below it, which hold the outer variables and
their updated values. A reference cannot outlive the block it borrows from, so nothing can name a
dropped binding. Pinned by `regress_228_interp_loop_env.con` (binary 42; `run_tests.sh` also runs
it under `--interp` with a 20 s budget).

## Structural cause

Class 17 ("pass state in association lists"): scope was not part of the environment's structure.
Leaving a scope had to be done by hand at each construct that has one, and the loop construct did
it only at its end, so the cost of the list grew with execution rather than with nesting.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes), extended to the interpreter's environment: a scoped environment (frames that a
block or iteration pops) instead of a flat list trimmed at selected sites. The stackvm interpreter
still costs about 0.5 ms per VM step under `--interp`, linear but slow; that is not this bug.
