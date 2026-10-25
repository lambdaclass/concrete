# Bug 267 — SSACleanup settled one constant branch of a chain per run of its whole pass sequence

<!-- bug-ledger
summary: SSACleanup settled one link of a chain of constant branches per run of its whole-function pass sequence (n chained constant branches took n runs)
fixtures: regress_267_constant_branch_chains.con
class: 17 — SSACleanup's whole-function fixpoint
silent: no
-->

**Status:** FIXED (branch `wip/perf-backend`)
**Found:** 2026-10-25, profiling the `let`-then-`if` long-function shape (bug 266's reproducer):
`cleanupFixpoint` was 74% of the time; the fixpoint ran its pass sequence 402 times for 400 `if`s.
**Severity:** loud (performance). A function with a chain of n branches whose conditions become
constant one after another (each reading the phi the previous branch merged) took n runs of the
whole cleanup sequence over the whole function: time quadratic in its size, on top of bug 266.

## Reproducer

`let s<i>: i64 = s<i-1> + 1; if s<i> > t { t = t + 1; }` for i = 1..n, `s0` and `t` constant: every
condition folds, but only after the previous `if`'s merge phi does. n = 400: 402 runs of
`cleanupBlocks` (callgrind), 5.0 G instructions of cleanup; fixed, 2 runs and 68 M. n = 2000: the
`lower` stage took 36.9 s on main (Lower's own share, bug 266, aside); fixed, 40 ms.

## Cause

`IR/SSACleanup.lean`, `cleanupBlocks`: `eliminateConstantBranches` turns the branch into a `br`, but
the arm it no longer takes leaves only in the final `eliminateDeadBlocks`, after the second
`eliminateTrivialPhis`; so the merge phi the dead arm fed becomes trivial only in the next run, and
only then can `foldConstants` fold the next condition, which reads it. `cleanupFixpoint` reruns the
whole sequence until the block and instruction counts stop changing: one link per run.

## Fix

`resolveConstantBranches`, run first in each sequence, settles a whole chain in one forward walk:
it propagates the constants `foldConstants` proves (the same folds of the same operands), tracks
which edges can be taken, and reads a phi as the value `findTrivialPhis` would reduce it to once the
entries of untaken edges are gone; a branch whose condition it proves constant becomes a `br`. It
never concludes more than the sequence eventually does (an edge from a block not yet visited, a
loop back edge, counts as taken with an unknown value; other instructions prove nothing; a block
with no takable edge is skipped), so each rewrite is one the sequence makes in a later run, and the
sequence, still run to its fixpoint, reaches the same result in a few runs. The pass rewrites
branches in 85 of the 1934 corpus programs and in 2123 of 3000 generated fuzz programs;
`--emit-ssa` and `--emit-llvm` are byte-identical before and after on all of them.

## Structural cause

Class 17: a whole-function fixpoint whose iteration discovers one link of a dependency chain.
Bug 178 fixed the straight-line form (forward constant propagation in `foldConstants`); the branch
form remained because edge liveness was decided by separate passes later in the sequence.

## Structural owner

R-0422 (complexity envelopes): the cleanup's passes as one sparse, worklist-driven propagation
(the verified pipeline's cleanup stage, R-0491 – R-0509, would own its correctness argument).
