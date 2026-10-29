# Bug 269 — the post-mono reference-binding verifier searched an environment list per assignment

<!-- bug-ledger
summary: the post-mono reference-binding verifier (REF-6@1) kept its environment in an association list searched per assignment (bindings × assignments per function)
fixtures: error_269_far_binding_assigned.con regress_269_generic_bindings_at_value.con
class: 17 — the post-mono verifier's environment
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/perf-backend`)
**Found:** 2026-10-25, profiling the `let`-then-`while` long-function shape: `refBindStmts` (in
`Check/Verify.lean`) was the largest self cost after Lower's.
**Severity:** loud (performance). Every non-generic function is verified after monomorphization; each
assignment searched a list of every binding declared before it, so a function of n bindings and m
assignments cost n·m.
Confirmed real by Codex review, 2026-10-25: `refBindStmt` prepends every binding and runs `env.lookup` per assignment, O(n·m) and quadratic for assignments to an increasingly distant binding.

## Reproducer

`let s<i>: i64 = s<i-1> + 1; if s<i> > t { t = t + 1; }` for i = 1..n (one assignment per `if`).
The `monomorphize` stage (which runs `verifyPostMono`) took 1.86 s at 20000 and 149.3 s at 80000 on
the front end of bugs 255-261; fixed, 0.24 s and 1.1 s.

## Measurements

`monomorphize` stage in ms, on the front end of bugs 255-261, before and after:

| n | before | after |
|---|---|---|
| 10000 | 180 | 45 |
| 20000 | 1864 | 233 |
| 40000 | 7476 | 543 |
| 80000 | 149306 | 1089 |

## Cause

`Check/Verify.lean`, `refBindExpr`/`refBindArm`/`refBindStmts`/`refBindStmt`: the environment was a
`List (LocalKey × Ty)`, prepended per binding and searched with `lookup` per assignment.

## Fix

A persistent map (`Std.TreeMap LocalKey Ty LocalKey.cmp`). A later binding of a key replaces the
earlier one, which is what the newest-first `lookup` found (parameters and arm binders are inserted
in order, so the last one wins as in the reversed list). The fixtures pin the refusal of an
assignment far from its binding and the acceptance at a value type.

## Structural cause

Class 17: pass state in association lists, the same shape as bug 261 (CoreCheck) one pass later.

## Structural owner

R-0422 (complexity envelopes). run_tests.sh's long-function leg bounds the `monomorphize` stage
of 40000 `let`s each followed by an `if` (3 s, retried with 6 s).
