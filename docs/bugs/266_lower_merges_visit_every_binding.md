# Bug 266 — every branch merge in Lower visited every binding in scope

<!-- bug-ledger
summary: every branch merge in Lower visited every binding in scope and walked the branch bodies once per binding (lets interleaved with ifs or matches were cubic to lower)
fixtures: regress_266_branch_merges.con
class: 17 — Lower's branch merges
silent: no
-->

**Status:** FIXED (branch `wip/perf-backend`)
**Found:** 2026-10-25, measuring the back end over the long-function shapes of bugs 255-261 (the
front-end agent's report: 4000 `let`s each followed by an `if` took 137 s to lower, with
`Lower.reconcileBranchVars` and `SSACleanup.cleanupFixpoint` on top of the profile).
**Severity:** loud (performance). With n bindings in scope, each `if`, value `if`, `match` and
`&&`/`||` cost n searches of each arm's table and n walks of the branch bodies: a function of n
`let`s each followed by a branch took time cubic in n to lower.

## Reproducer

`let s<i>: i64 = s<i-1> + 1;` each followed by `if s<i> > t { t = t + 1; }` or by
`match s<i> { 0 => { t = t + 1; }, _ => { t = t + 2; }, }`. The `match` shape's `lower` stage took
1.3 s at 1000 and 91.8 s at 4000; fixed, 44 ms and 571 ms (the `if` shape's time is mostly bug 267).
`regress_266_branch_merges.con` pins the merges; run_tests.sh's long-function leg bounds the time.

## Measurements

`lower` stage in ms, on the front end of bugs 255-261, before and after this branch (with bug 267's
fix; one run each on a loaded host, so the after column is noisy above 10000; callgrind instruction
counts of the after build grow 2.0-2.1× per doubling: cleanup 1.44 G at 8000 `match`es, 2.89 G at
16000; Lower 0.89 G and 1.88 G):

| n | `if` before | after | `match` before | after |
|---|---|---|---|---|
| 1000 | 3273 | 26 | 1331 | 44 |
| 4000 | 134815 | 100 | 91767 | 571 |
| 10000 | over 300 s | 446 | over 300 s | 1336 |
| 20000 | — | 1707 | — | 3792 |
| 40000 | — | 3253 | — | 7504 |
| 80000 | — | 10947 | — | 17461 |

## Cause

`IR/Lower.lean`: `reconcileBranchVars` iterated every pre-branch binding and searched every arm's
end table for it; the scope cleanup after an `if` or a `match` rebuilt the table from every
pre-branch binding with a search of the current one; the `&&`/`||` merge did the same; and
`prePromoteAddrTaken` asked, per binding in scope, whether the branch body takes its address,
walking the body each time (the while loop did that walk, a `List.contains` and a list append per
binding too).

## Fix

Every write to the variable table takes a fresh tick from one counter of the function, and the
table indexes its bindings by latest-write tick and keeps the set whose value is not a register
(bug 265's `VarMap`). A table derived from a branch's snapshot by writes holds exactly the
snapshot's bindings at their old ticks plus the bindings written since, above the snapshot's
watermark. A merge visits only the bindings an arm wrote since the branch began, and those whose
value is not a register (the merge rules count such a binding as changed even when no arm wrote
it; every other binding holds its snapshot value, a register, in every arm, and visiting it did
nothing). The scope cleanup writes back only the bindings written since the snapshot. The
address-taken names are collected in one walk (`cstmtsAddrTaken`, each clause the old predicate's
disjunction as a union). The bindings visited are visited in the old listing order, so the emitted
SSA is unchanged: `--emit-ssa` and `--emit-llvm` of the corpus (1934 files) and of 3000 generated
fuzz programs are byte-identical.

## Structural cause

Class 17: per-query recomputation. Each merge recomputed "which bindings changed" by comparing
every binding, because the table recorded values but not when they were written.

## Structural owner

R-0422 (complexity envelopes). Lower's loops still give every binding in scope a header phi
(bug 270), the remaining back-end quadratic of these shapes.
