# Bug 258 — the checker keeps a function's bindings in a list walked per use, per binding and per block

<!-- bug-ledger
summary: the checker kept a function's bindings in a list filtered per binding, walked per lookup, rebuilt per state change and scanned whole at every block, branch, arm, loop, `defer` scope and exit (40000 `let`s: 52.8 s; 20000 `let`s each with an `if`, `while` or `match`: over 120 s for the front end; now 166 ms and 2.6 s). `VarEnv` iterates in the old newest-first order, with logarithmic lookups and updates, subsets of the non-Copy, reference and defer-marked bindings, and change ticks for what a block changed. Pinned by `regress_258_checker_binding_states.con`, `error_258_*` (first refusal unchanged) and the long-function leg. Found with bug 255. Class 17.
fixtures: regress_258_checker_binding_states.con error_258_branch_disagreement_order.con error_258_block_leak_order.con
class: 17 — one function of many statements: the parser's statement lists, the local binder table, the resolver's scopes, the checker's bindings, Elab's binder frames, ElabVerify's and CoreCheck's scopes, each a list appended or searched per statement
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/perf-frontend`)
**Found:** 2026-10-25, measuring the long-function quadratic the previous perf agent reported (40000 `s = s + 1;` statements: 8.5 s parse, 13.4 s resolve, 9.5 s check, 9.5 s elaborate), per stage, for flat statements, long `if`/`while` bodies, chained `let`s and `let`s interleaved with blocks.
**Severity:** loud (performance). Checking a function of n bindings cost n² (40000 `let`s: 52.8 s),
and one of n bindings and m blocks n·m with a factor of n inside (n = 4000 `let`s each followed by an
`if`: 30 s).

## Reproducer

`fn main` with 40000 chained `let`s: check 52.8 s; fixed 166 ms (80000: 409 ms). 10000 `let`s
each followed by `if s<i> > t { t = t + 1; }`: check 6.6 s after the point lookups were fixed, 58 ms
after the subsets; n = 20000 with an `if`, a `while` or a `match` per `let`: over 120 s before,
2.6 s after for the whole front end. `regress_258_checker_binding_states.con` pins the ownership
states the new paths compute (both-branch consumption, a loop that leaves an outer value alone,
consumption in every arm, `defer` in a nested block, a borrow block over a reserved value, block
locals); `error_258_branch_disagreement_order.con` and `error_258_block_leak_order.con` pin that
the first refusal still names the newest binding, as the list order did.

## Measurements

Per-stage times in ms (`--emit-trace-json`, dev host, one run each, other agents loading the
machine), before (train 23, 67e020e2) and after (the fixes of bugs 255-261 together). `resolve`
includes the local walk; `elaborate` includes canonicalization and ElabVerify.

`s = s + 1;` repeated n times in one function:

| n | parse before | after | resolve before | after | check before | after | elaborate before | after |
|---|---|---|---|---|---|---|---|---|
| 10000 | 292 | 26 | 746 | 4 | 611 | 24 | 603 | 27 |
| 20000 | 1614 | 52 | 2929 | 9 | 2259 | 53 | 2276 | 61 |
| 40000 | 9402 | 113 | 12369 | 28 | 9124 | 132 | 8793 | 149 |
| 80000 | — | 220 | — | 186 | — | 280 | — | 384 |

`let s<i>: i32 = s<i-1> + 1;` for i = 1..n:

| n | parse before | after | resolve before | after | check before | after | elaborate before | after |
|---|---|---|---|---|---|---|---|---|
| 10000 | 288 | 20 | 1021 | 9 | 1194 | 30 | 1612 | 22 |
| 20000 | 1614 | 47 | 4177 | 24 | 5377 | 78 | 6988 | 90 |
| 40000 | 12381 | 93 | 221279 | 63 | 52826 | 166 | 30951 | 188 |
| 80000 | — | 209 | — | 153 | — | 409 | — | 454 |

The same statements inside one `if` or `while` body scale as the flat case (40000: parse 8827,
resolve 11921, check 9585, elaborate 9707 before; 62, 21, 71, 85 after). Each `let` followed by an
`if`, a `while` or a `match` reading a binding declared at the top (`t`), n = 20000: over 120 s for
the front end (`--emit-core`) before, 2.6 s after; n = 4000: check 30201 and elaborate 48599 before,
41 and 34 after.

## Cause

`Check/CheckError.lean`, `TypeEnv.vars : List (LocalRef × VarInfo)`. `addVar` filtered the whole
list per binding; every lookup (`lookupLocal`, `lookupByUid`) walked it; every state change
(`useVar`, `consumeVar`, assignments, defers, freezes, merges) rebuilt it with `map`. At every block,
branch, arm, loop, `defer` scope, `return`, `break` and assignment, whole-list scans
(`deferScopeEntry`/`Exit`, `checkBlockLocalsConsumed`, `mergeVarStates`, `closeBlockScope`,
`activeBorrowRefs`, `restoreAfterLoop`, the match-arm agreement and restore) walked every binding,
each with a nested `lookupByUid` walk.

## Fix

`VarEnv`, an ordered persistent map that iterates newest first exactly as the list did (so every
scan, and the first refusal it reports, is unchanged). Lookup, registration and single-binding
update are logarithmic. Three derived subsets kept by every mutation (the non-Copy bindings, the
bindings with a borrow source, the bindings a `defer` reserves, pins or holds) serve the scans that
can only refuse or change those, and a change tick per binding (`touchedSince`, `closeTo`) finds
what a block, loop or arm changed relative to the snapshot it descends from.

## Structural cause

Class 17: pass state in association lists and whole-state recomputation per construct. The
ownership state of every binding was re-derived by scanning at each construct instead of being
indexed by the questions asked of it. The other front-end stages had the same shape in the same functions: bugs 255-261 were one
measurement, one cause per structure.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes): per-function pass state sized for the
function (maps and indexed subsets), with the long-function shapes as its envelope.
