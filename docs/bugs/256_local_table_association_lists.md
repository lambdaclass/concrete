# Bug 256 — local binder table lookups search association lists (quadratic in function size)

**Status:** FIXED (branch `wip/perf-frontend`)
**Found:** 2026-10-25, measuring the long-function quadratic the previous perf agent reported (40000 `s = s + 1;` statements: 8.5 s parse, 13.4 s resolve, 9.5 s check, 9.5 s elaborate), per stage, for flat statements, long `if`/`while` bodies, chained `let`s and `let`s interleaved with blocks.
**Severity:** loud (performance). Every function of n statements cost the square of n to resolve,
check and elaborate: every binder and every use of a local name was looked up in lists as long
as the function.

## Reproducer

`fn main` with 40000 `s = s + 1;` statements: resolve 12.4 s, check 9.1 s, elaborate 8.8 s; fixed
28 ms, 132 ms and 149 ms (80000: 186, 280, 384 ms). `regress_256_local_table_lookups.con` pins
the lookups (a use far from its binding, an or-pattern arm body reached once per alternative,
arm, `for`-header, borrow-block and destructuring binders); the long-function leg bounds the time.

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

`Frontend/LocalId.lean`, `LocalTable`: `bindersByKey` and `uses` were association lists. The local
walk (`mintBinder`, `mintUse`) searched them before every insertion, and Check (`localAt?`,
`binderRef`) and Elab (`useLocal?`, `binderId`) searched them for every use and binder.

## Fix

Both are `Std.HashMap`s. They are only looked up, never traversed, so no order is observable;
`collisions` is an `Array` pushed in order (the E0301 diagnostics keep their order).

## Structural cause

Class 17: pass state in association lists. The table was introduced by R-0492 slice 1 as the one
answer to "which binding does this use denote"; its representation was never sized for the
function, and three passes query it per node. The other front-end stages had the same shape in the same functions: bugs 255-261 were one
measurement, one cause per structure.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes): per-function pass state sized for the
function (maps and indexed subsets), with the long-function shapes as its envelope.
