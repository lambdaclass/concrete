# Bug 257 — resolve-stage visible bindings are lists searched by spelling (quadratic in bindings)

<!-- bug-ledger
summary: the resolve stage's visible bindings (the local walk's chain and Resolve's local scopes) were lists searched by spelling per binder and per use (40000 `let`s: 221 s to resolve, now 63 ms). Persistent maps. Pinned by `regress_257_scope_visibility.con`, `error_257_*` and the long-function leg. Found with bug 255. Class 17.
fixtures: regress_257_scope_visibility.con error_257_read_after_block.con error_257_rebind_visible.con
class: 17 — one function of many statements: the parser's statement lists, the local binder table, the resolver's scopes, the checker's bindings, Elab's binder frames, ElabVerify's and CoreCheck's scopes, each a list appended or searched per statement
silent: no
-->

**Status:** FIXED (branch `wip/perf-frontend`)
**Found:** 2026-10-25, measuring the long-function quadratic the previous perf agent reported (40000 `s = s + 1;` statements: 8.5 s parse, 13.4 s resolve, 9.5 s check, 9.5 s elaborate), per stage, for flat statements, long `if`/`while` bodies, chained `let`s and `let`s interleaved with blocks.
**Severity:** loud (performance). A function of n bindings cost the square of n to resolve: 40000
`let`s took 221 s.

## Reproducer

`fn main` with 40000 chained `let`s: resolve 221 s; fixed 63 ms (80000: 153 ms).
`regress_257_scope_visibility.con` pins sibling blocks reusing a spelling, a far read, an outer
binding read after an inner block closes and a `for` header binder; `error_257_read_after_block.con`
(E0100) and `error_257_rebind_visible.con` (E0301) pin the refusals; the long-function leg bounds
the time.

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

Two structures of the resolve stage. `Resolve/Locals.lean`, the local walk: the visible bindings
were a `List Visible`, and every binder asked `vis.any` (the `shadows` fact, a walk of the whole
chain when the spelling is new, the common case) and `vis.find?` (the SCOPE-1 collision), and every
use asked `vis.find?`. `Resolve/Resolve.lean`, `lookupName`: each local scope was a list searched by
spelling for every identifier, so a read of a binding declared far above walked every binding
between.

## Fix

The walk keeps, per spelling, the innermost source binding and whether any visible binding is an
`elabVar`: the only questions it asks. Persistent `Std.TreeMap`s, since sibling scopes extend one
chain independently. Resolve's scopes are `Std.TreeMap`s searched by membership (their recorded
kind is never read).

## Structural cause

Class 17: scopes represented as lists searched per lookup, the same shape as the interpreter's
environment in bug 228. The other front-end stages had the same shape in the same functions: bugs 255-261 were one
measurement, one cause per structure.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes): per-function pass state sized for the
function (maps and indexed subsets), with the long-function shapes as its envelope.
