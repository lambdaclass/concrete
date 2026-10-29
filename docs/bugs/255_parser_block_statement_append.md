# Bug 255 — the parser appends each statement of a block to the list built so far (quadratic)

<!-- bug-ledger
summary: the parser appended each statement of a block to the list built so far, so a block of n statements took time n² to parse (40000 statements: 9.4 s, now 0.11 s). Accumulated in an `Array`. Pinned by `regress_255_block_statement_order.con` and the long-function leg (functions of 80000 statements, 80000 `let`s and 20000 `let`s with an `if`, `while` or `match` each, through `--emit-core` within 10 s). Found measuring the long-function quadratic the previous perf agent reported. Class 17.
fixtures: regress_255_block_statement_order.con
class: 17 — one function of many statements: the parser's statement lists, the local binder table, the resolver's scopes, the checker's bindings, Elab's binder frames, ElabVerify's and CoreCheck's scopes, each a list appended or searched per statement
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/perf-frontend`)
**Found:** 2026-10-25, measuring the long-function quadratic the previous perf agent reported (40000 `s = s + 1;` statements: 8.5 s parse, 13.4 s resolve, 9.5 s check, 9.5 s elaborate), per stage, for flat statements, long `if`/`while` bodies, chained `let`s and `let`s interleaved with blocks.
**Severity:** loud (performance). Every block of n statements (a function body, a branch, a loop
body, an arm) cost the square of n to parse, for the binary, `--interp` and every report.

## Reproducer

`fn main` with 40000 `s = s + 1;` statements: 9.4-11 s to parse; fixed 0.11 s (80000: 0.22 s).
`regress_255_block_statement_order.con` pins statement order in plain and if-expression blocks,
every assignment form and trailing values; the long-function suite leg parses and elaborates
generated functions of 80000 statements (and the other shapes below) within 10 s each, retried
once with 20 s; the old front end exceeded the retry on every shape.

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

`Parser.lean`, `parseStmtList` and `parseExprBlock`: each parsed statement was appended to the
statements so far (`stmts := stmts ++ [stmt]`), copying the list every time.

## Fix

Both accumulate in an `Array` and convert to a list once. The append ratchet for the parser is
59 -> 51.

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): a list grown by append
in a loop, the shape bug 249 fixed for array literals in the same file. The other front-end stages had the same shape in the same functions: bugs 255-261 were one
measurement, one cause per structure.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes): per-function pass state sized for the
function (maps and indexed subsets), with the long-function shapes as its envelope.
