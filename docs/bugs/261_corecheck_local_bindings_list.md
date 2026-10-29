# Bug 261 — CoreCheck searches a list of local bindings per reference (quadratic in function size)

<!-- bug-ledger
summary: CoreCheck looked up every reference to a local in a list of the bindings introduced so far, so a function whose statements read an early binding took time n² to validate (40000 `let`s each with an `if` reading one: 16 s of `--emit-core`, now 3.7 s). Persistent maps by identity and by Core name. Pinned by `regress_261_corecheck_far_reads.con` and the long-function leg. Found timing bug 255's leg. Class 17.
fixtures: regress_261_corecheck_far_reads.con
class: 17 — one function of many statements: the parser's statement lists, the local binder table, the resolver's scopes, the checker's bindings, Elab's binder frames, ElabVerify's and CoreCheck's scopes, each a list appended or searched per statement
silent: no
area: Check/CoreCheck — the record's Fix (CoreVars); the commits naming it are a seven-bug batch
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/perf-frontend`)
**Found:** 2026-10-25, timing the long-function suite leg after bugs 255-260 were fixed: `--emit-core` of 40000 `let`s each with an `if` still took 16 s, and a stage probe put it in CoreCheck.
**Severity:** loud (performance). Validating the Core of a function whose statements read an early
binding cost the square of its length.

## Reproducer

40000 `let`s each followed by `if s<i> > t { t = t + 1; }` (reading `t`, declared at the top):
`--emit-core` took 16.3 s with bugs 255-260 fixed, almost all CoreCheck; 3.7 s fixed (dev host
under load), with CoreCheck itself 0.1 s. `regress_261_corecheck_far_reads.con` pins far reads, sibling arm binders and parameters; the long-function leg (its `if`, `while` and `match`
shapes read `t` per statement) bounds the time.

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

`Check/CoreCheck.lean`, `CoreCheckEnv.vars : List (LocalKey × Ty)`: `lookupVar` and the indirect
call check searched it for every reference, newest first, so a read of an early binding walked
every binding introduced after it.

## Fix

`CoreVars`, persistent maps by identity and by Core name; a later binding of a key replaces the
earlier (the list's first match was the newest); a parameter list keeps the first of a repeated
key, as before. Blocks and arms restore the earlier map, which shares structure.

## Structural cause

Class 17: pass state in association lists (bug 256's shape, one stage later). The other front-end stages had the same shape in the same functions: bugs 255-261 were one
measurement, one cause per structure.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes): per-function pass state sized for the
function (maps and indexed subsets), with the long-function shapes as its envelope.
