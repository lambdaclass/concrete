# Bug 259 — Elab grows binder frames by append and rebuilds them at every block exit

<!-- bug-ledger
summary: Elab appended every binder's spelling to its lexical frame, rebuilt every frame with tombstones at each block exit and searched a list of bound locals per use (40000 `let`s: 31 s; 4000 `let`s each with an `if`: 48.6 s; now 188 ms and 34 ms). Frames are kept as their shape, which is all a binder position needs (no reference resolves by spelling since R-0492), and bound locals in a map; positions are unchanged. Pinned by `regress_259_binder_positions.con`, the proof and identity gates and the long-function leg. Found with bug 255. Class 17.
fixtures: regress_259_binder_positions.con
class: 17 — one function of many statements: the parser's statement lists, the local binder table, the resolver's scopes, the checker's bindings, Elab's binder frames, ElabVerify's and CoreCheck's scopes, each a list appended or searched per statement
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/perf-frontend`)
**Found:** 2026-10-25, measuring the long-function quadratic the previous perf agent reported (40000 `s = s + 1;` statements: 8.5 s parse, 13.4 s resolve, 9.5 s check, 9.5 s elaborate), per stage, for flat statements, long `if`/`while` bodies, chained `let`s and `let`s interleaved with blocks.
**Severity:** loud (performance). Elaborating a function of n bindings cost n² (40000 `let`s:
31 s), and each block exit cost the bindings so far (n = 4000 `let`s each followed by an `if`: 48.6 s).

## Reproducer

`fn main` with 40000 chained `let`s: elaborate 31 s; fixed 188 ms (80000: 454 ms); 4000 `let`s
each followed by an `if`: 48.6 s, fixed 34 ms. `regress_259_binder_positions.con` elaborates the
constructs whose binders take frame positions; the positions themselves (evidence binder
references, proof fingerprints) are unchanged, which the proof and identity-row gates pin; the
long-function leg bounds the time.

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

`Elab/Elab.lean`. `extendFrame` appended each binder's spelling to its frame (`f.concat name`) and
`addBinder` measured the frame and the stack (`length`) per binder; `exitBlock` rebuilt every frame
of the stack with tombstones at each block exit (`tombstoneBlock`); `bound`, the elaborated
bindings, was a list searched per use (`useLocal?`).

## Fix

The frame stack is its shape (`FrameShape`: binder count per frame and depth), which is all a
binder position needs: since R-0492 step 2 nothing resolves a reference by spelling, so frame
spellings were never read and the tombstones changed nothing observable (`exitBlock` leaves the
shape unchanged). `bound` is a persistent `Std.TreeMap` keyed by identity (a scope restore hands
back an earlier map, so it must share structure). Positions are computed exactly as before.

## Structural cause

Class 17. The frame stack kept a representation (spellings) for a lookup R-0492 retired, and
paid for its upkeep per binder and per block. The other front-end stages had the same shape in the same functions: bugs 255-261 were one
measurement, one cause per structure.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes): per-function pass state sized for the
function (maps and indexed subsets), with the long-function shapes as its envelope.
