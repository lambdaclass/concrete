# Bug 234 — the interpreter's recursion is quadratic in its call depth

<!-- bug-ledger
summary: under `--interp` a recursion of depth n took time n² (depth 64000: 44 s; 0.11 s after): every call, branch, `match` arm, block and borrow read the environment's length, and the environment, one association list shared by every frame, was walked to count it. `Interp.Env` keeps its length. Pinned by `regress_234_interp_deep_recursion.con` (depth 100000 under `--interp` within 20 s). Found by the differential hunt of 2026-10-24. Class 17.
fixtures: regress_234_interp_deep_recursion.con
class: 17 — the same environment's length, a scope's restore point and a reference's frame, recomputed by a walk at every call, branch, arm and borrow
silent: no
ruling: Outside the spec: performance — interpreter recursion quadratic in call depth (environment length recomputed by a walk)
-->

**Status:** FIXED (branch `wip/diffhunt-1025`)
**Found:** 2026-10-24, by the differential hunt on main `9622f7e0` (loops and calls under
`--interp`): `fn depth(n) { if n == 0 { return 0; } return 1 + depth(n - 1); }` at depth 100000
returns at once natively and did not finish in 60 s under `--interp`.
**Severity:** loud (performance). `--interp`, the oracle of every `@run` case and of the
differential checks, could not run a deep recursion: the time per call grew with the depth of the
call stack.

Confirmed real by Codex review, 2026-10-25: ROADMAP R-0422 performance defect: repeated List.length traversals made recursion quadratic in depth; 0be0684f replaces traversal with a stored count.

## Reproducer

`tests/programs/regress_234_interp_deep_recursion.con`: a recursion of depth 100000 whose frames
each make a call, take a branch, match an enum and borrow a local. The binary returns 42. The plain
recursion above, under `--interp`:

| depth | main | fixed |
|---|---|---|
| 8000 | 0.40 s | 0.03 s |
| 16000 | 2.50 s | 0.04 s |
| 32000 | 10.7 s | 0.06 s |
| 64000 | 43.8 s | 0.11 s |
| 100000 | > 60 s | 0.17 s |

The fixture at depth 16000 takes 31 s on main; at 100000 it takes 0.4 s fixed.

## Cause

The interpreter's environment (`Interp.Env`) was a bare association list shared by all frames, and
its length is how the interpreter marks a scope: a call, an `if` branch, a `match` arm, a block and
a constant initializer each save `env.length` and drop back to it, and a reference records the
length at its borrow as its frame (`RefPath.frame`). `List.length` walks the whole list, which
holds every enclosing frame's bindings, so each of those operations cost the depth of the call
stack. Bug 228 trimmed loop iterations the same way, so its fix did not reach calls.

## Fix

`Env` is a structure of the bindings and their number (`items`, `size`): binding increments the
count, dropping a scope subtracts, and `Env.length` reads it in O(1). Lookups and updates are the
same list walks as before over `items`; nothing else about the environment changed. Pinned by
`regress_234_interp_deep_recursion.con` (`run_ok … 42`, and `--interp` = 42 within 20 s under
`bounded_run -r 20`, so its retry is 20 s too).

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): a scope's restore point
and a reference's frame are both positions in a list whose length was recomputed per query, so the
cost of every scoped construct grew with execution depth rather than with the construct.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes), extended to the interpreter's
environment: frames that a call or block pushes and pops, with references naming a frame and slot
instead of a list position. A lookup of a borrowed binding still walks the list down to it.
