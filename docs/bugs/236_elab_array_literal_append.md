# Bug 236 — Elab appends an array literal's elements one by one (quadratic)

<!-- bug-ledger
summary: Elab built an array literal's Core element list by appending one element at a time, so `[v; n]` (n elements after parsing) took time n² for every consumer (n = 40000: 4 s; 200000: over a minute; now 0.14 s). Prepended and reversed once, as its evidence list already was. Pinned by `regress_236_array_repeat_literal.con`. Found isolating bug 235. Class 17.
fixtures: regress_236_array_repeat_literal.con
class: 17 — Elab appended an array literal's elements one at a time
silent: no
area: Elab/Elab — the record's summary and Fix (Elab's element list); its commits are a seven-bug batch
-->

**Status:** FIXED (branch `wip/diffhunt-1025`)
**Found:** 2026-10-24, by the differential hunt on main `9622f7e0`, isolating bug 235: a program
whose only statement is `let a: [i64; 40000] = [1; 40000];` took 4 s under `--interp` and as long
with `--emit-core`, while `--emit-ast` took 0.01 s.
**Severity:** loud (performance). Every array literal cost the square of its length in Elab, for
the binary, `--interp` and every report alike.

Confirmed real by Codex review, 2026-10-25: ROADMAP exit 2o requires near-linear stage scaling; Elab’s repeated list append was quadratic. Its custom bounded gate is valid, despite lacking plain run_ok registration.

## Reproducer

`tests/programs/regress_236_array_repeat_literal.con`: `[7; 200000]`. On main `--emit-core` did
not finish in 60 s (n = 10000: 0.22 s, 20000: 1.0 s, 40000: 4.0 s); fixed it takes 0.14 s, and
`--interp` returns 42.

## Cause

The parser expands `[v; n]` to n element expressions (bug 026 made that O(n)). Elab's `.arrayLit`
case built the Core element list by `cElems := cElems ++ [cE]` per element, copying the list built
so far each time. The evidence list beside it had been fixed to prepend-and-reverse; the element
list had not.

## Fix

The elements are prepended and the list reversed once, as the evidence list already was. Pinned by
`regress_236_array_repeat_literal.con`: `run_tests.sh` elaborates it (`--emit-core`) and runs it
under `--interp` (42) within 20 s each under `bounded_run -r 20` (retry included). The literal is not compiled natively in
the suite: the backend's cost for a literal of that size is bug 027's.

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): a list grown by append
in a loop; `check_no_quadratic_append.sh` ratchets the count of `++ [` per file but does not find
an existing occurrence, and this one predated the ratchet's baseline.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes).
