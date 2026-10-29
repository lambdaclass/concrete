# Bug 235 — the interpreter's array element read walks the whole array

<!-- bug-ledger
summary: under `--interp` every `a[i]` converted the array to a list and walked it (`arrayGet` over `elems.toList`, the form the R-0460 bounds theorems were proved against), so a read cost the array's length (400000 reads of 10000 elements: 35 s, now 0.6 s). `arrayGet` is `elems[n]?` and the theorems are restated against it. Pinned by `regress_235_interp_array_read.con`. Found by the differential hunt of 2026-10-24. Class 17.
fixtures: regress_235_interp_array_read.con
class: 17 — an interpreter array read converted the array to a list per read
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/diffhunt-1025`)
**Found:** 2026-10-24, by the differential hunt on main `9622f7e0` (loops under `--interp`): a loop
reading a 40000-element array took 20 s under `--interp` (after bug 236's share is removed, about
16 s of it was the reads), against milliseconds natively.
**Severity:** loud (performance). Every `a[i]` under `--interp`, the oracle of the differential
checks, cost the array's length.

Confirmed real by Codex review, 2026-10-25: ROADMAP R-0422 performance defect: every indexed read materialized and traversed an array-sized list; 2f6720da uses direct indexing without changing bounds semantics.

## Reproducer

`tests/programs/regress_235_interp_array_read.con`: 400000 reads of a 10000-element array. The
binary returns 42. Under `--interp`: 35 s on main, 0.6 s fixed.

## Cause

`Interp.arrayGet`, the lookup of an index read (`a[i]`) and of a reference's `.index` step, was
`listGet elems.toList n`: it converted the value's `Array` to a `List` and walked it, an O(size)
copy and walk per read. It was written so for the R-0460 theorems
(`bounds_obligation_sufficient` / `_necessary`), which were proved by induction over `listGet`.

## Fix

`arrayGet elems n = elems[n]?`, the array's own O(1) lookup. The two R-0460 theorems are restated
against it and proved by `simp` from the same hypotheses (their statements are unchanged), and the
list lookup and its two lemmas, now unused, are removed. Pinned by
`regress_235_interp_array_read.con` (`run_ok … 42`, and `--interp` = 42 within 20 s under
`bounded_run -r 20`: the retry has the same budget, below the 35 s the defect takes). An element WRITE under `--interp` still copies the array (the environment holds the
old value while `Array.set` runs), linear per write; that is the environment design R-0422 owns, not
this bug.

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): the interpreter's value
model holds arrays as `Array`, and the lookup recomputed a list view of the whole value per query
to match the form a proof was written against.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes), with the interpreter's environment and
value storage.
