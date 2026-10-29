# Bug 248 — an element write under `--interp` copies the whole array

<!-- bug-ledger
summary: under `--interp` an element write (`a[i] = v`, `s.f[i] = v`, through `&mut`, `vec_push`/`vec_set`/`vec_pop`) built the new array while the environment still held the old one, so every write copied the whole array (regression program: 64.6 s, now 5.5 s). A write takes the value out of its binding and container first, so the update is in place. Pinned by `regress_248_interp_array_write.con`. Found during bug 235's fix. Class 17.
fixtures: regress_248_interp_array_write.con
class: 17 — an interpreter element write copied the array the environment still held
silent: no
ruling: Outside the spec: performance — interpreter element writes copied the whole array
-->

**Status:** FIXED (branch `wip/perf-quadratics`)
**Found:** 2026-10-24, during bug 235's fix (its record notes that a write still copied the array);
filed 2026-10-25.
**Severity:** loud (performance). Under `--interp`, the oracle of the differential checks, every
`a[i] = v`, `s.f[i] = v`, write through `&mut`, `vec_push`, `vec_set` and `vec_pop` cost the
length of the array or Vec it changed.

Confirmed real by Codex review, 2026-10-25: ROADMAP R-0422 performance defect: retaining the old array forced whole-array copies on individual updates; 0026f7b0 removes that accidental sharing.

## Reproducer

`tests/programs/regress_248_interp_array_write.con`: 100000 writes each into a local array, through
`&mut`, into a struct's array field, and 100000 `vec_push`, 100000 `vec_set` and 50000 `vec_pop` on
a Vec, all of 100000 elements. The binary returns 42. Under `--interp`: 64.6 s on main, 5.5 s fixed.
A loop writing a 10000-element array 400000 times: 4.1 s, 0.8 s fixed; 100000 `vec_push` and
100000 `vec_set`: 16.7 s, 0.55 s fixed.

## Cause

`Interp.lean`: an assignment to a place read the root binding's value (`envGetLocal`, or
`envGet` below a reference's frame in `updatePath`) and built the new value (`updateAt`) while the
environment still held the old one; the Vec builtins read the Vec through `lookupPath` and wrote
a new one back through `updatePath`. With the old array still referenced, `Array.set` and
`Array.push` could not reuse it and copied it. `updateAt` likewise recursed into a field or element
still held by its container, so a write below an aggregate copied that aggregate too.

## Fix

A write takes the value out first. `envTakeLocal` and `envTakeSkipping` remove the root binding's
value from the environment (leaving `.unit` in its slot, rewritten before anything can read it),
`modifyAt` (which `updateAt` now is) takes each field or element out of its container before
rewriting it, and `modifyPath` applies a Vec builtin's change to the value where it is held. The
array then has one reference when it is set or pushed, so the update is in place. Results,
errors and their messages are unchanged: the Vec builtins still validate the path through
`lookupPath` first. Pinned by `regress_248_interp_array_write.con` (`run_ok … 42`, and `--interp`
= 42 within 20 s under `bounded_run -r 20`, which the old interpreter cannot meet).

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): the interpreter's
environment is a persistent association list, and its update path read a value out and wrote a
new one back, so the old value stayed shared during the update and every update of an array was
a copy. Bugs 228, 234 and 235 are the same environment's other costs.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes), with the interpreter's environment and
value storage.
