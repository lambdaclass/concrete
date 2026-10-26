# Bug 268 — check_binder_refs.sh's empty-scope leg did not detect eagerly opened binder frames

<!-- bug-ledger
summary: check_binder_refs.sh's empty-scope leg read only an `if`, which never enters a frame scope, so binder frames opened eagerly on scope entry passed it; mutation 56 modeled another defect
coverage: check_binder_refs.sh leg "a match arm that binds nothing does not shift framesOut", and test_mutation.sh mutation 56 (frames open eagerly, on scope entry) is KILLED by it
class: 11 — a gate that could not see the defect it names
silent: no
-->

**Status:** FIXED (branch `wip/perf-backend`)
**Found:** 2026-10-25, by the front-end perf agent while repointing mutation 56 to Elab's new frame
shape (bug 259): the faithful mutation survived.
**Severity:** gate coverage. A real regression (binder frames pushed when a scope is entered rather
than when its first binder arrives) would shift the relative frame of every reference inside a
match arm that binds nothing, moving proof subject digests for a semantically irrelevant edit, and
no gate would fail.
Confirmed real by Codex review, 2026-10-25: CHECK 1 never exercises `openScope`, the structural leg only searches for `pendingFrame`, and the old mutation pushed a frame per binder rather than making scope entry eager.

## Reproducer

Replace `openScope`'s body with `setEnv { env with bodyScope := env.bodyScope.push [], pendingFrame
:= false }` (frames open on scope entry): `check_binder_refs.sh` passed 15/15 (2026-10-25, on
main 4b04e4d8).

## Cause

The leg "a scope that binds nothing does not shift framesOut" reads `if p > 0 { return p; }`. An
`if` never calls `openScope` (`enterBlock` opens no frame); only match arms do. The mechanism leg
greps for `pendingFrame`, which the mutation keeps. Mutation 56 (`extendFrame .. true`: every
binder its own frame) was killed by the positional legs, so the harness reported the empty-scope
leg as load-bearing when nothing exercised it.

## Fix

Leg 1b: a match arm that binds nothing (`E::B { } => { return p; }`) must keep `p` in the function
frame (positions `(0,0) (0,0) (0,1)`). Mutation 56 is now the faithful defect and is KILLED by that
leg (`test_mutation.sh --mutation 56`, confirmed green once restored).

## Structural cause

Class 11: absence read as a fact. A leg that holds because its input never reaches the mechanism
reads as a pass; the mutation that was meant to prove the leg load-bearing modeled a different
defect, which other legs caught.

## Structural owner

R-0513 (impact gate selection and mutation coverage): every mutation models the defect its
description names, and its kill is attributed to the leg that names it.
