# Bug 324 — A Copy type or an array satisfies a Destroy bound without a Destroy impl

<!-- bug-ledger
summary: A Copy type or an array satisfies a Destroy bound without a Destroy impl, which TRAIT-10 refuses as the owner decided OD28 on 2026-10-29
coverage: reproducer tests/regressions/bug324_a_copy_type_or_an_array_satisfies_a_destroy_boun/repro.con, found checking main against OD28 on 2026-10-29; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 42) and check_spec.sh S4 (the pending TRAIT-10 example)
class: 13
silent: yes
-->

**Status:** OPEN
**Found:** 2026-10-29, by checking main against the owner's tightening OD28 ("strict across the board", 2026-10-29), one program per tightening, native against `--interp`, on main 0bf8350a.
**Severity:** silent under the decided rule: a program TRAIT-10 refuses is accepted, and both consumers agree on its result.

Codex confirmation 2026-10-29: CONFIRMED (all ten tightenings OD23 – OD32 were confirmed in one review of main's measured behaviour; for OD28: the code site below produces it).

## Reproducer

`tests/regressions/bug324_a_copy_type_or_an_array_satisfies_a_destroy_boun/repro.con`.

## Observed

On main 0bf8350a (built on OVH): `fn id<T: Destroy>(x: T) -> T` called with `42` compiles and both consumers return 42.

## Expected

[TRAIT-10](../spec/traits.md#TRAIT-10) (OD28): a `Destroy` bound is satisfied only by an impl; a Copy type is not `Destroy` and is discarded with `discard`.

## Cause

`Concrete/Check/CheckHelpers.lean`, `checkTraitBounds`: `Destroy` is satisfied by any Copy type and by an array of destroyable elements.

## Structural cause

Class 13: a decided rule with no enforcing site. The rule changed on 2026-10-29 and main still
implements the earlier, looser one.

## Structural owner

Exit 2q (one migration of std, examples and tests to the decided rules) with R-0499 (the Core
checker, by stable rule ID).

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg, and TRAIT-10's
pending example records main's outcome. A program written to the decided rule is accepted today too.
