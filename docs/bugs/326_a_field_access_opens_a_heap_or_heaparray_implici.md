# Bug 326 — A field access opens a Heap or HeapArray implicitly

<!-- bug-ledger
summary: A field access opens a Heap or HeapArray implicitly, which MEM-4 refuses as the owner decided OD30 on 2026-10-29
coverage: reproducer tests/regressions/bug326_a_field_access_opens_a_heap_or_heaparray_implici/repro.con, found checking main against OD30 on 2026-10-29; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 42) and check_spec.sh S4 (the pending MEM-4 example)
class: 13
silent: yes
-->

**Status:** OPEN
**Found:** 2026-10-29, by checking main against the owner's tightening OD30 ("strict across the board", 2026-10-29), one program per tightening, native against `--interp`, on main 0bf8350a.
**Severity:** silent under the decided rule: a program MEM-4 refuses is accepted, and both consumers agree on its result.

Codex confirmation 2026-10-29: CONFIRMED (all ten tightenings OD23 – OD32 were confirmed in one review of main's measured behaviour; for OD30: the code site below produces it).

## Reproducer

`tests/regressions/bug326_a_field_access_opens_a_heap_or_heaparray_implici/repro.con`.

## Observed

On main 0bf8350a (built on OVH): `h.v` on `h: Heap<P>` reads the field through the heap cell and both consumers return 42.

## Expected

[MEM-4](../spec/memory.md#MEM-4) (OD30): `e.f` requires `e` to be the struct or a reference to it; a `Heap` or `HeapArray` is opened with an explicit non-consuming place form, and `*` stays the consuming move-out.

## Cause

`Concrete/Check/Check.lean`, `checkExprCore`, and `Concrete/Elab/Elab.lean`, `elabExprEv`: a field access dereferences one reference and then one `Heap` layer implicitly.

## Structural cause

Class 13: a decided rule with no enforcing site. The rule changed on 2026-10-29 and main still
implements the earlier, looser one.

## Structural owner

Exit 2q (one migration of std, examples and tests to the decided rules) with R-0499 (the Core
checker, by stable rule ID).

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg, and MEM-4's
pending example records main's outcome. A program written to the decided rule is accepted today too.
