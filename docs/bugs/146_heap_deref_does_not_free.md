# Bug 146 — `*h` on a `Heap<T>` loaded the value but never freed the cell

<!-- bug-ledger
summary: `*h` on an owned `Heap<T>` loaded the value but never freed the cell, although the checker consumes `h` and MEMORY_SEMANTICS says it frees (a leak; found by Codex reviewing bug 144's migration). Elab now lowers it to the `free` intrinsic. `regress_146_heap_deref_frees.con` plus an emitted-SSA leg. Joins exit 0b. ((`safety-eval2`))
fixtures: regress_146_heap_deref_frees.con regress_146_heap_deref_cast.con error_146_deref_heap_place_write.con error_146_deref_heap_field_read.con
class: 8 — `*h` lowered as a load only
silent: no
exit: 0b
-->

**Status:** FIXED 2026-10-22 (branch `safety-eval2`)
**Found:** 2026-10-22, by the Codex review of bug 144's migration (which moves heap nodes out with
`*h`).
**Severity:** a leak of every heap cell taken apart with `*h` in safe code. The checker consumes `h`
and MEMORY_SEMANTICS §8 says `*h` "loads the value from the heap and frees the memory"; nothing
reported the leak.

## Reproducer

`tests/regressions/bug146_heap_deref_does_not_free/repro.con`:

```con
fn main() with(Alloc) -> Int {
    let h: Heap<Int> = alloc(42);
    return *h;
}
```

On main (`0408f4d4`): `--emit-ssa` shows `call ptr @malloc(i64 8)`, a store, a load and `ret`, and no
call to `free`. Fixed: the emitted SSA calls `@free` on the cell after loading it.

## Cause

Elab elaborated `*h` as `CExpr.deref` whatever the operand type, and `Lower`'s `.deref` emits a
load only. For a reference or raw pointer that is right; for an owned heap shell the dereference is
also the cell's release, which nothing emitted.

## Fix

Elab elaborates a dereference whose operand is a `Heap<T>` as the `free` intrinsic (which loads the
value and then frees the cell), keeping the evidence node a dereference so proof digests do not
move. Field access through a heap shell (`h.f`) is elaborated separately and is unchanged.
Because `*h` now frees, it may not be used as a place (Codex review, round 3): `(*h).x = 5` made
the assignment's base a `free` call that the lowering evaluates twice (a double release), and
`(*h).x` as a projection dropped the moved-out value's other, non-Copy fields. Check refuses a heap
dereference in place position (E0290); a field is read or written through the shell as `h.x`.
A by-value method receiver is a place position too (receivers are checked as places before the
method's receiver mode is known), so `(*h).take()` is refused as well; the spelling is
`let s = *h; s.take()`. That is a deliberate over-approximation, not a soundness need
(`error_091_norm_paren_heap_load.con` now expects E0290).
`regress_146_heap_deref_frees.con` (run, prints 42), `error_146_deref_heap_place_write.con`,
`error_146_deref_heap_field_read.con`, and a `run_tests.sh` leg asserting the reproducer's emitted
SSA calls `@free`. A cast operand is a value, so `(*h) as i32` moves the value out and frees
(`regress_146_heap_deref_cast.con`; Codex review). Migration: 0 sites.

## Structural cause

Storage decided ad hoc in `Lower` ([structural class](STRUCTURAL_CLASSES.md) 8): one Core node
carried two meanings (a load through a reference, a move out of an owned cell), and the lowering
implemented only the first.

## Structural owner

Containment: ROADMAP exit 0b. The structural repair is R-0501 (Place MIR: a move out of an owned
place and its release are explicit operations).
