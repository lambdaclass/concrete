# Bug 146 — `*h` on a `Heap<T>` loaded the value but never freed the cell

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
`regress_146_heap_deref_frees.con` (run, prints 42) and a `run_tests.sh` leg asserting the
reproducer's emitted SSA calls `@free`.

## Structural cause

Storage decided ad hoc in `Lower` ([structural class](STRUCTURAL_CLASSES.md) 8): one Core node
carried two meanings (a load through a reference, a move out of an owned cell), and the lowering
implemented only the first.

## Structural owner

Containment: ROADMAP exit 0b. The structural repair is R-0501 (Place MIR: a move out of an owned
place and its release are explicit operations).
