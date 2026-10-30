# Bug 338 — A borrow mut block borrows a binding not declared mut, so the block writes to an immutable binding

**Status:** OPEN
**Found:** 2026-10-30, by the outside-reader test of the specification (60 programs written from SPEC.md alone on wip/spec-phase2 d21a4cfd, run native and under `--interp` on main 71e3e804 and on d21a4cfd).
**Severity:** silent: a program that writes to an immutable binding through `borrow mut` is accepted, and both the binary and `--interp` run it and observe the write (exit 42), where OWN-10 refuses it at check.

Codex confirmation 2026-10-30: REAL-BUG. "OWN-10 allows mutable borrowing 'only when the binding is declared `mut`,' and REF-D3 explicitly says `borrow mut` produces an `&mut T`. Together these make the restriction apply to this borrow block. The documented bug 216 concerns field, nested-element, and heap-cell writes; it does not mark this mutable-borrow bypass as pending."

<!-- bug-ledger
summary: A borrow mut block borrows a binding not declared mut, so the block writes to an immutable binding
coverage: reproducer tests/regressions/bug338_a_borrow_mut_block_borrows_a_binding_not_declare/repro.con, found by the outside-reader spec test 2026-10-30; pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 42)
class: 5
silent: yes
exit: 0b
ruling: Violates: OWN-10@2
-->

## Reproducer

`tests/regressions/bug338_a_borrow_mut_block_borrows_a_binding_not_declare/repro.con`:

```con
fn inc(r: &mut Int) {
    *r = *r + 1;
}

fn main() -> Int {
    let x: Int = 41;
    borrow mut x as r in R {
        inc(r);
    }
    return x;
}
```

## Observed

The program compiles; the binary exits 42 and `--interp` prints 42. The same program with
`inc(&mut x)` in place of the borrow block is refused with E0233 ("cannot mutably borrow").

## Expected

[OWN-10@2](../spec/ownership.md#OWN-10): a binding "is assigned, or borrowed with `&mut`, only when
the binding is declared `mut`", and a `borrow mut x as r` block binds `r` to a `&mut T` of `x`
(REF-D3@1 on the specification branch wip/spec-phase2, not yet on main; on main,
[REF-13@1](../spec/references.md#REF-13) states that such a `&mut` refers to the place it borrows). The program is refused at check with E0233, on both consumers.

## Cause

`Concrete/Check/Check.lean`, `checkStmt`'s `.borrowIn` case: it checks that the owner is not
moved (`cannotBorrowMoved`), not frozen (`variableFrozenByBorrow`) and not already borrowed
(`cannotMutBorrowImmBorrowed`, `cannotMutBorrowAlreadyBorrowed`), then freezes it and binds `r`
at `Ty.refMut`, but never asks whether the owner is declared `mut`. The `mut` test
(`cannotMutBorrowImmutable`, E0233) lives in `checkMutAccess`, which the `&mut e` expression
form calls and the borrow-block statement form does not.

## Structural cause

Class 5 (borrow tracking by syntax instead of normalized places): OWN-10's mutability half is
applied per surface form (`&mut e`, assignment, `&mut self` receiver) rather than once at the
normalized place every `&mut` loan is taken from, so a form that takes a mutable loan by another
route (here the borrow block) skips it. Bug 216 is the same rule missed for field, element and
heap-cell writes.

## Structural owner

R-0499 (S8 over Core places: one mutable-access rule for every write and every `&mut` loan,
whatever form takes it), as for bug 216.

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg (it still exits
42). Meanwhile a reviewer treats a `borrow mut` of a binding as a write to it and checks that the
binding is declared `mut`.
