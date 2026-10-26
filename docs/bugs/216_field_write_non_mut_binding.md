# Bug 216 — a field of a binding not declared `mut` can be assigned

<!-- bug-ledger
summary: a write into a field, a nested array element or a heap cell of a binding not declared `mut` is accepted, while `&mut` of the same place is refused (E0233) and a direct array element write is refused (E0217, bug 030); both backends agree. Whether OWN-10 covers such writes is not stated (post-R-0492 differential hunt, 2026-10-23) ((needs a language decision))
coverage: a write into a field, nested element or heap cell of a binding not declared mut is accepted while &mut of the same place is refused (needs the OWN-10 scope decision); reproducer tests/regressions/bug216_field_write_non_mut_binding/repro.con, found by the post-R-0492 differential hunt 2026-10-23; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, compiles)
class: 5 — OWN-10's mutability half applied per statement form, not at the normalized place: field writes are not checked
silent: no
-->

**Status:** OPEN (needs a language decision: whether OWN-10 covers writes into a binding's fields,
nested elements and heap cell)
**Found:** 2026-10-23, in the post-R-0492 differential hunt, while re-running the fuzz farm's
`crash-206151600c0e` (a mutated program assigned `s0.y = ...` with `let s0`, not `let mut s0`).
**Severity:** a program the mutability rule appears to refuse is accepted; both backends agree, so no
wrong code. The compiler is inconsistent with itself: `&mut s.y` on the same binding is refused
(E0233) and so is `a[1] = v` on a non-`mut` array (E0217, bug 030).

Codex review 2026-10-25: NOT CONFIRMED — pending owner decision OD17/exit 2n: whether OWN-10 covers field and nested-place writes is explicitly unanswered; acceptance alone proves no defect.

## Reproducer

`tests/regressions/bug216_field_write_non_mut_binding/repro.con`:

```con
struct Copy P { x: i32, y: i32 }
struct Copy Q { a: [i32; 2] }

fn main() -> Int {
    let s: P = P { x: 1, y: 1 };
    s.y = 20;
    let q: Q = Q { a: [1, 2] };
    q.a[1] = 20;
    return (s.x + s.y + q.a[1]) as Int;
}
```

Observed on main `dcc48dc1`: compiles; the binary and `--interp` return 41. The same holds for
`a[1].x = v` on a non-`mut` array of structs and `h.x = v` on a non-`mut` `Heap<P>` binding, while
`set(&mut h.x)` and `s.bump()` (a `&mut self` method) are refused with E0233.

## Cause

`Check.checkWriteAccess`, the rule for a write into a normalized place, checks aliasing only; its
docstring records "the binding-mutability rule for field writes is unchanged". `checkMutAccess`,
the rule for `&mut place`, also requires the owned root to be `mut`. Bug 030 added the mutability
requirement to `arrayIndexAssign` for a directly named array only, so the rule is applied per
statement form: a bare-name assignment, a direct array element, a `&mut` borrow, but not a field,
a nested element or a heap cell.

## Decision needed

OWN-10@1 reads "a binding is assigned, or borrowed with `&mut`, only when it is declared `mut`".
Whether writing a field or element of a binding is "assigning" it is not stated. If it is (the
reading bug 030 took for arrays), the contained fix is one line in `checkWriteAccess`: an owned
root that is not `mut` is E0217, the same predicate `checkMutAccess` applies, after which bug 030's
array-specific check is redundant. If it is not, `&mut s.f` and `a[i] = v` should be admitted
instead. The corpus has to be swept either way (bug 030's fix found one fixture relying on the
hole). Not changed here: the language-tightening proposals are under negotiation.

## Structural cause

Class 5 ("borrow tracking by syntax instead of normalized places"): the mutability half of OWN-10
is enforced per statement form (bare assignment, direct array element, `&mut` borrow) instead of
once at the normalized place every write and borrow goes through.

## Structural owner

R-0499 (S8 over Core places: one mutable-access rule for every write and `&mut`), after the
OWN-10 scope decision.
