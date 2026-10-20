# Bug 104 — a reference assigned to an outer variable outlives its block-local source

**Status:** FIXED 2026-10-20 (batch 2, bug 091 normalized place rule)
**Found:** 2026-10-20 by the V1 design examples (branch `v1-design-examples`, design question B6:
alias propagation through assignment).
**Severity:** critical. Use after free in safe code, silent wrong code. Assigning a reference to a
block-local value into an outer reference variable records no borrow, so the source can be freed, or
can go out of scope, while the outer reference is still read.

## Reproducer

```con
struct Copy B { v: Int }

fn f(init: &Heap<B>) with(Alloc) -> Int {
    let mut leaked: &Heap<B> = init;
    if true {
        let h: Heap<B> = alloc(B { v: 77 });
        leaked = &h;
        free(h);
    }
    let h2: Heap<B> = alloc(B { v: 99 });
    let out: Int = leaked.v;
    free(h2);
    return out;
}

fn main() with(Alloc) -> Int {
    let h0: Heap<B> = alloc(B { v: 1 });
    let r: Int = f(&h0);
    free(h0);
    return r;
}
```

On `094e51bf` (main) the checker accepts it and the binary exits **0**: `leaked.v` reads the freed
allocation. The control that reads `leaked.v` before `free(h)` (otherwise identical) exits 77. The
interpreter cannot run either (`interp: undefined function 'alloc'`), so the interpreter-vs-native
differential gates do not see it.

The same hole without a heap: a reference to a block-local stack value survives the block.

```con
struct Copy Point { x: Int, y: Int }

fn main() -> Int {
    let p: Point = Point { x: 10, y: 20 };
    let mut leaked: &Point = &p;
    if true {
        let q: Point = Point { x: 30, y: 40 };
        leaked = &q;
    }
    return leaked.x;
}
```

Accepted; the binary exits 30 by reading `q`'s dead stack slot, and the interpreter refuses it
(`interp: ref path base 'q' not in env`). That disagreement is itself the defect's signature: the
interpreter's environment model knows `q` is gone, the checker does not.

## Cause

`Concrete/Check/Check.lean` records a reference's source (`borrowedFrom`) only in `letDecl`, and
only for a bare `let x = &y` / `let x = &mut y`. The `.assign` case checks only that the right-hand
side is not a borrow-block reference (E0234); `leaked = &h` records nothing. So `free(h)` sees no
active borrow of `h` (`activeBorrowRefs` finds none) and no check compares the lifetime of `h`'s
scope with the scope of the variable the reference is stored in. References are second-class and
never returned (E0236) or stored in an aggregate (bug 092, E0239 on `bug090-checker-ownership` /
`land4`), but they can still be stored, by assignment, in a longer-lived local.

The bug 092 fix covers aggregates only (struct fields, enum payloads, array elements and types that
hold a reference); a bare reference local reassigned from an inner scope is outside it. Binding the
borrow first (`let r2: &Heap<B> = &h; leaked = r2; free(h);`) is already refused on main (E0204,
`h` is borrowed), because `let` records the source; only the direct assignment form escapes.

## Candidate repair

Batch 2 (`land2-fix` at `0b897854` and again at `69254623`, bug 091's round-3 normalized place rule) rejects both
reproducers with E0238: `recordRefProvenance` runs at reassignment, and a reassignment that
re-points a reference binding at different storage is refused ("provenance is not joined across
branches; use a new binding"), a deliberate restriction until R-0499. Verified by building that
branch: `esc4` and the stack variant are both E0238 at `leaked = &h` / `leaked = &q`. The bug closes
when batch 2 lands with both programs as permanent rejection fixtures.

## Fix

Bug 091's round-3 normalized place rule (`land2-fix`): `recordRefProvenance` runs at every `let` and
reassignment of a reference binding, recording the normalized source place, and a reassignment that
re-points a reference at different storage is refused (E0238; provenance is not joined across
branches). Both reproducers above are rejected at `leaked = &h` / `leaked = &q`. Permanent rejection
fixtures: `tests/programs/error_104_assigned_ref_heap_block.con`,
`tests/programs/error_104_assigned_ref_stack_block.con` (run_tests.sh, `r091_table`'s neighbours).
The rejection is a deliberate restriction (KNOWN_HOLES H28) that R-0499 lifts with flow-sensitive
loans.

## Structural cause

The source checker has no reference-provenance model. Borrow facts are attached to the syntactic
form that created them (`let x = &y`), keyed by a source name, rather than derived from where a
reference value flows: an assignment, a branch merge or a reborrow moves a reference without moving
its provenance. Each such path has been patched one form at a time (092 aggregates, 091 receivers
and arguments, 105 references chosen between branches), and this is the assignment form of the
same missing model. It is also an instance of borrow tracking by syntax: each check recognizes the forms
it was written for, and bug 091 converged only once one normalized-place rule replaced them
([structural classes](STRUCTURAL_CLASSES.md) 4 and 5).

## Structural owner

Queue row 4 (R-0499, the authoritative checker on Core): references carry provenance (a loan on a
normalized place, by `LocalId`) through every assignment and merge, and a loan's place must outlive
every variable that can hold it; exit 2i states the second-class reference rule this must enforce.
Until the Core checker is authoritative, the repair under exit 0b is batch 2's normalized place
rule (provenance recorded on assignment, re-pointing refused), with both programs above as
permanent rejection fixtures.

**Repair owner:** queue exit 0b (memory safety in safe code).
