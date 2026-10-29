# Bug 133 — a borrow of a field through an explicit dereference, `&mut (*p).f`, borrows a copy

<!-- bug-ledger
summary: `&mut (*p).f` borrowed the field of a COPY of `*p` (`Lower.placeAddr` had no dereference case), so a write through it was silently lost in the binary while `--interp` performed it; `*r` as a place is now the storage `r` points at; pinned by `regress_133_deref_field_borrow.con` (found by the Codex review of bug 116)
fixtures: regress_133_deref_field_borrow.con
class: 7
class: 8
silent: yes
exit: 0b
area: Lower/Lower — the record's Fix (placeAddr in Lower.lean)
ruling: Violates: MEM-4@2
-->

**Status:** FIXED (branch `backend-108-116`, commit 35a39f6c)
**Found:** 2026-10-21, by the Codex round 2 review of the bug 116 fix (reported as a residual
`@vec_push` shape; the lost write behind it was measured while reproducing it).
**Severity:** silent wrong code: a write through the borrow is lost in the binary while `--interp`
performs it.

## Reproducer

`tests/regressions/bug133_deref_field_borrow_copies/repro.con`:

```concrete
struct P {
    v: i32,
    n: i32,
}

fn incr(r: &mut i32) {
    *r = *r + 1;
}

fn bump(p: &mut P) {
    incr(&mut (*p).n);
}

fn main() -> i32 {
    let mut p: P = P { v: 0, n: 0 };
    bump(&mut p);
    let P { v, n } = p;
    return v + n;
}
```

## Observed

On main (`9d17ccc5` and before) the binary exits 0: `p.n` is unchanged. `--interp` prints 1.
For a first-field `Vec`, `vec_push::<i32>(&mut (*p).v, x)` also fails LLVM validation with `use of
undefined value '@vec_push'`, the bug 116 symptom.

## Expected

`*r` as a place is the storage `r` points at, so `&mut (*p).n` is the address of `p.n` (exit 1),
exactly as `&mut p.n` is when `p` is a reference.

## Cause

`Concrete/IR/Lower.lean`, `placeAddr`: it handled array-element and field places and delegated
everything else to `addrOfLocal`, which has no case for a dereference. The field case's base
`(*p)` has type `P` (not a reference), so it fell back to `lowerExpr (*p)`, which LOADS the struct
into a value register; the field's address was then taken in the backend's spill of that value. A
write through the borrow updated the temporary. The `&*r` reborrow alone had a dedicated case in the
`.borrow` / `.borrowMut` lowering, so the defect showed only with a projection after the
dereference.

## Fix

`placeAddr` gained a `.deref inner _` case: when `inner` is a reference, raw pointer or `Heap`, the
place's address is `inner`'s value (cast to the requested pointer type), the same address the
reborrow uses. Permanent legs: `tests/programs/regress_133_deref_field_borrow.con` in `run_tests.sh`
(run_ok 42, the interpreter agrees: increments through `&mut (*p).a` (first field) and
`&mut (*p).n` must reach `p`; the pre-fix binary returns 39), and the `&mut (*p).v` push and set legs
of `tests/programs/regress_116_vec_push_first_field.con` (bug 116). The reproducer exits 1 natively
and under `--interp`.

## Structural cause

Storage and places decided per construct in `Lower` ([structural class](STRUCTURAL_CLASSES.md) 8,
with 078, 095, 102, 105, 110, 116): an address is computed by whichever case of `placeAddr`,
`addrOfLocal` or the borrow lowering happens to match, and a place shape none of them names falls
back to evaluating the place as a VALUE and addressing a copy, silently, instead of failing. The
interpreter, which follows places by path, did it right, so this is also a semantics implemented
twice (class 7). See [bug 116](116_vec_push_first_field_through_ref.md), which led to it.

## Structural owner

R-0501 (S10 Place MIR: every place, dereferences included, has one address computation and no
by-value fallback), with R-0491's differential harness (exit 2c) as the detector; the repair is
listed under exit 0b (silent wrong code).
