# Bug 116 — `vec_push` on the first field of a struct reached through a `&mut` parameter calls an undefined `@vec_push`

**Status:** FIXED (branch `backend-108-116`)
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** compile failure at LLVM validation.

## Reproducer

`tests/regressions/bug116_vec_push_first_field_through_ref/repro.con`:

```concrete
// Bug 116: vec_push on `&mut p.v`, where p is a `&mut P` parameter and v is
// P's FIRST field, lowers to a call to the generic, undefined `@vec_push`
// (llvm-as: "use of undefined value '@vec_push'"). Moving `v` to a later
// field compiles. Expected: exit 1.
struct P {
    v: Vec<i32>,
}
fn add(p: &mut P, x: i32) with(Alloc) {
    vec_push::<i32>(&mut p.v, x);
}
fn main() with(Alloc) -> Int {
    let mut p: P = P { v: vec_new::<i32>() };
    add(&mut p, 5);
    let n: Int = vec_len::<i32>(&mut p.v) as Int;
    let P { v } = p;
    vec_free::<i32>(v);
    return n;
}
```

## Observed

`vec_push::<i32>(&mut p.v, x)` with `p: &mut P` and `v` the FIRST field of `P` lowers to `call void @vec_push(ptr %p, i32 %x)` (the generic, unspecialized name): `use of undefined value '@vec_push'`. With `v` as a later field it compiles (the field projection is then a GEP, and the intrinsic is specialized).

## Expected

Specialize the intrinsic call regardless of whether the field projection folds to the base pointer.

## Where it showed up

found while porting examples/json (its pool struct puts the Vec second).

## Cause

Not `Lower`: `placeAddr` lowers `&mut p.v` to `gep p, 0` typed `&mut Vec<i32>`. The SSA cleanup pass
`eliminateGepZero` (`Concrete/IR/SSACleanup.lean`) then substituted every use of that register with
the base SVal `p` **including its type** `&mut P`. `EmitSSA.resolveVecCall` reads the element type
from the first argument's type, found no `Vec<_>`, and fell through to the generic call path with
the unspecialized name. Any later field is a nonzero GEP, which the pass leaves alone.

## Fix

A zero-index GEP is the identity on the ADDRESS, not on the type: `applyReplacements` gained
`keepUseTy`, and GEP-zero elimination substitutes the base register under the type each use was
written with, when both are reference/pointer types. A GEP can also be based on an aggregate VALUE
register (the backend spills it to address it); there the base keeps its own type, which the first
draft of this fix got wrong (elf_header then failed with `'%t19' defined with type
'%struct.FixedSinkState' but expected 'ptr'`). The other replacement passes cannot change a use's
type: identity-cast elimination and store-to-load forwarding substitute only between equal types,
and redundant-load elimination keys on the loaded type.
Permanent leg: `tests/programs/regress_116_vec_push_first_field.con` in `run_tests.sh` (run_ok 42:
pushes to a first-field `Vec<i32>` and a second-field `Vec<u8>` through `&mut P`, reads both back).
The reproducer exits 1 natively; the interpreter cannot run it (bug 120, field borrow through a
reference), so the differential half waits on 120.

## Structural cause

Intrinsic specialization (`vec_push` -> `vec_push_4`) depends on the shape of the lowered argument: when the field projection folds to the base pointer (first field), the element type is not recovered and the generic name is emitted. Storage and places decided ad hoc in Lower ([structural class](STRUCTURAL_CLASSES.md) 8) combined with dispatch by synthesized names (class 3). Confirmed on the fix, with one refinement: the fold is in SSA cleanup, not Lower. An SSA register's type lives on each use (`SVal.reg name ty`), not on its definition, so a substitution pass can silently change the type a use sees; the specialization then reads that operand type instead of the checked call's type argument (`vec_push::<i32>`), which R-0510 removes.

## Structural owner

R-0501 (S10 Place MIR) with R-0510 (S8a checked specialization: an intrinsic's instance is chosen from the checked type, not the lowered operand).

