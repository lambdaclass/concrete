# Bug 116 — `vec_push` on the first field of a struct reached through a `&mut` parameter calls an undefined `@vec_push`

<!-- bug-ledger
summary: `vec_push(&mut p.v, x)` with `v` the first field of a `&mut P` called the undefined `@vec_push`: SSA GEP-zero elimination replaced `gep p, 0` by `p` with `p`'s type, so the call lost its `Vec` element type; a pointer use now keeps its own type; pinned by `regress_116_vec_push_first_field.con`
fixtures: regress_116_vec_push_first_field.con
class: 3
class: 8
silent: no
-->

**Status:** FIXED (branch `backend-108-116`, commit 595f0fc5, follow-ups 48d813da and 35a39f6c)
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

A zero-index GEP is the identity on the ADDRESS, not on the type. Every substitution pass
(`applyReplacements` -> `replaceRegInSVal`) now keeps the type a use was written with when a
pointer register replaces a pointer register. The first draft limited that to GEP-zero
elimination; the Codex review showed that identity-cast elimination, which runs later, undid it:
`&mut (p as &mut P).v` became `gep q, 0` -> `q` typed `&mut Vec<i32>` -> `p` typed `&mut P`, and
`@vec_push` came back (the cast's operand and result types are equal; its uses' types need not
be). A GEP can also be based on an aggregate VALUE register (the backend spills it to address
it); there the base keeps its own type, which an earlier draft got wrong (elf_header then failed
with `'%t19' defined with type '%struct.FixedSinkState' but expected 'ptr'`).

The Codex round 2 review found the same symptom through an explicit dereference,
`vec_push::<i32>(&mut (*p).v, x)`. Its cause is a separate defect with a worse consequence, filed as
[bug 133](133_deref_field_borrow_copies.md): `Lower.placeAddr` had no dereference case, so
`&mut (*p).f` borrowed the field of a COPY of `*p` (a write through it was silently lost). Its fix
(`35a39f6c`) also repairs this shape.
Permanent leg: `tests/programs/regress_116_vec_push_first_field.con` in `run_tests.sh` (run_ok 42:
pushes to a first-field `Vec<i32>` through `&mut P` directly and through an identity-cast alias,
`vec_set` on the first field (specialized from its first argument as well), push and set through
`&mut (*p).v`, a second-field `Vec<u8>`, each value read back); bug 133's lost-write leg is
`tests/programs/regress_133_deref_field_borrow.con`.
The reproducer exits 1 natively and under `--interp`, and the regression returns 42 on both (bug
120, which kept the interpreter from borrowing a field through a reference, was fixed by batch 2).

## Structural cause

Intrinsic specialization (`vec_push` -> `vec_push_4`) depends on the shape of the lowered argument: when the field projection folds to the base pointer (first field), the element type is not recovered and the generic name is emitted. Storage and places decided ad hoc in Lower ([structural class](STRUCTURAL_CLASSES.md) 8) combined with dispatch by synthesized names (class 3). Confirmed on the fix, with one refinement: the fold is in SSA cleanup, not Lower. An SSA register's type lives on each use (`SVal.reg name ty`), not on its definition, so a substitution pass can silently change the type a use sees; the specialization then reads that operand type instead of the checked call's type argument (`vec_push::<i32>`), which R-0510 removes.

## Structural owner

R-0501 (S10 Place MIR) with R-0510 (S8a checked specialization: an intrinsic's instance is chosen from the checked type, not the lowered operand).

