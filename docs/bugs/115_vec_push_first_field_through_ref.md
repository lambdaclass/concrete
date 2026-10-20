# Bug 115 — `vec_push` on the first field of a struct reached through a `&mut` parameter calls an undefined `@vec_push`

**Status:** OPEN
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** compile failure at LLVM validation.

## Reproducer

`tests/regressions/bug115_vec_push_first_field_through_ref/repro.con`:

```concrete
// Bug 115: vec_push on `&mut p.v`, where p is a `&mut P` parameter and v is
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

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
