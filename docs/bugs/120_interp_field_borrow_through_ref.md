# Bug 120 — the interpreter cannot borrow a field through a reference parameter

**Status:** OPEN
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** interpreter/native disagreement; INTERPRETER_TRUST.md lists path borrows across calls as supported.

## Reproducer

`tests/regressions/bug120_interp_field_borrow_through_ref/repro.con`:

```concrete
// Bug 120: the interpreter cannot borrow a field through a reference
// parameter (`&mut p.v` where p: &mut P): "interp: ref step .field on
// non-struct"; the binary exits 1. (`v` is the second field here so the
// native side is not also hit by bug 115.)
struct P {
    n: Int,
    v: Vec<i32>,
}
fn add(p: &mut P, x: i32) with(Alloc) {
    vec_push::<i32>(&mut p.v, x);
}
fn main() with(Alloc) -> Int {
    let mut p: P = P { n: 0, v: vec_new::<i32>() };
    add(&mut p, 5);
    let len: Int = vec_len::<i32>(&mut p.v) as Int;
    let P { n, v } = p;
    vec_free::<i32>(v);
    return len;
}
```

## Observed

`fn add(p: &mut P, x: i32) { vec_push::<i32>(&mut p.v, x); }`: the binary exits 1; `--interp` fails `interp: ref step .field on non-struct`.

## Expected

Follow the reference before applying the field step.

## Where it showed up

examples/json (`StringPool.store`), recorded `interp=fails:` in the manifest.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
