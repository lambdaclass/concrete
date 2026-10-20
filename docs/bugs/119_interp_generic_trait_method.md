# Bug 119 — the interpreter cannot call a trait method on a type parameter

**Status:** OPEN
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** interpreter/native disagreement.

## Reproducer

`tests/regressions/bug119_interp_generic_trait_method/repro.con`:

```concrete
// Bug 119: the interpreter runs pre-monomorphization Core and cannot resolve
// a trait method called on a type parameter: "interp: undefined function
// 'U_v'"; the binary exits 9.
trait T {
    fn v(&self) -> Int;
}
struct Copy S { x: Int }
impl T for S {
    fn v(&self) -> Int { return self.x; }
}
fn g<U: T + Copy>(u: U) -> Int {
    return u.v();
}
fn main() -> Int {
    let s: S = S { x: 9 };
    return g(s);
}
```

## Observed

`fn g<U: T + Copy>(u: U) -> Int { return u.v(); }` called with `S`: the binary exits 9; `--interp` fails `interp: undefined function 'U_v'` (it runs pre-monomorphization Core, where the call names the type parameter).

## Expected

Dispatch on the runtime value's type in the interpreter (or interpret monomorphized Core).

## Where it showed up

examples/snippets/trait_dispatch.con, recorded `interp=fails:` in the manifest.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
