# Bug 119 — the interpreter cannot call a trait method on a type parameter

**Status:** FIXED 2026-10-20 (batch 2)
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

## Fix

Batch 2 (bug 052 Codex round 2, "destroy dispatch carries the trait; interp uses the impl table"): the interpreter dispatches a `T$m` call on a
type parameter through the trait-impl table on the runtime receiver's type (`evalTraitParamCall`,
`traitImplKey`), as Mono's per-instantiation rewrite does. The reproducer now agrees: `--interp`
and the binary both give 9. Permanent leg: `run_tests.sh` compares `--interp` with the native exit
on `tests/regressions/bug119_interp_generic_trait_method/repro.con`; `examples/snippets/trait_dispatch.con`
is promoted to `interp=yes` in the example manifest.

## Structural cause

The interpreter executes pre-monomorphization Core, where a trait method called on a type parameter is spelled `U_v`; only Mono resolves it. Semantics implemented twice, at different pipeline stages ([structural class](STRUCTURAL_CLASSES.md) 7).

## Structural owner

R-0496 (S7: the interpreter runs the same Core the backend lowers) with R-0510 (S8a checked specialization).

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
