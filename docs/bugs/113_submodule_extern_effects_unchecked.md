# Bug 113 — R-0484 R3 (every foreign binding declares its effects) is not enforced in nested submodules

**Status:** FIXED 2026-10-21 (branch `symbols`)
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** a checker rule is silently skipped: an extern with no `with(...)` is accepted (and charged `Unsafe` at the call).

## Reproducer

`tests/regressions/bug113_submodule_extern_effects_unchecked/repro.con`:

```concrete
// Bug 113: R-0484 R3 (every foreign binding declares its effects, E0116) is
// enforced only for top-level modules. The same `extern fn` with no `with(...)`
// inside a nested submodule is accepted (and charged Unsafe at the call).
// Expected: E0116 on `abs`. Actual: compiles and exits 3.
mod t {
    mod ffi {
        pub extern fn abs(x: i32) -> i32;
    }
    pub fn main() with(Unsafe) -> i32 {
        return ffi::abs(0 - 3);
    }
}
```

## Observed

The same `extern fn abs(x: i32) -> i32;` is E0116 in a top-level module but accepted inside `mod t { mod ffi { ... } }`; the program compiles and exits 3. `resolveModule` checks `m.externFns` of the module it is given and the nested submodules are not visited for this check.

## Expected

Apply the extern-effects check to every module, nested ones included.

## Where it showed up

found while porting examples/snippets/direct_module_use.con.

## Fix

`resolveModule` now runs R3 through `externEffectsCheck`, which walks the module it is handed and
every nested submodule, so both halves of the rule (E0116 for an undeclared binding, E0117 for a
known effectful symbol declared `with()`) hold at any depth. The reproducer is refused with E0116;
permanent fixtures `tests/programs/error_113_submodule_extern_undeclared.con` (the reproducer,
E0116) and `error_113_submodule_extern_effectful_pure.con` (E0117, two levels down) run in
`run_tests.sh`. The open-bug reproducer row is dropped.

Walking the submodules exposed bug 112: in a project build std's modules are submodules, so
std.mem's `#[intrinsic]` declarations of `sizeof` and `alignof` reached R3 for the first time and
were refused. An `#[intrinsic]` declaration is implemented by the compiler and is not a foreign
binding (R-0484 R10), so the walk skips it; that fixes bug 112's reproducer as well.

## Structural cause

The R-0484 R3 rule is enforced by a walk over the module it is handed, and nested submodules are not visited for it: a rule whose enforcing site does not cover every module ([structural class](STRUCTURAL_CLASSES.md) 13; 077 shows the same nesting gap in lowering).

## Structural owner

R-0498 (S3, exit 2l: every rule has an enforcing site, with a nested-module case); proposed: run the extern-effects check from the same traversal that checks submodule bodies (H12).

## Containment while open

While open, `scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") required the recorded failure; the row was removed with the fix.
