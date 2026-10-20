# Bug 112 — R-0484 R3 (every foreign binding declares its effects) is not enforced in nested submodules

**Status:** OPEN
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** a checker rule is silently skipped: an extern with no `with(...)` is accepted (and charged `Unsafe` at the call).

## Reproducer

`tests/regressions/bug112_submodule_extern_effects_unchecked/repro.con`:

```concrete
// Bug 112: R-0484 R3 (every foreign binding declares its effects, E0116) is
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

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
