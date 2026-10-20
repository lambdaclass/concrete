# Bug 113 — an intrinsic declared in a nested submodule and called by path lowers to a C call

**Status:** OPEN
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** link failure (a valid program is rejected after checking passed).

## Reproducer

`tests/regressions/bug113_submodule_intrinsic_links_c_symbol/repro.con`:

```concrete
// Bug 113: a `#[intrinsic = "sizeof"]` declared in a nested submodule and
// called by path (`intrinsics::sizeof`) is not intercepted: it lowers to a C
// call and the link fails ("_sizeof" undefined). Expected: exit 4.
mod t {
    mod intrinsics {
        #[intrinsic = "sizeof"]
        pub trusted fn sizeof<T>() -> u64;
    }
    pub fn main() -> i32 {
        return intrinsics::sizeof::<i32>() as i32;
    }
}
```

## Observed

`intrinsics::sizeof::<i32>()` against `mod intrinsics { #[intrinsic = "sizeof"] pub trusted fn sizeof<T>() -> u64; }` is not intercepted; the call is emitted as a call to the C symbol `sizeof`, and the link fails (`Undefined symbols ... "_sizeof"` on macOS, `undefined reference to sizeof` on Linux). The top-level form is blocked by bug 111.

## Expected

Resolve the intrinsic by its declaration, not by the spelling of the call path.

## Where it showed up

examples/snippets/direct_module_use.con, manifest `known-bug 113`.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
