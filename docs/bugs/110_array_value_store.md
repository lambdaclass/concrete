# Bug 110 — an array value is stored as an aggregate where the SSA value is a pointer

**Status:** OPEN
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** compile failure at LLVM validation.

## Reproducer

`tests/regressions/bug110_array_value_store/repro.con` (the cast shape is `repro_cast.con` beside it):

```concrete
// Bug 110 (witness 1): assigning an array value to an array element stores the
// SSA pointer as if it were the aggregate. Expected: exit 3 (interp prints 3).
// Actual: "'%tN' defined with type 'ptr' but expected '[2 x i64]'".
fn main() -> i32 {
    let mut a: [[i32; 2]; 2] = [[1, 2], [9, 9]];
    a[1] = [3, 4];
    return a[1][0];
}
```

## Observed

Two shapes: assigning an array value to an array element (`a[1] = [3, 4];`) and using an array local cast to a raw pointer (`x as *mut u8` passed to `puts`). Both emit `store [N x T] %tK, ptr ...` where `%tK` is a `ptr`: `'%tK' defined with type 'ptr' but expected '[2 x i64]'` (resp. `'[3 x i8]'`). The interpreter returns 3 for the first shape. The two shapes share the symptom; whether they share a root cause in Lower is not yet established.

## Expected

Store the aggregate (load it first, or memcpy from the pointer).

## Where it showed up

examples/snippets/arrays.con and examples/snippets/hello_world_array.con, manifest `known-bug 110`.

## Structural cause

Lower decides ad hoc whether an array value is an SSA aggregate or a pointer to storage, and the store it emits assumes the other one. This is storage and places decided per construct in Lower ([structural class](STRUCTURAL_CLASSES.md) 8, with 078, 095, 102, 105).

## Structural owner

R-0501 (S10 Place MIR: every aggregate has one storage representation and stores go through places), with R-0504 (S12 validators).

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
