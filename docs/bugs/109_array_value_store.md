# Bug 109 — an array value is stored as an aggregate where the SSA value is a pointer

**Status:** OPEN
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** compile failure at LLVM validation.

## Reproducer

`tests/regressions/bug109_array_value_store/repro.con` (the cast shape is `repro_cast.con` beside it):

```concrete
// Bug 109 (witness 1): assigning an array value to an array element stores the
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

examples/snippets/arrays.con and examples/snippets/hello_world_array.con, manifest `known-bug 109`.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
