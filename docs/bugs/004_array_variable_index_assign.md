# Bug 004: Array Variable-Index Assignment Produces Wrong Code

**Status:** Fixed
**Discovered:** 2026-03-15
**Regression test:** `lean_tests/bug_array_var_index_assign.con`

## Symptom

`arr[i] = val` where `i` is a runtime variable (not a literal) silently writes to the wrong memory location. The assignment appears to succeed but the value is not stored at the correct index. Literal indices (`arr[1] = val`) work correctly.

## Reproduction

```
fn main() -> i32 {
    let arr: [i32; 3] = [0, 0, 0];
    let i: i32 = 1;
    arr[i] = 99;
    return arr[1];  // returns 0, should return 99
}
```

## Root Cause

Two issues in `Concrete/Lower.lean` line 1460 (`arrayIndexAssign` case):

1. **GEP base type is wrong**: The GEP instruction receives `value.ty` which resolves to the internal runtime type (`i64`) rather than the array element type (`i32`). This causes the pointer offset calculation to use 8-byte strides over a 4-byte element array.

2. **Store width is wrong**: The store instruction writes an `i64` value into an `i32` slot, overwriting adjacent memory.

Generated LLVM IR for the reproduction case:
```llvm
  ; Variable-index assign (WRONG):
  %t4 = getelementptr i64, ptr %t0, i32 1   ; steps 8 bytes, should step 4
  store i64 99, ptr %t4                       ; writes 8 bytes, should write 4

  ; Compare with literal-index init (CORRECT):
  %t2 = getelementptr i32, ptr %t0, i64 1   ; steps 4 bytes ✓
  store i32 0, ptr %t2                        ; writes 4 bytes ✓
```

The literal-index path (array initialization in Lower.lean) correctly uses the element type for GEP and store. The variable-index assignment path uses the Concrete-internal type representation instead.

## Impact

- Any `arr[i] = val` with a variable index silently produces corrupt data
- Loop-based array filling is broken
- Only affects assignment; `arr[i]` reads with variable indices appear correct (separate code path at line 857)
- Literal-index assignments work correctly because they use a different code path

## Fix Direction

In `Lower.lean` at the `arrayIndexAssign` case (line 1455-1461):
- Extract the element type from the array type of `arr`
- Use the element type (not `value.ty`) for both the GEP instruction and store width
- May need to truncate/extend the value to match the element type if it differs from the internal representation
