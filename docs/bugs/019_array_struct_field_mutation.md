# Bug 019: Array Extracted From Struct Field Cannot Be Mutated

**Status:** Fixed
**Discovered:** 2026-03-20
**Discovered in:** `tests/programs/bug_array_struct_field_mutation.con`

## Description

When a fixed-size array is extracted from a struct field into a mutable local, mutations to the local array are lost. The array appears to alias or not be properly allocated for mutation.

```concrete
struct Buf {
    data: [i32; 4],
    len: u64,
}

fn test_from_struct(b: Buf) -> i32 {
    let mut d: [i32; 4] = b.data;
    d[0] = 99;
    return d[0];  // returns 0 instead of 99
}
```

Direct array init works (`let mut d: [i32; 4] = [0; 4]; d[0] = 99;` returns 99), but extracting from a struct field and mutating doesn't.

## Root Cause

The lowering pass was not generating a proper alloca+memcpy for array-typed struct field extractions, so the mutation wrote to a temporary rather than the local variable's storage.

## Fix

Fixed in the SSA lowering to ensure struct field extractions of array type produce a fresh alloca with a copy of the field data.

## Regression Test

`tests/programs/bug_array_struct_field_mutation.con`
