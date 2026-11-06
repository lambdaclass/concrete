# Mixed-Size Struct Field Layout Bug

**Status:** Open (bug)

## Description

Copy structs with mixed-size fields (u8, u16, u32, u64, i32 in the same struct) produce incorrect field access offsets. Specifically, when a struct return value is read by the caller, the field offsets don't match the layout that was written.

## Reproduction

A struct like:

```concrete
struct Copy DecodeResult {
    version: u8,
    msg_type: u8,
    payload_len: u16,
    seq_num: u32,
    checksum: u16,
    payload_bytes: u64,
    error: i32,
}
```

When returned from a function and the `error` field is read by the caller, the value is garbage (uninitialized memory from wrong offset).

## Workaround

Use uniform field sizes (all i32 or all u64). The same struct with all-i32 fields works correctly.

## Likely Cause

The LLVM struct layout (which uses natural alignment and padding) doesn't match the GEP indices the compiler generates for field access. Either:

1. The compiler assumes packed layout but LLVM uses padded layout
2. The field index calculation doesn't account for alignment padding
3. The struct return ABI for mixed-size aggregates is handled differently

## Where to Look

- `Concrete/Layout.lean` — struct layout calculation
- `Concrete/Lower.lean` — struct field access lowering (GEP generation)
- `Concrete/EmitLLVM.lean` — struct type emission
