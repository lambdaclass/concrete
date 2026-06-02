# ABI and FFI Maturity Statement

This document describes Concrete's current ABI stability, FFI safety model, platform assumptions, and what is intentionally left unstable.

For the exploratory design direction behind the source-facing `repr` surface, see [../research/language/layout-contract-surface.md](../research/language/layout-contract-surface.md).

## Stability Summary

| Area | Status | Stable? |
|------|--------|---------|
| FFI-safe scalar types (i8–i64, u8–u64, f32, f64, bool) | Implemented, tested | **Yes** — C-compatible by-value passing |
| `#[repr(C)]` struct **in-memory layout** | Implemented, tested | **Yes** — field order, padding, and alignment match C |
| `#[repr(packed)]` struct layout | Implemented, tested | **Yes** — no padding between fields |
| `#[repr(align(N)]` minimum alignment | Implemented, tested | **Yes** — power-of-two enforced |
| `extern fn` declarations (scalar params) | Implemented, tested | **Yes** — scalars passed by value, C-compatible |
| `extern fn` declarations (`#[repr(C)]` struct params) | Implemented, tested | **Yes** — `#[repr(C)]` structs passed by value per C ABI |
| `trusted extern fn` declarations | Implemented, tested | **Yes** — no capability required |
| FFI safety validation | Implemented, tested | **Yes** — CoreCheck enforces at compile time |
| Non-repr struct layout | Implemented | **No** — compiler may change field ordering or padding |
| Enum representation | Implemented | **No** — i32 tag + payload is an implementation detail |
| Pass-by-pointer convention | Implemented | **No** — which types are passed by pointer may change |
| Calling convention | LLVM default | **No** — no explicit calling convention annotation |
| String/Vec/HashMap internal layout | Implemented | **No** — opaque to FFI; sizes may change |
| Linker symbol naming | Implemented | **No** — mangling scheme is not specified |

## Platform Assumptions

Concrete currently targets **64-bit platforms only**. The following sizes are hardcoded in `Concrete/Layout.lean`:

| Type | Size (bytes) | Alignment (bytes) | Notes |
|------|-------------|-------------------|-------|
| `Int` / `Uint` | 8 | 8 | Always 64-bit |
| `i32` / `u32` | 4 | 4 | |
| `i16` / `u16` | 2 | 2 | |
| `i8` / `u8` | 1 | 1 | |
| `f64` | 8 | 8 | |
| `f32` | 4 | 4 | |
| `Bool` | 1 | 1 | LLVM `i1`, stored as `i8` in aggregates |
| `Char` | 1 | 1 | ASCII byte |
| `()` (unit) | 0 | 1 | Zero-sized |
| Pointers (`&T`, `*mut T`, etc.) | 8 | 8 | 64-bit pointers |
| `String` | 24 | 8 | `ptr + i64 len + i64 cap` |
| `Vec<T>` | 24 | 8 | `ptr + i64 len + i64 cap` |
| `HashMap<K,V>` | 40 | 8 | 5 × 8-byte fields |
| Enum tag | 4 | 4 | Always `i32` discriminant |

**No 32-bit support.** There is no conditional compilation, no target triple awareness, and no platform-dependent layout logic. All layout decisions are compile-time constants in Lean.

### Supported targets

| Target | Status | Notes |
|--------|--------|-------|
| x86_64-apple-darwin | **Primary** | Development and CI target |
| aarch64-apple-darwin | **Primary** | Apple Silicon, development target |
| x86_64-linux-gnu | Expected to work | Same ABI assumptions; not CI-tested |
| aarch64-linux-gnu | Expected to work | Same ABI assumptions; not CI-tested |
| 32-bit targets | **Not supported** | Pointer size hardcoded to 8 bytes |
| Windows | **Not tested** | May work with MSVC ABI differences in struct layout |

## FFI Safety Model

### What is FFI-safe

A type is FFI-safe (can appear in `extern fn` signatures and `#[repr(C)]` struct fields) if it is one of:

- Integer types: `i8`, `i16`, `i32`, `Int` (i64), `u8`, `u16`, `u32`, `Uint` (u64)
- Float types: `f32`, `f64`
- `Bool`, `Char`, `()` (unit)
- Raw pointers: `*mut T`, `*const T`
- `#[repr(C)]` structs (recursively: all fields must also be FFI-safe) — layout-compatible and passed by value in `extern fn` calls

### What is NOT FFI-safe

- `String`, `Vec<T>`, `HashMap<K,V>` — opaque managed types
- References: `&T`, `&mut T` — not raw pointers
- Enums — even with all-FFI-safe variant fields
- Non-`#[repr(C)]` structs
- Generic types (type parameters)
- `Heap<T>`, `HeapArray<T>` — managed heap types

### Validation

FFI safety is enforced at compile time by `CoreCheck`:

- `extern fn` parameters and return types must be FFI-safe
- `#[repr(C)]` struct fields must be FFI-safe
- `#[repr(C)]` structs cannot have type parameters (no generic repr(C))
- `#[repr(packed)]` and `#[repr(align(N))]` cannot be combined
- `#[repr(align(N))]` requires `N` to be a power of two
- `#[repr(C)]` cannot be applied to enums

Violations are compile errors — there is no way to bypass FFI safety without modifying the compiler.

### Capability requirements

- `extern fn` calls require the `Unsafe` capability
- `trusted extern fn` calls require no capability (the compiler author vouches for safety)
- Regular `fn` declared inside a `trusted impl` block inherits the trusted boundary

## Struct Layout Rules

### `#[repr(C)]` structs

Fields are laid out in declaration order with natural alignment padding, matching the C struct layout convention. Struct size is rounded up to the struct's alignment.

**Calling convention:** `#[repr(C)]` structs are passed **by value** in `extern fn` calls, matching the standard C ABI. The compiler detects extern fn calls and emits by-value parameters for `#[repr(C)]` struct arguments instead of the pointer indirection used for internal Concrete calls. This means `extern fn` declarations that take `#[repr(C)]` struct parameters interoperate correctly with C code that expects by-value structs. Non-repr(C) structs are still passed by pointer in all contexts (see Pass-by-Pointer Convention below).

Example: `#[repr(C)] struct Packet { tag: i8, payload: i32, flags: i16 }`
- `tag` at offset 0, size 1
- 3 bytes padding (align `payload` to 4)
- `payload` at offset 4, size 4
- `flags` at offset 8, size 2
- 2 bytes tail padding (align struct to 4)
- Total size: 12, alignment: 4

### `#[repr(packed)]` structs

No padding between fields. Alignment is 1. Fields at consecutive byte offsets.

Same example packed: size 7 (1 + 4 + 2), alignment 1.

### `#[repr(align(N))]` structs

Minimum alignment of `N` bytes (must be power of two). Combines with natural field layout — `align` increases but never decreases alignment.

### Non-repr structs

Compiler-controlled layout. Currently follows declaration order with natural alignment, but this is **not guaranteed** and may change.

## Enum Layout

Enums use a tagged-union representation:

```
{ i32 tag, [payload_bytes x i8] payload }
```

- Tag is always `i32` (4 bytes) at offset 0
- Payload starts at `alignUp(4, max_field_alignment)`
- Payload size is the maximum variant payload size
- Total size is `alignUp(tag_offset + payload_size, max(4, payload_align))`

This representation is **not stable** and should not be relied upon across FFI boundaries. Enums are not FFI-safe.

## Pass-by-Pointer Convention

Aggregate types (structs, enums, arrays, `String`, `Vec`, `HashMap`) are passed by pointer in **internal** Concrete function calls. The caller allocates stack space, stores the value, and passes a pointer. Scalar types (integers, floats, bool, char) are passed by value.

**Exception for extern fn:** `#[repr(C)]` struct arguments in `extern fn` calls are flattened to integer registers per the platform C ABI. On ARM64: structs ≤ 8 bytes are passed as one `i64`, structs 9-16 bytes as two `i64`s, structs > 16 bytes by pointer. Return values from extern fns follow the same flattening for ≤ 8 byte structs. This matches clang's calling convention and enables correct interoperability with C code.

Non-repr(C) structs in extern fn calls still use pointer passing (but non-repr(C) structs are rejected in extern fn signatures by the FFI type checker, so this case should not arise in practice).

The pass-by-pointer set for internal calls is **not stable** — which types are passed by pointer may change in future compiler versions.

## Known Limitations

These are not hypothetical — they are current implementation gaps that affect real FFI usage:

1. **No empirical cross-platform validation.** Layout is verified by Lean unit tests and by cross-target IR compilation (25 programs verified against x86_64 via clang). There are no runtime cross-platform tests — only same-platform Concrete↔C interop validation (sizeof/offsetof/by-value passing on ARM64).

2. **Struct return flattening limited to ≤ 8 bytes.** Extern fns returning repr(C) structs ≤ 8 bytes have the return value correctly flattened to i64. Larger struct returns still use pointer indirection, which may not match the C ABI for 9-16 byte structs.

## Desired Source-Level Contract Surface

Before Concrete freezes its first public stdlib and FFI story, the source-level layout contract should become smaller and sharper than the current implementation accident.

The intended direction is:

- explicit `repr` forms define the stable layout surface; unannotated structs stay compiler-controlled
- `#[repr(C)]`, `#[repr(packed)]`, and `#[repr(align(N))]` remain the only first-class guaranteed layout forms unless a later note justifies more
- transparent wrappers should only land if the wrapper/newtype story stays simple and obviously zero-cost
- enums remain intentionally out of the stable FFI layout surface
- reports and package/interface artifacts should say whether a type's layout is guaranteed or opaque, not force reviewers to infer it from context

The language should not promise a large Rust-style menu of representation tricks unless each one clears the same audit and proof bar as the rest of the surface.

## What We Intentionally Do Not Promise

1. **ABI compatibility across compiler versions.** Recompile everything when the compiler changes.
2. **Stable symbol names.** Function name mangling is not specified.
3. **Non-repr struct layout.** Only `#[repr(C)]` and `#[repr(packed)]` have guaranteed layout.
4. **Enum representation.** Tag size, payload offset, and discriminant values may change.
5. **32-bit support.** Not planned for the near term.
6. **Cross-language enum interop.** Enums are not FFI-safe.
7. **Stable pass-by-pointer set.** Which types are passed by pointer is an optimization decision.
8. **C-ABI by-value struct return > 8 bytes.** Struct return values > 8 bytes use pointer indirection. C callers expecting register-returned structs of 9-16 bytes should use a pointer-based wrapper.

## Layout Verification

The layout module (`Concrete/Layout.lean`) is the single source of truth for all type sizes, alignments, field offsets, pass-by-pointer decisions, and LLVM type mappings. Both `Lower.lean` and `EmitSSA.lean` delegate to `Layout` rather than maintaining their own layout logic.

Layout properties are verified by:

- **Compile-time computation:** All layout functions are pure Lean functions evaluated at compile time. Sizes and offsets are computed deterministically from type definitions.
- **FFI safety tests:** 17 test files in `lean_tests/` cover `repr(C)`, `repr(packed)`, `repr(align)`, extern functions, and error cases for all safety violations.
- **Report assertions:** `--report layout` produces human-readable layout information that is regression-tested.

### Layout model assumptions (cross-platform)

The following table shows the layout properties assumed by `Layout.lean`. These are **model-based** — they are verified by Lean unit tests against the layout helper functions, not by empirical cross-platform compilation or runtime checks. Since all layout decisions are compile-time constants with no platform-dependent branches, the same values apply to all 64-bit targets.

| Property | Value | Verified by |
|----------|-------|-------------|
| `sizeof(Int)` = 8 | Compile-time constant | `Concrete/PipelineTest.lean` layout test, `Layout.lean tySize .int = 8` |
| `sizeof(i32)` = 4 | Compile-time constant | `Concrete/PipelineTest.lean` layout test, `Layout.lean tySize .i32 = 4` |
| `sizeof(ptr)` = 8 | Compile-time constant | `Concrete/PipelineTest.lean` layout test, `Layout.lean tySize (.ref _) = 8` |
| `sizeof(String)` = 24 | Compile-time constant | `Concrete/PipelineTest.lean` builtin size test |
| `sizeof(Vec)` = 24 | Compile-time constant | `Concrete/PipelineTest.lean` builtin size test |
| `alignof(i64)` = 8 | Compile-time constant | `Layout.lean tyAlign .int = 8` |
| `alignof(i32)` = 4 | Compile-time constant | `Layout.lean tyAlign .i32 = 4` |
| Enum tag = i32 | Compile-time constant | `Layout.lean alignUp 4 payloadAlign` |
| repr(C) field order | Declaration order | `fieldOffset` iterates in declaration order; tested in `Concrete/PipelineTest.lean` |
| repr(packed) no padding | Consecutive offsets | `fieldOffset` sums sizes without alignment; tested in `Concrete/PipelineTest.lean` |
| Pass-by-ptr for structs | All named types | `isPassByPtr` returns true for named types; tested in `Concrete/PipelineTest.lean` |

**What this does not verify:** emitted LLVM IR signatures, actual runtime struct layout on target hardware, or foreign interop correctness. The tests confirm the layout model is internally consistent, not that it produces correct binaries on all targets. Empirical cross-target validation (compiling and running FFI tests on x86_64 and aarch64) is future work.
