# ABI/Layout Reports as First-Class Artifacts

Status: research

## Problem

Systems programmers working with FFI, embedded targets, network protocols, or shared memory need precise knowledge of struct layout: field offsets, alignment, padding, total size. Most languages force you to guess or use external tools.

Concrete already computes this in Layout.lean and partially exposes it via `--report layout`. The gap is making it complete and explicit enough to be a first-class audit artifact.

## What Already Exists

- `Layout.lean` (352 lines): `tySize`, `tyAlign`, `fieldOffset`, `alignUp`
- `--report layout` in Report.lean (lines 463-482): shows struct size, alignment, per-field offset/size/align
- Packed struct support (`#[repr(packed)]`)
- Repr(C) struct support with ABI-correct calling convention

### Current report output

```
struct MyStruct (size: 16, align: 8)
    offset 0   size 4   align 4   x: i32
    offset 8   size 8   align 8   y: i64
```

### What's missing

- **Padding visualization**: the 4 bytes of padding between x and y are invisible
- **Enum layout details**: tag offset, payload offset, variant sizes
- **Array stride**: element size and alignment for array indexing
- **ABI compatibility notes**: whether a struct matches C layout
- **Cross-target comparison**: same struct on different architectures

## Proposed Enhancements

### 1. Explicit padding (EASY — 1 day)

```
struct MyStruct (size: 16, align: 8)
    offset 0   size 4   align 4   x: i32
    offset 4   size 4              [padding]
    offset 8   size 8   align 8   y: i64
```

Change: ~20 lines in Report.lean. Compute `next_offset - current_offset - current_size` between consecutive fields.

### 2. Enum layout details (EASY — 1-2 days)

```
enum Option<i32> (size: 8, align: 4)
    tag: offset 0, size 4
    payload: offset 4
    variant Some: payload size 4 (i32)
    variant None: payload size 0
```

Change: ~50 lines in Report.lean. Layout.lean already computes tag/payload offsets.

### 3. Struct ABI compatibility flag (EASY — 1 day)

```
struct Point (size: 8, align: 4, repr: C-compatible)
struct MyStruct (size: 24, align: 8, repr: Concrete-native)
```

Compare Concrete layout to C layout rules. Flag structs that happen to be C-compatible even without `#[repr(C)]`.

### 4. Machine-readable output (MODERATE — 2-3 days)

```json
{"struct": "Point", "size": 8, "align": 4, "fields": [
  {"name": "x", "offset": 0, "size": 4, "type": "i32"},
  {"name": "y", "offset": 4, "size": 4, "type": "i32"}
]}
```

Enables tooling integration: IDEs, debuggers, FFI generators, protocol validators.

## Difficulty Assessment

| Enhancement | Effort | Value |
|-------------|--------|-------|
| Padding visualization | 1 day | High — makes audit real |
| Enum layout details | 1-2 days | High — enums are the complex case |
| ABI compatibility flag | 1 day | Medium — useful for FFI work |
| Machine-readable output | 2-3 days | Medium — enables tooling |
| Cross-target comparison | 1 week+ | Low — needs cross-compilation support |
| **Total (items 1-3)** | **3-4 days** | |

All of this is pure report formatting — no changes to Layout.lean's computation logic needed.

## Interaction with Other Features

- **FFI**: Explicit layout reports make FFI debugging trivial. "Why does my C struct not match?" is answered by comparing layouts.
- **Proof story**: Layout properties (size, alignment, field offsets) are provable. `--report layout` output could eventually be backed by Lean proofs.
- **High-integrity profile**: Layout auditing is required for safety-critical embedded code (DO-178C, IEC 61508).
- **Package model**: Layout reports per package would show ABI stability across versions.
