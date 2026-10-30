# ABI and Layout

Status: stable reference

This document describes Concrete's current ABI/layout boundary and the compiler components that own it.

For current priorities, see [../ROADMAP.md](../../ROADMAP.md). For the surrounding compiler pipeline, see [ARCHITECTURE.md](../compiler/ARCHITECTURE.md).

## Scope

The ABI/layout subsystem is responsible for:

- type size and alignment
- field offsets
- enum payload layout
- LLVM type definition generation
- FFI-safety checks
- the low-level representation of `#[repr(C)]`, `#[repr(packed)]`, and `#[repr(align(N))]`

The implementation authority today is `Concrete/Check/Layout.lean`.

## Source of Truth

`Layout.lean` is the shared authority for:

- `tySize`
- `tyAlign`
- field-offset computation
- enum payload offsets and layout
- `structTypeDef`
- `enumTypeDefs`
- `builtinTypeDefs`
- `isFFISafe`

Both `CoreCheck` and `EmitSSA` delegate to `Layout` rather than carrying separate copies of layout or FFI logic.

## Representation Features

### `#[repr(C)]`

The rules are the specification's: which types are FFI-safe and the `#[repr(C)]` restrictions
([FFI-3](../spec/ffi.md#FFI-3), [FFI-4](../spec/ffi.md#FFI-4)), what `#[repr(C)]` and
`#[repr(packed)]` promise ([FFI-8](../spec/ffi.md#FFI-8)), the four layout forms
([FFI-10](../spec/ffi.md#FFI-10)) and the limits of `#[repr(align(N))]` (FFI-4). `Layout.lean`
implements them; `#[repr(packed)]` is ABI-sensitive and its misaligned fields are
[OPEN-111](../spec/decisions.md#OPEN-111).

## Enum Layout

Concrete now centralizes enum layout in `Layout.lean`, including:

- tag storage
- payload offset
- variant field layout
- LLVM type-definition generation

This includes both user enums and the builtin generic enums used in the runtime.

## Builtin Generic Enums

Builtin `Option` and `Result` currently use a whole-program monomorphic ABI choice during LLVM emission:

- the compiler scans all concrete instantiations used in the program
- it emits one `%enum.Option` and one `%enum.Result`
- those LLVM types are sized for the largest payload seen across all instantiations

This disagrees with the per-instantiation size `Layout.tySize` computes, so aggregate copies can
move or overrun fields ([bug 122](../bugs/122_builtin_enum_canonical_layout.md)); the fix lays each
instantiation out on its own, like every generic enum. The two enums' definitions are
[TYPE-23](../spec/types.md#TYPE-23), and their layout is unspecified like any enum's
([FFI-7](../spec/ffi.md#FFI-7)).

## Extern ABI Boundary

Extern-facing decisions should remain centralized here rather than spread across multiple passes.

This includes:

- argument/return low-level representation
- pass-by-value versus pass-by-pointer policy
- bool/char ABI details
- FFI-safe checks

One explicit example:

- `Bool` lowers to LLVM `i1` internally
- in extern signatures it is promoted/truncated for C ABI compatibility

## Current Status

Done enough for the current architecture phase:

- layout logic is centralized
- checking and codegen share the same source of truth
- aligned struct and enum layout bugs were fixed
- builtin `Option` / `Result` payload layout is no longer hardcoded to `i64`

Still useful future refinement:

- clearer calling-convention documentation
- more edge-case layout tests
- possible per-instantiation builtin generic enum LLVM types
- eventual dedicated FFI reference doc if the extern surface grows substantially
