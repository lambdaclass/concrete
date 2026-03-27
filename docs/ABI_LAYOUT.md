# ABI and Layout

Status: stable reference

This document describes Concrete's current ABI/layout boundary and the compiler components that own it.

For current priorities, see [../ROADMAP.md](../ROADMAP.md). For the surrounding compiler pipeline, see [ARCHITECTURE.md](ARCHITECTURE.md).

## Scope

The ABI/layout subsystem is responsible for:

- type size and alignment
- field offsets
- enum payload layout
- LLVM type definition generation
- FFI-safety checks
- the low-level representation of `#[repr(C)]`, `#[repr(packed)]`, and `#[repr(align(N))]`

The implementation authority today is `Concrete/Layout.lean`.

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

`#[repr(C)]` gives a C-facing struct layout with explicit validation at extern boundaries.

Current rules:

- `#[repr(C)]` structs cannot have type parameters
- all `#[repr(C)]` struct fields must be FFI-safe
- `extern fn` parameters and return types must be FFI-safe

FFI-safe today includes:

- integer types
- float types
- `Bool`
- `Char`
- `()`
- raw pointers (`*mut T`, `*const T`)
- `#[repr(C)]` structs

### `#[repr(packed)]`

`#[repr(packed)]` suppresses normal alignment padding.

It exists for low-level layout control, but should be treated as an ABI-sensitive feature and used carefully.

### `#[repr(align(N))]`

`#[repr(align(N))]` raises alignment.

Current validation ensures alignment requests are coherent and use valid power-of-two values.

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

This is correct for the current implementation, but can waste space when small and large payload instantiations coexist in one program.

Future refinement:

- emit distinct LLVM types per concrete instantiation if the additional complexity becomes worthwhile

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
