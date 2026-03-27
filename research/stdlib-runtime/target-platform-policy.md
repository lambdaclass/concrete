# Target And Platform Policy

**Status:** Open research direction
**Affects:** Runtime model, ABI policy, backend support, release policy, testing strategy
**Date:** 2026-03-15

## Purpose

This note defines the questions Concrete should answer before claiming meaningful target or platform support.

The main goal is to replace implicit "LLVM probably handles it" assumptions with an explicit support policy.

## Why This Matters

Concrete already makes strong claims about:

- layout
- FFI
- runtime behavior
- auditability

Those claims are incomplete if the project does not also say:

- which targets are supported
- which targets are only experimental
- which ABI assumptions are baked in
- what CI or verification depth each target tier receives

## The Core Policy Questions

Concrete eventually needs explicit answers for:

1. Which architectures are supported?
2. Which OS/runtime environments are supported?
3. Which target triples are first-class, tier-2, or experimental?
4. Which ABI/layout assumptions are considered stable per tier?
5. What test depth is required before a target is called supported?
6. What parts of the compiler/runtime model are target-dependent?

## Recommended Support Tiers

Concrete should likely use a simple tier model:

### Tier 1

Fully supported targets:

- expected to pass CI consistently
- ABI/layout assumptions documented
- runtime model documented
- release-blocking when broken

### Tier 2

Intended-but-less-proven targets:

- builds expected to work
- narrower CI coverage
- known gaps tolerated if documented

### Experimental

Exploratory targets:

- may compile partially
- no compatibility promise
- useful for research, not for claims of support

## Likely Initial Direction

The most realistic initial policy is:

- 64-bit only
- hosted POSIX-like environments first
- narrow architecture set first, such as `x86_64` and `aarch64`
- freestanding deferred until the hosted/runtime contract is boring

That would align with the current implementation reality much better than pretending all LLVM targets are equally meaningful.

## What "Supported" Should Mean

A target should not be called supported just because LLVM can emit code for it.

Supported should mean:

- layout assumptions are documented
- ABI behavior is tested at the promised depth
- runtime boundary assumptions are documented
- core stdlib/runtime-facing surfaces behave as documented
- relevant reports remain truthful on that target

## Verification Depth

Target support should be tied to verification depth, for example:

- model-based layout tests
- compile/link/run coverage
- FFI ABI checks
- selected integration programs
- report consistency checks

The exact matrix can vary by support tier, but the project should say what each tier earns.

## Relationship To Other Research

- [no-std-freestanding.md](no-std-freestanding.md)
- [complete-language-system.md](complete-language-system.md)
- [mlir-backend-shape.md](mlir-backend-shape.md)

## Working Conclusion

Concrete should adopt an explicit target/platform policy early in Phase E rather than letting platform claims emerge indirectly from LLVM support or ad hoc testing.
