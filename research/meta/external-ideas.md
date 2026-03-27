# External Ideas

**Status:** Open research direction
**Affects:** Tooling, stdlib design, backend design, language boundaries
**Date:** 2026-03-09

## Purpose

This note tracks ideas from other languages that may be useful for Concrete.

The point is not to copy trends. The point is to identify recent or durable ideas that:

- fit Concrete's philosophy
- improve auditability or reliability
- improve compiler structure or tooling
- avoid hidden machinery

## Strong External Ideas To Copy

### 1. Make hidden allocation even harder

**Source language:** Odin

Odin disallowed dynamic array and map literals that allocate implicitly unless a compatibility feature flag is enabled.

Concrete lesson:

- keep `Alloc` visible in signatures
- keep allocator binding visible at call sites
- resist literal or collection sugar that would allocate implicitly

Reference:

- [Odin 2025 Q1 Newsletter](https://odin-lang.org/news/newsletter-2025-q1/)

### 2. Sharper low-level stdlib design

**Source language:** Odin

Odin's `core:os` redesign emphasizes:

- explicit allocators where allocation occurs
- typed error values
- clearer resource-handle types

Concrete lesson:

- stdlib APIs should make allocation, ownership, and failure modes obvious from signatures and types

Reference:

- [Moving Towards a New `core:os`](https://odin-lang.org/news/moving-towards-a-new-core-os/)

### 3. Better inspectability and compiler/tooling graph awareness

**Source language:** Zig

Zig has continued investing in file watching, incremental compilation work, and a more explicit build/module graph model.

Concrete lesson:

- make compiler artifacts and module relationships explicit enough that tooling can inspect and reuse them
- use this to support file summaries, import graphs, and cacheable artifacts

Reference:

- [Zig 0.14.0 Release Notes](https://ziglang.org/download/0.14.0/release-notes.html)

### 4. Curated diagnostics

**Source language:** Rust

Rust added ways to suppress technically-correct but misleading suggestions.

Concrete lesson:

- diagnostics should be curated, not merely mechanically correct
- keep them phase-owned and high-signal

Reference:

- [Rust 1.85.0 announcement](https://blog.rust-lang.org/2025/02/20/Rust-1.85.0/)

### 5. Compiler-backed rename/find references

**Source language:** Gleam

Gleam improved project-wide rename and find-references by retaining richer reference information in compiler/language-server data.

Concrete lesson:

- compiler artifacts should support tooling, not just code generation
- module summaries and cross-file reference data should be reusable by editor tooling

Reference:

- [Gleam v1.10.0: Global rename and find references](https://gleam.run/news/global-rename-and-find-references/)

## Deferred But Worth Remembering

### Explicit ABI escape hatches

**Source language:** Rust

Rust stabilized naked functions for writing functions with no compiler-generated prologue/epilogue, using a tightly constrained unsafe assembly body.

Why it is interesting:

- could be useful later for runtimes, bootstrapping, context switching, interrupt/trap handlers, and low-level hooks

Why it is deferred:

- Concrete should not consider this until ABI/layout rules are sharper, the `Unsafe` boundary is tighter, and the allowed body shape can be stated precisely

References:

- [Rust 1.88.0 announcement](https://blog.rust-lang.org/2025/06/26/Rust-1.88.0/)
- [Stabilizing naked functions](https://blog.rust-lang.org/2025/07/03/stabilizing-naked-functions/)

## Main Takeaway

The strongest external ideas for Concrete are not new abstraction features. They are:

- stricter visibility around allocation
- sharper low-level stdlib design
- more explicit compiler/tooling artifacts
- curated diagnostics
- careful, narrow low-level escape hatches

These strengthen the philosophy Concrete already has rather than pulling it in a different direction.

## Related Notes

- [design-filters.md](design-filters.md)
- [candidate-ideas.md](candidate-ideas.md)
- [stdlib-design.md](stdlib-design.md)
