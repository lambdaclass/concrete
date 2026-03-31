+++
title = "Architecture Truth"
+++

# Current Architecture Truth

This page is the short, current answer to "what is actually true right now?"

Concrete is no longer an experimental AST-to-backend compiler. The active pipeline is:

```text
Source -> Parse -> Resolve -> Check -> Elab -> CoreCanonicalize -> CoreCheck -> ValidatedCore -> Mono -> Lower -> SSAVerify -> SSACleanup -> EmitSSA -> LLVM toolchain
```

What that means:

- `Core` is the semantic center of the compiler
- `CoreCheck` is the post-elaboration semantic authority
- `ValidatedCore` is the proof-facing semantic boundary
- `SSA` is backend territory, not where language meaning should be re-decided
- the LLVM emission path is now structured end to end

## What Is Shipped

- a real `Core -> SSA -> LLVM` compiler pipeline
- a real stdlib with collections, text, process, fs, net, and test coverage
- module-targeted stdlib testing
- diagnostics with snippets and carets
- a formatter baseline
- audit-style reports for capabilities, trust/unsafe boundaries, allocation, layout, interface, and monomorphization

## What Is Active

Phase D is the active frontier.

That means the main work is:

- smarter testing infrastructure built on compiler artifacts and dependency information
- stronger SSA/backend contracts
- reusable pipeline artifacts
- the first real Lean 4 proof workflow for selected Concrete functions

## What Is Planned

Concrete is explicitly trying to become:

- a low-level language optimized for auditability and explicit trust boundaries
- a compiler whose semantic boundary is clean enough for Lean 4 proofs of selected Concrete functions
- a language that can later grow a high-integrity profile for critical code

## The Short Version

Concrete already has the shape of a serious compiler.

The remaining work is no longer "make it basically function." It is:

- make the backend contract cleaner
- make testing smarter and faster
- make artifacts reusable
- make Lean-side proofs of selected Concrete code real
