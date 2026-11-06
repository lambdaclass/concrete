# Status

Concrete is real and usable in-tree, but it is still evolving quickly.

This page is the blunt version of the project state.

## Works Today

- full compiler pipeline through Core and SSA
- explicit capabilities, ownership, borrows, `defer`, traits, FFI, and layout attributes
- stdlib foundation with collections, systems modules, formatting, parsing, and reports
- built-in test runner and module-targeted stdlib testing
- audit/report modes for authority, trust, layout, monomorphization, and allocation
- formatter baseline and structured diagnostics

## Active Frontier

The current center of gravity is **Phase D**:

- structured LLVM backend
- stronger SSA/backend contract
- reusable pipeline artifacts
- formalization over validated Core
- the first real Lean 4 proof workflow for selected Concrete functions

## Not Done Yet

- structured non-string backend
- full runtime/execution-model definition
- high-integrity profile/subset
- package/dependency model
- maintained editor/LSP-quality workflow
- operational maturity and compatibility policy

## Proof Story: Real But Early

The project is serious about proofs, but it is important to separate what exists from what is planned.

What exists architecturally:

- compiler implemented in Lean 4
- validated Core after `CoreCheck` as the proof boundary
- explicit proof-oriented roadmap and research direction

What does not exist yet:

- actual Lean 4 proofs of selected Concrete functions
- whole-compiler formal verification

Those are active future goals, not shipped claims.

## If You Want The Current Truth

Read:

- [Repository README](../../../README.md)
- [Roadmap](../../../ROADMAP.md)
- [Changelog](../../../CHANGELOG.md)
- [Testing Concrete](./testing.md)
