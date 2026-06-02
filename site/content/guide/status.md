+++
title = "Status"
+++

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

What exists today:

- compiler implemented in Lean 4
- validated Core after `CoreCheck` as the proof boundary
- **kernel-checked Lean 4 proofs on selected functions** across the graduated
  flagships (plus the in-progress `hmac_sha256`), tied to source by body
  fingerprints with a spec-drift gate
- a documented, additive provable subset (`ProvableV1`, R-1…R-28) covering
  integer/bool, calls, lets, structs/enums, pattern matching, array
  read/update, bounded loops (incl. array-element writes), casts, and
  width-tagged `mod`/`div`/`bitand`/`bitor`/`bitxor`/`shl`/`shr`/wrapping-`add`
- a **reusable proof layer** (evaluator fuel monotonicity + bounded counter-loop
  induction) that makes loop/array proofs systematic, and kernel-checked
  `bv_decide` automation now backing a committed proof
- the **first refinement proofs against an independent spec**: a `BitVec`-valued
  SHA-256 spec, proofs that the extracted Boolean round functions (`ch`, `maj`)
  compute exactly that spec for all inputs (word-level core discharged by
  `bv_decide`, kernel-checked), and the **first loop refinement** —
  `block_to_words_refines_spec` proves the 16-iteration big-endian packing loop
  refines the spec for all 64 input bytes, via the bounded-counter-loop
  induction lemma

What does not exist yet (planned, not shipped):

- **source-level contracts** (`requires`/`ensures`/`invariant`/`ghost`) and
  automatic verification-condition generation — see the design in the docs
- **full refinement of a real primitive**: the Boolean round functions refine
  their spec today, but the rotations/sigmas, the message schedule, compression,
  and the full `hash`/`hmac` composition (HMAC bar #2) are not yet proved
- whole-compiler formal verification

Those last three are active goals, not shipped claims. The discipline is to
separate what is proved from what is planned — see the proof-ladder and
contracts docs in the repository.

## If You Want The Current Truth

Read:

- [Repository README](https://github.com/unbalancedparentheses/concrete2/blob/main/README.md)
- [Roadmap](https://github.com/unbalancedparentheses/concrete2/blob/main/ROADMAP.md)
- [Changelog](https://github.com/unbalancedparentheses/concrete2/blob/main/CHANGELOG.md)
- [Testing Concrete](@/guide/testing.md)
