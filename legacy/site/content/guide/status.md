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
- source-level contracts (`requires`/`ensures`/`invariant`) and
  verification-condition generation — the next step now that the Lean proof
  workflow has shipped (refinement proofs + spec-drift gate, demonstrated end to
  end on HMAC-SHA256) and its reusable machinery is factored into `ProofKit`

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
- **kernel-checked Lean 4 proofs on selected functions** across the five
  graduated flagships, tied to source by body fingerprints with a spec-drift
  gate that revokes a `proved` claim when the body changes
- a documented, additive provable subset (`ProvableV1`, R-1…R-28) covering
  integer/bool, calls, lets, structs/enums, pattern matching, array
  read/update, bounded loops (incl. array-element writes), casts, and
  width-tagged `mod`/`div`/`bitand`/`bitor`/`bitxor`/`shl`/`shr`/wrapping-`add`
- a **reusable proof layer** (evaluator fuel monotonicity + bounded counter-loop
  induction) that makes loop/array proofs systematic, with kernel-checked
  `bv_decide` automation
- **full refinement of a real cryptographic primitive against an independent
  spec**: a `BitVec`-valued SHA-256/HMAC spec, and a proof that the **entire**
  extracted SHA-256/HMAC chain refines it for all inputs in documented bounds.
  The `hmac_sha256` flagship carries **11 registered theorems**, kernel-checked
  (`--report check-proofs` = 11 verified, 0 failed); nine are full-contract
  refinements (block-to-words, schedule, round, compression, state
  serialization, multi-block padded hash, and the outer HMAC). Editing a source
  body turns the proof `stale` — regression-verified by perturbing the HMAC
  ipad constant (11 proved/0 stale → 10 proved/1 stale)
- a reusable, `fns`-generic **Proof Kit** (`Concrete.ProofKit`) harvested from
  that work, so later proofs import the machinery instead of copying it; see the
  proof-kit guide

What does not exist yet (planned, not shipped):

- **source-level contracts** (`requires`/`ensures`/`invariant`/`ghost`) and
  automatic verification-condition generation — see the design in the docs
- **unbounded / general refinement**: the HMAC chain is proved within documented
  input bounds (`k_len ≤ 128`, `m_len ≤ 256`, `len ≤ 375`); removing the bounds
  needs fuel-parametric induction. The proofs are functional-correctness only —
  not cryptographic security, and not machine-level constant time
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
