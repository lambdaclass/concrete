+++
title = "Direction"
+++

# Project Direction

Concrete is not trying to become "every systems language feature at once."

The project direction is organized around a few major ideas:

- auditable low-level programming
- explicit authority and trust boundaries
- a small and honest semantic surface
- a compiler architecture shaped for formal reasoning

## Current Phase Order

The roadmap is currently structured around major phases:

- fast feedback and compiler stability
- semantic cleanup
- tooling and stdlib hardening
- backend and trust multipliers
- runtime and execution model
- capability and safety productization
- language surface and feature discipline
- package/dependency ecosystem
- project and operational maturity

The important point is not only the list. It is the sequencing: architecture and semantics first, then tooling and stdlib hardening, then deeper backend/proof/runtime/ecosystem work.

## Research Direction

The main long-term differentiators are:

- audit outputs as first-class compiler products
- proving the language and compiler in Lean
- eventually proving selected Concrete programs in Lean through formalized Core semantics
- explainable compiler decisions and inspectable artifacts
- reproducible and trustworthy project operations over time

## A Key Phase D Milestone

One of the most important planned breakthroughs sits in Phase D:

- a user writes a selected Concrete function
- the compiler exposes its validated Core / `ProofCore` representation
- the user writes Lean 4 proof code about that function

This is intentionally narrower than "prove the whole compiler." It is also much more achievable earlier.

Why this matters:

- it gives Concrete an earlier proof milestone than full compiler verification
- it validates that validated Core is the right proof boundary
- it shows that Concrete can be a real low-level implementation language while Lean 4 is used to prove properties about that code

So the project is not waiting for "fully verified compiler or nothing." Phase D should already make selected-function proofs real.

## Where To Go Deeper

Read:

- [`ROADMAP.md`](https://github.com/unbalancedparentheses/concrete2/blob/main/ROADMAP.md)
- [`CHANGELOG.md`](https://github.com/unbalancedparentheses/concrete2/blob/main/CHANGELOG.md)
- [`docs/IDENTITY.md`](@/reference/IDENTITY.md)
- [`research/README.md`](https://github.com/unbalancedparentheses/concrete2/blob/main/research/README.md)
