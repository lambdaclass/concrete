+++
title = "Architecture"
+++

# Architecture

Concrete's compiler pipeline is intentionally explicit:

```text
Source -> Parse -> Resolve -> Check -> Elab -> CoreCanonicalize -> CoreCheck -> Mono -> Lower -> SSAVerify -> SSACleanup -> EmitSSA -> clang
```

The important part is not only that these passes exist. It is that each pass has a clear boundary:

- parsing owns syntax
- resolution owns name binding
- checking owns the remaining surface-sensitive semantic work
- elaboration ends the surface language
- CoreCheck is the main post-elaboration semantic authority
- lowering produces the backend-oriented SSA program
- SSA verification and cleanup define the backend contract

## Why The Architecture Matters

Concrete is trying to become a language where:

- semantics stay explicit
- compiler magic stays narrow
- backend work does not re-decide language meaning
- proofs, reports, and tooling can all build on the same boundaries

That is why the project cares so much about Core, SSA, verifier boundaries, and removing raw string-based semantic dispatch.

## Current Direction

Recent architectural themes include:

- replacing semantic raw-name handling with typed identities
- hardening lowering around mutable aggregate storage and merge points
- expanding direct testing of reports, SSA shape, and optimized builds
- keeping the backend boundary explicit and inspectable

## Why Selected-Function Proofs Come Earlier

Concrete's proof story has two different scales:

- proving properties of selected Concrete functions
- proving the whole compiler pipeline

Those are not equally hard.

Selected-function proofs can attach to validated Core after `CoreCheck` and before `Mono`. That is a much smaller and cleaner semantic object than the whole compiler. It means the project can aim for an earlier milestone where:

- a Concrete function is elaborated and validated
- the compiler exposes a proof-oriented view of that Core
- Lean 4 is used to prove properties about the function

Whole-compiler proofs are broader because they have to cover the pass structure itself: elaboration, monomorphization, lowering, SSA, backend preservation, and the surrounding trust assumptions.

That is why the validated-Core boundary matters so much. It creates a serious proof target earlier than "prove the whole compiler."

## Where To Go Deeper

Read the stable architecture references:

- [`docs/ARCHITECTURE.md`](@/reference/ARCHITECTURE.md)
- [`docs/PASSES.md`](@/reference/PASSES.md)
- [`docs/LANGUAGE_INVARIANTS.md`](@/reference/LANGUAGE_INVARIANTS.md)
- [`docs/VALUE_MODEL.md`](@/reference/VALUE_MODEL.md)
