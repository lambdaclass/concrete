+++
title = "Why Not"
+++

# Why Not X?

Concrete has several deliberate "no" or "not yet" decisions.

This page exists so those decisions do not look accidental.

## Why Not Closures?

Because Concrete is trying to keep ownership, effect boundaries, and auditability explicit.

Closures tend to hide capture and lifetime structure unless the language pays a lot of extra complexity for them. Concrete is choosing not to pay that cost.

See also:

- `research/language/no-closures.md`

## Why Not Trait Objects?

Concrete is trying to keep dynamic behavior explicit and analyzable.

Trait objects bring hidden indirection and more runtime semantics than the language currently wants.

See also:

- `research/language/no-trait-objects.md`

## Why Not Contracts First?

Because Concrete already has a simpler and more central proof path:

- validated Core
- explicit authority and trust boundaries
- Lean 4 proving selected Concrete functions

Contracts may still make sense later, but they are not part of the core philosophy today.

See also:

- `research/language/pre-post-conditions.md`

## Why Not MLIR First?

Because MLIR is not a substitute for cleaning up the current backend contract.

The project first needs:

- structured LLVM emission
- a stronger SSA/backend contract
- reusable artifacts

MLIR can be evaluated later if it earns its complexity.

See also:

- `research/compiler/mlir-backend-shape.md`

## Why Not Self-Hosting First?

Because self-hosting is not the same thing as semantic clarity, trust, or proof readiness.

Concrete gets more leverage from:

- semantic cleanup
- backend cleanup
- a stronger artifact story
- Lean-side proof support

Self-hosting can become useful later. It is not the first trust milestone.

## Why Not Prove The Whole Compiler First?

Because proving selected Concrete functions is a much earlier and more practical milestone.

The compiler-proof story is broader:

- more passes
- more preservation lemmas
- more global architecture work

The selected-function proof story can start earlier over validated Core.

## Why Not Surface Syntax As The Proof Target?

Because source syntax is the wrong level of truth.

The right proof boundary is validated Core:

- sugar removed
- semantics explicit
- legality checks already run

That is a much cleaner object to reason about.

## The General Rule

Concrete is trying to stay small, explicit, and audit-friendly.

So the default answer to new complexity is:

- not now unless it clearly strengthens that identity
