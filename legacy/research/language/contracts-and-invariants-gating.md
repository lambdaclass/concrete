# Contracts And Invariants Gating

Status: open

This note records when Concrete should consider adding:

- source-level preconditions
- source-level postconditions
- loop invariants
- ghost/proof-only code

The question is not whether these features are useful.
They clearly are.
The question is when they become justified without weakening the current verification story.

## Current Position

Concrete already has a proof direction, but it is still centered on:

- Lean-attached specs and theorems
- artifact-backed proof status
- ProofCore extraction
- stale/blocked/ineligible reporting
- explicit trust and backend boundaries

That is the right order.

Adding contracts or invariants too early would create a stronger-looking proof surface before:

- the semantic boundary is mature
- the diagnostics are good enough
- the user workflow is usable enough
- the trust story is explicit enough

That would be a mistake.

## What Must Be True First

Contracts, invariants, and ghost code should remain gated until all of the following are true:

### 1. Proof Workflow Is A Normal Product Surface

Concrete should already have a coherent:

- build
- check
- report
- prove
- stale-detect
- repair

workflow.

If proof state is still a specialist workflow, adding more proof syntax will only multiply confusion.

### 2. Proof Status Is Actionable

Users should already be able to understand:

- proved
- stale
- blocked
- missing
- ineligible

without reading compiler internals.

Contracts and invariants should not arrive before these statuses are reliable and easy to act on.

### 3. ProofCore Boundary Is Stable Enough

Before source-level proof-oriented constructs exist, Concrete should already know:

- what semantics are inside the proof-relevant subset
- what constructs are excluded
- what extraction preserves
- what “proved” means relative to source and compiled behavior

If that boundary is still shifting, contracts and invariants will sit on unstable semantics.

### 4. Artifact And Registry Semantics Are Stable Enough

Concrete should already have:

- stable enough proof artifacts
- explicit stale detection
- attachment validation
- theorem identity checking
- traceability from source to proof target

Otherwise proof annotations in source will only hide the fact that the underlying artifact model is still moving.

### 5. Diagnostics And Repair Workflow Are Good Enough

When a contract or invariant fails, the user must get:

- a useful location
- a useful reason
- a useful next action

If the diagnostics are still weak, source-level contracts will look impressive but feel hostile.

## Gating By Construct

### Preconditions / Postconditions

These are the earliest plausible candidates, but only after the conditions above hold.

Why they are the earliest:

- they fit explicit interface reasoning
- they align with Concrete's boundary-first philosophy
- they can remain narrower and more auditable than full dependent typing

### Loop Invariants

These should come later than basic contracts.

Why:

- they raise proof burden substantially
- they are one of the fastest ways to make proof UX unpleasant
- they need very good diagnostics and repair guidance

### Ghost / Proof-Only Code

This should come later still.

Why:

- it requires an explicit erasure story
- it increases the gap between executable code and proof-facing code
- it is easy to overuse if the product workflow is still immature

## What Should Not Happen

Concrete should avoid:

- adding broad source-level proof syntax just because Lean attachment exists
- making contracts look stronger than the current artifact and proof boundary can support
- copying SPARK/F*/Dafny surface features before the workflow and semantics are ready

The correct order is:

1. stable proof/evidence workflow
2. clear statuses and diagnostics
3. stable semantic/artifact boundary
4. then narrow, explicit proof-facing source constructs if examples truly need them

## Recommendation

Keep contracts, invariants, and ghost code research-gated.

Revisit them only after:

- proof workflow is a normal toolchain story
- stale/blocked repair is usable
- ProofCore and attachment semantics are stable enough
- at least one real proof-backed example demonstrates the need

Until then, Lean-attached specs plus artifact-backed evidence are the right model.

## Related Notes

- [Pre/Post Conditions](./pre-post-conditions.md)
- [Verification Product Model](../proof-evidence/verification-product-model.md)
- [Proof UX And Authoring Loop](../proof-evidence/proof-ux-and-authoring-loop.md)
- [Verification Surface](../proof-evidence/verification-surface.md)
