# Proof UX And Authoring Loop

Status: open

This note defines the user-facing proof workflow Concrete should eventually make normal.

The problem is not only "can a theorem be attached".
The product problem is:

- can a user understand proof state
- can a user repair stale proofs
- can a user tell what is blocked and why
- can a user land proof-preserving changes without reading compiler internals

## Desired Developer Loop

The end state should feel like one coherent toolchain workflow:

1. build / check code
2. inspect evidence and proof status
3. attach or update a spec/theorem
4. detect stale or blocked proof state
5. repair the proof or attachment
6. rerun the same normal workflow

This should not require ad hoc scripts or private knowledge of internal artifact shapes.

## Required Status Vocabulary

The proof-facing UX should make the following distinctions obvious:

- proved
- stale
- blocked
- missing
- ineligible
- trusted assumption

Each status should answer:

- what happened
- why it matters
- what to do next

## Required Diagnostics Quality

Good proof diagnostics should tell the user:

- which function/spec/proof is involved
- which artifact or fingerprint changed
- whether the problem is eligibility, staleness, attachment mismatch, unsupported construct, or missing theorem
- one plausible repair action

This should apply to:

- CLI output
- report artifacts
- CI failures
- debug bundles

## Stale-Proof Repair Workflow

Concrete should eventually make these cases routine:

### Attachment drift

- theorem name changed
- spec name changed
- registry entry changed

### Semantic drift

- function body changed
- extracted ProofCore target changed
- fingerprint changed

### Eligibility drift

- the function gained unsupported constructs
- the function crossed a trusted/effect boundary
- the function is no longer in the provable subset

Each should lead to a clear repair path rather than "something is broken".

## AI-Assisted Repair

AI can help with:

- suggesting stale-proof repairs
- proposing new attachment entries
- drafting theorem updates
- explaining blocked or ineligible states

But the trust model stays:

- AI proposes
- Lean kernel checks
- compiler validates artifacts and consistency

Concrete should never let AI-generated proof text become trusted without the normal kernel/artifact checks.

## Product Bar Before Source-Level Contracts

Concrete should not add broad source-level proof syntax until this loop is good enough that:

- a normal engineer can understand proof status
- CI failures are actionable
- stale proofs are repairable
- registry/artifact drift is visible

If that is not true, source-level contracts and invariants will only multiply confusion.

## Release Relevance

Before the stronger proof story is presented publicly, Concrete should be able to show:

- at least one example with a clear proof authoring and repair loop
- actionable stale-proof diagnostics
- stable enough artifact semantics that proofs do not feel like hidden metadata

## Related Notes

- [Verification Product Model](./verification-product-model.md)
- [Proof UX And Verification Influences](./proof-ux-and-verification-influences.md)
- [Spec Attachment](./spec-attachment.md)
- [Verification Surface](./verification-surface.md)
