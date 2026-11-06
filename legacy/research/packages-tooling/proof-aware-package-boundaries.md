# Proof-Aware Package Boundaries

Status: open

This note makes one package-level idea explicit:

- package boundaries should eventually carry proof/evidence meaning, not only source/build meaning

Concrete already wants proof-aware artifacts.
This note narrows that into the package-boundary question.

## Why This Matters

A language with:

- proof attachments
- obligations
- trust boundaries
- capability summaries
- predictable/high-integrity directions

cannot treat packages as only:

- source folders
- dependency edges
- version numbers

Package boundaries eventually need to answer review questions such as:

- what trusted assumptions does this dependency introduce?
- what proof-backed guarantees does it actually export?
- what capabilities does it require?
- what profile claims does it satisfy or fail?
- what changed semantically between package versions?

## Package Boundary Summary

At a useful maturity level, a package should eventually be able to summarize:

- exported APIs
- exported capability requirements
- exported proof/evidence summaries
- trusted and FFI boundaries
- profile-relevant facts
- dependency-level trust widening

This is a boundary summary, not a replacement for the underlying facts.

## Good First Package-Level Surfaces

The first package-aware summary should stay narrow:

- exported function/type signatures
- capability summaries
- trusted/FFI boundary summaries
- proof status summaries
- profile membership summaries where available

That is already enough to improve auditability and review.

## Why This Fits Concrete

Concrete already emphasizes:

- explicit capabilities
- explicit trusted boundaries
- artifact-backed evidence
- semantic/trust drift review

Package boundaries are where those ideas become operational for real users.

Without package-aware summaries, those strengths stay too local.

## Relation To High-Integrity

High-integrity and review-heavy workflows need package boundaries to say more than:

- version `1.2.3`
- depends on `foo` and `bar`

They need package-visible answers to:

- did this dependency widen trust?
- did it add new authority?
- did proof status regress?
- did profile membership change?

That makes proof-aware package boundaries one of the strongest long-term multipliers for Concrete.

## Roadmap Implications

This note supports:

- proof-aware package artifacts
- dependency trust policy
- semantic diff / trust-drift workflow
- package-aware review and release gates

It should remain downstream of:

- stable fact/query identity
- stable proof/evidence artifacts
- interface/body artifact separation
