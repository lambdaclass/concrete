# Self-Describing Binaries And Proof-Facing Release Artifacts

Status: research

## Problem

Concrete's proof and evidence story naturally raises a tempting idea:

- can an emitted binary carry enough semantic identity that review and verification become easier?

This is attractive, but easy to overstate.

## Honest Version

The useful version is narrow:

- a binary or adjacent release artifact may embed or reference:
  - source/build identity
  - report identities
  - proof-facing subject identities
  - bundle manifest identity

Then a verifier can:

- recover those identities
- fetch or read the corresponding compiler artifacts
- check reports, proofs, and reproducibility against the same build subject

This is strong because it reduces ambiguity about what the binary corresponds to.

## Dangerous Version

The wrong version sounds like:

- "the executable proves itself"
- "the binary carries its full formal semantics"

That phrasing is too loose.

Concrete should keep the semantic authority in:

- validated Core
- proof-facing exports
- machine-readable reports
- evidence bundles

The binary may point to or carry identities for those artifacts, but it should not be treated as a magical standalone proof object.

## Why This Fits Concrete

This fits if kept narrow because it reinforces:

- artifact identity
- proof traceability
- review workflows
- release verification

without inventing a second semantic system.

## Preconditions

This idea depends on:

- stable proof-facing subject identity
- stable report identity
- deterministic enough build identity
- explicit release/bundle workflow

Without those, "self-describing binaries" are mostly marketing language.

## Roadmap Placement

This belongs across:

- **Phase I** for proof-facing export identities
- **Phase L** for release/bundle/product workflow

## Current Recommendation

Keep this visible as:

- **research**

and phrase it conservatively:

- binaries may carry or reference proof/report/bundle identities
- verification still runs over the explicit compiler artifacts those identities refer to
