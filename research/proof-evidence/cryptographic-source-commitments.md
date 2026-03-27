# Cryptographic Source Commitments And Reproducible Evidence Bundles

Status: research

## Problem

Concrete already points toward:

- reproducible trust bundles
- machine-readable reports
- proof-facing artifacts
- provenance-aware package and release workflows

The missing question is how strong the final operational claim should be.

One plausible long-term direction is:

- every build produces a deterministic bundle manifest
- the manifest commits to source, dependencies, compiler identity, target/build settings, and evidence outputs
- later, the bundle may also carry signatures or attestations
- verification can rebuild and check the result against the committed identities

## Why This Fits Concrete

This is a good fit because it strengthens:

- auditability
- reproducibility
- review workflows
- package/release trust
- proof/evidence packaging

without adding new language semantics.

The important point is that this is an operational artifact story, not a new type-system feature.

## Good First Version

The first useful version should stay narrow:

1. deterministic machine-readable bundle manifest
2. stable identities for source/build/report/proof-facing artifacts
3. replayable verification that rebuilds and compares
4. clear mismatch reporting when reproduction fails

That is enough to make the provenance story operational without requiring:

- signing infrastructure
- distribution policy
- external attestation integration

## Later Version

Only after the basic artifact and reproducibility story is solid should Concrete consider:

- signed bundles
- CI identity binding
- package publication provenance
- interoperability with broader attestation ecosystems

Those are workflow and policy questions, not core language questions.

## What Not To Do

Do not:

- turn signing into a language-semantic concept
- promise "perfect supply-chain security" from a compiler feature
- make bundle production mandatory for ordinary development
- create a second metadata system detached from compiler facts

The right direction is:

- one artifact identity system
- one evidence/report system
- optional stronger provenance layers on top

## Roadmap Placement

This belongs mainly in:

- **Phase L** for operational trust bundles and verification workflows
- **Phase J** where package graph identity and provenance-aware publishing become real
- **Phase I** only insofar as proof-facing artifacts become part of the bundle contents

## Current Recommendation

Treat this as:

- **committed operational direction**

but keep:

- signing
- publication policy
- attestation interoperability

as research and workflow design questions rather than near-term promises.
