# Specification Attachment

**Status:** Open

This note defines how specifications should attach to Concrete functions in a Lean-backed proof workflow.

## Why This Matters

Proofs are only useful if it is clear:

1. what function the proof is about
2. what property is claimed
3. what preconditions are assumed
4. where that specification lives

If the spec model is unclear, proof-backed evidence will remain niche and fragile.

## The Design Space

Likely options include:

1. specs live entirely in Lean, keyed to extracted function artifacts
2. Concrete eventually gains lightweight source-level markers that point to external Lean specs
3. reports carry spec/proof links without changing ordinary function syntax

Concrete should start with the lightest option that still gives clear traceability.

## Recommended Sequence

Do not add a broad source-level contract language first.

1. **Lean-attached specs first.** Keep specs and theorems next to the Lean proof model. Key them to a qualified Concrete function identity and a checked body fingerprint, the same way proof evidence is attached.
2. **External spec/proof/result registry next.** Move the relationship between Concrete function, extracted proof term, spec name, obligation name, body fingerprint, proof result, and trusted assumptions into a reproducible artifact.
3. **Optional source-level spec markers later.** Add Concrete syntax only if the Lean-attached and artifact-backed workflow is working and users need a small marker that points to a spec. Do not make ordinary systems code look like a proof script by default.

This order keeps the proof workflow testable before committing to source syntax.

## What This Note Should Decide

1. whether specifications are source-visible, artifact-visible, or Lean-only
2. how preconditions and postconditions are named
3. how one theorem is tied to one Concrete function revision
4. whether profile compliance and proof obligations can share artifact structure
