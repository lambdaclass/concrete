# Concrete-to-Lean Pipeline

**Status:** Open

This note defines the core proof bridge for Concrete: how checked Concrete code becomes something Lean can reason about faithfully.

## Why This Note Matters

If Concrete wants to be more than Lean-adjacent, it needs a clear answer to:

1. what representation of Concrete is proved in Lean
2. at what compiler stage that representation is extracted
3. how the extracted object traces back to the original source function
4. what assumptions remain outside the proof boundary

Without this, "prove Concrete functions in Lean" is a slogan rather than a compiler architecture.

## The Main Design Question

Concrete should not try to prove arbitrary LLVM or arbitrary source text directly.

The likely design is:

1. Concrete source is checked and elaborated
2. a proof-friendly Core representation is extracted
3. Lean definitions are generated from that checked representation
4. theorems are stated and proved over the Lean representation
5. reports/artifacts preserve traceability back to the source function

## What The Theorem Is About

This note should make explicit whether a proof is a statement about:

1. source syntax
2. validated Core
3. monomorphized Core
4. an extracted Lean model of one of those stages

That distinction is essential for trust and auditability.

## What Success Looks Like

Concrete should be able to point to:

1. one Concrete function
2. its extracted Lean representation
3. one theorem over that representation
4. a report or artifact linking the theorem back to the original function

That is the minimum credible end-to-end bridge.
