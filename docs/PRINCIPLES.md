# Concrete Principles

Status: stable reference

This document states the stable design principles behind Concrete's language
shape.

Concrete is trying to be a minimalist systems programming language with explicit
authority, explicit operational boundaries, and a path to real Lean 4-backed
proofs.

## Core Principles

1. **Systems-first, not theorem-prover-first**
   Concrete is a systems language implemented in Lean 4, not Lean 4 with a
   different runtime. The source language should feel like systems programming,
   not like writing proof scripts.

2. **No hidden runtime model**
   Concrete has no GC and no hidden reference counting. Memory/resource behavior
   should be visible enough to audit and reason about.

3. **Linearity and ownership are part of the model**
   Resource-sensitive values should have explicit ownership rules. The language
   prefers linear/resource-aware rules over implicit lifetime or runtime
   machinery.

4. **Capabilities and trust boundaries must be visible**
   Effects belong in signatures. `trusted` code, `with(Unsafe)`, FFI crossings,
   allocation, and blocking authority should be syntactically and semantically
   visible.

5. **Predictable execution matters**
   The compiler should make recursion, loop boundedness, allocation, blocking,
   FFI, and other execution-shape risks visible. A pure or bounded core should
   be distinguishable from an effectful shell.

6. **Core/shell separation is a feature, not a workaround**
   The language should make it natural to write a bounded, analyzable core and
   keep I/O, host calls, and other effectful operations at the edge.

7. **Artifact-first auditability**
   The compiler should emit facts and evidence artifacts that humans, CI, and AI
   tools can consume directly. Concrete should not rely on "read the compiler
   source" as the audit workflow. The primary audience is still humans, but the
   same machine-readable explicitness is intentionally useful for LLM-assisted
   coding and review.

8. **Lean 4 proofs should attach to real Concrete code**
   The goal is not merely to reimplement functions in Lean. The language should
   expose a well-defined proof target so real Concrete functions can carry Lean
   4-backed evidence where the proof subset permits it.

9. **Small analyzable core over feature growth**
   Concrete should prefer a smaller language with stronger boundaries over a
   larger feature surface with weaker analysis. If a feature reduces clarity, it
   must clear a high bar.

10. **Honest trust boundaries**
    Concrete must distinguish what is:
    - enforced by the compiler
    - reported by analysis
    - proved through Lean-backed artifacts
    - trusted at the source, backend, toolchain, or target boundary

## Short Positioning

Concrete is a minimalist, no-GC, linear/resource-aware systems language
implemented in Lean 4. It uses explicit capabilities, predictable-execution
analysis, and artifact-backed auditability to make authority, trust, and proof
status visible in ordinary systems code.
