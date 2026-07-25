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

4. **References are scoped access, not ordinary values**
   Safe references are second-class. They may flow down into calls, callbacks,
   and borrow blocks, but safe APIs do not return or store `&T` / `&mut T`.
   Accessors use scoped callbacks, value returns, owned views, or trusted/raw
   pointer boundaries instead of lifetime-bearing reference APIs.

5. **Capabilities and trust boundaries must be visible**
   Effects belong in signatures. `trusted` code, `with(Unsafe)`, FFI crossings,
   allocation, and blocking authority should be syntactically and semantically
   visible.

6. **Predictable execution matters**
   The compiler should make recursion, loop boundedness, allocation, blocking,
   FFI, and other execution-shape risks visible. A pure or bounded core should
   be distinguishable from an effectful shell.

7. **Core/shell separation is a feature, not a workaround**
   The language should make it natural to write a bounded, analyzable core and
   keep I/O, host calls, and other effectful operations at the edge.

8. **Artifact-first auditability**
   The compiler should emit facts and evidence artifacts that humans, CI, and AI
   tools can consume directly. Concrete should not rely on "read the compiler
   source" as the audit workflow. The primary audience is still humans, but the
   same machine-readable explicitness is intentionally useful for LLM-assisted
   coding and review.

9. **Lean 4 proofs should attach to real Concrete code**
   The goal is not merely to reimplement functions in Lean. The language should
   expose a well-defined proof target so real Concrete functions can carry Lean
   4-backed evidence where the proof subset permits it.

10. **Small analyzable core over feature growth**
   Concrete should prefer a smaller language with stronger boundaries over a
   larger feature surface with weaker analysis. If a feature reduces clarity, it
   must clear a high bar.

11. **Honest trust boundaries**
    Concrete must distinguish what is:
    - enforced by the compiler
    - reported by analysis
    - proved through Lean-backed artifacts
    - trusted at the source, backend, toolchain, or target boundary

12. **Semantic entities have compiler-owned identity**
    Source names and link symbols are *representations* of an entity, never the
    entity itself. Every transformation either preserves an entity's identity or
    creates a checked derivation from it. This covers type, function, module,
    builtin, specialization, layout, and source-origin identity.

    This principle is written from failures, not from theory. Bugs 039, 044, 045,
    050, 051, 054, 055, and — found after this principle was written, by looking
    for the class it names — 056, 057 and 061 have one shared cause: a
    representation used as identity and re-interpreted by a later pass — an import alias resolved twice, a match
    binder sharing a slot with its outer namesake, a local callable rewritten
    into a direct call because its text matched a global, one enum declaration
    serving instantiations of different sizes, a specialization name a user
    program can spell. The later three extend the class beyond source names: a
    function reference carried as a REGISTER NAME (`@fnref.f`) that a phi cannot
    hold (056), a struct size restated as a CONSTANT that drifted from the
    declaration until by-value copies truncated and segfaulted (057), and a proof
    model that spells a parameter application exactly like a call of a global
    (061). Identity-as-representation is not only a string problem; any restated
    fact drifts. Each was found separately and fixed separately; the principle is
    what makes the class visible in advance.

    Corollary for evidence: a value-level test cannot establish an identity or
    layout property. R-0001 showed why — under a mutation that removed
    specialization, programs whose wider instantiation was emitted first still
    returned correct results. Identity claims need structural assertions
    (distinct declarations, distinct footprints, distinct symbols), and each
    phase should earn its invariants through a validated artifact rather than by
    convention.

    The live identity seams are indexed below so the principle is not applied
    only after another defect. This is an ownership index, not a second task
    queue; execution order remains the roadmap's file order.

    | Entity / invariant | Representation that must not become identity | Owner |
    |---|---|---|
    | Direct call vs callable value | callee spelling | R-0002 (landed), R-0436 |
    | Module/import target | alias or basename | R-0008 |
    | Generic specialization | forgeable display/link symbol | R-0007 |
    | Aggregate declaration and payload footprint | type name or restated size constant | R-0434 |
    | Structural destruction | `tyName` lookup / empty fallback | R-0006 |
    | ProofCore global call vs local application | one `PExpr.call` string | R-0442 |

## Short Positioning

Concrete is a minimalist, no-GC, linear/resource-aware systems language
implemented in Lean 4. It uses scoped second-class references, explicit
capabilities, predictable-execution analysis, and artifact-backed auditability to
make authority, trust, and proof status visible in ordinary systems code.
