# Arithmetic Overflow Policy

**Status:** Open

Concrete needs an explicit arithmetic-policy story before it can make strong predictable, proof, or high-integrity claims.

## Problem

Today, fixed-width integer arithmetic wraps silently at runtime while the current proof model uses Lean integers with no overflow. That creates three kinds of drift:

- source readers cannot tell whether code intends wrapping or checked arithmetic
- diagnostics and reports do not surface the active arithmetic assumption clearly
- proof-backed claims risk sounding stronger than they are when overflow is left ambient

## Design Goals

1. **No hidden arithmetic mode.**
   Build mode, optimizer choice, or backend details should not silently change overflow semantics.

2. **A very small set of modes.**
   Concrete should prefer a tiny explicit set such as wrapping and checked arithmetic. Additional modes must earn their complexity.

3. **Profile and source visibility.**
   Reviewers should be able to see the active policy in source, diagnostics, and artifacts.

4. **Proof honesty.**
   Proof/evidence outputs must say whether a claim assumes no overflow, models wrapping arithmetic, or excludes the operation.

5. **Conversion consistency.**
   Narrowing casts and numeric decoding helpers should align with the same checked/unchecked vocabulary rather than inventing their own ad hoc rules.

## Likely Direction

The most promising first-release shape is:

- explicit wrapping and checked arithmetic as the main language-level policies
- policy selection at a visible boundary such as profile, module, or function scope
- reports and diagnostics that name the active policy rather than treating it as ambient
- saturating arithmetic, if needed, as library APIs first rather than a broad operator-level surface

## Things To Avoid

- debug-vs-release arithmetic semantics
- optimizer-dependent overflow promises
- a large menu of arithmetic modes with weak diagnostic/report support
- implicit coercions or hidden widening/narrowing as an attempted workaround

## Why This Fits Concrete

Concrete already treats authority, trust, allocation, and failure shape as public facts. Arithmetic policy should meet the same bar. This is not about convenience. It is about making behavior legible enough to audit and honest enough to prove against.

## Roadmap Fit

This belongs in the stdlib/syntax freeze and runtime-profile work:

- profile documentation
- diagnostics and evidence artifacts
- proof semantics boundary
- parser/byte-cursor and checked-conversion APIs
