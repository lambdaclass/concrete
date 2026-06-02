# Opaque Validated Types

**Status:** Open

Concrete needs a small, explicit way to carry validated domain values without falling back to raw integers, raw strings, or convention-heavy comments.

## Problem

Many important systems values are "just integers" at runtime but not semantically interchangeable:

- ports
- user IDs
- packet lengths
- nonzero sizes
- ASCII-only text
- already-validated handles or identifiers

Without a better type surface, validation happens at the boundary and then dissolves back into primitive types. That weakens APIs, reviews, and proof attachment points.

## Design Goals

1. **Zero-cost representation.**
   The wrapper should not imply hidden allocation or runtime indirection.

2. **No implicit coercions.**
   The wrapper should sharpen meaning, not disappear into the underlying type automatically.

3. **Validation stays explicit.**
   Construction should happen through visible constructors or validation functions, not hidden magic.

4. **Conversions use one honest vocabulary.**
   Parsing, narrowing, and checked conversion should look like one family rather than many ad hoc conventions.

5. **The feature must stay small.**
   This should not grow into a large trait-derivation or inference-heavy abstraction system.

## Likely Direction

The first-release shape should look more like a narrow newtype facility than a broad abstraction framework:

- one-field wrapper types with explicit constructor visibility
- explicit `into_inner`-style extraction
- explicit `try_from`-style conversion for narrowing and validation
- parser and boundary helpers that return validated wrappers directly

Example shapes worth supporting:

- `Port`
- `PacketLen`
- `NonZeroU32`
- `AsciiText`
- `UserId`

## Interaction With Layout

If Concrete eventually supports transparent wrappers for ABI/layout purposes, that support should be tied to the layout-contract work and kept extremely small. Validated wrappers should not force a large representation feature set by themselves.

## Things To Avoid

- implicit wrapper/unwrapper conversions
- validation hidden behind assignment or coercion
- broad derive systems just to make wrapper types ergonomic
- wrapper features that depend on inference-heavy trait machinery

## Why This Fits Concrete

This is a good fit because it makes boundaries sharper:

- APIs say more with the same runtime cost
- validation happens once at the edge and stays visible in the type
- proof targets and reports can attach to a domain-specific value rather than a raw primitive

## Roadmap Fit

This belongs in the stdlib/syntax freeze, especially alongside:

- checked indexing and byte-cursor APIs
- error/result ergonomics
- explicit layout/ABI contracts
- proof and report surfaces that care about validated invariants
