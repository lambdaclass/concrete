# Layout Contract Surface

**Status:** Open

Concrete already has an implementation-level layout system. The open question is narrower: what source-level layout and ABI promises should the language actually make stable?

## Problem

The implementation currently knows a lot about size, alignment, offsets, enum payload layout, and extern ABI behavior. That is not the same as having a clean source contract.

Without a smaller explicit contract, the project risks drifting into one of two bad states:

- users rely on implementation accidents that were never meant to be stable
- docs over-promise a broad layout story that the language cannot defend clearly

## Design Goals

1. **Keep the promised surface smaller than the implemented surface.**
   The compiler may know more than the language chooses to guarantee.

2. **Make stable vs opaque obvious.**
   Reviewers should not have to infer whether a type's layout is guaranteed.

3. **Keep FFI and package boundaries auditable.**
   Layout-relevant facts should be visible in reports and interface artifacts.

4. **Avoid a large menu of representation knobs.**
   Each extra `repr` rule multiplies audit cost, proof cost, and backend coupling.

## Likely First-Release Shape

The most plausible first-release contract is:

- `#[repr(C)]` for C-facing struct layout
- `#[repr(packed)]` for tightly packed layout when explicitly requested
- `#[repr(align(N))]` for explicit raised alignment
- unannotated structs remain compiler-controlled and are not FFI-stable
- enums remain outside the stable FFI layout surface

Transparent-wrapper support may be justified later, especially if opaque validated types land, but only if the rule stays trivial to explain and verify.

## Artifact Requirements

If Concrete makes layout promises, the evidence surface should carry them directly:

- repr annotations
- size and alignment
- field offsets where relevant
- whether the layout is guaranteed or intentionally opaque
- target/ABI caveats where the guarantee is not cross-target

These facts should appear in layout reports and eventually in module/package interface artifacts.

## Things To Avoid

- a broad Rust-style `repr` feature catalog without matching diagnostics and report support
- stable promises about enum layout before the project wants to defend them
- target-independent promises where the real contract is target-specific
- letting validated wrappers or FFI growth silently expand the stable layout surface

## Why This Fits Concrete

Concrete wants explicit authority, explicit trust boundaries, and artifact-first auditability. Representation promises should follow the same discipline. A smaller, sharper layout contract is better than a rich one that reviewers cannot actually trust.

## Roadmap Fit

This work belongs before the first public stdlib and FFI freeze, and before package/interface artifacts start carrying representation-sensitive facts as stable external contracts.
