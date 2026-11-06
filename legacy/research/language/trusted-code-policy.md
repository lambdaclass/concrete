# Trusted Code Policy

**Status:** Open

This note defines the policy questions around `trusted` code beyond the local language mechanism.

## Why This Matters

Concrete already has a coherent `trusted` boundary design. What it does not yet fully define is the project policy around trusted code:

1. how much trusted code is acceptable
2. how trusted code should be audited
3. how reports should expose it
4. how restricted profiles should treat it

That policy matters if Concrete wants trusted code to remain small, reviewable, and evidence-friendly.

## Local Mechanism vs Project Policy

The local mechanism is already clear:

1. `trusted` contains pointer-level implementation details
2. semantic effects still stay visible in capability signatures
3. FFI remains separate from ordinary trusted pointer work

The missing part is the higher-level question: what standards should the project apply to trusted regions?

## Key Policy Questions

### 1. Trusted code budget

Concrete may eventually want a visible answer to:

1. how much trusted code exists?
2. where is it concentrated?
3. is it shrinking or growing over time?

This matters for the trusted computing base story.

### 2. Audit expectations

Trusted code should likely carry stronger expectations:

1. clearer comments about invariants
2. focused tests
3. stronger report visibility
4. possibly separate review expectations

### 3. Profile interaction

Restricted profiles may want to distinguish:

1. trusted code that is allowed as audited infrastructure
2. trusted code that is forbidden inside profile code itself
3. trusted wrappers over FFI versus trusted internal data-structure code

### 4. Reporting

Concrete should decide whether reports eventually expose:

1. which functions use trusted code
2. transitive dependence on trusted code
3. package/module-level trusted-code summaries

This is especially important for high-integrity or predictable-execution workflows.

## Relationship To Other Notes

1. [trusted-boundary.md](trusted-boundary.md)
2. [high-integrity-profile.md](high-integrity-profile.md)
3. [../predictable-execution/effect-taxonomy.md](../predictable-execution/effect-taxonomy.md)
4. [../proof-evidence/trust-multipliers.md](../proof-evidence/trust-multipliers.md)

## Bottom Line

The language mechanism for `trusted` is only half the story. Concrete also needs a project-level trusted-code policy so that:

1. the trusted boundary stays reviewable
2. profiles can restrict or classify trusted usage coherently
3. reports can support a real trusted-computing-base story
