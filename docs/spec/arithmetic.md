# Arithmetic

This chapter is a stub. It holds the foundation decision about floating point. The rest of the
area (integer semantics, overflow, checked, wrapping and saturating operations, casts) migrates
here from ARITHMETIC_POLICY and NUMERIC_MODEL in README §8, step 4.

## Rules

<a id="ARITH-1"></a>
### ARITH-1. Floating point is IEEE-754 with round-to-nearest-even

**ARITH-1@1.** `f32` and `f64` are IEEE-754 binary32 and binary64, every operation rounds to nearest with ties to even, and no operations are contracted (no fused multiply-add unless written).

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0493
- Semantics: none
- Obligation: a total formal reference model for floats, proved for the profile or labelled trusted (VERIFIED_PIPELINE, "Floats")
- Evidence: tested

**Rationale.** A fixed rounding mode and no contraction make float results deterministic across
the interpreter, the LLVM profile and the native profile, so the consumers can be compared and a
reference model can define them. NaN bits, subnormals, signed zero and conversions are specified
together with the reference model.
