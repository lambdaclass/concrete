# Bug 109 — an `f32` literal that is not exactly representable is emitted as a decimal LLVM `float` constant

**Status:** FIXED (branch `backend-108-116`, commit 0ee4252e)
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** compile failure at LLVM validation for `f32`; found while fixing it, the same printer was **silent wrong code** for `f64` (six significant decimals: `0.123456789` compiled as `0.123457`, `0.0000000001` as `0`).

## Reproducer

`tests/regressions/bug109_f32_literal_invalid_ir/repro.con`:

```concrete
// Bug 109: an f32 literal that is not exactly representable (0.1) is emitted
// as a decimal LLVM `float` constant, which llvm-as rejects.
// Expected: exit 1. Actual: "floating point constant invalid for type".
fn main() -> i32 {
    let x: f32 = 0.1;
    if x > 0.0 { return 1; }
    return 0;
}
```

## Observed

`let x: f32 = 0.1;` emits `fcmp ogt float 0.100000, ...`; llvm-as rejects it: `floating point constant invalid for type`. LLVM accepts a decimal `float` constant only when it is exactly representable; otherwise the constant must be written as the hex form of the double that rounds to the f32 value.

## Expected

Emit `f32` constants in LLVM hex form (the f32 value widened to double). (As filed this read "`f64` literals are unaffected"; the fix showed they were silently truncated to six decimals, see Fix.)

## Where it showed up

examples/snippets/floats.con, manifest `known-bug 109`.

## Cause

`Concrete/Backend/EmitLLVM.lean`, `printLLVMOperand` for `.floatLit`: the constant was rendered with
Lean's `toString` on `Float`, which prints six digits after the point. For `float` that is invalid
whenever the value is not exactly an f32 (LLVM reads a `float` constant as a double and requires it to
be exactly representable); for `double` it is accepted and wrong. `EmitSSA.svalToOperand` passed the
f64 value of an f32 literal through unrounded.

## Fix

Every float constant is printed as `0x` plus the 16 hex digits of its IEEE double bits, the form LLVM
reads exactly for both types, and `EmitSSA` rounds an `f32` constant to f32 (`Float.toFloat32`) before
it is printed. The rounding goes decimal -> double (correctly rounded, in the lexer) -> f32, which can
differ from a direct decimal -> f32 rounding in a double-rounding tie; that residual is recorded here,
not fixed. Permanent leg: `tests/programs/regress_109_float_literals.con` in `run_tests.sh` (run_ok
42: f32 0.1 compares and widens to exactly 0.100000001490116119384765625; f64 0.123456789 and
0.0000000001 keep their value). The interpreter has no floats, so the leg is native-only.
examples/snippets/floats.con is promoted to `compile` with a `@run` rule.

Not repaired here (same lossy `toString` on `Float`, different consumers; the formatter and the
SSA debug printer were since repaired as [bug 135](135_fmt_float_literals_lossy.md), with the Core
printer): the formatter
(`Frontend/Format.lean`, so `concrete fmt` rewrites `0.0000000001` to `0.000000`), the legacy proof
fingerprint (`ProofCore.fingerprintExpr`, `(float {v})`, where two distinct literals can render
alike; floats are outside ProvableV1, so it carries no claim today) and the SSA debug printer
(`IR/SSA.lean`, `--emit-ssa` only). The v2 identity bytes are exact: `EvidenceBuild.evFloatLit`
stores `Float.toBits` and `IdentityUseBytes` serializes those bits.

## Structural cause

Floating-point literals are rendered to LLVM text with a decimal formatter that assumes every constant is exactly representable; the literal's value is never converted to the target width before it is printed. The emitter decides a numeric encoding locally, unchecked against the type: the lexer computes the literal's value correctly and the printer gives the binary a different one (class 7 in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)). The rendering of a float is also kept in several places (LLVM emitter, formatter, legacy proof fingerprint, SSA debug printer), all with the same lossy `toString` (class 12); the v2 identity bytes alone use the exact bits. Reclassified from class 8 on the fix: the defect is in the emitter, not in `Lower`'s storage decisions.

## Structural owner

R-0502 (S11 one `Layout`/scalar encoding for the backend) with R-0504 (S12 validators); f32 is outside ProvableV1, so the proposed interim owner is the float-cast gate (`check_float_cast.sh`) extended to f32 literals.

