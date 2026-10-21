# Bug 139 — SSA cleanup folds `x != x` to false (and `x == x` to true) for a float `x`

**Status:** FIXED 2026-10-21 (interpreter-coverage pass, branch `interp-coverage`)
**Found:** 2026-10-21, by the interpreter-coverage pass: once `--interp` could run floats
(bug 128), the first float probe disagreed with the binary on NaN comparisons.
**Severity:** silent wrong code in safe code. A NaN test written `x != x` (the portable spelling)
is always false in the binary, so a NaN passes every such check.

## Reproducer

`tests/regressions/bug139_float_self_compare_fold/repro.con`:

```con
fn mk(z: f64) -> f64 { return z / z; }
fn main() -> Int {
    let nan: f64 = mk(0.0);
    let mut r: Int = 0;
    if nan != nan { r = r + 1; }
    if nan == nan { r = r + 10; }
    return r;
}
```

On main (`5a064e77`) the binary exits **10** (`nan == nan` held, `nan != nan` did not); `--interp`
(with this pass's float support) prints **1**. Expected 1: IEEE equality is false on NaN and
inequality is true, which is what the backend's `fcmp oeq` / `fcmp une` compute.

## Cause

`foldBinOpAlgebraic` in `Concrete/IR/SSACleanup.lean` folds a comparison of a register with itself
(`x == x` → true, `x != x` → false, `<=`/`>=` → true, `<`/`>` → false) and guards the rule with
`isNonFloatTy ty`, meant to exclude floats. For a comparison, `ty` is the instruction's RESULT type,
always `bool`, so the guard never excluded anything; the emitted function returned the constant 10
without comparing. The neighbouring self-cancellation rule (`x - x` → 0) is correct, because there
the result type is the operand type.

## Fix

The four self-comparison rules test the operand's type (`lhs.ty`). The reproducer exits 1 natively
and prints 1 under `--interp`. Permanent legs: `tests/programs/regress_139_float_self_compare.con`
(`run_ok` 1 in `run_tests.sh`), the reproducer in `run_tests.sh`'s interp-vs-native loop, and
`tests/codegen/float_semantics.con` in `check_codegen_differential.sh` (NaN `==`, `!=`, `<`, `>=`).

## Structural cause

Semantics implemented several times ([structural class](STRUCTURAL_CLASSES.md) 7): the SSA folder
carries its own copy of each comparison's meaning, separate from the emitter's choice of `fcmp`
predicates, and nothing checked the two agree on floats. The interpreter, the differential
reference, had no floats at all (bug 128's "float literals not yet supported"), so the
interp-vs-native gates could not see it: absence of coverage read as agreement (class 11).

## Structural owner

R-0493 (S6 one arithmetic authority for evaluation, folding and the model; its scope is integer
arithmetic today and should name float comparison too) with the differential harness R-0491
(exit 2c), which now carries float programs.
