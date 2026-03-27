# Bug 002: i32 Literal Type Mismatch in Binary Operations

**Status:** Fixed
**Discovered:** 2026-03-14
**Fixed:** 2026-03-14
**Regression test:** `lean_tests/bug_i32_literal_type.con`

## Symptom

`0 - a` where `a: i32` generates `sub i64 0, %i32_val` in LLVM IR — a type mismatch between i64 and i32 operands. Similarly, any integer literal used as the left operand of a binary operation with an i32 variable produces mismatched LLVM IR.

```
let a: i32 = 10;
let b: i32 = 0 - a;  // LLVM: sub i64 0, %i32_val  (crash or wrong result)
```

## Root Cause

**File:** `Concrete/Elab.lean`, `elabExpr` binOp case (line ~292)

Integer literals with no type hint default to `.int` (i64). In a binary operation `0 - a`:

1. LHS `0` is elaborated first with `hint=None` → type `.int` (i64)
2. RHS `a` is elaborated with `hint=some .int` → stays `.i32` (its declared type)
3. The result type is taken from the LHS → `.int` (i64)
4. `EmitSSA.emitBinOp` uses the LHS operand type (i64) for the LLVM instruction

This produced `sub i64 0, %i32_val` — an LLVM type mismatch.

## Fix

**File:** `Concrete/Elab.lean`

After elaborating both operands, check if one is a default-typed literal (Int) and the other has a concrete smaller integer type. If so, re-elaborate the literal with the concrete type as hint:

```lean
| .binOp _ op lhs rhs =>
    let cLhs ← elabExpr lhs hint
    let lTy := cLhs.ty
    let cRhs ← elabExpr rhs (some lTy)
    let rTy := cRhs.ty
    let (cLhs, cRhs, opTy) ← do
      let lhsIsDefaultInt := lTy == .int && isIntLit lhs
      let rhsIsDefaultInt := rTy == .int && isIntLit rhs
      if lhsIsDefaultInt && isIntegerType rTy && rTy != .int then do
        let cLhs' ← elabExpr lhs (some rTy)
        pure (cLhs', cRhs, rTy)
      else if rhsIsDefaultInt && isIntegerType lTy && lTy != .int then
        pure (cLhs, cRhs, lTy)
      else
        pure (cLhs, cRhs, lTy)
    ...
```

Also added `isIntLit` helper to detect integer literal expressions (including parenthesized ones).

## Impact

Any arithmetic expression with an integer literal and an i32/i16/i8/u32/u16/u8 variable where the literal appeared on the left side would produce incorrect LLVM IR. Common patterns: `0 - x` (negation), `1 + x`, `100 * x`. Workaround before fix: bind the literal to a typed variable first (`let zero: i32 = 0; zero - x`).
