# Bug 088 — an unhinted literal was an unchecked `Int`, and the constant folder compared it unwrapped

**Status:** FIXED (branch `bug088-literal-range`)
**Found:** 2026-10-18 by the model/interp/binary literal hunt (`hunt-model/biglit`, `hunt-model/lits`).
**Severity:** silent wrong code, three ways. The interpreter, the proof model and the binary computed
different values for the same accepted program, and the differential fuzzer never generated a
literal above `Int` max.

## Mechanism

**C1, the defaulted type had no range.** `TypeJudgment.intLitDecision` attached a range obligation only
when the hint was a concrete integer type. An unhinted literal (or one under a non-integer hint)
defaulted to `Int` with `range := none`, "matching the historical Check behavior", so Check's E0227
never fired. `fn g() -> i64 { let x = 18446744073709551615; return x; }` was accepted. The interpreter
and the proof model kept 2^64-1; the binary emitted `i64 18446744073709551615`, which LLVM truncates
to -1.

**C1', a literal at a type variable was range-checked by nobody.** `let y: T = 300` adopts `T` (no
range: the width is unknown), and Mono substitutes `T := u8` silently. `k::<u8>` returned 300 in the
interpreter (it runs the generic Core) and 44 in the binary.

**C2, the folder computed on unnormalized constants.** `0xFFFFFFFFFFFFFFFF as i64 < 0`: the operand
was the unchecked `Int` 2^64-1, so the cast was `Int → Int`. Identity-cast elimination
(`SSACleanup.eliminateIdentityCasts`) forwarded the operand unchanged, and `foldBinOpConst` compared
the mathematical 2^64-1 with 0. Result: `false` compiled, `true` interpreted (`evalCast` wraps with
`IntArith.wrapToType`). The folder had no cast fold at all; it only had the identity elimination, which
assumed every constant already held its type's value.

## Fixed

- **Every concrete literal type carries its range.** `intLitDecision` gives the defaulted `Int` the
  `Int` range, so an unhinted literal outside `Int` is E0227 in Check. Only a type-variable hint has no
  range at Check time.
- **Cast operands:** `TypeJudgment.castOperandHint`, shared by Check and Elab. A cast operand has no
  type context, so a bare literal defaults to `Int`, except a literal in `(Int max, Uint max]`, which is
  typed `Uint`. `0xFFFFFFFFFFFFFFFF as i64` is a well-typed `u64` reinterpreted: -1 in the
  interpreter, the folder and the binary. `0xFFFFFFFFFFFFFFFF` is valid wherever the type is `u64`
  (`let x: u64 = …`, `… as u64`). Anything wider than `Uint` is still E0227.
- **Post-mono literal range (`Verify.verifyLiteralRanges`, part of `verifyPostMono`).** Every literal
  in a monomorphized function or constant must fit its now-concrete type. A negated literal `-N` is
  checked as `-N`, so `-128 : i8` passes. The diagnostic is user-facing E0227 ("after
  monomorphization"). `--interp` now runs the same monomorphization and post-mono verification before
  interpreting, so both paths reject the program.
- **Folder normalization (`SSACleanup.normIntConst`, `foldCastConst`).** Every folded operand is read
  through `IntArith.wrapToType` at its own type. Integer-to-integer casts of constants now fold with the
  interpreter's wrap, applied to the normalized source value (so a signed source sign-extends and an
  unsigned source zero-extends). Identity-cast elimination forwards the normalized constant. The single
  exception is a negation's operand, which is read raw: `-128 : i8` lowers to `neg 128`, and the fold to
  -128 is what keeps the binary from trapping.

## Folder audit

- **Comparisons** (`eq/neq/lt/gt/leq/geq`) were the one fold computing on raw constants outside
  `IntArith`. They are now normalized, so unsigned and signed comparisons are correct by construction.
- **`+ - * / %`** fold through `IntArith.foldIntBinOp`. These were correct for normalized inputs and now
  always get normalized inputs.
- **Shifts and bitwise ops** are not value-folded. Only identities with zero are applied, and those
  read the normalized constant.
- **Unsigned div → shr strength reduction** reads the normalized divisor.
- **Casts:** see Fixed.

## Prevention

`check_cast_matrix.sh` gained two sections:

- 20 constant-expression rows. These are casts, signed and unsigned compares, unsigned
  division/remainder, logical and arithmetic shifts, and narrowing and widening chains. In each row the
  interpreter and the binary must both print the pinned value. With the old compiler, the
  `u64max as i64 < 0` row prints 0 compiled and 1 interpreted.
- 4 rejection rows (unhinted 2^64-1 and 2^63, a cast operand above `Uint` max, and a generic literal
  instantiated at `u8`). In each, the compiler and `--interp` must both refuse with E0227.

`tests/programs/regress_088_const_fold_casts.con` (run_ok `1048575`; the old compiler prints `1048574`)
and `error_088_unhinted_literal_range.con` (run_err) are the corpus fixtures.

## Limits

The proof model's treatment of a narrowing cast is bug 080's fix (`PExpr.wrapCast`, branch
`bug080-cast-model`), not this one. This fix guarantees that no literal reaches the model unchecked.
The interpreter still evaluates generic Core with type-variable tags, so arithmetic inside a generic
body is not width-checked by the interpreter. Out-of-range literals there are now rejected before it
runs, but checked overflow of in-range values at `T` is not covered by this fix. Hex literals do not
accept `_` separators (`0xFFFF_FFFF_FFFF_FFFF` is a parse error).
