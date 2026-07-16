import Concrete.Proof.Proof

/-!
# pure-core stdlib — slice 1 proofs (PURE_CORE_PROOF_ARC.md)

The FIRST stdlib (not example-flagship) proof links: theorems stated against
the registered spec PExprs (`Concrete.Proof.specs`), kernel-checked by
building this library, referenced from `std/src/*.con` via
`#[proof_by(...)]` + `#[proof_fingerprint(...)]`.

Evidence class: everything here is Lean-kernel `proved`. SMT-assisted facts
(none yet in this file) are reported `solver_checked` until replayed.
-/

namespace Examples.PureCore.Proofs

open Concrete.Proof

set_option linter.unusedSimpArgs false in
/-- `bytes.view` guard correctness against the abstract byte-sequence model:
    for ALL total/start/vlen/base, the view is `None` exactly when the
    H2 overflow-safe guard rejects (`start > total ∨ vlen > total - start`),
    and otherwise is `Some(BytesRaw { ptr = base + start, len = cap = vlen })`.
    The trusted pointer body is REFINED by this model: the model quantifies
    the decision and the produced view geometry; the raw pointer is `base`
    abstractly. -/
theorem bytes_view_guard_correct (total start vlen base : Int) (fuel : Nat) :
    eval pureCoreFns
      (((Env.empty.bind "total" (.int total)).bind "start" (.int start)).bind
        "vlen" (.int vlen) |>.bind "base" (.int base))
      (fuel + 6) bytesViewExpr
    = some (if start > total ∨ vlen > total - start
            then .enum_ "Option" "None" []
            else .enum_ "Option" "Some"
              [("value", .struct_ "BytesRaw"
                [ ("ptr", .int (base + start))
                , ("len", .int vlen)
                , ("cap", .int vlen) ])]) := by
  by_cases h1 : start > total
  · have hd1 : decide (total < start) = true := decide_eq_true h1
    simp [bytesViewExpr, eval, eval.evalFields, Env.bind, evalBinOp, hd1, h1]
  · have hd1 : decide (total < start) = false := decide_eq_false (by omega)
    by_cases h2 : vlen > total - start
    · have hd2 : decide (total - start < vlen) = true := decide_eq_true h2
      simp [bytesViewExpr, eval, eval.evalFields, Env.bind, evalBinOp, hd1, hd2, h1, h2]
    · have hd2 : decide (total - start < vlen) = false := decide_eq_false (by omega)
      simp [bytesViewExpr, eval, eval.evalFields, Env.bind, evalBinOp, hd1, hd2, h1, h2]

set_option linter.unusedSimpArgs false in
/-- `option.unwrap_or` — the FIRST fully-`proved` (non-trusted) stdlib API:
    `unwrap_or(Some v, d) = v` and `unwrap_or(None, d) = d`, universally
    quantified over payload and default. -/
theorem option_unwrap_or_correct (v d : Int) (fuel : Nat) :
    (eval pureCoreFns
      ((Env.empty.bind "self" (.enum_ "Option" "Some" [("value", .int v)])).bind
        "default" (.int d))
      (fuel + 4) optionUnwrapOrExpr = some (.int v))
    ∧
    (eval pureCoreFns
      ((Env.empty.bind "self" (.enum_ "Option" "None" [])).bind
        "default" (.int d))
      (fuel + 4) optionUnwrapOrExpr = some (.int d)) := by
  constructor <;>
    simp [optionUnwrapOrExpr, eval, eval.evalFields, eval.evalArms, eval.matchPat, eval.bindEnumFields, BEq.beq,
          Env.bind, evalBinOp]

set_option linter.unusedSimpArgs false in
/-- `option.map` structural laws for the registered representative callback
    `f(x) = 3x + 1`: `map(Some v, f) = Some (f v)` and `map(None, f) = None`,
    quantified over the payload. Callback quantification is a recorded model
    gap (coverage: representative). -/
theorem option_map_correct (v : Int) (fuel : Nat) :
    (eval pureCoreFns (Env.empty.bind "self" (.enum_ "Option" "Some" [("value", .int v)]))
      (fuel + 6) optionMapExpr
      = some (.enum_ "Option" "Some" [("value", .int (v * 3 + 1))]))
    ∧
    (eval pureCoreFns (Env.empty.bind "self" (.enum_ "Option" "None" []))
      (fuel + 6) optionMapExpr = some (.enum_ "Option" "None" [])) := by
  constructor <;>
    simp [optionMapExpr, pureCoreFns, pureCoreReprFFn, pureCoreReprFExpr,
          eval, eval.evalFields, eval.evalArms, eval.matchPat,
          eval.bindEnumFields, eval.evalArgs, bindArgs,
          Env.bind, evalBinOp, BEq.beq]

set_option linter.unusedSimpArgs false in
/-- `result.map` structural laws (representative callback): Ok maps, Err
    passes through untouched. -/
theorem result_map_correct (v e : Int) (fuel : Nat) :
    (eval pureCoreFns (Env.empty.bind "self" (.enum_ "Result" "Ok" [("value", .int v)]))
      (fuel + 6) resultMapExpr
      = some (.enum_ "Result" "Ok" [("value", .int (v * 3 + 1))]))
    ∧
    (eval pureCoreFns (Env.empty.bind "self" (.enum_ "Result" "Err" [("error", .int e)]))
      (fuel + 6) resultMapExpr
      = some (.enum_ "Result" "Err" [("error", .int e)])) := by
  constructor <;>
    simp [resultMapExpr, pureCoreFns, pureCoreReprFFn, pureCoreReprFExpr,
          eval, eval.evalFields, eval.evalArms, eval.matchPat,
          eval.bindEnumFields, eval.evalArgs, bindArgs,
          Env.bind, evalBinOp, BEq.beq]

set_option linter.unusedSimpArgs false in
/-- `result.map_err` structural laws (representative callback): Err maps,
    Ok passes through untouched. -/
theorem result_map_err_correct (v e : Int) (fuel : Nat) :
    (eval pureCoreFns (Env.empty.bind "self" (.enum_ "Result" "Ok" [("value", .int v)]))
      (fuel + 6) resultMapErrExpr
      = some (.enum_ "Result" "Ok" [("value", .int v)]))
    ∧
    (eval pureCoreFns (Env.empty.bind "self" (.enum_ "Result" "Err" [("error", .int e)]))
      (fuel + 6) resultMapErrExpr
      = some (.enum_ "Result" "Err" [("error", .int (e * 3 + 1))])) := by
  constructor <;>
    simp [resultMapErrExpr, pureCoreFns, pureCoreReprFFn, pureCoreReprFExpr,
          eval, eval.evalFields, eval.evalArms, eval.matchPat,
          eval.bindEnumFields, eval.evalArgs, bindArgs,
          Env.bind, evalBinOp, BEq.beq]

set_option linter.unusedSimpArgs false in
/-- `numeric` try_new (shared by NonZeroU32/NonZeroU64/Port): zero is
    rejected, any other value is wrapped — for ALL values, iff-shaped via
    the if on the RHS. The newtype constructor is a width-erased identity
    cast in the model. -/
theorem numeric_try_new_correct (v : Int) (fuel : Nat) :
    eval pureCoreFns (Env.empty.bind "value" (.int v)) (fuel + 4) numericTryNewExpr
    = some (if v = 0 then .enum_ "Option" "None" []
            else .enum_ "Option" "Some" [("value", .int v)]) := by
  by_cases h : v = 0
  · have hd : decide (v = 0) = true := decide_eq_true h
    simp [numericTryNewExpr, eval, eval.evalFields, Env.bind, evalBinOp, BEq.beq, hd, h]
  · have hd : decide (v = 0) = false := decide_eq_false h
    simp [numericTryNewExpr, eval, eval.evalFields, Env.bind, evalBinOp, BEq.beq, hd, h]

set_option linter.unusedSimpArgs false in
/-- `numeric` try_from (shared by NonZeroU32::try_from_u64 with
    max = u32::MAX and Port::try_from_u32 with max = u16::MAX): zero and
    out-of-range rejected, in-range wrapped. The narrowing cast is
    guard-dominated, so the identity-cast model is faithful on every
    reached path. -/
theorem numeric_try_from_correct (max v : Int) (fuel : Nat) :
    eval pureCoreFns (Env.empty.bind "value" (.int v)) (fuel + 6) (numericTryFromExpr max)
    = some (if v = 0 ∨ v > max then .enum_ "Option" "None" []
            else .enum_ "Option" "Some" [("value", .int v)]) := by
  by_cases h0 : v = 0
  · have hd0 : decide (v = 0) = true := decide_eq_true h0
    simp [numericTryFromExpr, eval, eval.evalFields, Env.bind, evalBinOp, BEq.beq, hd0, h0]
  · have hd0 : decide (v = 0) = false := decide_eq_false h0
    by_cases h1 : v > max
    · have hd1 : decide (max < v) = true := decide_eq_true h1
      simp [numericTryFromExpr, eval, eval.evalFields, Env.bind, evalBinOp, BEq.beq, hd0, hd1, h0, h1]
    · have hd1 : decide (max < v) = false := decide_eq_false (by omega)
      simp [numericTryFromExpr, eval, eval.evalFields, Env.bind, evalBinOp, BEq.beq, hd0, hd1, h0, h1]

/-- NonZeroU32::try_from_u64 — `numeric_try_from_correct` at max = u32::MAX. -/
theorem nonzero_u32_try_from_u64_correct (v : Int) (fuel : Nat) :
    eval pureCoreFns (Env.empty.bind "value" (.int v)) (fuel + 6)
      (numericTryFromExpr 4294967295)
    = some (if v = 0 ∨ v > 4294967295 then .enum_ "Option" "None" []
            else .enum_ "Option" "Some" [("value", .int v)]) :=
  numeric_try_from_correct 4294967295 v fuel

/-- Port::try_from_u32 — `numeric_try_from_correct` at max = u16::MAX. -/
theorem port_try_from_u32_correct (v : Int) (fuel : Nat) :
    eval pureCoreFns (Env.empty.bind "value" (.int v)) (fuel + 6)
      (numericTryFromExpr 65535)
    = some (if v = 0 ∨ v > 65535 then .enum_ "Option" "None" []
            else .enum_ "Option" "Some" [("value", .int v)]) :=
  numeric_try_from_correct 65535 v fuel

/-- H1 radix-overflow guard STEP fact (kernel decision, no solver needed):
    if the pre-shift guard accepts (`acc ≤ (2^64-1) >> 4`) then one hex
    accumulation step stays within u64 — the loop invariant's inductive
    step, NOT whole-loop parser correctness (recorded scope limit). -/
theorem hex_guard_step_preserves_u64 (acc val : Int)
    (hguard : acc ≤ 1152921504606846975) (hval : 0 ≤ val ∧ val ≤ 15) :
    acc * 16 + val ≤ 18446744073709551615 := by omega

end Examples.PureCore.Proofs
