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

/-- Semantic mirror of `base64.char_of` — the RFC 4648 alphabet as a plain
    Int function (guard-shaped, so the eval theorem's cases align 1:1). -/
def base64CharOfSem (v : Int) : Int :=
  if v < 26 then 65 + v
  else if v < 52 then 97 + (v - 26)
  else if v < 62 then 48 + (v - 52)
  else if v = 62 then 43
  else 47

/-- Semantic mirrors of `base64.val_of`'s continuation levels (the source's
    fall-through guard pairs duplicate each continuation — mirrored here so
    each eval lemma is 3 cases, not a 31-leaf tree). -/
def base64ValOfSemK3 (c : Int) : Int :=
  if c = 43 then 62 else if c = 47 then 63 else 255

def base64ValOfSemK2 (c : Int) : Int :=
  if 48 ≤ c then (if c ≤ 57 then c - 48 + 52 else base64ValOfSemK3 c)
  else base64ValOfSemK3 c

def base64ValOfSemK1 (c : Int) : Int :=
  if 97 ≤ c then (if c ≤ 122 then c - 97 + 26 else base64ValOfSemK2 c)
  else base64ValOfSemK2 c

def base64ValOfSem (c : Int) : Int :=
  if 65 ≤ c then (if c ≤ 90 then c - 65 else base64ValOfSemK1 c)
  else base64ValOfSemK1 c

set_option linter.unusedSimpArgs false in
/-- `base64.char_of` computes the RFC 4648 alphabet byte — total over v. -/
theorem base64_char_of_correct (v : Int) (fuel : Nat) :
    eval pureCoreFns (Env.empty.bind "v" (.int v)) (fuel + 8) base64CharOfExpr
    = some (.int (base64CharOfSem v)) := by
  unfold base64CharOfSem
  by_cases h1 : v < 26
  · have hd1 : decide (v < 26) = true := decide_eq_true h1
    simp [base64CharOfExpr, eval, Env.bind, evalBinOp, BEq.beq, hd1, h1]
  · have hd1 : decide (v < 26) = false := decide_eq_false h1
    by_cases h2 : v < 52
    · have hd2 : decide (v < 52) = true := decide_eq_true h2
      simp [base64CharOfExpr, eval, Env.bind, evalBinOp, BEq.beq, hd1, hd2, h1, h2]
    · have hd2 : decide (v < 52) = false := decide_eq_false h2
      by_cases h3 : v < 62
      · have hd3 : decide (v < 62) = true := decide_eq_true h3
        simp [base64CharOfExpr, eval, Env.bind, evalBinOp, BEq.beq, hd1, hd2, hd3, h1, h2, h3]
      · have hd3 : decide (v < 62) = false := decide_eq_false h3
        by_cases h4 : v = 62
        · have hd4 : decide (v = 62) = true := decide_eq_true h4
          simp [base64CharOfExpr, eval, Env.bind, evalBinOp, BEq.beq, hd1, hd2, hd3, hd4, h1, h2, h3, h4]
        · have hd4 : decide (v = 62) = false := decide_eq_false h4
          simp [base64CharOfExpr, eval, Env.bind, evalBinOp, BEq.beq, hd1, hd2, hd3, hd4, h1, h2, h3, h4]

set_option linter.unusedSimpArgs false in
private theorem base64_val_of_k3 (c : Int) (fuel : Nat) :
    eval pureCoreFns (Env.empty.bind "c" (.int c)) (fuel + 4) base64ValOfK3Expr
    = some (.int (base64ValOfSemK3 c)) := by
  unfold base64ValOfSemK3
  by_cases h1 : c = 43
  · have hd1 : decide (c = 43) = true := decide_eq_true h1
    simp [base64ValOfK3Expr, eval, Env.bind, evalBinOp, BEq.beq, hd1, h1]
  · have hd1 : decide (c = 43) = false := decide_eq_false h1
    by_cases h2 : c = 47
    · have hd2 : decide (c = 47) = true := decide_eq_true h2
      simp [base64ValOfK3Expr, eval, Env.bind, evalBinOp, BEq.beq, hd1, hd2, h1, h2]
    · have hd2 : decide (c = 47) = false := decide_eq_false h2
      simp [base64ValOfK3Expr, eval, Env.bind, evalBinOp, BEq.beq, hd1, hd2, h1, h2]

set_option linter.unusedSimpArgs false in
private theorem base64_val_of_k2 (c : Int) (fuel : Nat) :
    eval pureCoreFns (Env.empty.bind "c" (.int c)) (fuel + 8) base64ValOfK2Expr
    = some (.int (base64ValOfSemK2 c)) := by
  unfold base64ValOfSemK2
  by_cases h1 : 48 ≤ c
  · have hd1 : decide (c ≥ 48) = true := decide_eq_true h1
    by_cases h2 : c ≤ 57
    · have hd2 : decide (c ≤ 57) = true := decide_eq_true h2
      simp [base64ValOfK2Expr, eval, Env.bind, evalBinOp, BEq.beq, hd1, hd2, h1, h2]
    · have hd2 : decide (c ≤ 57) = false := decide_eq_false h2
      simpa [base64ValOfK2Expr, eval, Env.bind, evalBinOp, BEq.beq, hd1, hd2, h1, h2]
        using base64_val_of_k3 c (fuel + 2)
  · have hd1 : decide (c ≥ 48) = false := decide_eq_false h1
    simpa [base64ValOfK2Expr, eval, Env.bind, evalBinOp, BEq.beq, hd1, h1]
      using base64_val_of_k3 c (fuel + 3)

set_option linter.unusedSimpArgs false in
private theorem base64_val_of_k1 (c : Int) (fuel : Nat) :
    eval pureCoreFns (Env.empty.bind "c" (.int c)) (fuel + 12) base64ValOfK1Expr
    = some (.int (base64ValOfSemK1 c)) := by
  unfold base64ValOfSemK1
  by_cases h1 : 97 ≤ c
  · have hd1 : decide (c ≥ 97) = true := decide_eq_true h1
    by_cases h2 : c ≤ 122
    · have hd2 : decide (c ≤ 122) = true := decide_eq_true h2
      simp [base64ValOfK1Expr, eval, Env.bind, evalBinOp, BEq.beq, hd1, hd2, h1, h2]
    · have hd2 : decide (c ≤ 122) = false := decide_eq_false h2
      simpa [base64ValOfK1Expr, eval, Env.bind, evalBinOp, BEq.beq, hd1, hd2, h1, h2]
        using base64_val_of_k2 c (fuel + 2)
  · have hd1 : decide (c ≥ 97) = false := decide_eq_false h1
    simpa [base64ValOfK1Expr, eval, Env.bind, evalBinOp, BEq.beq, hd1, h1]
      using base64_val_of_k2 c (fuel + 3)

set_option linter.unusedSimpArgs false in
/-- `base64.val_of` computes the RFC 4648 alphabet inverse, and 255 rejects
    every byte outside the alphabet — total over c. -/
theorem base64_val_of_correct (c : Int) (fuel : Nat) :
    eval pureCoreFns (Env.empty.bind "c" (.int c)) (fuel + 16) base64ValOfExpr
    = some (.int (base64ValOfSem c)) := by
  unfold base64ValOfSem
  by_cases h1 : 65 ≤ c
  · have hd1 : decide (c ≥ 65) = true := decide_eq_true h1
    by_cases h2 : c ≤ 90
    · have hd2 : decide (c ≤ 90) = true := decide_eq_true h2
      simp [base64ValOfExpr, eval, Env.bind, evalBinOp, BEq.beq, hd1, hd2, h1, h2]
    · have hd2 : decide (c ≤ 90) = false := decide_eq_false h2
      simpa [base64ValOfExpr, eval, Env.bind, evalBinOp, BEq.beq, hd1, hd2, h1, h2]
        using base64_val_of_k1 c (fuel + 2)
  · have hd1 : decide (c ≥ 65) = false := decide_eq_false h1
    simpa [base64ValOfExpr, eval, Env.bind, evalBinOp, BEq.beq, hd1, h1]
      using base64_val_of_k1 c (fuel + 3)

set_option linter.unusedSimpArgs false in
/-- Alphabet ROUNDTRIP (the conversion contract the decoder relies on):
    every 6-bit value survives encode-then-decode. Pure Int fact over the
    semantic mirrors, so it composes with the two eval theorems. -/
theorem base64_alphabet_roundtrip (v : Int) (h0 : 0 ≤ v) (h63 : v ≤ 63) :
    base64ValOfSem (base64CharOfSem v) = v := by
  unfold base64CharOfSem base64ValOfSem base64ValOfSemK1 base64ValOfSemK2 base64ValOfSemK3
  by_cases h1 : v < 26
  · simp [h1]; omega
  · by_cases h2 : v < 52
    · simp [h1, h2]; omega
    · by_cases h3 : v < 62
      · simp [h1, h2, h3]; omega
      · by_cases h4 : v = 62
        · simp [h1, h2, h3, h4]
        · simp [h1, h2, h3, h4]; omega

/-- H1 radix-overflow guard STEP fact (kernel decision, no solver needed):
    if the pre-shift guard accepts (`acc ≤ (2^64-1) >> 4`) then one hex
    accumulation step stays within u64 — the loop invariant's inductive
    step, NOT whole-loop parser correctness (recorded scope limit). -/
theorem hex_guard_step_preserves_u64 (acc val : Int)
    (hguard : acc ≤ 1152921504606846975) (hval : 0 ≤ val ∧ val ≤ 15) :
    acc * 16 + val ≤ 18446744073709551615 := by omega

end Examples.PureCore.Proofs
