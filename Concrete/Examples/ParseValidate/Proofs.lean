import Concrete.Proof

/-!
# parse_validate — example proofs

The proof THEOREMS for the `parse_validate` flagship, moved out of the
`Concrete.Proof` compiler namespace into this per-example namespace
(`Examples.ParseValidate.Proofs`).

What stays in `Concrete.Proof` (by policy — these are compiler-owned, not
example cruft):
  * the registered spec PExprs `validateVersionExpr` / `validateHeaderFieldsExpr`
    / `parseHeaderExpr` — they are entries in `Concrete.Proof.specs`, the
    spec-drift oracle the compiler consumes;
  * the eval scaffolding (`parseValidateFns`, the `*Fn` defs, the helper
    `*Expr`s) these theorems evaluate against.
Only the user-facing proof theorems (the `#[proof_by(...)]` targets) move.
-/

namespace Examples.ParseValidate.Proofs

open Concrete.Proof

/-- validate_version returns 0 iff v == 1, and 1 otherwise. -/
theorem validate_version_correct (v : Int) (fuel : Nat) :
    eval parseValidateFns (Env.empty.bind "v" (.int v)) (fuel + 2) validateVersionExpr
    = some (.int (if v = 1 then 0 else 1)) := by
  by_cases h : v = 1 <;>
    simp_all [validateVersionExpr, eval, Env.bind, evalBinOp, BEq.beq]

set_option linter.unusedSimpArgs false in
/-- validate_header_fields composition theorem (success direction).

    The central thesis claim of the parse_validate pilot, at the scope
    ProofCore can currently extract: "successful return implies multiple
    structural invariants." Stated as the *contrapositive* — when every
    structural precondition holds, the composition returns 0.

    The function returns 0 iff every component validator succeeds. This
    theorem proves the success direction (preconditions ⟹ returns 0).
    The full iff (failure direction with return code matching the first
    failing check) is a 256-branch case split that exhausts heartbeats;
    splitting it into per-failure theorems is follow-up work. -/
theorem validate_header_fields_success
    (v t plen total_len cs_expected cs_computed : Int) (fuel : Nat)
    (h_tl5 : total_len ≥ 5) (h_v : v = 1)
    (h_t1 : t ≥ 1) (h_t4 : t ≤ 4)
    (h_p0 : plen ≥ 0) (h_p240 : plen ≤ 240)
    (h_tlp : total_len ≥ 4 + plen) (h_cs : cs_expected = cs_computed) :
    eval parseValidateFns
      (((((Env.empty.bind "v" (.int v)).bind "t" (.int t)).bind "plen" (.int plen)).bind
        "total_len" (.int total_len)).bind "cs_expected" (.int cs_expected) |>.bind
        "cs_computed" (.int cs_computed))
      (fuel + 20) validateHeaderFieldsExpr
    = some (.int 0) := by
  -- Substitute the equalities so v becomes 1 and cs_expected becomes cs_computed.
  subst h_v
  subst h_cs
  simp_all [validateHeaderFieldsExpr,
            validateVersionFn, validateVersionExpr,
            validateMsgTypeFn, validateMsgTypeExpr,
            validatePayloadLenFn, validatePayloadLenExpr,
            validateTotalLenFn, validateTotalLenExpr,
            validateChecksumFn, validateChecksumExpr,
            eval, eval.evalArgs, parseValidateFns,
            Env.bind, evalBinOp, bindArgs, BEq.beq]

set_option linter.unusedSimpArgs false in
/-- parse_header returns `Err(TooShort)` whenever `len < 5`.

    First attached theorem on the actual `parse_header` function (not
    the scalar-parameter scaffold `validate_header_fields`). The
    failure-path theorems are the ones we can prove today without
    modelling `compute_checksum` (whose body uses a while loop, a
    ProofCore subgoal still pending). The success-path theorem will
    follow either bounded-while-loop extraction or a hand-modelled
    compute_checksum convention. -/
theorem parse_header_too_short
    (data : List PVal) (len : Int) (fuel : Nat) (h : len < 5) :
    eval parseValidateFns
      ((Env.empty.bind "data" (.array_ data)).bind "len" (.int len))
      (fuel + 10) parseHeaderExpr
    = some (.enum_ "Result" "Err"
        [("error", .enum_ "ParseError" "TooShort" [])]) := by
  -- validate_total_len(len, 5) = 1 when len < 5, by its definition.
  have h_dec : decide (5 ≤ len) = false := decide_eq_false (by omega)
  simp [parseHeaderExpr, errResultExpr,
        validateTotalLenFn, validateTotalLenExpr,
        eval, eval.evalArgs, eval.evalFields, parseValidateFns,
        Env.bind, evalBinOp, bindArgs, BEq.beq, h_dec]

-- The remaining failure-path theorems all share the same shape:
-- bind data to an array whose first few elements satisfy enough
-- preconditions for the earlier validators to pass, but whose
-- current field violates the validator under test. Each theorem
-- targets one specific Err variant.

set_option linter.unusedSimpArgs false in
/-- parse_header returns `Err(BadVersion)` when `data[0] ≠ 1`
    (and `len ≥ 5` so the length check passes first). -/
theorem parse_header_bad_version
    (v : Int) (rest : List PVal) (len : Int) (fuel : Nat)
    (h_len : len ≥ 5) (h_v : v ≠ 1) :
    eval parseValidateFns
      ((Env.empty.bind "data" (.array_ (.int v :: rest))).bind "len" (.int len))
      (fuel + 15) parseHeaderExpr
    = some (.enum_ "Result" "Err"
        [("error", .enum_ "ParseError" "BadVersion" [])]) := by
  have h_len_dec : decide (5 ≤ len) = true := decide_eq_true h_len
  have h_v_dec  : decide (v = 1) = false := decide_eq_false h_v
  simp [parseHeaderExpr, errResultExpr,
        validateTotalLenFn, validateTotalLenExpr,
        validateVersionFn, validateVersionExpr,
        eval, eval.evalArgs, eval.evalFields, eval.lookupIndex,
        parseValidateFns,
        Env.bind, evalBinOp, bindArgs, BEq.beq, h_len_dec, h_v_dec]

set_option linter.unusedSimpArgs false in
/-- parse_header returns `Err(BadType)` when `data[1] ∉ [1, 4]`
    (and earlier checks pass: `len ≥ 5`, `data[0] = 1`). -/
theorem parse_header_bad_type
    (t : Int) (rest : List PVal) (len : Int) (fuel : Nat)
    (h_len : len ≥ 5) (h_t : t < 1 ∨ t > 4) :
    eval parseValidateFns
      ((Env.empty.bind "data" (.array_ (.int 1 :: .int t :: rest))).bind "len" (.int len))
      (fuel + 15) parseHeaderExpr
    = some (.enum_ "Result" "Err"
        [("error", .enum_ "ParseError" "BadType" [])]) := by
  have h_len_dec : decide (5 ≤ len) = true := decide_eq_true h_len
  -- t < 1 or t > 4 ⟹ validate_msg_type returns 1.
  -- Expressed concretely: ¬ (1 ≤ t) ∨ ¬ (t ≤ 4).
  have h_t_low  : ¬ (1 ≤ t ∧ t ≤ 4) := by omega
  simp [parseHeaderExpr, errResultExpr,
        validateTotalLenFn, validateTotalLenExpr,
        validateVersionFn, validateVersionExpr,
        validateMsgTypeFn, validateMsgTypeExpr,
        eval, eval.evalArgs, eval.evalFields, eval.lookupIndex,
        parseValidateFns,
        Env.bind, evalBinOp, bindArgs, BEq.beq, h_len_dec]
  -- Two sub-cases on whether t < 1 or t > 4.
  rcases h_t with h_lo | h_hi
  · have hd1 : decide (1 ≤ t) = false := decide_eq_false (by omega)
    simp [hd1]
  · have hd2 : decide (t ≤ 4) = false := decide_eq_false (by omega)
    have hd1 : decide (1 ≤ t) = true  := decide_eq_true (by omega)
    simp [hd1, hd2]

set_option linter.unusedSimpArgs false in
/-- parse_header returns `Err(PayloadTooBig)` when `data[2] < 0` or
    `data[2] > 240` (earlier checks pass: `len ≥ 5`, `data[0] = 1`,
    `data[1] ∈ [1, 4]`). -/
theorem parse_header_payload_too_big
    (plen : Int) (rest : List PVal) (len : Int) (fuel : Nat)
    (h_len : len ≥ 5) (h_plen : plen < 0 ∨ plen > 240) :
    eval parseValidateFns
      ((Env.empty.bind "data" (.array_ (.int 1 :: .int 1 :: .int plen :: rest))).bind "len" (.int len))
      (fuel + 15) parseHeaderExpr
    = some (.enum_ "Result" "Err"
        [("error", .enum_ "ParseError" "PayloadTooBig" [])]) := by
  have h_len_dec : decide (5 ≤ len) = true := decide_eq_true h_len
  simp [parseHeaderExpr, errResultExpr,
        validateTotalLenFn, validateTotalLenExpr,
        validateVersionFn, validateVersionExpr,
        validateMsgTypeFn, validateMsgTypeExpr,
        validatePayloadLenFn, validatePayloadLenExpr,
        eval, eval.evalArgs, eval.evalFields, eval.lookupIndex,
        parseValidateFns,
        Env.bind, evalBinOp, bindArgs, BEq.beq, h_len_dec]
  rcases h_plen with h_lo | h_hi
  · have hd0 : decide (0 ≤ plen) = false := decide_eq_false (by omega)
    simp [hd0]
  · have hd240 : decide (plen ≤ 240) = false := decide_eq_false (by omega)
    have hd0   : decide (0 ≤ plen) = true  := decide_eq_true (by omega)
    simp [hd0, hd240]

set_option linter.unusedSimpArgs false in
/-- parse_header returns `Err(Truncated)` when `len < 4 + data[2]`
    (and earlier checks pass: validation up through payload_len). -/
theorem parse_header_truncated
    (plen : Int) (rest : List PVal) (len : Int) (fuel : Nat)
    (h_len_5 : len ≥ 5) (h_plen_lo : 0 ≤ plen) (h_plen_hi : plen ≤ 240)
    (h_trunc : len < 4 + plen) :
    eval parseValidateFns
      ((Env.empty.bind "data" (.array_ (.int 1 :: .int 1 :: .int plen :: rest))).bind "len" (.int len))
      (fuel + 20) parseHeaderExpr
    = some (.enum_ "Result" "Err"
        [("error", .enum_ "ParseError" "Truncated" [])]) := by
  have h_len_dec   : decide (5 ≤ len) = true := decide_eq_true h_len_5
  have h_plen_lo_d : decide (0 ≤ plen) = true := decide_eq_true h_plen_lo
  have h_plen_hi_d : decide (plen ≤ 240) = true := decide_eq_true h_plen_hi
  have h_trunc_d   : decide (4 + plen ≤ len) = false := decide_eq_false (by omega)
  simp [parseHeaderExpr, errResultExpr,
        validateTotalLenFn, validateTotalLenExpr,
        validateVersionFn, validateVersionExpr,
        validateMsgTypeFn, validateMsgTypeExpr,
        validatePayloadLenFn, validatePayloadLenExpr,
        eval, eval.evalArgs, eval.evalFields, eval.lookupIndex,
        parseValidateFns,
        Env.bind, evalBinOp, bindArgs, BEq.beq,
        h_len_dec, h_plen_lo_d, h_plen_hi_d, h_trunc_d]
end Examples.ParseValidate.Proofs
