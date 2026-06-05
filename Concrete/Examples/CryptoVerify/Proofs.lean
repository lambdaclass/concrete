import Concrete.Proof

/-!
# crypto_verify — example proofs

Proof theorems for the `crypto_verify` flagship (toy authenticated-tag model),
moved out of the `Concrete.Proof` compiler namespace into this per-example
namespace. The registered spec PExprs (`computeTagExpr` / `verifyTagExpr` /
`checkNonceExpr` / `verifyMessageExpr`) and the `cryptoFns` / `*Fn` eval
scaffolding stay in `Concrete.Proof` (they are the spec-drift oracle). Only the
proof theorems move.
-/

namespace Examples.CryptoVerify.Proofs

open Concrete.Proof

/-- compute_tag is key * message + nonce for all integers. -/
theorem compute_tag_correct (key message nonce : Int) (fuel : Nat) :
    eval cryptoFns
      (((Env.empty.bind "key" (.int key)).bind "message" (.int message)).bind "nonce" (.int nonce))
      (fuel + 1) computeTagExpr
    = some (.int (key * message + nonce)) := by
  simp [computeTagExpr, eval, Env.bind, evalBinOp]

/-- verify_tag returns 1 when the expected tag equals key * message + nonce.
    This is the MAC verification correctness property: if verify succeeds,
    the tag authenticates the message under the given key. -/
theorem verify_tag_correct (key message nonce : Int) (fuel : Nat) :
    let env := (((Env.empty.bind "key" (.int key)).bind "message" (.int message)).bind "nonce" (.int nonce)).bind "expected_tag" (.int (key * message + nonce))
    eval cryptoFns env (fuel + 6) verifyTagExpr = some (.int 1) := by
  simp [verifyTagExpr, eval, eval.evalArgs, cryptoFns, computeTagFn, computeTagExpr,
        Env.bind, evalBinOp, bindArgs]

/-- verify_tag returns 0 when the expected tag does not match.
    This is the forgery rejection property: a wrong tag is always detected. -/
theorem verify_tag_rejects (key message nonce expected : Int)
    (h : expected ≠ key * message + nonce) (fuel : Nat) :
    let env := (((Env.empty.bind "key" (.int key)).bind "message" (.int message)).bind "nonce" (.int nonce)).bind "expected_tag" (.int expected)
    eval cryptoFns env (fuel + 6) verifyTagExpr = some (.int 0) := by
  have hne : (key * message + nonce == expected) = false := by
    show decide (key * message + nonce = expected) = false
    exact decide_eq_false (Ne.symm h)
  simp [verifyTagExpr, eval, eval.evalArgs, cryptoFns, computeTagFn, computeTagExpr,
        Env.bind, evalBinOp, bindArgs, hne]

/-- check_nonce returns 1 for nonces in range [1, max_nonce]. -/
theorem check_nonce_accepts_valid (nonce maxNonce : Int)
    (hpos : 0 < nonce) (hmax : nonce ≤ maxNonce) (fuel : Nat) :
    eval cryptoFns
      ((Env.empty.bind "nonce" (.int nonce)).bind "max_nonce" (.int maxNonce))
      (fuel + 3) checkNonceExpr
    = some (.int 1) := by
  have hgt : decide (0 < nonce) = true := decide_eq_true hpos
  have hle : decide (nonce ≤ maxNonce) = true := decide_eq_true hmax
  simp [checkNonceExpr, eval, Env.bind, evalBinOp, hgt, hle]

/-- check_nonce returns 0 for non-positive nonces. -/
theorem check_nonce_rejects_nonpositive (nonce maxNonce : Int)
    (h : nonce ≤ 0) (fuel : Nat) :
    eval cryptoFns
      ((Env.empty.bind "nonce" (.int nonce)).bind "max_nonce" (.int maxNonce))
      (fuel + 2) checkNonceExpr
    = some (.int 0) := by
  have hgt : decide (0 < nonce) = false := decide_eq_false (by omega)
  simp [checkNonceExpr, eval, Env.bind, evalBinOp, hgt]

/-- check_nonce returns 0 for nonces exceeding the maximum. -/
theorem check_nonce_rejects_over_max (nonce maxNonce : Int)
    (hpos : 0 < nonce) (hover : maxNonce < nonce) (fuel : Nat) :
    eval cryptoFns
      ((Env.empty.bind "nonce" (.int nonce)).bind "max_nonce" (.int maxNonce))
      (fuel + 3) checkNonceExpr
    = some (.int 0) := by
  have hgt : decide (0 < nonce) = true := decide_eq_true hpos
  have hle : decide (nonce ≤ maxNonce) = false := decide_eq_false (by omega)
  simp [checkNonceExpr, eval, Env.bind, evalBinOp, hgt, hle]

set_option linter.unusedSimpArgs false in
/-- Composition theorem for verify_message (success direction).

    A message+tag pair is acceptable iff:
      1. the tag matches under the given key   (verify_tag → 1)
      2. the nonce is in the accepted range    (check_nonce → 1)

    This theorem proves that when both component validators succeed,
    the composed verify_message returns 1. The component theorems
    (verify_tag_correct, check_nonce_accepts_valid) carry the
    individual properties; this theorem chains them.

    The failure direction (returns 0 if either sub-check fails) is
    follow-up — same per-failure pattern as parse_validate. -/
theorem verify_message_composed_correct
    (key message nonce maxNonce : Int) (fuel : Nat)
    (h_nonce_pos : 0 < nonce) (h_nonce_max : nonce ≤ maxNonce) :
    let env := (((((Env.empty.bind "key" (.int key)).bind
                    "message" (.int message)).bind
                    "nonce" (.int nonce)).bind
                    "expected_tag" (.int (key * message + nonce))).bind
                    "max_nonce" (.int maxNonce))
    eval cryptoFns env (fuel + 20) verifyMessageExpr = some (.int 1) := by
  have hgt : decide (0 < nonce) = true := decide_eq_true h_nonce_pos
  have hle : decide (nonce ≤ maxNonce) = true := decide_eq_true h_nonce_max
  simp_all [verifyMessageExpr,
            verifyTagFn, verifyTagExpr,
            checkNonceFn, checkNonceExpr,
            computeTagFn, computeTagExpr,
            eval, eval.evalArgs, cryptoFns,
            Env.bind, evalBinOp, bindArgs, BEq.beq]

/-- Full contract for check_nonce: returns 1 iff nonce ∈ [1, max_nonce], 0 otherwise.
    This is the theorem attached in the proof registry for main.check_nonce. -/
theorem check_nonce_correct (nonce maxNonce : Int) (fuel : Nat) :
    eval cryptoFns
      ((Env.empty.bind "nonce" (.int nonce)).bind "max_nonce" (.int maxNonce))
      (fuel + 3) checkNonceExpr
    = some (.int (if 0 < nonce ∧ nonce ≤ maxNonce then 1 else 0)) := by
  by_cases hpos : 0 < nonce
  · by_cases hle : nonce ≤ maxNonce
    · have hgt : decide (0 < nonce) = true := decide_eq_true hpos
      have hle' : decide (nonce ≤ maxNonce) = true := decide_eq_true hle
      have hboth : 0 < nonce ∧ nonce ≤ maxNonce := ⟨hpos, hle⟩
      simp [checkNonceExpr, eval, Env.bind, evalBinOp, hboth]
    · have hgt : decide (0 < nonce) = true := decide_eq_true hpos
      have hle' : decide (nonce ≤ maxNonce) = false := decide_eq_false hle
      have hnboth : ¬(0 < nonce ∧ nonce ≤ maxNonce) := fun h => hle h.2
      simp [checkNonceExpr, eval, Env.bind, evalBinOp, hgt, hle', hnboth]
  · have hgt : decide (0 < nonce) = false := decide_eq_false hpos
    have hnboth : ¬(0 < nonce ∧ nonce ≤ maxNonce) := fun h => hpos h.1
    simp [checkNonceExpr, eval, Env.bind, evalBinOp, hgt, hnboth]
end Examples.CryptoVerify.Proofs
