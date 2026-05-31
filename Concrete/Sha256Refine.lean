/-
  Concrete.Sha256Refine — the first eval ↔ spec REFINEMENT theorems.

  Where `Concrete.Proof` characterises the *extracted* IR (e.g. point
  facts such as `ch_selects_high`), and `Concrete.Sha256Spec` gives an
  independent, `BitVec`-valued FIPS 180-4 specification, this module
  closes the gap: it proves that the extracted source expressions
  *refine* the spec — for ALL inputs, not just witnesses.

  This is the HACL*/seL4 discipline (docs/PROOF_LADDER.md): the payoff
  is not "the program evaluates to these bytes" but "the program
  computes the mathematical function the spec names."

  The word-level core of each theorem is discharged by `bv_decide`
  (`Std.Tactic.BVDecide`): it bit-blasts to SAT and the Lean kernel
  re-checks an LRAT certificate, so this is the *kernel-checked decision*
  tier — automation with NO trusted-base growth, distinct from any
  external SMT/solver-trusted path. This module is its first real use.

  Scope (task #17): the SHA-256 Boolean round functions `ch` and `maj`,
  which are exactly the round functions expressed over the forced u32
  bitwise surface (R-22 `bitand` + u32 `bitxor`) with no shift-amount
  arithmetic. The rotation-based functions (`rotr`, the sigmas) and the
  message schedule additionally involve a `32 - n` shift amount and
  `rotr` *calls*, and are refined alongside the schedule/round work
  (#20), where that machinery is built.
-/
import Std.Tactic.BVDecide
import Concrete.Proof
import Concrete.Sha256Spec

namespace Concrete.Proof

-- ------------------------------------------------------------------
-- Bridging lemmas: a 32-bit word survives the round-trip through the
-- evaluator's `Int` representation. The evaluator binds a `u32` value
-- as an `Int` and re-reads it with `BitVec.ofInt 32`; both the env
-- binding (`Nat.cast`) and the per-op result reconstruction
-- (`Int.ofNat`) wrap a `BitVec.toNat`, and both collapse to identity.
-- ------------------------------------------------------------------

/-- Round-trip through `Nat.cast` (how `eval` binds a `u32` argument). -/
theorem ofInt_natCast_toNat (W : BitVec 32) :
    BitVec.ofInt 32 (W.toNat : Int) = W := by
  apply BitVec.eq_of_toNat_eq; simp

/-- Round-trip through `Int.ofNat` (how `evalBinOp` reconstructs a u32
    bitwise result). -/
theorem ofInt_ofNat_toNat (W : BitVec 32) :
    BitVec.ofInt 32 (Int.ofNat W.toNat) = W := by
  apply BitVec.eq_of_toNat_eq; simp

-- ------------------------------------------------------------------
-- Extracted round-function expressions
-- ------------------------------------------------------------------

/-- Extracted spec for `hmac_sha256.maj`:
    `(x AND y) XOR (x AND z) XOR (y AND z)` (FIPS 180-4 § 4.1.2),
    left-associated as the source `^` is. -/
def majExpr : PExpr :=
  .binOp (.bitxor 32 false)
    (.binOp (.bitxor 32 false)
      (.binOp (.bitand 32 false) (.var "x") (.var "y"))
      (.binOp (.bitand 32 false) (.var "x") (.var "z")))
    (.binOp (.bitand 32 false) (.var "y") (.var "z"))

-- ------------------------------------------------------------------
-- Refinement theorems (kernel-checked decision tier)
-- ------------------------------------------------------------------

set_option linter.unusedSimpArgs false in
/-- `chExpr` refines `Sha256Spec.ch`: for ALL words `X Y Z`, evaluating
    the extracted `Ch` expression yields exactly the spec value. The
    first eval ↔ spec refinement; the word-level identity
    `(X∧Y) ⊕ ((X⊕1ⁿ)∧Z) = (X∧Y) ⊕ (¬X∧Z)` is closed by `bv_decide`. -/
theorem ch_refines (X Y Z : BitVec 32) (fuel : Nat) :
    eval (fun _ => none)
      (((Env.empty.bind "x" (.int X.toNat)).bind "y" (.int Y.toNat)).bind
        "z" (.int Z.toNat))
      (fuel + 1) chExpr
      = some (.int (Sha256Spec.ch X Y Z).toNat) := by
  simp only [chExpr, eval, Env.bind, evalBinOp, Sha256Spec.ch,
    beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq,
    ofInt_natCast_toNat, ofInt_ofNat_toNat, Option.some.injEq,
    PVal.int.injEq, Int.ofNat_eq_natCast, Int.natCast_inj, BitVec.toNat_inj]
  bv_decide

set_option linter.unusedSimpArgs false in
/-- `majExpr` refines `Sha256Spec.maj`: for ALL words `X Y Z`. Here the
    source is written verbatim as the spec, so the word-level cores
    coincide after the round-trip collapse — `bv_decide` discharges any
    residual (and is a no-op when `simp` already closes it). -/
theorem maj_refines (X Y Z : BitVec 32) (fuel : Nat) :
    eval (fun _ => none)
      (((Env.empty.bind "x" (.int X.toNat)).bind "y" (.int Y.toNat)).bind
        "z" (.int Z.toNat))
      (fuel + 1) majExpr
      = some (.int (Sha256Spec.maj X Y Z).toNat) := by
  simp only [majExpr, eval, Env.bind, evalBinOp, Sha256Spec.maj,
    beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq,
    ofInt_natCast_toNat, ofInt_ofNat_toNat, Option.some.injEq,
    PVal.int.injEq, Int.ofNat_eq_natCast, Int.natCast_inj, BitVec.toNat_inj]
    <;> bv_decide

end Concrete.Proof
