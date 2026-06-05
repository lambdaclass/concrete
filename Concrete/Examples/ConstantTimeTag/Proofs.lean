import Concrete.Proof

/-!
# constant_time_tag — example proofs

Proof theorems + helper lemmas for the `constant_time_tag` flagship, moved out of
`Concrete.Proof` into this per-example namespace. The registered spec PExpr
`ctCompareExpr` and the `ctTagFns` table stay in `Concrete.Proof`. The bit-level
helper lemmas (currently used only by these proofs) move here too; if another
example needs them later they can be promoted to ProofKit.
-/

namespace Examples.ConstantTimeTag.Proofs

open Concrete.Proof

/-- Helper: any u8 value xor'd with itself is 0 at our unsigned-
    u8 evalBinOp encoding. -/
theorem bitxor_u8_self_zero (n : Int) :
    evalBinOp (.bitxor 8 false) (.int n) (.int n) = some (.int 0) := by
  simp [evalBinOp, BitVec.xor_self]

/-- Helper: `0 | 0 = 0` at u8 unsigned width.  Used by the
    universal same-tag theorem: when the accumulator `diff` is 0
    and `a[i] ^ b[i] = 0` (which holds when `a[i] = b[i]`), the
    next `diff` stays 0. -/
theorem bitor_u8_zero_zero :
    evalBinOp (.bitor 8 false) (.int 0) (.int 0) = some (.int 0) := by
  rfl

/-- Composed helper: for any int `n`, the loop body's effect
    `diff := 0 | (n ^ n)` produces accumulator `0` at u8 unsigned.
    This is the per-iteration invariant of the same-tag universal
    proof.  Formulated as a sequenced evalBinOp pair so simp can
    rewrite both ops in a single step. -/
theorem ct_loop_iteration_invariant (n : Int) :
    (match evalBinOp (.bitxor 8 false) (.int n) (.int n) with
      | some xorRes => evalBinOp (.bitor 8 false) (.int 0) xorRes
      | none => none) = some (.int 0) := by
  rw [bitxor_u8_self_zero]; rfl

set_option maxHeartbeats 4000000 in
set_option linter.unusedSimpArgs false in
/-- **Universal same-tag theorem** (AUDIT bar #2):
    `ct_compare a a = 1` for any 16-element tag `a`, where each
    byte is given as an `Int` (wrapped in `PVal.int`).

    Phrased over a tuple of 16 Ints rather than an opaque `List
    PVal` so each `lookupIndex` reduces concretely under simp.
    The shape `[.int b0, .int b1, ..., .int b15]` is the actual
    image of any `[u8; 16]` source array under PExpr extraction;
    no information is lost.

    Per-iteration: `diff := 0 | (bᵢ ^ bᵢ) = 0 | 0 = 0`.
    After 16 iterations `i = 16` makes `i < 16` false, falling
    through to `if 0 == 0 then 1 else 0` which returns 1.

    Stronger than `ct_compare_equal_zeros_correct` because the
    byte values are arbitrary; the previous theorem held only
    for the all-zero tag.  This theorem is the credible
    crypto-adjacent claim: **equal tags always pass, with all
    16 loop iterations executed**. -/
theorem ct_compare_same_tag_correct
    (b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 : Int)
    (fuel : Nat) :
    let tag : PVal := .array_
      [.int b0, .int b1, .int b2, .int b3,
       .int b4, .int b5, .int b6, .int b7,
       .int b8, .int b9, .int b10, .int b11,
       .int b12, .int b13, .int b14, .int b15]
    eval ctTagFns
      ((Env.empty.bind "a" tag).bind "b" tag)
      (fuel + 200) ctCompareExpr
    = some (.int 1) := by
  -- Each lookupIndex returns the matching .int bᵢ on both
  -- sides; bᵢ ^ bᵢ = 0 via BitVec.xor_self; 0 | 0 = 0.  Simp
  -- chains the 16 iterations using the helpers above as
  -- rewrite rules.
  simp [ctCompareExpr,
        eval, eval.evalAssigns, eval.lookupIndex,
        ctTagFns, Env.bind, evalBinOp,
        BitVec.xor_self, BitVec.zero_or]

set_option maxHeartbeats 2000000 in
set_option linter.unusedSimpArgs false in
/-- `ct_compare zeros zeros = 1`: comparing the canonical
    all-zero tag against itself returns 1.

    First attached theorem on constant_time_tag (AUDIT bar #1).
    Exercises every Phase 4 extension the candidate forced:
      * u8 `bitxor` at unsigned width
      * u8 `bitor`  at unsigned width
      * bounded 16-iteration `while_` (flat-assign body)
      * source-level no-early-exit discipline
      * final `if diff == 0` branch

    The proof reduces by simp: each iteration evaluates
    `0 | (0 ^ 0) = 0` and `0 + 1 = 1` ... `15 + 1 = 16`,
    then `16 < 16 = false` fires the fall-through, and the
    final `0 == 0` branch returns 1.

    NOTE: this is the concrete all-zero case.  The
    universal `ct_compare a a = 1` for any `a : [u8; 16]`
    is the natural next step, tracked as the AUDIT bar #2
    composition theorem.  The universal version requires
    a small lemma `x ^ x = 0` at u8 (which closes by
    decide on each byte value) plus induction over the
    array; the concrete version covers the substantive
    machinery (u8 bitor/bitxor + 16-iteration while_ +
    final eq branch) without the induction. -/
theorem ct_compare_equal_zeros_correct (fuel : Nat) :
    eval ctTagFns
      ((Env.empty.bind "a" (.array_ (List.replicate 16 (.int 0)))).bind
        "b" (.array_ (List.replicate 16 (.int 0))))
      (fuel + 200) ctCompareExpr
    = some (.int 1) := by
  simp [ctCompareExpr,
        eval, eval.evalAssigns, eval.lookupIndex,
        ctTagFns, Env.bind, evalBinOp, List.replicate]

/-- `Nat` bitwise fact: xor is zero iff the operands are equal. Proved by bit
    extensionality (`Nat.eq_of_testBit_eq` + `Nat.testBit_xor`); no Mathlib. -/
theorem nat_xor_eq_zero_iff (m n : Nat) : (m ^^^ n = 0) ↔ m = n := by
  constructor
  · intro h
    apply Nat.eq_of_testBit_eq; intro i
    have hb : (m ^^^ n).testBit i = false := by rw [h]; simp
    rw [Nat.testBit_xor] at hb
    revert hb; cases m.testBit i <;> cases n.testBit i <;> simp
  · intro h; subst h; simp [Nat.xor_self]

/-- `Nat` bitwise fact: lor is zero iff both operands are zero. -/
theorem nat_lor_eq_zero_iff (a b : Nat) : (a ||| b = 0) ↔ (a = 0 ∧ b = 0) := by
  constructor
  · intro h
    refine ⟨Nat.eq_of_testBit_eq ?_, Nat.eq_of_testBit_eq ?_⟩ <;>
      (intro i; have hb : (a ||| b).testBit i = false := by rw [h]; simp
       rw [Nat.testBit_or] at hb; simp at hb; simp [hb])
  · intro ⟨h1, h2⟩; subst h1; subst h2; rfl

set_option maxRecDepth 8000 in
set_option maxHeartbeats 8000000 in
/-- **Universal different-tag theorem** (closes AUDIT bar #2's converse):
    `ct_compare a b = 0` whenever the two 16-byte tags differ at the u8 level.

    The hypothesis is exactly the u8-level inequality: not all 16 byte slots
    agree after the `BitVec.ofInt 8` reduction the eval applies (i.e. the tags
    differ as `[u8; 16]` values). For valid u8 source bytes (0..255), this is
    just `a ≠ b`.

    Together with `ct_compare_same_tag_correct` (a == a → 1), this gives the
    full functional-correctness iff: ct_compare returns 1 iff the tags are equal
    and 0 iff they differ. The OR-accumulated `diff` is nonzero exactly when
    some byte differs — proved by collapsing the unfolded 16-way OR/XOR chain
    with `nat_lor_eq_zero_iff` / `nat_xor_eq_zero_iff`. -/
theorem ct_compare_different_tag_correct
    (a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 : Int)
    (b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 : Int)
    (fuel : Nat)
    (hne : ¬ ((a0 % 256).toNat % 256 = (b0 % 256).toNat % 256 ∧
              (a1 % 256).toNat % 256 = (b1 % 256).toNat % 256 ∧
              (a2 % 256).toNat % 256 = (b2 % 256).toNat % 256 ∧
              (a3 % 256).toNat % 256 = (b3 % 256).toNat % 256 ∧
              (a4 % 256).toNat % 256 = (b4 % 256).toNat % 256 ∧
              (a5 % 256).toNat % 256 = (b5 % 256).toNat % 256 ∧
              (a6 % 256).toNat % 256 = (b6 % 256).toNat % 256 ∧
              (a7 % 256).toNat % 256 = (b7 % 256).toNat % 256 ∧
              (a8 % 256).toNat % 256 = (b8 % 256).toNat % 256 ∧
              (a9 % 256).toNat % 256 = (b9 % 256).toNat % 256 ∧
              (a10 % 256).toNat % 256 = (b10 % 256).toNat % 256 ∧
              (a11 % 256).toNat % 256 = (b11 % 256).toNat % 256 ∧
              (a12 % 256).toNat % 256 = (b12 % 256).toNat % 256 ∧
              (a13 % 256).toNat % 256 = (b13 % 256).toNat % 256 ∧
              (a14 % 256).toNat % 256 = (b14 % 256).toNat % 256 ∧
              (a15 % 256).toNat % 256 = (b15 % 256).toNat % 256)) :
    let tagA : PVal := .array_ [.int a0,.int a1,.int a2,.int a3,.int a4,.int a5,.int a6,.int a7,.int a8,.int a9,.int a10,.int a11,.int a12,.int a13,.int a14,.int a15]
    let tagB : PVal := .array_ [.int b0,.int b1,.int b2,.int b3,.int b4,.int b5,.int b6,.int b7,.int b8,.int b9,.int b10,.int b11,.int b12,.int b13,.int b14,.int b15]
    eval ctTagFns ((Env.empty.bind "a" tagA).bind "b" tagB) (fuel + 200) ctCompareExpr
    = some (.int 0) := by
  simp [ctCompareExpr, eval, eval.evalAssigns, eval.lookupIndex, Env.bind, evalBinOp]
  split <;>
    first
      | rfl
      | (rename_i h
         simp only [Option.some.injEq, PVal.bool.injEq, beq_iff_eq, Int.natCast_eq_zero,
                    nat_lor_eq_zero_iff, nat_xor_eq_zero_iff, and_assoc] at h
         exact absurd h hne)
      | simp_all
end Examples.ConstantTimeTag.Proofs
