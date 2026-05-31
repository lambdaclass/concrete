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


-- ==================================================================
-- block_to_words: the first refinement of a Concrete LOOP vs spec
-- (task #19 / ROADMAP Phase 8 — the HMAC bar-#2 gateway).
-- ==================================================================

set_option linter.unusedSimpArgs false

-- ===== helper lemmas =====
theorem lookupIndex_eq (l : List PVal) (m : Nat) : eval.lookupIndex l m = l[m]? := by
  induction l generalizing m with
  | nil => simp [eval.lookupIndex]
  | cons x xs ih => cases m with
    | zero => simp [eval.lookupIndex]
    | succ n => simp [eval.lookupIndex, ih]

theorem lookupIndex_range_map (f : Nat → PVal) (n m : Nat) (h : m < n) :
    eval.lookupIndex ((List.range n).map f) m = some (f m) := by
  rw [lookupIndex_eq]; simp [h]

theorem ofInt32_byte (B : BitVec 8) :
    BitVec.ofInt 32 (Int.ofNat B.toNat) = B.setWidth 32 := by
  apply BitVec.eq_of_toNat_eq; simp [BitVec.toNat_setWidth]
theorem ofInt32_byte_cast (B : BitVec 8) :
    BitVec.ofInt 32 (B.toNat : Int) = B.setWidth 32 := by
  apply BitVec.eq_of_toNat_eq; simp [BitVec.toNat_setWidth]

theorem readN (b : Nat → BitVec 8) (m : Nat) (hm : m < 64) :
    (if ((m:Int)) < 0 then (none : Option PVal)
     else eval.lookupIndex (List.map (fun j => PVal.int ↑(b j).toNat) (List.range 64)) ((m:Int).toNat))
    = some (PVal.int ↑(b m).toNat) := by
  rw [if_neg (by omega)]
  simp only [Int.toNat_natCast]
  exact lookupIndex_range_map _ 64 m hm

-- ===== the packing word and the source block_to_words expression =====
/-- The big-endian-packed word j of a 64-byte block. -/
def pwd (b : Nat → BitVec 8) (j : Nat) : BitVec 32 :=
  Sha256Spec.packWord (b (4*j)) (b (4*j+1)) (b (4*j+2)) (b (4*j+3))

/-- The input block, as the evaluator's array value. -/
def blockArr (b : Nat → BitVec 8) : List PVal :=
  (List.range 64).map (fun j => PVal.int ↑(b j).toNat)

private def idx0 : PExpr := .binOp .mul (.var "i") (.lit (.int 4))
private def idxO (o : Int) : PExpr := .binOp .add idx0 (.lit (.int o))

/-- The big-endian word-packing expression in `block_to_words`'s loop
    body: `(block[i*4]<<24) | (block[i*4+1]<<16) | (block[i*4+2]<<8) |
    block[i*4+3]`, each byte cast to u32. -/
def packExpr : PExpr :=
  .binOp (.bitor 32 false)
    (.binOp (.bitor 32 false)
      (.binOp (.bitor 32 false)
        (.binOp (.shl 32 false) (.cast (.arrayIndex (.var "block") idx0)) (.lit (.int 24)))
        (.binOp (.shl 32 false) (.cast (.arrayIndex (.var "block") (idxO 1))) (.lit (.int 16))))
      (.binOp (.shl 32 false) (.cast (.arrayIndex (.var "block") (idxO 2))) (.lit (.int 8))))
    (.cast (.arrayIndex (.var "block") (idxO 3)))

set_option linter.unusedSimpArgs false in
/-- The loop body's packing expression evaluates to the spec packed
    word `pwd b k`, in any env that binds `block` and `i` suitably. -/
theorem packExpr_eval (b : Nat → BitVec 8) (env : Env) (k : Nat) (hk : k < 16) (fuel : Nat)
    (hb : env "block" = some (.array_ (blockArr b)))
    (hi : env "i" = some (.int (k:Int))) :
    eval (fun _ => none) env (fuel + 4) packExpr
      = some (.int (pwd b k).toNat) := by
  simp only [packExpr, idx0, idxO, blockArr, pwd, eval, evalBinOp, hb, hi]
  rw [show ((k:Int) * 4 + 1) = ((4*k+1 : Nat) : Int) by omega,
     show ((k:Int) * 4 + 2) = ((4*k+2 : Nat) : Int) by omega,
     show ((k:Int) * 4 + 3) = ((4*k+3 : Nat) : Int) by omega,
     show ((k:Int) * 4) = ((4*k : Nat) : Int) by omega]
  rw [readN b (4*k) (by omega), readN b (4*k+1) (by omega),
      readN b (4*k+2) (by omega), readN b (4*k+3) (by omega)]
  simp only [Sha256Spec.packWord, ofInt32_byte, ofInt32_byte_cast,
    ofInt_ofNat_toNat, ofInt_natCast_toNat, Option.some.injEq, PVal.int.injEq,
    Int.ofNat_eq_natCast, Int.natCast_inj, BitVec.toNat_inj]
  bv_decide

-- ===== loop state =====
/-- Working array after `k` iterations: first `k` words packed, rest 0. -/
def wList (b : Nat → BitVec 8) (k : Nat) : List PVal :=
  (List.range 16).map (fun j => if j < k then PVal.int ↑(pwd b j).toNat else PVal.int 0)

def stEnv (b : Nat → BitVec 8) (k : Nat) : Env :=
  ((Env.empty.bind "block" (.array_ (blockArr b))).bind
    "w" (.array_ (wList b k))).bind "i" (.int (k:Int))

theorem wList_length (b) (k : Nat) : (wList b k).length = 16 := by
  simp [wList]

theorem wList_zero (b : Nat → BitVec 8) :
    wList b 0 = List.replicate 16 (PVal.int 0) := by
  simp [wList]
  rfl

/-- Setting slot `k` of `wList b k` to the packed word `pwd b k` gives
    `wList b (k+1)` — the loop's array invariant advances by one word. -/
theorem wList_set (b : Nat → BitVec 8) (k : Nat) :
    (wList b k).set k (PVal.int ↑(pwd b k).toNat) = wList b (k + 1) := by
  apply List.ext_getElem (by simp [wList])
  intro i h1 _
  by_cases hki : k = i
  · subst hki
    simp only [wList, List.getElem_set_self, List.getElem_map, List.getElem_range]
    simp
  · rw [List.getElem_set_ne hki]
    simp only [wList, List.getElem_map, List.getElem_range]
    rcases Nat.lt_or_ge i k with h | h
    · rw [if_pos h, if_pos (by omega)]
    · rw [if_neg (by omega), if_neg (by omega)]

-- ===== the loop expression =====
def cond_e : PExpr := .binOp .lt (.var "i") (.lit (.int 16))
def assigns_e : List (String × PExpr) :=
  [ ("w", .arraySet (.var "w") (.var "i") packExpr)
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]

theorem cond_true (b : Nat → BitVec 8) (k : Nat) (hk : k < 16) (base : Nat) :
    eval (fun _ => none) (stEnv b k) (base + 1) cond_e = some (.bool true) := by
  simp only [cond_e, stEnv, eval, Env.bind, evalBinOp]
  simp; omega

theorem cond_false (b : Nat → BitVec 8) (base : Nat) :
    eval (fun _ => none) (stEnv b 16) (base + 1) cond_e = some (.bool false) := by
  simp only [cond_e, stEnv, eval, Env.bind, evalBinOp]
  simp

/-- Generic single-step `arraySet` evaluation, given the three sub-eval
    results.  Keeps `packExpr` opaque (it enters as `ev`) so its proof is
    not re-derived. -/
theorem eval_arraySet_lemma (fns : FnTable) (env : Env) (fuel : Nat)
    (arr idx val : PExpr) (es : List PVal) (i : Int) (v : PVal)
    (ea : eval fns env fuel arr = some (.array_ es))
    (ei : eval fns env fuel idx = some (.int i))
    (ev : eval fns env fuel val = some v)
    (hi0 : 0 ≤ i) (hib : i.toNat < es.length) :
    eval fns env (fuel + 1) (.arraySet arr idx val)
      = some (.array_ (es.set i.toNat v)) := by
  simp only [eval, ea, ei, ev]
  rw [if_neg (by omega), if_neg (by omega)]

theorem assigns_step (b : Nat → BitVec 8) (k : Nat) (hk : k < 16) (fuel : Nat) :
    eval.evalAssigns (fun _ => none) (stEnv b k) (fuel + 5) assigns_e
      = some (stEnv b (k + 1)) := by
  have hb : (stEnv b k) "block" = some (.array_ (blockArr b)) := by
    simp [stEnv, Env.bind]
  have hi : (stEnv b k) "i" = some (.int (k:Int)) := by
    simp [stEnv, Env.bind]
  have hpack : eval (fun _ => none) (stEnv b k) (fuel + 4) packExpr
      = some (.int (pwd b k).toNat) := packExpr_eval b (stEnv b k) k hk fuel hb hi
  have hwv : eval (fun _ => none) (stEnv b k) (fuel + 4) (.var "w")
      = some (.array_ (wList b k)) := by simp [eval, stEnv, Env.bind]
  have hiv : eval (fun _ => none) (stEnv b k) (fuel + 4) (.var "i")
      = some (.int (k:Int)) := by simp [eval, stEnv, Env.bind]
  -- the `w := packExpr` assignment
  have harr : eval (fun _ => none) (stEnv b k) (fuel + 5)
      (.arraySet (.var "w") (.var "i") packExpr)
      = some (.array_ (wList b (k + 1))) := by
    rw [eval_arraySet_lemma (fun _ => none) (stEnv b k) (fuel + 4)
        (.var "w") (.var "i") packExpr (wList b k) (k:Int) (.int (pwd b k).toNat)
        hwv hiv hpack (by omega) (by rw [wList_length]; simp; omega)]
    rw [show ((k:Int)).toNat = k by omega, wList_set b k]
  -- thread the first assignment (harr) while stEnv is still folded
  simp only [assigns_e, eval.evalAssigns, harr]
  -- thread the `i := i + 1` assignment and close against stEnv b (k+1)
  simp only [eval.evalAssigns, eval, evalBinOp, Env.bind, stEnv,
    beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq, reduceCtorEq,
    Option.some.injEq]
  funext n
  simp only [Env.bind]
  by_cases h1 : (n == "i") = true <;> by_cases h2 : (n == "w") = true <;>
    simp_all [Option.some.injEq, PVal.int.injEq] <;> omega

-- ===== assembling the full loop =====
def blockToWordsExpr : PExpr :=
  .letIn "w" (.arrayLit (List.replicate 16 (.lit (.int 0))))
    (.letIn "i" (.lit (.int 0))
      (.while_ cond_e assigns_e (.var "w")))

theorem eval_letIn (fns : FnTable) (env : Env) (fuel : Nat)
    (name : String) (val body : PExpr) (v : PVal)
    (hv : eval fns env (fuel + 1) val = some v) :
    eval fns env (fuel + 1) (.letIn name val body)
      = eval fns (env.bind name v) fuel body := by
  simp only [eval, hv]

theorem evalElems_replicate_lit (fns : FnTable) (env : Env) (fuel : Nat)
    (n : Nat) (v : PVal) :
    eval.evalElems fns env (fuel + 1) (List.replicate n (.lit v))
      = some (List.replicate n v) := by
  induction n with
  | zero => simp [List.replicate, eval.evalElems]
  | succ m ih => simp [List.replicate, eval.evalElems, eval, ih]

theorem arrayLit_zeros_eval (fns : FnTable) (env : Env) (fuel : Nat) :
    eval fns env (fuel + 2) (.arrayLit (List.replicate 16 (.lit (.int 0))))
      = some (.array_ (List.replicate 16 (PVal.int 0))) := by
  simp only [eval, evalElems_replicate_lit]

theorem getD_range_map (b : Nat → BitVec 8) (m : Nat) (hm : m < 64) :
    ((List.range 64).map b).getD m 0 = b m := by
  rw [List.getD_eq_getElem?_getD]
  simp [hm]

/-- The fully-packed working array equals the spec's `blockToWords` of
    the same bytes, word for word. -/
theorem wList16_spec (b : Nat → BitVec 8) :
    wList b 16
      = (Sha256Spec.blockToWords ((List.range 64).map b)).map
          (fun w => PVal.int w.toNat) := by
  apply List.ext_getElem (by simp [wList, Sha256Spec.blockToWords])
  intro j h1 _
  simp only [wList, List.length_map, List.length_range] at h1
  simp only [wList, Sha256Spec.blockToWords, List.getElem_map, List.getElem_range,
    if_pos h1, pwd]
  rw [getD_range_map b (4*j) (by omega), getD_range_map b (4*j+1) (by omega),
      getD_range_map b (4*j+2) (by omega), getD_range_map b (4*j+3) (by omega)]

theorem loop_eval (b : Nat → BitVec 8) (fuel : Nat) :
    eval (fun _ => none) (stEnv b 0) (fuel + 22) (.while_ cond_e assigns_e (.var "w"))
      = some (.array_ (wList b 16)) := by
  have hwc := eval_while_count (fun _ => none) cond_e assigns_e (.var "w") (stEnv b)
      16 (fuel + 5)
      (fun k hk => ⟨cond_true b k hk (fuel + 4), assigns_step b k hk fuel⟩)
      (cond_false b (fuel + 4))
  rw [show fuel + 22 = (fuel + 5) + 16 + 1 by omega, hwc]
  simp [eval, stEnv, Env.bind]

set_option linter.unusedSimpArgs false in
/-- **`block_to_words` refines its spec.** For ALL 64 input bytes,
    evaluating the extracted big-endian word-packing loop yields exactly
    `Sha256Spec.blockToWords` of those bytes, word for word. The first
    refinement of a Concrete *loop* against an independent spec —
    combining `eval_while_count`, the array set/get lemmas, the `bv_decide`
    packing fact, and the spec layer. -/
theorem block_to_words_refines_spec (b : Nat → BitVec 8) (fuel : Nat) :
    eval (fun _ => none)
      (Env.empty.bind "block" (.array_ (blockArr b)))
      (fuel + 24) blockToWordsExpr
    = some (.array_ ((Sha256Spec.blockToWords ((List.range 64).map b)).map
        (fun w => PVal.int w.toNat))) := by
  rw [← wList16_spec]
  rw [show fuel + 24 = (fuel + 23) + 1 by omega, blockToWordsExpr,
      eval_letIn _ _ _ _ _ _ _ (arrayLit_zeros_eval _ _ (fuel + 22))]
  rw [show fuel + 23 = (fuel + 22) + 1 by omega,
      eval_letIn _ _ _ _ _ _ (PVal.int 0) (by simp [eval])]
  rw [← wList_zero b]
  exact loop_eval b fuel

end Concrete.Proof
