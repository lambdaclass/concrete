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
theorem packExpr_eval (fns : FnTable) (b : Nat → BitVec 8) (env : Env) (k : Nat) (hk : k < 16) (fuel : Nat)
    (hb : env "block" = some (.array_ (blockArr b)))
    (hi : env "i" = some (.int (k:Int))) :
    eval fns env (fuel + 4) packExpr
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

/-- Generic counter-loop array-update **frame lemma**: setting slot `m` of a
    "first-`m`-cells-filled" range-map to its `m`-th value extends it to
    "first-`m+1`-filled". The frame — every cell `i ≠ m` is unchanged — is the
    single `i ≠ m` branch below, proven ONCE here and reused for every
    iteration of every such loop (`wList_set`, `wschedList_set`, …) via
    `eval_while_count`. This is why the SHA-256 loop proofs do not pay a
    per-index / per-iteration frame cost; see ROADMAP "Frame inference" for the
    heavier separation-logic story that a flat-heap model would need. -/
theorem set_in_counter_map {α : Type} (n : Nat) (f : Nat → α) (z : α) (m : Nat) :
    ((List.range n).map (fun j => if j < m then f j else z)).set m (f m)
      = (List.range n).map (fun j => if j < m + 1 then f j else z) := by
  apply List.ext_getElem (by simp)
  intro i h1 _
  by_cases hmi : m = i
  · subst hmi
    simp only [List.getElem_set_self, List.getElem_map, List.getElem_range]; simp
  · rw [List.getElem_set_ne hmi]
    simp only [List.getElem_map, List.getElem_range]
    rcases Nat.lt_or_ge i m with h | h
    · rw [if_pos h, if_pos (by omega)]
    · rw [if_neg (by omega), if_neg (by omega)]

/-- Setting slot `k` of `wList b k` to the packed word `pwd b k` gives
    `wList b (k+1)` — instance of the generic frame lemma. -/
theorem wList_set (b : Nat → BitVec 8) (k : Nat) :
    (wList b k).set k (PVal.int ↑(pwd b k).toNat) = wList b (k + 1) :=
  set_in_counter_map 16 (fun j => PVal.int ↑(pwd b j).toNat) (PVal.int 0) k

-- ===== the loop expression =====
def cond_e : PExpr := .binOp .lt (.var "i") (.lit (.int 16))
def assigns_e : List (String × PExpr) :=
  [ ("w", .arraySet (.var "w") (.var "i") packExpr)
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]

theorem cond_true (fns : FnTable) (b : Nat → BitVec 8) (k : Nat) (hk : k < 16) (base : Nat) :
    eval fns (stEnv b k) (base + 1) cond_e = some (.bool true) := by
  simp only [cond_e, stEnv, eval, Env.bind, evalBinOp]
  simp; omega

theorem cond_false (fns : FnTable) (b : Nat → BitVec 8) (base : Nat) :
    eval fns (stEnv b 16) (base + 1) cond_e = some (.bool false) := by
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

theorem assigns_step (fns : FnTable) (b : Nat → BitVec 8) (k : Nat) (hk : k < 16) (fuel : Nat) :
    eval.evalAssigns fns (stEnv b k) (fuel + 5) assigns_e
      = some (stEnv b (k + 1)) := by
  have hb : (stEnv b k) "block" = some (.array_ (blockArr b)) := by
    simp [stEnv, Env.bind]
  have hi : (stEnv b k) "i" = some (.int (k:Int)) := by
    simp [stEnv, Env.bind]
  have hpack : eval fns (stEnv b k) (fuel + 4) packExpr
      = some (.int (pwd b k).toNat) := packExpr_eval fns b (stEnv b k) k hk fuel hb hi
  have hwv : eval fns (stEnv b k) (fuel + 4) (.var "w")
      = some (.array_ (wList b k)) := by simp [eval, stEnv, Env.bind]
  have hiv : eval fns (stEnv b k) (fuel + 4) (.var "i")
      = some (.int (k:Int)) := by simp [eval, stEnv, Env.bind]
  -- the `w := packExpr` assignment
  have harr : eval fns (stEnv b k) (fuel + 5)
      (.arraySet (.var "w") (.var "i") packExpr)
      = some (.array_ (wList b (k + 1))) := by
    rw [eval_arraySet_lemma fns (stEnv b k) (fuel + 4)
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

theorem loop_eval (fns : FnTable) (b : Nat → BitVec 8) (fuel : Nat) :
    eval fns (stEnv b 0) (fuel + 22) (.while_ cond_e assigns_e (.var "w"))
      = some (.array_ (wList b 16)) := by
  have hwc := eval_while_count fns cond_e assigns_e (.var "w") (stEnv b)
      16 (fuel + 5)
      (fun k hk => ⟨cond_true fns b k hk (fuel + 4), assigns_step fns b k hk fuel⟩)
      (cond_false fns b (fuel + 4))
  rw [show fuel + 22 = (fuel + 5) + 16 + 1 by omega, hwc]
  simp [eval, stEnv, Env.bind]

set_option linter.unusedSimpArgs false in
/-- **`block_to_words` refines its spec.** For ALL 64 input bytes,
    evaluating the extracted big-endian word-packing loop yields exactly
    `Sha256Spec.blockToWords` of those bytes, word for word. The first
    refinement of a Concrete *loop* against an independent spec —
    combining `eval_while_count`, the array set/get lemmas, the `bv_decide`
    packing fact, and the spec layer. -/
theorem block_to_words_refines_spec (fns : FnTable) (b : Nat → BitVec 8) (fuel : Nat) :
    eval fns
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
  exact loop_eval fns b fuel


-- ==================================================================
-- SHA-256 round FUNCTIONS refine their spec (task #20, part 1):
-- rotation, the four sigmas, and the function-call/bind scaffolding
-- that lets extracted `rotr(...)`/`ch(...)`/`maj(...)`/`sigma(...)`
-- calls reduce cleanly. The Boolean functions (ch/maj) are above;
-- here we add `rotr` (with its `32 - n` shift amount) and the sigmas.
-- ==================================================================

/-- Extracted `rotr(x, n) = (x >> n) | (x << (32 - n))` (FIPS 180-4
    §3.2). The `32 - n` shift amount is the construct that the Boolean
    functions did not exercise. -/
def rotrExpr : PExpr :=
  .binOp (.bitor 32 false)
    (.binOp (.shr 32 false) (.var "x") (.var "n"))
    (.binOp (.shl 32 false) (.var "x") (.binOp .sub (.lit (.int 32)) (.var "n")))

set_option linter.unusedSimpArgs false in
/-- `rotrExpr` refines `Sha256Spec.rotr` for all words and all shift
    amounts `n ≤ 32`. (`rotrExpr` contains no calls, so this holds in
    any function table.) -/
theorem rotr_refines (fns : FnTable) (X : BitVec 32) (n : Nat) (hn : n ≤ 32) (fuel : Nat) :
    eval fns ((Env.empty.bind "x" (.int X.toNat)).bind "n" (.int (n:Int))) (fuel + 1) rotrExpr
      = some (.int (Sha256Spec.rotr X n).toNat) := by
  simp only [rotrExpr, eval, Env.bind, evalBinOp, Sha256Spec.rotr,
    beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq, reduceCtorEq,
    ofInt_natCast_toNat]
  rw [show ((n:Int)).toNat = n by omega, show ((32:Int) - (n:Int)).toNat = 32 - n by omega]
  simp only [Option.some.injEq, PVal.int.injEq, Int.ofNat_eq_natCast, Int.natCast_inj,
    BitVec.toNat_inj, ofInt_natCast_toNat, ofInt_ofNat_toNat]

def bigSigma0Expr : PExpr :=
  .binOp (.bitxor 32 false)
    (.binOp (.bitxor 32 false)
      (.call "rotr" [.var "x", .lit (.int 2)]) (.call "rotr" [.var "x", .lit (.int 13)]))
    (.call "rotr" [.var "x", .lit (.int 22)])
def bigSigma1Expr : PExpr :=
  .binOp (.bitxor 32 false)
    (.binOp (.bitxor 32 false)
      (.call "rotr" [.var "x", .lit (.int 6)]) (.call "rotr" [.var "x", .lit (.int 11)]))
    (.call "rotr" [.var "x", .lit (.int 25)])
def smallSigma0Expr : PExpr :=
  .binOp (.bitxor 32 false)
    (.binOp (.bitxor 32 false)
      (.call "rotr" [.var "x", .lit (.int 7)]) (.call "rotr" [.var "x", .lit (.int 18)]))
    (.binOp (.shr 32 false) (.var "x") (.lit (.int 3)))
def smallSigma1Expr : PExpr :=
  .binOp (.bitxor 32 false)
    (.binOp (.bitxor 32 false)
      (.call "rotr" [.var "x", .lit (.int 17)]) (.call "rotr" [.var "x", .lit (.int 19)]))
    (.binOp (.shr 32 false) (.var "x") (.lit (.int 10)))

private def addwE (a b : PExpr) : PExpr := .binOp (.addw 32 false) a b
private def stateAt (i : Int) : PExpr := .arrayIndex (.var "state") (.lit (.int i))

/-- Extracted `sha256_round(state, k, w)` body. -/
def roundExpr : PExpr :=
  .letIn "t1"
    (addwE (addwE (addwE (addwE (stateAt 7) (.call "big_sigma1" [stateAt 4]))
              (.call "ch" [stateAt 4, stateAt 5, stateAt 6])) (.var "k")) (.var "w"))
    (.letIn "t2"
      (addwE (.call "big_sigma0" [stateAt 0]) (.call "maj" [stateAt 0, stateAt 1, stateAt 2]))
      (.arrayLit [ addwE (.var "t1") (.var "t2"), stateAt 0, stateAt 1, stateAt 2,
                   addwE (stateAt 3) (.var "t1"), stateAt 4, stateAt 5, stateAt 6 ]))

-- Schedule / k / round expression data, relocated above `shaFns` so the
-- unified table can name every callee `sha256_compress` reaches.
def addwS (a b : PExpr) : PExpr := .binOp (.addw 32 false) a b
def wIdx (c : Int) : PExpr := .arrayIndex (.var "w") (.binOp .sub (.var "i") (.lit (.int c)))
def expansionExpr : PExpr :=
  addwS (addwS (addwS (.call "small_sigma1" [wIdx 2]) (wIdx 7))
          (.call "small_sigma0" [wIdx 15])) (wIdx 16)
def assigns1 : List (String × PExpr) :=
  [ ("w", .arraySet (.var "w") (.var "i") (.arrayIndex (.var "w16") (.var "i")))
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
def assigns2 : List (String × PExpr) :=
  [ ("w", .arraySet (.var "w") (.var "i") expansionExpr)
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
def condE (bound : Int) : PExpr := .binOp .lt (.var "i") (.lit (.int bound))
def scheduleExpr : PExpr :=
  .letIn "w" (.arrayLit (List.replicate 64 (.lit (.int 0))))
    (.letIn "i" (.lit (.int 0))
      (.while_ (condE 16) assigns1
        (.letIn "i" (.lit (.int 16))
          (.while_ (condE 64) assigns2 (.var "w")))))
/-- Extracted `sha256_k()` body: the 64 round constants K[0..63]. -/
def sha256kExpr : PExpr := .arrayLit [.lit (.int 0x428a2f98), .lit (.int 0x71374491), .lit (.int 0xb5c0fbcf), .lit (.int 0xe9b5dba5), .lit (.int 0x3956c25b), .lit (.int 0x59f111f1), .lit (.int 0x923f82a4), .lit (.int 0xab1c5ed5), .lit (.int 0xd807aa98), .lit (.int 0x12835b01), .lit (.int 0x243185be), .lit (.int 0x550c7dc3), .lit (.int 0x72be5d74), .lit (.int 0x80deb1fe), .lit (.int 0x9bdc06a7), .lit (.int 0xc19bf174), .lit (.int 0xe49b69c1), .lit (.int 0xefbe4786), .lit (.int 0xfc19dc6), .lit (.int 0x240ca1cc), .lit (.int 0x2de92c6f), .lit (.int 0x4a7484aa), .lit (.int 0x5cb0a9dc), .lit (.int 0x76f988da), .lit (.int 0x983e5152), .lit (.int 0xa831c66d), .lit (.int 0xb00327c8), .lit (.int 0xbf597fc7), .lit (.int 0xc6e00bf3), .lit (.int 0xd5a79147), .lit (.int 0x6ca6351), .lit (.int 0x14292967), .lit (.int 0x27b70a85), .lit (.int 0x2e1b2138), .lit (.int 0x4d2c6dfc), .lit (.int 0x53380d13), .lit (.int 0x650a7354), .lit (.int 0x766a0abb), .lit (.int 0x81c2c92e), .lit (.int 0x92722c85), .lit (.int 0xa2bfe8a1), .lit (.int 0xa81a664b), .lit (.int 0xc24b8b70), .lit (.int 0xc76c51a3), .lit (.int 0xd192e819), .lit (.int 0xd6990624), .lit (.int 0xf40e3585), .lit (.int 0x106aa070), .lit (.int 0x19a4c116), .lit (.int 0x1e376c08), .lit (.int 0x2748774c), .lit (.int 0x34b0bcb5), .lit (.int 0x391c0cb3), .lit (.int 0x4ed8aa4a), .lit (.int 0x5b9cca4f), .lit (.int 0x682e6ff3), .lit (.int 0x748f82ee), .lit (.int 0x78a5636f), .lit (.int 0x84c87814), .lit (.int 0x8cc70208), .lit (.int 0x90befffa), .lit (.int 0xa4506ceb), .lit (.int 0xbef9a3f7), .lit (.int 0xc67178f2)]

/-- The SHA-256 helper function table: the extracted bodies of `rotr`,
    `ch`, `maj`, the four sigmas, and the compression `round`, keyed by
    their source names. -/
def shaFns : FnTable
  | "rotr"         => some ⟨"rotr",         ["x", "n"],         rotrExpr⟩
  | "ch"           => some ⟨"ch",           ["x", "y", "z"],    chExpr⟩
  | "maj"          => some ⟨"maj",          ["x", "y", "z"],    majExpr⟩
  | "big_sigma0"   => some ⟨"big_sigma0",   ["x"],              bigSigma0Expr⟩
  | "big_sigma1"   => some ⟨"big_sigma1",   ["x"],              bigSigma1Expr⟩
  | "small_sigma0" => some ⟨"small_sigma0", ["x"],              smallSigma0Expr⟩
  | "small_sigma1" => some ⟨"small_sigma1", ["x"],              smallSigma1Expr⟩
  | "sha256_round"    => some ⟨"sha256_round",    ["state", "k", "w"], roundExpr⟩
  | "block_to_words"  => some ⟨"block_to_words",  ["block"],            blockToWordsExpr⟩
  | "sha256_schedule" => some ⟨"sha256_schedule", ["w16"],             scheduleExpr⟩
  | "sha256_k"        => some ⟨"sha256_k",        [],                  sha256kExpr⟩
  | _                 => none

/-- Completeness: the unified table resolves every function
    `sha256_compress` reaches, directly (`block_to_words`,
    `sha256_schedule`, `sha256_k`, `sha256_round`) and transitively
    (`ch`/`maj`/`rotr` and the four sigmas). No call bottoms out at a
    missing callee. -/
theorem shaFns_resolves_compress_calls :
    (shaFns "block_to_words").isSome ∧ (shaFns "sha256_schedule").isSome ∧
    (shaFns "sha256_k").isSome ∧ (shaFns "sha256_round").isSome ∧
    (shaFns "ch").isSome ∧ (shaFns "maj").isSome ∧ (shaFns "rotr").isSome ∧
    (shaFns "big_sigma0").isSome ∧ (shaFns "big_sigma1").isSome ∧
    (shaFns "small_sigma0").isSome ∧ (shaFns "small_sigma1").isSome := by
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩ <;> rfl

/-- A call `rotr(xe, c)` where `xe` evaluates to `X` and the literal
    `c = n` reduces to `Sha256Spec.rotr X n`. The bridge from source
    call sites to `rotr_refines`. -/
theorem rotr_call (X : BitVec 32) (n : Nat) (c : Int) (hc : c = (n:Int)) (hn : n ≤ 32)
    (env : Env) (xe : PExpr) (fuel : Nat)
    (hx : eval shaFns env (fuel + 1) xe = some (.int (X.toNat : Int))) :
    eval shaFns env (fuel + 2) (.call "rotr" [xe, .lit (.int c)])
      = some (.int (Sha256Spec.rotr X n).toNat) := by
  subst hc
  simp only [eval, shaFns, eval.evalArgs, hx, bindArgs]
  exact rotr_refines shaFns X n hn fuel

set_option linter.unusedSimpArgs false in
theorem big_sigma0_refines (X : BitVec 32) (fuel : Nat) :
    eval shaFns (Env.empty.bind "x" (.int X.toNat)) (fuel + 2) bigSigma0Expr
      = some (.int (Sha256Spec.bigSigma0 X).toNat) := by
  have hx : eval shaFns (Env.empty.bind "x" (.int X.toNat)) (fuel + 1) (.var "x")
      = some (.int (X.toNat : Int)) := by simp [eval, Env.bind]
  have h2 := rotr_call X 2 2 (by omega) (by omega) _ _ fuel hx
  have h13 := rotr_call X 13 13 (by omega) (by omega) _ _ fuel hx
  have h22 := rotr_call X 22 22 (by omega) (by omega) _ _ fuel hx
  simp only [bigSigma0Expr, eval, h2, h13, h22, evalBinOp, Sha256Spec.bigSigma0,
    Option.some.injEq, PVal.int.injEq, Int.ofNat_eq_natCast, Int.natCast_inj,
    BitVec.toNat_inj, ofInt_natCast_toNat, ofInt_ofNat_toNat]

set_option linter.unusedSimpArgs false in
theorem big_sigma1_refines (X : BitVec 32) (fuel : Nat) :
    eval shaFns (Env.empty.bind "x" (.int X.toNat)) (fuel + 2) bigSigma1Expr
      = some (.int (Sha256Spec.bigSigma1 X).toNat) := by
  have hx : eval shaFns (Env.empty.bind "x" (.int X.toNat)) (fuel + 1) (.var "x")
      = some (.int (X.toNat : Int)) := by simp [eval, Env.bind]
  have h6 := rotr_call X 6 6 (by omega) (by omega) _ _ fuel hx
  have h11 := rotr_call X 11 11 (by omega) (by omega) _ _ fuel hx
  have h25 := rotr_call X 25 25 (by omega) (by omega) _ _ fuel hx
  simp only [bigSigma1Expr, eval, h6, h11, h25, evalBinOp, Sha256Spec.bigSigma1,
    Option.some.injEq, PVal.int.injEq, Int.ofNat_eq_natCast, Int.natCast_inj,
    BitVec.toNat_inj, ofInt_natCast_toNat, ofInt_ofNat_toNat]

set_option linter.unusedSimpArgs false in
theorem small_sigma0_refines (X : BitVec 32) (fuel : Nat) :
    eval shaFns (Env.empty.bind "x" (.int X.toNat)) (fuel + 2) smallSigma0Expr
      = some (.int (Sha256Spec.smallSigma0 X).toNat) := by
  have hx : eval shaFns (Env.empty.bind "x" (.int X.toNat)) (fuel + 1) (.var "x")
      = some (.int (X.toNat : Int)) := by simp [eval, Env.bind]
  have h7 := rotr_call X 7 7 (by omega) (by omega) _ _ fuel hx
  have h18 := rotr_call X 18 18 (by omega) (by omega) _ _ fuel hx
  simp only [smallSigma0Expr, eval, h7, h18, evalBinOp, Sha256Spec.smallSigma0,
    Env.bind, beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq, reduceCtorEq,
    Option.some.injEq, PVal.int.injEq, Int.ofNat_eq_natCast, Int.natCast_inj,
    BitVec.toNat_inj, ofInt_natCast_toNat, ofInt_ofNat_toNat]
  rw [show ((3:Int)).toNat = 3 by omega]

set_option linter.unusedSimpArgs false in
theorem small_sigma1_refines (X : BitVec 32) (fuel : Nat) :
    eval shaFns (Env.empty.bind "x" (.int X.toNat)) (fuel + 2) smallSigma1Expr
      = some (.int (Sha256Spec.smallSigma1 X).toNat) := by
  have hx : eval shaFns (Env.empty.bind "x" (.int X.toNat)) (fuel + 1) (.var "x")
      = some (.int (X.toNat : Int)) := by simp [eval, Env.bind]
  have h17 := rotr_call X 17 17 (by omega) (by omega) _ _ fuel hx
  have h19 := rotr_call X 19 19 (by omega) (by omega) _ _ fuel hx
  simp only [smallSigma1Expr, eval, h17, h19, evalBinOp, Sha256Spec.smallSigma1,
    Env.bind, beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq, reduceCtorEq,
    Option.some.injEq, PVal.int.injEq, Int.ofNat_eq_natCast, Int.natCast_inj,
    BitVec.toNat_inj, ofInt_natCast_toNat, ofInt_ofNat_toNat]
  rw [show ((10:Int)).toNat = 10 by omega]


-- ==================================================================
-- sha256_round refines its spec (task #20, part 2): the compression
-- round — Sigma1/Ch/Sigma0/Maj over the working state plus the
-- wrapping-add (`addw`) chain t1 = h+Σ1(e)+Ch(e,f,g)+k+w,
-- t2 = Σ0(a)+Maj(a,b,c), next = [t1+t2, a,b,c, d+t1, e,f,g].
-- The helper *calls* (rotr/ch/maj/sigmas) and the state[i] reads are
-- unfolded inline; the whole word-level identity is kernel-checked by
-- `bv_decide`.
-- ==================================================================

set_option maxHeartbeats 4000000 in
set_option linter.unusedSimpArgs false in
/-- `roundExpr` refines `Sha256Spec.round` for ALL working-state words
    and round constants — the SHA-256 compression round computes exactly
    the spec round. -/
theorem round_refines (S0 S1 S2 S3 S4 S5 S6 S7 K W : BitVec 32) (fuel : Nat) :
    eval shaFns
      (((Env.empty.bind "state" (.array_
          [.int S0.toNat, .int S1.toNat, .int S2.toNat, .int S3.toNat,
           .int S4.toNat, .int S5.toNat, .int S6.toNat, .int S7.toNat])).bind
        "k" (.int K.toNat)).bind "w" (.int W.toNat))
      (fuel + 8) roundExpr
    = some (.array_ ((Sha256Spec.round
        [S0, S1, S2, S3, S4, S5, S6, S7] K W).map (fun w => PVal.int w.toNat))) := by
  simp only [roundExpr, addwE, stateAt, eval, Env.bind, evalBinOp, eval.evalArgs,
    eval.evalElems, eval.lookupIndex, bindArgs, shaFns, rotrExpr, chExpr, majExpr,
    bigSigma0Expr, bigSigma1Expr, Sha256Spec.round, Sha256Spec.ch, Sha256Spec.maj,
    Sha256Spec.bigSigma0, Sha256Spec.bigSigma1, Sha256Spec.rotr,
    beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq, reduceCtorEq,
    Int.reduceToNat, Int.reduceLT, Int.reduceSub, Nat.reduceSub, reduceIte,
    List.getD_cons_zero, List.getD_cons_succ, List.map_cons, List.map_nil,
    ofInt_natCast_toNat, ofInt_ofNat_toNat,
    Option.some.injEq, PVal.array_.injEq, PVal.int.injEq, List.cons.injEq,
    and_true, true_and, and_self, Int.ofNat_eq_natCast, Int.natCast_inj, BitVec.toNat_inj]
  have hnot : ∀ v : BitVec 32, v ^^^ BitVec.ofInt 32 4294967295 = ~~~v := fun v => by bv_decide
  simp only [hnot]
  bv_decide


-- ==================================================================
-- Spec-side schedule recurrence (task #20, part 3a): the message
-- schedule `Sha256Spec.expandSchedule` (defined imperatively via
-- `Id.run`/`for`) satisfies the FIPS 180-4 §6.2 recurrence
--   W[i] = sigma1(W[i-2]) + W[i-7] + sigma0(W[i-15]) + W[i-16].
-- Proved independently of the evaluator by converting the for-loop to
-- a `List.range'` fold and inducting (append-only ⇒ prefix-stable).
-- This is the refinement target's recurrence that the loop proof
-- (`sha256_schedule_refines_spec`) feeds each iteration.
-- ==================================================================

private abbrev schedW := BitVec 32
private def newword (w : List schedW) (i : Nat) : schedW :=
  Sha256Spec.smallSigma1 (w.getD (i-2) 0) + w.getD (i-7) 0 + Sha256Spec.smallSigma0 (w.getD (i-15) 0) + w.getD (i-16) 0
private def estep (w : List schedW) (i : Nat) : List schedW := w ++ [newword w i]
private def sched (w16 : List schedW) (n : Nat) : List schedW := (List.range' 16 n).foldl estep w16

set_option maxHeartbeats 1000000 in
private theorem expandSchedule_eq_sched (w16 : List schedW) :
    Sha256Spec.expandSchedule w16 = sched w16 48 := by
  unfold Sha256Spec.expandSchedule sched
  simp only [Id.run, bind_pure_comp, pure_bind, map_pure,
    Std.Legacy.Range.forIn_eq_forIn_range', Std.Legacy.Range.size,
    List.forIn_pure_yield_eq_foldl, Nat.reduceDiv, Nat.reduceSub, Nat.reduceAdd]
  rfl

private theorem sched_succ (w16 : List schedW) (n : Nat) :
    sched w16 (n+1) = estep (sched w16 n) (16 + n) := by
  unfold sched
  rw [List.range'_concat, List.foldl_append]
  simp only [List.foldl_cons, List.foldl_nil, Nat.one_mul]

private theorem sched_length (w16 : List schedW) (hlen : w16.length = 16) (n : Nat) :
    (sched w16 n).length = 16 + n := by
  induction n with
  | zero => simpa [sched]
  | succ m ih =>
    rw [sched_succ, estep]
    simp only [List.length_append, List.length_cons, List.length_nil, ih]; omega

private theorem sched_prefix (w16 : List schedW) (n d : Nat) :
    ∃ tail, sched w16 (n + d) = sched w16 n ++ tail := by
  induction d with
  | zero => exact ⟨[], by simp⟩
  | succ e ih =>
    obtain ⟨tail, ht⟩ := ih
    refine ⟨tail ++ [newword (sched w16 (n+e)) (16 + (n+e))], ?_⟩
    rw [show n + (e+1) = (n+e)+1 by omega, sched_succ, estep, ht, List.append_assoc]

private theorem sched_getD_stable (w16 : List schedW) (hlen : w16.length = 16) (n d j : Nat)
    (hj : j < 16 + n) :
    (sched w16 (n + d)).getD j 0 = (sched w16 n).getD j 0 := by
  obtain ⟨tail, ht⟩ := sched_prefix w16 n d
  rw [ht, List.getD_eq_getElem?_getD, List.getD_eq_getElem?_getD,
      List.getElem?_append_left (by rw [sched_length w16 hlen]; omega)]

private theorem sched_getD_new (w16 : List schedW) (hlen : w16.length = 16) (k : Nat) :
    (sched w16 (k+1)).getD (16+k) 0 = newword (sched w16 k) (16+k) := by
  have hl : (sched w16 k).length = 16 + k := sched_length w16 hlen k
  rw [sched_succ, estep, List.getD_eq_getElem?_getD,
      List.getElem?_append_right (by omega), hl]
  simp

private theorem sched_lt16 (w16 : List schedW) (hlen : w16.length = 16) (n j : Nat) (hj : j < 16) :
    (sched w16 n).getD j 0 = w16.getD j 0 := by
  rw [show n = 0 + n by omega, sched_getD_stable w16 hlen 0 n j (by omega)]
  rfl

private theorem sched_getD_48 (w16 : List schedW) (hlen : w16.length = 16) (k j : Nat)
    (hk : k ≤ 48) (hj : j < 16 + k) :
    (sched w16 48).getD j 0 = (sched w16 k).getD j 0 := by
  rw [show (48:Nat) = k + (48 - k) by omega, sched_getD_stable w16 hlen k (48-k) j hj]

set_option maxHeartbeats 1000000 in
theorem expandSchedule_recurrence (w16 : List schedW) (hlen : w16.length = 16) (k : Nat) (hk : k < 48) :
    (Sha256Spec.expandSchedule w16).getD (16+k) 0
      = Sha256Spec.smallSigma1 ((Sha256Spec.expandSchedule w16).getD (14+k) 0)
        + (Sha256Spec.expandSchedule w16).getD (9+k) 0
        + Sha256Spec.smallSigma0 ((Sha256Spec.expandSchedule w16).getD (1+k) 0)
        + (Sha256Spec.expandSchedule w16).getD k 0 := by
  rw [expandSchedule_eq_sched,
      sched_getD_48 w16 hlen (k+1) (16+k) (by omega) (by omega), sched_getD_new w16 hlen]
  unfold newword
  rw [show 16+k-2 = 14+k by omega, show 16+k-7 = 9+k by omega,
      show 16+k-15 = 1+k by omega, show 16+k-16 = k by omega,
      sched_getD_48 w16 hlen k (14+k) (by omega) (by omega),
      sched_getD_48 w16 hlen k (9+k) (by omega) (by omega),
      sched_getD_48 w16 hlen k (1+k) (by omega) (by omega),
      sched_getD_48 w16 hlen k k (by omega) (by omega)]

theorem expandSchedule_length (w16 : List schedW) (hlen : w16.length = 16) :
    (Sha256Spec.expandSchedule w16).length = 64 := by
  rw [expandSchedule_eq_sched, sched_length w16 hlen]

theorem expandSchedule_lt16 (w16 : List schedW) (hlen : w16.length = 16) (j : Nat) (hj : j < 16) :
    (Sha256Spec.expandSchedule w16).getD j 0 = w16.getD j 0 := by
  rw [expandSchedule_eq_sched, sched_lt16 w16 hlen 48 j hj]

-- ==================================================================
-- sha256_schedule refines its spec (task #20, part 3b): the message
-- schedule's two loops (copy 0..15, then the self-referential
-- expansion 16..63) compute exactly Sha256Spec.expandSchedule for ALL
-- inputs. Copy loop + expansion loop each via eval_while_count; reads
-- of earlier schedule words via prefix structure (wread); small_sigma
-- calls via unary_call + small_sigma*_refines; per-step value via the
-- committed expandSchedule_recurrence.
-- ==================================================================

-- input words as a list
abbrev w16of (wf : Nat → BitVec 32) : List (BitVec 32) := (List.range 16).map wf

theorem w16of_length (wf) : (w16of wf).length = 16 := by simp [w16of]
theorem w16of_getD (wf) (j : Nat) (hj : j < 16) : (w16of wf).getD j 0 = wf j := by
  rw [List.getD_eq_getElem?_getD]; simp [w16of, hj]

-- the j-th schedule word
def swd (wf : Nat → BitVec 32) (j : Nat) : BitVec 32 :=
  (Sha256Spec.expandSchedule (w16of wf)).getD j 0

theorem swd_lt16 (wf) (j : Nat) (hj : j < 16) : swd wf j = wf j := by
  unfold swd
  rw [expandSchedule_lt16 (w16of wf) (w16of_length wf) j hj, w16of_getD wf j hj]

theorem swd_rec (wf) (k : Nat) (hk : k < 48) :
    swd wf (16+k)
      = Sha256Spec.smallSigma1 (swd wf (14+k)) + swd wf (9+k)
        + Sha256Spec.smallSigma0 (swd wf (1+k)) + swd wf k := by
  unfold swd
  exact expandSchedule_recurrence (w16of wf) (w16of_length wf) k hk

-- working array after the first m schedule words are filled
def wschedList (wf : Nat → BitVec 32) (m : Nat) : List PVal :=
  (List.range 64).map (fun j => if j < m then PVal.int ↑(swd wf j).toNat else PVal.int 0)

theorem wschedList_length (wf) (m) : (wschedList wf m).length = 64 := by simp [wschedList]

/-- Setting slot `m` of `wschedList wf m` to schedule word `swd wf m` gives
    `wschedList wf (m+1)` — instance of the generic frame lemma. -/
theorem wschedList_set (wf) (m : Nat) :
    (wschedList wf m).set m (PVal.int ↑(swd wf m).toNat) = wschedList wf (m + 1) :=
  set_in_counter_map 64 (fun j => PVal.int ↑(swd wf j).toNat) (PVal.int 0) m

-- read from the working array: index value j < M gives schedule word swd j
theorem wread (wf : Nat → BitVec 32) (M : Nat) (env : Env) (fuel : Nat) (j : Nat) (hj : j < M)
    (hM : M ≤ 64)
    (hw : env "w" = some (.array_ (wschedList wf M)))
    (idxE : PExpr) (hidx : eval shaFns env (fuel + 1) idxE = some (.int (j:Int))) :
    eval shaFns env (fuel + 1) (.arrayIndex (.var "w") idxE) = some (.int (swd wf j).toNat) := by
  simp only [eval, hw, hidx, wschedList]
  rw [if_neg (by omega), show ((j:Int)).toNat = j by omega,
      lookupIndex_range_map _ 64 j (by omega)]
  simp [hj]

-- read from the input array w16: index value j < 16 gives wf j
theorem w16read (wf : Nat → BitVec 32) (env : Env) (fuel : Nat) (j : Nat) (hj : j < 16)
    (hw16 : env "w16" = some (.array_ ((List.range 16).map (fun k => PVal.int ↑(wf k).toNat))))
    (idxE : PExpr) (hidx : eval shaFns env (fuel + 1) idxE = some (.int (j:Int))) :
    eval shaFns env (fuel + 1) (.arrayIndex (.var "w16") idxE) = some (.int (wf j).toNat) := by
  simp only [eval, hw16, hidx]
  rw [if_neg (by omega), show ((j:Int)).toNat = j by omega,
      lookupIndex_range_map _ 16 j hj]

-- generic unary helper call (sigmas)
theorem unary_call (name : String) (body : PExpr) (specf : BitVec 32 → BitVec 32)
    (hfn : shaFns name = some ⟨name, ["x"], body⟩)
    (href : ∀ (Y : BitVec 32) (f : Nat),
      eval shaFns (Env.empty.bind "x" (.int Y.toNat)) (f + 2) body = some (.int (specf Y).toNat))
    (X : BitVec 32) (env : Env) (xe : PExpr) (fuel : Nat)
    (hx : eval shaFns env (fuel + 2) xe = some (.int (X.toNat : Int))) :
    eval shaFns env (fuel + 3) (.call name [xe]) = some (.int (specf X).toNat) := by
  simp only [eval, hfn, eval.evalArgs, hx, bindArgs]
  exact href X fuel

abbrev w16arr (wf : Nat → BitVec 32) : PVal :=
  .array_ ((List.range 16).map (fun k => PVal.int ↑(wf k).toNat))
def st2 (wf : Nat → BitVec 32) (k : Nat) : Env :=
  ((Env.empty.bind "w16" (w16arr wf)).bind
    "w" (.array_ (wschedList wf (16+k)))).bind "i" (.int ((16+k:Nat):Int))
theorem idxSub_eval (k c : Nat) (hc : c ≤ 16) (env : Env) (F : Nat)
    (hi : env "i" = some (.int ((16+k:Nat):Int))) :
    eval shaFns env (F + 1) (.binOp .sub (.var "i") (.lit (.int (c:Int))))
      = some (.int ((16+k-c : Nat):Int)) := by
  simp only [eval, hi, evalBinOp, Option.some.injEq, PVal.int.injEq]
  omega

set_option maxHeartbeats 1000000 in
theorem expansion_value (wf : Nat → BitVec 32) (k : Nat) (hk : k < 48) (fuel : Nat) :
    eval shaFns (st2 wf k) (fuel + 5) expansionExpr = some (.int (swd wf (16+k)).toNat) := by
  have hw : (st2 wf k) "w" = some (.array_ (wschedList wf (16+k))) := by simp [st2, Env.bind]
  have hi : (st2 wf k) "i" = some (.int ((16+k:Nat):Int)) := by simp [st2, Env.bind]
  -- index sub-expressions
  have hx2 := idxSub_eval k 2 (by omega) (st2 wf k) (fuel+3) hi
  have hx7 := idxSub_eval k 7 (by omega) (st2 wf k) (fuel+4) hi
  have hx15 := idxSub_eval k 15 (by omega) (st2 wf k) (fuel+3) hi
  have hx16 := idxSub_eval k 16 (by omega) (st2 wf k) (fuel+4) hi
  -- reads (call args at fuel+4, direct reads at fuel+5)
  have ra2 : eval shaFns (st2 wf k) (fuel+4) (wIdx 2) = some (.int (swd wf (14+k)).toNat) :=
    wread wf (16+k) (st2 wf k) (fuel+3) (14+k) (by omega) (by omega) hw _
      (by rw [show (14+k:Nat) = 16+k-2 by omega]; exact hx2)
  have ra15 : eval shaFns (st2 wf k) (fuel+4) (wIdx 15) = some (.int (swd wf (1+k)).toNat) :=
    wread wf (16+k) (st2 wf k) (fuel+3) (1+k) (by omega) (by omega) hw _
      (by rw [show (1+k:Nat) = 16+k-15 by omega]; exact hx15)
  have ra7 : eval shaFns (st2 wf k) (fuel+5) (wIdx 7) = some (.int (swd wf (9+k)).toNat) :=
    wread wf (16+k) (st2 wf k) (fuel+4) (9+k) (by omega) (by omega) hw _
      (by rw [show (9+k:Nat) = 16+k-7 by omega]; exact hx7)
  have ra16 : eval shaFns (st2 wf k) (fuel+5) (wIdx 16) = some (.int (swd wf k).toNat) :=
    wread wf (16+k) (st2 wf k) (fuel+4) k (by omega) (by omega) hw _
      (by have h := hx16; rw [show 16+k-16 = k by omega] at h; exact h)
  -- sigma calls
  have hsm1 := unary_call "small_sigma1" smallSigma1Expr Sha256Spec.smallSigma1 rfl
    small_sigma1_refines (swd wf (14+k)) (st2 wf k) (wIdx 2) (fuel+2) ra2
  have hsm0 := unary_call "small_sigma0" smallSigma0Expr Sha256Spec.smallSigma0 rfl
    small_sigma0_refines (swd wf (1+k)) (st2 wf k) (wIdx 15) (fuel+2) ra15
  -- combine
  simp only [expansionExpr, addwS, eval, hsm1, ra7, hsm0, ra16, evalBinOp,
    ofInt_natCast_toNat, ofInt_ofNat_toNat, Option.some.injEq, PVal.int.injEq,
    Int.ofNat_eq_natCast, Int.natCast_inj, BitVec.toNat_inj]
  rw [swd_rec wf k hk]

theorem expansion_step (wf : Nat → BitVec 32) (k : Nat) (hk : k < 48) (fuel : Nat) :
    eval.evalAssigns shaFns (st2 wf k) (fuel + 6) assigns2 = some (st2 wf (k + 1)) := by
  have hw : (st2 wf k) "w" = some (.array_ (wschedList wf (16+k))) := by simp [st2, Env.bind]
  have hi : (st2 wf k) "i" = some (.int ((16+k:Nat):Int)) := by simp [st2, Env.bind]
  have hwv : eval shaFns (st2 wf k) (fuel + 5) (.var "w")
      = some (.array_ (wschedList wf (16+k))) := by simp [eval, st2, Env.bind]
  have hiv : eval shaFns (st2 wf k) (fuel + 5) (.var "i")
      = some (.int ((16+k:Nat):Int)) := by simp [eval, st2, Env.bind]
  have hev := expansion_value wf k hk fuel
  have harr : eval shaFns (st2 wf k) (fuel + 6)
      (.arraySet (.var "w") (.var "i") expansionExpr)
      = some (.array_ (wschedList wf (16 + (k+1)))) := by
    rw [eval_arraySet_lemma shaFns (st2 wf k) (fuel + 5)
        (.var "w") (.var "i") expansionExpr (wschedList wf (16+k)) ((16+k:Nat):Int)
        (.int (swd wf (16+k)).toNat) hwv hiv hev (by omega)
        (by rw [wschedList_length]; omega)]
    rw [show ((16+k:Nat):Int).toNat = 16+k by omega, wschedList_set wf (16+k),
        show 16 + k + 1 = 16 + (k+1) by omega]
  simp only [assigns2, eval.evalAssigns, harr]
  simp only [eval, evalBinOp, Env.bind, st2,
    beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq, reduceCtorEq,
    Option.some.injEq]
  funext n
  by_cases h1 : (n == "i") = true <;> by_cases h2 : (n == "w") = true <;>
    simp_all [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq,
      reduceCtorEq, Option.some.injEq, PVal.int.injEq] <;> omega

def st1 (wf : Nat → BitVec 32) (k : Nat) : Env :=
  ((Env.empty.bind "w16" (w16arr wf)).bind
    "w" (.array_ (wschedList wf k))).bind "i" (.int (k:Int))

theorem copy_step (wf : Nat → BitVec 32) (k : Nat) (hk : k < 16) (fuel : Nat) :
    eval.evalAssigns shaFns (st1 wf k) (fuel + 6) assigns1 = some (st1 wf (k + 1)) := by
  have hw : (st1 wf k) "w" = some (.array_ (wschedList wf k)) := by simp [st1, Env.bind]
  have hwv : eval shaFns (st1 wf k) (fuel + 5) (.var "w")
      = some (.array_ (wschedList wf k)) := by simp [eval, st1, Env.bind]
  have hiv : eval shaFns (st1 wf k) (fuel + 5) (.var "i") = some (.int (k:Int)) := by
    simp [eval, st1, Env.bind]
  have hw16 : (st1 wf k) "w16"
      = some (.array_ ((List.range 16).map (fun j => PVal.int ↑(wf j).toNat))) := by
    simp [st1, Env.bind, w16arr]
  have hcv : eval shaFns (st1 wf k) (fuel + 5) (.arrayIndex (.var "w16") (.var "i"))
      = some (.int (swd wf k).toNat) := by
    rw [swd_lt16 wf k hk]
    exact w16read wf (st1 wf k) (fuel + 4) k hk hw16 (.var "i") hiv
  have harr : eval shaFns (st1 wf k) (fuel + 6)
      (.arraySet (.var "w") (.var "i") (.arrayIndex (.var "w16") (.var "i")))
      = some (.array_ (wschedList wf (k+1))) := by
    rw [eval_arraySet_lemma shaFns (st1 wf k) (fuel + 5)
        (.var "w") (.var "i") (.arrayIndex (.var "w16") (.var "i"))
        (wschedList wf k) (k:Int) (.int (swd wf k).toNat) hwv hiv hcv (by omega)
        (by rw [wschedList_length]; omega)]
    rw [show ((k:Int)).toNat = k by omega, wschedList_set wf k]
  simp only [assigns1, eval.evalAssigns, harr]
  simp only [eval, evalBinOp, Env.bind, st1,
    beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq, reduceCtorEq,
    Option.some.injEq]
  funext n
  by_cases h1 : (n == "i") = true <;> by_cases h2 : (n == "w") = true <;>
    simp_all [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq,
      reduceCtorEq, Option.some.injEq, PVal.int.injEq] <;> omega

theorem cond_copy_true (wf) (k : Nat) (hk : k < 16) (F : Nat) :
    eval shaFns (st1 wf k) (F + 1) (condE 16) = some (.bool true) := by
  simp only [condE, st1, eval, Env.bind, evalBinOp]; simp; omega
theorem cond_copy_false (wf) (F : Nat) :
    eval shaFns (st1 wf 16) (F + 1) (condE 16) = some (.bool false) := by
  simp only [condE, st1, eval, Env.bind, evalBinOp]; simp
theorem cond_exp_true (wf) (k : Nat) (hk : k < 48) (F : Nat) :
    eval shaFns (st2 wf k) (F + 1) (condE 64) = some (.bool true) := by
  simp only [condE, st2, eval, Env.bind, evalBinOp]; simp; omega
theorem cond_exp_false (wf) (F : Nat) :
    eval shaFns (st2 wf 48) (F + 1) (condE 64) = some (.bool false) := by
  simp only [condE, st2, eval, Env.bind, evalBinOp]; simp

theorem wschedList_zero (wf) : wschedList wf 0 = List.replicate 64 (PVal.int 0) := by
  simp [wschedList]; rfl

theorem wschedList_64_spec (wf : Nat → BitVec 32) :
    wschedList wf 64
      = (Sha256Spec.expandSchedule (w16of wf)).map (fun w => PVal.int w.toNat) := by
  apply List.ext_getElem
  · rw [List.length_map, expandSchedule_length (w16of wf) (w16of_length wf)]; simp [wschedList]
  · intro j h1 _
    simp only [wschedList, List.length_map, List.length_range] at h1
    have hlen : j < (Sha256Spec.expandSchedule (w16of wf)).length := by
      rw [expandSchedule_length (w16of wf) (w16of_length wf)]; omega
    simp only [wschedList, List.getElem_map, List.getElem_range, if_pos h1, swd,
      List.getD_eq_getElem?_getD, List.getElem?_eq_getElem hlen, Option.getD_some]

theorem copy_loop_eval (wf : Nat → BitVec 32) (cont : PExpr) (base : Nat) :
    eval shaFns (st1 wf 0) ((base + 6) + 16 + 1) (.while_ (condE 16) assigns1 cont)
      = eval shaFns (st1 wf 16) (base + 6) cont :=
  eval_while_count shaFns (condE 16) assigns1 cont (st1 wf) 16 (base + 6)
    (fun k hk => ⟨cond_copy_true wf k hk (base + 5), copy_step wf k hk base⟩)
    (cond_copy_false wf (base + 5))

theorem exp_loop_eval (wf : Nat → BitVec 32) (cont : PExpr) (base : Nat) :
    eval shaFns (st2 wf 0) ((base + 6) + 48 + 1) (.while_ (condE 64) assigns2 cont)
      = eval shaFns (st2 wf 48) (base + 6) cont :=
  eval_while_count shaFns (condE 64) assigns2 cont (st2 wf) 48 (base + 6)
    (fun k hk => ⟨cond_exp_true wf k hk (base + 5), expansion_step wf k hk base⟩)
    (cond_exp_false wf (base + 5))

theorem st1_16_bind_eq (wf) : (st1 wf 16).bind "i" (.int 16) = st2 wf 0 := by
  funext n
  simp only [st1, st2, Env.bind]
  by_cases h : (n == "i") = true <;> simp_all

theorem arrayLit64_eval (env : Env) (fuel : Nat) :
    eval shaFns env (fuel + 2) (.arrayLit (List.replicate 64 (.lit (.int 0))))
      = some (.array_ (List.replicate 64 (PVal.int 0))) := by
  simp only [eval, evalElems_replicate_lit]

theorem st1_zero_eq (wf) :
    ((Env.empty.bind "w16" (w16arr wf)).bind "w" (.array_ (wschedList wf 0))).bind "i" (.int 0)
      = st1 wf 0 := rfl

set_option maxHeartbeats 1000000 in
theorem sha256_schedule_refines_spec (wf : Nat → BitVec 32) (fuel : Nat) :
    eval shaFns (Env.empty.bind "w16" (w16arr wf)) (fuel + 75) scheduleExpr
      = some (.array_ ((Sha256Spec.expandSchedule (w16of wf)).map (fun w => PVal.int w.toNat))) := by
  rw [← wschedList_64_spec, scheduleExpr,
      show fuel + 75 = (fuel + 74) + 1 by omega,
      eval_letIn _ _ _ _ _ _ _ (arrayLit64_eval _ (fuel + 73)),
      show fuel + 74 = (fuel + 73) + 1 by omega,
      eval_letIn _ _ _ _ _ _ (PVal.int 0) (by simp [eval]),
      ← wschedList_zero wf, st1_zero_eq wf,
      show fuel + 73 = ((fuel + 50) + 6) + 16 + 1 by omega,
      copy_loop_eval wf _ (fuel + 50),
      show (fuel + 50) + 6 = (fuel + 55) + 1 by omega,
      eval_letIn _ _ _ _ _ _ (PVal.int 16) (by simp [eval]),
      st1_16_bind_eq wf,
      show fuel + 55 = (fuel + 6) + 48 + 1 by omega,
      exp_loop_eval wf _ fuel]
  simp [eval, st2, Env.bind]

-- ==================================================================
-- Spec-side compression recurrence (task #20, part 4a): the 64-round
-- Davies-Meyer body of Sha256Spec.compress reduces to a sequential
-- fold whose step is exactly one Sha256Spec.round, and compress is that
-- fold followed by the feed-forward add. Independent of the evaluator
-- (build-order: spec recurrence before the loop proof). Simpler than
-- the schedule (no self-reference — the fold accumulator IS the state).
-- ==================================================================

/-- Working state after `n` compression rounds over schedule `w`. -/
def compressFold (state0 w : List (BitVec 32)) (n : Nat) : List (BitVec 32) :=
  (List.range' 0 n).foldl
    (fun s i => Sha256Spec.round s (Sha256Spec.k.getD i 0) (w.getD i 0)) state0

theorem compressFold_succ (state0 w : List (BitVec 32)) (n : Nat) :
    compressFold state0 w (n+1)
      = Sha256Spec.round (compressFold state0 w n) (Sha256Spec.k.getD n 0) (w.getD n 0) := by
  unfold compressFold
  rw [List.range'_concat, List.foldl_append]
  simp [Nat.one_mul]

set_option maxHeartbeats 1000000 in
/-- `Sha256Spec.compress` is the round fold (`compressFold .. 64`) followed by
    the Davies-Meyer feed-forward `state[j] + s[j]`. -/
theorem compress_eq_feedforward (state0 : List (BitVec 32)) (block : List Sha256Spec.Byte) :
    Sha256Spec.compress state0 block
      = (List.range 8).map (fun j => state0.getD j 0
          + (compressFold state0 (Sha256Spec.expandSchedule (Sha256Spec.blockToWords block)) 64).getD j 0) := by
  unfold Sha256Spec.compress compressFold
  simp only [Id.run, bind_pure_comp, pure_bind, map_pure,
    Std.Legacy.Range.forIn_eq_forIn_range', Std.Legacy.Range.size,
    List.forIn_pure_yield_eq_foldl, Nat.reduceDiv, Nat.reduceSub, Nat.reduceAdd]
  rfl

-- ==================================================================
-- sha256_compress body refines its spec (task #20, part 4b): the
-- Davies-Meyer compression body — `s := state; 64 rounds of
-- sha256_round over the schedule; feed-forward state[j] + s[j]` —
-- computes exactly Sha256Spec.compress, given schedule `w` and round
-- constants `k` in scope. The 64 rounds go via eval_while_count with
-- the state invariant `s = compressFold .. m` (round_call lifts
-- round_refines to the opaque length-8 working state); each step uses
-- compressFold_succ; the read-only schedule/constants pay no frame
-- cost. (The full sha256_compress(state, block) additionally calls
-- block_to_words / sha256_schedule / sha256_k to build w and k — that
-- setup-call composition is the remaining glue, with #21's hash/hmac.)
-- ==================================================================

-- a length-8 list equals the explicit list of its 8 entries
theorem list8_self {α} (L : List α) (dflt : α) (h : L.length = 8) :
    L = [L.getD 0 dflt, L.getD 1 dflt, L.getD 2 dflt, L.getD 3 dflt,
         L.getD 4 dflt, L.getD 5 dflt, L.getD 6 dflt, L.getD 7 dflt] := by
  match L, h with
  | [a, b, c, d, e, f, g, hh], _ => rfl

-- Sha256Spec.round always returns a length-8 state
theorem round_length (s : List (BitVec 32)) (kc wc : BitVec 32) :
    (Sha256Spec.round s kc wc).length = 8 := by simp [Sha256Spec.round]

-- compressFold preserves length 8
theorem compressFold_length (state0 w : List (BitVec 32)) (h0 : state0.length = 8) (n : Nat) :
    (compressFold state0 w n).length = 8 := by
  induction n with
  | zero => simpa [compressFold]
  | succ m ih => rw [compressFold_succ]; exact round_length _ _ _

theorem round_refines_list (s : List (BitVec 32)) (hs : s.length = 8) (K W : BitVec 32) (fuel : Nat) :
    eval shaFns
      (((Env.empty.bind "state" (.array_ (s.map (fun x => PVal.int x.toNat)))).bind
        "k" (.int K.toNat)).bind "w" (.int W.toNat))
      (fuel + 8) roundExpr
      = some (.array_ ((Sha256Spec.round s K W).map (fun x => PVal.int x.toNat))) := by
  rw [list8_self s 0 hs]
  simp only [List.map_cons, List.map_nil]
  exact round_refines (s.getD 0 0) (s.getD 1 0) (s.getD 2 0) (s.getD 3 0)
    (s.getD 4 0) (s.getD 5 0) (s.getD 6 0) (s.getD 7 0) K W fuel

theorem round_call (s : List (BitVec 32)) (hs : s.length = 8) (K W : BitVec 32)
    (env : Env) (se ke we : PExpr) (fuel : Nat)
    (hse : eval shaFns env (fuel + 8) se = some (.array_ (s.map (fun x => PVal.int x.toNat))))
    (hke : eval shaFns env (fuel + 8) ke = some (.int K.toNat))
    (hwe : eval shaFns env (fuel + 8) we = some (.int W.toNat)) :
    eval shaFns env (fuel + 9) (.call "sha256_round" [se, ke, we])
      = some (.array_ ((Sha256Spec.round s K W).map (fun x => PVal.int x.toNat))) := by
  simp only [eval, shaFns, eval.evalArgs, hse, hke, hwe, bindArgs]
  exact round_refines_list s hs K W fuel

-- generic read of L.map enc at index j -> enc (L.getD j 0)
theorem arr_read (L : List (BitVec 32)) (env : Env) (fuel : Nat) (j : Nat) (hj : j < L.length)
    (name : String) (hL : env name = some (.array_ (L.map (fun x => PVal.int x.toNat))))
    (idxE : PExpr) (hidx : eval shaFns env (fuel + 1) idxE = some (.int (j:Int))) :
    eval shaFns env (fuel + 1) (.arrayIndex (.var name) idxE)
      = some (.int (L.getD j 0).toNat) := by
  simp only [eval, hL, hidx]
  rw [if_neg (by omega), show ((j:Int)).toNat = j by omega, lookupIndex_eq,
      List.getElem?_map, List.getElem?_eq_getElem hj]
  simp [List.getD_eq_getElem?_getD, List.getElem?_eq_getElem hj]

def stc (e : Env) (state0 wlist : List (BitVec 32)) (m : Nat) : Env :=
  ((((e.bind "state" (.array_ (state0.map (fun x => PVal.int x.toNat)))).bind
    "w" (.array_ (wlist.map (fun x => PVal.int x.toNat)))).bind
    "k" (.array_ (Sha256Spec.k.map (fun x => PVal.int x.toNat)))).bind
    "s" (.array_ ((compressFold state0 wlist m).map (fun x => PVal.int x.toNat)))).bind "i" (.int (m:Int))
def assignsC : List (String × PExpr) :=
  [ ("s", .call "sha256_round" [.var "s", .arrayIndex (.var "k") (.var "i"),
                                .arrayIndex (.var "w") (.var "i")])
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]

theorem cstep (e : Env) (state0 wlist : List (BitVec 32)) (h0 : state0.length = 8) (hwl : wlist.length = 64)
    (m : Nat) (hm : m < 64) (fuel : Nat) :
    eval.evalAssigns shaFns (stc e state0 wlist m) (fuel + 9) assignsC
      = some (stc e state0 wlist (m + 1)) := by
  have hslen : (compressFold state0 wlist m).length = 8 := compressFold_length state0 wlist h0 m
  have hkbind : (stc e state0 wlist m) "k" = some (.array_ (Sha256Spec.k.map (fun x => PVal.int x.toNat))) := by
    simp [stc, Env.bind]
  have hwbind : (stc e state0 wlist m) "w" = some (.array_ (wlist.map (fun x => PVal.int x.toNat))) := by
    simp [stc, Env.bind]
  have hiv : eval shaFns (stc e state0 wlist m) (fuel + 8) (.var "i") = some (.int (m:Int)) := by
    simp [eval, stc, Env.bind]
  have hsv : eval shaFns (stc e state0 wlist m) (fuel + 8) (.var "s")
      = some (.array_ ((compressFold state0 wlist m).map (fun x => PVal.int x.toNat))) := by simp [eval, stc, Env.bind]
  have hke : eval shaFns (stc e state0 wlist m) (fuel + 8) (.arrayIndex (.var "k") (.var "i"))
      = some (.int (Sha256Spec.k.getD m 0).toNat) :=
    arr_read Sha256Spec.k (stc e state0 wlist m) (fuel + 7) m (by simp [Sha256Spec.k]; omega)
      "k" hkbind _ hiv
  have hwe : eval shaFns (stc e state0 wlist m) (fuel + 8) (.arrayIndex (.var "w") (.var "i"))
      = some (.int (wlist.getD m 0).toNat) :=
    arr_read wlist (stc e state0 wlist m) (fuel + 7) m (by omega) "w" hwbind _ hiv
  have hround : eval shaFns (stc e state0 wlist m) (fuel + 9)
      (.call "sha256_round" [.var "s", .arrayIndex (.var "k") (.var "i"),
                             .arrayIndex (.var "w") (.var "i")])
      = some (.array_ ((compressFold state0 wlist (m+1)).map (fun x => PVal.int x.toNat))) := by
    rw [round_call (compressFold state0 wlist m) hslen (Sha256Spec.k.getD m 0) (wlist.getD m 0)
        (stc e state0 wlist m) _ _ _ (fuel + 0) (by simpa using hsv) (by simpa using hke)
        (by simpa using hwe)]
    rw [← compressFold_succ]
  simp only [assignsC, eval.evalAssigns, hround]
  simp only [eval, evalBinOp, Env.bind, stc, beq_self_eq_true, if_true, beq_iff_eq, if_false,
    String.reduceEq, reduceCtorEq, Option.some.injEq]
  funext n
  by_cases h1 : (n == "i") = true <;> by_cases h2 : (n == "s") = true <;>
    simp_all [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq,
      reduceCtorEq, Option.some.injEq, PVal.int.injEq] <;> omega

def condC : PExpr := .binOp .lt (.var "i") (.lit (.int 64))
theorem cond_c_true (e : Env) (state0 wlist) (m : Nat) (hm : m < 64) (F : Nat) :
    eval shaFns (stc e state0 wlist m) (F + 1) condC = some (.bool true) := by
  simp only [condC, stc, eval, Env.bind, evalBinOp]; simp; omega
theorem cond_c_false (e : Env) (state0 wlist) (F : Nat) :
    eval shaFns (stc e state0 wlist 64) (F + 1) condC = some (.bool false) := by
  simp only [condC, stc, eval, Env.bind, evalBinOp]; simp

theorem compress_loop_eval (e : Env) (state0 wlist : List (BitVec 32))
    (h0 : state0.length = 8) (hwl : wlist.length = 64) (cont : PExpr) (base : Nat) :
    eval shaFns (stc e state0 wlist 0) ((base + 9) + 64 + 1) (.while_ condC assignsC cont)
      = eval shaFns (stc e state0 wlist 64) (base + 9) cont :=
  eval_while_count shaFns condC assignsC cont (stc e state0 wlist) 64 (base + 9)
    (fun m hm => ⟨cond_c_true e state0 wlist m hm (base + 8), cstep e state0 wlist h0 hwl m hm base⟩)
    (cond_c_false e state0 wlist (base + 8))

def aw2 (a b : PExpr) : PExpr := .binOp (.addw 32 false) a b
def ix (nm : String) (j : Nat) : PExpr := .arrayIndex (.var nm) (.lit (.int (j : Int)))
def feedforwardExpr : PExpr :=
  .arrayLit [ aw2 (ix "state" 0) (ix "s" 0), aw2 (ix "state" 1) (ix "s" 1),
              aw2 (ix "state" 2) (ix "s" 2), aw2 (ix "state" 3) (ix "s" 3),
              aw2 (ix "state" 4) (ix "s" 4), aw2 (ix "state" 5) (ix "s" 5),
              aw2 (ix "state" 6) (ix "s" 6), aw2 (ix "state" 7) (ix "s" 7) ]

set_option maxHeartbeats 1000000 in
theorem ff_eval (e : Env) (state0 wlist : List (BitVec 32)) (h0 : state0.length = 8) (fuel : Nat) :
    eval shaFns (stc e state0 wlist 64) (fuel + 2) feedforwardExpr
      = some (.array_ ((List.range 8).map (fun j =>
          PVal.int (state0.getD j 0 + (compressFold state0 wlist 64).getD j 0).toNat))) := by
  have hsl : (compressFold state0 wlist 64).length = 8 := compressFold_length state0 wlist h0 64
  have hst : (stc e state0 wlist 64) "state"
      = some (.array_ (state0.map (fun x => PVal.int x.toNat))) := by simp [stc, Env.bind]
  have hsb : (stc e state0 wlist 64) "s"
      = some (.array_ ((compressFold state0 wlist 64).map (fun x => PVal.int x.toNat))) := by
    simp [stc, Env.bind]
  have rst : ∀ j : Nat, j < 8 →
      eval shaFns (stc e state0 wlist 64) (fuel + 1) (ix "state" j)
        = some (.int (state0.getD j 0).toNat) := fun j hj =>
    arr_read state0 _ fuel j (by omega) "state" hst _ (by simp [ix, eval])
  have rsf : ∀ j : Nat, j < 8 →
      eval shaFns (stc e state0 wlist 64) (fuel + 1) (ix "s" j)
        = some (.int ((compressFold state0 wlist 64).getD j 0).toNat) := fun j hj =>
    arr_read (compressFold state0 wlist 64) _ fuel j (by omega) "s" hsb _ (by simp [ix, eval])
  simp only [feedforwardExpr, aw2, eval, eval.evalElems, evalBinOp,
    rst 0 (by omega), rst 1 (by omega), rst 2 (by omega), rst 3 (by omega),
    rst 4 (by omega), rst 5 (by omega), rst 6 (by omega), rst 7 (by omega),
    rsf 0 (by omega), rsf 1 (by omega), rsf 2 (by omega), rsf 3 (by omega),
    rsf 4 (by omega), rsf 5 (by omega), rsf 6 (by omega), rsf 7 (by omega),
    ofInt_natCast_toNat, Option.some.injEq, List.range, List.range.loop, List.map_cons,
    List.map_nil, PVal.array_.injEq, List.cons.injEq, PVal.int.injEq, BitVec.toNat_inj,
    Int.ofNat_eq_natCast]

def compressBodyExpr : PExpr :=
  .letIn "s" (.var "state")
    (.letIn "i" (.lit (.int 0)) (.while_ condC assignsC feedforwardExpr))

theorem stc_zero_eq (e : Env) (state0 wlist : List (BitVec 32)) :
    ((((e.bind "state" (.array_ (state0.map (fun x => PVal.int x.toNat)))).bind
      "w" (.array_ (wlist.map (fun x => PVal.int x.toNat)))).bind
      "k" (.array_ (Sha256Spec.k.map (fun x => PVal.int x.toNat)))).bind
      "s" (.array_ (state0.map (fun x => PVal.int x.toNat)))).bind "i" (.int 0)
      = stc e state0 wlist 0 := rfl

set_option maxHeartbeats 1000000 in
theorem compress_body_refines (e : Env) (state0 wlist : List (BitVec 32))
    (h0 : state0.length = 8) (hwl : wlist.length = 64) (fuel : Nat) :
    eval shaFns
      (((e.bind "state" (.array_ (state0.map (fun x => PVal.int x.toNat)))).bind
        "w" (.array_ (wlist.map (fun x => PVal.int x.toNat)))).bind
        "k" (.array_ (Sha256Spec.k.map (fun x => PVal.int x.toNat))))
      (fuel + 76) compressBodyExpr
      = some (.array_ ((List.range 8).map (fun j =>
          PVal.int (state0.getD j 0 + (compressFold state0 wlist 64).getD j 0).toNat))) := by
  rw [compressBodyExpr, show fuel + 76 = (fuel + 75) + 1 by omega,
      eval_letIn _ _ _ _ _ _ _ (by simp [eval, Env.bind] :
        eval shaFns _ (fuel + 76) (.var "state")
          = some (.array_ (state0.map (fun x => PVal.int x.toNat)))),
      show fuel + 75 = (fuel + 74) + 1 by omega,
      eval_letIn _ _ _ _ _ _ (PVal.int 0) (by simp [eval]),
      stc_zero_eq e state0 wlist,
      show fuel + 74 = ((fuel + 0) + 9) + 64 + 1 by omega,
      compress_loop_eval e state0 wlist h0 hwl _ (fuel + 0)]
  exact ff_eval e state0 wlist h0 (fuel + 7)

/-- The compression body (`s := state; 64 rounds; Davies-Meyer feed-forward`)
    computes exactly `Sha256Spec.compress`, given the schedule `w` and round
    constants `k` already in scope. -/
theorem compress_body_refines_spec (e : Env) (state0 : List (BitVec 32)) (h0 : state0.length = 8)
    (block : List Sha256Spec.Byte) (fuel : Nat) :
    eval shaFns
      (((e.bind "state" (.array_ (state0.map (fun x => PVal.int x.toNat)))).bind
        "w" (.array_ ((Sha256Spec.expandSchedule (Sha256Spec.blockToWords block)).map
              (fun x => PVal.int x.toNat)))).bind
        "k" (.array_ (Sha256Spec.k.map (fun x => PVal.int x.toNat))))
      (fuel + 76) compressBodyExpr
      = some (.array_ ((Sha256Spec.compress state0 block).map (fun x => PVal.int x.toNat))) := by
  rw [compress_eq_feedforward, List.map_map]
  have hwl : (Sha256Spec.expandSchedule (Sha256Spec.blockToWords block)).length = 64 :=
    expandSchedule_length _ (by simp [Sha256Spec.blockToWords])
  have := compress_body_refines e state0 (Sha256Spec.expandSchedule (Sha256Spec.blockToWords block))
    h0 hwl fuel
  rw [this]
  rfl

-- ==================================================================
-- Compress composition wrappers (task #21, step 4 — part 1): the
-- function-call lemmas + glue that wire the proven refinements into a
-- full sha256_compress. Each call gets a clean bindArgs env, so these
-- have no env-shadowing issue; the final assembly (running the body
-- under an env that still carries the w16/block locals) needs an
-- eval-env-congruence lemma, tracked separately.
-- ==================================================================

set_option maxHeartbeats 1000000 in
theorem sha256_k_refines (env : Env) (fuel : Nat) :
    eval shaFns env (fuel + 2) sha256kExpr
      = some (.array_ (Sha256Spec.k.map (fun x => PVal.int x.toNat))) := by
  simp [sha256kExpr, eval, eval.evalElems, Sha256Spec.k]

theorem mapEnc_eq_rangeGetD (L : List (BitVec 32)) :
    L.map (fun x => PVal.int x.toNat)
      = (List.range L.length).map (fun j => PVal.int (L.getD j 0).toNat) := by
  apply List.ext_getElem (by simp)
  intro j h1 _
  simp only [List.length_map] at h1
  simp only [List.getElem_map, List.getElem_range,
    List.getD_eq_getElem?_getD, List.getElem?_eq_getElem h1, Option.getD_some]

theorem block_to_words_call (b : Nat → BitVec 8) (env : Env) (be : PExpr) (fuel : Nat)
    (hbe : eval shaFns env (fuel + 24) be = some (.array_ (blockArr b))) :
    eval shaFns env (fuel + 25) (.call "block_to_words" [be])
      = some (.array_ ((Sha256Spec.blockToWords ((List.range 64).map b)).map
          (fun x => PVal.int x.toNat))) := by
  simp only [eval, shaFns, eval.evalArgs, hbe, bindArgs]
  exact block_to_words_refines_spec shaFns b fuel

theorem schedule_call (wf : Nat → BitVec 32) (env : Env) (we : PExpr) (fuel : Nat)
    (hwe : eval shaFns env (fuel + 75) we = some (w16arr wf)) :
    eval shaFns env (fuel + 76) (.call "sha256_schedule" [we])
      = some (.array_ ((Sha256Spec.expandSchedule (w16of wf)).map (fun x => PVal.int x.toNat))) := by
  simp only [eval, shaFns, eval.evalArgs, hwe, bindArgs]
  exact sha256_schedule_refines_spec wf fuel

theorem sha256_k_call (env : Env) (fuel : Nat) :
    eval shaFns env (fuel + 3) (.call "sha256_k" [])
      = some (.array_ (Sha256Spec.k.map (fun x => PVal.int x.toNat))) := by
  simp only [eval, shaFns, eval.evalArgs, bindArgs]
  exact sha256_k_refines Env.empty fuel

-- ==================================================================
-- sha256_compress refines its spec (task #21, step 4 — COMPLETE): the
-- full extracted sha256_compress(state, block) — w16=block_to_words(block);
-- w=sha256_schedule(w16); k=sha256_k(); 64-round Davies-Meyer body —
-- computes exactly Sha256Spec.compress. Threads the three setup CALLS
-- (block_to_words_call/schedule_call/sha256_k_call) + the list->function
-- glue (cWf/w16_glue/cWf_w16of) and applies compress_body_refines_spec at
-- the right base env (env_reshape) — no eval_env_congr needed, thanks to
-- the base-env generalization of the compress-body chain.
-- ==================================================================

theorem list_self_rangeGetD (L : List (BitVec 32)) :
    (List.range L.length).map (fun j => L.getD j 0) = L := by
  apply List.ext_getElem (by simp)
  intro j h1 _
  simp only [List.length_map, List.length_range] at h1
  simp only [List.getElem_map, List.getElem_range]
  rw [List.getD_eq_getElem?_getD, List.getElem?_eq_getElem h1, Option.getD_some]

theorem blockToWords_length (bb : List Sha256Spec.Byte) :
    (Sha256Spec.blockToWords bb).length = 16 := by simp [Sha256Spec.blockToWords]

def cWf (b : Nat → BitVec 8) : Nat → BitVec 32 :=
  fun j => (Sha256Spec.blockToWords ((List.range 64).map b)).getD j 0
theorem cWf_w16of (b : Nat → BitVec 8) :
    w16of (cWf b) = Sha256Spec.blockToWords ((List.range 64).map b) := by
  show (List.range 16).map (cWf b) = Sha256Spec.blockToWords ((List.range 64).map b)
  rw [show (16:Nat) = (Sha256Spec.blockToWords ((List.range 64).map b)).length
        from (blockToWords_length _).symm]
  exact list_self_rangeGetD _

def cEnv0 (state0 : List (BitVec 32)) (b : Nat → BitVec 8) : Env :=
  (Env.empty.bind "state" (.array_ (state0.map (fun x => PVal.int x.toNat)))).bind
    "block" (.array_ (blockArr b))

theorem w16_glue (b : Nat → BitVec 8) (env : Env) (fuel : Nat)
    (h : env "w16" = some (.array_ ((Sha256Spec.blockToWords ((List.range 64).map b)).map
          (fun x => PVal.int x.toNat)))) :
    eval shaFns env (fuel + 1) (.var "w16") = some (w16arr (cWf b)) := by
  simp only [eval, h, w16arr, cWf]
  rw [mapEnc_eq_rangeGetD, blockToWords_length]

theorem env_reshape (state0 : List (BitVec 32)) (b : Nat → BitVec 8) (w16v wv kv : PVal) :
    (((cEnv0 state0 b).bind "w16" w16v).bind "w" wv).bind "k" kv
      = (((((cEnv0 state0 b).bind "w16" w16v).bind "state"
            (.array_ (state0.map (fun x => PVal.int x.toNat)))).bind "w" wv).bind "k" kv) := by
  funext n
  by_cases hk : n = "k" <;> by_cases hw : n = "w" <;> by_cases hst : n = "state" <;>
    by_cases hw16 : n = "w16" <;> by_cases hbl : n = "block" <;> simp_all [cEnv0, Env.bind]

def sha256_compressExpr : PExpr :=
  .letIn "w16" (.call "block_to_words" [.var "block"])
    (.letIn "w" (.call "sha256_schedule" [.var "w16"])
      (.letIn "k" (.call "sha256_k" []) compressBodyExpr))

set_option maxHeartbeats 2000000 in
theorem sha256_compress_refines_spec (state0 : List (BitVec 32)) (h0 : state0.length = 8)
    (b : Nat → BitVec 8) (fuel : Nat) :
    eval shaFns (cEnv0 state0 b) (fuel + 79) sha256_compressExpr
      = some (.array_ ((Sha256Spec.compress state0 ((List.range 64).map b)).map
          (fun x => PVal.int x.toNat))) := by
  have hwe : eval shaFns ((cEnv0 state0 b).bind "w16"
        (.array_ ((Sha256Spec.blockToWords ((List.range 64).map b)).map (fun x => PVal.int x.toNat))))
      (fuel + 77) (.var "w16") = some (w16arr (cWf b)) :=
    w16_glue b _ (fuel + 76) (by simp [cEnv0, Env.bind])
  rw [sha256_compressExpr, show fuel + 79 = (fuel + 78) + 1 by omega,
      eval_letIn _ _ _ _ _ _ _
        (block_to_words_call b (cEnv0 state0 b) (.var "block") (fuel + 54)
          (by simp [cEnv0, eval, Env.bind]))]
  rw [show fuel + 78 = (fuel + 77) + 1 by omega,
      eval_letIn _ _ _ _ _ _ _ (schedule_call (cWf b) _ (.var "w16") (fuel + 2) hwe)]
  rw [show fuel + 77 = (fuel + 76) + 1 by omega,
      eval_letIn _ _ _ _ _ _ _ (sha256_k_call _ (fuel + 74))]
  rw [cWf_w16of, env_reshape state0 b]
  exact compress_body_refines_spec
    ((cEnv0 state0 b).bind "w16"
      (.array_ ((Sha256Spec.blockToWords ((List.range 64).map b)).map (fun x => PVal.int x.toNat))))
    state0 h0 ((List.range 64).map b) fuel

-- ==================================================================
-- Spec-side multi-block hash fold (task #21, sha256_hash step 1): the
-- independent risk-check. Sha256Spec.hashState is a foldl of `compress`
-- over the padded blocks; its recurrence (hashFold_succ) falls out of
-- List.range_succ + foldl_append. (The HARD remaining part of
-- sha256_hash is the EVAL side: the offset compress_at variant, the
-- data-dependent block-count loop, and the padding writes at
-- data-dependent indices — not this spec fold.)
-- ==================================================================

def hashFold (padded : List Sha256Spec.Byte) (n : Nat) : List (BitVec 32) :=
  (List.range n).foldl (fun st blk => Sha256Spec.compress st (Sha256Spec.blockAt padded blk))
    Sha256Spec.initState

theorem hashFold_succ (padded : List Sha256Spec.Byte) (n : Nat) :
    hashFold padded (n+1)
      = Sha256Spec.compress (hashFold padded n) (Sha256Spec.blockAt padded n) := by
  unfold hashFold
  rw [List.range_succ, List.foldl_append]
  rfl

theorem hashState_eq_fold (message : List Sha256Spec.Byte) :
    Sha256Spec.hashState message
      = hashFold (Sha256Spec.padMessage message)
          ((Sha256Spec.padMessage message).length / 64) := rfl

-- ==================================================================
-- block_to_words_at refines its spec (task #21, sha256_hash step 2):
-- the OFFSET variant — w[k] = pack(buf[off+k*4 .. off+k*4+3]) — refines
-- Sha256Spec.blockToWords of the 64-byte slice at off, for any in-range
-- buffer and offset. Mirrors block_to_words (same eval_while_count +
-- the GENERIC set_in_counter_map frame lemma), with the offset threaded
-- through the packing reads. Foundation for sha256_compress_at.
-- ==================================================================

def bufArr (bf : Nat → BitVec 8) : List PVal := (List.range 384).map (fun j => PVal.int ↑(bf j).toNat)
def pwdAt (bf : Nat → BitVec 8) (off k : Nat) : BitVec 32 :=
  Sha256Spec.packWord (bf (off+4*k)) (bf (off+4*k+1)) (bf (off+4*k+2)) (bf (off+4*k+3))

theorem readNbuf (bf : Nat → BitVec 8) (c : Int) (m : Nat) (hc : c = (m:Int)) (hm : m < 384) :
    (if c < 0 then (none : Option PVal) else eval.lookupIndex (bufArr bf) c.toNat)
      = some (.int ↑(bf m).toNat) := by
  subst hc
  rw [if_neg (by omega), show ((m:Int)).toNat = m by omega]
  simp only [bufArr]; exact lookupIndex_range_map _ 384 m hm

def bIdx0 : PExpr := .binOp .add (.var "off") (.binOp .mul (.var "i") (.lit (.int 4)))
def bIdxO (o : Int) : PExpr := .binOp .add bIdx0 (.lit (.int o))
def packExprAt : PExpr :=
  .binOp (.bitor 32 false)
    (.binOp (.bitor 32 false)
      (.binOp (.bitor 32 false)
        (.binOp (.shl 32 false) (.cast (.arrayIndex (.var "buf") bIdx0)) (.lit (.int 24)))
        (.binOp (.shl 32 false) (.cast (.arrayIndex (.var "buf") (bIdxO 1))) (.lit (.int 16))))
      (.binOp (.shl 32 false) (.cast (.arrayIndex (.var "buf") (bIdxO 2))) (.lit (.int 8))))
    (.cast (.arrayIndex (.var "buf") (bIdxO 3)))

set_option linter.unusedSimpArgs false in
theorem packExprAt_eval (bf : Nat → BitVec 8) (env : Env) (off k : Nat)
    (hk : k < 16) (hoff : off + 64 ≤ 384) (fuel : Nat)
    (hb : env "buf" = some (.array_ (bufArr bf)))
    (hoffv : env "off" = some (.int (off:Int)))
    (hi : env "i" = some (.int (k:Int))) :
    eval (fun _ => none) env (fuel + 4) packExprAt
      = some (.int (pwdAt bf off k).toNat) := by
  simp only [packExprAt, bIdx0, bIdxO, eval, evalBinOp, hb, hoffv, hi]
  rw [readNbuf bf ((off:Int) + (k:Int)*4) (off+4*k) (by omega) (by omega),
      readNbuf bf ((off:Int) + (k:Int)*4 + 1) (off+4*k+1) (by omega) (by omega),
      readNbuf bf ((off:Int) + (k:Int)*4 + 2) (off+4*k+2) (by omega) (by omega),
      readNbuf bf ((off:Int) + (k:Int)*4 + 3) (off+4*k+3) (by omega) (by omega)]
  simp only [pwdAt, Sha256Spec.packWord, ofInt32_byte, ofInt32_byte_cast,
    ofInt_ofNat_toNat, ofInt_natCast_toNat, Option.some.injEq, PVal.int.injEq,
    Int.ofNat_eq_natCast, Int.natCast_inj, BitVec.toNat_inj]
  bv_decide

def sliceAt (bf : Nat → BitVec 8) (off : Nat) : List Sha256Spec.Byte :=
  (List.range 64).map (fun j => bf (off + j))
theorem sliceAt_getD (bf) (off m : Nat) (hm : m < 64) : (sliceAt bf off).getD m 0 = bf (off + m) := by
  rw [List.getD_eq_getElem?_getD]; simp [sliceAt, hm]

def wListAt (bf : Nat → BitVec 8) (off m : Nat) : List PVal :=
  (List.range 16).map (fun j => if j < m then PVal.int ↑(pwdAt bf off j).toNat else PVal.int 0)
theorem wListAt_length (bf off m) : (wListAt bf off m).length = 16 := by simp [wListAt]
theorem wListAt_set (bf : Nat → BitVec 8) (off m : Nat) :
    (wListAt bf off m).set m (PVal.int ↑(pwdAt bf off m).toNat) = wListAt bf off (m + 1) :=
  set_in_counter_map 16 (fun j => PVal.int ↑(pwdAt bf off j).toNat) (PVal.int 0) m
theorem wListAt_zero (bf off) : wListAt bf off 0 = List.replicate 16 (PVal.int 0) := by
  simp [wListAt]; rfl
theorem wListAt16_spec (bf : Nat → BitVec 8) (off : Nat) :
    wListAt bf off 16 = (Sha256Spec.blockToWords (sliceAt bf off)).map (fun w => PVal.int w.toNat) := by
  apply List.ext_getElem (by simp [wListAt, Sha256Spec.blockToWords])
  intro j h1 _
  simp only [wListAt, List.length_map, List.length_range] at h1
  simp only [wListAt, Sha256Spec.blockToWords, List.getElem_map, List.getElem_range, if_pos h1, pwdAt]
  rw [sliceAt_getD bf off (4*j) (by omega), sliceAt_getD bf off (4*j+1) (by omega),
      sliceAt_getD bf off (4*j+2) (by omega), sliceAt_getD bf off (4*j+3) (by omega)]
  congr 2 <;> omega

def stEnvAt (bf : Nat → BitVec 8) (off m : Nat) : Env :=
  (((Env.empty.bind "buf" (.array_ (bufArr bf))).bind "off" (.int (off:Int))).bind
    "w" (.array_ (wListAt bf off m))).bind "i" (.int (m:Int))
def condAt : PExpr := .binOp .lt (.var "i") (.lit (.int 16))
def assignsAt : List (String × PExpr) :=
  [ ("w", .arraySet (.var "w") (.var "i") packExprAt)
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]

theorem stepAt (bf : Nat → BitVec 8) (off m : Nat) (hm : m < 16) (hoff : off + 64 ≤ 384) (fuel : Nat) :
    eval.evalAssigns (fun _ => none) (stEnvAt bf off m) (fuel + 5) assignsAt
      = some (stEnvAt bf off (m + 1)) := by
  have hbf : (stEnvAt bf off m) "buf" = some (.array_ (bufArr bf)) := by simp [stEnvAt, Env.bind]
  have hoffv : (stEnvAt bf off m) "off" = some (.int (off:Int)) := by simp [stEnvAt, Env.bind]
  have hi : (stEnvAt bf off m) "i" = some (.int (m:Int)) := by simp [stEnvAt, Env.bind]
  have hpack := packExprAt_eval bf (stEnvAt bf off m) off m hm hoff fuel hbf hoffv hi
  have hwv : eval (fun _ => none) (stEnvAt bf off m) (fuel + 4) (.var "w")
      = some (.array_ (wListAt bf off m)) := by simp [eval, stEnvAt, Env.bind]
  have hiv : eval (fun _ => none) (stEnvAt bf off m) (fuel + 4) (.var "i")
      = some (.int (m:Int)) := by simp [eval, stEnvAt, Env.bind]
  have harr : eval (fun _ => none) (stEnvAt bf off m) (fuel + 5)
      (.arraySet (.var "w") (.var "i") packExprAt) = some (.array_ (wListAt bf off (m+1))) := by
    rw [eval_arraySet_lemma (fun _ => none) (stEnvAt bf off m) (fuel + 4)
        (.var "w") (.var "i") packExprAt (wListAt bf off m) (m:Int) (.int (pwdAt bf off m).toNat)
        hwv hiv hpack (by omega) (by rw [wListAt_length]; simp; omega)]
    rw [show ((m:Int)).toNat = m by omega, wListAt_set bf off m]
  simp only [assignsAt, eval.evalAssigns, harr]
  simp only [eval, evalBinOp, Env.bind, stEnvAt, beq_self_eq_true, if_true, beq_iff_eq, if_false,
    String.reduceEq, reduceCtorEq, Option.some.injEq]
  funext n
  by_cases h1 : (n == "i") = true <;> by_cases h2 : (n == "w") = true <;>
    simp_all [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq,
      reduceCtorEq, Option.some.injEq, PVal.int.injEq] <;> omega

theorem condAt_true (bf off m) (hm : m < 16) (F) :
    eval (fun _ => none) (stEnvAt bf off m) (F + 1) condAt = some (.bool true) := by
  simp only [condAt, stEnvAt, eval, Env.bind, evalBinOp]; simp; omega
theorem condAt_false (bf off) (F) :
    eval (fun _ => none) (stEnvAt bf off 16) (F + 1) condAt = some (.bool false) := by
  simp only [condAt, stEnvAt, eval, Env.bind, evalBinOp]; simp

theorem loop_evalAt (bf : Nat → BitVec 8) (off : Nat) (hoff : off + 64 ≤ 384) (cont : PExpr) (base : Nat) :
    eval (fun _ => none) (stEnvAt bf off 0) ((base + 5) + 16 + 1) (.while_ condAt assignsAt cont)
      = eval (fun _ => none) (stEnvAt bf off 16) (base + 5) cont :=
  eval_while_count (fun _ => none) condAt assignsAt cont (stEnvAt bf off) 16 (base + 5)
    (fun m hm => ⟨condAt_true bf off m hm (base + 4), stepAt bf off m hm hoff base⟩)
    (condAt_false bf off (base + 4))

def blockToWordsAtExpr : PExpr :=
  .letIn "w" (.arrayLit (List.replicate 16 (.lit (.int 0))))
    (.letIn "i" (.lit (.int 0)) (.while_ condAt assignsAt (.var "w")))

theorem stAt_zero_eq (bf : Nat → BitVec 8) (off : Nat) :
    (((((Env.empty.bind "buf" (.array_ (bufArr bf))).bind "off" (.int (off:Int))).bind
      "w" (.array_ (wListAt bf off 0))).bind "i" (.int 0)))
      = stEnvAt bf off 0 := rfl

set_option maxHeartbeats 1000000 in
theorem block_to_words_at_refines_spec (bf : Nat → BitVec 8) (off : Nat)
    (hoff : off + 64 ≤ 384) (fuel : Nat) :
    eval (fun _ => none)
      ((Env.empty.bind "buf" (.array_ (bufArr bf))).bind "off" (.int (off:Int)))
      (fuel + 24) blockToWordsAtExpr
    = some (.array_ ((Sha256Spec.blockToWords (sliceAt bf off)).map (fun w => PVal.int w.toNat))) := by
  rw [← wListAt16_spec, blockToWordsAtExpr, show fuel + 24 = (fuel + 23) + 1 by omega,
      eval_letIn _ _ _ _ _ _ _ (arrayLit_zeros_eval _ _ (fuel + 22))]
  rw [show fuel + 23 = (fuel + 22) + 1 by omega, eval_letIn _ _ _ _ _ _ (PVal.int 0) (by simp [eval])]
  rw [← wListAt_zero bf off]
  rw [stAt_zero_eq]
  rw [show fuel + 22 = ((fuel + 5) + 16 + 1) by omega, loop_evalAt bf off hoff _ fuel]
  simp [eval, stEnvAt, Env.bind]
end Concrete.Proof
