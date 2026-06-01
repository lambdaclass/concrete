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

/-- The SHA-256 helper function table: the extracted bodies of `rotr`,
    `ch`, `maj`, and the four sigmas, keyed by their source names. -/
def shaFns : FnTable
  | "rotr"         => some ⟨"rotr",         ["x", "n"],      rotrExpr⟩
  | "ch"           => some ⟨"ch",           ["x", "y", "z"], chExpr⟩
  | "maj"          => some ⟨"maj",          ["x", "y", "z"], majExpr⟩
  | "big_sigma0"   => some ⟨"big_sigma0",   ["x"],           bigSigma0Expr⟩
  | "big_sigma1"   => some ⟨"big_sigma1",   ["x"],           bigSigma1Expr⟩
  | "small_sigma0" => some ⟨"small_sigma0", ["x"],           smallSigma0Expr⟩
  | "small_sigma1" => some ⟨"small_sigma1", ["x"],           smallSigma1Expr⟩
  | _              => none

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

theorem wschedList_set (wf) (m : Nat) :
    (wschedList wf m).set m (PVal.int ↑(swd wf m).toNat) = wschedList wf (m + 1) := by
  apply List.ext_getElem (by simp [wschedList])
  intro i h1 _
  by_cases hmi : m = i
  · subst hmi
    simp only [wschedList, List.getElem_set_self, List.getElem_map, List.getElem_range]
    simp
  · rw [List.getElem_set_ne hmi]
    simp only [wschedList, List.getElem_map, List.getElem_range]
    rcases Nat.lt_or_ge i m with h | h
    · rw [if_pos h, if_pos (by omega)]
    · rw [if_neg (by omega), if_neg (by omega)]

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
def addwS (a b : PExpr) : PExpr := .binOp (.addw 32 false) a b
def wIdx (c : Int) : PExpr := .arrayIndex (.var "w") (.binOp .sub (.var "i") (.lit (.int c)))
def expansionExpr : PExpr :=
  addwS (addwS (addwS (.call "small_sigma1" [wIdx 2]) (wIdx 7))
          (.call "small_sigma0" [wIdx 15])) (wIdx 16)

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

def assigns2 : List (String × PExpr) :=
  [ ("w", .arraySet (.var "w") (.var "i") expansionExpr)
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]

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
def assigns1 : List (String × PExpr) :=
  [ ("w", .arraySet (.var "w") (.var "i") (.arrayIndex (.var "w16") (.var "i")))
  , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
def condE (bound : Int) : PExpr := .binOp .lt (.var "i") (.lit (.int bound))

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

def scheduleExpr : PExpr :=
  .letIn "w" (.arrayLit (List.replicate 64 (.lit (.int 0))))
    (.letIn "i" (.lit (.int 0))
      (.while_ (condE 16) assigns1
        (.letIn "i" (.lit (.int 16))
          (.while_ (condE 64) assigns2 (.var "w")))))

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
end Concrete.Proof
