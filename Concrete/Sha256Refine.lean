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
import Concrete.ProofKit.Eval
import Concrete.ProofKit.BitVec
import Concrete.ProofKit.Array
import Concrete.ProofKit.Refinement
import Concrete.Sha256Spec

namespace Concrete.Proof

-- ------------------------------------------------------------------
-- Bridging lemmas: a 32-bit word survives the round-trip through the
-- evaluator's `Int` representation. The evaluator binds a `u32` value
-- as an `Int` and re-reads it with `BitVec.ofInt 32`; both the env
-- binding (`Nat.cast`) and the per-op result reconstruction
-- (`Int.ofNat`) wrap a `BitVec.toNat`, and both collapse to identity.
-- ------------------------------------------------------------------

-- `ofInt_natCast_toNat`, `ofInt_ofNat_toNat` relocated to
-- `Concrete.ProofKit.BitVec` (Proof Kit v1).

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
-- `lookupIndex_eq`, `lookupIndex_range_map`, `ofInt32_byte`,
-- `ofInt32_byte_cast` relocated to `Concrete.ProofKit.BitVec` (Proof Kit v1).

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

-- `idx0`, `idxO`, `packExpr`, `cond_e`, `assigns_e`, `blockToWordsExpr`
-- are now defined in `Concrete.Proof` (relocated for the spec-drift gate,
-- task #22) in exact extracted shape — `idxO o = add (lit o) (mul i 4)`.

set_option linter.unusedSimpArgs false in
/-- The loop body's packing expression evaluates to the spec packed
    word `pwd b k`, in any env that binds `block` and `i` suitably. -/
theorem packExpr_eval (fns : FnTable) (b : Nat → BitVec 8) (env : Env) (k : Nat) (hk : k < 16) (fuel : Nat)
    (hb : env "block" = some (.array_ (blockArr b)))
    (hi : env "i" = some (.int (k:Int))) :
    eval fns env (fuel + 4) packExpr
      = some (.int (pwd b k).toNat) := by
  simp only [packExpr, idx0, idxO, blockArr, pwd, eval, evalBinOp, hb, hi]
  rw [show ((1:Int) + (k:Int) * 4) = ((4*k+1 : Nat) : Int) by omega,
     show ((2:Int) + (k:Int) * 4) = ((4*k+2 : Nat) : Int) by omega,
     show ((3:Int) + (k:Int) * 4) = ((4*k+3 : Nat) : Int) by omega,
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

-- `set_in_counter_map` (generic counter-loop array-update frame lemma)
-- relocated to `Concrete.ProofKit.Array` (Proof Kit v1).

/-- Setting slot `k` of `wList b k` to the packed word `pwd b k` gives
    `wList b (k+1)` — instance of the generic frame lemma. -/
theorem wList_set (b : Nat → BitVec 8) (k : Nat) :
    (wList b k).set k (PVal.int ↑(pwd b k).toNat) = wList b (k + 1) :=
  set_in_counter_map 16 (fun j => PVal.int ↑(pwd b j).toNat) (PVal.int 0) k

-- ===== the loop expression (cond_e/assigns_e relocated to Concrete.Proof) =====
theorem cond_true (fns : FnTable) (b : Nat → BitVec 8) (k : Nat) (hk : k < 16) (base : Nat) :
    eval fns (stEnv b k) (base + 1) cond_e = some (.bool true) := by
  simp only [cond_e, stEnv, eval, Env.bind, evalBinOp]
  simp; omega

theorem cond_false (fns : FnTable) (b : Nat → BitVec 8) (base : Nat) :
    eval fns (stEnv b 16) (base + 1) cond_e = some (.bool false) := by
  simp only [cond_e, stEnv, eval, Env.bind, evalBinOp]
  simp

-- `eval_arraySet_lemma` relocated to `Concrete.ProofKit.Array` (Proof Kit v1).
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

-- ===== assembling the full loop (blockToWordsExpr relocated to Concrete.Proof) =====
-- `eval_letIn`, `evalElems_replicate_lit`, and the generic
-- `arrayLit_replicate_eval` relocated to `Concrete.ProofKit.Eval` (Proof Kit
-- v1). The per-size zero-array helpers below are now thin aliases of it.
theorem arrayLit_zeros_eval (fns : FnTable) (env : Env) (fuel : Nat) :
    eval fns env (fuel + 2) (.arrayLit (List.replicate 16 (.lit (.int 0))))
      = some (.array_ (List.replicate 16 (PVal.int 0))) :=
  arrayLit_replicate_eval fns env fuel 16 (.int 0)

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

-- `addwE`, `stateAt`, `roundExpr` (sha256_round) and `addwS`, `wIdx`,
-- `expansionExpr`, `assigns1`, `assigns2`, `condE`, `scheduleExpr`
-- (sha256_schedule) are relocated to `Concrete.Proof` (task #22, verbatim —
-- exact extracted shape). `sha256kExpr` (a leaf, called by name) stays here.
/-- Extracted `sha256_k()` body: the 64 round constants K[0..63]. -/
def sha256kExpr : PExpr := .arrayLit [.lit (.int 0x428a2f98), .lit (.int 0x71374491), .lit (.int 0xb5c0fbcf), .lit (.int 0xe9b5dba5), .lit (.int 0x3956c25b), .lit (.int 0x59f111f1), .lit (.int 0x923f82a4), .lit (.int 0xab1c5ed5), .lit (.int 0xd807aa98), .lit (.int 0x12835b01), .lit (.int 0x243185be), .lit (.int 0x550c7dc3), .lit (.int 0x72be5d74), .lit (.int 0x80deb1fe), .lit (.int 0x9bdc06a7), .lit (.int 0xc19bf174), .lit (.int 0xe49b69c1), .lit (.int 0xefbe4786), .lit (.int 0xfc19dc6), .lit (.int 0x240ca1cc), .lit (.int 0x2de92c6f), .lit (.int 0x4a7484aa), .lit (.int 0x5cb0a9dc), .lit (.int 0x76f988da), .lit (.int 0x983e5152), .lit (.int 0xa831c66d), .lit (.int 0xb00327c8), .lit (.int 0xbf597fc7), .lit (.int 0xc6e00bf3), .lit (.int 0xd5a79147), .lit (.int 0x6ca6351), .lit (.int 0x14292967), .lit (.int 0x27b70a85), .lit (.int 0x2e1b2138), .lit (.int 0x4d2c6dfc), .lit (.int 0x53380d13), .lit (.int 0x650a7354), .lit (.int 0x766a0abb), .lit (.int 0x81c2c92e), .lit (.int 0x92722c85), .lit (.int 0xa2bfe8a1), .lit (.int 0xa81a664b), .lit (.int 0xc24b8b70), .lit (.int 0xc76c51a3), .lit (.int 0xd192e819), .lit (.int 0xd6990624), .lit (.int 0xf40e3585), .lit (.int 0x106aa070), .lit (.int 0x19a4c116), .lit (.int 0x1e376c08), .lit (.int 0x2748774c), .lit (.int 0x34b0bcb5), .lit (.int 0x391c0cb3), .lit (.int 0x4ed8aa4a), .lit (.int 0x5b9cca4f), .lit (.int 0x682e6ff3), .lit (.int 0x748f82ee), .lit (.int 0x78a5636f), .lit (.int 0x84c87814), .lit (.int 0x8cc70208), .lit (.int 0x90befffa), .lit (.int 0xa4506ceb), .lit (.int 0xbef9a3f7), .lit (.int 0xc67178f2)]

-- `bIdx0`, `bIdxO`, `packExprAt`, `condAt`, `assignsAt`, `blockToWordsAtExpr`
-- relocated to `Concrete.Proof` (task #22) in exact extracted shape: the
-- source loop variable is `k` and the offset index is lit-first
-- (`bIdxO o = add (lit o) (add off (mul k 4))`).

/-- The SHA-256 helper function table: the extracted bodies of `rotr`,
    `ch`, `maj`, the four sigmas, and the compression `round`, keyed by
    their source names. -/
-- `assignsC`, `condC`, `aw2`, `ix`, `feedforwardExpr`, `compressBodyExpr`,
-- `sha256_compressExpr`, `sha256_compressAtExpr` relocated to `Concrete.Proof`
-- (task #22, verbatim). The compress body is shared by both variants.

-- state_to_bytes / sha256_init expression data, relocated above `shaFns`
-- so both are table entries the `sha256_hash` body and its return resolve.
-- `sbStore`, `sbStore3`, `oIdx0`, `oIdxK`, `condS`, `assignsS`,
-- `stateToBytesExpr` (state_to_bytes) and `condH`, `assignsH`, `lenStore0`,
-- `lenStoreS`, `sha256_hashExpr` (sha256_hash) relocated to `Concrete.Proof`
-- (task #22, verbatim).

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
  | "sha256_k"          => some ⟨"sha256_k",          [],                   sha256kExpr⟩
  | "block_to_words_at" => some ⟨"block_to_words_at", ["buf", "off"],      blockToWordsAtExpr⟩
  | "sha256_compress_at" => some ⟨"sha256_compress_at", ["state", "buf", "off"], sha256_compressAtExpr⟩
  | "sha256_init"        => some ⟨"sha256_init",        [],                   sha256_initExpr⟩
  | "state_to_bytes"     => some ⟨"state_to_bytes",     ["state"],            stateToBytesExpr⟩
  | "sha256_hash"        => some ⟨"sha256_hash",        ["data", "len"],      sha256_hashExpr⟩
  | _                   => none

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

-- `sha256_compressExpr` relocated to `Concrete.Proof` (task #22, verbatim).

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


set_option linter.unusedSimpArgs false in
theorem packExprAt_eval (fns : FnTable) (bf : Nat → BitVec 8) (env : Env) (off k : Nat)
    (hk : k < 16) (hoff : off + 64 ≤ 384) (fuel : Nat)
    (hb : env "buf" = some (.array_ (bufArr bf)))
    (hoffv : env "off" = some (.int (off:Int)))
    (hi : env "k" = some (.int (k:Int))) :
    eval fns env (fuel + 4) packExprAt
      = some (.int (pwdAt bf off k).toNat) := by
  simp only [packExprAt, bIdx0, bIdxO, eval, evalBinOp, hb, hoffv, hi]
  rw [readNbuf bf ((off:Int) + (k:Int)*4) (off+4*k) (by omega) (by omega),
      readNbuf bf ((1:Int) + ((off:Int) + (k:Int)*4)) (off+4*k+1) (by omega) (by omega),
      readNbuf bf ((2:Int) + ((off:Int) + (k:Int)*4)) (off+4*k+2) (by omega) (by omega),
      readNbuf bf ((3:Int) + ((off:Int) + (k:Int)*4)) (off+4*k+3) (by omega) (by omega)]
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
    "w" (.array_ (wListAt bf off m))).bind "k" (.int (m:Int))

theorem stepAt (fns : FnTable) (bf : Nat → BitVec 8) (off m : Nat) (hm : m < 16) (hoff : off + 64 ≤ 384) (fuel : Nat) :
    eval.evalAssigns fns (stEnvAt bf off m) (fuel + 5) assignsAt
      = some (stEnvAt bf off (m + 1)) := by
  have hbf : (stEnvAt bf off m) "buf" = some (.array_ (bufArr bf)) := by simp [stEnvAt, Env.bind]
  have hoffv : (stEnvAt bf off m) "off" = some (.int (off:Int)) := by simp [stEnvAt, Env.bind]
  have hi : (stEnvAt bf off m) "k" = some (.int (m:Int)) := by simp [stEnvAt, Env.bind]
  have hpack := packExprAt_eval fns bf (stEnvAt bf off m) off m hm hoff fuel hbf hoffv hi
  have hwv : eval fns (stEnvAt bf off m) (fuel + 4) (.var "w")
      = some (.array_ (wListAt bf off m)) := by simp [eval, stEnvAt, Env.bind]
  have hiv : eval fns (stEnvAt bf off m) (fuel + 4) (.var "k")
      = some (.int (m:Int)) := by simp [eval, stEnvAt, Env.bind]
  have harr : eval fns (stEnvAt bf off m) (fuel + 5)
      (.arraySet (.var "w") (.var "k") packExprAt) = some (.array_ (wListAt bf off (m+1))) := by
    rw [eval_arraySet_lemma fns (stEnvAt bf off m) (fuel + 4)
        (.var "w") (.var "k") packExprAt (wListAt bf off m) (m:Int) (.int (pwdAt bf off m).toNat)
        hwv hiv hpack (by omega) (by rw [wListAt_length]; simp; omega)]
    rw [show ((m:Int)).toNat = m by omega, wListAt_set bf off m]
  simp only [assignsAt, eval.evalAssigns, harr]
  simp only [eval, evalBinOp, Env.bind, stEnvAt, beq_self_eq_true, if_true, beq_iff_eq, if_false,
    String.reduceEq, reduceCtorEq, Option.some.injEq]
  funext n
  by_cases h1 : (n == "k") = true <;> by_cases h2 : (n == "w") = true <;>
    simp_all [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq,
      reduceCtorEq, Option.some.injEq, PVal.int.injEq] <;> omega

theorem condAt_true (fns : FnTable) (bf off m) (hm : m < 16) (F) :
    eval fns (stEnvAt bf off m) (F + 1) condAt = some (.bool true) := by
  simp only [condAt, stEnvAt, eval, Env.bind, evalBinOp]; simp; omega
theorem condAt_false (fns : FnTable) (bf off) (F) :
    eval fns (stEnvAt bf off 16) (F + 1) condAt = some (.bool false) := by
  simp only [condAt, stEnvAt, eval, Env.bind, evalBinOp]; simp

theorem loop_evalAt (fns : FnTable) (bf : Nat → BitVec 8) (off : Nat) (hoff : off + 64 ≤ 384) (cont : PExpr) (base : Nat) :
    eval fns (stEnvAt bf off 0) ((base + 5) + 16 + 1) (.while_ condAt assignsAt cont)
      = eval fns (stEnvAt bf off 16) (base + 5) cont :=
  eval_while_count fns condAt assignsAt cont (stEnvAt bf off) 16 (base + 5)
    (fun m hm => ⟨condAt_true fns bf off m hm (base + 4), stepAt fns bf off m hm hoff base⟩)
    (condAt_false fns bf off (base + 4))


theorem stAt_zero_eq (bf : Nat → BitVec 8) (off : Nat) :
    (((((Env.empty.bind "buf" (.array_ (bufArr bf))).bind "off" (.int (off:Int))).bind
      "w" (.array_ (wListAt bf off 0))).bind "k" (.int 0)))
      = stEnvAt bf off 0 := rfl

set_option maxHeartbeats 1000000 in
theorem block_to_words_at_refines_spec (fns : FnTable) (bf : Nat → BitVec 8) (off : Nat)
    (hoff : off + 64 ≤ 384) (fuel : Nat) :
    eval fns
      ((Env.empty.bind "buf" (.array_ (bufArr bf))).bind "off" (.int (off:Int)))
      (fuel + 24) blockToWordsAtExpr
    = some (.array_ ((Sha256Spec.blockToWords (sliceAt bf off)).map (fun w => PVal.int w.toNat))) := by
  rw [← wListAt16_spec, blockToWordsAtExpr, show fuel + 24 = (fuel + 23) + 1 by omega,
      eval_letIn _ _ _ _ _ _ _ (arrayLit_zeros_eval _ _ (fuel + 22))]
  rw [show fuel + 23 = (fuel + 22) + 1 by omega, eval_letIn _ _ _ _ _ _ (PVal.int 0) (by simp [eval])]
  rw [← wListAt_zero bf off]
  rw [stAt_zero_eq]
  rw [show fuel + 22 = ((fuel + 5) + 16 + 1) by omega, loop_evalAt fns bf off hoff _ fuel]
  simp [eval, stEnvAt, Env.bind]

/-- A call `block_to_words_at(bufE, offE)` reduces to blockToWords of the slice
    at `off`. The bridge from the offset call site to its refinement. -/
theorem block_to_words_at_call (bf : Nat → BitVec 8) (off : Nat) (hoff : off + 64 ≤ 384)
    (env : Env) (bufE offE : PExpr) (fuel : Nat)
    (hbuf : eval shaFns env (fuel + 24) bufE = some (.array_ (bufArr bf)))
    (hoffe : eval shaFns env (fuel + 24) offE = some (.int (off:Int))) :
    eval shaFns env (fuel + 25) (.call "block_to_words_at" [bufE, offE])
      = some (.array_ ((Sha256Spec.blockToWords (sliceAt bf off)).map (fun w => PVal.int w.toNat))) := by
  simp only [eval, shaFns, eval.evalArgs, hbuf, hoffe, bindArgs]
  exact block_to_words_at_refines_spec shaFns bf off hoff fuel

-- ==================================================================
-- sha256_compress_at refines its spec (task #21, sha256_hash step 4):
-- the OFFSET compression `w = schedule(block_to_words_at(buf,off)); k;
-- 64-round body` computes exactly Sha256Spec.compress of the 64-byte
-- slice at off. Mirrors sha256_compress with the nested
-- schedule∘block_to_words_at call. This is the per-block step the
-- multi-block sha256_hash loop invokes.
-- ==================================================================

def cWfAt (bf : Nat → BitVec 8) (off : Nat) : Nat → BitVec 32 :=
  fun j => (Sha256Spec.blockToWords (sliceAt bf off)).getD j 0
theorem cWfAt_w16of (bf : Nat → BitVec 8) (off : Nat) :
    w16of (cWfAt bf off) = Sha256Spec.blockToWords (sliceAt bf off) := by
  show (List.range 16).map (cWfAt bf off) = Sha256Spec.blockToWords (sliceAt bf off)
  rw [show (16:Nat) = (Sha256Spec.blockToWords (sliceAt bf off)).length
        from (blockToWords_length _).symm]
  exact list_self_rangeGetD _

def cEnv0At (state0 : List (BitVec 32)) (bf : Nat → BitVec 8) (off : Nat) : Env :=
  ((Env.empty.bind "state" (.array_ (state0.map (fun x => PVal.int x.toNat)))).bind
    "buf" (.array_ (bufArr bf))).bind "off" (.int (off:Int))

theorem env_reshapeAt (state0 : List (BitVec 32)) (bf : Nat → BitVec 8) (off : Nat) (wv kv : PVal) :
    ((cEnv0At state0 bf off).bind "w" wv).bind "k" kv
      = ((((cEnv0At state0 bf off).bind "state"
            (.array_ (state0.map (fun x => PVal.int x.toNat)))).bind "w" wv).bind "k" kv) := by
  funext n
  by_cases hk : n = "k" <;> by_cases hw : n = "w" <;> by_cases hst : n = "state" <;>
    by_cases hb : n = "buf" <;> by_cases ho : n = "off" <;> simp_all [cEnv0At, Env.bind]


set_option maxHeartbeats 2000000 in
theorem sha256_compress_at_refines_spec (state0 : List (BitVec 32)) (h0 : state0.length = 8)
    (bf : Nat → BitVec 8) (off : Nat) (hoff : off + 64 ≤ 384) (fuel : Nat) :
    eval shaFns (cEnv0At state0 bf off) (fuel + 78) sha256_compressAtExpr
      = some (.array_ ((Sha256Spec.compress state0 (sliceAt bf off)).map (fun x => PVal.int x.toNat))) := by
  have hwe : eval shaFns (cEnv0At state0 bf off) (fuel + 77)
      (.call "block_to_words_at" [.var "buf", .var "off"]) = some (w16arr (cWfAt bf off)) := by
    rw [block_to_words_at_call bf off hoff (cEnv0At state0 bf off) (.var "buf") (.var "off")
        (fuel + 52) (by simp [cEnv0At, eval, Env.bind]) (by simp [cEnv0At, eval, Env.bind])]
    simp only [w16arr, cWfAt]
    rw [mapEnc_eq_rangeGetD, blockToWords_length]
  rw [sha256_compressAtExpr, show fuel + 78 = (fuel + 77) + 1 by omega,
      eval_letIn _ _ _ _ _ _ _ (schedule_call (cWfAt bf off) _ _ (fuel + 2) hwe)]
  rw [show fuel + 77 = (fuel + 76) + 1 by omega,
      eval_letIn _ _ _ _ _ _ _ (sha256_k_call _ (fuel + 74))]
  rw [cWfAt_w16of, env_reshapeAt]
  exact compress_body_refines_spec (cEnv0At state0 bf off) state0 h0 (sliceAt bf off) fuel
-- ==================================================================
-- FIPS 180-4 padding (task #21, sha256_hash steps 1-3): the byte
-- function after sha256_hash's five padding stores agrees, on the
-- whole padded prefix [0, plen), with Sha256Spec.padMessage. This is
-- the spec-side + buffer-update model. `bufArr_set` bridges the
-- evaluator's List.set to a point-update on the byte function;
-- `padFn`/`padFn_eq` characterize the post-store buffer; `sliceAt_padFn`
-- delivers each 64-byte block in the exact `blockAt` shape the
-- multi-block hash loop (via sha256_compress_at) consumes.
-- ==================================================================

-- `bufUpd` relocated to `Concrete.ProofKit.Array` (Proof Kit v1).

theorem bufArr_set (bf : Nat → BitVec 8) (i : Nat) (v : BitVec 8) :
    (bufArr bf).set i (PVal.int v.toNat) = bufArr (bufUpd bf i v) := by
  apply List.ext_getElem (by simp [bufArr])
  intro m h1 _
  simp only [bufArr, List.length_map, List.length_range] at h1
  rw [List.getElem_set]
  simp only [bufArr, List.getElem_map, List.getElem_range, bufUpd]
  by_cases hmi : m = i
  · simp [hmi]
  · simp [hmi, Ne.symm hmi]

-- ---- spec-side padding scaffolding ----
def plenOf (len : Nat) : Nat := ((len + 9 + 63) / 64) * 64

theorem plen_ge (len : Nat) : plenOf len ≥ len + 9 := by unfold plenOf; omega

theorem ofNat64_eq_setWidth32 (n : Nat) (h : n < 2^32) :
    BitVec.ofNat 64 n = (BitVec.ofNat 32 n).setWidth 64 := by
  apply BitVec.toNat_inj.mp
  simp only [BitVec.toNat_ofNat, BitVec.toNat_setWidth]
  omega

theorem be64_getD (n i : Nat) (hi : i < 8) :
    (Sha256Spec.be64 n).getD i 0 = (BitVec.ofNat 64 n >>> (8 * (7 - i))).setWidth 8 := by
  simp [Sha256Spec.be64, List.getD_eq_getElem?_getD, List.getElem?_range, hi]

theorem be64_high_zero (n : Nat) (hn : n < 2^32) (i : Nat) (hi : i < 4) :
    (Sha256Spec.be64 n).getD i 0 = 0 := by
  rw [be64_getD n i (by omega), ofNat64_eq_setWidth32 n hn]
  have : i = 0 ∨ i = 1 ∨ i = 2 ∨ i = 3 := by omega
  rcases this with rfl|rfl|rfl|rfl <;> bv_decide

theorem be64_low_byte (n : Nat) (hn : n < 2^32) (s : Nat) (hs : s < 4) :
    (Sha256Spec.be64 n).getD (4 + s) 0 = ((BitVec.ofNat 32 n) >>> (8 * (3 - s))).setWidth 8 := by
  rw [be64_getD n (4 + s) (by omega), ofNat64_eq_setWidth32 n hn]
  have : s = 0 ∨ s = 1 ∨ s = 2 ∨ s = 3 := by omega
  rcases this with rfl|rfl|rfl|rfl <;> bv_decide

-- ---- structural characterization of padMessage's bytes by region ----
theorem pad_getD (df : Nat → BitVec 8) (len i : Nat) (hi : i < plenOf len) :
    (Sha256Spec.padMessage ((List.range len).map df)).getD i 0 =
      (if i < len then df i
       else if i = len then 128
       else if i < plenOf len - 8 then 0
       else (Sha256Spec.be64 (len * 8)).getD (i - (plenOf len - 8)) 0) := by
  have hpg := plen_ge len
  rw [List.getD_eq_getElem?_getD, Sha256Spec.padMessage]
  simp only [List.length_map, List.length_range,
    show ((len + 9 + 63) / 64) * 64 = plenOf len from rfl]
  simp only [List.getElem?_append, List.length_map, List.length_range, List.length_append,
    List.length_cons, List.length_nil, List.length_replicate]
  by_cases h1 : i < len
  · rw [if_pos (show i < len + (0+1) + (plenOf len - len - 9) by omega),
        if_pos (show i < len + (0+1) by omega), if_pos h1, List.getElem?_map,
        List.getElem?_range h1, if_pos h1]
    rfl
  · by_cases h2 : i = len
    · subst h2
      rw [if_pos (show i < i + (0+1) + (plenOf i - i - 9) by omega),
          if_pos (show i < i + (0+1) by omega), if_neg (show ¬ i < i by omega),
          if_neg h1, if_pos rfl]
      simp [Nat.sub_self]
    · by_cases h3 : i < plenOf len - 8
      · rw [if_pos (show i < len + (0+1) + (plenOf len - len - 9) by omega),
            if_neg (show ¬ i < len + (0+1) by omega), List.getElem?_replicate,
            if_pos (show i - (len + (0+1)) < plenOf len - len - 9 by omega),
            if_neg h1, if_neg h2, if_pos h3]
        rfl
      · rw [if_neg (show ¬ i < len + (0+1) + (plenOf len - len - 9) by omega),
            if_neg h1, if_neg h2, if_neg h3, List.getD_eq_getElem?_getD,
            show i - (len + (0+1) + (plenOf len - len - 9)) = i - (plenOf len - 8) by omega]

-- ---- the post-store buffer function and its agreement with padMessage ----
/-- The `s`-th big-endian length byte the source writes (`(bits >>> 8s) as u8`),
    `bits = (len*8) : u32`. -/
def lenByte (len s : Nat) : BitVec 8 := ((BitVec.ofNat 32 (len * 8)) >>> (8 * s)).setWidth 8

/-- The byte function after `sha256_hash`'s five padding stores: the `0x80`
    marker at `len`, and the four low length bytes at `plen-1 .. plen-4`. -/
def padFn (df : Nat → BitVec 8) (len : Nat) : Nat → BitVec 8 :=
  let plen := plenOf len
  bufUpd (bufUpd (bufUpd (bufUpd (bufUpd df len 128)
    (plen - 1) (lenByte len 0)) (plen - 2) (lenByte len 1)) (plen - 3) (lenByte len 2))
    (plen - 4) (lenByte len 3)

theorem padFn_eq (df : Nat → BitVec 8) (len : Nat) (hlen : len ≤ 375)
    (hz : ∀ i, len ≤ i → df i = 0) (i : Nat) (hi : i < plenOf len) :
    padFn df len i = (Sha256Spec.padMessage ((List.range len).map df)).getD i 0 := by
  have hpg : plenOf len ≥ len + 9 := plen_ge len
  have hb : len * 8 < 2 ^ 32 := by omega
  rw [pad_getD df len i hi]
  simp only [padFn, bufUpd, lenByte]
  by_cases e4 : i = plenOf len - 4
  · rw [if_pos e4, if_neg (show ¬ i < len by omega), if_neg (show ¬ i = len by omega),
      if_neg (show ¬ i < plenOf len - 8 by omega),
      show i - (plenOf len - 8) = 4 + 0 by omega, be64_low_byte (len * 8) hb 0 (by omega)]
  · by_cases e3 : i = plenOf len - 3
    · rw [if_neg e4, if_pos e3, if_neg (show ¬ i < len by omega),
        if_neg (show ¬ i = len by omega), if_neg (show ¬ i < plenOf len - 8 by omega),
        show i - (plenOf len - 8) = 4 + 1 by omega, be64_low_byte (len * 8) hb 1 (by omega)]
    · by_cases e2 : i = plenOf len - 2
      · rw [if_neg e4, if_neg e3, if_pos e2, if_neg (show ¬ i < len by omega),
          if_neg (show ¬ i = len by omega), if_neg (show ¬ i < plenOf len - 8 by omega),
          show i - (plenOf len - 8) = 4 + 2 by omega, be64_low_byte (len * 8) hb 2 (by omega)]
      · by_cases e1 : i = plenOf len - 1
        · rw [if_neg e4, if_neg e3, if_neg e2, if_pos e1, if_neg (show ¬ i < len by omega),
            if_neg (show ¬ i = len by omega), if_neg (show ¬ i < plenOf len - 8 by omega),
            show i - (plenOf len - 8) = 4 + 3 by omega, be64_low_byte (len * 8) hb 3 (by omega)]
        · rw [if_neg e4, if_neg e3, if_neg e2, if_neg e1]
          by_cases hl : i < len
          · rw [if_neg (show ¬ i = len by omega), if_pos hl]
          · by_cases he : i = len
            · rw [if_pos he, if_neg hl, if_pos he]
            · rw [if_neg he, if_neg hl, if_neg he]
              by_cases h8 : i < plenOf len - 8
              · rw [if_pos h8, hz i (by omega)]
              · rw [if_neg h8, be64_high_zero (len * 8) hb (i - (plenOf len - 8)) (by omega),
                  hz i (by omega)]

-- ---- bridge to the spec's block view ----
theorem padMessage_length (msg : List Sha256Spec.Byte) :
    (Sha256Spec.padMessage msg).length = plenOf msg.length := by
  have hpg : plenOf msg.length ≥ msg.length + 9 := plen_ge msg.length
  simp only [Sha256Spec.padMessage, Sha256Spec.be64, List.length_append, List.length_cons,
    List.length_nil, List.length_replicate, List.length_map, List.length_range]
  show msg.length + (0 + 1) + (plenOf msg.length - msg.length - 9) + 8 = plenOf msg.length
  omega

-- `getD_eq_getElem_mem` relocated to `Concrete.ProofKit.Refinement` (Proof Kit v1).
theorem sliceAt_padFn (df : Nat → BitVec 8) (len : Nat) (hlen : len ≤ 375)
    (hz : ∀ i, len ≤ i → df i = 0) (blk : Nat) (hblk : blk < plenOf len / 64) :
    sliceAt (padFn df len) (blk * 64)
      = Sha256Spec.blockAt (Sha256Spec.padMessage ((List.range len).map df)) blk := by
  have hdvd : plenOf len % 64 = 0 := by unfold plenOf; omega
  have hplen : (Sha256Spec.padMessage ((List.range len).map df)).length = plenOf len := by
    rw [padMessage_length]; simp
  have hb1 : (blk + 1) * 64 ≤ plenOf len := by omega
  apply List.ext_getElem
  · simp only [sliceAt, List.length_map, List.length_range, Sha256Spec.blockAt,
      List.length_take, List.length_drop, hplen]; omega
  · intro j h1 _
    have hj : j < 64 := by simpa only [sliceAt, List.length_map, List.length_range] using h1
    have hidx : blk * 64 + j < plenOf len := by omega
    simp only [sliceAt, List.getElem_map, List.getElem_range]
    simp only [Sha256Spec.blockAt, List.getElem_take, List.getElem_drop]
    rw [padFn_eq df len hlen hz (blk * 64 + j) hidx,
        getD_eq_getElem_mem _ _ (by rw [hplen]; exact hidx)]

-- ==================================================================
-- Multi-block hash loop (task #21, sha256_hash step 4): the
-- `for blk in 0..nblocks { state = sha256_compress_at(state, buf, blk*64) }`
-- loop folds `Sha256Spec.compress` over every padded block, producing
-- `hashFold padded nblocks = Sha256Spec.hashState`. Proved with
-- `eval_while_count` (state invariant `state = hashFold padded blk`,
-- `sha256_compress_at_call` lifting the per-block refinement, and the
-- `sliceAt = blockAt` hypothesis discharged later by `sliceAt_padFn`).
-- ==================================================================

theorem compress_length (s : List Sha256Spec.W) (b : List Sha256Spec.Byte) :
    (Sha256Spec.compress s b).length = 8 := by
  simp [Sha256Spec.compress]

theorem hashFold_length (padded : List Sha256Spec.Byte) (n : Nat) :
    (hashFold padded n).length = 8 := by
  cases n with
  | zero => rfl
  | succ k => rw [hashFold_succ]; exact compress_length _ _

theorem sha256_compress_at_call (state0 : List (BitVec 32)) (h0 : state0.length = 8)
    (bf : Nat → BitVec 8) (off : Nat) (hoff : off + 64 ≤ 384)
    (env : Env) (stateE bufE offE : PExpr) (fuel : Nat)
    (hstate : eval shaFns env (fuel + 78) stateE = some (.array_ (state0.map (fun x => PVal.int x.toNat))))
    (hbuf : eval shaFns env (fuel + 78) bufE = some (.array_ (bufArr bf)))
    (hoffe : eval shaFns env (fuel + 78) offE = some (.int (off:Int))) :
    eval shaFns env (fuel + 79) (.call "sha256_compress_at" [stateE, bufE, offE])
      = some (.array_ ((Sha256Spec.compress state0 (sliceAt bf off)).map (fun x => PVal.int x.toNat))) := by
  simp only [eval, shaFns, eval.evalArgs, hstate, hbuf, hoffe, bindArgs]
  exact sha256_compress_at_refines_spec state0 h0 bf off hoff fuel

-- ---- multi-block hash loop ----

def hsEnv (e : Env) (bff : Nat → BitVec 8) (padded : List Sha256Spec.Byte) (nblocks blk : Nat) : Env :=
  (((e.bind "state" (.array_ ((hashFold padded blk).map (fun x => PVal.int x.toNat)))).bind
    "buf" (.array_ (bufArr bff))).bind "nblocks" (.int (nblocks:Int))).bind "blk" (.int (blk:Int))

theorem hstep (e : Env) (bff : Nat → BitVec 8) (padded : List Sha256Spec.Byte) (nblocks blk : Nat)
    (hblk : blk < nblocks) (hbound : nblocks * 64 ≤ 384)
    (hslice : sliceAt bff (blk * 64) = Sha256Spec.blockAt padded blk) (fuel : Nat) :
    eval.evalAssigns shaFns (hsEnv e bff padded nblocks blk) (fuel + 79) assignsH
      = some (hsEnv e bff padded nblocks (blk + 1)) := by
  have h8 : (hashFold padded blk).length = 8 := hashFold_length padded blk
  have hsv : eval shaFns (hsEnv e bff padded nblocks blk) (fuel + 78) (.var "state")
      = some (.array_ ((hashFold padded blk).map (fun x => PVal.int x.toNat))) := by simp [eval, hsEnv, Env.bind]
  have hbv : eval shaFns (hsEnv e bff padded nblocks blk) (fuel + 78) (.var "buf")
      = some (.array_ (bufArr bff)) := by simp [eval, hsEnv, Env.bind]
  have hoffv : eval shaFns (hsEnv e bff padded nblocks blk) (fuel + 78)
      (.binOp .mul (.var "blk") (.lit (.int 64))) = some (.int ((blk * 64 : Nat) : Int)) := by
    simp only [eval, hsEnv, Env.bind, evalBinOp, beq_self_eq_true, if_true, beq_iff_eq, if_false,
      String.reduceEq, reduceCtorEq, Option.some.injEq, PVal.int.injEq]; omega
  have hoffbound : blk * 64 + 64 ≤ 384 := by omega
  have hcompress := sha256_compress_at_call (hashFold padded blk) h8 bff (blk * 64) hoffbound
    (hsEnv e bff padded nblocks blk) _ _ _ fuel hsv hbv hoffv
  rw [hslice, ← hashFold_succ] at hcompress
  simp only [assignsH, eval.evalAssigns, hcompress]
  simp only [eval, evalBinOp, Env.bind, hsEnv, beq_self_eq_true, if_true, beq_iff_eq, if_false,
    String.reduceEq, reduceCtorEq, Option.some.injEq]
  funext n
  by_cases h1 : (n == "blk") = true <;> by_cases h2 : (n == "state") = true <;>
    simp_all [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq,
      reduceCtorEq, Option.some.injEq, PVal.int.injEq]

theorem cond_h_true (e bff padded nblocks blk) (hblk : blk < nblocks) (F : Nat) :
    eval shaFns (hsEnv e bff padded nblocks blk) (F + 1) condH = some (.bool true) := by
  simp only [condH, hsEnv, eval, Env.bind, evalBinOp]; simp; omega
theorem cond_h_false (e bff padded nblocks) (F : Nat) :
    eval shaFns (hsEnv e bff padded nblocks nblocks) (F + 1) condH = some (.bool false) := by
  simp only [condH, hsEnv, eval, Env.bind, evalBinOp]; simp

theorem hash_loop_eval (e : Env) (bff : Nat → BitVec 8) (padded : List Sha256Spec.Byte) (nblocks : Nat)
    (hbound : nblocks * 64 ≤ 384)
    (hslice : ∀ blk, blk < nblocks → sliceAt bff (blk * 64) = Sha256Spec.blockAt padded blk)
    (cont : PExpr) (base : Nat) :
    eval shaFns (hsEnv e bff padded nblocks 0) ((base + 79) + nblocks + 1) (.while_ condH assignsH cont)
      = eval shaFns (hsEnv e bff padded nblocks nblocks) (base + 79) cont :=
  eval_while_count shaFns condH assignsH cont (hsEnv e bff padded nblocks) nblocks (base + 79)
    (fun m hm => ⟨cond_h_true e bff padded nblocks m hm (base + 78),
                  hstep e bff padded nblocks m hm hbound (hslice m hm) base⟩)
    (cond_h_false e bff padded nblocks (base + 78))

-- ==================================================================
-- state_to_bytes refines its spec (task #21, sha256_hash step 5): the
-- 8-iteration loop that writes 4 big-endian bytes per state word refines
-- Sha256Spec.stateToBytes. The first MULTI-store-per-iteration loop:
-- each iteration advances the byte-fill boundary by 4 (obAt invariant +
-- set_in_counter_map applied four times via a threaded env), with the
-- per-byte (>>>k)&255 store value matched to the spec byte by bv_decide.
-- flatMap_getD_4 indexes the spec's flatMap digest.
-- ==================================================================

theorem wordToBytes_length (x : Sha256Spec.W) : (Sha256Spec.wordToBytes x).length = 4 := rfl

theorem flatMap_getD_4 (L : List Sha256Spec.W) (i r : Nat) (hr : r < 4) :
    (L.flatMap Sha256Spec.wordToBytes).getD (4 * i + r) 0
      = (Sha256Spec.wordToBytes (L.getD i 0)).getD r 0 := by
  induction L generalizing i with
  | nil =>
    simp only [List.flatMap_nil, List.getD_nil, List.getD_eq_getElem?_getD]
    rcases (by omega : r = 0 ∨ r = 1 ∨ r = 2 ∨ r = 3) with rfl|rfl|rfl|rfl <;>
      simp [Sha256Spec.wordToBytes]
  | cons x xs ih =>
    rw [List.flatMap_cons]
    cases i with
    | zero =>
      simp only [Nat.mul_zero, Nat.zero_add, List.getD_eq_getElem?_getD,
        List.getElem?_append_left (by rw [wordToBytes_length]; omega : r < (Sha256Spec.wordToBytes x).length),
        List.getD_cons_zero]
      rfl
    | succ i' =>
      have h4 : 4 * (i' + 1) + r = (Sha256Spec.wordToBytes x).length + (4 * i' + r) := by
        rw [wordToBytes_length]; omega
      rw [List.getD_eq_getElem?_getD, h4, List.getElem?_append_right (by omega),
          Nat.add_sub_cancel_left, ← List.getD_eq_getElem?_getD, ih i']
      rfl

theorem wordToBytes_getD (y : Sha256Spec.W) (r : Nat) (hr : r < 4) :
    (Sha256Spec.wordToBytes y).getD r 0 = (y >>> (8 * (3 - r))).setWidth 8 := by
  rcases (by omega : r = 0 ∨ r = 1 ∨ r = 2 ∨ r = 3) with rfl|rfl|rfl|rfl <;>
    simp [Sha256Spec.wordToBytes]

def sbyte (state : List Sha256Spec.W) (j : Nat) : BitVec 8 :=
  (state.getD (j / 4) 0 >>> (8 * (3 - j % 4))).setWidth 8

theorem stateToBytes_length (state : List Sha256Spec.W) (h8 : 8 ≤ state.length) :
    (Sha256Spec.stateToBytes state).length = 32 := by
  have ht : (state.take 8).length = 8 := by rw [List.length_take]; omega
  simp only [Sha256Spec.stateToBytes, List.length_flatMap]
  rw [show (List.map (fun a => (Sha256Spec.wordToBytes a).length) (state.take 8))
        = List.replicate 8 4 by
      apply List.ext_getElem (by simp [List.length_map, List.length_replicate, List.length_take]; omega)
      intro n h1 _
      rw [List.getElem_map, List.getElem_replicate, wordToBytes_length]]
  decide

theorem stateToBytes_getD (state : List Sha256Spec.W) (j : Nat) (hj : j < 32) :
    (Sha256Spec.stateToBytes state).getD j 0 = sbyte state j := by
  have hjr : 4 * (j / 4) + j % 4 = j := by omega
  have hq : j / 4 < 8 := by omega
  rw [Sha256Spec.stateToBytes, ← hjr,
      flatMap_getD_4 (state.take 8) (j / 4) (j % 4) (by omega),
      wordToBytes_getD _ _ (by omega)]
  rw [show (state.take 8).getD (j / 4) 0 = state.getD (j / 4) 0 by
        rw [List.getD_eq_getElem?_getD, List.getD_eq_getElem?_getD, List.getElem?_take, if_pos hq]]
  rw [hjr]; rfl

-- ---- eval side: invariant array, store values ----
def obAt (state : List Sha256Spec.W) (m : Nat) : List PVal :=
  (List.range 32).map (fun j => if j < m then PVal.int (sbyte state j).toNat else PVal.int 0)
theorem obAt_length (state m) : (obAt state m).length = 32 := by simp [obAt]
theorem obAt_set (state : List Sha256Spec.W) (m : Nat) :
    (obAt state m).set m (PVal.int (sbyte state m).toNat) = obAt state (m + 1) :=
  set_in_counter_map 32 (fun j => PVal.int (sbyte state j).toNat) (PVal.int 0) m
theorem obAt_zero (state) : obAt state 0 = List.replicate 32 (PVal.int 0) := by
  simp [obAt]; rfl
theorem obAt_32 (state : List Sha256Spec.W) (h8 : 8 ≤ state.length) :
    obAt state 32 = (Sha256Spec.stateToBytes state).map (fun b => PVal.int b.toNat) := by
  apply List.ext_getElem (by simp [obAt, stateToBytes_length state h8])
  intro j h1 _
  simp only [obAt, List.length_map, List.length_range] at h1
  simp only [obAt, List.getElem_map, List.getElem_range, if_pos h1]
  rw [← getD_eq_getElem_mem _ j (by rw [stateToBytes_length state h8]; exact h1),
      stateToBytes_getD state j h1]

-- store-value: the shifted+masked byte (r=0,1,2) and the masked-only byte (r=3)

theorem and255_lo (y : BitVec 32) : (y &&& BitVec.ofInt 32 255).toNat = (BitVec.setWidth 8 y).toNat := by
  have h : y &&& BitVec.ofInt 32 255 = (BitVec.setWidth 8 y).setWidth 32 := by bv_decide
  rw [h, BitVec.toNat_setWidth,
    Nat.mod_eq_of_lt (Nat.lt_of_lt_of_le (BitVec.setWidth 8 y).isLt (by decide))]

theorem sb_shr_eval (state : List Sha256Spec.W) (i : Nat) (env : Env) (fuel s : Nat) (hi : i < 8)
    (h8 : 8 ≤ state.length)
    (hst : env "state" = some (.array_ (state.map (fun x => PVal.int x.toNat))))
    (hiv : env "i" = some (.int (i:Int))) :
    eval shaFns env (fuel + 3) (sbStore s)
      = some (.int ((state.getD i 0 >>> s).setWidth 8).toNat) := by
  have hre : eval shaFns env (fuel + 2) (.arrayIndex (.var "state") (.var "i"))
      = some (.int (state.getD i 0).toNat) :=
    arr_read state env (fuel + 1) i (by omega) "state" hst _ (by simp [eval, hiv])
  simp only [sbStore, eval, hre, evalBinOp, ofInt_natCast_toNat, ofInt_ofNat_toNat,
    Int.toNat_natCast, Option.some.injEq, PVal.int.injEq, and255_lo, Int.ofNat_eq_natCast]

theorem sb_and_eval (state : List Sha256Spec.W) (i : Nat) (env : Env) (fuel : Nat) (hi : i < 8)
    (h8 : 8 ≤ state.length)
    (hst : env "state" = some (.array_ (state.map (fun x => PVal.int x.toNat))))
    (hiv : env "i" = some (.int (i:Int))) :
    eval shaFns env (fuel + 3) sbStore3
      = some (.int ((state.getD i 0).setWidth 8).toNat) := by
  have hre : eval shaFns env (fuel + 2) (.arrayIndex (.var "state") (.var "i"))
      = some (.int (state.getD i 0).toNat) :=
    arr_read state env (fuel + 1) i (by omega) "state" hst _ (by simp [eval, hiv])
  simp only [sbStore3, eval, hre, evalBinOp, ofInt_natCast_toNat, ofInt_ofNat_toNat,
    Option.some.injEq, PVal.int.injEq, and255_lo, Int.ofNat_eq_natCast]

-- ---- 4-stores-per-iteration step + loop ----
theorem bind_shadow (env : Env) (k : String) (a b : PVal) :
    (env.bind k a).bind k b = env.bind k b := by
  funext n; simp only [Env.bind]; by_cases h : (n == k) = true <;> simp [h]

def envL (e : Env) (state : List Sha256Spec.W) (i_m : Nat) (L : List PVal) : Env :=
  ((e.bind "state" (.array_ (state.map (fun x => PVal.int x.toNat)))).bind
    "i" (.int (i_m:Int))).bind "out" (.array_ L)

theorem sbyte_at (state : List Sha256Spec.W) (i r : Nat) (hr : r < 4) :
    sbyte state (4 * i + r) = (state.getD i 0 >>> (8 * (3 - r))).setWidth 8 := by
  simp only [sbyte, show (4 * i + r) / 4 = i by omega, show (4 * i + r) % 4 = r by omega]


-- one array store advancing obAt by one position `p`
theorem store_eval (e : Env) (state : List Sha256Spec.W) (m p : Nat) (hp : p < 32)
    (idxE valE : PExpr) (fuel : Nat)
    (hidx : eval shaFns (envL e state m (obAt state p)) (fuel + 4) idxE
      = some (.int ((p : Nat) : Int)))
    (hval : eval shaFns (envL e state m (obAt state p)) (fuel + 4) valE
      = some (.int (sbyte state p).toNat)) :
    eval shaFns (envL e state m (obAt state p)) (fuel + 5)
      (.arraySet (.var "out") idxE valE)
      = some (.array_ (obAt state (p + 1))) := by
  have hout : eval shaFns (envL e state m (obAt state p)) (fuel + 4) (.var "out")
      = some (.array_ (obAt state p)) := by simp [eval, envL, Env.bind]
  rw [eval_arraySet_lemma shaFns _ (fuel + 4) (.var "out") idxE valE
      (obAt state p) ((p : Nat) : Int) (.int (sbyte state p).toNat)
      hout hidx hval (by omega) (by rw [obAt_length]; omega)]
  rw [show (((p : Nat) : Int)).toNat = p by omega, obAt_set]

theorem envL_rebind (e : Env) (state : List Sha256Spec.W) (m : Nat) (L L' : List PVal) :
    (envL e state m L).bind "out" (.array_ L') = envL e state m L' := by
  simp only [envL]; rw [bind_shadow]

theorem idx0_eval (e : Env) (state : List Sha256Spec.W) (m : Nat) (L : List PVal) (fuel : Nat) :
    eval shaFns (envL e state m L) (fuel + 4) oIdx0 = some (.int ((4 * m : Nat) : Int)) := by
  simp only [oIdx0, eval, envL, Env.bind, evalBinOp, beq_self_eq_true, if_true, beq_iff_eq,
    if_false, String.reduceEq, reduceCtorEq, Option.some.injEq, PVal.int.injEq]; omega
theorem idxK_eval (e : Env) (state : List Sha256Spec.W) (m : Nat) (L : List PVal) (fuel k : Nat) :
    eval shaFns (envL e state m L) (fuel + 4) (oIdxK (k : Int)) = some (.int ((4 * m + k : Nat) : Int)) := by
  simp only [oIdxK, oIdx0, eval, envL, Env.bind, evalBinOp, beq_self_eq_true, if_true, beq_iff_eq,
    if_false, String.reduceEq, reduceCtorEq, Option.some.injEq, PVal.int.injEq]; omega

theorem val_shr (e : Env) (state : List Sha256Spec.W) (m r : Nat) (hr : r < 3) (hm : m < 8)
    (h8 : 8 ≤ state.length) (L : List PVal) (fuel : Nat) :
    eval shaFns (envL e state m L) (fuel + 4) (sbStore (8 * (3 - r)))
      = some (.int (sbyte state (4 * m + r)).toNat) := by
  rw [sbyte_at state m r (by omega)]
  exact sb_shr_eval state m (envL e state m L) (fuel + 1) (8 * (3 - r)) hm h8
    (by simp [envL, Env.bind]) (by simp [envL, Env.bind])
theorem val_and (e : Env) (state : List Sha256Spec.W) (m : Nat) (hm : m < 8)
    (h8 : 8 ≤ state.length) (L : List PVal) (fuel : Nat) :
    eval shaFns (envL e state m L) (fuel + 4) sbStore3
      = some (.int (sbyte state (4 * m + 3)).toNat) := by
  rw [sbyte_at state m 3 (by omega), show 8 * (3 - 3) = 0 by rfl]
  have := sb_and_eval state m (envL e state m L) (fuel + 1) hm h8
    (by simp [envL, Env.bind]) (by simp [envL, Env.bind])
  simpa using this

theorem val_shr0 (e : Env) (state : List Sha256Spec.W) (m : Nat) (hm : m < 8)
    (h8 : 8 ≤ state.length) (L : List PVal) (fuel : Nat) :
    eval shaFns (envL e state m L) (fuel + 4) (sbStore 24)
      = some (.int (sbyte state (4 * m)).toNat) := by
  rw [show sbyte state (4 * m) = (state.getD m 0 >>> 24).setWidth 8 by
        simp only [sbyte, show 4 * m / 4 = m by omega, show 4 * m % 4 = 0 by omega]]
  exact sb_shr_eval state m (envL e state m L) (fuel + 1) 24 hm h8
    (by simp [envL, Env.bind]) (by simp [envL, Env.bind])

set_option maxHeartbeats 1000000 in
theorem stb_step (e : Env) (state : List Sha256Spec.W) (m : Nat) (hm : m < 8)
    (h8 : 8 ≤ state.length) (fuel : Nat) :
    eval.evalAssigns shaFns (envL e state m (obAt state (4 * m))) (fuel + 5) assignsS
      = some (envL e state (m + 1) (obAt state (4 * (m + 1)))) := by
  have s0 := store_eval e state m (4 * m) (by omega) oIdx0 (sbStore 24) fuel
    (idx0_eval e state m _ fuel) (val_shr0 e state m hm h8 _ fuel)
  have s1 := store_eval e state m (4 * m + 1) (by omega) (oIdxK 1) (sbStore 16) fuel
    (idxK_eval e state m _ fuel 1) (val_shr e state m 1 (by omega) hm h8 _ fuel)
  have s2 := store_eval e state m (4 * m + 2) (by omega) (oIdxK 2) (sbStore 8) fuel
    (idxK_eval e state m _ fuel 2) (val_shr e state m 2 (by omega) hm h8 _ fuel)
  have s3 := store_eval e state m (4 * m + 3) (by omega) (oIdxK 3) sbStore3 fuel
    (idxK_eval e state m _ fuel 3) (val_and e state m hm h8 _ fuel)
  simp only [assignsS, eval.evalAssigns, s0, envL_rebind, s1, s2, s3,
    show 4 * m + 1 = 4 * m + 1 from rfl]
  simp only [eval, envL, Env.bind, evalBinOp, beq_self_eq_true, if_true, beq_iff_eq, if_false,
    String.reduceEq, reduceCtorEq, Option.some.injEq]
  funext n
  by_cases h1 : (n == "i") = true <;> by_cases h2 : (n == "out") = true <;>
    simp_all [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq,
      reduceCtorEq, Option.some.injEq, PVal.int.injEq, show 4 * (m + 1) = 4 * m + 3 + 1 by omega]

theorem condS_true (e : Env) (state : List Sha256Spec.W) (m : Nat) (L : List PVal) (hm : m < 8) (F : Nat) :
    eval shaFns (envL e state m L) (F + 1) condS = some (.bool true) := by
  simp only [condS, envL, eval, Env.bind, evalBinOp]; simp; omega
theorem condS_false (e : Env) (state : List Sha256Spec.W) (L : List PVal) (F : Nat) :
    eval shaFns (envL e state 8 L) (F + 1) condS = some (.bool false) := by
  simp only [condS, envL, eval, Env.bind, evalBinOp]; simp

theorem stb_loop (e : Env) (state : List Sha256Spec.W) (h8 : 8 ≤ state.length)
    (cont : PExpr) (base : Nat) :
    eval shaFns (envL e state 0 (obAt state (4 * 0))) ((base + 5) + 8 + 1) (.while_ condS assignsS cont)
      = eval shaFns (envL e state 8 (obAt state (4 * 8))) (base + 5) cont :=
  eval_while_count shaFns condS assignsS cont (fun m => envL e state m (obAt state (4 * m))) 8 (base + 5)
    (fun m hm => ⟨condS_true e state m _ hm (base + 4), stb_step e state m hm h8 base⟩)
    (condS_false e state _ (base + 4))

theorem arrayLit_zeros32_eval (fns : FnTable) (env : Env) (fuel : Nat) :
    eval fns env (fuel + 2) (.arrayLit (List.replicate 32 (.lit (.int 0))))
      = some (.array_ (List.replicate 32 (PVal.int 0))) := by
  simp only [eval, evalElems_replicate_lit]


theorem state_to_bytes_refines_spec (e : Env) (state : List Sha256Spec.W) (h8 : state.length = 8)
    (fuel : Nat) :
    eval shaFns (e.bind "state" (.array_ (state.map (fun x => PVal.int x.toNat)))) (fuel + 16) stateToBytesExpr
      = some (.array_ ((Sha256Spec.stateToBytes state).map (fun b => PVal.int b.toNat))) := by
  rw [stateToBytesExpr, show fuel + 16 = (fuel + 15) + 1 by omega,
      eval_letIn _ _ _ _ _ _ _ (arrayLit_zeros32_eval _ _ (fuel + 14))]
  rw [show fuel + 15 = (fuel + 14) + 1 by omega, eval_letIn _ _ _ _ _ _ (PVal.int 0) (by simp [eval])]
  rw [← obAt_zero state,
      show (((e.bind "state" (.array_ (state.map (fun x => PVal.int x.toNat)))).bind
            "out" (.array_ (obAt state 0))).bind "i" (.int 0))
          = envL e state 0 (obAt state (4 * 0)) by
        funext n; simp only [envL, Env.bind, Nat.mul_zero]
        by_cases h1 : (n == "i") = true <;> by_cases h2 : (n == "out") = true <;> simp_all]
  rw [show fuel + 14 = (fuel + 5) + 8 + 1 by omega, stb_loop e state (by omega) _ fuel,
      show (4 * 8 : Nat) = 32 from rfl]
  simp only [eval, envL, Env.bind, beq_self_eq_true, if_true, obAt_32 state (by omega)]

-- ==================================================================
-- sha256_hash store-chain components (task #21, sha256_hash glue):
-- the sdiv/Nat.div bridge for the symbolic i32 nblocks index
-- (sdiv64_bridge), the generic buffer store (buf_store, via bufArr_set),
-- the four length-byte store values (lenStore*, matched by bv_decide),
-- the sha256_init / state_to_bytes call wrappers, and sha256_hashExpr.
-- ==================================================================

-- sdiv bridge (verified earlier)
theorem ofNat32_msb_false (a : Nat) (ha : a < 2^31) : (BitVec.ofNat 32 a).msb = false := by
  rw [BitVec.msb_eq_false_iff_two_mul_lt, BitVec.toNat_ofNat, Nat.mod_eq_of_lt (by omega)]; omega

theorem sdiv64_bridge (L : Nat) (hL : L ≤ 375) :
    ((BitVec.ofInt 32 ((L:Int) + 9 + 63)).sdiv (BitVec.ofInt 32 64)).toInt
      = (((L + 72) / 64 : Nat) : Int) := by
  rw [show (L:Int) + 9 + 63 = ((L + 72 : Nat) : Int) by omega,
      show (64 : Int) = ((64 : Nat) : Int) by rfl,
      BitVec.ofInt_natCast, BitVec.ofInt_natCast,
      BitVec.sdiv_eq, ofNat32_msb_false (L + 72) (by omega), ofNat32_msb_false 64 (by omega),
      BitVec.udiv_eq]
  rw [BitVec.toInt_eq_toNat_of_lt (by
        rw [BitVec.toNat_udiv, BitVec.toNat_ofNat, BitVec.toNat_ofNat,
            Nat.mod_eq_of_lt (show L + 72 < 2^32 by omega), Nat.mod_eq_of_lt (show 64 < 2^32 by omega)]
        omega)]
  rw [BitVec.toNat_udiv, BitVec.toNat_ofNat, BitVec.toNat_ofNat,
      Nat.mod_eq_of_lt (show L + 72 < 2^32 by omega), Nat.mod_eq_of_lt (show 64 < 2^32 by omega)]

theorem sha256_init_call (env : Env) (fuel : Nat) :
    eval shaFns env (fuel + 3) (.call "sha256_init" [])
      = some (.array_ (Sha256Spec.initState.map (fun x => PVal.int x.toNat))) := by
  simp only [eval, shaFns, eval.evalArgs, bindArgs]
  simp [eval, sha256_initExpr, eval.evalElems, Sha256Spec.initState]

theorem state_to_bytes_call (state : List Sha256Spec.W) (h8 : state.length = 8) (env : Env)
    (stateE : PExpr) (fuel : Nat)
    (hstate : eval shaFns env (fuel + 16) stateE = some (.array_ (state.map (fun x => PVal.int x.toNat)))) :
    eval shaFns env (fuel + 17) (.call "state_to_bytes" [stateE])
      = some (.array_ ((Sha256Spec.stateToBytes state).map (fun b => PVal.int b.toNat))) := by
  simp only [eval, shaFns, eval.evalArgs, hstate, bindArgs]
  exact state_to_bytes_refines_spec Env.empty state h8 fuel


theorem lenStore0_eval (env : Env) (fuel len : Nat)
    (hb : env "bits" = some (.int ((len * 8 : Nat) : Int))) :
    eval shaFns env (fuel + 2) lenStore0 = some (.int (lenByte len 0).toNat) := by
  simp only [lenStore0, eval, hb, evalBinOp, ofInt_natCast_toNat, ofInt_ofNat_toNat,
    Option.some.injEq, PVal.int.injEq, and255_lo, Int.ofNat_eq_natCast, lenByte, BitVec.ofInt_natCast,
    Nat.mul_zero, BitVec.ushiftRight_zero]
theorem lenStoreS_eval (env : Env) (fuel len s : Nat)
    (hb : env "bits" = some (.int ((len * 8 : Nat) : Int))) :
    eval shaFns env (fuel + 3) (lenStoreS (8 * s)) = some (.int (lenByte len s).toNat) := by
  simp only [lenStoreS, eval, hb, evalBinOp, ofInt_natCast_toNat, ofInt_ofNat_toNat,
    Int.toNat_natCast, Option.some.injEq, PVal.int.injEq, and255_lo, Int.ofNat_eq_natCast, lenByte, BitVec.ofInt_natCast, BitVec.ofNat_toNat, BitVec.setWidth_eq]


-- helper: bits binding value
theorem bits_val (e : Env) (len : Nat) (hlenv : e "len" = some (.int (len : Int))) (fuel : Nat) :
    eval shaFns e (fuel + 2) (.cast (.binOp .mul (.var "len") (.lit (.int 8))))
      = some (.int ((len * 8 : Nat) : Int)) := by
  simp only [eval, hlenv, evalBinOp, Option.some.injEq, PVal.int.injEq]; omega

theorem bufArr_length (bf : Nat → BitVec 8) : (bufArr bf).length = 384 := by simp [bufArr]

theorem buf_store (df : Nat → BitVec 8) (E : Env) (L : Nat) (idxE valE : PExpr) (idx : Nat) (v : BitVec 8)
    (hbuf : E "buf" = some (.array_ (bufArr df))) (hidx : eval shaFns E (L + 1) idxE = some (.int (idx : Int)))
    (hval : eval shaFns E (L + 1) valE = some (.int (v.toNat : Int))) (hb : idx < 384) :
    eval shaFns E (L + 2) (.arraySet (.var "buf") idxE valE)
      = some (.array_ (bufArr (bufUpd df idx v))) := by
  rw [eval_arraySet_lemma shaFns E (L + 1) (.var "buf") idxE valE (bufArr df) (idx : Int) (.int (v.toNat : Int))
      (by simp only [eval, hbuf]) hidx hval (by omega)
      (by rw [show ((idx : Int)).toNat = idx by omega, bufArr_length]; omega)]
  rw [show ((idx : Int)).toNat = idx by omega, bufArr_set]

theorem var_read (E : Env) (nm : String) (val : PVal) (F : Nat) (h : E nm = some val) :
    eval shaFns E (F + 1) (.var nm) = some val := by
  simp only [eval]; exact h

-- ==================================================================
-- sha256_hash refines its spec (task #21, sha256_hash END-TO-END):
-- the full `sha256_hash(data, len)` — copy buffer, 0x80 marker, FIPS
-- length stores at the computed plen indices, multi-block compress loop,
-- digest unpack — computes exactly `Sha256Spec.hash` of the message, for
-- ANY len ≤ 375 with a zero-padded buffer (data[len..] = 0). Assembles
-- the store-chain (bufArr_set x5 -> padFn), the symbolic-index sdiv
-- bridge, the multi-block loop (hash_loop_eval via sliceAt_padFn), and
-- state_to_bytes. HMAC bar #2 for sha256_hash is closed.
-- ==================================================================

theorem nblocks_eval (E : Env) (len : Nat) (hlen : len ≤ 375) (G nbv : Nat)
    (hnbv : (len + 9 + 63) / 64 = nbv) (hle : E "len" = some (.int (len : Int))) :
    eval shaFns E (G + 1) (.binOp (.div 32 true)
        (.binOp .add (.binOp .add (.var "len") (.lit (.int 9))) (.lit (.int 63))) (.lit (.int 64)))
      = some (.int ((nbv : Nat) : Int)) := by
  subst hnbv
  simp only [eval, evalBinOp, var_read E "len" (.int (len:Int)) G hle]
  rw [if_neg (show ¬ ((64:Int) = 0) by decide)]
  rw [show (len + 9 + 63) = (len + 72) by omega, sdiv64_bridge len hlen]

theorem plen_eval (E : Env) (nbv : Nat) (G : Nat) (hn : E "nblocks" = some (.int (nbv : Int))) :
    eval shaFns E (G + 1) (.binOp .mul (.var "nblocks") (.lit (.int 64)))
      = some (.int ((nbv * 64 : Nat) : Int)) := by
  simp only [eval, evalBinOp, var_read E "nblocks" (.int (nbv:Int)) G hn,
    Option.some.injEq, PVal.int.injEq]; omega

theorem hsEnv_self (E : Env) (bff : Nat → BitVec 8) (padded : List Sha256Spec.Byte) (nbv : Nat)
    (hst : E "state" = some (.array_ ((hashFold padded 0).map (fun x => PVal.int x.toNat))))
    (hbf : E "buf" = some (.array_ (bufArr bff)))
    (hnn : E "nblocks" = some (.int (nbv : Int))) (hbk : E "blk" = some (.int 0)) :
    hsEnv E bff padded nbv 0 = E := by
  funext n
  simp only [hsEnv, Env.bind]
  by_cases h1 : n = "blk" <;> by_cases h2 : n = "state" <;> by_cases h3 : n = "buf" <;>
    by_cases h4 : n = "nblocks" <;> simp_all [beq_iff_eq]

set_option maxHeartbeats 16000000 in
theorem sha256_hash_refines_spec (df : Nat → BitVec 8) (len : Nat) (hlen : len ≤ 375)
    (hz : ∀ i, len ≤ i → df i = 0) (e : Env) (fuel : Nat)
    (hdata : e "data" = some (.array_ (bufArr df)))
    (hlenv : e "len" = some (.int (len : Int))) :
    eval shaFns e (fuel + 91 + (len + 9 + 63) / 64) sha256_hashExpr
      = some (.array_ ((Sha256Spec.hash ((List.range len).map df)).map (fun b => PVal.int b.toNat))) := by
  have hple : plenOf len ≥ len + 9 := plen_ge len
  have hpb : plenOf len ≤ 384 := by unfold plenOf; omega
  generalize hnbe : (len + 9 + 63) / 64 = nb
  have hplnb : plenOf len = nb * 64 := by rw [show plenOf len = (len+9+63)/64*64 from rfl, hnbe]
  have hnbb : nb * 64 ≤ 384 := by rw [← hplnb]; exact hpb
  have hidxk : ∀ (E : Env) (k G : Nat), k ≤ 4 → E "plen" = some (.int ((nb * 64 : Nat) : Int)) →
      eval shaFns E (G + 1) (.binOp .sub (.var "plen") (.lit (.int (k : Int))))
        = some (.int ((plenOf len - k : Nat) : Int)) := by
    intro E k G hk hp
    simp only [eval, evalBinOp, var_read E "plen" (.int ((nb*64:Nat):Int)) G hp,
      Option.some.injEq, PVal.int.injEq]; omega
  rw [sha256_hashExpr]
  rw [show fuel + 91 + nb = (fuel + 90 + nb) + 1 by omega,
      eval_letIn _ _ (fuel + 90 + nb) _ _ _ _ (var_read e "data" _ (fuel + 90 + nb) hdata)]
  rw [show fuel + 90 + nb = ((fuel + 88 + nb) + 1) + 1 by omega,
      eval_letIn _ _ ((fuel + 88 + nb) + 1) _ _ _ _
        (buf_store df _ (fuel + 88 + nb) (.var "len") (.lit (.int 128)) len (128 : BitVec 8)
          (by simp [Env.bind]) (var_read _ "len" (.int (len:Int)) (fuel + 88 + nb) (by simpa [Env.bind] using hlenv))
          (by simp [eval]) (by omega)),
      show (fuel + 88 + nb) + 1 = fuel + 89 + nb by omega]
  rw [show fuel + 89 + nb = (fuel + 88 + nb) + 1 by omega,
      eval_letIn _ _ (fuel + 88 + nb) _ _ _ (.int (nb : Int))
        (nblocks_eval _ len hlen (fuel + 88 + nb) nb hnbe (by simpa [Env.bind] using hlenv))]
  rw [show fuel + 88 + nb = (fuel + 87 + nb) + 1 by omega,
      eval_letIn _ _ (fuel + 87 + nb) _ _ _ (.int ((nb * 64 : Nat) : Int))
        (plen_eval _ nb (fuel + 87 + nb) (by simp [Env.bind]))]
  rw [show fuel + 87 + nb = ((fuel + 85 + nb) + 1) + 1 by omega,
      eval_letIn _ _ ((fuel + 85 + nb) + 1) _ _ _ _
        (bits_val _ len (by simpa [Env.bind] using hlenv) (fuel + 85 + nb)),
      show (fuel + 85 + nb) + 1 = fuel + 86 + nb by omega]
  rw [show fuel + 86 + nb = ((fuel + 84 + nb) + 1) + 1 by omega,
      eval_letIn _ _ ((fuel + 84 + nb) + 1) _ _ _ _
        (buf_store (bufUpd df len 128) _ (fuel + 84 + nb) (.binOp .sub (.var "plen") (.lit (.int 1))) lenStore0
          (plenOf len - 1) (lenByte len 0) (by simp [Env.bind])
          (hidxk _ 1 (fuel + 84 + nb) (by omega) (by simp [Env.bind]))
          (by rw [show (fuel + 84 + nb) + 1 = (fuel + 83 + nb) + 2 by omega]
              exact lenStore0_eval _ (fuel + 83 + nb) len (by simp [Env.bind])) (by omega)),
      show (fuel + 84 + nb) + 1 = fuel + 85 + nb by omega]
  rw [show fuel + 85 + nb = ((fuel + 83 + nb) + 1) + 1 by omega,
      eval_letIn _ _ ((fuel + 83 + nb) + 1) _ _ _ _
        (buf_store (bufUpd (bufUpd df len 128) (plenOf len - 1) (lenByte len 0)) _ (fuel + 83 + nb)
          (.binOp .sub (.var "plen") (.lit (.int 2))) (lenStoreS 8)
          (plenOf len - 2) (lenByte len 1) (by simp [Env.bind])
          (hidxk _ 2 (fuel + 83 + nb) (by omega) (by simp [Env.bind]))
          (by rw [show (fuel + 83 + nb) + 1 = (fuel + 81 + nb) + 3 by omega]
              exact lenStoreS_eval _ (fuel + 81 + nb) len 1 (by simp [Env.bind])) (by omega)),
      show (fuel + 83 + nb) + 1 = fuel + 84 + nb by omega]
  rw [show fuel + 84 + nb = ((fuel + 82 + nb) + 1) + 1 by omega,
      eval_letIn _ _ ((fuel + 82 + nb) + 1) _ _ _ _
        (buf_store (bufUpd (bufUpd (bufUpd df len 128) (plenOf len - 1) (lenByte len 0)) (plenOf len - 2) (lenByte len 1)) _ (fuel + 82 + nb)
          (.binOp .sub (.var "plen") (.lit (.int 3))) (lenStoreS 16)
          (plenOf len - 3) (lenByte len 2) (by simp [Env.bind])
          (hidxk _ 3 (fuel + 82 + nb) (by omega) (by simp [Env.bind]))
          (by rw [show (fuel + 82 + nb) + 1 = (fuel + 80 + nb) + 3 by omega]
              exact lenStoreS_eval _ (fuel + 80 + nb) len 2 (by simp [Env.bind])) (by omega)),
      show (fuel + 82 + nb) + 1 = fuel + 83 + nb by omega]
  rw [show fuel + 83 + nb = ((fuel + 81 + nb) + 1) + 1 by omega,
      eval_letIn _ _ ((fuel + 81 + nb) + 1) _ _ _ _
        (buf_store (bufUpd (bufUpd (bufUpd (bufUpd df len 128) (plenOf len - 1) (lenByte len 0)) (plenOf len - 2) (lenByte len 1)) (plenOf len - 3) (lenByte len 2)) _ (fuel + 81 + nb)
          (.binOp .sub (.var "plen") (.lit (.int 4))) (lenStoreS 24)
          (plenOf len - 4) (lenByte len 3) (by simp [Env.bind])
          (hidxk _ 4 (fuel + 81 + nb) (by omega) (by simp [Env.bind]))
          (by rw [show (fuel + 81 + nb) + 1 = (fuel + 79 + nb) + 3 by omega]
              exact lenStoreS_eval _ (fuel + 79 + nb) len 3 (by simp [Env.bind])) (by omega)),
      show (fuel + 81 + nb) + 1 = fuel + 82 + nb by omega]
  rw [show fuel + 82 + nb = ((fuel + 79 + nb) + 2) + 1 by omega,
      eval_letIn _ _ ((fuel + 79 + nb) + 2) _ _ _ _ (sha256_init_call _ (fuel + 79 + nb)),
      show (fuel + 79 + nb) + 2 = fuel + 81 + nb by omega]
  rw [show fuel + 81 + nb = (fuel + 80 + nb) + 1 by omega,
      eval_letIn _ _ (fuel + 80 + nb) _ _ _ (.int 0) (by simp [eval])]
  rw [← hsEnv_self (((((((((((e.bind "buf" (.array_ (bufArr df))).bind "buf" (.array_ (bufArr (bufUpd df len 128)))).bind "nblocks" (.int (nb:Int))).bind "plen" (.int ((nb*64:Nat):Int))).bind "bits" (.int ((len*8:Nat):Int))).bind "buf" (.array_ (bufArr (bufUpd (bufUpd df len 128) (plenOf len - 1) (lenByte len 0))))).bind "buf" (.array_ (bufArr (bufUpd (bufUpd (bufUpd df len 128) (plenOf len - 1) (lenByte len 0)) (plenOf len - 2) (lenByte len 1))))).bind "buf" (.array_ (bufArr (bufUpd (bufUpd (bufUpd (bufUpd df len 128) (plenOf len - 1) (lenByte len 0)) (plenOf len - 2) (lenByte len 1)) (plenOf len - 3) (lenByte len 2))))).bind "buf" (.array_ (bufArr (bufUpd (bufUpd (bufUpd (bufUpd (bufUpd df len 128) (plenOf len - 1) (lenByte len 0)) (plenOf len - 2) (lenByte len 1)) (plenOf len - 3) (lenByte len 2)) (plenOf len - 4) (lenByte len 3))))).bind "state" (.array_ (Sha256Spec.initState.map (fun x => PVal.int x.toNat)))).bind "blk" (.int 0)) (padFn df len) (Sha256Spec.padMessage ((List.range len).map df)) nb
        (by simp [Env.bind, hashFold]) (by simp [Env.bind, padFn])
        (by simp [Env.bind]) (by simp [Env.bind])]
  rw [show fuel + 80 + nb = (fuel + 79) + nb + 1 by omega,
      hash_loop_eval _ (padFn df len) (Sha256Spec.padMessage ((List.range len).map df)) nb hnbb
        (fun blk hblk => sliceAt_padFn df len hlen hz blk (by rw [hplnb]; omega)) _ fuel]
  have hml : (Sha256Spec.padMessage ((List.range len).map df)).length = plenOf len := by
    rw [padMessage_length]; simp
  have hfeq : hashFold (Sha256Spec.padMessage ((List.range len).map df)) nb
      = Sha256Spec.hashState ((List.range len).map df) := by
    rw [hashState_eq_fold, hml]; congr 1; omega
  rw [state_to_bytes_call (hashFold (Sha256Spec.padMessage ((List.range len).map df)) nb)
        (hashFold_length _ _) _ (.var "state") (fuel + 62)
        (var_read _ "state" (.array_ ((hashFold (Sha256Spec.padMessage ((List.range len).map df)) nb).map (fun x => PVal.int x.toNat))) (fuel + 77) (by simp only [hsEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]))]
  rw [hfeq, Sha256Spec.hash]

-- ==================================================================
-- HMAC infrastructure (task #21, hmac_sha256 composition): a
-- size-generic buffer model `arrN n` (generalizing `bufArr` = `arrN 384`)
-- for the differently-sized hmac buffers (kp:64, k:128, m:256), with the
-- set/read lemmas; and `copyFn`/`copyFn_step`, the byte-function frame for
-- the `for i in 0..N { dst[off+i] = src[i] }` copy loops hmac runs four
-- times (key copy, digest copy, message copy, inner-digest copy).
-- ==================================================================

-- `arrN`, `arrN_length`, `arrN_set`, `arrN_read` relocated to
-- `Concrete.ProofKit.Array` (Proof Kit v1).

/-- The byte function after copying `src[0..m)` into `dst` at offset `off`. -/
def copyFn (dstFn srcFn : Nat → BitVec 8) (off m : Nat) : Nat → BitVec 8 :=
  fun j => if off ≤ j ∧ j < off + m then srcFn (j - off) else dstFn j

theorem copyFn_zero (dstFn srcFn off) : copyFn dstFn srcFn off 0 = dstFn := by
  funext j; simp only [copyFn, Nat.add_zero]; rw [if_neg (by omega)]

theorem copyFn_step (dstFn srcFn : Nat → BitVec 8) (off m : Nat) :
    bufUpd (copyFn dstFn srcFn off m) (off + m) (srcFn m) = copyFn dstFn srcFn off (m + 1) := by
  funext j
  simp only [bufUpd, copyFn]
  by_cases h : j = off + m
  · subst h
    rw [if_pos rfl, if_pos (show off ≤ off + m ∧ off + m < off + (m + 1) by omega),
        Nat.add_sub_cancel_left]
  · rw [if_neg h]
    by_cases h2 : off ≤ j ∧ j < off + m
    · rw [if_pos h2, if_pos (by omega)]
    · rw [if_neg h2, if_neg (by omega)]

-- ==================================================================
-- Generic copy loop (task #21, hmac): `for i in 0..N { dst[off+i] = src[i] }`
-- refines `copyFn` — fills dst at [off, off+N) from src. Reused for hmac's
-- key copy, key-hash-digest copy, message copy, and inner-digest copy.
-- ==================================================================

def copyEnv (dstNm srcNm iNm : String) (e : Env) (dn sn off : Nat)
    (dstFn srcFn : Nat → BitVec 8) (m : Nat) : Env :=
  ((e.bind dstNm (.array_ (arrN dn (copyFn dstFn srcFn off m)))).bind srcNm
    (.array_ (arrN sn srcFn))).bind iNm (.int (m : Int))

theorem cpy_step (dstNm srcNm iNm : String)
    (hds : dstNm ≠ srcNm) (hdi : dstNm ≠ iNm) (hsi : srcNm ≠ iNm)
    (e : Env) (dn sn off : Nat) (dstFn srcFn : Nat → BitVec 8) (m : Nat)
    (hdsn : off + m < dn) (hsm : m < sn) (idxE : PExpr) (fuel : Nat)
    (hidx : eval shaFns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m) (fuel + 1) idxE
      = some (.int ((off + m : Nat) : Int))) :
    eval.evalAssigns shaFns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m) (fuel + 2)
      [ (dstNm, .arraySet (.var dstNm) idxE (.arrayIndex (.var srcNm) (.var iNm)))
      , (iNm, .binOp .add (.var iNm) (.lit (.int 1))) ]
      = some (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn (m + 1)) := by
  have e_id : (iNm == dstNm) = false := beq_false_of_ne (Ne.symm hdi)
  have e_di : (dstNm == iNm) = false := beq_false_of_ne hdi
  have e_ds : (dstNm == srcNm) = false := beq_false_of_ne hds
  have e_sd : (srcNm == dstNm) = false := beq_false_of_ne (Ne.symm hds)
  have e_si : (srcNm == iNm) = false := beq_false_of_ne hsi
  have e_is : (iNm == srcNm) = false := beq_false_of_ne (Ne.symm hsi)
  have hdb : (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m) dstNm
      = some (.array_ (arrN dn (copyFn dstFn srcFn off m))) := by
    simp [copyEnv, Env.bind, e_di, e_ds]
  have hsv : eval shaFns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m) (fuel + 1)
      (.arrayIndex (.var srcNm) (.var iNm)) = some (.int ↑(srcFn m).toNat) := by
    simp only [eval, copyEnv, Env.bind, e_si, e_is, if_false, if_true,
      beq_self_eq_true, beq_iff_eq, reduceCtorEq]
    exact arrN_read sn srcFn (m : Int) m rfl hsm
  have harr : eval shaFns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m) (fuel + 2)
      (.arraySet (.var dstNm) idxE (.arrayIndex (.var srcNm) (.var iNm)))
      = some (.array_ (arrN dn (copyFn dstFn srcFn off (m + 1)))) := by
    rw [eval_arraySet_lemma shaFns _ (fuel + 1) (.var dstNm) idxE _
        (arrN dn (copyFn dstFn srcFn off m)) ((off + m : Nat) : Int) (.int ↑(srcFn m).toNat)
        (by simp only [eval]; exact hdb) hidx hsv (by omega)
        (by rw [show ((off + m : Nat) : Int).toNat = off + m by omega, arrN_length]; omega)]
    rw [show ((off + m : Nat) : Int).toNat = off + m by omega, arrN_set, copyFn_step]
  simp only [eval.evalAssigns, harr]
  simp only [eval, evalBinOp, Env.bind, copyEnv, e_id, e_di, e_ds, e_sd, e_si, e_is,
    beq_self_eq_true, if_true, beq_iff_eq, if_false, reduceCtorEq, Option.some.injEq]
  funext n
  by_cases h1 : (n == iNm) = true <;> by_cases h2 : (n == dstNm) = true <;>
    simp_all [Env.bind, copyEnv, beq_iff_eq, Option.some.injEq, PVal.int.injEq] <;> omega

theorem copy_loop (dstNm srcNm iNm : String)
    (hds : dstNm ≠ srcNm) (hdi : dstNm ≠ iNm) (hsi : srcNm ≠ iNm)
    (e : Env) (dn sn off : Nat) (dstFn srcFn : Nat → BitVec 8) (N : Nat)
    (hdn : off + N ≤ dn) (hsn : N ≤ sn) (condE idxE cont : PExpr) (base : Nat)
    (hidx : ∀ m, m < N → eval shaFns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m) (base + 1) idxE
        = some (.int ((off + m : Nat) : Int)))
    (hct : ∀ m, m < N → eval shaFns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m) (base + 2) condE
        = some (.bool true))
    (hcf : eval shaFns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn N) (base + 2) condE
        = some (.bool false)) :
    eval shaFns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn 0) ((base + 2) + N + 1)
      (.while_ condE
        [ (dstNm, .arraySet (.var dstNm) idxE (.arrayIndex (.var srcNm) (.var iNm)))
        , (iNm, .binOp .add (.var iNm) (.lit (.int 1))) ] cont)
      = eval shaFns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn N) (base + 2) cont :=
  eval_while_count shaFns condE _ cont (fun m => copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m)
    N (base + 2)
    (fun m hm => ⟨hct m hm, cpy_step dstNm srcNm iNm hds hdi hsi e dn sn off dstFn srcFn m
      (by omega) (by omega) idxE base (hidx m hm)⟩)
    hcf

-- ==================================================================
-- ipad/opad xor loop (task #21, hmac): the two-buffer loop
-- `for i in 0..64 { inner[i] = kp[i]^0x36; outer[i] = kp[i]^0x5c }` fills
-- inner/outer with the key XORed against the FIPS pads (= copyFn from the
-- xored key into a zero buffer). copyFn_step0 (off=0), xorval_eval
-- (the per-byte xor, bridged by ofInt8_natCast_toNat).
-- ==================================================================

-- `ofInt8_natCast_toNat` relocated to `Concrete.ProofKit.BitVec` (Proof Kit v1).

theorem xorval_eval (kpFn : Nat → BitVec 8) (env : Env) (i : Nat) (c : Int) (hi : i < 64) (fuel : Nat)
    (hkp : env "kp" = some (.array_ (arrN 64 kpFn))) (hiv : env "i" = some (.int (i : Int))) :
    eval shaFns env (fuel + 2)
      (.binOp (.bitxor 8 false) (.arrayIndex (.var "kp") (.var "i")) (.lit (.int c)))
      = some (.int (kpFn i ^^^ BitVec.ofInt 8 c).toNat) := by
  have hr : eval shaFns env (fuel + 2) (.arrayIndex (.var "kp") (.var "i"))
      = some (.int ↑(kpFn i).toNat) := by
    simp only [eval, hkp, hiv]
    exact arrN_read 64 kpFn (i : Int) i rfl hi
  simp only [eval, hr, evalBinOp, ofInt8_natCast_toNat, Option.some.injEq, PVal.int.injEq, Int.ofNat_eq_natCast]

theorem copyFn_step0 (dstFn srcFn : Nat → BitVec 8) (m : Nat) :
    bufUpd (copyFn dstFn srcFn 0 m) m (srcFn m) = copyFn dstFn srcFn 0 (m + 1) := by
  have h := copyFn_step dstFn srcFn 0 m; rwa [Nat.zero_add] at h

-- `zfn` relocated to `Concrete.ProofKit.Array` (Proof Kit v1).
def ipadFn (kpFn : Nat → BitVec 8) (c : Int) : Nat → BitVec 8 := fun j => kpFn j ^^^ BitVec.ofInt 8 c

def xorEnv (e : Env) (kpFn : Nat → BitVec 8) (m : Nat) : Env :=
  ((((e.bind "inner" (.array_ (bufArr (copyFn zfn (ipadFn kpFn 54) 0 m)))).bind
     "outer" (.array_ (bufArr (copyFn zfn (ipadFn kpFn 92) 0 m)))).bind
     "kp" (.array_ (arrN 64 kpFn))).bind "i" (.int (m : Int)))

-- `xorE`, `xorAssigns` relocated to `Concrete.Proof` (task #22).
set_option maxHeartbeats 1000000 in
theorem xor_step (e : Env) (kpFn : Nat → BitVec 8) (m : Nat) (hm : m < 64) (fuel : Nat) :
    eval.evalAssigns shaFns (xorEnv e kpFn m) (fuel + 3) xorAssigns = some (xorEnv e kpFn (m + 1)) := by
  have hkp : (xorEnv e kpFn m) "kp" = some (.array_ (arrN 64 kpFn)) := by simp [xorEnv, Env.bind]
  have hiv : (xorEnv e kpFn m) "i" = some (.int (m : Int)) := by simp [xorEnv, Env.bind]
  have hinner : eval shaFns (xorEnv e kpFn m) (fuel + 2) (.var "inner")
      = some (.array_ (bufArr (copyFn zfn (ipadFn kpFn 54) 0 m))) := by simp [eval, xorEnv, Env.bind]
  -- inner store
  have harr1 : eval shaFns (xorEnv e kpFn m) (fuel + 3)
      (.arraySet (.var "inner") (.var "i") (xorE 54))
      = some (.array_ (bufArr (copyFn zfn (ipadFn kpFn 54) 0 (m + 1)))) := by
    rw [eval_arraySet_lemma shaFns _ (fuel + 2) (.var "inner") (.var "i") (xorE 54)
        (bufArr (copyFn zfn (ipadFn kpFn 54) 0 m)) (m : Int) (.int (kpFn m ^^^ BitVec.ofInt 8 54).toNat)
        hinner (by simp [eval, hiv]) (xorval_eval kpFn _ m 54 hm fuel hkp hiv) (by omega)
        (by rw [show ((m:Int)).toNat = m by omega, bufArr]; simp; omega)]
    rw [show ((m:Int)).toNat = m by omega,
        show (kpFn m ^^^ BitVec.ofInt 8 54) = ipadFn kpFn 54 m from rfl, bufArr_set, copyFn_step0]
  -- outer store, under the env with "inner" already updated
  have harr2 : eval shaFns ((xorEnv e kpFn m).bind "inner"
        (.array_ (bufArr (copyFn zfn (ipadFn kpFn 54) 0 (m + 1))))) (fuel + 3)
      (.arraySet (.var "outer") (.var "i") (xorE 92))
      = some (.array_ (bufArr (copyFn zfn (ipadFn kpFn 92) 0 (m + 1)))) := by
    have ho : eval shaFns ((xorEnv e kpFn m).bind "inner"
          (.array_ (bufArr (copyFn zfn (ipadFn kpFn 54) 0 (m + 1))))) (fuel + 2) (.var "outer")
        = some (.array_ (bufArr (copyFn zfn (ipadFn kpFn 92) 0 m))) := by simp [eval, xorEnv, Env.bind]
    rw [eval_arraySet_lemma shaFns _ (fuel + 2) (.var "outer") (.var "i") (xorE 92)
        (bufArr (copyFn zfn (ipadFn kpFn 92) 0 m)) (m : Int) (.int (kpFn m ^^^ BitVec.ofInt 8 92).toNat)
        ho (by simp [eval, xorEnv, Env.bind])
        (xorval_eval kpFn _ m 92 hm fuel (by simp [xorEnv, Env.bind]) (by simp [xorEnv, Env.bind])) (by omega)
        (by rw [show ((m:Int)).toNat = m by omega, bufArr]; simp; omega)]
    rw [show ((m:Int)).toNat = m by omega,
        show (kpFn m ^^^ BitVec.ofInt 8 92) = ipadFn kpFn 92 m from rfl, bufArr_set, copyFn_step0]
  simp only [xorAssigns, eval.evalAssigns, harr1, harr2]
  simp only [eval, evalBinOp, Env.bind, xorEnv, beq_self_eq_true, if_true, beq_iff_eq, if_false,
    String.reduceEq, reduceCtorEq, Option.some.injEq]
  funext n
  by_cases h1 : (n == "i") = true <;> by_cases h2 : (n == "outer") = true <;>
    by_cases h3 : (n == "inner") = true <;>
    simp_all [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, if_false, String.reduceEq,
      reduceCtorEq, Option.some.injEq, PVal.int.injEq] <;> omega

-- `condX` relocated to `Concrete.Proof` (task #22).
theorem condX_true (e : Env) (kpFn : Nat → BitVec 8) (m : Nat) (hm : m < 64) (F : Nat) :
    eval shaFns (xorEnv e kpFn m) (F + 1) condX = some (.bool true) := by
  simp only [condX, xorEnv, eval, Env.bind, evalBinOp]; simp; omega
theorem condX_false (e : Env) (kpFn : Nat → BitVec 8) (F : Nat) :
    eval shaFns (xorEnv e kpFn 64) (F + 1) condX = some (.bool false) := by
  simp only [condX, xorEnv, eval, Env.bind, evalBinOp]; simp

theorem xor_loop (e : Env) (kpFn : Nat → BitVec 8) (cont : PExpr) (base : Nat) :
    eval shaFns (xorEnv e kpFn 0) ((base + 3) + 64 + 1) (.while_ condX xorAssigns cont)
      = eval shaFns (xorEnv e kpFn 64) (base + 3) cont :=
  eval_while_count shaFns condX xorAssigns cont (fun m => xorEnv e kpFn m) 64 (base + 3)
    (fun m hm => ⟨condX_true e kpFn m hm (base + 2), xor_step e kpFn m hm base⟩)
    (condX_false e kpFn (base + 2))

theorem sha256_hash_call (df : Nat → BitVec 8) (len : Nat) (hlen : len ≤ 375)
    (hz : ∀ i, len ≤ i → df i = 0) (env : Env) (dataE lenE : PExpr) (fuel : Nat)
    (hdata : eval shaFns env (fuel + 91 + (len + 9 + 63) / 64) dataE = some (.array_ (bufArr df)))
    (hlenv : eval shaFns env (fuel + 91 + (len + 9 + 63) / 64) lenE = some (.int (len : Int))) :
    eval shaFns env ((fuel + 91 + (len + 9 + 63) / 64) + 1) (.call "sha256_hash" [dataE, lenE])
      = some (.array_ ((Sha256Spec.hash ((List.range len).map df)).map (fun b => PVal.int b.toNat))) := by
  simp only [eval, shaFns, eval.evalArgs, hdata, hlenv, bindArgs]
  exact sha256_hash_refines_spec df len hlen hz _ fuel (by simp [Env.bind]) (by simp [Env.bind])

-- ==================================================================
-- HMAC spec-side correspondences (task #21): a copyFn-built buffer's
-- prefix splits as `dst-prefix ++ src` (copyFn_map_append) — matching the
-- spec's `(kp^pad) ++ message` shape — and a copy into zeros is
-- `src ++ zeros` (copyFn_zfn_map) — matching keyPrep's 64-byte padding.
-- ==================================================================

theorem copyFn_map_append (dstFn srcFn : Nat → BitVec 8) (off N : Nat) :
    (List.range (off + N)).map (copyFn dstFn srcFn off N)
      = ((List.range off).map dstFn) ++ ((List.range N).map srcFn) := by
  apply List.ext_getElem (by simp)
  intro j h1 _
  simp only [List.length_map, List.length_range] at h1
  rw [List.getElem_map, List.getElem_range, List.getElem_append]
  by_cases hj : j < off
  · rw [dif_pos (by simp only [List.length_map, List.length_range]; exact hj)]
    simp only [List.getElem_map, List.getElem_range, copyFn,
      if_neg (show ¬ (off ≤ j ∧ j < off + N) by omega)]
  · rw [dif_neg (by simp only [List.length_map, List.length_range]; omega)]
    simp only [List.length_map, List.length_range, List.getElem_map, List.getElem_range, copyFn,
      if_pos (show off ≤ j ∧ j < off + N by omega)]

/-- A `copyFn` into a zero buffer, viewed over `[0, T)`, is the copied prefix
    followed by zeros — exactly the shape of `keyPrep`. -/
theorem copyFn_zfn_map (srcFn : Nat → BitVec 8) (k_len T : Nat) (h : k_len ≤ T) :
    (List.range T).map (copyFn zfn srcFn 0 k_len)
      = ((List.range k_len).map srcFn) ++ List.replicate (T - k_len) 0 := by
  apply List.ext_getElem (by simp; omega)
  intro j h1 _
  simp only [List.length_map, List.length_range] at h1
  rw [List.getElem_map, List.getElem_range, List.getElem_append]
  by_cases hj : j < k_len
  · rw [dif_pos (by simp only [List.length_map, List.length_range]; exact hj)]
    simp only [List.getElem_map, List.getElem_range, copyFn, Nat.sub_zero, if_pos (show 0 ≤ j ∧ j < 0 + k_len by omega)]
  · rw [dif_neg (by simp only [List.length_map, List.length_range]; omega)]
    simp only [List.getElem_replicate, copyFn, zfn, if_neg (show ¬ (0 ≤ j ∧ j < 0 + k_len) by omega)]

-- ==================================================================
-- HMAC key-prep correspondence (task #21): the post-branch kp buffer
-- equals Sha256Spec.keyPrep of the key — both branches. kp_else (short
-- key: copy then zero-pad to 64); kp_if (long key: hash then zero-pad).
-- Plus the digest length facts (hashState = 8 words, hash = 32 bytes).
-- ==================================================================

theorem hashState_length (msg : List Sha256Spec.Byte) : (Sha256Spec.hashState msg).length = 8 := by
  rw [hashState_eq_fold]; exact hashFold_length _ _

theorem hash_length (msg : List Sha256Spec.Byte) : (Sha256Spec.hash msg).length = 32 := by
  rw [Sha256Spec.hash]; exact stateToBytes_length _ (Nat.le_of_eq (hashState_length msg).symm)

theorem kp_else (kFn : Nat → BitVec 8) (k_len : Nat) (h : k_len ≤ 64) :
    (List.range 64).map (copyFn zfn kFn 0 k_len) = Sha256Spec.keyPrep ((List.range k_len).map kFn) := by
  rw [copyFn_zfn_map kFn k_len 64 h]
  unfold Sha256Spec.keyPrep
  simp only [List.length_map, List.length_range, gt_iff_lt, if_neg (by omega : ¬ (64 < k_len))]

theorem kp_if (khFn : Nat → BitVec 8) (key : List Sha256Spec.Byte) (hlen : key.length > 64)
    (hkh : (List.range 32).map khFn = Sha256Spec.hash key) :
    (List.range 64).map (copyFn zfn khFn 0 32) = Sha256Spec.keyPrep key := by
  rw [copyFn_zfn_map khFn 32 64 (by omega), hkh]
  unfold Sha256Spec.keyPrep
  simp only [if_pos hlen, hash_length]

-- ==================================================================
-- HMAC assembly scaffolding (task #21): the forced local helpers for
-- wiring hmac_sha256_refines_spec — zero-buffer / env-reshape lemmas
-- (arrN_zfn, xorEnv_self, copyEnv_self), the linear-body expression
-- (hmacLinearExpr) + its loop conds/assigns, and the monotone
-- (fixed-fuel) hash-call wrapper sha256_hash_call_at, which discharges
-- the symbolic-nb fuel so the two nested sha256_hash calls thread at a
-- clean fuel (nb ≤ 6 for len ≤ 375, via eval_fuel_le).
-- ==================================================================

-- `arrN_zfn` relocated to `Concrete.ProofKit.Array` (Proof Kit v1).
theorem bufArr_zfn : bufArr zfn = List.replicate 384 (PVal.int 0) := arrN_zfn 384
theorem bufArr_eq (f : Nat → BitVec 8) : bufArr f = arrN 384 f := rfl

theorem xorEnv_self (e : Env) (kpFn : Nat → BitVec 8)
    (hin : e "inner" = some (.array_ (bufArr zfn))) (hout : e "outer" = some (.array_ (bufArr zfn)))
    (hkp : e "kp" = some (.array_ (arrN 64 kpFn))) (hi : e "i" = some (.int 0)) :
    xorEnv e kpFn 0 = e := by
  funext n
  simp only [xorEnv, Env.bind, copyFn_zero]
  by_cases h1 : n = "i" <;> by_cases h2 : n = "kp" <;> by_cases h3 : n = "outer" <;>
    by_cases h4 : n = "inner" <;> simp_all [beq_iff_eq]
theorem copyEnv_self (dstNm srcNm iNm : String) (e : Env) (dn sn off : Nat)
    (dstFn srcFn : Nat → BitVec 8)
    (hd : e dstNm = some (.array_ (arrN dn dstFn))) (hs : e srcNm = some (.array_ (arrN sn srcFn)))
    (hi : e iNm = some (.int 0)) :
    copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn 0 = e := by
  funext n
  simp only [copyEnv, Env.bind, copyFn_zero]
  by_cases h1 : n = iNm <;> by_cases h2 : n = srcNm <;> by_cases h3 : n = dstNm <;>
    simp_all [beq_iff_eq]

-- `condMsg`, `msgAssigns`, `condIh`, `ihAssigns`, `hmacLinearExpr` relocated
-- to `Concrete.Proof` (task #22).
theorem arrayLit_zeros384 (fns : FnTable) (env : Env) (fuel : Nat) :
    eval fns env (fuel + 2) (.arrayLit (List.replicate 384 (.lit (.int 0))))
      = some (.array_ (List.replicate 384 (PVal.int 0))) := by
  simp only [eval, evalElems_replicate_lit]

-- a length-N list equals arrN N of its getD
-- `map_toNat_eq_arrN` relocated to `Concrete.ProofKit.Refinement` (Proof Kit v1).

theorem sha256_hash_call_at (df : Nat → BitVec 8) (len : Nat) (hlen : len ≤ 375)
    (hz : ∀ i, len ≤ i → df i = 0) (env : Env) (dataE lenE : PExpr) (G : Nat) (hG : 97 ≤ G)
    (hdata : eval shaFns env G dataE = some (.array_ (bufArr df)))
    (hlenv : eval shaFns env G lenE = some (.int (len : Int))) :
    eval shaFns env (G + 1) (.call "sha256_hash" [dataE, lenE])
      = some (.array_ ((Sha256Spec.hash ((List.range len).map df)).map (fun b => PVal.int b.toNat))) := by
  have hnb6 : (len + 9 + 63) / 64 ≤ 6 := by omega
  have key : G = (G - 91 - (len + 9 + 63) / 64) + 91 + (len + 9 + 63) / 64 := by omega
  have h := sha256_hash_call df len hlen hz env dataE lenE (G - 91 - (len + 9 + 63) / 64)
      (by rw [← key]; exact hdata) (by rw [← key]; exact hlenv)
  rwa [← key] at h

-- ==================================================================
-- HMAC linear composition (task #21): given kp = keyPrep(key), the body
-- after key-prep — ipad/opad xor, message copy, inner sha256_hash, digest
-- copy, outer sha256_hash — computes exactly Sha256Spec.hmac key message.
-- The full eval assembly threaded through the loop/call lemmas, folded to
-- the spec via copyFn_map_append + copyFn_zfn_map + the keyPrep bridge.
-- ==================================================================

theorem ofInt8_const54 : BitVec.ofInt 8 54 = (54 : BitVec 8) := by decide
theorem ofInt8_const92 : BitVec.ofInt 8 92 = (92 : BitVec 8) := by decide

-- the [0,64) prefix of a copy-into-zeros over [0,64) is just the source map
theorem ipad_prefix (kpFn : Nat → BitVec 8) (c : Int) :
    (List.range 64).map (copyFn zfn (ipadFn kpFn c) 0 64) = (List.range 64).map (ipadFn kpFn c) := by
  rw [show (64 : Nat) = 0 + 64 by omega, copyFn_map_append]; simp
-- `list_eq_rangeGetD` relocated to `Concrete.ProofKit.Refinement` (Proof Kit v1).

set_option maxRecDepth 8000 in
set_option maxHeartbeats 4000000 in
theorem hmac_linear (kpFn mFn : Nat → BitVec 8) (KEY : List Sha256Spec.Byte) (m_len : Nat)
    (hml : m_len ≤ 256) (hkp : (List.range 64).map kpFn = Sha256Spec.keyPrep KEY)
    (e : Env) (fuel : Nat)
    (hkpv : e "kp" = some (.array_ (arrN 64 kpFn)))
    (hmv : e "m" = some (.array_ (arrN 256 mFn)))
    (hmlen : e "m_len" = some (.int (m_len : Int))) :
    eval shaFns e (fuel + 203 + m_len) hmacLinearExpr
      = some (.array_ ((Sha256Spec.hmac KEY ((List.range m_len).map mFn)).map (fun b => PVal.int b.toNat))) := by
  rw [hmacLinearExpr]
  -- init letIns
  rw [show fuel + 203 + m_len = ((fuel + 201 + m_len) + 1) + 1 by omega,
      eval_letIn _ _ ((fuel + 201 + m_len) + 1) _ _ _ _ (arrayLit_zeros384 _ _ (fuel + 201 + m_len)),
      show (fuel + 201 + m_len) + 1 = fuel + 202 + m_len by omega]
  rw [show fuel + 202 + m_len = ((fuel + 200 + m_len) + 1) + 1 by omega,
      eval_letIn _ _ ((fuel + 200 + m_len) + 1) _ _ _ _ (arrayLit_zeros384 _ _ (fuel + 200 + m_len)),
      show (fuel + 200 + m_len) + 1 = fuel + 201 + m_len by omega]
  rw [show fuel + 201 + m_len = (fuel + 200 + m_len) + 1 by omega,
      eval_letIn _ _ (fuel + 200 + m_len) _ _ _ (.int 0) (by simp [eval])]
  -- xor loop
  rw [← xorEnv_self (((e.bind "inner" (.array_ (List.replicate 384 (PVal.int 0)))).bind "outer" (.array_ (List.replicate 384 (PVal.int 0)))).bind "i" (.int 0)) kpFn
        (by simp only [Env.bind, bufArr_zfn, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false])
        (by simp only [Env.bind, bufArr_zfn, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false])
        (by simp only [Env.bind, beq_self_eq_true, if_true, String.reduceEq, reduceCtorEq, if_false]; exact hkpv)
        (by simp only [Env.bind, beq_self_eq_true, if_true])]
  rw [show fuel + 200 + m_len = ((fuel + 132 + m_len) + 3) + 64 + 1 by omega, xor_loop _ kpFn _ (fuel + 132 + m_len)]
  -- stage 2: reset i + message copy
  rw [show fuel + 132 + m_len + 3 = (fuel + 134 + m_len) + 1 by omega,
      eval_letIn _ _ (fuel + 134 + m_len) _ _ _ (.int 0) (by simp [eval])]
  rw [← copyEnv_self "inner" "m" "i" ((xorEnv (((e.bind "inner" (.array_ (List.replicate 384 (PVal.int 0)))).bind "outer" (.array_ (List.replicate 384 (PVal.int 0)))).bind "i" (.int 0)) kpFn 64).bind "i" (.int 0)) 384 256 64 (copyFn zfn (ipadFn kpFn 54) 0 64) mFn
        (by simp only [Env.bind, xorEnv, bufArr_eq, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false])
        (by simp only [Env.bind, xorEnv, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; exact hmv) (by simp only [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false])]
  simp only [msgAssigns]
  rw [show fuel + 134 + m_len = (fuel + 131 + 2) + m_len + 1 by omega,
      copy_loop "inner" "m" "i" (by decide) (by decide) (by decide) _ 384 256 64
        (copyFn zfn (ipadFn kpFn 54) 0 64) mFn m_len (by omega) hml condMsg
        (.binOp .add (.lit (.int 64)) (.var "i")) _ (fuel + 131)
        (fun mm hmm => by simp only [eval, evalBinOp, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false, Option.some.injEq, PVal.int.injEq]; omega)
        (fun mm hmm => by simp only [condMsg, eval, evalBinOp, copyEnv, xorEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false, hmlen]; simp; omega)
        (by simp only [condMsg, eval, evalBinOp, copyEnv, xorEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false, hmlen]; simp)]
  -- stage 3: inner hash
  rw [show fuel + 133 = (fuel + 132) + 1 by omega,
      eval_letIn _ _ (fuel + 132) _ _ _ _
        (sha256_hash_call_at (copyFn (copyFn zfn (ipadFn kpFn 54) 0 64) mFn 64 m_len) (64 + m_len) (by omega)
          (fun i hi => by simp only [copyFn, zfn]; rw [if_neg (by omega), if_neg (by omega)])
          (copyEnv "inner" "m" "i" ((xorEnv (((e.bind "inner" (.array_ (List.replicate 384 (PVal.int 0)))).bind "outer" (.array_ (List.replicate 384 (PVal.int 0)))).bind "i" (.int 0)) kpFn 64).bind "i" (.int 0)) 384 256 64 (copyFn zfn (ipadFn kpFn 54) 0 64) mFn m_len) (.var "inner") (.binOp .add (.lit (.int 64)) (.var "m_len")) (fuel + 132) (by omega)
          (by simp only [eval, copyEnv, Env.bind, bufArr_eq, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false])
          (by simp only [eval, evalBinOp, copyEnv, xorEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false, hmlen, Option.some.injEq, PVal.int.injEq]; omega))]
  rw [map_toNat_eq_arrN, hash_length]
  -- stage 4: reset i + ih copy into outer
  rw [show fuel + 132 = (fuel + 131) + 1 by omega,
      eval_letIn _ _ (fuel + 131) _ _ _ (.int 0) (by simp [eval])]
  rw [← copyEnv_self "outer" "ih" "i" (((copyEnv "inner" "m" "i" ((xorEnv (((e.bind "inner" (.array_ (List.replicate 384 (PVal.int 0)))).bind "outer" (.array_ (List.replicate 384 (PVal.int 0)))).bind "i" (.int 0)) kpFn 64).bind "i" (.int 0)) 384 256 64 (copyFn zfn (ipadFn kpFn 54) 0 64) mFn m_len).bind "ih" (.array_ (arrN 32 (fun j => (Sha256Spec.hash ((List.range (64 + m_len)).map (copyFn (copyFn zfn (ipadFn kpFn 54) 0 64) mFn 64 m_len))).getD j 0)))).bind "i" (.int 0)) 384 32 64 (copyFn zfn (ipadFn kpFn 92) 0 64) (fun j => (Sha256Spec.hash ((List.range (64 + m_len)).map (copyFn (copyFn zfn (ipadFn kpFn 54) 0 64) mFn 64 m_len))).getD j 0)
        (by simp only [Env.bind, copyEnv, xorEnv, bufArr_eq, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false])
        (by simp only [Env.bind, copyEnv, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]) (by simp only [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false])]
  simp only [ihAssigns]
  rw [show fuel + 131 = (fuel + 96 + 2) + 32 + 1 by omega,
      copy_loop "outer" "ih" "i" (by decide) (by decide) (by decide) _ 384 32 64
        (copyFn zfn (ipadFn kpFn 92) 0 64) (fun j => (Sha256Spec.hash ((List.range (64 + m_len)).map (copyFn (copyFn zfn (ipadFn kpFn 54) 0 64) mFn 64 m_len))).getD j 0) 32 (by omega) (by omega) condIh
        (.binOp .add (.lit (.int 64)) (.var "i")) _ (fuel + 96)
        (fun mm hmm => by simp only [eval, evalBinOp, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false, Option.some.injEq, PVal.int.injEq]; omega)
        (fun mm hmm => by simp only [condIh, eval, evalBinOp, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; simp; omega)
        (by simp only [condIh, eval, evalBinOp, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; simp)]
  -- stage 5: outer hash
  rw [show fuel + 98 = (fuel + 97) + 1 by omega,
      sha256_hash_call_at (copyFn (copyFn zfn (ipadFn kpFn 92) 0 64) (fun j => (Sha256Spec.hash ((List.range (64 + m_len)).map (copyFn (copyFn zfn (ipadFn kpFn 54) 0 64) mFn 64 m_len))).getD j 0) 64 32) 96 (by omega)
        (fun i hi => by simp only [copyFn, zfn]; rw [if_neg (by omega), if_neg (by omega)])
        _ (.var "outer") (.lit (.int 96)) (fuel + 97) (by omega)
        (by simp only [eval, copyEnv, Env.bind, bufArr_eq, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false])
        (by simp [eval])]
  -- fold to hmac
  have hkp54 : (Sha256Spec.keyPrep KEY).map (fun b => b ^^^ 54) = (List.range 64).map (ipadFn kpFn 54) := by
    rw [← hkp, List.map_map]; apply List.map_congr_left; intro j _
    simp only [ipadFn, ofInt8_const54, Function.comp]
  have hkp92 : (Sha256Spec.keyPrep KEY).map (fun b => b ^^^ 92) = (List.range 64).map (ipadFn kpFn 92) := by
    rw [← hkp, List.map_map]; apply List.map_congr_left; intro j _
    simp only [ipadFn, ofInt8_const92, Function.comp]
  have hihlist : (List.range 32).map (fun j => (Sha256Spec.hash ((List.range (64 + m_len)).map (copyFn (copyFn zfn (ipadFn kpFn 54) 0 64) mFn 64 m_len))).getD j 0)
      = Sha256Spec.hash ((List.range (64 + m_len)).map (copyFn (copyFn zfn (ipadFn kpFn 54) 0 64) mFn 64 m_len)) := by
    rw [show (32:Nat) = (Sha256Spec.hash ((List.range (64 + m_len)).map (copyFn (copyFn zfn (ipadFn kpFn 54) 0 64) mFn 64 m_len))).length from (hash_length _).symm]
    exact list_eq_rangeGetD _
  have hinner : (List.range (64 + m_len)).map (copyFn (copyFn zfn (ipadFn kpFn 54) 0 64) mFn 64 m_len)
      = (Sha256Spec.keyPrep KEY).map (fun b => b ^^^ 54) ++ (List.range m_len).map mFn := by
    rw [copyFn_map_append, ipad_prefix, hkp54]
  have houter : (List.range 96).map (copyFn (copyFn zfn (ipadFn kpFn 92) 0 64) (fun j => (Sha256Spec.hash ((List.range (64 + m_len)).map (copyFn (copyFn zfn (ipadFn kpFn 54) 0 64) mFn 64 m_len))).getD j 0) 64 32)
      = (Sha256Spec.keyPrep KEY).map (fun b => b ^^^ 92)
        ++ Sha256Spec.hash ((List.range (64 + m_len)).map (copyFn (copyFn zfn (ipadFn kpFn 54) 0 64) mFn 64 m_len)) := by
    rw [show (96:Nat) = 64 + 32 by omega, copyFn_map_append, ipad_prefix, hkp92, hihlist]
  rw [houter, hinner]
  rfl

-- ==================================================================
-- hmac_sha256 refines its spec (task #21, HMAC bar #2 CLOSED):
-- the full hmac_sha256(k, k_len, m, m_len) — key-prep (if k_len>64: hash
-- the key; else copy), ipad/opad, message, and the three sha256_hash calls
-- — computes exactly Sha256Spec.hmac key message, for k_len ≤ 128 and
-- m_len ≤ 256. Composes hmac_linear (post-key-prep body) with the
-- if-branch (kpCopyElse_eval / thenE_eval) via kp_else / kp_if.
-- ==================================================================

-- `elseBranch` (the `k_len ≤ 64` branch, with the HMAC continuation inlined
-- as the loop's `cont`) is defined in `Concrete.Proof`. Evaluating it copies
-- the key into `kp` (copy_loop, cont-generic) then runs the continuation
-- (`hmac_linear`), yielding `Sha256Spec.hmac` directly.
set_option maxRecDepth 8000 in
theorem elseBranch_eval (kFn mFn : Nat → BitVec 8) (k_len m_len : Nat)
    (hkl : k_len ≤ 64) (hml : m_len ≤ 256) (e : Env) (base : Nat) (hb : 201 + m_len ≤ base)
    (hkp0 : e "kp" = some (.array_ (arrN 64 zfn))) (hkv : e "k" = some (.array_ (arrN 128 kFn)))
    (hklv : e "k_len" = some (.int (k_len : Int)))
    (hmv : e "m" = some (.array_ (arrN 256 mFn)))
    (hmlen : e "m_len" = some (.int (m_len : Int))) :
    eval shaFns e ((base + 2) + k_len + 1 + 1) elseBranch
      = some (.array_ ((Sha256Spec.hmac ((List.range k_len).map kFn) ((List.range m_len).map mFn)).map
          (fun b => PVal.int b.toNat))) := by
  rw [elseBranch, show (base + 2) + k_len + 1 + 1 = ((base + 2) + k_len + 1) + 1 by omega,
      eval_letIn _ _ ((base + 2) + k_len + 1) _ _ _ (.int 0) (by simp [eval])]
  rw [← copyEnv_self "kp" "k" "i" (e.bind "i" (.int 0)) 64 128 0 zfn kFn
        (by simp only [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; exact hkp0)
        (by simp only [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; exact hkv)
        (by simp only [Env.bind, beq_self_eq_true, if_true])]
  rw [copy_loop "kp" "k" "i" (by decide) (by decide) (by decide) (e.bind "i" (.int 0)) 64 128 0 zfn kFn k_len
        (by omega) (by omega) (.binOp .lt (.var "i") (.var "k_len")) (.var "i") hmacLinearExpr base
        (fun mm hmm => by simp only [eval, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false, Option.some.injEq, PVal.int.injEq]; omega)
        (fun mm hmm => by simp only [eval, evalBinOp, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false, hklv]; simp; omega)
        (by simp only [eval, evalBinOp, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false, hklv]; simp)]
  rw [show base + 2 = (base - 201 - m_len) + 203 + m_len by omega]
  exact hmac_linear (copyFn zfn kFn 0 k_len) mFn ((List.range k_len).map kFn) m_len hml
    (kp_else kFn k_len (by omega)) _ (base - 201 - m_len)
    (by simp only [copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false])
    (by simp only [copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; exact hmv)
    (by simp only [copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; exact hmlen)

theorem key_copyFn (kFn : Nat → BitVec 8) (k_len : Nat) :
    (List.range k_len).map (copyFn zfn kFn 0 k_len) = (List.range k_len).map kFn := by
  apply List.map_congr_left; intro j hj
  simp only [List.mem_range] at hj
  simp only [copyFn, if_pos (show 0 ≤ j ∧ j < 0 + k_len by omega), Nat.sub_zero]

-- `thenBranch` (the `k_len > 64` branch, with the HMAC continuation inlined
-- as the inner loop's `cont`) is defined in `Concrete.Proof`.
theorem arrayLit_z384 (fns : FnTable) (env : Env) (fuel : Nat) :
    eval fns env (fuel + 2) (.arrayLit (List.replicate 384 (.lit (.int 0))))
      = some (.array_ (List.replicate 384 (PVal.int 0))) :=
  arrayLit_replicate_eval fns env fuel 384 (.int 0)

set_option maxRecDepth 8000 in
set_option maxHeartbeats 2000000 in
theorem thenBranch_eval (kFn mFn : Nat → BitVec 8) (k_len m_len : Nat)
    (hkl : k_len ≤ 128) (hk : 64 < k_len) (hml : m_len ≤ 256) (e : Env) (base : Nat) (hb : 96 + m_len ≤ base)
    (hkp0 : e "kp" = some (.array_ (arrN 64 zfn))) (hkv : e "k" = some (.array_ (arrN 128 kFn)))
    (hklv : e "k_len" = some (.int (k_len : Int)))
    (hmv : e "m" = some (.array_ (arrN 256 mFn)))
    (hmlen : e "m_len" = some (.int (m_len : Int))) :
    eval shaFns e (base + 145 + k_len) thenBranch
      = some (.array_ ((Sha256Spec.hmac ((List.range k_len).map kFn) ((List.range m_len).map mFn)).map
          (fun b => PVal.int b.toNat))) := by
  rw [thenBranch]
  -- letIn kbuf
  rw [show base + 145 + k_len = ((base + 143 + k_len) + 1) + 1 by omega,
      eval_letIn _ _ ((base + 143 + k_len) + 1) _ _ _ _ (arrayLit_z384 _ _ (base + 143 + k_len)),
      show (base + 143 + k_len) + 1 = base + 144 + k_len by omega]
  -- letIn i
  rw [show base + 144 + k_len = (base + 143 + k_len) + 1 by omega,
      eval_letIn _ _ (base + 143 + k_len) _ _ _ (.int 0) (by simp [eval])]
  rw [← copyEnv_self "kbuf" "k" "i" ((e.bind "kbuf" (.array_ (List.replicate 384 (PVal.int 0)))).bind "i" (.int 0)) 384 128 0 zfn kFn
        (by simp only [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; rw [← arrN_zfn 384])
        (by simp only [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; exact hkv) (by simp only [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false])]
  rw [show base + 143 + k_len = (base + 140 + 2) + k_len + 1 by omega,
      copy_loop "kbuf" "k" "i" (by decide) (by decide) (by decide) ((e.bind "kbuf" (.array_ (List.replicate 384 (PVal.int 0)))).bind "i" (.int 0)) 384 128 0 zfn kFn k_len
        (by omega) (by omega) (.binOp .lt (.var "i") (.var "k_len")) (.var "i") _ (base + 140)
        (fun mm hmm => by simp only [eval, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false, Option.some.injEq, PVal.int.injEq]; omega)
        (fun mm hmm => by simp only [eval, evalBinOp, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false, hklv]; simp; omega)
        (by simp only [eval, evalBinOp, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false, hklv]; simp)]
  -- kh letIn (hash of kbuf)
  rw [show base + 142 = (base + 141) + 1 by omega,
      eval_letIn _ _ (base + 141) _ _ _ _
        (sha256_hash_call_at (copyFn zfn kFn 0 k_len) k_len (by omega)
          (fun i hi => by simp only [copyFn, zfn]; rw [if_neg (by omega)])
          (copyEnv "kbuf" "k" "i" ((e.bind "kbuf" (.array_ (List.replicate 384 (PVal.int 0)))).bind "i" (.int 0)) 384 128 0 zfn kFn k_len) (.var "kbuf") (.var "k_len") (base + 141) (by omega)
          (by simp only [eval, copyEnv, Env.bind, bufArr_eq, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false])
          (by simp only [eval, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false, hklv]))]
  rw [key_copyFn, map_toNat_eq_arrN, hash_length]
  -- reset i + kp copy from kh
  rw [show base + 141 = (base + 140) + 1 by omega,
      eval_letIn _ _ (base + 140) _ _ _ (.int 0) (by simp [eval])]
  rw [← copyEnv_self "kp" "kh" "i" (((copyEnv "kbuf" "k" "i" ((e.bind "kbuf" (.array_ (List.replicate 384 (PVal.int 0)))).bind "i" (.int 0)) 384 128 0 zfn kFn k_len).bind "kh" (.array_ (arrN 32 (fun j => (Sha256Spec.hash ((List.range k_len).map kFn)).getD j 0)))).bind "i" (.int 0)) 64 32 0 zfn (fun j => (Sha256Spec.hash ((List.range k_len).map kFn)).getD j 0)
        (by simp only [Env.bind, copyEnv, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; exact hkp0)
        (by simp only [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]) (by simp only [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false])]
  rw [show base + 140 = (base + 105 + 2) + 32 + 1 by omega,
      copy_loop "kp" "kh" "i" (by decide) (by decide) (by decide) (((copyEnv "kbuf" "k" "i" ((e.bind "kbuf" (.array_ (List.replicate 384 (PVal.int 0)))).bind "i" (.int 0)) 384 128 0 zfn kFn k_len).bind "kh" (.array_ (arrN 32 (fun j => (Sha256Spec.hash ((List.range k_len).map kFn)).getD j 0)))).bind "i" (.int 0)) 64 32 0 zfn (fun j => (Sha256Spec.hash ((List.range k_len).map kFn)).getD j 0) 32
        (by omega) (by omega) (.binOp .lt (.var "i") (.lit (.int 32))) (.var "i") hmacLinearExpr (base + 105)
        (fun mm hmm => by simp only [eval, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false, Option.some.injEq, PVal.int.injEq]; omega)
        (fun mm hmm => by simp only [eval, evalBinOp, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; simp; omega)
        (by simp only [eval, evalBinOp, copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; simp)]
  -- post key-prep: kp = hash(key); run the HMAC continuation via hmac_linear
  have hkp : (List.range 64).map (copyFn zfn (fun j => (Sha256Spec.hash ((List.range k_len).map kFn)).getD j 0) 0 32)
      = Sha256Spec.keyPrep ((List.range k_len).map kFn) := by
    apply kp_if _ _ (by simp; omega)
    rw [show (32:Nat) = (Sha256Spec.hash ((List.range k_len).map kFn)).length from (hash_length _).symm]
    exact list_eq_rangeGetD _
  rw [show base + 105 + 2 = (base - 96 - m_len) + 203 + m_len by omega]
  exact hmac_linear (copyFn zfn (fun j => (Sha256Spec.hash ((List.range k_len).map kFn)).getD j 0) 0 32)
    mFn ((List.range k_len).map kFn) m_len hml hkp _ (base - 96 - m_len)
    (by simp only [copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false])
    (by simp only [copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; exact hmv)
    (by simp only [copyEnv, Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; exact hmlen)

theorem arrayLit_z64 (fns : FnTable) (env : Env) (fuel : Nat) :
    eval fns env (fuel + 2) (.arrayLit (List.replicate 64 (.lit (.int 0))))
      = some (.array_ (List.replicate 64 (PVal.int 0))) :=
  arrayLit_replicate_eval fns env fuel 64 (.int 0)

-- `hmac_sha256Expr` is defined in `Concrete.Proof` in EXACT extracted shape:
-- `letIn kp zeros (ifThenElse (gt k_len 64) thenBranch elseBranch)` — the `if`
-- duplicates the HMAC continuation into both branches, as the source extracts.

-- `eval_ite_true`, `eval_ite_false` relocated to `Concrete.ProofKit.Eval`.

set_option maxRecDepth 8000 in
set_option maxHeartbeats 4000000 in
theorem hmac_sha256_refines_spec (kFn mFn : Nat → BitVec 8) (k_len m_len : Nat)
    (hkl : k_len ≤ 128) (hml : m_len ≤ 256) (e : Env) (fuel : Nat)
    (hkv : e "k" = some (.array_ (arrN 128 kFn)))
    (hklv : e "k_len" = some (.int (k_len : Int)))
    (hmv : e "m" = some (.array_ (arrN 256 mFn)))
    (hmlen : e "m_len" = some (.int (m_len : Int))) :
    eval shaFns e (fuel + 350 + k_len + m_len) hmac_sha256Expr
      = some (.array_ ((Sha256Spec.hmac ((List.range k_len).map kFn) ((List.range m_len).map mFn)).map
          (fun b => PVal.int b.toNat))) := by
  rw [hmac_sha256Expr]
  rw [show fuel + 350 + k_len + m_len = ((fuel + 348 + k_len + m_len) + 1) + 1 by omega,
      eval_letIn _ _ ((fuel + 348 + k_len + m_len) + 1) _ _ _ _ (arrayLit_z64 _ _ (fuel + 348 + k_len + m_len)),
      show (fuel + 348 + k_len + m_len) + 1 = fuel + 349 + k_len + m_len by omega]
  have hkp0 : (e.bind "kp" (.array_ (List.replicate 64 (PVal.int 0)))) "kp" = some (.array_ (arrN 64 zfn)) := by
    rw [← arrN_zfn 64]; simp [Env.bind]
  have hkv' : (e.bind "kp" (.array_ (List.replicate 64 (PVal.int 0)))) "k" = some (.array_ (arrN 128 kFn)) := by
    simp only [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; exact hkv
  have hklv' : (e.bind "kp" (.array_ (List.replicate 64 (PVal.int 0)))) "k_len" = some (.int (k_len:Int)) := by
    simp only [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; exact hklv
  have hmv' : (e.bind "kp" (.array_ (List.replicate 64 (PVal.int 0)))) "m" = some (.array_ (arrN 256 mFn)) := by
    simp only [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; exact hmv
  have hmlen' : (e.bind "kp" (.array_ (List.replicate 64 (PVal.int 0)))) "m_len" = some (.int (m_len:Int)) := by
    simp only [Env.bind, beq_self_eq_true, if_true, beq_iff_eq, String.reduceEq, reduceCtorEq, if_false]; exact hmlen
  -- The extracted `if (k_len > 64)` duplicates the continuation into both
  -- branches; each branch is closed by thenBranch_eval / elseBranch_eval.
  rw [show fuel + 349 + k_len + m_len = (fuel + 348 + k_len + m_len) + 1 by omega]
  by_cases hk : 64 < k_len
  · -- long key: hash it, then continuation
    refine eval_ite_true shaFns _ (fuel + 348 + k_len + m_len)
      (.binOp .gt (.var "k_len") (.lit (.int 64))) thenBranch elseBranch _ ?_ ?_
    · simp only [eval, evalBinOp, hklv']; simp; omega
    · rw [show fuel + 348 + k_len + m_len = (fuel + 203 + m_len) + 145 + k_len by omega]
      exact thenBranch_eval kFn mFn k_len m_len hkl hk hml _ (fuel + 203 + m_len) (by omega) hkp0 hkv' hklv' hmv' hmlen'
  · -- short key: copy, then continuation
    refine eval_ite_false shaFns _ (fuel + 348 + k_len + m_len)
      (.binOp .gt (.var "k_len") (.lit (.int 64))) thenBranch elseBranch _ ?_ ?_
    · simp only [eval, evalBinOp, hklv']; simp; omega
    · rw [show fuel + 348 + k_len + m_len = (fuel + 344 + m_len) + 2 + k_len + 1 + 1 by omega]
      exact elseBranch_eval kFn mFn k_len m_len (by omega) hml _ (fuel + 344 + m_len) (by omega) hkp0 hkv' hklv' hmv' hmlen'

end Concrete.Proof
