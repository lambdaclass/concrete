import Concrete.Proof.Proof
import Concrete.ProofKit.Array

/-!
# Concrete Proof Kit — Loops

The reusable counter-loop *templates*: the generic copy-into-buffer loop
`for i in 0..N { dst[off+i] = src[i] }` and its `copyFn` spec, generalized over
an arbitrary function table `fns : FnTable` (no SHA `shaFns` assumption).
Extracted and de-specialized from `Concrete.Sha256Refine` (Proof Kit v1.1).

Builds on the loop-induction keystone `eval_while_count` (in `Concrete.Proof`)
and the buffer model in `ProofKit.Array`.
-/

namespace Concrete.Proof

set_option linter.unusedSimpArgs false

/-- The byte function after copying `src[0..m)` into `dst` at offset `off`. -/
def copyFn (dstFn srcFn : Nat → BitVec 8) (off m : Nat) : Nat → BitVec 8 :=
  fun j => if off ≤ j ∧ j < off + m then srcFn (j - off) else dstFn j

theorem copyFn_zero (dstFn srcFn off) : copyFn dstFn srcFn off 0 = dstFn := by
  funext j; simp only [copyFn, Nat.add_zero]; rw [if_neg (by omega)]

/-- One copy step: writing `src[m]` at `dst[off+m]` extends the copied prefix. -/
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

theorem copyFn_step0 (dstFn srcFn : Nat → BitVec 8) (m : Nat) :
    bufUpd (copyFn dstFn srcFn 0 m) m (srcFn m) = copyFn dstFn srcFn 0 (m + 1) := by
  have h := copyFn_step dstFn srcFn 0 m; rwa [Nat.zero_add] at h

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
    followed by zeros — the shape of a padded fixed-size buffer. -/
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

/-- The loop-state env after `m` copy iterations. -/
def copyEnv (dstNm srcNm iNm : String) (e : Env) (dn sn off : Nat)
    (dstFn srcFn : Nat → BitVec 8) (m : Nat) : Env :=
  ((e.bind dstNm (.array_ (arrN dn (copyFn dstFn srcFn off m)))).bind srcNm
    (.array_ (arrN sn srcFn))).bind iNm (.int (m : Int))

/-- Single copy-loop iteration: the assign list `[dst[idx]=src[i]; i++]` carries
    `copyEnv … m` to `copyEnv … (m+1)`. Generic over any `fns : FnTable`. -/
theorem cpy_step (fns : FnTable) (dstNm srcNm iNm : String)
    (hds : dstNm ≠ srcNm) (hdi : dstNm ≠ iNm) (hsi : srcNm ≠ iNm)
    (e : Env) (dn sn off : Nat) (dstFn srcFn : Nat → BitVec 8) (m : Nat)
    (hdsn : off + m < dn) (hsm : m < sn) (idxE : PExpr) (fuel : Nat)
    (hidx : eval fns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m) (fuel + 1) idxE
      = some (.int ((off + m : Nat) : Int))) :
    eval.evalAssigns fns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m) (fuel + 2)
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
  have hsv : eval fns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m) (fuel + 1)
      (.arrayIndex (.var srcNm) (.var iNm)) = some (.int ↑(srcFn m).toNat) := by
    simp only [eval, copyEnv, Env.bind, e_si, e_is, if_false, if_true,
      beq_self_eq_true, beq_iff_eq, reduceCtorEq]
    exact arrN_read sn srcFn (m : Int) m rfl hsm
  have harr : eval fns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m) (fuel + 2)
      (.arraySet (.var dstNm) idxE (.arrayIndex (.var srcNm) (.var iNm)))
      = some (.array_ (arrN dn (copyFn dstFn srcFn off (m + 1)))) := by
    rw [eval_arraySet_lemma fns _ (fuel + 1) (.var dstNm) idxE _
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

/-- **Generic copy-into-buffer loop.** `for i in 0..N { dst[off+i] = src[i] }`
    carries `copyEnv … 0` to `copyEnv … N` (then evaluates `cont`), for any
    `fns : FnTable`. The reusable template behind hmac's key copy, key-hash
    digest copy, message copy, and inner-digest copy. -/
theorem copy_loop (fns : FnTable) (dstNm srcNm iNm : String)
    (hds : dstNm ≠ srcNm) (hdi : dstNm ≠ iNm) (hsi : srcNm ≠ iNm)
    (e : Env) (dn sn off : Nat) (dstFn srcFn : Nat → BitVec 8) (N : Nat)
    (hdn : off + N ≤ dn) (hsn : N ≤ sn) (condE idxE cont : PExpr) (base : Nat)
    (hidx : ∀ m, m < N → eval fns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m) (base + 1) idxE
        = some (.int ((off + m : Nat) : Int)))
    (hct : ∀ m, m < N → eval fns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m) (base + 2) condE
        = some (.bool true))
    (hcf : eval fns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn N) (base + 2) condE
        = some (.bool false)) :
    eval fns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn 0) ((base + 2) + N + 1)
      (.while_ condE
        [ (dstNm, .arraySet (.var dstNm) idxE (.arrayIndex (.var srcNm) (.var iNm)))
        , (iNm, .binOp .add (.var iNm) (.lit (.int 1))) ] cont)
      = eval fns (copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn N) (base + 2) cont :=
  eval_while_count fns condE _ cont (fun m => copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn m)
    N (base + 2)
    (fun m hm => ⟨hct m hm, cpy_step fns dstNm srcNm iNm hds hdi hsi e dn sn off dstFn srcFn m
      (by omega) (by omega) idxE base (hidx m hm)⟩)
    hcf

/-- At iteration 0 the loop-state env collapses back to the starting env. -/
theorem copyEnv_self (dstNm srcNm iNm : String) (e : Env) (dn sn off : Nat)
    (dstFn srcFn : Nat → BitVec 8)
    (hd : e dstNm = some (.array_ (arrN dn dstFn))) (hs : e srcNm = some (.array_ (arrN sn srcFn)))
    (hi : e iNm = some (.int 0)) :
    copyEnv dstNm srcNm iNm e dn sn off dstFn srcFn 0 = e := by
  funext n
  simp only [copyEnv, Env.bind, copyFn_zero]
  by_cases h1 : n = iNm <;> by_cases h2 : n = srcNm <;> by_cases h3 : n = dstNm <;>
    simp_all [beq_iff_eq]

end Concrete.Proof
