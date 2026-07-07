import Concrete.Proof.Proof
import Concrete.ProofKit.BitVec

/-!
# Concrete Proof Kit — Array / buffer model

The size-generic array model (`arrN`), its point-update (`bufUpd`) and the
read/write/frame lemmas that make loop-body array updates tractable. Reusable by
any proof over fixed-size mutable buffers. Extracted from `Concrete.Sha256Refine`
(Proof Kit v1).
-/

namespace Concrete.Proof

set_option linter.unusedSimpArgs false

/-- Point-update of a byte function: `bufUpd bf i v` agrees with `bf` except at `i`. -/
def bufUpd (bf : Nat → BitVec 8) (i : Nat) (v : BitVec 8) : Nat → BitVec 8 :=
  fun j => if j = i then v else bf j

/-- The size-`n` evaluator array view of a byte function. -/
def arrN (n : Nat) (bf : Nat → BitVec 8) : List PVal :=
  (List.range n).map (fun j => PVal.int ↑(bf j).toNat)

theorem arrN_length (n : Nat) (bf : Nat → BitVec 8) : (arrN n bf).length = n := by simp [arrN]

/-- Writing byte `v` at slot `i` of `arrN n bf` is `arrN n (bufUpd bf i v)`. -/
theorem arrN_set (n : Nat) (bf : Nat → BitVec 8) (i : Nat) (v : BitVec 8) :
    (arrN n bf).set i (PVal.int v.toNat) = arrN n (bufUpd bf i v) := by
  apply List.ext_getElem (by simp [arrN])
  intro m h1 _
  simp only [arrN, List.length_map, List.length_range] at h1
  rw [List.getElem_set]
  simp only [arrN, List.getElem_map, List.getElem_range, bufUpd]
  by_cases hmi : m = i
  · simp [hmi]
  · simp [hmi, Ne.symm hmi]

/-- Reading an in-bounds slot of `arrN n bf` (with the evaluator's `< 0` guard). -/
theorem arrN_read (n : Nat) (bf : Nat → BitVec 8) (c : Int) (m : Nat) (hc : c = (m:Int)) (hm : m < n) :
    (if c < 0 then (none : Option PVal) else eval.lookupIndex (arrN n bf) c.toNat)
      = some (.int ↑(bf m).toNat) := by
  subst hc
  rw [if_neg (by omega), show ((m:Int)).toNat = m by omega]
  simp only [arrN]; exact lookupIndex_range_map _ n m hm

/-- The all-zeros byte function. -/
def zfn : Nat → BitVec 8 := fun _ => 0

/-- `arrN n zfn` is the `n`-element zero array. -/
theorem arrN_zfn (n : Nat) : arrN n zfn = List.replicate n (PVal.int 0) := by
  apply List.ext_getElem (by simp [arrN])
  intro j h1 _
  simp only [arrN, List.length_map, List.length_range] at h1
  simp [arrN, List.getElem_replicate, zfn]

/-- **Generic counter-loop array-update frame lemma.** Setting slot `m` of a
    "first-`m`-cells-filled" range-map to its `m`-th value extends it to
    "first-`m+1`-filled" — the single frame fact (cells `i ≠ m` unchanged) reused
    for every iteration of every such loop, proven once. -/
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

/-- Generic single-step `arraySet` evaluation, given the three sub-eval results.
    Keeps the stored value opaque (it enters as `ev`) so its proof is not
    re-derived per call site. -/
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

end Concrete.Proof
