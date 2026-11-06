import Concrete.Proof.Proof
import Concrete.ProofKit.Array

/-!
# Concrete Proof Kit — Refinement bridges

The `List` ↔ spec-function bridges that connect a concrete result list to the
`fun j => L.getD j 0` view the refinement specs are stated over, and to the
`arrN` buffer model. Reusable by any refinement proof that compares an
evaluator array against a `BitVec`-list spec. Extracted from
`Concrete.Sha256Refine` (Proof Kit v1).
-/

namespace Concrete.Proof

/-- `getD` at an in-bounds index is the element. -/
theorem getD_eq_getElem_mem (l : List (BitVec 8)) (k : Nat) (h : k < l.length) :
    l.getD k 0 = l[k] := by
  rw [List.getD_eq_getElem?_getD, List.getElem?_eq_getElem h]; rfl

/-- A list equals the range-map of its own `getD` view. -/
theorem list_eq_rangeGetD (L : List (BitVec 8)) :
    (List.range L.length).map (fun j => L.getD j 0) = L := by
  apply List.ext_getElem (by simp)
  intro j h1 _
  simp only [List.getElem_map, List.getElem_range]
  exact getD_eq_getElem_mem L j (by simp only [List.length_map, List.length_range] at h1; exact h1)

/-- The evaluator view of a byte list equals `arrN` of its `getD` view —
    the bridge from a concrete result `List (BitVec 8)` to the `arrN` buffer
    model the loop lemmas produce. -/
theorem map_toNat_eq_arrN (L : List (BitVec 8)) :
    L.map (fun b => PVal.int ↑b.toNat) = arrN L.length (fun j => L.getD j 0) := by
  apply List.ext_getElem (by simp [arrN])
  intro j h1 _
  simp only [List.length_map] at h1
  simp only [arrN, List.getElem_map, List.getElem_range]
  rw [List.getD_eq_getElem?_getD, List.getElem?_eq_getElem h1, Option.getD_some]

end Concrete.Proof
