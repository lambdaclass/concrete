import Std.Data.HashMap.Lemmas

namespace Concrete.Index

/-- A numeric lookup index. Inserting the list from right to left preserves first-match lookup. -/
def first {α : Type} (key : α → Nat) (xs : List α) : Std.HashMap Nat α :=
  xs.foldr (fun x index => index.insert (key x) x) {}

theorem first_get {α : Type} (key : α → Nat) (xs : List α) (k : Nat) :
    (first key xs)[k]? = xs.find? (fun x => key x == k) := by
  induction xs with
  | nil => simp [first]
  | cons x xs ih =>
    change ((first key xs).insert (key x) x)[k]? = _
    by_cases h : key x = k <;>
      simp [Std.HashMap.getElem?_insert, ih, List.find?, h,
        beq_eq_false_iff_ne.mpr]

theorem first_contains {α : Type} (key : α → Nat) (xs : List α) (k : Nat) :
    (first key xs).contains k = (xs.map key).contains k := by
  rw [Std.HashMap.contains_eq_isSome_getElem?, first_get]
  induction xs with
  | nil => rfl
  | cons x xs ih =>
    by_cases h : key x = k
    · simp [List.find?, h]
    · rw [List.find?, beq_eq_false_iff_ne.mpr h, ih, List.map_cons]
      simp [Ne.symm h]

end Concrete.Index
