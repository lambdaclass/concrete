namespace ConcreteSpec.Repro

def find : List Nat → Nat → Bool
  | [], _ => false
  | y :: ys, x => y == x || find ys x

def findBad (_l : List Nat) (_x : Nat) : Bool := true

/-- Law (subject `ConcreteSpec.Repro.find`). An absent element is not found. -/
theorem find_absent (l : List Nat) (x : Nat) (hx : x ∉ l) : find l x = false := by
  induction l with
  | nil => rfl
  | cons y ys ih =>
    have h1 : y ≠ x := fun h => hx (h ▸ List.Mem.head ys)
    have h2 : x ∉ ys := fun h => hx (List.Mem.tail y h)
    simp [find, h1, ih h2]

/-- Instance (law `ConcreteSpec.Repro.find_absent`, case `decided_notin`). -/
example : find [1, 2] 3 = false := find_absent [1, 2] 3 (by decide)

/-- Planted (law `ConcreteSpec.Repro.find_absent`, case `decided_notin`, subject `ConcreteSpec.Repro.find`, mutant `ConcreteSpec.Repro.findBad`). -/
example : (3 ∉ [1, 2]) ∧ ¬ (findBad [1, 2] 3 = false) := by decide

def sumL : List Nat → Nat
  | [] => 0
  | y :: ys => y + sumL ys

def sumBad (_l : List Nat) : Nat := 1

/-- Law (subject `ConcreteSpec.Repro.sumL`). A list of zeros sums to zero. -/
theorem sum_zero (l : List Nat) (h : ∀ y, y ∈ l → y = 0) : sumL l = 0 := by
  induction l with
  | nil => rfl
  | cons y ys ih =>
    show y + sumL ys = 0
    rw [h y (List.Mem.head ys), ih (fun z hz => h z (List.Mem.tail y hz))]

/-- Helper (inversion). Every element of the singleton list is zero (approved: repro:r1). -/
theorem single_zero : ∀ y, y ∈ [0] → y = 0 := by
  intro y hy
  cases hy with
  | head => rfl
  | tail _ hy => cases hy

/-- Instance (law `ConcreteSpec.Repro.sum_zero`, case `helper_cases`). -/
example : sumL [0] = 0 := sum_zero [0] single_zero

/-- Planted (law `ConcreteSpec.Repro.sum_zero`, case `helper_cases`, subject `ConcreteSpec.Repro.sumL`, mutant `ConcreteSpec.Repro.sumBad`). -/
example : (∀ y, y ∈ [0] → y = 0) ∧ ¬ (sumBad [0] = 0) := ⟨single_zero, by decide⟩

/-- Instance (law `ConcreteSpec.Repro.sum_zero`, case `decided_ball`). -/
example : sumL [0, 0] = 0 := sum_zero [0, 0] (by decide)

/-- Planted (law `ConcreteSpec.Repro.sum_zero`, case `decided_ball`, subject `ConcreteSpec.Repro.sumL`, mutant `ConcreteSpec.Repro.sumBad`). -/
example : (∀ y, y ∈ [0, 0] → y = 0) ∧ ¬ (sumBad [0, 0] = 0) := ⟨by decide, by decide⟩

/-- Instance (law `ConcreteSpec.Repro.sum_zero`, case `rfl_only`). -/
example : sumL [] = 0 := sum_zero [] (fun _ h => nomatch h)

/-- Planted (law `ConcreteSpec.Repro.sum_zero`, case `rfl_only`, subject `ConcreteSpec.Repro.sumL`, mutant `ConcreteSpec.Repro.sumBad`). -/
example : (∀ y, y ∈ ([] : List Nat) → y = 0) ∧ ¬ (sumBad [] = 0) := ⟨(fun _ h => nomatch h), by decide⟩

def lenL (l : List Nat) : Nat := l.length
def lenBad (_l : List Nat) : Nat := 0

/-- Law (subject `ConcreteSpec.Repro.lenL`). A list with a member is nonempty. -/
theorem len_pos (l : List Nat) (x : Nat) (h : x ∈ l) : 0 < lenL l := by
  cases h with
  | head => exact Nat.succ_pos _
  | tail _ _ => exact Nat.succ_pos _

/-- Instance (law `ConcreteSpec.Repro.len_pos`, case `simp_member`). -/
example : 0 < lenL [1, 2] := len_pos [1, 2] 2 (by simp)

/-- Planted (law `ConcreteSpec.Repro.len_pos`, case `simp_member`, subject `ConcreteSpec.Repro.lenL`, mutant `ConcreteSpec.Repro.lenBad`). -/
example : (2 ∈ [1, 2]) ∧ ¬ (0 < lenBad [1, 2]) := by decide

/-- Helper (computation). The last element is a member of a two-element list (approved: repro:r1). -/
theorem last_mem (a b : Nat) : b ∈ [a, b] := by simp

/-- Instance (law `ConcreteSpec.Repro.len_pos`, case `closed_helper_simp`). -/
example : 0 < lenL [3, 4] := len_pos [3, 4] 4 (last_mem 3 4)

/-- Planted (law `ConcreteSpec.Repro.len_pos`, case `closed_helper_simp`, subject `ConcreteSpec.Repro.lenL`, mutant `ConcreteSpec.Repro.lenBad`). -/
example : (4 ∈ [3, 4]) ∧ ¬ (0 < lenBad [3, 4]) := by decide

/-- Instance (law `ConcreteSpec.Repro.len_pos`, case `constructor_member`). -/
example : 0 < lenL [5, 6] := len_pos [5, 6] 6 (.tail 5 (.head []))

/-- Planted (law `ConcreteSpec.Repro.len_pos`, case `constructor_member`, subject `ConcreteSpec.Repro.lenL`, mutant `ConcreteSpec.Repro.lenBad`). -/
example : (6 ∈ [5, 6]) ∧ ¬ (0 < lenBad [5, 6]) := by decide

/-- Instance (law `ConcreteSpec.Repro.find_absent`, case `decided_eq_shape`). -/
example : find [] 3 = false := find_absent [] 3 (fun h => nomatch h)

/-- Planted (law `ConcreteSpec.Repro.find_absent`, case `decided_eq_shape`, subject `ConcreteSpec.Repro.find`, mutant `ConcreteSpec.Repro.findBad`). -/
example : (3 ∉ ([] : List Nat)) ∧ ¬ (findBad [] 3 = false) := ⟨(fun h => nomatch h), by decide⟩

end ConcreteSpec.Repro
