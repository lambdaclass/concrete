namespace ConcreteSpec.Repro

inductive Result (α : Type) where
  | ok : α → Result α
  | trap : Result α

def S {α : Type} (x : α) : Result (α × Nat) := .ok (x, 1)
def mutant {α : Type} (x : α) : Result (α × Nat) := .ok (x, 0)

/-- Law (subject `ConcreteSpec.Repro.S`). -/
theorem growth {α : Type} (x : α) : (match S x with
    | .ok (y, n) => y = x ∧ 0 < n
    | _ => True) := ⟨rfl, Nat.zero_lt_succ 0⟩

/-- Instance (law `ConcreteSpec.Repro.growth`, case `two`). -/
example : (match S 2 with
    | .ok (y, n) => y = 2 ∧ 0 < n
    | _ => True) := growth 2

/-- Planted (law `ConcreteSpec.Repro.growth`, case `two`, subject `ConcreteSpec.Repro.S`, mutant `ConcreteSpec.Repro.mutant`). -/
example : ¬ (match mutant 2 with
    | .ok (y, n) => y = 2 ∧ 0 < n
    | _ => True) := by
  change ¬ (2 = 2 ∧ 0 < 0)
  decide

end ConcreteSpec.Repro
