import ConcreteSpec.Spec.Basics
import ConcreteSpec.Proofs.Basics

namespace ConcreteSpec.Core

/-- Instance: refusal imposes no postcondition, even an impossible one. -/
theorem example_refusal_imposes_no_postcondition_even_impossible : ResultSatisfies (fun _ : Nat => False) none := trivial

/-- Instance: a successful result must satisfy its actual postcondition. -/
theorem example_successful_result_must_satisfy_its_actual : ResultSatisfies (fun n : Nat => n = 7) (some 7) ∧
    ¬ ResultSatisfies (fun n : Nat => n = 7) (some 8) := by decide

/-- Planted: requiring success wrongly rejects refusal. -/
private def resultRequiresSuccess {α : Type} (P : α → Prop) (r : Option α) : Prop :=
  ∃ a, r = some a ∧ P a

/-- Planted: the refusal case distinguishes the generic policy from mandatory success. -/
theorem planted_refusal_case_distinguishes_generic_policy_from : ¬ (resultRequiresSuccess (fun n : Nat => n = 7) none ↔
    ∀ a, (none : Option Nat) = some a → a = 7) := by
  simp [resultRequiresSuccess]

/-- Planted: ignoring a successful value admits a failed postcondition. -/
private def resultIgnoresValue {α : Type} (_ : α → Prop) (_ : Option α) : Prop := True

/-- Planted: a wrong successful value distinguishes the policy from ignoring success. -/
theorem planted_wrong_successful_value_distinguishes_policy_from : ¬ (resultIgnoresValue (fun n : Nat => n = 7) (some 8) ↔
    ∀ a, (some 8 : Option Nat) = some a → a = 7) := by
  simp [resultIgnoresValue]

/-- Instance: the equivalence holds in both directions at a populated success and a refusal. -/
theorem example_equivalence_holds_in_both_directions_at : (ResultSatisfies (fun n : Nat => n = 7) (some 7) ↔
    ∀ a, (some 7 : Option Nat) = some a → a = 7) ∧
    (ResultSatisfies (fun _ : Type => False) none ↔
    ∀ a, (none : Option Type) = some a → False) :=
  ⟨resultSatisfies_iff _ _, resultSatisfies_iff _ _⟩

end ConcreteSpec.Core
