import ConcreteSpec.Spec.Basics
import ConcreteSpec.Proofs.Basics

/-!
# Numeric local identity

Semantic operations cannot see spellings by type. The required local_identity environment gate
is the load-bearing check: it rejects diagnostics tables and Var/String pairings in semantic
interfaces, including aliases and structures. These laws constrain numeric identity and lookup;
they do not substitute for that structural check. Future codecs are tracked in ISS-0060.
-/
namespace ConcreteSpec.LocalIdentity
open Core

/-- Numeric identity is complete: there is no hidden spelling component. For example, `⟨7⟩ = ⟨7⟩`. -/
theorem numeric_identity (x y : Var) : x = y ↔ x.id = y.id := by
  cases x; cases y; constructor <;> intro h
  · exact congrArg Var.id h
  · cases h; rfl

/-- The first binding wins; for example, looking up 7 in `[(⟨7⟩, 1), (⟨7⟩, 2)]` returns 1. -/
theorem first_binding {α : Type} (x : Var) (a : α) (rest : AList Var α) :
    AList.lookup ((x, a) :: rest) x = some a := by
  rw [AList.lookup_cons, ite_eq_left rfl]

/-- Different numbers remain different locals; for example, locals 0 and 1 cannot alias.
This is the trivial direction of `numeric_identity`, covered by that law’s negative control. -/
theorem distinct_numbers (x y : Var) (h : x.id ≠ y.id) : x ≠ y := by
  intro heq
  exact h (congrArg Var.id heq)

end ConcreteSpec.LocalIdentity
