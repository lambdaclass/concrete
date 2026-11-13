import ConcreteSpec.Diagnostics.Types
import ConcreteSpec.Spec.Scalar

/-! Scalar conformance fixtures with presentation data, outside semantic operations. -/
namespace ConcreteSpec.Diagnostics.ScalarFixtures
open Core

/-- Two numeric locals, deliberately carrying different scalar values. -/
def values : AList Var Int := [(⟨7⟩, 12), (⟨8⟩, 19)]

/-- The same numeric program before a diagnostic rename. -/
def original : Elaborated CTerm :=
  ⟨.machine .i8 (.plain .add) (.var ⟨7⟩) (.lit 1),
    ⟨[(⟨7⟩, "count"), (⟨8⟩, "other")]⟩⟩

/-- Only the diagnostic presentation changes. -/
def renamed : Elaborated CTerm :=
  ⟨original.code, ⟨[(⟨7⟩, "total"), (⟨8⟩, "other")]⟩⟩

/-- A plausible faulty implementation: resolve numeric locals through presentation-keyed values. -/
def byPresentation (names : LocalNames) (stored : AList String Int) (x : Var) : Option Int :=
  (AList.lookup names.entries x).bind (AList.lookup stored)

/-- The same scalar computation survives renaming; a stale spelling-keyed environment loses it. -/
example : original.code.eval (AList.lookup values) = some 13 ∧
    renamed.code.eval (AList.lookup values) = some 13 ∧
    original.code.eval (byPresentation original.names [("count", 12), ("other", 19)]) = some 13 ∧
    renamed.code.eval (byPresentation renamed.names [("count", 12), ("other", 19)]) ≠ some 13 := by
  decide

/-- Different numeric locals may have identical presentations (for example in distinct scopes). -/
def sameSpelling : LocalNames := ⟨[(⟨7⟩, "count"), (⟨8⟩, "count")]⟩

/-- Distinct numeric inputs remain distinct under both presentations; collapsing the names
aliases the second input and changes the subtraction from -7 to 0. -/
example : let term := CTerm.machine .i8 (.plain .sub) (.var ⟨7⟩) (.var ⟨8⟩)
    (Elaborated.mk term original.names).code.eval (AList.lookup values) = some (-7) ∧
    (Elaborated.mk term sameSpelling).code.eval (AList.lookup values) = some (-7) ∧
    term.eval (byPresentation original.names [("count", 12), ("other", 19)]) = some (-7) ∧
    term.eval (byPresentation sameSpelling [("count", 12), ("count", 19)]) ≠ some (-7) := by
  decide

end ConcreteSpec.Diagnostics.ScalarFixtures
