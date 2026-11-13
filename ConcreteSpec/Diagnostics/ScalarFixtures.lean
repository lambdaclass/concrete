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
  ⟨original.code, ⟨[(⟨7⟩, "total"), (⟨8⟩, "remaining")]⟩⟩

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

/-- Assignment's observable contract includes both the target and the untouched local. -/
def assigned (stored : AList Var Int) : Prop :=
  (CTerm.var ⟨8⟩).eval (AList.lookup stored) = some 42 ∧
  (CTerm.var ⟨7⟩).eval (AList.lookup stored) = some 12

/-- Scope exit removes the target while retaining the other numeric identity. -/
def removed (stored : AList Var Int) : Prop :=
  (CTerm.var ⟨8⟩).eval (AList.lookup stored) = none ∧
  (CTerm.var ⟨7⟩).eval (AList.lookup stored) = some 12

/-- Fault: assignment chooses the first local with the target's spelling rather than its number.
It still uses the real shared update, isolating the erroneous choice of identity. -/
def spellingUpdate (names : LocalNames) (stored : AList Var Int) (x : Var)
    (f : Int → Int) : AList Var Int :=
  match stored.find? (fun entry =>
      AList.lookup names.entries entry.1 == AList.lookup names.entries x) with
  | none => stored
  | some entry => AList.update stored entry.1 f

/-- Fault: scope exit erases every local with the target's spelling. -/
def spellingErase (names : LocalNames) (stored : AList Var Int) (x : Var) : AList Var Int :=
  stored.filter (fun entry =>
    AList.lookup names.entries entry.1 != AList.lookup names.entries x)

/-- Paired assignment under the original, renamed and colliding presentations. The semantic
update consumes the numeric code of the presented variable in every case. -/
example : ∀ names ∈ [original.names, renamed.names, sameSpelling],
    assigned (AList.update values (Elaborated.mk (Var.mk 8) names).code (fun _ => 42)) := by
  intro names _
  change assigned (AList.update values ⟨8⟩ (fun _ => 42))
  unfold assigned
  decide

/-- The identical assignment contract accepts the spelling-based control with unique names,
but rejects it when distinct numbers share a spelling: it updates 7 instead of 8. -/
example : assigned (spellingUpdate original.names values ⟨8⟩ (fun _ => 42)) ∧
    assigned (spellingUpdate renamed.names values ⟨8⟩ (fun _ => 42)) ∧
    ¬ assigned (spellingUpdate sameSpelling values ⟨8⟩ (fun _ => 42)) := by
  unfold assigned
  decide

/-- Paired scope removal under all three presentations preserves the other local. -/
example : ∀ names ∈ [original.names, renamed.names, sameSpelling],
    removed (AList.erase values (Elaborated.mk (Var.mk 8) names).code) := by
  intro names _
  change removed (AList.erase values ⟨8⟩)
  unfold removed
  decide

/-- The identical scope-exit contract rejects spelling-based erasure: the colliding presentation
would erase both numbers. Unique-name controls show this is specifically an identity fault. -/
example : removed (spellingErase original.names values ⟨8⟩) ∧
    removed (spellingErase renamed.names values ⟨8⟩) ∧
    ¬ removed (spellingErase sameSpelling values ⟨8⟩) := by
  unfold removed
  decide

/-- Exact-term conversion is the other Scalar operation on syntax containing locals. Its result
also survives renaming; the stale spelling valuation is rejected after conversion as before it. -/
example : original.code.toExact.eval (AList.lookup values) = some 13 ∧
    renamed.code.toExact.eval (AList.lookup values) = some 13 ∧
    original.code.toExact.eval
      (byPresentation original.names [("count", 12), ("other", 19)]) = some 13 ∧
    renamed.code.toExact.eval
      (byPresentation renamed.names [("count", 12), ("other", 19)]) ≠ some 13 := by
  decide

/-! Coverage boundary: `CTerm.eval` (including reads and arithmetic) and `CTerm.toExact` are
Scalar's only operations consuming syntax with locals. Assignment and scope exit above exercise
the shared `AList.update` and `AList.erase` used by scalar valuations. All other Scalar operations
consume scalar values/types, not locals or presentations. F02's dropping and ownership cases
belong to the later lifecycle/ownership arrivals; Scalar defines neither operation. -/

end ConcreteSpec.Diagnostics.ScalarFixtures
