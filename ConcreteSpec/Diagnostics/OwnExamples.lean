import ConcreteSpec.Spec.Own
import ConcreteSpec.Diagnostics.Types

/-!
# Ownership fixtures with presentation names

Use the one diagnostics table from Diagnostics.Types. The Own definitions receive only
numeric variables; presentation spellings never enter the ownership state or Exec driver.
-/
namespace ConcreteSpec.Diagnostics.OwnExamples
open ConcreteSpec.Core ConcreteSpec.Core.Own ConcreteSpec.Core.Own.Examples

/-- Two populated spelling tables display the same numeric local differently. -/
def spellingsBefore : LocalNames := ⟨[(x, "source_x"), (y, "source_y")]⟩
def spellingsAfter : LocalNames := ⟨[(x, "renamed_x"), (y, "source_x")]⟩

/-- Instance: changed presentation cannot change the owner or value read at local 0. -/
example : AList.lookup spellingsBefore.entries x ≠ AList.lookup spellingsAfter.entries renamedX ∧
    (initial.frames.head?.bind fun f => AList.lookup f.locals renamedX) = some ⟨0⟩ ∧
    eval [(x, 7), (y, 8)] initial (.read renamedX) = .ok (7, initial) := by decide

/-- Instance: equal presentation text for different numbers cannot merge owners, reads or scope
removal. Exec's restriction and Own's lookup both use Basics numeric identity. -/
example : AList.lookup spellingsBefore.entries x = AList.lookup spellingsAfter.entries y ∧ x ≠ y ∧
    eval [(x, 7), (y, 8)] initial (.read y) = .stuck ∧
    Exec.restrict [y] [(renamedX, (7 : Int)), (y, 8)] = [(y, 8)] := by decide

/-- Both numeric locals coexist despite colliding presentation text. -/
def populated : State := { initial with
  nextPlace := 2
  slots := ⟨⟨1⟩, 0, ⟨.int .i32, false, true⟩, .owned⟩ :: initial.slots
  frames := [⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩] }
/-- Resolve the numeric variable before borrowing; spelling tables are diagnostics only. -/
def borrowVar (s : State) (v : Var) : Option State := do
  let f ← s.frames.head?
  let p ← AList.lookup f.locals v
  borrow s p [] .shared
/-- Both linear locals have been consumed before scope teardown. -/
def movedBoth : State := (do
  let s ← move populated ⟨0⟩
  move s ⟨1⟩).getD initial
/-- One parameter is already pending; equal numeric identity is refused, distinct identity accepted. -/
def pending : CallState := (passOwner ⟨populated, []⟩ ⟨0⟩ [] .shared x).getD ⟨initial, []⟩
/-- Instance: numeric lookup and move evaluation select each colliding local independently;
parameter admission, drop and exit also retain numeric identity with both locals present. -/
example : AList.lookup spellingsBefore.entries x = AList.lookup spellingsAfter.entries y ∧ x ≠ y ∧
    (populated.frames.head?.bind fun f => AList.lookup f.locals renamedX) = some ⟨0⟩ ∧
    (populated.frames.head?.bind fun f => AList.lookup f.locals y) = some ⟨1⟩ ∧
    (eval [(x, 7), (y, 8)] populated (.move renamedX)) =
      .ok (7, (move populated ⟨0⟩).getD initial) ∧
    (eval [(x, 7), (y, 8)] populated (.move y)) =
      .ok (8, (move populated ⟨1⟩).getD initial) ∧
    ((borrowVar populated renamedX).getD initial).loans.head?.map Loan.root = some ⟨0⟩ ∧
    ((borrowVar populated y).getD initial).loans.head?.map Loan.root = some ⟨1⟩ ∧
    passOwner pending ⟨0⟩ [] .shared renamedX = none ∧
    (passOwner pending ⟨0⟩ [] .shared y).isSome = true ∧
    ((drop [renamedX] movedBoth).getD initial).slots.map Slot.place = [⟨1⟩] ∧
    ((drop [y] movedBoth).getD initial).slots.map Slot.place = [⟨0⟩] ∧
    (leave movedBoth).isSome = true ∧
    ((leave movedBoth).getD initial).slots = [] ∧
    ((leave movedBoth).getD initial).frames = [] := by decide

end ConcreteSpec.Diagnostics.OwnExamples
