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

end ConcreteSpec.Diagnostics.OwnExamples
