import ConcreteSpec.Admission
import ConcreteSpec.StageInterface

namespace ConcreteSpec.Admission

/-- The stage's canonical Slice 0 input uses the same traversal as Slice 1. -/
def admitEarly (program : StageInterface.EarlyProgram) :
    Except Refusal (Batch program.checkedSyntax) :=
  admitProgram program.checkedSyntax

theorem admitEarly_iff (program : StageInterface.EarlyProgram) :
    (∃ batch, admitEarly program = .ok batch) ↔ Checked program.checkedSyntax :=
  admitProgram_iff _

end ConcreteSpec.Admission
