import ConcreteSpec.Admission.Early

namespace ConcreteSpec.Admission.Controls

private def return42 : StageInterface.EarlyProgram :=
  ⟨[], .lit 42, ⟨0, 2, 1, 1, ⟨0⟩⟩⟩

private def return256 : StageInterface.EarlyProgram :=
  { return42 with ret := .lit 256 }

theorem return42_admitted : (admitEarly return42).isOk = true := rfl

theorem return256_refused :
    admitEarly return256 = .error (.atPosition 0 (.literalOutOfRange .u8 256)) := rfl

private def addition : Program :=
  ⟨[], [.let_ (.name ⟨0⟩) (.scalar (.int .u8)) (.add (.int .u8 19) (.int .u8 23))],
    .ret (.copy ⟨0⟩)⟩

private def issued : Discharge := ⟨0, .u8, .int .u8 19, .int .u8 23, []⟩

private def batch : Batch addition := ⟨[issued], rfl⟩

theorem issued_admitted : admitProgram addition = .ok batch := rfl

theorem issued_valid : batch.validates [issued] = true := by decide

theorem residual_valid : batch.validates [] = true := rfl

theorem wrong_site_refused : batch.validates [{ issued with position := 1 }] = false := by decide

theorem wrong_operand_refused :
    batch.validates [{ issued with left := issued.right, right := issued.left }] = false := by decide

theorem wrong_context_refused :
    batch.validates [{ issued with context := [(⟨0⟩, ⟨.scalar (.int .u8), true, true, none, []⟩)] }]
      = false := by decide

theorem duplicate_refused : batch.validates [issued, issued] = false := by decide

end ConcreteSpec.Admission.Controls
