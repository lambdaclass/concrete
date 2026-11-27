import ConcreteSpec.Admission
import ConcreteSpec.StageInterface

namespace ConcreteSpec.Admission
open StageInterface

/-- Preserve the represented program in the result type, including the evidence
batch issued by its one checker traversal. -/
def admitView (view : AdmissionView) : Except Refusal (Batch view.program) :=
  admitProgram view.program

theorem admitView_iff (view : AdmissionView) :
    (∃ batch, admitView view = .ok batch) ↔ Checked view.program :=
  admitProgram_iff _

structure InputBatch {language : CoreLanguage} (input : CoreInput language) where
  view : AdmissionView
  represents : view.Represents input
  batch : Batch view.program

theorem InputBatch.accepted {language} {i : CoreInput language} (result : InputBatch i) :
    i.Accepted ∧ i.StaticallyTyped :=
  ⟨⟨_, result.represents, (checkProgram_iff _).mp ⟨_, result.batch.issued⟩⟩,
    ⟨_, result.represents, result.batch.staticallyTyped⟩⟩

/-- The bridge can fail structurally before checking. Every language refusal is
rendered from the same traversal and canonical view. -/
def admitInput {language : CoreLanguage} (input : CoreInput language) :
    Except AdmissionFailure (InputBatch input) :=
  match decoded : input.decode with
  | .error r => .error (.fragment r)
  | .ok view =>
    match admitView view with
    | .ok batch => .ok ⟨view, (input.decode_iff _).mp decoded, batch⟩
    | .error reason =>
      match view.render reason with
      | some diagnostic => .error (.refused diagnostic)
      | none => .error (.missingLocation reason)

theorem admitInput_refused {language} {input : CoreInput language} {failure}
    (ran : admitInput input = .error (.refused failure)) : input.CheckerRefuses failure := by
  unfold admitInput at ran
  split at ran
  · cases ran
  · rename_i view decoded
    have exactView := (input.decode_iff view).mp decoded
    split at ran
    · contradiction
    · rename_i reason rejected
      split at ran
      · rename_i diagnostic rendered
        cases ran
        have checked : checkProgram view.program = .error reason := by
          unfold admitView admitProgram at rejected
          split at rejected
          · contradiction
          · cases rejected; assumption
        refine ⟨?_, view, reason, exactView, checked, rendered⟩
        rintro ⟨other, represents, accepted⟩
        cases view.unique exactView represents
        obtain ⟨_, ok⟩ := (checkProgram_iff _).mpr accepted
        rw [checked] at ok
        contradiction
      · cases ran

/-- The approved or independently typed program supplies precisely these validated
nominal declarations to Exec and the lowerings. -/
theorem declarationsOfTyped {language} {i : CoreInput language} (typed : i.StaticallyTyped) :
    DeclsOk (List.map Prod.snd i.machineProgram.declarations) := by
  obtain ⟨v, ⟨_, eq, code⟩, decls, _⟩ := typed
  cases eq
  simp only [CoreInput.machineProgram, code, Payload.execution]
  cases v <;> exact decls

/-- The issuer fixes the pre-read context and operands in the dependent batch.
Only an ordered subset of that batch can cross into operation-keyed evidence. -/
def InputBatch.encode {language} {input : CoreInput language} (checked : InputBatch input)
    (selected : List Discharge) : Option (StageInterface.Discharges language) := do
  if !checked.batch.validates selected then none else
  let operations := input.machineProgram.operations
  selected.mapM fun d => do
    let origin ← checked.view.originAt d.position
    let op ← Core.AList.lookup operations origin.operation
    let [condition] := language.safety op | none
    if condition.trap != .overflow then none else
    return (origin.operation, [⟨origin.operation, origin.evidenceKey,
      ⟨origin.operation, .safety, condition.discriminator⟩, condition.trap, condition.operands⟩])

/-- Executable checked entry for the early fragment. The result distinguishes a
language refusal from missing bridge support or failure to encode issued evidence. -/
def checkInput {language} (input : CoreInput language) :
    Except AdmissionFailure (StageInterface.Discharges language) := do
  let batch ← admitInput input
  let some evidence := batch.encode batch.batch.discharges | throw .unmappedEvidence
  return evidence

/-- Fix c's remaining simulation obligation transports the issued straight-line
facts to every matching reachable Exec configuration of this same input. -/
def DischargeTransfer : Prop :=
  ∀ {slice} (input : CoreInput (sliceLanguage slice)) (batch : InputBatch input)
    (selected : List Discharge) (out : StageInterface.Discharges (sliceLanguage slice)),
    input.WF → batch.encode selected = some out → ValidDischarges input out

end ConcreteSpec.Admission
