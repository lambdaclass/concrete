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

theorem InputBatch.accepted {language} {i : CoreInput language} (result : InputBatch i) (wf : i.WF) :
    i.Accepted := by
  have checked := (checkProgram_iff _).mp ⟨_, result.batch.issued⟩
  exact ⟨⟨wf, ⟨_, result.represents, result.batch.staticallyTyped⟩,
    _, result.represents, checked.2.runtime⟩, _, result.represents, checked⟩

/-- The shared traversal decides every static obligation on a well-located input. -/
theorem checkStatic_iff {language} {input : CoreInput language} (wf : input.WF) :
    input.checkStatic = .ok () ↔ input.UncheckedWF := by
  cases h : input.decode with
  | error reason =>
    constructor
    · simp [CoreInput.checkStatic, h, Except.mapError, Bind.bind, Except.bind]
    · rintro ⟨_, _, v, hv, _⟩
      rw [(input.decode_iff v).2 hv] at h
      contradiction
  | ok v =>
    have represented := (input.decode_iff v).1 h
    cases ran : checkProgramWith .runtime v.program with
    | ok ds =>
      obtain ⟨decls, body⟩ := (checkProgramWith_iff .runtime _).1 ⟨_, ran⟩
      exact ⟨fun _ => ⟨wf, ⟨v, represented, decls, body_staticallyTyped body⟩, v, represented, body⟩,
        fun _ => by simp [CoreInput.checkStatic, h, ran, Except.mapError, Bind.bind, Except.bind, Pure.pure, Except.pure]⟩
    | error reason =>
      constructor
      · simp [CoreInput.checkStatic, h, ran, Except.mapError, Bind.bind, Except.bind]
      · rintro ⟨_, ⟨t, ht, decls, _⟩, w, hw, body⟩
        cases t.unique ht represented
        cases w.unique hw represented
        obtain ⟨_, good⟩ := (checkProgramWith_iff .runtime _).2 ⟨decls, body⟩
        rw [ran] at good
        contradiction

/-- Every rendered static refusal from `CoreInput.checkStatic` preserves the exact
runtime-policy checker reason, registered rule, primary span, and related spans. -/
theorem checkStatic_refused {language} {input : CoreInput language} {failure}
    (ran : input.checkStatic = .error (.refused failure)) : input.StaticRefuses failure := by
  cases decoded : input.decode with
  | error r =>
    simp [CoreInput.checkStatic, decoded, Except.mapError, Bind.bind, Except.bind] at ran
  | ok view =>
    have exactView := (input.decode_iff view).mp decoded
    cases checked : checkProgramWith .runtime view.program with
    | ok ds =>
      simp [CoreInput.checkStatic, decoded, checked, Except.mapError, Bind.bind, Except.bind, Pure.pure, Except.pure] at ran
    | error reason =>
      simp only [CoreInput.checkStatic, decoded, checked, AdmissionView.failure, Except.mapError,
        Bind.bind, Except.bind] at ran
      cases rendered : view.render reason with
      | none => simp [rendered] at ran
      | some diagnostic =>
        simp only [rendered, Except.error.injEq, AdmissionFailure.refused.injEq] at ran
        subst ran
        refine ⟨?_, view, reason, exactView, checked, rendered⟩
        rintro ⟨_, ⟨t, ht, decls, _⟩, w, hw, body⟩
        cases t.unique ht exactView
        cases w.unique hw exactView
        obtain ⟨_, good⟩ := (checkProgramWith_iff .runtime _).2 ⟨decls, body⟩
        rw [checked] at good
        contradiction

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
        rintro ⟨_, other, represents, accepted⟩
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

private theorem checkInput_of_checkProgram_error {language} {input : CoreInput language}
    {view : AdmissionView} {reason : Refusal} {cause : RefusalCause}
    (hdec : input.decode = .ok view)
    (hchk : checkProgram view.program = .error reason)
    (hren : view.render reason = some cause) :
    checkInput input = .error (.refused cause) := by
  have hadmit : admitInput input = .error (.refused cause) := by
    unfold admitInput
    split
    · rename_i heq; rw [hdec] at heq; cases heq
    · rename_i v heq
      rw [hdec] at heq
      cases heq
      split
      · rename_i _ hok
        unfold admitView admitProgram at hok
        split at hok
        · rename_i hok'; rw [hchk] at hok'; cases hok'
        · cases hok
      · rename_i r herr
        have hr : r = reason := by
          unfold admitView admitProgram at herr
          split at herr
          · cases herr
          · rename_i herr'; rw [hchk] at herr'; cases herr'; cases herr; rfl
        subst hr
        simp [hren]
  simp [checkInput, hadmit, Bind.bind, Except.bind]

/-- Left-to-right source-order refusal theorem across both Core routes: when a
prepared Slice 0 program contains an out-of-range `u8` literal and `(n, span)`
is the earliest such literal in source order (with arbitrary nesting on either
side of `+`), `CoreInput.checkStatic` refuses `Slice0Rules.LiteralTyped@1` at
`span`, and `checkInput` either produces that exact same refusal or stops even
earlier at an overflowing addition (`Slice0Rules.AdditionFits@1`) strictly
preceding `(n, span)`. -/
theorem checkInput_and_checkStatic_first_bad_literal
    (project : Core.SourcePathSpan.Project) (ast : Surface0.Program)
    (purpose : SurfacePreparation.Purpose) {p : EarlyProgram} {n : Nat} {span : Surface0.Span}
    (prepared : SurfacePreparation.prepareSurface0 ast purpose = .ok p)
    (firstBad : (SurfacePreparation.programLiterals ast).find?
      (fun lit => !IntTy.u8.inRange lit.1) = some (n, span)) :
    let input : CoreInput (sliceLanguage slice0) := ⟨project, slice0, .straightLine (.zero p) rfl⟩
    ∃ causeStatic,
      input.checkStatic = .error (.refused causeStatic) ∧
      causeStatic.rule = ⟨``Core.Slice0Rules.LiteralTyped, 1⟩ ∧
      causeStatic.primary = surfaceFileSpan span ∧
      (checkInput input = .error (.refused causeStatic) ∨
        ∃ j < p.bindings.length, ∃ causeOverflow,
          checkInput input = .error (.refused causeOverflow) ∧
          causeOverflow.rule = ⟨``Core.Slice0Rules.AdditionFits, 1⟩ ∧
          ((p.bindings.take (j + 1)).flatMap LocatedBinding.literals).find?
            (fun lit => !IntTy.u8.inRange lit.1) = none) := by
  intro input
  obtain ⟨reason, hrun, hren, hrule, hstatic⟩ :=
    SurfacePreparation.prepareSurface0_checkProgramWith_first_bad_literal ast purpose prepared firstBad
  let causeStatic : RefusalCause := .core .slice0 (surfaceFileSpan span) [] reason
  have hdec : input.decode = .ok (.zero p) := by
    simp [input, CoreInput.decode, AdmissionView.slice]
  have hcheckStatic : input.checkStatic = .error (.refused causeStatic) := by
    simp [causeStatic, CoreInput.checkStatic, hdec, hrun,
      AdmissionView.failure, hren, Except.mapError, Bind.bind, Except.bind]
  refine ⟨causeStatic, hcheckStatic, hrule, rfl, ?_⟩
  rcases hstatic with hsame | ⟨j, hj, hov, hprefix⟩
  · left
    exact checkInput_of_checkProgram_error hdec hsame hren
  · right
    obtain ⟨origin, horigin⟩ := StageInterface.originAt_isSome_early p (Nat.le_of_lt hj)
    let causeOverflow : RefusalCause := .core .slice0 origin.lexical [] (.atPosition j .additionOverflowUnproven)
    have hrenOv : (AdmissionView.zero p).render (.atPosition j .additionOverflowUnproven) =
        some causeOverflow := by
      simp [AdmissionView.render, horigin, causeOverflow]
      rfl
    exact ⟨j, hj, causeOverflow, checkInput_of_checkProgram_error hdec hov hrenOv, rfl, hprefix⟩

/-- Whenever `checkInput` refuses a prepared Slice 0 program under
`Slice0Rules.LiteralTyped@1`, it cites the earliest out-of-range literal in
source order and agrees verbatim with `CoreInput.checkStatic`. -/
theorem checkInput_literalTyped_agrees_with_checkStatic
    (project : Core.SourcePathSpan.Project) (ast : Surface0.Program)
    (purpose : SurfacePreparation.Purpose) {p : EarlyProgram} {n : Nat} {span : Surface0.Span}
    {cause : RefusalCause}
    (prepared : SurfacePreparation.prepareSurface0 ast purpose = .ok p)
    (firstBad : (SurfacePreparation.programLiterals ast).find?
      (fun lit => !IntTy.u8.inRange lit.1) = some (n, span))
    (checked : checkInput (⟨project, slice0, .straightLine (.zero p) rfl⟩ :
      CoreInput (sliceLanguage slice0)) = .error (.refused cause))
    (isLit : cause.rule = ⟨``Core.Slice0Rules.LiteralTyped, 1⟩) :
    cause.primary = surfaceFileSpan span ∧
    (⟨project, slice0, .straightLine (.zero p) rfl⟩ : CoreInput (sliceLanguage slice0)).checkStatic =
      .error (.refused cause) := by
  obtain ⟨causeStatic, hstatic, _, hprim, hcases⟩ :=
    checkInput_and_checkStatic_first_bad_literal project ast purpose prepared firstBad
  rcases hcases with hsame | ⟨_, _, causeOv, hov, hruleOv, _⟩
  · rw [checked] at hsame
    cases hsame
    exact ⟨hprim, hstatic⟩
  · rw [checked] at hov
    cases hov
    rw [isLit] at hruleOv
    exact absurd hruleOv (by decide)

/-- Fix c's remaining simulation obligation transports the issued straight-line
facts to every matching reachable Exec configuration of this same input. -/
def DischargeTransfer : Prop :=
  ∀ {slice} (input : CoreInput (sliceLanguage slice)) (batch : InputBatch input)
    (selected : List Discharge) (out : StageInterface.Discharges (sliceLanguage slice)),
    input.WF → batch.encode selected = some out → ValidDischarges input out

end ConcreteSpec.Admission
