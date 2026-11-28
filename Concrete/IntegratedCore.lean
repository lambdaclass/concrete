import Concrete.Interfaces.Pipeline
import Concrete.IntegratedCore.Agreement
import Concrete.Core.LowerMIR

/-! The elaboration implementation for the frozen slice-indexed Core handoff. -/

namespace Concrete.IntegratedCore

open Concrete.Interfaces
open ConcreteSpec.StageGrammar
open ConcreteSpec.StageInterface

private def fromTree (slice : Slice) (source : Source)
    (tree : SurfaceAST sliceASTExtra slice) : Except ElabRefusal (AdmissionInput (sliceLanguage slice)) :=
  if h : slice = slice0 then
    match tree with
    | .inl tree =>
      match Elab.surfaceProgram tree with
      | some program => .ok (.pending source.project (.zero program) (congrArg sliceLanguage h))
      | none => .error (.invalidTree source.entryFile)
    | .inr _ =>
      .error (.unexpectedExtension source.entryFile)
  else .error (.unsupportedSlice slice)

def elaborate (slice : Slice) (ast : Parsed ((sliceGrammar slice).value (sliceGrammar slice).start)) :
    Except ElabRefusal (AdmissionInput (sliceLanguage slice)) :=
  if ast.source.slice = slice then
    fromTree slice ast.source (cast (sliceGrammarStartShape slice) ast.tree)
  else
    .error (.sliceMismatch slice ast.source.slice)

theorem elaborate_slice0_inl (source : Source) (tree : SyntaxTree) (hslice : source.slice = slice0) :
    elaborate slice0 ⟨source, .inl tree⟩ =
      match Concrete.Elab.surfaceProgram tree with
      | some program => .ok (.pending source.project (.zero program) rfl)
      | none => .error (.invalidTree source.entryFile) := by
  unfold elaborate
  rw [ite_eq_left hslice]
  rfl

private theorem fromTree_corresponds (slice : Slice) (source : Source)
    (tree : SurfaceAST sliceASTExtra slice) {result}
    (reference : SliceElaboration slice source.project tree result) :
    fromTree slice source tree = result := by
  unfold SliceElaboration at reference
  unfold fromTree
  split
  · rename_i h
    simp only [dite_eq_left h] at reference
    cases tree with
    | inr _ => exact False.elim reference
    | inl _ =>
      obtain ⟨_, decoded, computed⟩ := reference
      simp only [decoded]
      exact computed
  · rename_i h
    simp only [dite_eq_right h] at reference
    exact reference.symm

theorem validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.elaborate = elaborate slice) :
    (registry passes).elaborator.Validated := by
  have corresponds (ast) (domain : Parsed.ElaborationDomain slice ast) :
      SliceElaboration slice ast.source.project (cast (sliceGrammarStartShape slice) ast.tree)
        (passes.elaborate ast) := by
    obtain ⟨_, reference⟩ := domain.2.2.2.1
    rw [implementation, elaborate, ite_eq_left domain.2.1, fromTree_corresponds _ _ _ reference]
    exact reference
  constructor
  · intro ast input domain ran
    have h := corresponds ast domain
    change passes.elaborate ast = .ok input at ran
    rw [ran] at h
    exact ⟨domain.2.2.2.2 input h, h⟩
  · intro ast failure domain ran
    have h := corresponds ast domain
    change passes.elaborate ast = .error failure at ran
    rwa [ran] at h

/-- Static typing gate validation on the unchecked route: every on-slice static
refusal renders to the single `.admission (.refused cause)` form backed by
`CoreInput.StaticRefuses`. -/
theorem typingEntry_validated (language : CoreLanguage) (slice : Slice) :
    (typingEntry language slice).Validated :=
  Concrete.Interfaces.typingEntry_validated_of_render language slice
    (fun v _ herr => Concrete.IntegratedCore.DischargeTransfer.render_of_checkProgramWith_error .runtime v herr)

/-- Constructive stage validation for `passes.admissionEntry slice` when wired to
`ConcreteSpec.Admission.checkInput` (independent of `registry`, zero `Classical.choice`). -/
theorem admissionEntry_validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.checkCore = ConcreteSpec.Admission.checkInput) :
    (passes.admissionEntry slice).Validated := by
  have hsound := Agreement.admissionSound (language := sliceLanguage slice)
  constructor
  · intro input output ready ran
    change (if input.slice = slice then _ else _) = Except.ok output at ran
    rw [ite_eq_left ready.1] at ran
    cases hprep : input.prepare with
    | error cause => simp [hprep, Except.mapError, Bind.bind, Except.bind] at ran
    | ok prepared =>
      simp only [hprep, Except.mapError, Bind.bind, Except.bind] at ran
      rw [implementation] at ran
      cases hchk : ConcreteSpec.Admission.checkInput prepared with
      | error _ => simp [hchk, Except.map] at ran
      | ok discharges =>
        simp only [hchk, Except.map, Except.ok.injEq] at ran
        subst output
        obtain ⟨hwf, hfrag⟩ := ready.prepared hprep
        obtain ⟨hiff, hok, _⟩ := hsound prepared hwf hfrag
        have hvalid : Admitted.Valid ⟨prepared, discharges⟩ :=
          ⟨hiff.mpr ⟨discharges, hchk⟩, (hok discharges hchk).2⟩
        exact ⟨hvalid, ready.1, prepared, hprep, rfl, hvalid⟩
  · intro input failure ready ran
    change (if input.slice = slice then _ else _) = Except.error failure at ran
    rw [ite_eq_left ready.1] at ran
    cases hprep : input.prepare with
    | error cause =>
      simp only [hprep, Except.mapError, Bind.bind, Except.bind, Except.error.injEq] at ran
      subst failure
      exact ⟨ready.1, by simp [AdmissionInput.CheckerRefuses, hprep]⟩
    | ok prepared =>
      simp only [hprep, Except.mapError, Bind.bind, Except.bind] at ran
      rw [implementation] at ran
      cases hchk : ConcreteSpec.Admission.checkInput prepared with
      | ok _ => simp [hchk, Except.map] at ran
      | error err =>
        simp only [hchk, Except.map, Except.error.injEq] at ran
        subst failure
        obtain ⟨hwf, hfrag⟩ := ready.prepared hprep
        obtain ⟨_, _, herr⟩ := hsound prepared hwf hfrag
        obtain ⟨diagnostic, rfl, href⟩ := herr err hchk
        exact ⟨ready.1, by simp [AdmissionInput.CheckerRefuses, hprep, href]⟩

/-- Universal stage validation for the registered `admission` edge when wired to
`ConcreteSpec.Admission.checkInput`. -/
theorem admission_validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.checkCore = ConcreteSpec.Admission.checkInput) :
    (registry passes).admission.Validated :=
  admissionEntry_validated passes implementation

/-- Full stage soundness (`Stage.Sound`) for the registered `admission` edge. -/
theorem admission_sound {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.checkCore = ConcreteSpec.Admission.checkInput) :
    Stage.Sound passes .admission :=
  ⟨admission_validated passes implementation, fun _ _ _ _ => trivial⟩

/-- Production checked-route Core-to-MIR lowering (`Admitted` → `MIR`). -/
abbrev lowerCore {slice : Slice} {profile : Profile} :
    Admitted (sliceLanguage slice) → Except StageFailure (MIR (sliceLoweringLanguage slice) profile) :=
  Concrete.Core.LowerMIR.lowerCore

/-- Production unchecked-route Core-to-MIR lowering (`CoreInput` → `MIR`). -/
abbrev lowerUnchecked {slice : Slice} {profile : Profile} :
    CoreInput (sliceLanguage slice) → Except StageFailure (MIR (sliceLoweringLanguage slice) profile) :=
  Concrete.Core.LowerMIR.lowerUnchecked

/-- Universal stage validation for the checked `lowerCore` edge in `Registry`. -/
theorem lowerCore_validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.lowerCore = lowerCore) :
    (registry passes).lowerCore.Validated :=
  Concrete.Core.LowerMIR.lowerCore_validated passes implementation

/-- Constructive stage validation for `passes.lowerUncheckedEntry` (independent of
`registry`, zero `Classical.choice`). -/
theorem lowerUncheckedEntry_validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.lowerUnchecked = lowerUnchecked) :
    passes.lowerUncheckedEntry.Validated :=
  Concrete.Core.LowerMIR.lowerUncheckedEntry_validated passes implementation

/-- Universal stage validation for the unchecked `lowerUnchecked` edge in `Registry`. -/
theorem lowerUnchecked_validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.lowerUnchecked = lowerUnchecked) :
    (registry passes).lowerUnchecked.Validated :=
  Concrete.Core.LowerMIR.lowerUnchecked_validated passes implementation

/-- Full stage soundness (`Stage.Sound`) for the checked `lowerCore` edge. -/
theorem lowerCore_sound {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.lowerCore = lowerCore) :
    Stage.Sound passes .lowerCore :=
  Concrete.Core.LowerMIR.lowerCore_sound passes implementation

/-- Full stage soundness (`Stage.Sound`) for the unchecked `lowerUnchecked` edge. -/
theorem lowerUnchecked_sound {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.lowerUnchecked = lowerUnchecked) :
    Stage.Sound passes .lowerUnchecked :=
  Concrete.Core.LowerMIR.lowerUnchecked_sound passes implementation

-- `RoutePipeline.mk` is private to `Concrete.Interfaces.Pipeline` and cannot be forged outside it.
/--
error: Unknown constant `Concrete.Interfaces.RoutePipeline.mk`
-/
#guard_msgs (error, drop info) in
#check fun (p : Pipeline Source (BackendOutput ⟨.lcb, .x86_64Linux, 8, 4096⟩)) =>
  (RoutePipeline.mk p : RoutePipeline slice0 ⟨.lcb, .x86_64Linux, 8, 4096⟩ .checked)

section LCB

open LeanCompilerBackend.Verified
open ConcreteSpec.Core.SourcePathSpan (Project File)

/-- A word in the exit-status range is observed as a return. -/
theorem lcb_return_word {profile : Profile} {Event : Type} (project : Project) (output : LCBInput profile)
    (state : Source.State) (status : UInt8)
    (step : Source.step output.body state = .ok (.returned (.word (BitVec.ofNat 32 status.toNat)))) :
    (LCBInput.Execution (profile := profile) Event project).result output (some state)
      (.returned status ByteArray.empty) := by
  change Backend.lcbTerminalFor _ _ output (some state) = _
  unfold Backend.lcbTerminalFor
  dsimp only [Bind.bind, Option.bind]
  rw [step]
  change (if (BitVec.ofNat 32 status.toNat).toNat < 256 then
    some (Contracts.Observation.returned (UInt8.ofNat (BitVec.ofNat 32 status.toNat).toNat) ByteArray.empty)
    else _) = _
  have bounded : status.toNat < 256 := status.toFin.isLt
  simp only [BitVec.toNat_ofNat, Nat.mod_eq_of_lt (show status.toNat < 2 ^ 32 by omega),
    ite_eq_left bounded, UInt8.ofNat_toNat]

/-- A trap word indexes the exact table. The observation takes its kind,
diagnostic site and stderr bytes from the indexed origin in the issuing project. -/
theorem lcb_trap_word {profile : Profile} {Event : Type} (project : Project) (output : LCBInput profile)
    (state : Source.State) (index : TrapIndex) (entry : TrapEntry) (file : File)
    (inWord : 256 + index.index < 2 ^ 32)
    (entryAt : output.sites[index.index]? = some entry)
    (fileAt : project.files[entry.origin.lexical.file.index]? = some file)
    (step : Source.step output.body state =
      .ok (.returned (.word (BitVec.ofNat 32 (256 + index.index))))) :
    (LCBInput.Execution (profile := profile) Event project).result output (some state)
      (.trapped 134 entry.kind
        ⟨⟨entry.origin.operation.id⟩, ⟨file.path.render.text, entry.origin.lexical.line, entry.origin.lexical.col⟩⟩
        (trapDiagnostic project entry.kind entry.origin.operation entry.origin.lexical)) := by
  change Backend.lcbTerminalFor _ _ output (some state) = _
  unfold Backend.lcbTerminalFor
  dsimp only [Bind.bind, Option.bind]
  rw [step]
  change (if (BitVec.ofNat 32 (256 + index.index)).toNat < 256 then _ else
    (output.sites[(BitVec.ofNat 32 (256 + index.index)).toNat - 256]?).bind _) = _
  have trapRange : ¬ 256 + index.index < 256 := by omega
  have slot : 256 + index.index - 256 = index.index := by omega
  simp only [BitVec.toNat_ofNat, Nat.mod_eq_of_lt inWord, ite_eq_right trapRange, slot,
    entryAt, Option.bind_some]
  have hFileAt : project.fileAt entry.origin.lexical.file = some file := by
    rw [ConcreteSpec.Core.SourcePathSpan.Project.fileAt, ← Array.getElem?_toList]
    exact fileAt
  simp only [runtimeSite, trapDiagnostic, hFileAt, Bind.bind, Option.bind,
    Pure.pure, Option.map_some, Option.getD_some]

end LCB

end Concrete.IntegratedCore
