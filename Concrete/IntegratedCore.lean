import Concrete.Interfaces.Pipeline

/-! The elaboration implementation for the frozen slice-indexed Core handoff. -/

namespace Concrete.IntegratedCore

open Concrete.Interfaces
open ConcreteSpec.StageGrammar
open ConcreteSpec.StageInterface

private def elaboratePayload (slice : Slice) (source : Source)
    (tree : SurfaceAST sliceASTExtra slice) : Except ElabRefusal (Payload (sliceLanguage slice)) :=
  if slice = slice0 then
    match tree with
    | .inl tree =>
      match Elab.surfaceProgram tree with
      | some program =>
        ((Elab.elaborateLocated program).map (earlyPayload (sliceEarlyCore slice))).mapError
          ElabRefusal.slice0
      | none => .error (.diagnostic ⟨``Elab.surfaceProgram, 1⟩ ⟨source.entryFile, 0, 0, 1, 1⟩ [])
    | .inr _ =>
      .error (.diagnostic ⟨``SliceElaboration, 1⟩ ⟨source.entryFile, 0, 0, 1, 1⟩ [])
  else .error (.unsupportedSlice slice)

/-- Elaborate directly to the Core input selected by the registry, retaining the
issuing project. The parser supplies the tree and location invariants. -/
def elaborate (slice : Slice) (ast : Parsed ((sliceGrammar slice).value (sliceGrammar slice).start)) :
    Except ElabRefusal (CoreInput (sliceLanguage slice)) :=
  if ast.source.slice = slice then
    (elaboratePayload slice ast.source (cast (sliceGrammarStartShape slice) ast.tree)).map
      (CoreInput.mk ast.source.project slice)
  else
    .error (.diagnostic ⟨``Parsed.ElaborationDomain, 1⟩ ⟨ast.source.entryFile, 0, 0, 1, 1⟩ [])

private theorem payload_corresponds (slice : Slice) (source : Source)
    (tree : SurfaceAST sliceASTExtra slice)
    {result : Except ElabRefusal (Payload (sliceLanguage slice))}
    (reference : SliceElaboration slice tree result) :
    elaboratePayload slice source tree = result := by
  unfold SliceElaboration at reference
  unfold elaboratePayload
  split at reference
  · rename_i early
    simp only [ite_eq_left early]
    cases tree with
    | inr _ => exact False.elim reference
    | inl _ =>
      obtain ⟨program, decoded, computed⟩ := reference
      simp only [decoded]
      exact computed
  · rename_i later
    simp only [ite_eq_right later]
    exact reference.symm

private theorem payload_inFragment (slice : Slice) (source : Source)
    (tree : SurfaceAST sliceASTExtra slice) {payload : Payload (sliceLanguage slice)}
    (ran : elaboratePayload slice source tree = .ok payload) :
    (CoreInput.mk source.project slice payload).InFragment := by
  unfold elaboratePayload at ran
  split at ran
  · rename_i early
    cases tree with
    | inr _ => cases ran
    | inl tree =>
      cases decoded : Elab.surfaceProgram tree with
      | none =>
        simp only [decoded] at ran
        cases ran
      | some program =>
        cases computed : Elab.elaborateLocated program with
        | error reason =>
          simp only [decoded, computed] at ran
          cases ran
        | ok out =>
          simp only [decoded, computed] at ran
          have payloadEq := Except.ok.inj ran
          exact ⟨out, early, sliceEarlyCore slice, payloadEq.symm⟩
  · cases ran

/-- On the fixed parser domain the executable result satisfies the reference
judgment, including on refusal. -/
private theorem corresponds (slice : Slice) (ast : Parsed ((sliceGrammar slice).value (sliceGrammar slice).start))
    (domain : Parsed.ElaborationDomain slice ast) :
    SliceElaboration slice (cast (sliceGrammarStartShape slice) ast.tree)
      (elaboratePayload slice ast.source (cast (sliceGrammarStartShape slice) ast.tree)) := by
  obtain ⟨result, reference⟩ := domain.2.2.2.1
  rw [payload_corresponds slice ast.source _ reference]
  exact reference

/-- Any registered pass bundle using this function satisfies the full
elaboration contract, including the invariant needed by admission and typing. -/
theorem validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.elaborate = elaborate slice) :
    (registry passes).elaborator.Validated := by
  constructor
  · intro ast core domain ran
    change passes.elaborate ast = .ok core at ran
    rw [implementation] at ran
    simp only [elaborate, ite_eq_left domain.2.1] at ran
    have reference := corresponds slice ast domain
    cases computed : elaboratePayload slice ast.source (cast (sliceGrammarStartShape slice) ast.tree) with
    | error reason =>
      rw [computed] at ran
      cases ran
    | ok payload =>
      rw [computed] at ran reference
      have output := Except.ok.inj ran
      subst core
      have wf := domain.2.2.2.2 payload reference
      exact ⟨⟨wf, payload_inFragment slice ast.source _ computed⟩,
        rfl, domain.2.1.symm, wf, reference⟩
  · intro ast refusal domain ran
    change passes.elaborate ast = .error refusal at ran
    rw [implementation] at ran
    simp only [elaborate, ite_eq_left domain.2.1] at ran
    have reference := corresponds slice ast domain
    cases computed : elaboratePayload slice ast.source (cast (sliceGrammarStartShape slice) ast.tree) with
    | ok payload =>
      rw [computed] at ran
      cases ran
    | error reason =>
      rw [computed] at ran reference
      have same := Except.error.inj ran
      subst refusal
      exact reference

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
  change (project.files[entry.origin.lexical.file.index]?).bind _ = _
  rw [fileAt]
  simp only [trapDiagnostic, fileAt, Option.map_some, Option.getD_some]
  rfl

end LCB

end Concrete.IntegratedCore
