import Concrete.Interfaces.Pipeline
import Concrete.IntegratedCore.Agreement

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
