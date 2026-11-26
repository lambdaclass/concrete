import Concrete.Core
import Concrete.Admission
import Concrete.BIR
import Concrete.Elab
import Concrete.MIR
import Concrete.Memory
import Concrete.Refusal
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Slice0
import ConcreteSpec.Spec.Slice1Semantics
import ConcreteSpec.Spec.Slice1Syntax
import ConcreteSpec.Spec.Surface0
import LeanCompilerBackend.Verified.Job
import LeanCompilerBackend.Verified.Source
import Concrete.Lex
import Concrete.Parse
import Concrete.SSA
import Concrete.Backend.LCB

namespace Concrete.Stages

open ConcreteSpec.Core SourcePathSpan

/-- A compilation input resolved against the project table that owns its file ID. -/
structure Input where
  project : Project
  fileId : FileId
  file : File
  loaded : project.fileAt fileId = some file

def Input.path (input : Input) : String := input.file.path.render.text

def Input.source (input : Input) : ByteArray := ⟨input.file.source.toBytes.toArray⟩

/-- Resolve the exact file that supplies this compilation's bytes. -/
def Input.resolve (project : Project) (fileId : FileId) : Except Refusal Input :=
  match found : project.fileAt fileId with
  | none => .error (.internal s!"unknown project file ID {fileId.index}")
  | some file => .ok ⟨project, fileId, file, found⟩

/-- Load a single source file using the same table representation as multi-file builds. -/
def Input.load (path : String) (bytes : ByteArray) : Except Refusal Input := do
  let .ok path := SourcePath.parse (.ofBytes path.toUTF8.data.toList)
    | throw (.syntaxError "expected a canonical project-relative source path")
  Input.resolve (.singleton path (.ofBytes bytes.data.toList)) ⟨0⟩

structure Output where
  private mk ::
  project : Project
  fileId : FileId
  tokens : List ConcreteSpec.Surface0.Token
  ast : ConcreteSpec.Surface0.Program
  core : Elab.Output
  evidence : List Admission.Token
  mir : MIR.Program
  memory : Memory.Program
  bir : BIR.Program
  /-- The site table comes from elaborating the exact file in the issuing project. -/
  provenance : ∃ file,
    project.fileAt fileId = some file ∧
    Lex.lex ⟨file.source.toBytes.toArray⟩ fileId = .ok tokens ∧
    Parse.parse tokens = .ok ast ∧ Elab.elaborate ast = .ok core

/-- Resolve an actual elaborated operator site against the table used to compile it. -/
def Output.operatorSpan (out : Output) (index : Nat) : Option (ScopedSpan out.project) := do
  let site ← out.core.sites[index]?
  let op := site.operator
  if op.fileId = out.fileId then
    let projected ← projectSpanAt out.project out.fileId op.start op.stop
    if projected.line = op.line ∧ projected.col = op.column then some projected else none
  else none

private def lex (input : Input) := Lex.lex input.source input.fileId

private def memory (p : MIR.Program) : Except Refusal Memory.Program := .ok (Memory.lower p)

private def admissionRefusal : Admission.Failure → Refusal
  | .rejected => .checker "Core check rejected the program"

private def loweringRefusal : MIR.LoweringFailure → Refusal
  | .missingAdditionSite => .internal "missing source site for addition"

/-- Prepare the requested LCB branch of the slice-0 pipeline. Both backends are part of slice 0
(decided-2026-11-23-slice0-and-roadmap-answers, answer 6). -/
def prepareLCB (p : BIR.Program) :
    Except Refusal LeanCompilerBackend.Verified.Source.Function := do
  let (f, _) ← Backend.LCB.emit p
  (LeanCompilerBackend.Verified.validateJob (Backend.LCB.job f)).mapError Backend.LCB.jobRefusal
  return f

-- A typed list of stages. Each edge keeps its input in the accumulated value.
private inductive Pipeline : Type → Type → Type 1 where
  | done {A : Type} : Pipeline A A
  | next {A B C : Type} (stage : A → Except Refusal B) (rest : Pipeline (A × B) C) : Pipeline A C

private def Pipeline.run {A C : Type} (stages : Pipeline A C) (input : A) : Except Refusal C :=
  match stages with
  | .done => .ok input
  | .next stage rest => do
    let output ← stage input
    rest.run (input, output)

private def stages (path : String) :=
  Pipeline.next lex <|
  .next (fun v => Parse.parse v.2) <|
  .next (fun v => Elab.elaborate v.2) <|
  .next (fun v => (Admission.check v.2).mapError admissionRefusal) <|
  .next (fun v => (MIR.lower v.2).mapError loweringRefusal) <|
  .next (fun v => memory v.2) <|
  .next (fun v => SSA.lower path v.2) .done

private theorem ok_of_mapError_ok {ε δ α : Type} {e : Except ε α} {f : ε → δ} {a : α}
    (h : e.mapError f = .ok a) : e = .ok a := by
  cases e with
  | error => contradiction
  | ok value =>
    have eq : value = a := Except.ok.inj h
    exact congrArg Except.ok eq

private theorem edges_of_run {input : Input} {resolved tokens ast core admitted mir mem bir}
    (h : (stages input.path).run input =
      .ok (((((((resolved, tokens), ast), core), admitted), mir), mem), bir)) :
    Lex.lex input.source input.fileId = .ok tokens ∧
    Parse.parse tokens = .ok ast ∧ Elab.elaborate ast = .ok core ∧
    Admission.check core = .ok admitted ∧ MIR.lower admitted = .ok mir ∧
    Memory.lower mir = mem ∧ SSA.lower input.path mem = .ok bir := by
  simp only [stages, Pipeline.run, lex, memory, bind, Except.bind] at h
  split at h <;> try contradiction
  next lexed hLex =>
    split at h <;> try contradiction
    next parsed hParse =>
      split at h <;> try contradiction
      next elaborated hElab =>
        split at h <;> try contradiction
        next checked hAdmit =>
          split at h <;> try contradiction
          next lowered hLower =>
            split at h <;> try contradiction
            next loweredSSA hSSA =>
              cases Except.ok.inj h
              exact ⟨hLex, hParse, hElab, ok_of_mapError_ok hAdmit, ok_of_mapError_ok hLower,
                rfl, hSSA⟩

def compile (input : Input) : Except Refusal Output :=
  match h : (stages input.path).run input with
  | .error e => .error e
  | .ok ((((((( _, tokens), ast), core), admitted), mir), mem), bir) =>
    have ⟨hLex, hParse, hElab, _, _, _, _⟩ := edges_of_run h
    .ok ⟨input.project, input.fileId, tokens, ast, core, admitted.tokens, mir, mem, bir,
      ⟨input.file, input.loaded, hLex, hParse, hElab⟩⟩

/-- Compile the exact bytes in a resolved slot of the shared project table. -/
def compileFile (project : Project) (fileId : FileId) : Except Refusal Output := do
  compile (← Input.resolve project fileId)

/-- Successful compilation retains the issuing project used on the lexical edge. -/
theorem compile_project {input : Input} {out : Output} (h : compile input = .ok out) :
    out.project = input.project ∧ out.fileId = input.fileId := by
  unfold compile at h
  split at h
  · contradiction
  · cases Except.ok.inj h
    exact ⟨rfl, rfl⟩

/-- The token list recorded by successful compilation satisfies the surface lexical relation. -/
theorem compile_lexes {input : Input} {out : Output} (h : compile input = .ok out) :
    ConcreteSpec.Surface0.Lexes input.source out.tokens input.fileId := by
  obtain ⟨projectEq, fileIdEq⟩ := compile_project h
  obtain ⟨file, loaded, hLex, _, _⟩ := out.provenance
  rw [projectEq, fileIdEq, input.loaded] at loaded
  cases Option.some.inj loaded
  exact Lex.lex_correct (fileIdEq ▸ hLex)

/-- Equalities for individual passes compose into a certificate about the production fold. -/
theorem compile_of_edges (input : Input) (tokens : List ConcreteSpec.Surface0.Token)
    (ast : ConcreteSpec.Surface0.Program) (core : Elab.Output) (admitted : Admission.Output)
    (mir : MIR.Program) (mem : Memory.Program) (bir : BIR.Program)
    (hLex : Lex.lex input.source input.fileId = .ok tokens)
    (hParse : Parse.parse tokens = .ok ast)
    (hElab : Elab.elaborate ast = .ok core)
    (hAdmit : Admission.check core = .ok admitted)
    (hLower : MIR.lower admitted = .ok mir)
    (hMem : Memory.lower mir = mem)
    (hSSA : SSA.lower input.path mem = .ok bir) :
    compile input = .ok ⟨input.project, input.fileId, tokens, ast, core, admitted.tokens, mir, mem, bir,
      ⟨input.file, input.loaded, hLex, hParse, hElab⟩⟩ := by
  have runEq : (stages input.path).run input =
      .ok (((((((input, tokens), ast), core), admitted), mir), mem), bir) := by
    simp [stages, Pipeline.run, lex, memory, bind, Except.bind,
      Except.mapError, hLex, hParse, hElab, hAdmit, hLower, hMem, hSSA]
  unfold compile
  split
  · next failed h => rw [runEq] at h; contradiction
  · next actual tokens' ast' core' admitted' mir' mem' bir' h =>
      rw [runEq] at h
      cases Except.ok.inj h
      rfl

/-- Successful production compilation preserves the Core outcome through Memory. -/
theorem compile_core_correct {input : Input} {out : Output}
    (h : compile input = .ok out) :
    let p := Core.toCore out.core.program
    Memory.run out.memory = Core.run p (p.body.length + 1) := by
  unfold compile at h
  split at h
  · contradiction
  · next actual tokens ast core admitted mir mem bir runEq =>
      obtain ⟨_, _, _, hAdmit, hLower, rfl, _⟩ := edges_of_run runEq
      cases Except.ok.inj h
      exact Memory.lower_admitted_correct hAdmit hLower

/-- Every accepted production pipeline result satisfies the SSA contract on its
recorded Memory and BIR programs. -/
theorem compile_ssa_correct {input : Input} {out : Output}
    (h : compile input = .ok out) : SSA.Preserves out.memory out.bir := by
  unfold compile at h
  split at h
  · contradiction
  · next actual tokens ast core admitted mir mem bir runEq =>
      obtain ⟨_, _, _, _, _, _, hSSA⟩ := edges_of_run runEq
      cases Except.ok.inj h
      exact SSA.lower_correct hSSA

/-- The LCB adapter and profile checks compose independently of the shared stages. -/
theorem prepareLCB_of_edges (bir : BIR.Program)
    (f : LeanCompilerBackend.Verified.Source.Function)
    (hEmit : Backend.LCB.emit bir = .ok (f, bir.sites))
    (hProfile : LeanCompilerBackend.Verified.validateJob (Backend.LCB.job f) = .ok ()) :
    prepareLCB bir = .ok f := by
  simp [prepareLCB, bind, Except.bind, Except.mapError, hEmit, hProfile] <;> rfl

/-- Conformance entry point for the residual check path. It goes through the same lexer,
parser, elaborator and lowering stages, with no admission or discharged evidence. -/
def unchecked (input : Input) : Except Refusal BIR.Program := do
  let tokens ← lex input
  let ast ← Parse.parse tokens
  let core ← Elab.elaborate ast
  let mir ← (MIR.lowerUnchecked core).mapError loweringRefusal
  let mem ← memory mir
  SSA.lower input.path mem

end Concrete.Stages
