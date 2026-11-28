import Concrete.Lex
import Concrete.Parse
import Concrete.SSA
import Concrete.Backend.LLVM
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

/-- Project-scoped Slice 0 stage source for the certified front end. -/
def Input.stageSource (input : Input) : ConcreteSpec.StageInterface.Source :=
  ⟨input.project, input.fileId, ConcreteSpec.StageInterface.slice0⟩

/-- Resolve the exact file that supplies this compilation's bytes. -/
def Input.resolve (project : Project) (fileId : FileId) : Except Refusal Input :=
  match found : project.fileAt fileId with
  | none => .error (.internal s!"unknown project file ID {fileId.index}")
  | some file => .ok ⟨project, fileId, file, found⟩

def Input.parsePath (path : String) : Except Refusal SourcePath :=
  (SourcePath.parse (.ofBytes path.toUTF8.data.toList)).mapError Refusal.path

def Input.ofPath (path : SourcePath) (bytes : ByteArray) : Except Refusal Input :=
  Input.resolve (.singleton path (.ofBytes bytes.data.toList)) ⟨0⟩

/-- Load a single source file using the same table representation as multi-file builds. -/
def Input.load (path : String) (bytes : ByteArray) : Except Refusal Input := do
  Input.ofPath (← Input.parsePath path) bytes

/-- Use the loaded file's coordinate index, including for a trivia-only source. -/
def fileEnd (file : File) (id : FileId) : ConcreteSpec.Surface0.Span :=
  let pos := file.positionAt file.length (Nat.le_refl _)
  ConcreteSpec.Surface0.Position.span pos pos id

def Input.eof (input : Input) := fileEnd input.file input.fileId

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
  /-- The site table comes from lexing, parsing and elaborating the exact file in the issuing project. -/
  provenance : ∃ file stream,
    project.fileAt fileId = some file ∧
    Lex.lexStream ⟨project, fileId, ConcreteSpec.StageInterface.slice0⟩ = .ok stream ∧
    tokens = Parse.surfaceTokens stream ∧
    Parse.parseStream stream = .ok ast ∧
    Admission.prepareParsed .admission ast = .ok core

/-- Construct a `Stages.Output` from explicit verified edge proofs, keeping the
raw record constructor private. -/
def Output.ofEdges (input : Input) (stream : ConcreteSpec.StageInterface.TokenStream)
    (ast : ConcreteSpec.Surface0.Program) (core : Elab.Output) (admitted : Admission.Output)
    (mir : MIR.Program) (mem : Memory.Program) (bir : BIR.Program)
    (hLex : Lex.lexStream input.stageSource = .ok stream)
    (hParse : Parse.parseStream stream = .ok ast)
    (hElab : Admission.prepareParsed .admission ast = .ok core) : Output :=
  ⟨input.project, input.fileId, Parse.surfaceTokens stream, ast, core, admitted.tokens, mir, mem, bir,
    ⟨input.file, stream, input.loaded, hLex, rfl, hParse, hElab⟩⟩

/-- Resolve an actual elaborated operator site against the table used to compile it. -/
def Output.operatorSpan (out : Output) (index : Nat) : Option (ScopedSpan out.project) := do
  let site ← out.core.sites[index]?
  let op := site.operator
  if op.fileId = out.fileId then
    let projected ← projectSpanAt out.project out.fileId op.start op.stop
    if projected.line = op.line ∧ projected.col = op.column then some projected else none
  else none

private def lex (input : Input) : Except Refusal ConcreteSpec.StageInterface.TokenStream :=
  (Lex.lexStream input.stageSource).mapError Parse.ofLexRefusal

private theorem lex_ok_iff {input : Input} {stream : ConcreteSpec.StageInterface.TokenStream} :
    lex input = .ok stream ↔ Lex.lexStream input.stageSource = .ok stream := by
  unfold lex
  cases Lex.lexStream input.stageSource <;> simp [Except.mapError]

private def memory (p : MIR.Program) : Except Refusal Memory.Program := .ok (Memory.lower p)

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
  .next (fun v => Parse.parseStream v.2) <|
  .next (fun v => Admission.prepareParsed .admission v.2) <|
  .next (fun v => Admission.check v.2) <|
  .next (fun v => MIR.lower v.2) <|
  .next (fun v => memory v.2) <|
  .next (fun v => SSA.lower path v.2) .done

private theorem edges_of_run {input : Input} {resolved stream ast core admitted mir mem bir}
    (h : (stages input.path).run input =
      .ok (((((((resolved, stream), ast), core), admitted), mir), mem), bir)) :
    Lex.lexStream input.stageSource = .ok stream ∧
    Parse.parseStream stream = .ok ast ∧ Admission.prepareParsed .admission ast = .ok core ∧
    Admission.check core = .ok admitted ∧ MIR.lower admitted = .ok mir ∧
    Memory.lower mir = mem ∧ SSA.lower input.path mem = .ok bir := by
  simp only [stages, Pipeline.run, memory, bind, Except.bind] at h
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
              exact ⟨lex_ok_iff.mp hLex, hParse, hElab, hAdmit, hLower, rfl, hSSA⟩

private abbrev RunResult :=
  Except Refusal (((((((Input × ConcreteSpec.StageInterface.TokenStream) ×
    ConcreteSpec.Surface0.Program) × Elab.Output) × Admission.Output) ×
    MIR.Program) × Memory.Program) × BIR.Program)

private def finishCompile (input : Input) (res : RunResult)
    (hres : (stages input.path).run input = res) : Except Refusal Output :=
  match res, hres with
  | .error e, _ => .error e
  | .ok (((((((_, stream), ast), core), admitted), mir), mem), bir), h =>
    have edges := edges_of_run h
    .ok (Output.ofEdges input stream ast core admitted mir mem bir edges.1 edges.2.1 edges.2.2.1)

private theorem finishCompile_eq (input : Input) {r1 r2 : RunResult}
    (h1 : (stages input.path).run input = r1)
    (h2 : (stages input.path).run input = r2) :
    finishCompile input r1 h1 = finishCompile input r2 h2 := by
  cases h1
  cases h2
  rfl

private def exceptError {α : Type} : Except Refusal α → Option Refusal
  | .error e => some e
  | .ok _ => none

private theorem finishCompile_error {input : Input} {res : RunResult}
    {hres : (stages input.path).run input = res} {e : Refusal}
    (h : exceptError res = some e) :
    finishCompile input res hres = .error e := by
  cases res with
  | error e' =>
    cases Option.some.inj h
    rfl
  | ok _ => cases h

private theorem finishCompile_isOk {input : Input} {res : RunResult}
    {hres : (stages input.path).run input = res}
    (h : res.isOk = true) :
    (finishCompile input res hres).isOk = true := by
  cases res with
  | error _ => cases h
  | ok val =>
    rcases val with ⟨⟨⟨⟨⟨⟨⟨_, stream⟩, ast⟩, core⟩, admitted⟩, mir⟩, mem⟩, bir⟩
    rfl

def compile (input : Input) : Except Refusal Output :=
  finishCompile input ((stages input.path).run input) rfl

/-- Compile the exact bytes in a resolved slot of the shared project table. -/
def compileFile (project : Project) (fileId : FileId) : Except Refusal Output := do
  compile (← Input.resolve project fileId)

private theorem finishCompile_project {input : Input} {res : RunResult}
    {hres : (stages input.path).run input = res} {out : Output}
    (h : finishCompile input res hres = .ok out) :
    out.project = input.project ∧ out.fileId = input.fileId := by
  cases res with
  | error _ => cases h
  | ok val =>
    rcases val with ⟨⟨⟨⟨⟨⟨⟨_, stream⟩, ast⟩, core⟩, admitted⟩, mir⟩, mem⟩, bir⟩
    cases Except.ok.inj h
    exact ⟨rfl, rfl⟩

/-- Successful compilation retains the issuing project used on the lexical edge. -/
theorem compile_project {input : Input} {out : Output} (h : compile input = .ok out) :
    out.project = input.project ∧ out.fileId = input.fileId :=
  finishCompile_project h

/-- The token list recorded by successful compilation satisfies the surface lexical relation. -/
theorem compile_lexes {input : Input} {out : Output} (h : compile input = .ok out) :
    ConcreteSpec.Surface0.Lexes input.source out.tokens input.fileId := by
  obtain ⟨projectEq, fileIdEq⟩ := compile_project h
  obtain ⟨file, stream, loaded, hLex, hToks, _, _⟩ := out.provenance
  rw [projectEq, fileIdEq] at loaded hLex
  rw [input.loaded] at loaded
  cases Option.some.inj loaded
  have hLex' : Lex.lex input.source input.fileId = .ok out.tokens := by
    rw [hToks]
    exact Parse.lexStream_surfaceTokens_eq input.loaded hLex
  exact Lex.lex_correct hLex'

/-- Every compiled output's AST is derived by `Surface0.Derives` from its recorded
`Surface0.Token` list via the certified `sliceParser slice0` + `Elab.surfaceProgram` path. -/
theorem Output.derives (out : Output) :
    ConcreteSpec.Surface0.Derives out.tokens out.ast := by
  obtain ⟨_, stream, _, hLex, hToks, hParse, _⟩ := out.provenance
  have hSlice : stream.source.slice = ConcreteSpec.StageInterface.slice0 :=
    congrArg ConcreteSpec.StageInterface.Source.slice (Parse.lexStream_source_eq hLex)
  rw [hToks]
  exact Parse.parseStream_derives hSlice hParse

/-- The AST recorded by successful compilation satisfies `Surface0.Derives`. -/
theorem compile_parses {input : Input} {out : Output} (_h : compile input = .ok out) :
    ConcreteSpec.Surface0.Derives out.tokens out.ast :=
  out.derives

/-- Equalities for individual passes compose into a certificate about the production fold. -/
theorem compile_of_edges (input : Input) (stream : ConcreteSpec.StageInterface.TokenStream)
    (ast : ConcreteSpec.Surface0.Program) (core : Elab.Output) (admitted : Admission.Output)
    (mir : MIR.Program) (mem : Memory.Program) (bir : BIR.Program)
    (hLex : Lex.lexStream input.stageSource = .ok stream)
    (hParse : Parse.parseStream stream = .ok ast)
    (hElab : Admission.prepareParsed .admission ast = .ok core)
    (hAdmit : Admission.check core = .ok admitted)
    (hLower : MIR.lower admitted = .ok mir)
    (hMem : Memory.lower mir = mem)
    (hSSA : SSA.lower input.path mem = .ok bir) :
    compile input = .ok (Output.ofEdges input stream ast core admitted mir mem bir hLex hParse hElab) := by
  have runEq : (stages input.path).run input =
      .ok (((((((input, stream), ast), core), admitted), mir), mem), bir) := by
    simp [stages, Pipeline.run, memory, bind, Except.bind,
      lex_ok_iff.mpr hLex, hParse, hElab, hAdmit, hLower, hMem, hSSA]
  exact finishCompile_eq input rfl runEq

private theorem finishCompile_core_correct {input : Input} {res : RunResult}
    {hres : (stages input.path).run input = res} {out : Output}
    (h : finishCompile input res hres = .ok out) :
    let p := Core.toCore out.core.program
    Memory.run out.memory = Core.run p (p.body.length + 1) := by
  cases res with
  | error _ => cases h
  | ok val =>
    rcases val with ⟨⟨⟨⟨⟨⟨⟨_, stream⟩, ast⟩, core⟩, admitted⟩, mir⟩, mem⟩, bir⟩
    obtain ⟨_, _, _, hAdmit, hLower, rfl, _⟩ := edges_of_run hres
    cases Except.ok.inj h
    exact Memory.lower_admitted_correct hAdmit hLower

/-- Successful production compilation preserves the Core outcome through Memory. -/
theorem compile_core_correct {input : Input} {out : Output}
    (h : compile input = .ok out) :
    let p := Core.toCore out.core.program
    Memory.run out.memory = Core.run p (p.body.length + 1) :=
  finishCompile_core_correct h

private theorem finishCompile_ssa_correct {input : Input} {res : RunResult}
    {hres : (stages input.path).run input = res} {out : Output}
    (h : finishCompile input res hres = .ok out) :
    SSA.Preserves out.memory out.bir := by
  cases res with
  | error _ => cases h
  | ok val =>
    rcases val with ⟨⟨⟨⟨⟨⟨⟨_, stream⟩, ast⟩, core⟩, admitted⟩, mir⟩, mem⟩, bir⟩
    obtain ⟨_, _, _, _, _, _, hSSA⟩ := edges_of_run hres
    cases Except.ok.inj h
    exact SSA.lower_correct hSSA

/-- Every accepted production pipeline result satisfies the SSA contract on its
recorded Memory and BIR programs. -/
theorem compile_ssa_correct {input : Input} {out : Output}
    (h : compile input = .ok out) : SSA.Preserves out.memory out.bir :=
  finishCompile_ssa_correct h

/-- The LCB adapter and profile checks compose independently of the shared stages. -/
theorem prepareLCB_of_edges (bir : BIR.Program)
    (f : LeanCompilerBackend.Verified.Source.Function)
    (hEmit : Backend.LCB.emit bir = .ok (f, bir.sites))
    (hProfile : LeanCompilerBackend.Verified.validateJob (Backend.LCB.job f) = .ok ()) :
    prepareLCB bir = .ok f := by
  simp [prepareLCB, bind, Except.bind, Except.mapError, hEmit, hProfile] <;> rfl

/-- Conformance entry point for the residual check path. It goes through the exact
same lossless lexer, certified `sliceParser slice0`, left-to-right surface preparation
and lowering stages, with every static rule and residual arithmetic checks. -/
def unchecked (input : Input) : Except Refusal BIR.Program := do
  let stream ← lex input
  let ast ← Parse.parseStream stream
  let core ← Admission.prepareParsed .eligibility ast
  let _ ← Admission.checkWith .runtime core
  let mir ← MIR.lowerUnchecked core
  let mem ← memory mir
  SSA.lower input.path mem

/-- Every successful unchecked run passes through `Lex.lexStream`, `Parse.parseStream`,
and `Admission.prepareParsed .eligibility`. -/
theorem unchecked_frontend {input : Input} {bir : BIR.Program}
    (ran : unchecked input = .ok bir) :
    ∃ stream ast core,
      Lex.lexStream input.stageSource = .ok stream ∧
      Parse.parseStream stream = .ok ast ∧
      Admission.prepareParsed .eligibility ast = .ok core := by
  simp only [unchecked, bind, Except.bind] at ran
  split at ran <;> try contradiction
  next stream lexed =>
    split at ran <;> try contradiction
    next ast parsed =>
      split at ran <;> try contradiction
      next core prepared =>
        exact ⟨stream, ast, core, lex_ok_iff.mp lexed, parsed, prepared⟩

/-! ## Proof-only sparse-lookup evaluation bridges for kernel tests -/

namespace KernelEval

private def stagesSparse (path : String) :=
  Pipeline.next lex <|
  .next (fun v => Parse.KernelEval.parseStreamSparse v.2) <|
  .next (fun v => Admission.prepareParsed .admission v.2) <|
  .next (fun v => Admission.check v.2) <|
  .next (fun v => MIR.lower v.2) <|
  .next (fun v => memory v.2) <|
  .next (fun v => SSA.lower path v.2) .done

private theorem stages_run_eq_sparse (path : String) (input : Input) :
    (stages path).run input = (stagesSparse path).run input := by
  simp only [stages, stagesSparse, Pipeline.run, bind, Except.bind]
  cases lex input with
  | error _ => rfl
  | ok stream =>
    dsimp only
    rw [Parse.KernelEval.parseStream_eq_sparse]

def compileSparse (input : Input) : Except Refusal Output :=
  finishCompile input ((stagesSparse input.path).run input) (stages_run_eq_sparse input.path input)

theorem compile_eq_sparse (input : Input) :
    compile input = compileSparse input :=
  finishCompile_eq input rfl (stages_run_eq_sparse input.path input)

def compileErrorSparse (input : Input) : Option Refusal :=
  exceptError ((stagesSparse input.path).run input)

theorem compile_eq_error_of_sparse {input : Input} {e : Refusal}
    (h : compileErrorSparse input = some e) :
    compile input = .error e := by
  rw [compile_eq_sparse]
  exact finishCompile_error h

def compileIsOkSparse (input : Input) : Bool :=
  ((stagesSparse input.path).run input).isOk

theorem compile_isOk_of_sparse {input : Input}
    (h : compileIsOkSparse input = true) :
    (compile input).isOk = true := by
  rw [compile_eq_sparse]
  exact finishCompile_isOk h

def loadCompileErrorSparse (path : String) (bytes : ByteArray) : Option Refusal :=
  exceptError ((Input.load path bytes).bind (fun input => (stagesSparse input.path).run input))

theorem load_compile_eq_error_of_sparse {path : String} {bytes : ByteArray} {e : Refusal}
    (h : loadCompileErrorSparse path bytes = some e) :
    (Input.load path bytes).bind compile = .error e := by
  unfold loadCompileErrorSparse at h
  cases hload : Input.load path bytes with
  | error _ =>
    rw [hload] at h
    cases Option.some.inj h
    rfl
  | ok input =>
    rw [hload] at h
    exact compile_eq_error_of_sparse h

def loadCompileIsOkSparse (path : String) (bytes : ByteArray) : Bool :=
  ((Input.load path bytes).bind (fun input => (stagesSparse input.path).run input)).isOk

theorem load_compile_isOk_of_sparse {path : String} {bytes : ByteArray}
    (h : loadCompileIsOkSparse path bytes = true) :
    ((Input.load path bytes).bind compile).isOk = true := by
  unfold loadCompileIsOkSparse at h
  cases hload : Input.load path bytes with
  | error _ => rw [hload] at h; cases h
  | ok input =>
    rw [hload] at h
    exact compile_isOk_of_sparse h

def uncheckedSparse (input : Input) : Except Refusal BIR.Program := do
  let stream ← lex input
  let ast ← Parse.KernelEval.parseStreamSparse stream
  let core ← Admission.prepareParsed .eligibility ast
  let _ ← Admission.checkWith .runtime core
  let mir ← MIR.lowerUnchecked core
  let mem ← memory mir
  SSA.lower input.path mem

theorem unchecked_eq_sparse (input : Input) :
    unchecked input = uncheckedSparse input := by
  unfold unchecked uncheckedSparse
  simp only [bind, Except.bind]
  cases lex input with
  | error _ => rfl
  | ok stream =>
    dsimp only
    rw [Parse.KernelEval.parseStream_eq_sparse]

def uncheckedErrorSparse (input : Input) : Option Refusal :=
  exceptError (uncheckedSparse input)

theorem unchecked_eq_error_of_sparse {input : Input} {e : Refusal}
    (h : uncheckedErrorSparse input = some e) :
    unchecked input = .error e := by
  rw [unchecked_eq_sparse]
  unfold uncheckedErrorSparse exceptError at h
  cases hu : uncheckedSparse input with
  | error _ =>
    rw [hu] at h
    cases Option.some.inj h
    rfl
  | ok _ =>
    rw [hu] at h
    cases h

theorem load_unchecked_eq_sparse (path : String) (bytes : ByteArray) :
    (Input.load path bytes).bind unchecked = (Input.load path bytes).bind uncheckedSparse := by
  cases Input.load path bytes with
  | error _ => rfl
  | ok input => exact unchecked_eq_sparse input

def loadUncheckedErrorSparse (path : String) (bytes : ByteArray) : Option Refusal :=
  exceptError ((Input.load path bytes).bind uncheckedSparse)

theorem load_unchecked_eq_error_of_sparse {path : String} {bytes : ByteArray} {e : Refusal}
    (h : loadUncheckedErrorSparse path bytes = some e) :
    (Input.load path bytes).bind unchecked = .error e := by
  rw [load_unchecked_eq_sparse]
  unfold loadUncheckedErrorSparse exceptError at h
  cases hu : (Input.load path bytes).bind uncheckedSparse with
  | error _ =>
    rw [hu] at h
    cases Option.some.inj h
    rfl
  | ok _ =>
    rw [hu] at h
    cases h

end KernelEval

end Concrete.Stages
