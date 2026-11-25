import Concrete.Lex
import Concrete.Parse
import Concrete.SSA
import Concrete.Backend.LLVM
import Concrete.Backend.LCB

namespace Concrete.Stages

structure Input where
  path : String
  source : ByteArray

structure Output where
  tokens : List ConcreteSpec.Surface0.Token
  ast : ConcreteSpec.Surface0.Program
  core : Elab.Output
  evidence : List Admission.Token
  mir : MIR.Program
  memory : Memory.Program
  bir : BIR.Program

private def lex (input : Input) := Lex.lex input.source

private def memory (p : MIR.Program) : Except Refusal Memory.Program := .ok (Memory.lower p)

/-- Prepare the requested LCB branch of the slice-0 pipeline. Both backends are part of slice 0
(decided-2026-11-23-slice0-and-roadmap-answers, answer 6). -/
def prepareLCB (p : BIR.Program) :
    Except Refusal LeanCompilerBackend.Verified.Source.Function := do
  let (f, _) ← (Backend.LCB.emit p).mapError Refusal.internal
  (LeanCompilerBackend.Verified.validateJob (Backend.LCB.job f)).mapError
    (fun
      | .admission (.resourceLimit resource measured limit) =>
        .backendResource "LCB" resource measured limit
      | e => .internal s!"LCB admission profile: {repr e}")
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
  .next (fun v => Admission.check v.2) <|
  .next (fun v => MIR.lower v.2) <|
  .next (fun v => memory v.2) <|
  .next (fun v => SSA.lower path v.2) .done

def compile (input : Input) : Except Refusal Output := do
  let ((((((( _, tokens), ast), core), admitted), mir), mem), bir) ← (stages input.path).run input
  return ⟨tokens, ast, core, admitted.tokens, mir, mem, bir⟩

/-- Equalities for individual passes compose into a certificate about the production fold. -/
theorem compile_of_edges (input : Input) (tokens : List ConcreteSpec.Surface0.Token)
    (ast : ConcreteSpec.Surface0.Program) (core : Elab.Output) (admitted : Admission.Output)
    (mir : MIR.Program) (mem : Memory.Program) (bir : BIR.Program)
    (hLex : Lex.lex input.source = .ok tokens)
    (hParse : Parse.parse tokens = .ok ast)
    (hElab : Elab.elaborate ast = .ok core)
    (hAdmit : Admission.check core = .ok admitted)
    (hLower : MIR.lower admitted = .ok mir)
    (hMem : Memory.lower mir = mem)
    (hSSA : SSA.lower input.path mem = .ok bir) :
    compile input = .ok ⟨tokens, ast, core, admitted.tokens, mir, mem, bir⟩ := by
  simp [compile, stages, Pipeline.run, lex, memory, bind, Except.bind,
    hLex, hParse, hElab, hAdmit, hLower, hMem, hSSA] <;> rfl

/-- Every accepted production pipeline result satisfies the SSA contract on its
recorded Memory and BIR programs. -/
theorem compile_ssa_correct {input : Input} {out : Output}
    (h : compile input = .ok out) : SSA.Preserves out.memory out.bir := by
  cases hLex : lex input with
  | error e => simp [compile, stages, Pipeline.run, bind, Except.bind, hLex] at h
  | ok tokens =>
    cases hParse : Parse.parse tokens with
    | error e => simp [compile, stages, Pipeline.run, bind, Except.bind, hLex, hParse] at h
    | ok ast =>
      cases hElab : Elab.elaborate ast with
      | error e => simp [compile, stages, Pipeline.run, bind, Except.bind, hLex, hParse, hElab] at h
      | ok core =>
        cases hAdmit : Admission.check core with
        | error e =>
          simp [compile, stages, Pipeline.run, bind, Except.bind, hLex, hParse, hElab, hAdmit] at h
        | ok admitted =>
          cases hLower : MIR.lower admitted with
          | error e =>
            simp [compile, stages, Pipeline.run, bind, Except.bind,
              hLex, hParse, hElab, hAdmit, hLower] at h
          | ok mir =>
            cases hSSA : SSA.lower input.path (Memory.lower mir) with
            | error e =>
              simp [compile, stages, Pipeline.run, memory, bind, Except.bind,
                hLex, hParse, hElab, hAdmit, hLower, hSSA] at h
            | ok bir =>
              have he := compile_of_edges input tokens ast core admitted mir (Memory.lower mir) bir
                hLex hParse hElab hAdmit hLower rfl hSSA
              rw [he] at h
              cases h
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
  let mir ← MIR.lowerUnchecked core
  let mem ← memory mir
  SSA.lower input.path mem

end Concrete.Stages
