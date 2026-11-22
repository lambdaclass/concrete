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
  llvm : String
  lcb : LeanCompilerBackend.Verified.Source.Function

private def lex (input : Input) := Lex.lex input.source

private def memory (p : MIR.Program) : Except Refusal Memory.Program := .ok (Memory.lower p)

private def llvm (p : BIR.Program) : Except Refusal String := .ok (Backend.LLVM.module p)

private def lcb (p : BIR.Program) :
    Except Refusal LeanCompilerBackend.Verified.Source.Function := do
  let (f, _) ← (Backend.LCB.emit p).mapError Refusal.internal
  (LeanCompilerBackend.Verified.validateJob (Backend.LCB.job f)).mapError
    (fun e => .unsupported s!"LCB admission profile: {repr e}")
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
  .next (fun v => SSA.lower path v.2) <|
  .next (fun v => llvm v.2) <|
  .next (fun v => lcb v.1.2) .done

def compile (input : Input) : Except Refusal Output := do
  let ((((((((( _, tokens), ast), core), admitted), mir), mem), bir), llvm), lcb) ← (stages input.path).run input
  return ⟨tokens, ast, core, admitted.tokens, mir, mem, bir, llvm, lcb⟩

/-- Equalities for individual passes compose into a certificate about the production fold. -/
theorem compile_of_edges (input : Input) (tokens : List ConcreteSpec.Surface0.Token)
    (ast : ConcreteSpec.Surface0.Program) (core : Elab.Output) (admitted : Admission.Output)
    (mir : MIR.Program) (mem : Memory.Program) (bir : BIR.Program)
    (lcbInput : LeanCompilerBackend.Verified.Source.Function)
    (hLex : Lex.lex input.source = .ok tokens)
    (hParse : Parse.parse tokens = .ok ast)
    (hElab : Elab.elaborate ast = .ok core)
    (hAdmit : Admission.check core = .ok admitted)
    (hLower : MIR.lower admitted = .ok mir)
    (hMem : Memory.lower mir = mem)
    (hSSA : SSA.lower input.path mem = .ok bir)
    (hEmit : Backend.LCB.emit bir = .ok (lcbInput, bir.sites))
    (hProfile : LeanCompilerBackend.Verified.validateJob (Backend.LCB.job lcbInput) = .ok ()) :
    compile input = .ok ⟨tokens, ast, core, admitted.tokens, mir, mem, bir,
      Backend.LLVM.module bir, lcbInput⟩ := by
  simp [compile, stages, Pipeline.run, lex, memory, llvm, lcb, bind, Except.bind,
    Except.mapError, hLex, hParse, hElab, hAdmit, hLower, hMem, hSSA, hEmit, hProfile] <;> rfl

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
