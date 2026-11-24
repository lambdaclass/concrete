import ConcreteSpec.Spec.Scalar
import Concrete.Admission

namespace Concrete.MIR

/-- An addition either has a discharged result or transfers to the trap at its source site
on overflow. The destination binder identifies the operation across the passes. -/
inductive Op where
  | copy (value : Core.Atom)
  | add (lhs rhs : Core.Atom) (origin : Elab.SourceSite) (discharged : Option Int)

structure Assignment where
  dest : ConcreteSpec.Var
  op : Op

structure Program where
  body : List Assignment
  result : Core.Atom

private def lowerStmt (sites : List Elab.SourceSite) (tokens : List Admission.Token)
    (stmt : Core.Stmt) : Except Refusal Assignment := do
  match stmt.e with
  | .atom a => return ⟨stmt.x, .copy a⟩
  | .add a b =>
    let some site := sites.find? (·.binding == stmt.x)
      | throw (.internal "missing source site for addition")
    let evidence := (tokens.find? (·.binding == stmt.x)).map (·.value)
    return ⟨stmt.x, .add a b site evidence⟩

private def lowerWith (core : Elab.Output) (tokens : List Admission.Token) : Except Refusal Program := do
  let body ← core.program.body.mapM (lowerStmt core.sites tokens)
  return ⟨body, core.program.ret⟩

def lower (out : Admission.Output) : Except Refusal Program :=
  lowerWith out.core out.tokens

def lowerUnchecked (core : Elab.Output) : Except Refusal Program :=
  lowerWith core []

end Concrete.MIR

namespace Concrete.MIR

private def value (env : List (ConcreteSpec.Var × Int)) :
    Core.Atom → Except ConcreteSpec.Outcome Int
  | .lit n => .ok n
  | .var x => match env.lookup x with
    | some n => .ok n
    | none => .error (.stuck .unbound)

private def evalOp (env : List (ConcreteSpec.Var × Int)) :
    Op → Except ConcreteSpec.Outcome Int
  | .copy a => value env a
  | .add lhs rhs _ discharged => do
    let a ← value env lhs
    let b ← value env rhs
    if discharged.isSome then
      return ConcreteSpec.Core.wrap .u8 (a + b)
    else
      match ConcreteSpec.checkedAdd .u8 a b with
      | .ok (.scalar (.int .u8 n)) => return n
      | .trap k => throw (.trap k)
      | _ => throw (.stuck .typeError)

def run (p : Program) : ConcreteSpec.Outcome :=
  let result : Except ConcreteSpec.Outcome Int := do
    let env ← p.body.foldlM (fun env stmt => do
      let n ← evalOp env stmt.op
      return (stmt.dest, n) :: env) []
    value env p.result
  match result with
  | .ok n => .exit n.toNat
  | .error e => e

end Concrete.MIR
