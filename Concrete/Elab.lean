import Concrete.Core
import Concrete.Interfaces.SurfaceElaboration

namespace Concrete.Elab
open ConcreteSpec.Surface0

private def evalExpr (env : List (String × Int)) : Expr → Except ConcreteSpec.Outcome Int
  | .integer value _ _ => .ok value
  | .local name _ => match env.lookup name with
    | some n => .ok n
    | none => .error (.stuck .unbound)
  | .add lhs rhs _ => do
    let a ← evalExpr env lhs
    let b ← evalExpr env rhs
    match ConcreteSpec.checkedAdd .u8 a b with
    | .ok (.scalar (.int .u8 value)) => return value
    | .trap k => throw (.trap k)
    | _ => throw (.stuck .typeError)

/-- The AST evaluator reads source names directly and evaluates operands left to right. -/
def eval (ast : Program) : ConcreteSpec.Outcome :=
  let result : Except ConcreteSpec.Outcome Int := do
    let (bindings, value) ← (checkBody ast.body).mapError (fun _ => .stuck .typeError)
    let env ← bindings.foldlM (fun env b => do
      let value ← evalExpr env b.rhs
      return (b.name, value) :: env) []
    evalExpr env value
  match result with
  | .ok n => .exit n.toNat
  | .error e => e

end Concrete.Elab
