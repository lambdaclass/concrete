import Concrete.Core
import ConcreteSpec.Spec.Surface0
import Concrete.Refusal

namespace Concrete.Elab
open ConcreteSpec.Surface0

structure SourceSite where
  binding : ConcreteSpec.Var
  operator : Span

structure Output where
  program : Core.Program
  sites : List SourceSite

private structure State where
  next : Nat := 0
  bindings : List Core.Stmt := []
  sites : List SourceSite := []

private def State.bind (s : State) (rhs : Core.Expr) (site : Option Span) :
    ConcreteSpec.Var × State :=
  let x : ConcreteSpec.Var := ⟨s.next⟩
  (x, ⟨s.next + 1, ⟨x, rhs⟩ :: s.bindings,
    match site with | some span => ⟨x, span⟩ :: s.sites | none => s.sites⟩)

abbrev Scope := List (String × ConcreteSpec.Var)

private inductive Rhs where
  | atom (value : Core.Atom)
  | add (lhs rhs : Core.Atom) (site : Span)

private def Rhs.bind (rhs : Rhs) (s : State) : ConcreteSpec.Var × State :=
  match rhs with
  | .atom a => s.bind (.atom a) none
  | .add a b site => s.bind (.add a b) (some site)

private def Rhs.asAtom (rhs : Rhs) (s : State) : Core.Atom × State :=
  match rhs with
  | .atom a => (a, s)
  | .add .. =>
    let (x, s) := rhs.bind s
    (.var x, s)

private def expr (scope : Scope) : Expr → State → Except Refusal (Rhs × State)
  | .integer n suffix span, s =>
    if suffix.isSome then .error (.checker s!"integer suffix in an expected-type position at byte {span.start}")
    else .ok (.atom (.lit n), s)
  | .local name span, s =>
    match scope.lookup name with
    | some x => .ok (.atom (.var x), s)
    | none => .error (.checker s!"unknown local '{name}' at byte {span.start}")
  | .add lhs rhs site, s => do
    let (left, s) ← expr scope lhs s
    let (a, s) := left.asAtom s
    let (right, s) ← expr scope rhs s
    let (b, s) := right.asAtom s
    return (.add a b site, s)

private def binding (acc : Scope × State) (b : Binding) : Except Refusal (Scope × State) := do
  let (scope, s) := acc
  if b.name == "_" then throw (.unsupported s!"wildcard binding at byte {b.span.start}")
  if b.name == "main" || (scope.lookup b.name).isSome then
    throw (.checker s!"binding shadows '{b.name}' at byte {b.span.start}")
  let (rhs, s) ← expr scope b.rhs s
  let (x, s) := rhs.bind s
  return ((b.name, x) :: scope, s)

def elaborate (ast : Program) : Except Refusal Output := do
  unless ast.name == "main" do
    throw (.unsupported s!"expected the entry point 'main' at byte {ast.nameSpan.start}")
  let (scope, s) ← ast.bindings.foldlM binding ([], {})
  let (rhs, s) ← expr scope ast.result s
  let (ret, s) := rhs.asAtom s
  return ⟨⟨s.bindings.reverse, ret⟩, s.sites.reverse⟩

private theorem bind_fresh (s : State) (rhs : Core.Expr) (site : Option Span)
    (x : ConcreteSpec.Var) (h : x.id < s.next) : (s.bind rhs site).1 ≠ x := by
  intro eq
  have he := congrArg ConcreteSpec.Var.id eq
  exact Nat.ne_of_lt h he.symm

end Concrete.Elab

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
    let env ← ast.bindings.foldlM (fun env b => do
      let value ← evalExpr env b.rhs
      return (b.name, value) :: env) []
    evalExpr env ast.result
  match result with
  | .ok n => .exit n.toNat
  | .error e => e

end Concrete.Elab
