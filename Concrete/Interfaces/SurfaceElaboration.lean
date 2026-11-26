import ConcreteSpec.Spec.Slice0
import ConcreteSpec.Spec.Surface0
import Concrete.Refusal

/-! The frozen Slice 0 elaboration used by both the driver and the stage contract. -/
namespace Concrete.Elab
open ConcreteSpec.Surface0

structure SourceSite where
  binding : ConcreteSpec.Var
  operator : Span

structure Output where
  program : ConcreteSpec.Slice0.Program
  sites : List SourceSite

structure LocatedOutput where
  bindings : List (ConcreteSpec.Slice0.Stmt × Span)
  ret : ConcreteSpec.Slice0.Atom
  returnSpan : Span

def LocatedOutput.erase (out : LocatedOutput) : Output :=
  let sites := out.bindings.filterMap fun (bind, span) =>
    match bind.e with
    | .atom _ => none
    | .add .. => some (⟨bind.x, span⟩ : SourceSite)
  ⟨⟨out.bindings.map Prod.fst, out.ret⟩, sites⟩

private structure State where
  next : Nat := 0
  bindings : List (ConcreteSpec.Slice0.Stmt × Span) := []

private def State.bind (s : State) (rhs : ConcreteSpec.Slice0.Expr) (span : Span) :
    ConcreteSpec.Var × State :=
  let x : ConcreteSpec.Var := ⟨s.next⟩
  (x, ⟨s.next + 1, (⟨x, rhs⟩, span) :: s.bindings⟩)

abbrev Scope := List (String × ConcreteSpec.Var)

private inductive Rhs where
  | atom (value : ConcreteSpec.Slice0.Atom) (span : Span)
  | add (lhs rhs : ConcreteSpec.Slice0.Atom) (site : Span)

private def Rhs.bind (rhs : Rhs) (s : State) : ConcreteSpec.Var × State :=
  match rhs with
  | .atom a span => s.bind (.atom a) span
  | .add a b site => s.bind (.add a b) site

private def Rhs.asAtom (rhs : Rhs) (s : State) : ConcreteSpec.Slice0.Atom × State :=
  match rhs with
  | .atom a _ => (a, s)
  | .add .. =>
    let (x, s) := rhs.bind s
    (.var x, s)

private def expr (scope : Scope) : Expr → State → Except Refusal (Rhs × State)
  | .integer n suffix span, s =>
    if suffix.isSome then .error (.checker s!"integer suffix in an expected-type position at byte {span.start}")
    else .ok (.atom (.lit n) span, s)
  | .local name span, s =>
    match scope.lookup name with
    | some x => .ok (.atom (.var x) span, s)
    | none => .error (.checker s!"unknown local '{name}' at byte {span.start}")
  | .add lhs rhs site, s => do
    let (left, s) ← expr scope lhs s
    let (a, s) := left.asAtom s
    let (right, s) ← expr scope rhs s
    let (b, s) := right.asAtom s
    return (.add a b site, s)

private def binding (acc : Scope × State) (b : Binding) : Except Refusal (Scope × State) := do
  let (scope, s) := acc
  unless BinderLegal (scope.map Prod.fst) b.name do
    throw (if b.name == "_" then .unsupported s!"wildcard binding at byte {b.span.start}"
      else .checker s!"binding shadows '{b.name}' at byte {b.span.start}")
  let (rhs, s) ← expr scope b.rhs s
  let (x, s) := rhs.bind s
  return ((b.name, x) :: scope, s)

/-- The same elaboration, retaining the location of each A-normal binding and
the final return operand for the integrated Core handoff. -/
def elaborateLocated (ast : Program) : Except Refusal LocatedOutput := do
  unless ast.name == "main" do
    throw (.unsupported s!"expected the entry point 'main' at byte {ast.nameSpan.start}")
  let (bindings, result) ← (checkBody ast.body).mapError fun
    | .fallthrough => .checker "reachable end of a u8 function (STMT-9)"
    | .unreachable span =>
      .checker s!"unreachable code (STMT-14) at {span.line}:{span.column}, bytes {span.start}..{span.stop}"
  let (scope, s) ← bindings.foldlM binding ([], {})
  let (rhs, s) ← expr scope result s
  let returnSpan := match rhs with
    | .atom _ span | .add _ _ span => span
  let (ret, s) := rhs.asAtom s
  return ⟨s.bindings.reverse, ret, returnSpan⟩

def elaborate (ast : Program) : Except Refusal Output :=
  (elaborateLocated ast).map LocatedOutput.erase

private theorem bind_fresh (s : State) (rhs : ConcreteSpec.Slice0.Expr) (span : Span)
    (x : ConcreteSpec.Var) (h : x.id < s.next) : (s.bind rhs span).1 ≠ x := by
  intro eq
  have he := congrArg ConcreteSpec.Var.id eq
  exact Nat.ne_of_lt h he.symm

end Concrete.Elab
