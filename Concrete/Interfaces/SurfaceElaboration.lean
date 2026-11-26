import ConcreteSpec.StageInterface
import ConcreteSpec.Spec.Surface0
import Concrete.Refusal

/-! The frozen Slice 0 elaboration used by both the driver and the stage contract. -/
namespace Concrete.Elab
open ConcreteSpec.Surface0

open ConcreteSpec.StageInterface (SyntaxTree FileSpan)

private def surfaceSpan (span : FileSpan) : Span :=
  ⟨span.startOffset, span.endOffset, span.line, span.col, span.file⟩

private def SyntaxTree.expression : SyntaxTree → Option Expr
  | .leaf ⟨span, .integer n _ suffix⟩ => some (.integer n suffix (surfaceSpan span))
  | .leaf ⟨span, .name name⟩ => some (.local name (surfaceSpan span))
  | .node .add [left, .leaf ⟨op, .fixed .plus _⟩, right] => do
    return .add (← expression left) (← expression right) (surfaceSpan op)
  | _ => none

private def SyntaxTree.statement : SyntaxTree → Option Statement
  | .node .return_ [.leaf ⟨start, .fixed .return_ _⟩, value, .leaf ⟨stop, .fixed .semi _⟩] =>
    return .ret (← expression value) (surfaceSpan {start with endOffset := stop.endOffset})
  | .node .binding [.leaf ⟨start, .fixed .let_ _⟩, .leaf ⟨nameSpan, .name name⟩,
      value, .leaf ⟨stop, .fixed .semi _⟩] =>
    return .bind ⟨name, surfaceSpan nameSpan, ← expression value⟩
      (surfaceSpan {start with endOffset := stop.endOffset})
  | _ => none

/-- Interpret the grammar's constructor tree. Token payloads and spans are
read directly from its leaves. -/
def surfaceProgram : SyntaxTree → Option Program
  | .node .program [.leaf ⟨span, .name name⟩, .node .body statements] =>
    return ⟨name, surfaceSpan span, ← statements.mapM SyntaxTree.statement⟩
  | _ => none

inductive Failure where
  | unexpectedEntry (span : Span)
  | fallthrough
  | unreachable (span : Span)
  | integerSuffix (span : Span)
  | unknownLocal (name : String) (span : Span)
  | wildcard (span : Span)
  | shadowing (name : String) (span : Span)

def Failure.diagnostic : Failure → Refusal
  | .unexpectedEntry span => .unsupported s!"expected the entry point 'main' at byte {span.start}"
  | .fallthrough => .checker "reachable end of a u8 function (STMT-9)"
  | .unreachable span =>
    .checker s!"unreachable code (STMT-14) at {span.line}:{span.column}, bytes {span.start}..{span.stop}"
  | .integerSuffix span => .checker s!"integer suffix in an expected-type position at byte {span.start}"
  | .unknownLocal name span => .checker s!"unknown local '{name}' at byte {span.start}"
  | .wildcard span => .unsupported s!"wildcard binding at byte {span.start}"
  | .shadowing name span => .checker s!"binding shadows '{name}' at byte {span.start}"

abbrev LocatedOutput := ConcreteSpec.StageInterface.EarlyProgram

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

private def expr (scope : Scope) : Expr → State → Except Failure (Rhs × State)
  | .integer n suffix span, s =>
    if suffix.isSome then .error (.integerSuffix span)
    else .ok (.atom (.lit n) span, s)
  | .local name span, s =>
    match scope.lookup name with
    | some x => .ok (.atom (.var x) span, s)
    | none => .error (.unknownLocal name span)
  | .add lhs rhs site, s => do
    let (left, s) ← expr scope lhs s
    let (a, s) := left.asAtom s
    let (right, s) ← expr scope rhs s
    let (b, s) := right.asAtom s
    return (.add a b site, s)

private def binding (acc : Scope × State) (b : Binding) : Except Failure (Scope × State) := do
  let (scope, s) := acc
  unless BinderLegal (scope.map Prod.fst) b.name do
    throw (if b.name == "_" then .wildcard b.span
      else .shadowing b.name b.span)
  let (rhs, s) ← expr scope b.rhs s
  let (x, s) := rhs.bind s
  return ((b.name, x) :: scope, s)

/-- The same elaboration, retaining the location of each A-normal binding and
the final return operand for the integrated Core handoff. -/
def elaborateLocated (ast : Program) : Except Failure LocatedOutput := do
  unless ast.name == "main" do
    throw (.unexpectedEntry ast.nameSpan)
  let (bindings, result) ← (checkBody ast.body).mapError fun
    | .fallthrough => .fallthrough
    | .unreachable span => .unreachable span
  let (scope, s) ← bindings.foldlM binding ([], {})
  let (rhs, s) ← expr scope result s
  let returnSpan := match rhs with
    | .atom _ span | .add _ _ span => span
  let (ret, s) := rhs.asAtom s
  return ⟨s.bindings.reverse, ret, returnSpan⟩

private theorem bind_fresh (s : State) (rhs : ConcreteSpec.Slice0.Expr) (span : Span)
    (x : ConcreteSpec.Var) (h : x.id < s.next) : (s.bind rhs span).1 ≠ x := by
  intro eq
  have he := congrArg ConcreteSpec.Var.id eq
  exact Nat.ne_of_lt h he.symm

end Concrete.Elab
