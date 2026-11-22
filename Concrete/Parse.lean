import ConcreteSpec.Spec.Surface0
import Concrete.Refusal

namespace Concrete.Parse
open ConcreteSpec.Surface0 Concrete.LL1

abbrev Table (T N : Type) := List ((N × Option T) × List (Sym T N))

def makeTable {T N : Type} [DecidableEq T] [DecidableEq N] (g : Grammar T N) : Table T N :=
  let (nu, fi, fo) := compute g
  g.prods.flatMap fun (lhs, rhs) => (predictB nu fi fo lhs rhs).map fun look => ((lhs, look), rhs)

open Sym in
-- The checked constant avoids recomputing FIRST/FOLLOW in the compiler or its certificates.
def table0 : Table Terminal Nonterminal :=
  let atoms := [Terminal.integer, .ident, .lparen]
  [((.program, some .fn), [t .fn, t .ident, t .lparen, t .rparen, t .arrow, t .u8,
                          t .lbrace, n .body, t .rbrace]),
   ((.body, some .let_), [t .let_, t .ident, t .colon, t .u8, t .equal, n .expr, t .semi, n .body]),
   ((.body, some .return_), [t .return_, n .expr, t .semi])] ++
  (atoms.map fun look => ((.expr, some look), [n .atom, n .more])) ++
  [((.more, some .plus), [t .plus, n .atom, n .more]),
   ((.more, some .semi), []), ((.more, some .rparen), []),
   ((.atom, some .integer), [t .integer]), ((.atom, some .ident), [t .ident]),
   ((.atom, some .lparen), [t .lparen, n .expr, t .rparen])]

theorem table0_checked :
    (table0.all (makeTable grammar0).contains &&
     (makeTable grammar0).all table0.contains) = true := by decide

inductive Task (T N : Type) where
  | read (symbol : Sym T N)
  | reduce (lhs : N) (arity : Nat)

/-- The parser machine is independent of the grammar and the semantic values on its stack. -/
def run {T N V : Type} [DecidableEq T] [DecidableEq N]
    (table : Table T N) (kind : Token → T) (leaf : Token → Except Refusal V)
    (reduce : N → List V → Except Refusal V)
    (mismatch : Sym T N → List Token → List V → Refusal) :
    Nat → List (Task T N) → List Token → List V → Except Refusal V
  | _, [], [], [result] => .ok result
  | _, [], _ :: _, _ => .error (.syntaxError "trailing tokens")
  | 0, _, _, _ => .error (.internal "parser exhausted its progress bound")
  | fuel + 1, .read (.n lhs) :: pending, input, values => do
    let look := input.head?.map kind
    let some (_, rhs) := table.find? (fun row => row.1 == (lhs, look))
      | throw (mismatch (.n lhs) input values)
    run table kind leaf reduce mismatch fuel
      (rhs.map Task.read ++ .reduce lhs rhs.length :: pending) input values
  | fuel + 1, .read (.t expected) :: pending, input, values => do
    match input with
    | tok :: rest =>
      if kind tok == expected then
        let value ← leaf tok
        run table kind leaf reduce mismatch fuel pending rest (value :: values)
      else throw (mismatch (.t expected) input values)
    | [] => throw (mismatch (.t expected) [] values)
  | fuel + 1, .reduce lhs count :: pending, input, values => do
    let value ← reduce lhs (values.take count).reverse
    run table kind leaf reduce mismatch fuel pending input (value :: values.drop count)
  | _, [], [], _ => .error (.internal "invalid parser result stack")

private inductive Value where
  | tok (token : Token)
  | expr (expr : Expr)
  | more (tail : List (Span × Expr))
  | body (bindings : List Binding) (result : Expr)
  | program (ast : Program)

private def leaf (tok : Token) : Except Refusal Value :=
  match tok.value with
  | .integer _ radix _ =>
    if radix == .decimal then .ok (.tok tok)
    else .error (.unsupported s!"non-decimal integer at byte {tok.span.start}")
  | _ => .ok (.tok tok)

private def reduce : Nonterminal → List Value → Except Refusal Value
  | .program, [_, .tok ⟨.name name, span⟩, _, _, _, _, _, .body bindings result, _] =>
    .ok (.program ⟨name, span, bindings, result⟩)
  | .body, [_, .tok ⟨.name name, span⟩, _, _, _, .expr rhs, _, .body bindings result] =>
    .ok (.body (⟨name, span, rhs⟩ :: bindings) result)
  | .body, [_, .expr result, _] => .ok (.body [] result)
  | .expr, [.expr head, .more tail] =>
    .ok (.expr (tail.foldl (fun lhs (span, rhs) => .add lhs rhs span) head))
  | .more, [.tok op, .expr rhs, .more tail] => .ok (.more ((op.span, rhs) :: tail))
  | .more, [] => .ok (.more [])
  | .atom, [.tok ⟨.integer value _ suffix, span⟩] => .ok (.expr (.integer value suffix span))
  | .atom, [.tok ⟨.name name, span⟩] => .ok (.expr (.local name span))
  | .atom, [_, .expr expr, _] => .ok (.expr expr)
  | _, _ => .error (.internal "grammar and semantic actions disagree")

private def mismatch (expected : Sym Terminal Nonterminal) (input : List Token)
    (values : List Value) : Refusal :=
  match expected, input, values with
  | .t .colon, ⟨.name _, _⟩ :: _, .tok ⟨.name "mut", span⟩ :: _ =>
    .unsupported s!"mutable binding at byte {span.start}"
  | .n .body, ⟨.name "if", span⟩ :: _, _ =>
    .unsupported s!"conditional at byte {span.start}"
  | _, tok :: _, _ => .syntaxError s!"unexpected token at byte {tok.span.start}"
  | _, [], _ => .syntaxError "unexpected end of file"

def parse (input : List Token) : Except Refusal Program := do
  -- Every expansion or reduction of grammar0 can be charged to a nearby consumed token.
  let value ← run table0 (·.value.kind) leaf reduce mismatch
    (16 * (input.length + 1)) [.read (.n grammar0.start)] input []
  match value with
  | .program ast => return ast
  | _ => throw (.internal "parser did not produce a program")

end Concrete.Parse
