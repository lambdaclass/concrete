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
  | reduce (lhs : N) (rhs : List (Sym T N))

/-- The parser machine is independent of the grammar and the semantic values on its stack. -/
def run {T N V : Type} [DecidableEq T] [DecidableEq N]
    (table : Table T N) (kind : Token → T) (leaf : Token → Except Refusal V)
    (reduce : N → List (Sym T N) → List V → Except Refusal V)
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
      (rhs.map Task.read ++ .reduce lhs rhs :: pending) input values
  | fuel + 1, .read (.t expected) :: pending, input, values => do
    match input with
    | tok :: rest =>
      if kind tok == expected then
        let value ← leaf tok
        run table kind leaf reduce mismatch fuel pending rest (value :: values)
      else throw (mismatch (.t expected) input values)
    | [] => throw (mismatch (.t expected) [] values)
  | fuel + 1, .reduce lhs rhs :: pending, input, values => do
    let value ← reduce lhs rhs (values.take rhs.length).reverse
    run table kind leaf reduce mismatch fuel pending input (value :: values.drop rhs.length)
  | _, [], [], _ => .error (.internal "invalid parser result stack")

private inductive Value where
  | tok (token : Token)
  | expr (expr : Expr)
  | more (tail : List (Span × Expr))
  | body (bindings : List Binding) (result : Expr)
  | program (ast : Program)

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

private def syntaxError (input : List Token) : Refusal :=
  match input with
  | tok :: _ => .syntaxError s!"unexpected token at byte {tok.span.start}"
  | [] => .syntaxError "unexpected end of file"

-- Contextual markers only for refusal recognition. The slice parser still sees the original
-- identifier tokens. In particular, `let mut: u8` is a binding named `mut`.
private def refusalTokens : List Token → List Token
  | keyword :: marker :: binder :: rest =>
    if keyword.value.kind == .let_ && marker.value == .name "mut" &&
        binder.value.kind == .ident then
      keyword :: { marker with value := .fixed .mut_ } :: refusalTokens (binder :: rest)
    else contextual keyword :: refusalTokens (marker :: binder :: rest)
  | tail => tail.map contextual
where
  contextual (tok : Token) : Token :=
    match tok.value with
    | .name "if" => { tok with value := .fixed .if_ }
    | .name "else" => { tok with value := .fixed .else_ }
    | _ => tok

private def refusalTable := makeTable refusalGrammar

private inductive BodyEnding where
  | empty | fallthrough | returns | afterReturn
  deriving DecidableEq

private structure Recognition where
  outsideSlice : Bool := false
  ending : BodyEnding := .empty

private def nonDecimal (tok : Token) : Bool :=
  match tok.value with
  | .integer _ radix _ => radix != .decimal
  | _ => false

private def recognizeProduction (lhs : Nonterminal) (rhs : List (Sym Terminal Nonterminal))
    (children : List Recognition) : Except Refusal Recognition := do
  let some outside := refusalProductions.lookup (lhs, rhs)
    | throw (.internal "unclassified refusal production")
  let ending := match lhs, rhs, children.reverse with
    | .program, _, _ :: body :: _ => body.ending
    | .body, .t .return_ :: _, tail :: _ =>
      if tail.ending == .empty then .returns else .afterReturn
    | .body, _ :: _, tail :: _ =>
      if tail.ending == .empty then .fallthrough else tail.ending
    | _, _, _ => .empty
  return ⟨outside || children.any (·.outsideSlice), ending⟩

private def recognizeDecided (input : List Token) : Except Refusal Recognition :=
  run refusalTable (·.value.kind) (fun tok => .ok { outsideSlice := nonDecimal tok })
    recognizeProduction (fun _ tokens _ => syntaxError tokens)
    (32 * (input.length + 1)) [.read (.n refusalGrammar.start)] (refusalTokens input) []

def parse (input : List Token) : Except Refusal Program := do
  -- Classify a form as unsupported only after consuming its entire syntax.
  match run table0 (·.value.kind) (fun tok => .ok (.tok tok)) (fun lhs _ => reduce lhs)
      (fun _ tokens _ => syntaxError tokens)
      (16 * (input.length + 1)) [.read (.n grammar0.start)] input [] with
  | .ok (.program ast) =>
    for tok in input do
      if nonDecimal tok then
        throw (.unsupported s!"non-decimal integer at byte {tok.span.start}")
    return ast
  | .ok _ => throw (.internal "parser did not produce a program")
  | .error (.syntaxError _) =>
    let recognized ← recognizeDecided input
    if recognized.outsideSlice then
      throw (.unsupported "program uses a form outside the slice-0 fragment")
    match recognized.ending with
    | .empty | .fallthrough => throw (.checker "reachable end of a u8 function (STMT-9)")
    -- Pending an owner ruling on the slice-0 classification of statements after return.
    | .afterReturn => throw (.unsupported "program uses a form outside the slice-0 fragment")
    | .returns => throw (.internal "in-slice returning body rejected by the slice parser")
  | .error e => throw e

end Concrete.Parse
