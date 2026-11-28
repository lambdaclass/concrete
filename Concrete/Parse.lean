import ConcreteSpec.Spec.Surface0
import Concrete.Refusal

namespace Concrete.Parse
open ConcreteSpec.Surface0 Concrete.LL1
open ConcreteSpec.StageInterface (RefusalCause)

abbrev Table (T N : Type) := List ((N × Option T) × List (Sym T N))

def makeTable {T N : Type} [DecidableEq T] [DecidableEq N] (g : Grammar T N) : Table T N :=
  let (nu, fi, fo) := compute g
  g.prods.flatMap fun (lhs, rhs) => (predictB nu fi fo lhs rhs).map fun look => ((lhs, look), rhs)

/-- The parser can only use the prediction table of a checked grammar. -/
structure CheckedTable (T N : Type) [DecidableEq T] [DecidableEq N] where
  grammar : CheckedGrammar T N
  entries : Table T N
  entries_eq : entries = makeTable grammar.grammar

open Sym in
-- The checked constant avoids recomputing FIRST/FOLLOW in the compiler or its certificates.
private def table0Entries : Table Terminal Nonterminal :=
  let atoms := [Terminal.integer, .ident, .lparen]
  [((.program, some .fn), [t .fn, t .ident, t .lparen, t .rparen, t .arrow, t .u8,
                          t .lbrace, n .body, t .rbrace]),
   ((.body, some .let_), [t .let_, t .ident, t .colon, t .u8, t .equal, n .expr, t .semi, n .body]),
   ((.body, some .return_), [t .return_, n .expr, t .semi, n .body]),
   ((.body, some .rbrace), [])] ++
  (atoms.map fun look => ((.expr, some look), [n .atom, n .more])) ++
  [((.more, some .plus), [t .plus, n .atom, n .more]),
   ((.more, some .semi), []), ((.more, some .rparen), []),
   ((.atom, some .integer), [t .integer]), ((.atom, some .ident), [t .ident]),
   ((.atom, some .lparen), [t .lparen, n .expr, t .rparen])]

theorem table0_checked :
    table0Entries = makeTable grammar0 := by decide

def table0 : CheckedTable Terminal Nonterminal :=
  ⟨checkedGrammar0, table0Entries, table0_checked⟩

private inductive Task (T N : Type) where
  | read (symbol : Sym T N)
  | reduce (lhs : N) (rhs : List (Sym T N)) (start : Span)

private def lookSpan (input : List Token) (eof : Span) : Span :=
  input.head?.map Token.span |>.getD eof

private def mismatch (input : List Token) (eof : Span) : RefusalCause :=
  match input with
  | tok :: _ => .parser .unexpectedToken tok.span
  | [] => .parser .unexpectedEnd eof

private def runTasks {T N V : Type} [DecidableEq T] [DecidableEq N]
    (table : CheckedTable T N) (kind : Token → T) (leaf : Token → V)
    (reduce : N → List (Sym T N) → Span → List V → Except RefusalCause V) (eof : Span) :
    Nat → List (Task T N) → List Token → List V → Except RefusalCause V
  | _, [], [], [result] => .ok result
  | _, [], tok :: _, _ => .error (.parser .trailingTokens tok.span)
  | 0, _, input, _ => .error (.parser (.internal (.advance 0)) (lookSpan input eof))
  | fuel + 1, .read (.n lhs) :: pending, input, values => do
    let look := input.head?.map kind
    let some (_, rhs) := table.entries.find? (fun row => row.1 == (lhs, look))
      | throw (mismatch input eof)
    runTasks table kind leaf reduce eof fuel
      (rhs.map Task.read ++ .reduce lhs rhs (lookSpan input eof) :: pending) input values
  | fuel + 1, .read (.t expected) :: pending, input, values => do
    match input with
    | tok :: rest =>
      if kind tok == expected then
        runTasks table kind leaf reduce eof fuel pending rest (leaf tok :: values)
      else throw (mismatch input eof)
    | [] => throw (mismatch [] eof)
  | fuel + 1, .reduce lhs rhs start :: pending, input, values => do
    let value ← reduce lhs rhs start (values.take rhs.length).reverse
    runTasks table kind leaf reduce eof fuel pending input (value :: values.drop rhs.length)
  | _, [], [], values => .error (.parser (.internal (.finish values.length)) eof)

/-- Parse from the checked grammar's start symbol. Reductions retain their first lookahead span. -/
def run {T N V : Type} [DecidableEq T] [DecidableEq N]
    (table : CheckedTable T N) (kind : Token → T) (leaf : Token → V)
    (reduce : N → List (Sym T N) → Span → List V → Except RefusalCause V)
    (eof : Span) (fuel : Nat) (input : List Token) : Except RefusalCause V :=
  runTasks table kind leaf reduce eof fuel [.read (.n table.grammar.grammar.start)] input []

private inductive Value where
  | tok (token : Token)
  | expr (expr : Expr)
  | more (tail : List (Span × Expr))
  | body (statements : List Statement)
  | program (ast : Program)

private def reduce (start : Span) : Nonterminal → List Value → Except RefusalCause Value
  | .program, [_, .tok ⟨.name name, span⟩, _, _, _, _, _, .body statements, .tok close] =>
    .ok (.program ⟨name, span, statements, close.span⟩)
  | .body, [.tok keyword, .tok ⟨.name name, span⟩, _, _, _, .expr rhs, .tok semi, .body rest] =>
    .ok (.body (.bind ⟨name, span, rhs⟩ (statementSpan keyword.span semi.span) :: rest))
  | .body, [.tok keyword, .expr result, .tok semi, .body rest] =>
    .ok (.body (.ret result (statementSpan keyword.span semi.span) :: rest))
  | .body, [] => .ok (.body [])
  | .expr, [.expr head, .more tail] =>
    .ok (.expr (tail.foldl (fun lhs (span, rhs) => .add lhs rhs span) head))
  | .more, [.tok op, .expr rhs, .more tail] => .ok (.more ((op.span, rhs) :: tail))
  | .more, [] => .ok (.more [])
  | .atom, [.tok ⟨.integer value _ suffix, span⟩] => .ok (.expr (.integer value suffix span))
  | .atom, [.tok ⟨.name name, span⟩] => .ok (.expr (.local name span))
  | .atom, [_, .expr expr, _] => .ok (.expr expr)
  | _, _ => .error (.parser (.internal (.reduce false)) start)

private def refusalTable : CheckedTable Terminal Nonterminal :=
  ⟨⟨sharedGrammar, sharedGrammar_ll1⟩, makeTable sharedGrammar, rfl⟩

private def literalIssue (tok : Token) : Option RefusalCause :=
  match tok.value with
  | .integer _ radix _ =>
    letI : Decidable (ConcreteSpec.Core.Slice0Rules.DecimalLiteral radix) :=
      inferInstanceAs (Decidable (radix = .decimal))
    if ConcreteSpec.Core.Slice0Rules.DecimalLiteral radix then none else some (.parser .nonDecimal tok.span)
  | _ => none

private def recognizeProduction (lhs : Nonterminal) (rhs : List (Sym Terminal Nonterminal))
    (start : Span) (children : List (Option RefusalCause)) : Except RefusalCause (Option RefusalCause) := do
  let some outside := (projectedProductions .shared).lookup (lhs, rhs)
    | throw (.parser (.internal (.reduce false)) start)
  return if outside then some (.parser .outsideProduction start) else children.findSome? id

/-- Consume the entire refusal grammar before reporting its first unsupported production or literal.
A malformed suffix therefore takes precedence over an otherwise unsupported prefix. -/
def parse (eof : Span) (input : List Token) : Except RefusalCause Program := do
  -- The checked grammars have bounded work and reductions between terminal reads.
  match run table0 (·.value.kind) Value.tok (fun lhs _ start => reduce start lhs)
      eof (16 * (input.length + 1)) input with
  | .ok (.program ast) =>
    if let some issue := input.findSome? literalIssue then throw issue
    return ast
  | .ok _ => throw (.parser (.internal (.finish 0)) eof)
  | .error e =>
    match e with
    | .parser .unexpectedToken _ | .parser .unexpectedEnd _ | .parser .trailingTokens _ =>
      match ← run refusalTable (·.value.kind) literalIssue recognizeProduction
          eof (32 * (input.length + 1)) input with
      | some issue => throw issue
      | none => throw (.parser (.internal (.reduce false)) e.primary.surface)
    | _ => throw e

/-- The EOF of an in-memory source includes trailing trivia and belongs to the lexed file. -/
def eofSpan (source : ByteArray) (fileId : ConcreteSpec.Core.FileId := ⟨0⟩) : Span :=
  let pos := source.data.foldl (fun (p : Position) b => p.advanceByte b.toNat) {}
  Position.span pos pos fileId

/-- Inject the located parser error into the driver's refusal vocabulary. -/
def parseForDriver (eof : Span) (input : List Token) : Except Refusal Program :=
  (parse eof input).mapError Refusal.source

@[simp] theorem parseForDriver_ok {eof tokens ast} :
    parseForDriver eof tokens = .ok ast ↔ parse eof tokens = .ok ast := by
  unfold parseForDriver
  cases parse eof tokens <;> simp [Except.mapError]

end Concrete.Parse
