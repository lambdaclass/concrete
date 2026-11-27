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
  | .program, [_, .tok ⟨.name name, span⟩, _, _, _, _, _, .body statements, _] =>
    .ok (.program ⟨name, span, statements⟩)
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

open ConcreteSpec.StageInterface hiding Terminal


/-- A shared production projects either its Slice 0 actions or a stop at the
first symbol outside Slice 0. Actions are evaluated only for a complete parse. -/
private inductive Projection where
  | token (t : Token)
  | endPart
  | stop (cause : RefusalCause)
  | head (name : String) (span : Span) (typeError : Option RefusalCause)
  | expr (value : SurfacePreparation.ExprAction)
  | tail (apply : SurfacePreparation.ExprAction → SurfacePreparation.ExprAction)
  | body (value : SurfacePreparation.BodyAction)
  | program (run : SurfacePreparation.Purpose → Except RefusalCause ConcreteSpec.StageInterface.EarlyProgram)

private def Projection.asExpr : Projection → Option SurfacePreparation.ExprAction
  | .expr value => some value
  | .stop cause => some (SurfacePreparation.blockedExpr cause)
  | _ => none

private def Projection.asTail : Projection → Option (SurfacePreparation.ExprAction → SurfacePreparation.ExprAction)
  | .tail apply => some apply
  | .stop cause => some (fun left => SurfacePreparation.afterExpr left cause)
  | _ => none

private def Projection.asBody (start : Span) : Projection → Option SurfacePreparation.BodyAction
  | .body action => some action
  | .stop cause => some (SurfacePreparation.blockedBody cause start)
  | _ => none

private def Projection.part : Projection → Option (Option RefusalCause)
  | .endPart => some none
  | .stop cause => some (some cause)
  | _ => none

private def project (lhs : Nonterminal) (start : Span) : List Projection → Option Projection
  | [.endPart, _, .token ⟨.name name, span⟩, _, _, _, result, _, body, _] => do
    let result ← result.part
    let body ← body.asBody start
    let body := result.elim body (fun c => SurfacePreparation.blockedBody c c.primary.surface)
    return .program fun purpose => SurfacePreparation.entry purpose name span body
  | [.stop cause, _, _, _, _, _, _, _, _, _] => some (.program (fun _ => .error cause))
  | [.token kw, h, _, e, .token semi, rest] => do
    let span := statementSpan kw.span semi.span
    let rest ← rest.asBody semi.span
    match h with
    | .stop cause => return .body (SurfacePreparation.blockedBody cause span)
    | .head name binder typeError =>
      let rhs ← e.asExpr
      return .body (SurfacePreparation.bindBody name binder span (typeError.elim rhs SurfacePreparation.blockedExpr) rest)
    | _ => none
  | [.token kw, e, .token semi, rest] => do
    return .body (SurfacePreparation.returnBody (← e.asExpr) (statementSpan kw.span semi.span) (← rest.asBody semi.span))
  | [.stop cause, _] => if lhs = .body then some (.body (SurfacePreparation.blockedBody cause start)) else
      if lhs = .product || lhs = .expr then some (.expr (SurfacePreparation.blockedExpr cause)) else some (.stop cause)
  | [.token _, e, .token _] => return .expr (← e.asExpr)
  | [.token op, e, tail] => do
    let rhs ← e.asExpr
    let apply ← tail.asTail
    return .tail fun left => apply (SurfacePreparation.add left rhs op.span)
  | [.token ⟨.name name, span⟩, part] => do
    let cause ← part.part
    if lhs = .letHead then return .head name span cause
    return .expr (cause.elim (SurfacePreparation.readLocal name span) SurfacePreparation.blockedExpr)
  | [.token _, part] => some part
  | [e, tail] => do return .expr ((← tail.asTail) (← e.asExpr))
  | [.token (tok@⟨.integer n _ suffix, span⟩)] =>
    some (.expr ((literalIssue tok).elim (SurfacePreparation.integer n suffix span) SurfacePreparation.blockedExpr))
  | [.token _] => some .endPart
  | [] => some (match lhs with
    | .body => .body SurfacePreparation.endBody
    | .more | .factors => .tail id
    | _ => .endPart)
  | _ => none

private def recognizeProduction (lhs : Nonterminal) (rhs : List (Sym Terminal Nonterminal))
    (start : Span) (children : List Projection) : Except RefusalCause Projection := do
  let mismatch := RefusalCause.parser (.internal (.reduce false)) start
  let some outside := (projectedProductions .shared).lookup (lhs, rhs) | throw mismatch
  if outside then
    let cause := RefusalCause.parser .outsideProduction start
    if lhs = .body then
      if let [_, _, _, .token semi, _] := children then
        return .body (SurfacePreparation.blockedBody cause (statementSpan start semi.span))
    return .stop cause
  -- The head and alternatives of an unsupported conditional never execute in Slice 0.
  if [Nonterminal.headExpr, .headMore, .headAtom, .alternative, .elseBody, .headProduct, .headFactors].contains lhs then
    return .endPart
  let some value := project lhs start children | throw mismatch
  return value

private def diagnoseShared (eof : Span) (input : List Token) (purpose : SurfacePreparation.Purpose) :
    RefusalCause :=
  match run refusalTable (·.value.kind) Projection.token recognizeProduction
      eof (32 * (input.length + 1)) input with
  | .error cause => cause
  | .ok (.program actions) => match actions purpose with
    | .error cause => cause
    | .ok _ => .parser (.internal (.reduce false)) eof
  | .ok _ => .parser (.internal (.finish 0)) eof

private def classify (eof : Span) (input : List Token) :
    Except (SurfacePreparation.Purpose → RefusalCause) Program :=
  match run table0 (·.value.kind) Value.tok (fun lhs _ start => reduce start lhs)
      eof (16 * (input.length + 1)) input with
  | .ok (.program ast) =>
    if (input.findSome? literalIssue).isNone then .ok ast else .error (diagnoseShared eof input)
  | .ok _ => .error fun _ => .parser (.internal (.finish 0)) eof
  | .error (.parser .unexpectedToken _) | .error (.parser .unexpectedEnd _)
  | .error (.parser .trailingTokens _) => .error (diagnoseShared eof input)
  | .error cause => .error fun _ => cause

/-- Syntactic validity qualifies the complete input. On recognized wider syntax,
walk the admitted prefix in source order up to its first failed judgment. -/
def parse (eof : Span) (input : List Token) : Except RefusalCause Program :=
  (classify eof input).mapError (· .admission)

/-- The EOF of an in-memory source includes trailing trivia and belongs to the lexed file. -/
def eofSpan (source : ByteArray) (fileId : ConcreteSpec.Core.FileId := ⟨0⟩) : Span :=
  let pos := source.data.foldl (fun (p : Position) b => p.advanceByte b.toNat) {}
  Position.span pos pos fileId

/-- Successful syntax is shared by checked and unchecked compilation. Only
arithmetic evidence in a refused input's semantic prefix depends on the entry. -/
def parseForEntry (purpose : SurfacePreparation.Purpose) (eof : Span) (input : List Token) :
    Except Refusal Program :=
  (classify eof input).mapError fun report => .source (report purpose)

abbrev parseForDriver := parseForEntry .admission

@[simp] theorem parseForEntry_ok {purpose eof tokens ast} :
    parseForEntry purpose eof tokens = .ok ast ↔ parse eof tokens = .ok ast := by
  unfold parseForEntry parse
  cases classify eof tokens <;> simp [Except.mapError]

theorem parseForDriver_ok {eof tokens ast} :
    parseForDriver eof tokens = .ok ast ↔ parse eof tokens = .ok ast := parseForEntry_ok

end Concrete.Parse
