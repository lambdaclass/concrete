import ConcreteSpec.SurfacePrefix
import Concrete.Lex

namespace Concrete.Parse
open ConcreteSpec.Surface0 Concrete.LL1
open ConcreteSpec.StageInterface (RefusalCause)

export ConcreteSpec.SurfacePrefix (Table CheckedTable makeTable run)

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

open ConcreteSpec.StageInterface hiding Terminal
open ConcreteSpec.SurfacePrefix (literalIssue diagnose)

private def classify (eof : Span) (input : List Token) :
    Except (SurfacePreparation.Purpose → RefusalCause) Program :=
  match run table0 (·.value.kind) Value.tok (fun lhs _ start => reduce start lhs)
      eof (16 * (input.length + 1)) input with
  | .ok (.program ast) =>
    if (input.findSome? literalIssue).isNone then .ok ast
    else .error fun purpose => diagnose eof input purpose (.parser .unexpectedEnd eof)
  | .ok _ => .error fun _ => .parser (.internal (.finish 0)) eof
  | .error (.parser .unexpectedToken _) | .error (.parser .unexpectedEnd _)
  | .error (.parser .trailingTokens _) => .error fun purpose => diagnose eof input purpose (.parser .unexpectedEnd eof)
  | .error cause => .error fun _ => cause

/-- A failed parse diagnoses the recognized prefix in source order. -/
def parse (eof : Span) (input : List Token) : Except RefusalCause Program :=
  (classify eof input).mapError (· .admission)

/-- The EOF of an in-memory source includes trailing trivia and belongs to the lexed file. -/
def eofSpan (source : ByteArray) (fileId : ConcreteSpec.Core.FileId := ⟨0⟩) : Span :=
  let pos := source.data.foldl (fun (p : Position) b => p.advanceByte b.toNat) {}
  Position.span pos pos fileId

/-- The scanner owns the boundary. The parser can only replace it with a
judgment witnessed on the pieces the scanner completed before that boundary. -/
def lexForEntry (purpose : SurfacePreparation.Purpose) (source : ByteArray)
    (fileId : ConcreteSpec.Core.FileId) : Except Refusal (List Token) :=
  let result := (Lex.scanLossless source fileId).map Lex.LosslessStream.parserView
  result.mapError fun failure => match failure.refusal with
    | .source cause => .source (diagnose cause.primary.surface
        (failure.pieces.filterMap Lex.Piece.parserToken) purpose cause)
    | other => other

@[simp] theorem lexForEntry_ok {purpose source fileId tokens} :
    lexForEntry purpose source fileId = .ok tokens ↔ Lex.lex source fileId = .ok tokens := by
  unfold lexForEntry Lex.lex Lex.lexLossless
  cases Lex.scanLossless source fileId <;> simp [Except.map, Except.mapError]

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
