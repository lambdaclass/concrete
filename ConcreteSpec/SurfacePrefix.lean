import ConcreteSpec.StageInterface

namespace ConcreteSpec.SurfacePrefix
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

private inductive Task (T N : Type) where
  | read (symbol : Sym T N)
  | reduce (lhs : N) (rhs : List (Sym T N)) (start : Span)

private def lookSpan (input : List Token) (eof : Span) : Span :=
  input.head?.map Token.span |>.getD eof

private def mismatch (input : List Token) (endCause : RefusalCause) : RefusalCause :=
  match input with
  | tok :: _ => .parser .unexpectedToken tok.span
  | [] => endCause

private def runTasks {T N V : Type} [DecidableEq T] [DecidableEq N]
    (table : CheckedTable T N) (kind : Token → T) (leaf : Token → V)
    (reduce : N → List (Sym T N) → Span → List V → Except RefusalCause V)
    (halt : RefusalCause → List (Task T N) → List V → Nat × RefusalCause)
    (endCause : RefusalCause) (eof : Span) :
    Nat → List (Task T N) → List Token → List V → Nat → Nat × Except RefusalCause V
  | _, [], [], [result], steps => (steps + 1, .ok result)
  | _, [], tok :: _, values, steps => stop steps (.parser .trailingTokens tok.span) [] values
  | 0, _, input, _, steps => (steps + 1, .error (.parser (.internal (.advance 0)) (lookSpan input eof)))
  | fuel + 1, .read (.n lhs) :: pending, input, values, steps => Id.run do
    let look := input.head?.map kind
    let some (_, rhs) := table.entries.find? (fun row => row.1 == (lhs, look))
      | return stop steps (mismatch input endCause) (.read (.n lhs) :: pending) values
    return runTasks table kind leaf reduce halt endCause eof fuel
      (rhs.map Task.read ++ .reduce lhs rhs (lookSpan input eof) :: pending) input values (steps + 1)
  | fuel + 1, .read (.t expected) :: pending, input, values, steps =>
    match input with
    | tok :: rest =>
      if kind tok == expected then
        runTasks table kind leaf reduce halt endCause eof fuel pending rest (leaf tok :: values) (steps + 1)
      else stop steps (mismatch input endCause) (.read (.t expected) :: pending) values
    | [] => stop steps endCause (.read (.t expected) :: pending) values
  | fuel + 1, .reduce lhs rhs start :: pending, input, values, steps =>
    match reduce lhs rhs start (values.take rhs.length).reverse with
    | .error cause => (steps + 1, .error cause)
    | .ok value =>
      runTasks table kind leaf reduce halt endCause eof fuel pending input (value :: values.drop rhs.length) (steps + 1)
  | _, [], [], values, steps => (steps + 1, .error (.parser (.internal (.finish values.length)) eof))
where
  stop steps cause tasks values :=
    let (work, cause) := halt cause tasks values
    (steps + 1 + work, .error cause)

/-- Parse from the checked grammar's start symbol. Reductions retain their first lookahead span. -/
def run {T N V : Type} [DecidableEq T] [DecidableEq N]
    (table : CheckedTable T N) (kind : Token → T) (leaf : Token → V)
    (reduce : N → List (Sym T N) → Span → List V → Except RefusalCause V)
    (eof : Span) (fuel : Nat) (input : List Token) : Except RefusalCause V :=
  (runTasks table kind leaf reduce (fun cause _ _ => (0, cause)) (.parser .unexpectedEnd eof) eof
    fuel [.read (.n table.grammar.grammar.start)] input [] 0).2

private def refusalTable : CheckedTable Terminal Nonterminal :=
  ⟨⟨sharedGrammar, sharedGrammar_ll1⟩, makeTable sharedGrammar, rfl⟩

def literalIssue (tok : Token) : Option RefusalCause :=
  match tok.value with
  | .integer _ radix _ =>
    letI : Decidable (ConcreteSpec.Core.Slice0Rules.DecimalLiteral radix) :=
      inferInstanceAs (Decidable (radix = .decimal))
    if ConcreteSpec.Core.Slice0Rules.DecimalLiteral radix then none else some (.parser .nonDecimal tok.span)
  | _ => none

open ConcreteSpec.StageInterface hiding Terminal

/-- A shared production projects either its Slice 0 actions or a stop at the
first symbol outside Slice 0. On a mismatch, open productions retain their already matched children. -/
private inductive Projection where
  | token (t : Token)
  | gap (cause : RefusalCause)
  | typedTail (typeError : Option RefusalCause)
  | endPart
  | stop (cause : RefusalCause)
  | head (name : String) (span : Span) (typeError : Option RefusalCause)
  | expr (value : SurfacePreparation.ExprAction)
  | tail (apply : SurfacePreparation.ExprAction → SurfacePreparation.ExprAction)
  | body (value : SurfacePreparation.BodyAction)
  | program (run : SurfacePreparation.Purpose → RefusalCause → Except RefusalCause ConcreteSpec.StageInterface.EarlyProgram)

private def Projection.asExpr : Projection → Option SurfacePreparation.ExprAction
  | .expr value => some value
  | .stop cause | .gap cause => some (SurfacePreparation.blockedExpr cause)
  | _ => none

private def Projection.asTail : Projection → Option (SurfacePreparation.ExprAction → SurfacePreparation.ExprAction)
  | .tail apply => some apply
  | .stop cause | .gap cause => some (fun left => SurfacePreparation.afterExpr left cause)
  | _ => none

private def Projection.asBody : Projection → Option SurfacePreparation.BodyAction
  | .body action => some action
  | .gap cause => some ⟨none, (SurfacePreparation.blockedBody cause cause.primary.surface).run⟩
  | .stop cause => some (SurfacePreparation.blockedBody cause cause.primary.surface)
  | _ => none

private def Projection.part : Projection → Option (Option RefusalCause)
  | .endPart => some none
  | .stop cause | .gap cause => some (some cause)
  | _ => none

private def project (lhs : Nonterminal) (start : Span) : List Projection → Option Projection
  | [.endPart, _, .token ⟨.name name, span⟩, openP, closeP, arrow, result, brace, body, _] => do
    let body ← body.asBody
    let headerStop := [openP, closeP, arrow, result, brace].findSome? fun
      | .gap c | .stop c => some c
      | _ => none
    let body := headerStop.elim body (fun c => SurfacePreparation.blockedBody c c.primary.surface)
    return .program fun purpose boundary => SurfacePreparation.entry purpose name span body (some boundary)
  | [.stop cause, _, _, _, _, _, _, _, _, _] => some (.program (fun _ _ => .error cause))
  | [.token kw, h, equals, e, semi, rest] => do
    let span := match semi with
      | .token t => statementSpan kw.span t.span
      | _ => kw.span
    match h with
    | .stop cause | .gap cause => return .body (SurfacePreparation.blockedBody cause span)
    | .head name binder typeError =>
      let rhs ← e.asExpr
      let rhs := match equals with
        | .gap c => SurfacePreparation.blockedExpr c
        | _ => rhs
      let rhs := typeError.elim rhs SurfacePreparation.blockedExpr
      let rhs := match semi with
        | .gap c => SurfacePreparation.afterExpr rhs c
        | _ => rhs
      return .body (SurfacePreparation.bindBody name binder span rhs (← rest.asBody))
    | _ => none
  | [.token kw, e, semi, rest] => do
    let rhs ← e.asExpr
    match semi with
    | .token t =>
      let span := statementSpan kw.span t.span
      return .body (SurfacePreparation.returnBody rhs span (← rest.asBody))
    | .gap c =>
      return .body (SurfacePreparation.returnBody (SurfacePreparation.afterExpr rhs c) kw.span (← rest.asBody))
    | _ => none
  | [.stop cause, _] => if lhs = .body then some (.body (SurfacePreparation.blockedBody cause start)) else
      if lhs = .product || lhs = .expr then some (.expr (SurfacePreparation.blockedExpr cause)) else some (.stop cause)
  | [.token ⟨.fixed .lparen _, _⟩, e, close] => do
    let expr ← e.asExpr
    return .expr (match close with
      | .gap c => SurfacePreparation.afterExpr expr c
      | _ => expr)
  | [.token op, e, tail] => do
    let rhs ← e.asExpr
    let apply ← tail.asTail
    return .tail fun left => apply (SurfacePreparation.add left rhs op.span)
  | [.token ⟨.name name, span⟩, .typedTail typeError] => some (.head name span typeError)
  | [.token ⟨.name name, span⟩, part] => do
    if lhs = .letHead then return part
    let cause ← part.part
    return .expr (cause.elim (SurfacePreparation.readLocal name span) SurfacePreparation.blockedExpr)
  | [.token ⟨.fixed .colon _, _⟩, part] => return .typedTail (← part.part)
  | [.token _, part] => some part
  | [.gap cause, _] => some (.gap cause)
  | [e, tail] => do return .expr ((← tail.asTail) (← e.asExpr))
  | [.token (tok@⟨.integer n _ suffix, span⟩)] =>
    some (.expr ((literalIssue tok).elim (SurfacePreparation.integer n suffix span) SurfacePreparation.blockedExpr))
  | [.token _] => some .endPart
  | [] => some (match lhs with
    | .body => .body SurfacePreparation.endBody
    | .more | .factors => .tail id
    | _ => .endPart)
  | parts => parts.find? fun | .gap _ => true | _ => false

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

private def evaluate (purpose : SurfacePreparation.Purpose) (fallback : RefusalCause) :
    Projection → RefusalCause
  | .program action => match action purpose fallback with
    | .error cause => cause
    | .ok _ => fallback
  | .gap c | .stop c => c
  | _ => fallback

/-- Close only productions the predictive machine actually entered. Each unread
symbol is a boundary carrying the original failure, never a synthetic token. -/
private def finishPrefix (purpose : SurfacePreparation.Purpose) (cause : RefusalCause) :
    List (Task Terminal Nonterminal) → List Projection → Nat → Nat × RefusalCause
  | [], [result], steps => (steps + 1, evaluate purpose cause result)
  | .read _ :: tasks, values, steps => finishPrefix purpose cause tasks (.gap cause :: values) (steps + 1)
  | .reduce lhs rhs start :: tasks, values, steps =>
    let (children, rest) := values.splitAt rhs.length
    match recognizeProduction lhs rhs start children.reverse with
    | .ok value => finishPrefix purpose cause tasks (value :: rest) (steps + 1)
    | .error internal => (steps + 1, internal)
  | [], _, steps => (steps + 1, .parser (.internal (.finish 0)) cause.primary.surface)

/-- Select the first failed judgment of the actual token prefix. The boundary
is EOF on complete input and the scanner's own cause on an interrupted stream. -/
def diagnoseCounted (eof : Span) (input : List Token) (purpose : SurfacePreparation.Purpose)
    (boundary : RefusalCause) (onComplete : RefusalCause := boundary) : Nat × RefusalCause :=
  let (steps, result) := runTasks refusalTable (·.value.kind) Projection.token recognizeProduction
      (fun c tasks values => finishPrefix purpose c tasks values 0) boundary eof (32 * (input.length + 1))
      [.read (.n .program)] input [] 0
  (steps, match result with
    | .error cause => cause
    | .ok value => evaluate purpose onComplete value)

def diagnose (eof : Span) (input : List Token) (purpose : SurfacePreparation.Purpose)
    (boundary : RefusalCause) : RefusalCause :=
  (diagnoseCounted eof input purpose boundary).2

end ConcreteSpec.SurfacePrefix
