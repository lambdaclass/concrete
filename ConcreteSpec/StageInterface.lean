import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Own
import ConcreteSpec.Spec.Observation
import ConcreteSpec.Spec.Surface0

/-! Spec-owned data and signatures at the compiler boundary. -/
namespace ConcreteSpec.StageInterface

open Core Core.SourcePathSpan

abbrev Slice := Fin 22

structure FileId where
  index : Nat
  deriving DecidableEq, Repr

/-- Coordinates use Core.Span's half-open byte range and 1-based line and column. -/
structure FileSpan where
  file : FileId
  startOffset : Nat
  endOffset : Nat
  line : Nat
  col : Nat
  deriving DecidableEq, Repr

def FileSpan.Binds (span : FileSpan) (project : Project) (specSpan : Span) : Prop :=
  ∃ file, project.files[span.file.index]? = some file ∧
    SourcePathSpan.Reference.Binds project specSpan file ∧ specSpan.LocationOk file.source ∧
    span.startOffset = specSpan.startOffset ∧ span.endOffset = specSpan.endOffset ∧
    span.line = specSpan.line ∧ span.col = specSpan.col

structure Source where
  project : Project
  entryFile : FileId
  slice : Slice

structure Located (A : Type) where
  span : FileSpan
  value : A

abbrev Terminal := ConcreteSpec.Surface0.Terminal

def Terminal.HasPayload : Terminal → Bool
  | .ident | .integer | .stringLiteral | .byteLiteral | .label => true
  | _ => false

/-- The raw spelling lives in the span. These are the decoded values used by actions. -/
inductive TokenValue where
  | fixed (kind : Terminal) (plain : Terminal.HasPayload kind = false)
  | name (text : String)
  | integer (value : Nat) (radix : ConcreteSpec.Surface0.Radix) (suffix : Option IntTy)
  | stringLiteral (bytes : List UInt8)
  | byteLiteral (byte : UInt8)
  | label (name : Name)
  deriving DecidableEq, Repr

def TokenValue.kind : TokenValue → Terminal
  | .fixed kind _ => kind
  | .name _ => .ident
  | .integer .. => .integer
  | .stringLiteral _ => .stringLiteral
  | .byteLiteral _ => .byteLiteral
  | .label _ => .label

structure TokenStream where
  source : Source
  tokens : Array (Located TokenValue)
  /-- Trivia preceding each token, followed by the EOF trivia. -/
  trivia : Array (Array FileSpan)

structure ParserView where
  source : Source
  tokens : Array (Located TokenValue)

private def sourcePositions (source : SourceText) : Array (Nat × Nat) :=
  (source.bytes.foldl (fun (positions, line, column) byte =>
    let next := if byte == 10 then (line + 1, 1) else (line, column + 1)
    (positions.push next, next)) (#[ (1, 1) ], 1, 1)).1

/-- Validate source coordinates before grammar execution. Build an indexed position
table in one pass over the project bytes, including the boundary at EOF. -/
def ParserView.invalidSpan (input : ParserView) : Option FileSpan :=
  let files := (input.source.project.files.map (sourcePositions ∘ File.source)).toArray
  if files[input.source.entryFile.index]?.isNone then
    some ⟨input.source.entryFile, 0, 0, 1, 1⟩
  else
    (input.tokens.find? fun token =>
      let span := token.span
      match files[span.file.index]? with
      | none => true
      | some positions => !(span.startOffset ≤ span.endOffset && span.endOffset < positions.size &&
          positions[span.startOffset]? == some (span.line, span.col))).map Located.span

def ParserView.WF (input : ParserView) : Prop :=
  input.invalidSpan = none

def TokenStream.parserView (stream : TokenStream) : ParserView :=
  ⟨stream.source, stream.tokens⟩

/-- AST is the result of the selected typed start action of the shared grammar.
Later slices supply larger action families through the same handoff. -/
structure Parsed (Ast : Type) where
  source : Source
  tree : Ast

inductive ParseRefusal where
  | invalidStream (atSpan : FileSpan)
  | malformed (rule : RuleId) (atSpan : FileSpan)
  | unsupported (rule : RuleId) (atSpan : FileSpan) (required : Slice)

structure Origin where
  operation : SiteId
  lexical : FileSpan
  evidenceKey : StableKey
  deriving DecidableEq, Repr

structure ResourceRefusal where
  rule : RuleId
  measured : Nat
  limit : Nat

structure CheckerRefusal where
  rule : RuleId
  primary : FileSpan
  related : List (Name × FileSpan)
  obligation : Option ObligationId

/-- Canonical identity of an ordered operand, shared with typed MIR reads.
Literal bytes are the integrated Core instance's canonical value encoding. -/
inductive OperandRead where
  | literal (encoding : List UInt8)
  | copy (place : PlaceId) (fields : List FieldId)
  | move (place : PlaceId) (fields : List FieldId)
  deriving DecidableEq

/-- The active syntax at an operation's evaluation point. Deferred calls and loop
tests are inspected when evaluated, rather than when their frames are installed. -/
inductive CoreOperation (O : Exec.Operands) where
  | expression (expr : O.Expr)
  | call (callee : FnId) (args : List O.Expr)
  | primitive (primitive : O.Prim) (args : List O.Expr)
  | abort

/-- The semantic operation survives instruction selection. Calls carry their actual callee. -/
inductive Operator where
  | read
  | arithmetic (op : MachineOp)
  | named (operation : Name)
  deriving DecidableEq

inductive Opcode where
  | operator (operation : Operator)
  | call (callee : FnId)
  deriving DecidableEq

structure SafetyCondition (Ty : Type) where
  discriminator : Nat
  trap : TrapKind
  operands : List (Ty × OperandRead)

/-- Every integrated instance shares the scalar representation. Aggregates,
references, and other values occupy the instance-specific arm. -/
inductive CoreValue (Other : Type) where
  | scalar (value : CVal)
  | other (value : Other)

/-- The operand family is supplied by ConcreteSpec. Control, calls and scoped lifetime use Exec. -/
structure CoreLanguage where
  operands : Exec.Operands
  hybridStore : operands.Store = Hybrid.State
  Declaration : Type
  /-- Exact ordered operand reads and trap conditions of the integrated spec operation. -/
  expressionOperator : operands.Expr → Operator
  primitiveOperator : operands.Prim → Operator
  /-- Arguments are already evaluated reads. A checked computation occurs in its own
  Core statement before the call, with its own site and result local. -/
  argumentRead : operands.Expr → Option (operands.Ty × OperandRead)
  argumentReadNoTrap : ∀ expr, (argumentRead expr).isSome →
    ∀ locals store kind, operands.eval locals store expr ≠ .trap kind
  OtherValue : Type
  valueShape : operands.Val = CoreValue OtherValue
  safety : CoreOperation operands → List (SafetyCondition operands.Ty)
  permit : AList Var Own.Descriptor → AList Var operands.Val → operands.Expr →
    Own.State → Option Own.State

def CoreLanguage.opcode (language : CoreLanguage) : CoreOperation language.operands → Opcode
  | .expression expr => .operator (language.expressionOperator expr)
  | .primitive primitive _ => .operator (language.primitiveOperator primitive)
  | .call callee _ => .call callee
  | .abort => .operator (.named ⟨"abort"⟩)

inductive HostedResult where
  | returned (status : UInt8)
  | trapped (status : UInt8) (kind : TrapKind) (site : SiteId)

/-- Hosted entry returns a u8. Normal statement completion and non-u8 results
have no hosted observation. -/
def CoreLanguage.hostedResult (language : CoreLanguage) :
    Exec.Final language.operands.Val → Option HostedResult
  | .normal => none
  | .returned value => do
    let .scalar (.int .u8 n) := language.valueShape ▸ value | none
    if 0 ≤ n ∧ n < 256 then
      some (.returned (UInt8.ofNat (Observe.Observation.exited n.toNat).status))
    else none
  | .trapped kind site =>
    some (.trapped (UInt8.ofNat (Observe.Observation.aborted site).status) kind site)

abbrev CoreLanguage.instrumented (language : CoreLanguage)
    (ownership : AList Var Own.Descriptor) : Exec.Operands :=
  Own.instrumentOperands language.operands (AList.lookup ownership) (language.permit ownership)

/-- The early fragment uses the common scalar vocabulary and nominal identities.
Declaration shape, Copy and fragment eligibility are checked by the spec. -/
inductive EarlyTy where
  | scalar (ty : CTy)
  | nominal (decl : DeclId)
  deriving DecidableEq

abbrev EarlyValue := CoreValue DeclId

inductive EarlyOperand where
  | literal (value : CVal)
  | copy (variableId : Var)
  | move (variableId : Var)

inductive EarlyExpr where
  | read (value : EarlyOperand)
  | add (ty : IntTy) (left right : EarlyOperand)
  | constructEmpty (decl : DeclId)

inductive EarlyPrim where
  | destructureEmpty (decl : DeclId)

abbrev EarlyDeclaration := Unit

structure EarlyCore (language : CoreLanguage) where
  values : language.operands.Val = EarlyValue
  types : language.operands.Ty = EarlyTy
  expressions : language.operands.Expr = EarlyExpr
  primitives : language.operands.Prim = EarlyPrim
  declarations : language.Declaration = EarlyDeclaration

structure Payload (language : CoreLanguage) where
  ownership : AList Var Own.Descriptor
  body : Exec.Program (language.instrumented ownership)
  entry : FnId
  origins : AList SiteId Origin
  declarations : AList DeclId language.Declaration

abbrev Payload.machine {language : CoreLanguage} (payload : Payload language) : Exec.Operands :=
  language.instrumented payload.ownership

def CoreLanguage.emptyStore (language : CoreLanguage) : Own.Store language.operands.Store :=
  ⟨language.hybridStore.symm ▸ (⟨[], Mem.Memory.empty⟩ : Hybrid.State), {}⟩

/-- Hosted Core starts at the program's entry with no language arguments,
cells or outstanding loans. -/
def Payload.initial? {language : CoreLanguage} (payload : Payload language) :
    Option (Exec.Config payload.machine) := do
  let fn ← AList.lookup payload.body.fns payload.entry
  if fn.params.isEmpty then Exec.enter fn [] language.emptyStore else none

def Payload.initial {language : CoreLanguage} (payload : Payload language) :
    Exec.Config payload.machine :=
  payload.initial?.getD ⟨.refused, [], language.emptyStore, []⟩

def stmtOperations {O : Exec.Operands} : Exec.Stmt O → List (SiteId × CoreOperation O)
  | .bind _ e site | .assign _ e site | .ret e site => [(site, .expression e)]
  | .cond e yes no site => (site, .expression e) :: stmtOperations yes ++ stmtOperations no
  | .loop _ cond test body site => (site, .expression test) :: stmtOperations cond ++ stmtOperations body
  | .call _ fn args site | .defer fn args site => [(site, .call fn args)]
  | .prim _ op args site => [(site, .primitive op args)]
  | .seq first rest => stmtOperations first ++ stmtOperations rest
  | .block _ body => stmtOperations body
  | .abort site => [(site, .abort)]
  | _ => []

def Payload.operations {language : CoreLanguage} (payload : Payload language) :
    List (SiteId × CoreOperation language.operands) :=
  payload.body.fns.flatMap fun (_, fn) =>
    (stmtOperations fn.body).map fun (site, op) => (site, match op with
      | .expression e => .expression e
      | .call fn args => .call fn args
      | .primitive p args => .primitive p args
      | .abort => .abort)

def Payload.activeOperation {language : CoreLanguage} (payload : Payload language)
    (state : Exec.Config payload.machine) : Option (SiteId × CoreOperation language.operands) :=
  match state.ctrl, state.frames with
  | .exec (.bind _ e site), _ | .exec (.assign _ e site), _ | .exec (.ret e site), _
  | .exec (.cond e _ _ site), _ | .done, .test _ _ e _ site _ :: _ =>
    some (site, .expression e)
  | .exec (.call _ fn args site), _ => some (site, .call fn args)
  | .exec (.prim _ p args site), _ => some (site, .primitive p args)
  | _, _ => none

structure CoreInput (language : CoreLanguage) where
  project : Project
  slice : Slice
  payload : Payload language

def CoreInput.ArgumentsWF {language : CoreLanguage} (input : CoreInput language) : Prop :=
  ∀ site op, (site, op) ∈ input.payload.operations →
    match op with
    | .call _ args | .primitive _ args => ∀ expr ∈ args, (language.argumentRead expr).isSome
    | _ => True

def CoreInput.LocationsWF {language : CoreLanguage} (input : CoreInput language) : Prop :=
  (List.map Prod.fst input.payload.origins).Nodup ∧
  (List.map Prod.fst input.payload.operations).Nodup ∧
  (List.map (fun (entry : SiteId × Origin) => entry.2.evidenceKey) input.payload.origins).Nodup ∧
  (∀ site op, (site, op) ∈ input.payload.operations →
    ∃ origin, AList.lookup input.payload.origins site = some origin) ∧
  (∀ state events kind site, Exec.Steps input.payload.body input.payload.initial events state →
    state.final? = some (.trapped kind site) →
    ∃ origin, AList.lookup input.payload.origins site = some origin) ∧
  ∀ (site : SiteId) (origin : Origin), (site, origin) ∈ input.payload.origins →
    site = origin.operation ∧ ∃ span, origin.lexical.Binds input.project span

def CoreInput.WF {language : CoreLanguage} (input : CoreInput language) : Prop :=
  input.LocationsWF ∧ input.ArgumentsWF

/-- A discharge identifies an exact operation, ordered typed operands and obligation.
The enclosing set belongs to the exact Core payload and its checking context. -/
structure Discharge (language : CoreLanguage) where
  operation : SiteId
  key : StableKey
  obligation : ObligationId
  trap : TrapKind
  operands : List (language.operands.Ty × OperandRead)

/-- An operation can have several independent conditions, such as divisor nonzero and
the signed division overflow guard. Each obligation appears once. -/
abbrev Discharges (language : CoreLanguage) := AList SiteId (List (Discharge language))

def Discharges.Contains {language : CoreLanguage} (discharges : Discharges language)
    (site : SiteId) (d : Discharge language) : Prop :=
  ∃ group, (site, group) ∈ discharges ∧ d ∈ group

structure Admitted (language : CoreLanguage) where
  core : CoreInput language
  discharges : Discharges language

/-- The spec owns the fragment, independent statics, acceptance, and operation safety judgments.
The checker is the single executable traversal. -/
structure Admission (language : CoreLanguage) where
  fragment : Slice → Payload language → Prop
  statics : CoreInput language → Prop
  accepted : CoreInput language → Prop
  rejects : CoreInput language → CheckerRefusal → Prop
  check : CoreInput language → Except CheckerRefusal (Discharges language)

def Payload.ConditionsAt {language : CoreLanguage} (payload : Payload language) (site : SiteId)
    (opcode : Opcode) (conditions : List (SafetyCondition language.operands.Ty)) : Prop :=
  ∃ op, (site, op) ∈ payload.operations ∧ language.opcode op = opcode ∧
    conditions = language.safety op

def Discharge.condition {language : CoreLanguage} (d : Discharge language) :
    SafetyCondition language.operands.Ty :=
  ⟨d.obligation.discriminator, d.trap, d.operands⟩

def ValidDischarges {language : CoreLanguage} (input : CoreInput language)
    (discharges : Discharges language) : Prop :=
  input.payload.initial?.isSome = true ∧
  (List.map Prod.fst discharges).Nodup ∧
  (∀ site group, (site, group) ∈ discharges →
    group ≠ [] ∧ (group.map Discharge.obligation).Nodup) ∧
  ∀ (site : SiteId) (d : Discharge language), discharges.Contains site d →
    site = d.operation ∧ d.obligation.site = site ∧ d.obligation.kind = .safety ∧
    (∃ opcode conditions, input.payload.ConditionsAt site opcode conditions ∧ d.condition ∈ conditions) ∧
    (∃ origin, (site, origin) ∈ input.payload.origins ∧ origin.evidenceKey = d.key) ∧
    ∀ state events op, Exec.Steps input.payload.body input.payload.initial events state →
      input.payload.activeOperation state = some (site, op) →
      (∃ label next, Exec.Step input.payload.body state label next) ∧
      (∀ label next, Exec.Step input.payload.body state label next →
        next.ctrl ≠ .trapped d.trap site)

def Admission.Sound {language : CoreLanguage} (spec : Admission language) : Prop :=
  ∀ input, input.WF → spec.fragment input.slice input.payload →
    (spec.accepted input ↔ ∃ discharges, spec.check input = .ok discharges) ∧
    (∀ discharges, spec.check input = .ok discharges →
      spec.statics input ∧ ValidDischarges input discharges) ∧
    (∀ refusal, spec.check input = .error refusal → spec.rejects input refusal)

/-- Layout is a spec function at the resolved target. A zero extent still has logical lifetime. -/
structure Layout where
  size : Nat
  align : Nat
  deriving DecidableEq, Repr

abbrev LayoutOf (Ty : Type) := Target → Ty → Option Layout

end ConcreteSpec.StageInterface
