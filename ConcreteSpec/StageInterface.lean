import ConcreteSpec.Spec.Slice0Layout
import ConcreteSpec.Spec.Slice0Syntax
import ConcreteSpec.Spec.Slice1Checker
import ConcreteSpec.Admission.Statics
import ConcreteSpec.Spec.Rules
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Own
import ConcreteSpec.Spec.Observation
import ConcreteSpec.Spec.Surface0

/-! Spec-owned data and signatures at the compiler boundary. -/
namespace ConcreteSpec.StageInterface

open Core Core.SourcePathSpan

abbrev Slice := Core.SliceId

def Slice.val (slice : Slice) : Nat := slice.n.val

def slice0 : Slice := ⟨⟨0, by decide⟩⟩

abbrev FileId := Core.FileId

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
    span.file = specSpan.fileId ∧ specSpan.LocationOk file.source ∧
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

/-- The decoded values used by grammar actions. -/
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

inductive SurfaceNode where
  | program | body | binding | return_ | add
  | extension (tag : Nat)

inductive SyntaxTree where
  | node (kind : SurfaceNode) (fields : List SyntaxTree)
  | leaf (token : Located TokenValue)

abbrev SurfaceAST (Extra : Slice → Type) (slice : Slice) :=
  SyntaxTree ⊕ Extra slice

abbrev RawPiece := Located ByteArray

structure RetainedToken where
  decoded : Located TokenValue
  spelling : ByteArray

structure TokenStream where
  source : Source
  tokens : Array RetainedToken
  /-- Trivia preceding each token, followed by the EOF trivia. -/
  trivia : Array (Array RawPiece)

/-- Reconstruct from retained pieces only. Each piece must begin at the
current byte offset and cover exactly the bytes it retains. -/
def reconstruct (entry : FileId) (tokens : Array RetainedToken)
    (trivia : Array (Array RawPiece)) : Option ByteArray := do
  if trivia.size != tokens.size + 1 then none else
  let append := fun ((bytes, position) : ByteArray × Position) (piece : RawPiece) =>
    if piece.span.file == entry && piece.span.startOffset == position.byte &&
        piece.span.endOffset == position.byte + piece.value.size && piece.value.size > 0 &&
        piece.span.line == position.line && piece.span.col == position.column then
      some (bytes ++ piece.value, piece.value.data.foldl
        (fun pos byte => pos.advanceByte byte.toNat) position)
    else none
  let state ← (List.range tokens.size).foldlM (fun state index => do
    let before ← trivia[index]?
    let state ← before.foldlM append state
    let token ← tokens[index]?
    append state ⟨token.decoded.span, token.spelling⟩) (ByteArray.empty, {})
  let trailing ← trivia[tokens.size]?
  return (← trailing.foldlM append state).1

structure ParserView where
  source : Source
  tokens : Array (Located TokenValue)

private def sourcePositions (source : SourceText) : Array (Nat × Nat) :=
  ((source.bytes.scanl Position.advanceByte {}).map fun pos => (pos.line, pos.column)).toArray

/-- Validate every token and return the entry file's EOF from the same coordinate
table. Even an empty file has its initial position in the table. -/
def ParserView.validateCoordinates (input : ParserView) : Except FileSpan FileSpan := do
  let files := (input.source.project.files.map (sourcePositions ∘ File.source)).toArray
  let entry := input.source.entryFile
  let missing : FileSpan := ⟨entry, 0, 0, 1, 1⟩
  let some positions := files[entry.index]? | throw missing
  let some (line, col) := positions.back? | throw missing
  if let some token := input.tokens.find? (fun token =>
      let span := token.span
      match files[span.file.index]? with
      | none => true
      | some table => !(span.startOffset ≤ span.endOffset && span.endOffset < table.size &&
          table[span.startOffset]? == some (span.line, span.col))) then
    throw token.span
  let endOffset := positions.size - 1
  return ⟨entry, endOffset, endOffset, line, col⟩

def ParserView.invalidSpan (input : ParserView) : Option FileSpan :=
  match input.validateCoordinates with
  | .error span => some span
  | .ok _ => none

def ParserView.WF (input : ParserView) : Prop :=
  input.invalidSpan = none

def TokenStream.parserView (stream : TokenStream) : ParserView :=
  ⟨stream.source, (stream.tokens.toList.map RetainedToken.decoded).toArray⟩

/-- AST is the result of the selected typed start action of the shared grammar.
Later slices supply larger action families through the same handoff. -/
structure Parsed (Ast : Type) where
  source : Source
  tree : Ast

structure Origin where
  operation : SiteId
  lexical : FileSpan
  evidenceKey : StableKey
  deriving DecidableEq, Repr

structure ResourceRefusal where
  rule : RuleId
  measured : Nat
  limit : Nat

def sliceOne : Slice := ⟨⟨1, by decide⟩⟩

inductive Surface0Issue where
  | unexpectedEntry (span : Surface0.Span)
  | wildcard (span : Surface0.Span)
  | fallthrough (span : Surface0.Span)
  | unreachable (span : Surface0.Span)
  | integerSuffix (span : Surface0.Span)
  | unknownLocal (name : String) (span : Surface0.Span)
  | duplicateBinder (name : String) (span : Surface0.Span)
  | reservedBinder (name : String) (span : Surface0.Span)
  deriving DecidableEq

def surfaceFileSpan (s : ConcreteSpec.Surface0.Span) : FileSpan :=
  ⟨s.fileId, s.start, s.stop, s.line, s.column⟩

inductive ParserIssue where
  | unexpectedToken | unexpectedEnd | trailingTokens
  | outsideProduction | nonDecimal
  | internal (transition : Slice0Rules.ParserTransition)
  deriving DecidableEq

inductive DecodeReason where
  | truncated | invalidContinuation | overlong | surrogate | tooLarge | unexpectedContinuation | nul | bom
  deriving DecidableEq, Repr

inductive LexIssue where
  | encoding (reason : DecodeReason)
  | integer | string | byte | character | comment
  | digitLimit (limit : Nat)
  deriving DecidableEq

/-- Core rules for ownership and arithmetic depend on the admitted slice and terminator. -/
inductive CoreContext where
  | slice0 | returning | aborting
  deriving DecidableEq

/-- Cite the precise static obligation rejected by the shared traversal. -/
private def CoreContext.rule (context : CoreContext) : Admission.Refusal → RegisteredRule
  | .atPosition _ reason => match reason with
    | .duplicateBinding _ => ⟨⟨``OwnershipRule.FreshBinder, 1⟩, by decide⟩
    | .patternBinderAlreadyBound _ | .repeatedPatternBinder _ => ⟨⟨``OwnershipRule.FreshPattern, 1⟩, by decide⟩
    | .movedBinding _ | .copyOfLinear _ | .moveOfCopy _ => ⟨⟨``OwnershipRule.ReadAllowed, 1⟩, by decide⟩
    | .liveLinearBinding _ => ⟨⟨``OwnershipRule.ReturnDischarged, 1⟩, by decide⟩
    | .unusedBinding _ => match context with
      | .slice0 => ⟨⟨``Slice0Rules.LocalsUsed, 1⟩, by decide⟩
      | .returning => ⟨⟨``OwnershipRule.ReturnDischarged, 1⟩, by decide⟩
      | .aborting => ⟨⟨``AbortReady, 1⟩, by decide⟩
    | .discardLinear _ => ⟨⟨``WildcardPermitted, 1⟩, by decide⟩
    | .literalOutOfRange .. | .additionOperandType _ (.atomFailure (.outOfRange ..)) => match context with
      | .slice0 => ⟨⟨``Slice0Rules.LiteralTyped, 1⟩, by decide⟩
      | .returning | .aborting => ⟨⟨``ProgramPermitted, 1⟩, by decide⟩
    | .additionOverflowUnproven => match context with
      | .slice0 => ⟨⟨``Slice0Rules.AdditionFits, 1⟩, by decide⟩
      | .returning | .aborting => ⟨⟨``ProgramPermitted, 1⟩, by decide⟩
    | _ => ⟨⟨``ProgramPermitted, 1⟩, by decide⟩
  | _ => ⟨⟨``ProgramPermitted, 1⟩, by decide⟩

/-- The producer fixes the violation and its location. Later stages transport this
value verbatim. Only Core needs additional locations for related declarations. -/
inductive RefusalCause where
  | lexical (issue : LexIssue) (span : Surface0.Span)
  | parser (issue : ParserIssue) (span : Surface0.Span)
  | surface (issue : Surface0Issue)
  | core (context : CoreContext) (primary : FileSpan) (related : List (Name × FileSpan))
      (reason : Admission.Refusal)
  deriving DecidableEq

private def Surface0Issue.location (reason : Surface0Issue) : RegisteredRule × Surface0.Span :=
  match reason with
  | .unexpectedEntry s => (⟨⟨``Slice0Rules.EntryName, 1⟩, by decide⟩, s)
  | .wildcard s => (⟨⟨``Slice0Rules.NamedBinding, 1⟩, by decide⟩, s)
  | .fallthrough s => (⟨⟨``Slice0Rules.Returns, 1⟩, by decide⟩, s)
  | .unreachable s => (⟨⟨``Slice0Rules.ReachableBody, 1⟩, by decide⟩, s)
  | .integerSuffix s => (⟨⟨``Slice0Rules.ExpectedTypeLiteral, 1⟩, by decide⟩, s)
  | .unknownLocal _ s => (⟨⟨``Slice0Rules.LocalBound, 1⟩, by decide⟩, s)
  | .duplicateBinder _ s => (⟨⟨``Slice0Rules.FreshLocal, 1⟩, by decide⟩, s)
  | .reservedBinder _ s => (⟨⟨``Slice0Rules.UnreservedLocal, 2⟩, by decide⟩, s)

def RefusalCause.citation : RefusalCause → RegisteredRule
  | .lexical (.digitLimit _) _ => ⟨⟨``Slice0Rules.WithinLimit, 1⟩, by decide⟩
  | .lexical _ _ => ⟨⟨``Slice0Rules.Lexed, 2⟩, by decide⟩
  | .parser (.internal _) _ => ⟨⟨``Slice0Rules.ParserCanStep, 1⟩, by decide⟩
  | .parser .outsideProduction _ => ⟨⟨``Slice0Rules.AdmittedProduction, 2⟩, by decide⟩
  | .parser .nonDecimal _ => ⟨⟨``Slice0Rules.DecimalLiteral, 1⟩, by decide⟩
  | .parser _ _ => ⟨⟨``Slice0Rules.Parsed, 1⟩, by decide⟩
  | .surface issue => issue.location.1
  | .core context _ _ reason => context.rule reason

def RefusalCause.rule (cause : RefusalCause) := cause.citation.val

def RefusalCause.primary : RefusalCause → FileSpan
  | .lexical _ span | .parser _ span => surfaceFileSpan span
  | .surface issue => surfaceFileSpan issue.location.2
  | .core _ span .. => span

def RefusalCause.related : RefusalCause → List (Name × FileSpan)
  | .core _ _ related _ => related
  | _ => []

def FileSpan.surface (span : FileSpan) : Surface0.Span :=
  ⟨span.startOffset, span.endOffset, span.line, span.col, span.file⟩

inductive ParseRefusal where
  | invalidStream (atSpan : FileSpan)
  | malformed (cause : RefusalCause)
  | unsupported (cause : RefusalCause) (required : Slice)

/-- Canonical identity of an ordered operand, shared with typed MIR reads.
Literal bytes are the integrated Core instance's canonical value encoding. -/
inductive OperandRead where
  | literal (encoding : List UInt8)
  | copy (place : PlaceId) (fields : List FieldId)
  | move (place : PlaceId) (fields : List FieldId)
  | address (place : PlaceId) (fields : List FieldId)
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

/-- Layout is a spec function at the resolved target. A zero extent still has logical lifetime. -/
structure Layout where
  size : Nat
  align : Nat
  deriving DecidableEq, Repr

abbrev Declarations := AList DeclId StructDecl

/-- Resolve a unique declared, non-Copy, empty struct. Both the qualified
identity and the spec declaration must agree. -/
def Declarations.emptyLinear (env : Declarations) (id : DeclId) : Bool :=
  match env.filter (fun entry => entry.1 == id) with
  | [(_, decl)] => decl.name == id.name && !decl.copy && decl.fields.isEmpty
  | _ => false

abbrev LayoutOf (Ty : Type) := Declarations → Target → Ty → Option Layout

inductive AddressStorage where
  | logical
  | zero
  | bytes
  deriving DecidableEq

/-- An address operand identifies its pointee even in the logical execution.
Physical execution also supplies the live, provenance-bearing byte range. -/
structure OperationAddress (Ty : Type) where
  pointee : Ty
  root : PlaceId
  fields : List FieldId
  range : Option Hybrid.Range
  provenance : Hybrid.Place
  storage : AddressStorage := .bytes

structure OperationStore (Value : Type) where
  bytes : Mem.Memory
  ownership : Own.State
  /-- Escaped logical roots, including zero-byte physical roots and suspended callers. -/
  addressedValues : AList Hybrid.Place Value := []

/-- The operand family is supplied by ConcreteSpec. Control, calls and scoped lifetime use Exec. -/
structure CoreLanguage where
  operands : Exec.Operands
  hybridStore : operands.Store = Hybrid.State
  evalIn : Declarations → AList Var operands.Val → operands.Store → operands.Expr →
    Res (operands.Val × operands.Store) := fun _ => operands.eval
  /-- Exact ordered operand reads and trap conditions of the integrated spec operation. -/
  expressionOperator : operands.Expr → Operator
  primitiveOperator : operands.Prim → Operator
  /-- Arguments are already evaluated reads. A checked computation occurs in its own
  Core statement before the call, with its own site and result local. -/
  argumentRead : operands.Expr → Option (operands.Ty × OperandRead)
  argumentReadNoTrap : ∀ expr, (argumentRead expr).isSome →
    ∀ declarations locals store kind, evalIn declarations locals store expr ≠ .trap kind
  OtherValue : Type
  valueShape : operands.Val = CoreValue OtherValue
  /-- The integrated spec determines layouts, scalar types and aggregate representation.
  Aggregate decoding cannot construct a scalar. -/
  layout : LayoutOf operands.Ty
  scalarType : operands.Ty → Option Hybrid.Ty
  literal : (ty : operands.Ty) → List UInt8 → Option operands.Val
  typeDecidableEq : DecidableEq operands.Ty
  fieldType : operands.Ty → FieldId → Option operands.Ty
  activeFields : Declarations → operands.Ty → operands.Val → Option (List FieldId)
  operation : Declarations → Operator → List operands.Ty → operands.Ty → List operands.Val →
    Target → List (OperationAddress operands.Ty) → OperationStore operands.Val →
    operands.Val → List operands.Event → OperationStore operands.Val → Prop
  /-- Relate logical references to physical values using only the backing ranges of
  related live allocations. Aggregate instances relate their fields recursively. -/
  representsValue : Target → AList Hybrid.Place (Option Hybrid.Range) → operands.Ty →
    operands.Val → operands.Val → Prop := fun _ _ _ logical physical => logical = physical
  checkTrap : Opcode → Nat → List operands.Val → Bool
  field : operands.Val → List FieldId → Option operands.Val
  replaceField : operands.Val → List FieldId → operands.Val → Option operands.Val
  fieldRange : operands.Ty → List FieldId → Layout → Option (Nat × Layout)
  encodeOther : Target → operands.Ty → OtherValue → Option (List (Nat × List Mem.MByte))
  decodeOther : Target → operands.Ty → List Mem.MByte → Option OtherValue
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
    (ownership : AList Var Own.Descriptor) (declarations : Declarations := []) : Exec.Operands :=
  Own.instrumentOperands {language.operands with eval := language.evalIn declarations} (AList.lookup ownership) (language.permit ownership)

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
  | add (ty : Core.IntTy) (left right : EarlyOperand)
  | constructEmpty (decl : DeclId)
  | consumeEmpty (decl : DeclId) (rhs : EarlyExpr)


def earlyRead (locals : AList Var EarlyValue) : EarlyOperand → Option EarlyValue
  | .literal value => some (.scalar value)
  | .copy localId => do
    let .scalar value ← AList.lookup locals localId | none
    some (.scalar value)
  | .move localId => AList.lookup locals localId

def earlyEval (env : Declarations) (locals : AList Var EarlyValue) : EarlyExpr → Res EarlyValue
  | .read operand =>
    match earlyRead locals operand with
    | some value => .ok value
    | none => .stuck
  | .add ty left right =>
    match earlyRead locals left, earlyRead locals right with
    | some (.scalar (.int lt l)), some (.scalar (.int rt r)) =>
      if lt = ty ∧ rt = ty then (plain ty .add l r).map (CoreValue.scalar ∘ CVal.int ty)
      else .stuck
    | _, _ => .stuck
  | .constructEmpty id => if env.emptyLinear id then .ok (.other id) else .stuck
  | .consumeEmpty id rhs =>
    match earlyEval env locals rhs with
    | .ok (.other actual) => if actual = id && env.emptyLinear id then .ok (.scalar .unit) else .stuck
    | .trap kind => .trap kind
    | _ => .stuck

def earlyPermitRead (own : Own.State) : EarlyOperand → Option Own.State
  | .literal _ => some own
  | .copy localId => do
    let frame ← own.frames.head?
    let root ← AList.lookup frame.locals localId
    Own.accessOwner own root .shared
  | .move localId => do
    let frame ← own.frames.head?
    Own.move own (← AList.lookup frame.locals localId)

def earlyPermit (own : Own.State) : EarlyExpr → Option Own.State
  | .read read => earlyPermitRead own read
  | .add _ left right => (earlyPermitRead own left).bind (earlyPermitRead · right)
  | .constructEmpty _ => some own
  | .consumeEmpty _ rhs => earlyPermit own rhs

private def earlyIntRead (ty : Core.IntTy) : EarlyOperand → Option (EarlyTy × OperandRead)
  | .literal (.int actual n) =>
    if ty = actual then
      some (.scalar (.int ty), .literal ((Mem.leBytes ty.byteSize (toBits ty n)).map
        fun b => UInt8.ofNat b.val))
    else none
  | .copy localId => some (.scalar (.int ty), .copy ⟨localId.id⟩ [])
  | _ => none

private def earlyOperation (env : Declarations) (op : Operator) (types : List (EarlyTy ⊕ Bool))
    (ty : EarlyTy ⊕ Bool) (values : List EarlyValue) : Res EarlyValue :=
  match op, types, ty, values with
  | .named ⟨"destructureEmpty"⟩, [.inl (.nominal id)], .inl (.scalar .unit), [.other actual] =>
    if actual = id && env.emptyLinear id then .ok (.scalar .unit) else .stuck
  | .named ⟨"constructEmpty"⟩, [], .inl (.nominal id), [] =>
    if env.emptyLinear id then .ok (.other id) else .stuck
  | .read, [.inl (.nominal src)], .inl (.nominal dst), [.other value] =>
    if src = dst && value = dst && env.emptyLinear dst then .ok (.other value) else .stuck
  | .read, [.inl (.scalar input)], .inl (.scalar result), [.scalar value] =>
    if input = result ∧ value.ty = result then .ok (.scalar value) else .stuck
  | .arithmetic (.plain .add), [.inl (.scalar (.int left)), .inl (.scalar (.int right))],
      .inl (.scalar (.int result)), [.scalar (.int lt l), .scalar (.int rt r)] =>
    if left = result ∧ right = result ∧ lt = result ∧ rt = result then
      (plain result .add l r).map (CoreValue.scalar ∘ CVal.int result)
    else .stuck
  | _, _, _, _ => .stuck

def earlyEvaluate (env : Declarations) (locals : AList Var EarlyValue)
    (store : Hybrid.State) : EarlyExpr ⊕ Bool → Res (EarlyValue × Hybrid.State)
  | .inl expr => (earlyEval env locals expr).map fun value => (value, store)
  | .inr _ => .stuck

/-- Closed scalar and nominal semantics for the implemented early fragments. -/
def sliceLanguage (_slice : Slice) : CoreLanguage where
  operands := {
    Val := EarlyValue
    Ty := EarlyTy ⊕ Bool
    Expr := EarlyExpr ⊕ Bool
    Prim := Empty
    Event := Empty
    Store := Hybrid.State
    unitVal := .scalar .unit
    unitTy := .inl (.scalar .unit)
    isCopy := fun ty => match ty with | .inl (.scalar _) => true | _ => false
    eval := earlyEvaluate []
    life := fun _ store => some store
    test := fun | .scalar (.bool value) => some value | _ => none
    allows := fun _ _ _ _ => false }
  hybridStore := rfl
  evalIn := earlyEvaluate
  expressionOperator
    | .inl (.add ..) => .arithmetic (.plain .add)
    | .inl (.constructEmpty _) => .named ⟨"constructEmpty"⟩
    | .inl (.consumeEmpty ..) => .named ⟨"destructureEmpty"⟩
    | _ => .read
  primitiveOperator p := nomatch p
  argumentRead := fun _ => none
  argumentReadNoTrap := by simp
  OtherValue := DeclId
  valueShape := rfl
  layout env target ty := do
    let profile := Slice0Layout.linuxX8664Profile.withByteOrder target.byteOrder
    if target ≠ profile.target then none else
    let shape ← match ty with
      | .inl (.scalar (.int intTy)) => some (.integer intTy)
      | .inl (.nominal id) => if env.emptyLinear id then some .logical else none
      | _ => none
    let layout ← Slice0Layout.query profile shape
    return ⟨layout.byteSize, layout.alignment⟩
  scalarType
    | .inl (.scalar ty) => some ty
    | _ => none
  literal ty bytes :=
    match ty with
    | .inl (.scalar (.int intTy)) =>
      if bytes.length = intTy.byteSize then
        some (.scalar (.int intTy (ofBits intTy (Mem.leValue (bytes.map UInt8.toFin)))))
      else none
    | _ => none
  typeDecidableEq := inferInstance
  fieldType := fun _ _ => none
  activeFields
    | env, .inl (.nominal id), .other actual =>
      if id = actual && env.emptyLinear id then some [] else none
    | _, _, _ => none
  operation := fun env op inputs output values _ addresses before result effects after =>
    addresses = [] ∧ earlyOperation env op inputs output values = .ok result ∧
      effects = [] ∧ before = after
  checkTrap
    | .operator (.arithmetic (.plain .add)), 0, [.scalar (.int lt l), .scalar (.int rt r)] =>
      lt == rt && decide (checked lt .add l r = .error .overflow)
    | _, _, _ => false
  field := fun _ _ => none
  replaceField := fun _ _ _ => none
  fieldRange := fun _ _ _ => none
  encodeOther := fun _ _ _ => none
  decodeOther := fun _ _ _ => none
  safety
    | .expression (.inl (.add ty left right)) =>
      match earlyIntRead ty left, earlyIntRead ty right with
      | some l, some r => [⟨0, .overflow, [(.inl l.1, l.2), (.inl r.1, r.2)]⟩]
      | _, _ => []
    | .abort => [⟨0, .abort, []⟩]
    | _ => []
  permit _ _ expr own :=
    match expr with
    | .inl e => earlyPermit own e
    | _ => none


structure MachineProgram (language : CoreLanguage) where
  ownership : AList Var Own.Descriptor
  declarations : Declarations
  body : Exec.Program (language.instrumented ownership declarations)
  entry : FnId
  origins : AList SiteId Origin


structure LocatedBinding where
  statement : ConcreteSpec.Slice0.Stmt
  operationSpan : Surface0.Span
  declarationSpan : Surface0.Span
  /-- An atomic RHS is located at operationSpan. An addition also carries each read. -/
  operandSpans : match statement.e with
    | .atom _ => Unit
    | .add .. => Surface0.Span × Surface0.Span
  name : ConstructName := .unnamed none

private def LocatedBinding.readSpans (b : LocatedBinding) : List (ConcreteSpec.Atom × Surface0.Span) :=
  match b with
  | ⟨⟨_, .atom a⟩, span, _, _, _⟩ => [(a.toCore, span)]
  | ⟨⟨_, .add l r⟩, _, _, (ls, rs), _⟩ => [(l.toCore, ls), (r.toCore, rs)]

structure EarlyProgram where
  bindings : List LocatedBinding
  ret : ConcreteSpec.Slice0.Atom
  returnSpan : Surface0.Span
  resultSpan : Surface0.Span

def EarlyProgram.eraseLocations (p : EarlyProgram) : Slice0.Program :=
  ⟨p.bindings.map (·.statement), p.ret⟩

def earlyOperand : ConcreteSpec.Slice0.Atom → EarlyOperand
  | .lit n => .literal (.int .u8 n)
  | .var id => .copy id

def earlyExpression : ConcreteSpec.Slice0.Expr → EarlyExpr
  | .atom atom => .read (earlyOperand atom)
  | .add left right => .add .u8 (earlyOperand left) (earlyOperand right)

private def earlyOrigin (site : SiteId) (span : Surface0.Span) (entry : FnId)
    (key : FunctionRelativeKey) : SiteId × Origin :=
  (site, ⟨site, ⟨span.fileId, span.start, span.stop, span.line, span.column⟩, ⟨.function entry, key⟩⟩)

/-- The standalone Slice 0 program has one internal function identity. -/
def earlyEntry : FnId :=
  ⟨⟨⟨⟨"standalone"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩, ⟨"main"⟩⟩

/-- The Slice 1 surface fragment has u8 and nominal empty linear structs.
The declaration and operation locations are part of the syntax being admitted. -/
inductive StraightType where
  | u8
  | nominal (name : Name)

inductive StraightExpr where
  | scalar (expr : ConcreteSpec.Slice0.Expr)
  | move (x : Var)
  | construct (name : Name)

inductive StraightBinding where
  | slice0 (statement : Slice0.Stmt)
  | bind (binder : ConcreteSpec.Binder) (ty : StraightType) (rhs : StraightExpr)
  | destructure (name : Name) (rhs : StraightExpr)

inductive StraightEnd where
  | ret (atom : ConcreteSpec.Slice0.Atom)
  | abort

structure LocatedStraightBinding where
  statement : StraightBinding
  operationSpan : Surface0.Span
  declarationSpan : Surface0.Span
  name : ConstructName := .unnamed none

structure StraightProgram where
  declarations : List (Name × Surface0.Span)
  bindings : List LocatedStraightBinding
  ending : StraightEnd
  endSpan : Surface0.Span

inductive AdmissionView where
  | zero (body : EarlyProgram)
  | one (body : StraightProgram)

def AdmissionView.slice : AdmissionView → Slice
  | .zero _ => slice0
  | .one _ => sliceOne

def AdmissionView.located : AdmissionView → StraightProgram
  | .one p => p
  | .zero p => ⟨[], p.bindings.map (fun b =>
      ⟨.slice0 b.statement, b.operationSpan, b.declarationSpan, b.name⟩),
      .ret p.ret, p.returnSpan⟩

def StraightType.toCore : StraightType → Ty
  | .u8 => .scalar (.int .u8)
  | .nominal name => .struct name

def StraightExpr.toCore : StraightExpr → Expr
  | .scalar e => e.toCore
  | .move x => .atom (.move x)
  | .construct name => .mk name []

def StraightBinding.toCore : StraightBinding → Stmt
  | .slice0 s => s.toCore
  | .bind binder ty rhs => .let_ binder ty.toCore rhs.toCore
  | .destructure name rhs => .letStruct name [] rhs.toCore

def StraightProgram.decls (p : StraightProgram) : Declarations :=
  p.declarations.map fun (name, _) => (⟨earlyEntry.module, name⟩, ⟨name, false, []⟩)

def AdmissionView.program : AdmissionView → ConcreteSpec.Program
  | .zero p => Slice0.toCore p.eraseLocations
  | .one p =>
    ⟨List.map Prod.snd p.decls, p.bindings.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement),
      match p.ending with
      | .ret a => .ret a.toCore
      | .abort => .abort ⟨p.bindings.length⟩⟩

def StraightProgram.endingOrigin (p : StraightProgram) : SiteId × Origin :=
  let endKey : KeyConstruct := ⟨(match p.ending with | .ret _ => .return | .abort => .abort), .unnamed none, []⟩
  earlyOrigin ⟨p.bindings.length⟩ p.endSpan earlyEntry (endKey.key [])

def StraightProgram.bindingOrigins (p : StraightProgram) : AList SiteId Origin :=
  KeyConstruct.mapSiblingKeys .binding (fun row => row.1.name)
    (fun (b, position) => earlyOrigin ⟨position⟩ b.operationSpan earlyEntry) [] p.bindings.zipIdx

def AdmissionView.originAt (v : AdmissionView) (position : Nat) : Option Origin :=
  if position = v.located.bindings.length then some v.located.endingOrigin.2 else
    v.located.bindingOrigins[position]?.map Prod.snd

def StraightBinding.destination : StraightBinding → Option Var
  | .slice0 s => some s.x
  | .bind (.name x) .. => some x
  | _ => none

def AdmissionView.bindingAt (v : AdmissionView) (x : Var) : Option Surface0.Span :=
  (v.located.bindings.find? (fun b => b.statement.destination == some x)).map (·.declarationSpan)

def StraightBinding.rhs : StraightBinding → StraightExpr
  | .slice0 s => .scalar s.e
  | .bind _ _ rhs | .destructure _ rhs => rhs

def StraightExpr.integrated : StraightExpr → EarlyExpr
  | .scalar e => earlyExpression e
  | .move x => .read (.move x)
  | .construct name => .constructEmpty ⟨earlyEntry.module, name⟩

def atomBound : Slice0.Atom → Nat
  | .lit _ => 0
  | .var x => x.id + 1

def StraightExpr.variableBound : StraightExpr → Nat
  | .scalar (.atom a) => atomBound a
  | .scalar (.add l r) => max (atomBound l) (atomBound r)
  | .move x => x.id + 1
  | .construct _ => 0

def StraightBinding.descriptor : StraightBinding → Own.Descriptor
  | .slice0 _ | .bind _ .u8 _ => ⟨.int .u8, true, false⟩
  | .bind _ (.nominal _) _ => ⟨.unit, false, false⟩
  | .destructure .. => ⟨.unit, true, false⟩

def StraightBinding.bound (b : StraightBinding) : Nat :=
  max ((b.destination.map (·.id + 1)).getD 0) b.rhs.variableBound

def StraightProgram.freshBase (p : StraightProgram) : Nat :=
  p.bindings.foldr (fun b n => max b.statement.bound n)
    (match p.ending with | .ret a => atomBound a | .abort => 0)

def StraightBinding.target (b : StraightBinding) (fresh position : Nat) : Var :=
  b.destination.getD ⟨fresh + position⟩

def StraightBinding.expression (b : StraightBinding) : EarlyExpr :=
  match b with
  | .destructure name rhs => .consumeEmpty ⟨earlyEntry.module, name⟩ rhs.integrated
  | _ => b.rhs.integrated

def StraightProgram.owners (fresh : Nat) : List LocatedStraightBinding → Nat → AList Var Own.Descriptor
  | [], _ => []
  | b :: rest, i => (b.statement.target fresh i, b.statement.descriptor) :: owners fresh rest (i + 1)

def StraightEnd.toCore (ending : StraightEnd) (site : SiteId) : Terminator :=
  match ending with
  | .ret a => .ret a.toCore
  | .abort => .abort site

def StraightEnd.machine {slice owners decls} (ending : StraightEnd) (site : SiteId) :
    Exec.Stmt ((sliceLanguage slice).instrumented owners decls) :=
  match ending with
  | .ret a => .ret (.inl (.read (earlyOperand a))) site
  | .abort => .abort site

def StraightProgram.machineBody {slice owners decls} (fresh : Nat)
    (ending : Exec.Stmt ((sliceLanguage slice).instrumented owners decls)) :
    List LocatedStraightBinding → Nat → Exec.Stmt ((sliceLanguage slice).instrumented owners decls)
  | [], _ => ending
  | b :: rest, i => .seq (.bind (b.statement.target fresh i) (.inl b.statement.expression) ⟨i⟩)
      (machineBody fresh ending rest (i + 1))

/-- Build the machine from the located syntax inspected by admission. Synthetic
destinations are above all source reads and writes, including unbound reads. -/
def AdmissionView.execution (v : AdmissionView) : MachineProgram (sliceLanguage v.slice) :=
  let p := v.located
  let ownership := StraightProgram.owners p.freshBase p.bindings 0
  let body := StraightProgram.machineBody p.freshBase
    (p.ending.machine (slice := v.slice) (owners := ownership) (decls := p.decls) p.endingOrigin.1) p.bindings 0
  ⟨ownership, p.decls, ⟨[(earlyEntry, ⟨[], .inl (.scalar (.int .u8)), ⟨0⟩, body⟩)]⟩,
    earlyEntry, p.bindingOrigins ++ [p.endingOrigin]⟩

/-- A fragment carries its canonical syntax once. General Exec functions use
the integrated constructor and need their own slice's admission rules. -/
inductive Payload (language : CoreLanguage) where
  | straightLine (v : AdmissionView) (instanceEq : language = sliceLanguage v.slice)
  | integrated (machine : MachineProgram language)

def Payload.execution {language} : Payload language → MachineProgram language
  | .straightLine v eq => eq.symm ▸ v.execution
  | .integrated machine => machine

abbrev MachineProgram.machine {language : CoreLanguage} (payload : MachineProgram language) : Exec.Operands :=
  language.instrumented payload.ownership payload.declarations

def CoreLanguage.emptyStore (language : CoreLanguage) : Own.Store language.operands.Store :=
  ⟨language.hybridStore.symm ▸ (⟨[], Mem.Memory.empty⟩ : Hybrid.State), {}⟩

/-- Hosted Core starts at the program's entry with no language arguments,
cells or outstanding loans. -/
def MachineProgram.initial? {language : CoreLanguage} (payload : MachineProgram language) :
    Option (Exec.Config payload.machine) := do
  let fn ← AList.lookup payload.body.fns payload.entry
  if fn.params.isEmpty then Exec.enter fn [] language.emptyStore else none

def MachineProgram.initial {language : CoreLanguage} (payload : MachineProgram language) :
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

def MachineProgram.operations {language : CoreLanguage} (payload : MachineProgram language) :
    List (SiteId × CoreOperation language.operands) :=
  payload.body.fns.flatMap fun (_, fn) =>
    (stmtOperations fn.body).map fun (site, op) => (site, match op with
      | .expression e => .expression e
      | .call fn args => .call fn args
      | .primitive p args => .primitive p args
      | .abort => .abort)

def MachineProgram.activeOperation {language : CoreLanguage} (payload : MachineProgram language)
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

def CoreInput.machineProgram {language} (input : CoreInput language) := input.payload.execution

def CoreInput.ArgumentsWF {language : CoreLanguage} (input : CoreInput language) : Prop :=
  ∀ site op, (site, op) ∈ input.machineProgram.operations →
    match op with
    | .call _ args | .primitive _ args => ∀ expr ∈ args, (language.argumentRead expr).isSome
    | _ => True

def CoreInput.LocationsWF {language : CoreLanguage} (input : CoreInput language) : Prop :=
  (List.map Prod.fst input.machineProgram.origins).Nodup ∧
  (List.map Prod.fst input.machineProgram.operations).Nodup ∧
  (List.map (fun (entry : SiteId × Origin) => entry.2.evidenceKey) input.machineProgram.origins).Nodup ∧
  (∀ site op, (site, op) ∈ input.machineProgram.operations →
    ∃ origin, AList.lookup input.machineProgram.origins site = some origin) ∧
  (∀ state events kind site, Exec.Steps input.machineProgram.body input.machineProgram.initial events state →
    state.final? = some (.trapped kind site) →
    ∃ origin, AList.lookup input.machineProgram.origins site = some origin) ∧
  ∀ (site : SiteId) (origin : Origin), (site, origin) ∈ input.machineProgram.origins →
    site = origin.operation ∧ ∃ span, origin.lexical.Binds input.project span

/-- Diagnostic spans in addition to the machine's operation origins. -/
def CoreInput.DiagnosticLocationsWF {language} (input : CoreInput language) : Prop :=
  match input.payload with
  | .integrated _ => True
  | .straightLine v _ =>
    let reads := match v with
      | .zero p => p.resultSpan :: p.bindings.flatMap (fun b => b.readSpans.map Prod.snd)
      | .one _ => []
    ∀ location ∈ List.map Prod.snd v.located.declarations ++ v.located.bindings.map (·.declarationSpan) ++ reads,
      ∃ span, (surfaceFileSpan location).Binds input.project span

def CoreInput.WF {language : CoreLanguage} (input : CoreInput language) : Prop :=
  input.LocationsWF ∧ input.ArgumentsWF ∧ input.DiagnosticLocationsWF

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

/-- Exact equality to the fragment constructor determines all program data and spans. -/
def AdmissionView.Represents {language} (v : AdmissionView) (i : CoreInput language) : Prop :=
  i.slice = v.slice ∧ ∃ eq, i.payload = .straightLine v eq

inductive BridgeRefusal where
  | unsupportedSlice (slice : Slice)
  | sliceMismatch (expected actual : Slice)
  | outsideFragment

/-- Structural bridge failures are explicit at execution time. A validated stage
on its declared domain can refuse only with a justified checker diagnostic. -/
inductive AdmissionFailure where
  | fragment (reason : BridgeRefusal)
  | missingLocation (reason : Admission.Refusal)
  | unmappedEvidence
  | refused (diagnostic : RefusalCause)

def CoreInput.decode {language} (i : CoreInput language) : Except BridgeRefusal AdmissionView :=
  let failure := if i.slice.val > 1 then BridgeRefusal.unsupportedSlice i.slice else .outsideFragment
  match i.payload with
  | .integrated _ => .error failure
  | .straightLine v _ => if i.slice = v.slice then .ok v else .error failure

theorem CoreInput.decode_iff {language} (i : CoreInput language) (v : AdmissionView) :
    i.decode = .ok v ↔ v.Represents i := by
  constructor
  · intro h
    cases code : i.payload with
    | integrated _ => simp [decode, code] at h
    | straightLine w eq =>
      by_cases same : i.slice = w.slice
      · simp [decode, code, same] at h
        subst w
        exact ⟨same, eq, code⟩
      · simp [decode, code, same] at h
  · rintro ⟨same, eq, code⟩
    simp [decode, code, same]

theorem AdmissionView.unique {language} {i : CoreInput language} {v w : AdmissionView}
    (hv : v.Represents i) (hw : w.Represents i) : v = w :=
  Except.ok.inj (((i.decode_iff v).2 hv).symm.trans ((i.decode_iff w).2 hw))

def CoreInput.InFragment {language} (input : CoreInput language) : Prop :=
  ∃ view : AdmissionView, view.Represents input

def CoreInput.StaticallyTyped {language} (input : CoreInput language) : Prop :=
  ∃ view : AdmissionView, view.Represents input ∧ Admission.StaticallyTyped view.program

/-- Both entries require all static judgments, including the ordinary use
and linear discharge checks at the terminator. Only arithmetic evidence varies. -/
def CoreInput.UncheckedWF {language} (input : CoreInput language) : Prop :=
  input.WF ∧ input.StaticallyTyped ∧ ∃ v : AdmissionView, v.Represents input ∧
    BodyOkWith ArithmeticPolicy.runtime.permits v.program.decls [] v.program.body v.program.ending

def CoreInput.Accepted {language} (input : CoreInput language) : Prop :=
  input.UncheckedWF ∧ ∃ view : AdmissionView, view.Represents input ∧ Checked view.program

private def EarlyProgram.readsAt (p : EarlyProgram) (position : Nat) :=
  if position = p.bindings.length then [(p.ret.toCore, p.resultSpan)]
  else (p.bindings[position]?.map LocatedBinding.readSpans).getD []

private def AdmissionView.context : AdmissionView → CoreContext
  | .zero _ => .slice0
  | .one p => match p.ending with
    | .ret _ => .returning
    | .abort => .aborting

/-- Operand-type refusals point to the rejected read in the checker's traversal order. -/
private def rejectedLiteral : Admission.Reason → Option ConcreteSpec.Atom
  | .literalOutOfRange ty n | .additionOperandType _ (.atomFailure (.outOfRange ty n)) => some (.int ty n)
  | _ => none

def AdmissionView.render (v : AdmissionView) (r : Admission.Refusal) : Option RefusalCause := do
  match r with
  | .atPosition pos reason =>
    let origin ← v.originAt pos
    let primary ← match v, rejectedLiteral reason with
      | .zero p, some atom => do
        let (_, span) ← (p.readsAt pos).find? (fun read => read.1 == atom)
        pure (surfaceFileSpan span)
      | _, _ => pure origin.lexical
    let related ← match reason with
      | .unusedBinding x | .liveLinearBinding x => do
        let span ← v.bindingAt x
        pure [(⟨"binding"⟩, surfaceFileSpan span)]
      | _ => pure []
    return .core v.context primary related r
  | .duplicateStructName name | .reservedStructName name | .duplicateDeclaredField name _ =>
    let (_, span) ← v.located.declarations.find? (fun d => d.1 == name)
    return .core v.context (surfaceFileSpan span) [] r

namespace SurfacePreparation
open ConcreteSpec.Surface0

/-- Select the subsequent Core entry. Both entries enforce identical surface statics. -/
inductive Purpose where
  | eligibility | admission
  deriving DecidableEq

/-- A prefix contains only real reads and A-normal bindings already visited. When a
later source action fails, the same Core judgments can still check this prefix. -/
private inductive PrefixStep where
  | integer (value : Nat) (span : Surface0.Span)
  | binding (value : LocatedBinding)

structure State where
  private next : Nat := 0
  private bindings : List LocatedBinding := []
  private visited : List PrefixStep := []
  private purpose : Purpose := .admission

private def Purpose.policy : Purpose → ArithmeticPolicy
  | .admission => .static
  | .eligibility => .runtime

private def State.refuse {α : Type} (s : State) (cause : RefusalCause) : Except RefusalCause α :=
  .error (visit [] 0 s.visited.reverse)
where
  visit (ctx : Ctx) (pos : Nat) : List PrefixStep → RefusalCause
    | [] => cause
    | .integer n span :: rest =>
      match inferAtomType ctx.tys (.int .u8 n) with
      | .error (.outOfRange ty value) =>
        .core .slice0 (surfaceFileSpan span) [] (.atPosition pos (.literalOutOfRange ty value))
      | _ => visit ctx pos rest
    | .binding b :: rest =>
      match checkStmtWith s.purpose.policy.permits [] ctx b.statement.toCore with
      | .ok next => visit next (pos + 1) rest
      | .error reason =>
        -- Every integer read was already checked at its token. Remaining statement
        -- conditions belong to the operation, including its arithmetic evidence.
        .core .slice0 (surfaceFileSpan b.operationSpan) [] (.atPosition pos reason)

abbrev Scope := List (String × ConcreteSpec.Var)

structure LocatedAtom where
  atom : Slice0.Atom
  span : Surface0.Span

inductive Rhs where
  | atom (value : LocatedAtom)
  | add (lhs rhs : LocatedAtom) (site : Surface0.Span)

private def Rhs.bind (rhs : Rhs) (s : State)
    (decl : Option Surface0.Span := none) (name : ConstructName := .unnamed none) : (Var × Surface0.Span) × State :=
  let x : Var := ⟨s.next⟩
  let b : LocatedBinding := match rhs with
    | .atom a => ⟨⟨x, .atom a.atom⟩, a.span, decl.getD a.span, (), name⟩
    | .add l r site => ⟨⟨x, .add l.atom r.atom⟩, site, decl.getD site, (l.span, r.span), name⟩
  ((x, b.operationSpan), {s with next := s.next + 1, bindings := b :: s.bindings, visited := .binding b :: s.visited})

private theorem Rhs.bind_fresh (rhs : Rhs) (s : State) (decl : Option Surface0.Span) (name : ConstructName)
    (x : Var) (below : x.id < s.next) : (rhs.bind s decl name).1.1 ≠ x := by
  intro same
  exact (Nat.ne_of_lt below) (congrArg Var.id same).symm

private def Rhs.asAtom (rhs : Rhs) (s : State) : LocatedAtom × State :=
  match rhs with
  | .atom a => (a, s)
  | .add .. =>
    let ((x, site), s) := rhs.bind s
    (⟨.var x, site⟩, s)

/-- Both an accepted syntax tree and a prefix of a larger slice use these same
semantic actions. A blocked action is reached only after its source predecessors. -/
abbrev ExprAction := Scope → State → Except RefusalCause (Rhs × State)

def integer (n : Nat) (suffix : Option IntTy) (span : Surface0.Span) : ExprAction := fun _ s =>
  letI : Decidable (Slice0Rules.ExpectedTypeLiteral suffix) := inferInstanceAs (Decidable (suffix = none))
  if ¬ Slice0Rules.ExpectedTypeLiteral suffix then s.refuse (.surface (.integerSuffix span))
  else .ok (.atom ⟨.lit n, span⟩, {s with visited := .integer n span :: s.visited})

def readLocal (name : String) (span : Surface0.Span) : ExprAction := fun scope s =>
  match scope.lookup name with
  | some x => .ok (.atom ⟨.var x, span⟩, s)
  | none => s.refuse (.surface (.unknownLocal name span))

def blockedExpr (cause : RefusalCause) : ExprAction := fun _ s => s.refuse cause

def afterExpr (left : ExprAction) (cause : RefusalCause) : ExprAction := fun scope s => do
  let (rhs, s) ← left scope s
  (rhs.asAtom s).2.refuse cause

def add (left right : ExprAction) (site : Surface0.Span) : ExprAction := fun scope s => do
  let (lhs, s) ← left scope s
  let (a, beforeRight) := lhs.asAtom s
  let (rhs, s) ← right scope beforeRight
  -- A-normalization emits a non-atomic right operand before the outer addition.
  -- Validate a literal left read before its check can move past that operation.
  if let .add .. := rhs then
    if let .lit _ := a.atom then
      if let .error reason := inferAddOperand [] a.atom.toCore then
        beforeRight.refuse (.core .slice0 (surfaceFileSpan a.span) [] (.atPosition beforeRight.next reason))
  let (b, s) := rhs.asAtom s
  return (.add a b site, s)

private def expr : Surface0.Expr → ExprAction
  | .integer n suffix span => integer n suffix span
  | .local name span => readLocal name span
  | .add lhs rhs site => add (expr lhs) (expr rhs) site

structure BodyAction where
  first : Option Surface0.Span
  run : Scope → State → Surface0.Span → Except RefusalCause EarlyProgram

def endBody : BodyAction :=
  ⟨none, fun _ s last => s.refuse (.surface (.fallthrough last))⟩

def blockedBody (cause : RefusalCause) (start : Surface0.Span) : BodyAction :=
  ⟨some start, fun _ s _ => s.refuse cause⟩

def bindBody (name : String) (binder span : Surface0.Span) (value : ExprAction) (rest : BodyAction) :
    BodyAction := ⟨some span, fun scope s _ => do
  letI : Decidable (Slice0Rules.NamedBinding name) := inferInstanceAs (Decidable (name ≠ "_"))
  unless Slice0Rules.NamedBinding name do s.refuse (.surface (.wildcard binder))
  if name ∈ scope.map Prod.fst then s.refuse (.surface (.duplicateBinder name binder))
  letI : Decidable (Slice0Rules.UnreservedLocal name) := inferInstanceAs (Decidable (UnreservedLocalName name))
  unless Slice0Rules.UnreservedLocal name do s.refuse (.surface (.reservedBinder name binder))
  let (rhs, s) ← value scope s
  let ((x, _), s) := rhs.bind s (some binder) (.named ⟨name⟩)
  rest.run ((name, x) :: scope) s span⟩

def returnBody (value : ExprAction) (span : Surface0.Span) (rest : BodyAction) : BodyAction :=
  ⟨some span, fun scope s _ => do
    let (rhs, s) ← value scope s
    let (ret, s) := rhs.asAtom s
    let program : EarlyProgram := ⟨s.bindings.reverse, ret.atom, span, ret.span⟩
    if let some next := rest.first then
      let view := AdmissionView.zero program
      if let .error reason := checkProgramWith s.purpose.policy view.program then
        if let some cause := view.render reason then throw cause
      throw (.surface (.unreachable next))
    return program⟩

def entry (purpose : Purpose) (name : String) (span : Surface0.Span) (body : BodyAction) :
    Except RefusalCause EarlyProgram := do
  letI : Decidable (Slice0Rules.EntryName name) := inferInstanceAs (Decidable (name = "main"))
  unless Slice0Rules.EntryName name do throw (.surface (.unexpectedEntry span))
  body.run [] {purpose} span

def prepareSurface0 (ast : Surface0.Program) (purpose : Purpose := .admission) :
    Except RefusalCause EarlyProgram :=
  entry purpose ast.name ast.nameSpan (ast.body.foldr statement endBody)
where
  statement : Statement → BodyAction → BodyAction
    | .bind b span, rest => bindBody b.name b.span span (expr b.rhs) rest
    | .ret value span, rest => returnBody (expr value) span rest

end SurfacePreparation

def AdmissionView.failure (v : AdmissionView) (reason : Admission.Refusal) : AdmissionFailure :=
  match v.render reason with
  | some cause => .refused cause
  | none => .missingLocation reason

def CoreInput.checkStatic {language} (input : CoreInput language) : Except AdmissionFailure Unit := do
  let view ← input.decode.mapError AdmissionFailure.fragment
  let _ ← (checkProgramWith .runtime view.program).mapError view.failure
  pure ()

def CoreInput.CheckerRefuses {language} (input : CoreInput language) (failure : RefusalCause) : Prop :=
  ¬ input.Accepted ∧ ∃ (v : AdmissionView) (reason : Admission.Refusal), v.Represents input ∧
    checkProgram v.program = .error reason ∧ v.render reason = some failure

/-- Located source before name resolution. The slice determines its syntax.
No diagnostic can be supplied on this handoff. -/
inductive SourceForm : Slice → Type where
  | zero (program : ConcreteSpec.Surface0.Program) : SourceForm slice0

inductive AdmissionInput (language : CoreLanguage) where
  | core (input : CoreInput language)
  | pending {slice : Slice} (project : Project) (source : SourceForm slice)
      (instanceEq : language = sliceLanguage slice)

def AdmissionInput.slice {language} : AdmissionInput language → Slice
  | .core input => input.slice
  | .pending (slice := s) .. => s

def AdmissionInput.prepare {language} (input : AdmissionInput language)
    (purpose : SurfacePreparation.Purpose := .admission) : Except RefusalCause (CoreInput language) :=
  match input with
  | .core i => .ok i
  | .pending project (.zero source) eq =>
    match SurfacePreparation.prepareSurface0 source purpose with
    | .error issue => .error issue
    | .ok body => .ok ⟨project, slice0, .straightLine (.zero body) eq⟩

theorem AdmissionInput.preparedSlice {language} (input : AdmissionInput language)
    {purpose output} (h : input.prepare purpose = .ok output) : output.slice = input.slice := by
  cases input with
  | core i => cases h; rfl
  | pending project source eq =>
    cases source with
    | zero ast =>
      change (match SurfacePreparation.prepareSurface0 ast purpose with
        | .error issue => Except.error issue
        | .ok body => .ok (CoreInput.mk project slice0 (.straightLine (.zero body) eq))) = .ok output at h
      split at h <;> cases h
      rfl

/-- Surface preparation constructs the canonical early fragment in either mode.
This holds independently of source locations and parser readiness. -/
theorem AdmissionInput.pending_inFragment {language slice} (project : Project)
    (source : SourceForm slice) (eq : language = sliceLanguage slice) {purpose output}
    (prepared : (AdmissionInput.pending project source eq).prepare purpose = .ok output) :
    output.InFragment := by
  cases source with
  | zero ast =>
    change (match SurfacePreparation.prepareSurface0 ast purpose with
      | .error issue => Except.error issue
      | .ok body => .ok (CoreInput.mk project slice0 (.straightLine (.zero body) eq))) = .ok output at prepared
    split at prepared
    · contradiction
    · cases prepared
      exact ⟨_, rfl, eq, rfl⟩

def AdmissionInput.Ready {language} (input : AdmissionInput language) (slice : Slice) : Prop :=
  input.slice = slice ∧ match input with
    | .core i => i.WF ∧ i.InFragment
    | pending@(.pending project ..) => ∀ purpose, match pending.prepare purpose with
      | .ok prepared => prepared.WF
      | .error refusal =>
        (∃ span, refusal.primary.Binds project span) ∧
        ∀ related ∈ refusal.related, ∃ span, related.2.Binds project span

theorem AdmissionInput.Ready.prepared {language slice purpose output} {input : AdmissionInput language}
    (ready : input.Ready slice) (ran : input.prepare purpose = .ok output) :
    output.WF ∧ output.InFragment := by
  cases input with
  | core i => cases ran; exact ready.2
  | pending project source eq =>
    have locations := ready.2 purpose
    rw [ran] at locations
    exact ⟨locations, pending_inFragment project source eq ran⟩

def AdmissionInput.CheckerRefuses {language} (input : AdmissionInput language) (r : RefusalCause) : Prop :=
  match input.prepare with
  | .error canonical => r = canonical
  | .ok prepared => prepared.CheckerRefuses r

def MachineProgram.ConditionsAt {language : CoreLanguage} (payload : MachineProgram language) (site : SiteId)
    (opcode : Opcode) (conditions : List (SafetyCondition language.operands.Ty)) : Prop :=
  ∃ op, (site, op) ∈ payload.operations ∧ (match op with | .abort => False | _ => True) ∧
    language.opcode op = opcode ∧
    conditions = language.safety op

def Discharge.condition {language : CoreLanguage} (d : Discharge language) :
    SafetyCondition language.operands.Ty :=
  ⟨d.obligation.discriminator, d.trap, d.operands⟩

def ValidDischarges {language : CoreLanguage} (input : CoreInput language)
    (discharges : Discharges language) : Prop :=
  input.machineProgram.initial?.isSome = true ∧
  (List.map Prod.fst discharges).Nodup ∧
  (∀ site group, (site, group) ∈ discharges →
    group ≠ [] ∧ (group.map Discharge.obligation).Nodup) ∧
  ∀ (site : SiteId) (d : Discharge language), discharges.Contains site d →
    site = d.operation ∧ d.obligation.site = site ∧ d.obligation.kind = .safety ∧
    (∃ opcode conditions, input.machineProgram.ConditionsAt site opcode conditions ∧ d.condition ∈ conditions) ∧
    (∃ origin, (site, origin) ∈ input.machineProgram.origins ∧ origin.evidenceKey = d.key) ∧
    ∀ state events op, Exec.Steps input.machineProgram.body input.machineProgram.initial events state →
      input.machineProgram.activeOperation state = some (site, op) →
      (∃ label next, Exec.Step input.machineProgram.body state label next) ∧
      (∀ label next, Exec.Step input.machineProgram.body state label next →
        next.ctrl ≠ .trapped d.trap site)

def Admitted.Valid {language : CoreLanguage} (admitted : Admitted language) : Prop :=
  admitted.core.Accepted ∧ ValidDischarges admitted.core admitted.discharges

def Admits {language : CoreLanguage} (input : CoreInput language) (out : Admitted language) : Prop :=
  out.core = input ∧ out.Valid

def AdmissionSound {language : CoreLanguage}
    (check : CoreInput language → Except AdmissionFailure (Discharges language)) : Prop :=
  ∀ input, input.WF → input.InFragment →
    (input.Accepted ↔ ∃ discharges, check input = .ok discharges) ∧
    (∀ discharges, check input = .ok discharges →
      input.UncheckedWF ∧ ValidDischarges input discharges) ∧
    (∀ refusal, check input = .error refusal → ∃ diagnostic, refusal = .refused diagnostic ∧ input.CheckerRefuses diagnostic)

end ConcreteSpec.StageInterface
