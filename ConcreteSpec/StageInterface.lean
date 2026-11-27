import ConcreteSpec.Spec.Slice0Layout
import ConcreteSpec.Spec.Slice0Syntax
import ConcreteSpec.Spec.Slice1Checker
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
  ⟨stream.source, (stream.tokens.toList.map RetainedToken.decoded).toArray⟩

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

abbrev LayoutOf (Ty : Type) := Target → Ty → Option Layout

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
  Declaration : Type
  extraEarlyTypes : Type := Empty
  extraEarlyExpressions : Type := Empty
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
  /-- The integrated spec determines layouts, scalar types and aggregate representation.
  Aggregate decoding cannot construct a scalar. -/
  layout : LayoutOf operands.Ty
  scalarType : operands.Ty → Option Hybrid.Ty
  literal : (ty : operands.Ty) → List UInt8 → Option operands.Val
  typeDecidableEq : DecidableEq operands.Ty
  fieldType : operands.Ty → FieldId → Option operands.Ty
  activeFields : operands.Ty → operands.Val → Option (List FieldId)
  operation : Operator → List operands.Ty → operands.Ty → List operands.Val →
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
  | add (ty : Core.IntTy) (left right : EarlyOperand)
  | constructEmpty (decl : DeclId)

inductive EarlyPrim where
  | destructureEmpty (decl : DeclId)

structure EarlyCore (language : CoreLanguage) : Prop where
  types : language.operands.Ty = (EarlyTy ⊕ language.extraEarlyTypes)
  expressions : language.operands.Expr = (EarlyExpr ⊕ language.extraEarlyExpressions)

private def earlyRead (locals : AList Var EarlyValue) : EarlyOperand → Option EarlyValue
  | .literal value => some (.scalar value)
  | .copy localId => do
    let .scalar value ← AList.lookup locals localId | none
    some (.scalar value)
  | .move _ => none

private def earlyEval (locals : AList Var EarlyValue) : EarlyExpr → Res EarlyValue
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
  | .constructEmpty _ => .stuck

private def earlyPermitRead (own : Own.State) : EarlyOperand → Option Own.State
  | .literal _ => some own
  | .copy localId => do
    let frame ← own.frames.head?
    let root ← AList.lookup frame.locals localId
    Own.accessOwner own root .shared
  | .move _ => none

private def earlyIntRead (ty : Core.IntTy) : EarlyOperand → Option (EarlyTy × OperandRead)
  | .literal (.int actual n) =>
    if ty = actual then
      some (.scalar (.int ty), .literal ((Mem.leBytes ty.byteSize (toBits ty n)).map
        fun b => UInt8.ofNat b.val))
    else none
  | .copy localId => some (.scalar (.int ty), .copy ⟨localId.id⟩ [])
  | _ => none

private def earlyOperation (op : Operator) (types : List (EarlyTy ⊕ Bool))
    (ty : EarlyTy ⊕ Bool) (values : List EarlyValue) : Res EarlyValue :=
  match op, types, ty, values with
  | .read, [.inl (.scalar input)], .inl (.scalar result), [.scalar value] =>
    if input = result ∧ value.ty = result then .ok (.scalar value) else .stuck
  | .arithmetic (.plain .add), [.inl (.scalar (.int left)), .inl (.scalar (.int right))],
      .inl (.scalar (.int result)), [.scalar (.int lt l), .scalar (.int rt r)] =>
    if left = result ∧ right = result ∧ lt = result ∧ rt = result then
      (plain result .add l r).map (CoreValue.scalar ∘ CVal.int result)
    else .stuck
  | _, _, _, _ => .stuck

/-- Closed semantics for the implemented early fragment. Additional slices
extend this definition together with their spec semantics. -/
def sliceLanguage (_slice : Slice) : CoreLanguage where
  operands := {
    Val := EarlyValue
    Ty := EarlyTy ⊕ Bool
    Expr := EarlyExpr ⊕ Bool
    Prim := EarlyPrim
    Event := Empty
    Store := Hybrid.State
    unitVal := .scalar .unit
    unitTy := .inl (.scalar .unit)
    isCopy := fun ty => match ty with | .inl (.scalar _) => true | _ => false
    eval := fun locals store expr => match expr with
      | .inl e => (earlyEval locals e).map fun value => (value, store)
      | .inr _ => .stuck
    life := fun _ store => some store
    test := fun | .scalar (.bool value) => some value | _ => none
    allows := fun _ _ _ _ => false }
  hybridStore := rfl
  Declaration := Bool
  extraEarlyTypes := Bool
  extraEarlyExpressions := Bool
  expressionOperator
    | .inl (.add ..) => .arithmetic (.plain .add)
    | .inl (.constructEmpty _) => .named ⟨"constructEmpty"⟩
    | _ => .read
  primitiveOperator := fun _ => .named ⟨"destructureEmpty"⟩
  argumentRead := fun _ => none
  argumentReadNoTrap := by simp
  OtherValue := DeclId
  valueShape := rfl
  layout target
    | .inl (.scalar (.int ty)) => do
      if target ≠ linuxX8664 then none else
      let .stored storage ← Slice0Layout.query Slice0Layout.linuxX8664Profile (.integer ty) | none
      some ⟨storage.byteSize, storage.align⟩
    | _ => none
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
  activeFields := fun _ _ => none
  operation := fun op inputs output values _ addresses before result effects after =>
    addresses = [] ∧ earlyOperation op inputs output values = .ok result ∧
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
    | .inl (.read read) => earlyPermitRead own read
    | .inl (.add _ left right) => (earlyPermitRead own left).bind (earlyPermitRead · right)
    | _ => none

theorem sliceEarlyCore (slice : Slice) : EarlyCore (sliceLanguage slice) := ⟨rfl, rfl⟩


structure Payload (language : CoreLanguage) where
  ownership : AList Var Own.Descriptor
  body : Exec.Program (language.instrumented ownership)
  entry : FnId
  origins : AList SiteId Origin
  declarations : AList DeclId language.Declaration

structure EarlyProgram where
  bindings : List (ConcreteSpec.Slice0.Stmt × Surface0.Span)
  ret : ConcreteSpec.Slice0.Atom
  returnSpan : Surface0.Span

def EarlyProgram.checkedSyntax (program : EarlyProgram) : ConcreteSpec.Program :=
  ⟨[], program.bindings.map (ConcreteSpec.Slice0.Stmt.toCore ∘ Prod.fst), .ret program.ret.toCore⟩

private def earlyOperand : ConcreteSpec.Slice0.Atom → EarlyOperand
  | .lit n => .literal (.int .u8 n)
  | .var id => .copy id

private def earlyExpression : ConcreteSpec.Slice0.Expr → EarlyExpr
  | .atom atom => .read (earlyOperand atom)
  | .add left right => .add .u8 (earlyOperand left) (earlyOperand right)

private def earlyOrigin (entry : FnId) (site : SiteId) (kind : ConstructKind)
    (precedingBindings : Nat) (span : ConcreteSpec.Surface0.Span) : SiteId × Origin :=
  let binding : KeyConstruct := ⟨.binding, .unnamed none, []⟩
  let construct : KeyConstruct := ⟨kind, .unnamed none, []⟩
  (site, ⟨site, ⟨span.fileId, span.start, span.stop, span.line, span.column⟩,
    ⟨.function entry, construct.keyAfterCopies binding precedingBindings⟩⟩)

/-- The standalone Slice 0 program has one internal function identity. -/
def earlyEntry : FnId :=
  ⟨⟨⟨⟨"standalone"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩, ⟨"main"⟩⟩

/-- Embed the existing Slice 0 elaboration in the integrated carrier. -/
def earlyPayload {language : CoreLanguage} (early : EarlyCore language)
    (out : EarlyProgram) : Payload language :=
  let entry := earlyEntry
  let scalar : CTy := .int .u8
  let ownership : AList ConcreteSpec.Var Own.Descriptor :=
    out.bindings.map fun (bind, _) => (bind.x, ⟨scalar, true, false⟩)
  let retSite : SiteId := ⟨out.bindings.length⟩
  let body : Exec.Stmt (language.instrumented ownership) :=
    out.bindings.foldr (fun (bind, _) rest =>
      .seq (.bind bind.x (early.expressions.symm ▸ Sum.inl (earlyExpression bind.e)) ⟨bind.x.id⟩) rest)
      (.ret (early.expressions.symm ▸ Sum.inl (EarlyExpr.read (earlyOperand out.ret))) retSite)
  let origins := out.bindings.mapIdx fun index (bind, span) =>
    earlyOrigin entry ⟨bind.x.id⟩ .binding index span
  ⟨ownership, ⟨[(entry, ⟨[], early.types.symm ▸ Sum.inl (EarlyTy.scalar scalar), ⟨0⟩, body⟩)]⟩,
    entry, origins ++ [earlyOrigin entry retSite .return out.bindings.length out.returnSpan], []⟩

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

/-- Recognize Slice 0 in the integrated carrier by its canonical embedding.
New spec fragments add their own cases when their integrated syntax is defined. -/
def CoreInput.EarlyProgram {language : CoreLanguage} (input : CoreInput language)
    (program : EarlyProgram) : Prop :=
  input.slice = slice0 ∧ ∃ early : EarlyCore language, input.payload = earlyPayload early program

def CoreInput.InFragment {language : CoreLanguage} (input : CoreInput language) : Prop :=
  ∃ program, input.EarlyProgram program

private def earlyBodyTyped (locals : TyCtx) : List ConcreteSpec.Slice0.Stmt → ConcreteSpec.Slice0.Atom → Prop
  | [], ret => AtomTy locals ret.toCore (.int .u8)
  | bind :: rest, ret =>
    HasType [] locals bind.e.toCore (.scalar (.int .u8)) ∧
      earlyBodyTyped ((bind.x, .scalar (.int .u8)) :: locals) rest ret

/-- Independent typing for the unchecked path. A typed addition may still overflow. -/
def CoreInput.StaticallyTyped {language : CoreLanguage} (input : CoreInput language) : Prop :=
  ∃ program, input.EarlyProgram program ∧ earlyBodyTyped [] (program.bindings.map Prod.fst) program.ret

/-- Acceptance uses the spec's independent checker judgment on the same fragment. -/
def CoreInput.Accepted {language : CoreLanguage} (input : CoreInput language) : Prop :=
  ∃ program, input.EarlyProgram program ∧ Checked program.checkedSyntax

def CoreInput.CheckerRefuses {language : CoreLanguage} (input : CoreInput language)
    (failure : CheckerRefusal) : Prop :=
  ¬ input.Accepted ∧ failure.rule = ⟨``ProgramPermitted, 1⟩ ∧
    failure.related = [] ∧ failure.obligation = none ∧
    ∃ site origin, (site, origin) ∈ input.payload.origins ∧ failure.primary = origin.lexical

def Payload.ConditionsAt {language : CoreLanguage} (payload : Payload language) (site : SiteId)
    (opcode : Opcode) (conditions : List (SafetyCondition language.operands.Ty)) : Prop :=
  ∃ op, (site, op) ∈ payload.operations ∧ (match op with | .abort => False | _ => True) ∧
    language.opcode op = opcode ∧
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

def Admitted.Valid {language : CoreLanguage} (admitted : Admitted language) : Prop :=
  admitted.core.WF ∧ admitted.core.Accepted ∧ admitted.core.StaticallyTyped ∧
  ValidDischarges admitted.core admitted.discharges

def Admits {language : CoreLanguage} (input : CoreInput language) (out : Admitted language) : Prop :=
  out.core = input ∧ input.Accepted ∧ input.StaticallyTyped ∧ ValidDischarges input out.discharges

def AdmissionSound {language : CoreLanguage}
    (check : CoreInput language → Except CheckerRefusal (Discharges language)) : Prop :=
  ∀ input, input.WF → input.InFragment →
    (input.Accepted ↔ ∃ discharges, check input = .ok discharges) ∧
    (∀ discharges, check input = .ok discharges →
      input.StaticallyTyped ∧ ValidDischarges input discharges) ∧
    (∀ refusal, check input = .error refusal → input.CheckerRefuses refusal)

end ConcreteSpec.StageInterface
