import Concrete.Interfaces.Carriers
import Concrete.Lex
import Concrete.Interfaces.Behavior
import Concrete.Backend.Observation
import Concrete.Interfaces.SurfaceElaboration
import ConcreteSpec.StageGrammar

namespace Concrete.Interfaces

open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface
open Concrete.Contracts (ProcessEvent Semantics EvidenceKind Graph)

inductive Observation where
  | returned (status : UInt8) (stderr : ByteArray)
  | trapped (status : UInt8) (kind : TrapKind) (site : FileSpan) (stderr : ByteArray)
  | startupRefused (status : UInt8) (stderr : ByteArray)
  | resourceExhausted

def Observation.stderr : Observation → ByteArray
  | .returned _ bytes | .trapped _ _ _ bytes | .startupRefused _ bytes => bytes
  | .resourceExhausted => ByteArray.empty

open Concrete.Contracts (Steps)

/-- Match representation checkpoints along target executions. The source
can choose a path that follows the target's allocation and control decisions.
A target step that takes no source steps strictly decreases the rank. -/
structure CheckpointSimulation {A B E O : Type} (s : Semantics A E) (t : Semantics B E)
    (sourceResult : A → s.State → O → Prop) (targetResult : B → t.State → O → Prop)
    (a : A) (b : B) where
  related : s.State → t.State → Prop
  rank : s.State → t.State → Nat
  targetInitial : ∀ q, t.initial b q → ∃ p, s.initial a p ∧ related p q
  targetAdvance : ∀ p q q', s.finite a p → t.finite b q → related p q → t.step b q q' →
    let atOld := fun x y => s.step a x y ∧ related y q
    (∃ before after, Steps atOld p before ∧ s.step a before after ∧ related after q') ∨
    (related p q' ∧ rank p q' < rank p q)
  targetFinish : ∀ p q o, s.finite a p → t.finite b q → related p q → targetResult b q o →
    ∃ p', Steps (fun x y => s.step a x y ∧ related y q) p p' ∧
      sourceResult a p' o ∧ s.trace a p' = t.trace b q

/-- The registry owns the function value and uses it for both execution and the proof graph. -/
structure Stage (Input Failure Output : Type) where
  run : Input → Except Failure Output
  kind : EvidenceKind
  inputInvariant : Input → Prop
  outputInvariant : Output → Prop
  success : Input → Output → Prop
  refusal : Input → Failure → Prop
  trust : Input → Output → Prop := fun _ _ => True

def Stage.Trusted {A R B : Type} (edge : Stage A R B) : Prop :=
  ∀ a b, edge.inputInvariant a → Graph edge.run a b → edge.trust a b

def Stage.Validated {A R B : Type} (edge : Stage A R B) : Prop :=
  (∀ a b, edge.inputInvariant a → Graph edge.run a b →
    edge.outputInvariant b ∧ edge.success a b) ∧
  (∀ a r, edge.inputInvariant a → edge.run a = .error r → edge.refusal a r)

def Stage.Checks {A R B : Type} (edge : Stage A R B) : Prop :=
  edge.Validated ∧ edge.Trusted

def Stage.Compatible {A R B S C : Type} (left : Stage A R B) (right : Stage B S C) : Prop :=
  ∀ b, left.outputInvariant b → right.inputInvariant b

/-- Composition uses heterogeneous stage entries directly, including their refusal channels. -/
inductive Pipeline : Type → Type → Type 1 where
  | done {A : Type} : Pipeline A A
  | next {A R B C : Type} (edge : Stage A R B)
      (rest : Pipeline B C) : Pipeline A C

def Pipeline.run {A C : Type} (pipeline : Pipeline A C) (input : A) :
    Except (Σ R : Type, R) C :=
  match pipeline with
  | .done => .ok input
  | .next stage rest =>
    match stage.run input with
    | .error failure => .error ⟨_, failure⟩
    | .ok output => rest.run output

private def fileSpan (span : ConcreteSpec.Surface0.Span) : FileSpan :=
  ⟨span.fileId, span.start, span.stop, span.line, span.column⟩

private def decodedValue : Concrete.Lex.LosslessValue → Option TokenValue
  | .parsed (.fixed kind) => if h : Terminal.HasPayload kind = false then some (.fixed kind h) else none
  | .parsed (.name name) => some (.name name)
  | .parsed (.integer value radix suffix) => some (.integer value radix suffix)
  | .stringLiteral text => some (.stringLiteral text.toUTF8.data.toList)
  | .byteLiteral byte => some (.byteLiteral byte)
  | .label name => some (.label ⟨name⟩)

private def referencePiece : Concrete.Lex.Piece → Option (FileSpan × Option TokenValue)
  | .token value span => (decodedValue value).map fun token => (fileSpan span, some token)
  | .whitespace span | .lineComment span => some (fileSpan span, none)

private def retainedPieces (stream : TokenStream) : List (FileSpan × Option TokenValue) :=
  (stream.tokens.toList.zipIdx.flatMap fun (token, index) =>
    (stream.trivia[index]?.getD #[]).toList.map (fun piece => (piece.span, none)) ++
      [(token.decoded.span, some token.decoded.value)]) ++
    (stream.trivia[stream.tokens.size]?.getD #[]).toList.map (fun piece => (piece.span, none))

/-- The proved lossless lexer gives the fixed lexical meaning of the retained bytes,
including token boundaries, trivia and the full decoded payloads. -/
def Lexes (source : Source) (stream : TokenStream) : Prop :=
  stream.source = source ∧ stream.parserView.WF ∧
  ∃ file, source.project.files[source.entryFile.index]? = some file ∧
    ∃ bytes, reconstruct source.entryFile stream.tokens stream.trivia = some bytes ∧
      bytes.data.toList = file.source.toBytes ∧
      ∃ reference, Concrete.Lex.lexLossless bytes source.entryFile = .ok reference ∧
        reference.pieces.mapM referencePiece = some (retainedPieces stream)

def LexRefuses (source : Source) (failure : LexRefusal) : Prop :=
  match source.project.fileAt source.entryFile with
  | none => failure = .missingFile source.entryFile
  | some file =>
    let bytes : ByteArray := ⟨file.source.toBytes.toArray⟩
    match failure with
    | .missingFile _ => False
    | .malformed diagnostic =>
      Concrete.Lex.lexLossless bytes source.entryFile = .error (.syntaxError diagnostic)
    | .digitLimit offset limit =>
      Concrete.Lex.lexLossless bytes source.entryFile = .error (.lexerDigitLimit offset limit)

/-- Each slice contributes a repository rule over that member of the AST and Core
families. Slice 0 uses the canonical injections of the shared carriers. -/
def SliceElaboration (slice : Slice) (ast : SurfaceAST ConcreteSpec.StageGrammar.sliceASTExtra slice)
    (result : Except ElabRefusal (Payload (sliceLanguage slice))) : Prop :=
  if slice = slice0 then
    match ast with
    | .inl tree => ∃ program, Concrete.Elab.surfaceProgram tree = some program ∧
        ((Concrete.Elab.elaborateLocated program).map (earlyPayload (sliceEarlyCore slice))).mapError
          ElabRefusal.slice0 = result
    | .inr _ => False
  else result = .error (.unsupportedSlice slice)

/-- Origin maps enumerate emitted copies as well as discharged and residual operations. -/
def PreservesOrigins (source target : List Origin) : Prop :=
  (∀ o ∈ target, o ∈ source) ∧ (∀ o ∈ source, o ∈ target)

def CFG.origins {P I T : Type} (graph : CFG P (LocatedOp I) T)
    (termOrigins : T → List Origin) : List Origin :=
  graph.blocks.toList.flatMap fun block =>
    block.body.map LocatedOp.origin ++ block.boundaryOrigins ++ termOrigins block.ending

def Terminator.origins {A C : Type} : Terminator A C → List Origin
  | .trap _ origin => [origin]
  | _ => []

def Read.identity {Ty : Type} {ty : Ty} : Read Ty (Literal Ty) ty → OperandRead
  | .literal value => .literal value.encoding
  | .copy place => .copy place.root place.path
  | .move place => .move place.root place.path

def Args.identities {Ty : Type} {types : List Ty} :
    Args (Read Ty (Literal Ty)) types → List (Ty × OperandRead)
  | .nil => []
  | @Args.cons _ _ ty _ read rest => (ty, read.identity) :: rest.identities

structure MIR.Occurrence (Ty : Type) where
  fn : FnId
  block : BlockId
  index : Nat
  origin : Origin
  opcode : Opcode
  operands : List (Ty × OperandRead)
  checks : List OperationCheck

def MIR.operations {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (mir : MIR lang profile) : List (MIR.Occurrence Ty) :=
  mir.functions.flatMap fun (fn, _, graph) =>
    graph.blocks.toList.flatMap fun block =>
      block.body.zipIdx.filterMap fun (inst, index) => match inst.instruction with
        | @MIRInst.op _ _ opcode _ _ _ _ args checks addresses =>
          some ⟨fn, block.id, index, inst.origin, .operator opcode,
            args.identities ++ addresses.map (fun p => (p.1, .address p.2.root p.2.path)), checks⟩
        | .call callee _ args checks =>
          some ⟨fn, block.id, index, inst.origin, .call callee,
            args.map (fun arg => (arg.1, arg.2.identity)), checks⟩
        | _ => none

def MIR.Occurrence.Justifies {language : CoreLanguage}
    (op : MIR.Occurrence language.operands.Ty) (check : OperationCheck)
    (d : Discharge language) : Prop :=
  check.disposition = .discharged ∧ op.origin.operation = d.operation ∧
  op.origin.evidenceKey = d.key ∧ check.discriminator = d.obligation.discriminator ∧
  check.trap = d.trap ∧ op.operands = d.operands

/-- Enumerate actual emitted operations. Each Core condition is handled at that occurrence,
and every discharge is consumed once at an occurrence with exactly the same typed reads. -/
def ConsumesEvidence {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (core : CoreInput language) (discharges : Discharges language)
    (mir : MIR lang profile) : Prop :=
  ((mir.operations.map fun op => (op.fn, op.block, op.index)).Nodup) ∧
  (∀ site opcode conditions, core.payload.ConditionsAt site opcode conditions → conditions ≠ [] →
    ∃ op ∈ mir.operations, op.origin.operation = site ∧ op.opcode = opcode) ∧
  (∀ op ∈ mir.operations,
    ∃ conditions, core.payload.ConditionsAt op.origin.operation op.opcode conditions ∧
      (op.checks.map OperationCheck.discriminator).Nodup ∧
      conditions = op.checks.map (fun check => ⟨check.discriminator, check.trap, op.operands⟩) ∧
      (∀ callee, op.opcode = .call callee →
        ∃ args, (op.origin.operation, .call callee args) ∈ core.payload.operations ∧
          args.mapM language.argumentRead = some op.operands) ∧
      ∀ check ∈ op.checks, check.disposition = .discharged →
        ∃ d, discharges.Contains op.origin.operation d ∧ op.Justifies check d) ∧
  (∀ site d, discharges.Contains site d →
    ∃ op ∈ mir.operations, ∃ check ∈ op.checks, op.Justifies check d ∧
      ∀ other ∈ mir.operations, ∀ c ∈ other.checks, other.Justifies c d →
        other = op ∧ c = check) ∧ core.ArgumentsWF

def MIR.origins {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (mir : MIR lang profile) : List Origin :=
  mir.functions.flatMap fun fn => fn.2.2.origins Terminator.origins

def Memory.origins {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (memory : Memory lang profile) : List Origin :=
  memory.functions.flatMap fun fn => fn.2.2.origins Terminator.origins

def SSA.origins {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (ssa : SSA lang profile) : List Origin :=
  ssa.functions.flatMap fun fn => fn.2.2.origins Terminator.origins

def BIR.origins {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) : List Origin :=
  bir.functions.flatMap fun fn => fn.2.2.origins fun
    | .trap slot => (bir.sites[slot.index]?.map TrapEntry.origin).toList
    | _ => []

def BIR.TrapSlotsWF {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) : Prop :=
  ∀ fn ∈ bir.functions, ∀ block ∈ fn.2.2.blocks.toList, ∀ slot,
    block.ending = .trap slot → ∃ entry, bir.sites[slot.index]? = some entry

/-- Every materialized root gets the spec layout. Virtual roots need no byte allocation. -/
def LayoutsMatch {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (layout : LayoutOf Ty) (memory : Memory lang profile) : Prop :=
  (∀ fn locals graph, (fn, locals, graph) ∈ memory.functions →
    ∀ place ty, (place, ty) ∈ locals →
      ∃ classes, AList.lookup memory.storage fn = some classes ∧
        (AList.lookup classes place = some .virtual ∨
          (AList.lookup classes place = some .materialized ∧
            ∃ slots description, AList.lookup memory.layouts fn = some slots ∧
              AList.lookup slots place = some description ∧
              layout profile.memoryTarget ty = some description))) ∧
  ∀ fn slots place description, (fn, slots) ∈ memory.layouts → (place, description) ∈ slots →
    ∃ locals graph ty classes, (fn, locals, graph) ∈ memory.functions ∧ (place, ty) ∈ locals ∧
      AList.lookup memory.storage fn = some classes ∧
      AList.lookup classes place = some .materialized ∧ layout profile.memoryTarget ty = some description

def MIR.Escapes {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (mir : MIR lang profile) (fn : FnId) (place : PlaceId) : Prop :=
  ∃ op ∈ mir.operations, op.fn = fn ∧
    ∃ ty path, (ty, .address place path) ∈ op.operands

def Memory.SignatureMatches {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (memory : Memory lang profile) (fn : FnId) (graph : MemoryGraph lang) : Prop :=
  ∃ signature, AList.lookup memory.signatures fn = some signature ∧
    (∃ entry ∈ graph.blocks.toList, entry.id = graph.entry ∧
      entry.params.map Sigma.fst = signature.parameters) ∧
    ∀ block ∈ graph.blocks.toList,
      (match block.ending with
      | .ret operand => operand.map Sigma.fst = signature.result
      | .invalid => False
      | _ => True) ∧
      ∀ op ∈ block.body, match op.instruction with
        | .call callee destination args _ =>
          ∃ target, AList.lookup memory.signatures callee = some target ∧
            args.map Sigma.fst = target.parameters ∧ destination.map Sigma.fst = target.result
        | _ => True

def Memory.TablesWF {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (memory : Memory lang profile) : Prop :=
  (List.map Prod.fst memory.functions).Nodup ∧ (List.map Prod.fst memory.signatures).Nodup ∧
  (List.map Prod.fst memory.storage).Nodup ∧ (List.map Prod.fst memory.layouts).Nodup ∧
  (∀ fn classes, (fn, classes) ∈ memory.storage → (List.map Prod.fst classes).Nodup) ∧
  (∀ fn layouts, (fn, layouts) ∈ memory.layouts → (List.map Prod.fst layouts).Nodup) ∧
  ∀ fn locals graph, (fn, locals, graph) ∈ memory.functions →
    (List.map Prod.fst locals).Nodup ∧ graph.WF Terminator.successors Sigma.fst Sigma.fst ∧
    memory.SignatureMatches fn graph

def PromotionSound {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (mir : MIR lang profile) (memory : Memory lang profile) : Prop :=
  (∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
    ∀ place ty, (place, ty) ∈ locals →
      ∃ classes storage, AList.lookup memory.storage fn = some classes ∧
        AList.lookup classes place = some storage ∧
        (storage = .virtual ↔ (language.scalarType ty).isSome ∧ ¬ mir.Escapes fn place)) ∧
  ∀ fn classes place, (fn, classes) ∈ memory.storage → (place, .virtual) ∈ classes →
    ∃ ty source sourceGraph target targetGraph,
      (fn, source, sourceGraph) ∈ mir.functions ∧ (place, ty) ∈ source ∧
      (fn, target, targetGraph) ∈ memory.functions ∧ (place, ty) ∈ target ∧
      (language.scalarType ty).isSome ∧ ¬ mir.Escapes fn place

/-- The logical MIR frame used to verify Memory's representation of live places. -/
structure LogicalFrame (Value : Type) where
  fn : FnId
  live : List PlaceId
  values : AList PlaceId Value
  movedFields : List (PlaceId × List FieldId) := []

abbrev MemoryRegisters (language : CoreLanguage) (profile : Profile) :=
  MemoryState language.operands.Ty language.operands.Val language.operands.Event profile

def MemoryRegisters.advance {language : CoreLanguage} {profile : Profile}
    (state : MemoryRegisters language profile) : MemoryRegisters language profile :=
  {state with instruction := state.instruction + 1}

namespace Memory

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
  {profile : Profile}

def graphAt (memory : Memory lang profile) (fn : FnId) : Option (MemoryGraph lang) :=
  (AList.lookup memory.functions fn).map Prod.snd

def blockAt (memory : Memory lang profile) (state : MemoryRegisters language profile) :
    Option (MemoryBlock lang) := do
  let graph ← memory.graphAt state.frame
  graph.blocks.find? (·.id == state.block)

def classAt (memory : Memory lang profile) (fn : FnId) (place : PlaceId) :
    Option StorageClass := do
  let classes ← AList.lookup memory.storage fn
  AList.lookup classes place

def layoutAt (memory : Memory lang profile) (fn : FnId) (place : PlaceId) : Option Layout := do
  let layouts ← AList.lookup memory.layouts fn
  AList.lookup layouts place

def rootRange (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) (range : Hybrid.Range) : Prop :=
  ∃ layout blockId block,
    memory.layoutAt state.frame place = some layout ∧ range.extent = layout.size ∧
    range.align = layout.align ∧ range.ptr.prov = some blockId ∧
    state.bytes.block blockId = some block ∧ block.live = true ∧
    block.space = range.ptr.space ∧ block.base = range.ptr.addr ∧
    block.size = range.extent ∧ block.align = range.align

def rootType (memory : Memory lang profile) (fn : FnId) (place : PlaceId) :
    Option language.operands.Ty := do
  let (locals, _) ← AList.lookup memory.functions fn
  AList.lookup locals place

def validPlace (memory : Memory lang profile) (state : MemoryRegisters language profile)
    {ty : language.operands.Ty} (place : MemoryPlace _ ty) : Bool :=
  letI := language.typeDecidableEq
  decide (memory.classAt state.frame place.logical.root = some place.storage ∧
    (do
      let rootType ← memory.rootType state.frame place.logical.root
      place.logical.path.foldlM language.fieldType rootType) = some ty)

def rangeAt (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty} (place : Place _ ty) :
    Option Hybrid.Range := do
  let root ← AList.lookup state.materialized place.root
  if place.path.isEmpty then return root
  let rootType ← memory.rootType state.frame place.root
  let (offset, field) ← language.fieldRange rootType place.path ⟨root.extent, root.align⟩
  if offset + field.size ≤ root.extent then
    some ⟨{root.ptr with addr := root.ptr.addr + offset}, field.size, field.align⟩
  else none

def zeroRoot (memory : Memory lang profile) (frame : FnId) (root : PlaceId) : Bool :=
  (memory.layoutAt frame root).any (fun layout => layout.size == 0)

private def pathsOverlap (left right : List FieldId) : Bool :=
  left.isPrefixOf right || right.isPrefixOf left

def initializedAt (state : MemoryRegisters language profile) (root : PlaceId)
    (path : List FieldId) : Bool :=
  state.initialized.contains root &&
    !(state.movedFields.any fun (owner, moved) => owner == root && pathsOverlap path moved)

/-- Select the owner whose permission is needed. Storage tracks initialization at
field granularity while the spec tracks loan exclusion at the root. The slot view
uses the selected field's Copy/status information without changing the loan root.
Unaddressed owners use the next unused identity for the permission check. -/
def ownerView (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (root : PlaceId) (ty : language.operands.Ty) (status : Own.Status) :
    Own.State × Hybrid.Place :=
  let id := (AList.lookup state.addressIds root).getD
    ⟨max state.nextAddressId state.ownership.nextPlace⟩
  let slot := Own.slot state.ownership id
  let mutable := (slot.map (·.desc.mutable)).getD
    (((AList.lookup memory.mutableLocals state.frame).getD []).contains root)
  let desc : Own.Descriptor :=
    ⟨(language.scalarType ty).getD .unit, language.operands.isCopy ty, mutable⟩
  let activation := (slot.map (·.activation)).getD 0
  let status := if (slot.map (·.status)).getD .owned == .moved then .moved else status
  ({state.ownership with slots := ⟨id, activation, desc, status⟩ ::
    state.ownership.slots.filter (·.place != id)}, id)

/-- Commit the root status from the spec transition to an existing instrumented
slot. A field transition only changes the interpreter's field liveness. -/
private def commitOwner (state : MemoryRegisters language profile) (root : PlaceId)
    (owners : Own.State) : Own.State :=
  match AList.lookup state.addressIds root with
  | none => state.ownership
  | some id =>
    {state.ownership with slots := state.ownership.slots.map fun slot =>
      if slot.place == id then {slot with status := ((Own.slot owners id).map (·.status)).getD slot.status}
      else slot}

private def loadBacking (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) : Option language.operands.Val := do
  if !(memory.validPlace state place) then none else
  if !state.live.contains place.logical.root then none else
  match place.storage with
  | .virtual =>
    let root ← AList.lookup state.virtualValues place.logical.root
    if place.logical.path.isEmpty then some root else language.field root place.logical.path
  | .materialized =>
    if memory.zeroRoot state.frame place.logical.root then do
      let root ← AList.lookup state.zeroValues place.logical.root
      if place.logical.path.isEmpty then some root else language.field root place.logical.path
    else do
      let range ← memory.rangeAt state place.logical
      let bytes ← state.bytes.load profile.memoryTarget range.ptr range.extent range.align
      match language.scalarType ty with
      | some scalar =>
        let value ← Hybrid.decode profile.memoryTarget scalar bytes
        return language.valueShape.symm ▸ CoreValue.scalar value
      | none =>
        let value ← language.decodeOther profile.memoryTarget ty bytes
        return language.valueShape.symm ▸ CoreValue.other value

def loadValue (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) : Option language.operands.Val :=
  if initializedAt state place.logical.root place.logical.path then memory.loadBacking state place
  else none

def byteRuns
    (target : Target) (ty : language.operands.Ty) (value : language.operands.Val) :
    Option (List (Nat × List Mem.MByte)) :=
  match language.valueShape ▸ value with
  | .scalar scalar => (Hybrid.encode target scalar).map fun bytes => [(0, bytes)]
  | .other other => language.encodeOther target ty other

private def StoresValue (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (value : language.operands.Val) : Prop :=
  memory.loadValue state place = some value ∧
  (place.storage = .materialized →
    memory.zeroRoot state.frame place.logical.root = true ∨
    ∃ range runs, memory.rangeAt state place.logical = some range ∧
      byteRuns profile.memoryTarget ty value = some runs ∧
      (0 < range.extent → ∃ offset bytes, (offset, bytes) ∈ runs ∧ bytes ≠ []) ∧
      ∀ offset bytes, (offset, bytes) ∈ runs →
        offset + bytes.length ≤ range.extent ∧ (∀ byte ∈ bytes, byte ≠ .uninit) ∧
        state.bytes.load profile.memoryTarget {range.ptr with addr := range.ptr.addr + offset}
          bytes.length 1 = some bytes)

/-- Ignore moved subtrees when checking for live linear fields at lifetime end or replacement. -/
private def canDiscard (language : CoreLanguage) (allowCopy : Bool := true) :
    Nat → language.operands.Ty → language.operands.Val → List (List FieldId) → Bool
  | fuel, ty, value, moved =>
    if moved.contains [] then true
    else if moved.isEmpty then allowCopy && language.operands.isCopy ty
    else match fuel with
      | 0 => false
      | n + 1 =>
        (language.activeFields ty value).any fun fields =>
          fields.all fun field =>
            let below := moved.filterMap fun path =>
              match path with
              | first :: rest => if first == field then some rest else none
              | [] => none
            match language.fieldType ty field, language.field value [field] with
            | some fieldTy, some fieldValue => canDiscard language allowCopy n fieldTy fieldValue below
            | _, _ => false

private def movedBelow (state : MemoryRegisters language profile)
    (root : PlaceId) (path : List FieldId) : List (List FieldId) :=
  state.movedFields.filterMap fun (owner, moved) =>
    if owner == root && path.isPrefixOf moved then some (moved.drop path.length) else none

private def discardRemainder (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (allowCopy : Bool) : Bool :=
  let moved := movedBelow state place.logical.root place.logical.path
  !moved.isEmpty && (memory.loadBacking state place).any fun value =>
    canDiscard language allowCopy (moved.foldl (fun depth suffix => max depth suffix.length) 0)
      ty value moved

def mayInitialize (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) : Bool :=
  let root := place.logical.root
  let path := place.logical.path
  if !state.initialized.contains root then path.isEmpty
  else if initializedAt state root path then true
  else if state.movedFields.any (fun (owner, moved) =>
      owner == root && moved != path && moved.isPrefixOf path) then false
  else if state.movedFields.contains (root, path) then true
  else memory.discardRemainder state place true

/-- A write's owner view records whether any active part of the destination
still holds a value. A partially moved destination can have a discardable
remainder, but replacing that remainder requires mutable owner access. -/
def writeOwnerView (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) : Own.State × Hybrid.Place :=
  let root := place.logical.root
  let path := place.logical.path
  let empty := !state.initialized.contains root ||
    state.movedFields.any (fun (owner, moved) => owner == root && moved.isPrefixOf path) ||
    memory.discardRemainder state place false
  memory.ownerView state root ty (if empty then .moved else .owned)

/-- A normal reinitialization uses the selected slot directly. Replacing a
live remainder requires both fieldwise discharge and mutable owner access. -/
def authorizeWrite (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) : Option Own.State :=
  let (owners, id) := memory.writeOwnerView state place
  if memory.discardRemainder state place true && Own.ownerAccess owners id .mutable then
    some owners
  else Own.reinitialize owners id

def writeValue (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (value : language.operands.Val) :
    Option (MemoryRegisters language profile) := do
  let root := place.logical.root
  let path := place.logical.path
  if !state.live.contains root || !memory.mayInitialize state place then none else
  let owners ← memory.authorizeWrite state place
  let state := {state with
    ownership := if path.isEmpty then commitOwner state root owners else state.ownership
    initialized := if state.initialized.contains root then state.initialized else root :: state.initialized
    movedFields := state.movedFields.filter (fun (owner, moved) =>
      owner != root || !path.isPrefixOf moved)}
  match place.storage with
  | .virtual =>
    let stored ← if place.logical.path.isEmpty then some value else do
      let before ← AList.lookup state.virtualValues root
      language.replaceField before place.logical.path value
    return {state with virtualValues := (root, stored) :: AList.erase state.virtualValues root}
  | .materialized =>
    if memory.zeroRoot state.frame root then
      let stored ← if place.logical.path.isEmpty then some value else do
        let before ← AList.lookup state.zeroValues root
        language.replaceField before place.logical.path value
      return {state with zeroValues := (root, stored) :: AList.erase state.zeroValues root}
    else do
      let range ← memory.rangeAt state place.logical
      let runs ← byteRuns profile.memoryTarget ty value
      let bytes ← runs.foldlM (fun mem (offset, bytes) =>
        if offset + bytes.length ≤ range.extent then
          mem.store profile.memoryTarget {range.ptr with addr := range.ptr.addr + offset} bytes 1
        else none) state.bytes
      return {state with bytes := bytes}

def readValue (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (read : MemoryRead _ lang.literal ty) :
    Option (language.operands.Val × MemoryRegisters language profile) := do
  match read with
  | .literal value => do
    let decoded ← match language.scalarType ty with
      | some (.int intTy) =>
        (Hybrid.decode profile.memoryTarget (.int intTy)
          (value.encoding.map fun byte => .init byte.toFin none)).map
          (fun scalar => language.valueShape.symm ▸ CoreValue.scalar scalar)
      | _ => language.literal ty value.encoding
    return (decoded, state)
  | .copy place => do
    let root := place.logical.root
    let (owners, id) := memory.ownerView state root ty .owned
    if !(language.operands.isCopy ty && Own.ownerAccess owners id .shared) then none else
    return (← memory.loadValue state place, state)
  | .move place => do
    let root := place.logical.root
    let (owners, id) := memory.ownerView state root ty .owned
    let owners ← Own.move owners id
    let value ← memory.loadValue state place
    if !place.logical.path.isEmpty then
      return (value, {state with movedFields := (root, place.logical.path) :: state.movedFields})
    let moved := {state with
      ownership := commitOwner state root owners
      initialized := state.initialized.erase root
      virtualValues := AList.erase state.virtualValues root
      zeroValues := AList.erase state.zeroValues root}
    return (value, moved)

theorem copyRead_iff (memory : Memory lang profile) (state : MemoryRegisters language profile)
    {ty : language.operands.Ty} (place : MemoryPlace _ ty) (value : language.operands.Val)
    (next : MemoryRegisters language profile) :
    memory.readValue state (.copy place) = some (value, next) ↔
      let view := memory.ownerView state place.logical.root ty .owned
      language.operands.isCopy ty = true ∧ Own.ownerAccess view.1 view.2 .shared = true ∧
        memory.loadValue state place = some value ∧ next = state := by
  simp only [readValue]
  generalize (memory.ownerView state place.logical.root ty .owned) = view
  cases language.operands.isCopy ty <;>
    cases Own.ownerAccess view.1 view.2 .shared <;>
    cases memory.loadValue state place <;> simp [eq_comm]

theorem moveRead_iff (memory : Memory lang profile) (state : MemoryRegisters language profile)
    {ty : language.operands.Ty} (place : MemoryPlace _ ty) :
    (memory.readValue state (.move place)).isSome = true ↔
      let view := memory.ownerView state place.logical.root ty .owned
      (Own.move view.1 view.2).isSome = true ∧ (memory.loadValue state place).isSome = true := by
  simp only [readValue]
  generalize (memory.ownerView state place.logical.root ty .owned) = view
  cases Own.move view.1 view.2 <;> cases memory.loadValue state place <;> simp
  split <;> rfl

theorem write_authorized (memory : Memory lang profile) (state : MemoryRegisters language profile)
    {ty : language.operands.Ty} (place : MemoryPlace _ ty) (value : language.operands.Val) {next} :
    memory.writeValue state place value = some next →
      let (owners, id) := memory.writeOwnerView state place
      (Own.reinitialize owners id).isSome = true ∨
        (memory.discardRemainder state place true = true ∧ Own.ownerAccess owners id .mutable = true) := by
  simp only [writeValue]
  split
  · simp
  · simp only [authorizeWrite]
    generalize memory.writeOwnerView state place = view
    obtain ⟨owners, id⟩ := view
    dsimp only
    split
    · rename_i permitted
      exact fun _ => Or.inr (Bool.and_eq_true_iff.mp permitted)
    · cases Own.reinitialize owners id <;> simp

def Writes (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (value : language.operands.Val)
    (next : MemoryRegisters language profile) : Prop :=
  (memory.validPlace state place = true ∧
    (initializedAt state place.logical.root place.logical.path = true →
      language.operands.isCopy ty = true)) ∧
  (place.storage = .materialized → ∃ layout,
    memory.layoutAt state.frame place.logical.root = some layout ∧
    (layout.size = 0 ↔ (AList.lookup state.materialized place.logical.root).isNone)) ∧
  memory.writeValue state place value = some next ∧
  memory.StoresValue next place value

def Represents (memory : Memory lang profile)
    (logical : LogicalFrame language.operands.Val) (state : MemoryRegisters language profile)
    (ranges : AList Hybrid.Place (Option Hybrid.Range) := []) : Prop :=
  (state.BytesWF ∧ logical.movedFields = state.movedFields) ∧ logical.fn = state.frame ∧ (∀ place ∈ logical.live, place ∈ state.live) ∧
  (List.map Prod.fst logical.values).Nodup ∧ (List.map Prod.fst state.materialized).Nodup ∧
  (List.map Prod.fst state.virtualValues).Nodup ∧
  (List.map Prod.fst state.zeroValues).Nodup ∧
  (∀ place value, (place, value) ∈ state.zeroValues →
    place ∈ state.initialized ∧ memory.classAt state.frame place = some .materialized ∧
    ∃ layout, memory.layoutAt state.frame place = some layout ∧ layout.size = 0) ∧
  (∀ place, place ∈ state.initialized ↔ (AList.lookup logical.values place).isSome) ∧
  (∀ place value, (place, value) ∈ logical.values → place ∈ logical.live) ∧
  (∀ place value, (place, value) ∈ state.virtualValues →
    place ∈ state.initialized ∧ memory.classAt state.frame place = some .virtual) ∧
  (∀ place range, (place, range) ∈ state.materialized →
    place ∈ state.live ∧ memory.classAt state.frame place = some .materialized ∧
    memory.rootRange state place range) ∧
  ∀ place ∈ state.live, ∃ locals graph ty storage,
    (state.frame, locals, graph) ∈ memory.functions ∧
    AList.lookup locals place = some ty ∧ memory.classAt state.frame place = some storage ∧
    (storage = .materialized → ∃ layout,
      memory.layoutAt state.frame place = some layout ∧
      (layout.size = 0 ↔ (AList.lookup state.materialized place).isNone)) ∧
    ∀ value, AList.lookup logical.values place = some value →
      if initializedAt state place [] then
        ∃ physical, language.representsValue profile.memoryTarget ranges ty value physical ∧
          memory.StoresValue state (ty := ty) ⟨⟨place, []⟩, storage⟩ physical
      else
        ∀ path fieldTy field, initializedAt state place path = true →
          path.foldlM language.fieldType ty = some fieldTy →
          language.field value path = some field →
          ∃ physical, language.representsValue profile.memoryTarget ranges fieldTy field physical ∧
            memory.StoresValue state (ty := fieldTy) ⟨⟨place, path⟩, storage⟩ physical

end Memory

namespace Memory
variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
  {profile : Profile}

def readArgs (memory : Memory lang profile) :
    {types : List language.operands.Ty} → Args (MemoryRead _ lang.literal) types →
    MemoryRegisters language profile →
    Option (List language.operands.Val × MemoryRegisters language profile)
  | _, .nil, state => some ([], state)
  | _, .cons read rest, state => do
    let (value, state) ← memory.readValue state read
    let (tail, state) ← memory.readArgs rest state
    return (value :: tail, state)

def Begins (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) (next : MemoryRegisters language profile) : Prop :=
  place ∉ state.live ∧
  let live := {state with live := place :: state.live}
  (memory.classAt state.frame place = some .virtual ∧ next = live) ∨
  (memory.classAt state.frame place = some .materialized ∧
    ∃ layout, memory.layoutAt state.frame place = some layout ∧
    ((layout.size = 0 ∧ next = live) ∨
    (0 < layout.size ∧ ∃ space base block bytes,
      state.bytes.alloc profile.memoryTarget space base layout.size layout.align = .ok (block, bytes) ∧
      next = {live with bytes := bytes, materialized :=
        (place, ⟨⟨space, base, some block⟩, layout.size, layout.align⟩) :: state.materialized})))

/-- A rejected address is only one allocator choice. Exhaustion requires a fresh,
nonempty materialized root with a known layout and no successful placement anywhere. -/
structure BeginExhausted (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) : Prop where
  fresh : place ∉ state.live
  materialized : memory.classAt state.frame place = some .materialized
  nonemptyLayout : ∃ layout, memory.layoutAt state.frame place = some layout ∧ 0 < layout.size
  unavailable : ¬ ∃ next, memory.Begins state place next

def endOwnerView (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) : Option (Own.State × Hybrid.Place) :=
  (memory.rootType state.frame place).map fun ty =>
    let moved := state.movedFields.filterMap fun (owner, path) =>
      if owner == place then some path else none
    let discharged := !state.initialized.contains place ||
      (!moved.isEmpty &&
        (memory.classAt state.frame place).any fun storage =>
          (memory.loadBacking state (ty := ty) ⟨⟨place, []⟩, storage⟩).any fun value =>
            canDiscard language true (moved.foldl (fun depth path => max depth path.length) 0) ty value moved)
    memory.ownerView state place ty (if discharged then .moved else .owned)

def canEnd (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) : Bool :=
  (memory.endOwnerView state place).any fun (owners, id) => Own.canDropPlaces owners [id]

theorem canEnd_iff (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) :
    memory.canEnd state place = true ↔
      ∃ view, memory.endOwnerView state place = some view ∧ Own.canDropPlaces view.1 [view.2] = true := by
  simp only [canEnd]
  cases found : memory.endOwnerView state place
  · simp
  · rename_i view
    cases view
    simp

def Ends (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) (next : MemoryRegisters language profile) : Prop :=
  (place ∈ state.live ∧ memory.canEnd state place) ∧
  let ownership := match AList.lookup state.addressIds place with
    | none => state.ownership
    | some id => Own.erasePlaces state.ownership [id]
  let dead := {state with
    ownership
    live := state.live.erase place
    initialized := state.initialized.erase place
    zeroValues := AList.erase state.zeroValues place
    virtualValues := AList.erase state.virtualValues place
    materialized := AList.erase state.materialized place
    addressIds := AList.erase state.addressIds place
    movedFields := state.movedFields.filter (fun (owner, _) => owner != place)}
  (memory.classAt state.frame place = some .virtual ∧ next = dead) ∨
  (memory.classAt state.frame place = some .materialized ∧
    ((∃ layout, memory.layoutAt state.frame place = some layout ∧ layout.size = 0 ∧ next = dead) ∨
    (∃ range block bytes, AList.lookup state.materialized place = some range ∧ range.ptr.prov = some block ∧
      state.bytes.free block = some bytes ∧ next = {dead with bytes := bytes})))

def readOperands (memory : Memory lang profile) :
    List (Sigma (MemoryRead _ lang.literal)) → MemoryRegisters language profile →
    Option (List language.operands.Val × MemoryRegisters language profile)
  | [], state => some ([], state)
  | ⟨_, operand⟩ :: rest, state => do
    let (value, afterRead) ← memory.readValue state operand
    let (values, next) ← memory.readOperands rest afterRead
    return (value :: values, next)

/-- One placement sequence for a parameter list. The flag records whether this
sequence encountered an unavailable reservation after its authorized writes. -/
inductive WriteParams (memory : Memory lang profile) :
    List (Sigma (MemoryPlace language.operands.Ty)) → List language.operands.Val →
      MemoryRegisters language profile → MemoryRegisters language profile → (exhausting : optParam Bool false) → Prop
  | nil (state) : WriteParams memory [] [] state state false
  | cons {ty} {place : MemoryPlace _ ty} {params value values state started stored next exhausting} :
      (if state.live.contains place.logical.root then started = state
        else memory.Begins state place.logical.root started) →
      memory.Writes started place value stored →
      WriteParams memory params values stored next exhausting →
      WriteParams memory (⟨ty, place⟩ :: params) (value :: values) state next exhausting
  | exhausted {ty} {place : MemoryPlace _ ty} {params value values state} :
      memory.BeginExhausted state place.logical.root →
      WriteParams memory (⟨ty, place⟩ :: params) (value :: values) state state true

/-- A compound transfer exhausts only if a reservation actually fails and no
placement sequence can complete the entire list from the original checkpoint. -/
def ParamOutcome (memory : Memory lang profile)
    (params : List (Sigma (MemoryPlace language.operands.Ty))) (values : List language.operands.Val)
    (state next : MemoryRegisters language profile) (exhausting : Bool) : Prop :=
  if exhausting then
    memory.WriteParams params values state next true ∧
      ¬ ∃ completed, memory.WriteParams params values state completed false
  else memory.WriteParams params values state next false

inductive EndLocals (memory : Memory lang profile) :
    List PlaceId → MemoryRegisters language profile → MemoryRegisters language profile → Prop
  | nil (state) : EndLocals memory [] state state
  | cons {place rest state ended next} :
      memory.Ends state place ended → EndLocals memory rest ended next →
      EndLocals memory (place :: rest) state next

def Call (memory : Memory lang profile) (callee : FnId)
    (dst : Option (Sigma (MemoryPlace language.operands.Ty)))
    (args : List (Sigma (MemoryRead _ lang.literal)))
    (state next : MemoryRegisters language profile) (exhausting : Bool := false) : Prop :=
  ∃ values afterRead graph entry,
    memory.readOperands args state = some (values, afterRead) ∧
    memory.graphAt callee = some graph ∧ graph.blocks.find? (·.id == graph.entry) = some entry ∧
    args.map Sigma.fst = entry.params.map Sigma.fst ∧
    memory.ParamOutcome entry.params values
      { toMemoryFrame := ⟨callee, graph.entry, 0, [], [], [], [], [], [], []⟩
        bytes := afterRead.bytes
        ownership := afterRead.ownership
        nextAddressId := afterRead.nextAddressId
        effects := afterRead.effects
        callers := ⟨afterRead.advance.toMemoryFrame, dst⟩ :: afterRead.callers } next exhausting

def Return (memory : Memory lang profile) (operand : Option (Sigma (MemoryRead _ lang.literal)))
    (state next : MemoryRegisters language profile) (exhausting : Bool := false) : Prop :=
  ∃ caller rest values afterRead ended,
    state.callers = caller :: rest ∧
    memory.readOperands operand.toList state = some (values, afterRead) ∧
    memory.EndLocals afterRead.live afterRead ended ∧
    operand.toList.map Sigma.fst = caller.destination.toList.map Sigma.fst ∧
    memory.ParamOutcome caller.destination.toList values
      { toMemoryFrame := caller.caller, bytes := ended.bytes, callers := rest, effects := ended.effects, ownership := ended.ownership, nextAddressId := ended.nextAddressId } next exhausting

def Transfer (memory : Memory lang profile) (edge : CFGEdge (Sigma (MemoryRead _ lang.literal)))
    (state next : MemoryRegisters language profile) (exhausting : Bool := false) : Prop :=
  ∃ values afterRead block,
    memory.readOperands edge.arguments state = some (values, afterRead) ∧
    memory.blockAt {state with block := edge.target} = some block ∧
    edge.arguments.map Sigma.fst = block.params.map Sigma.fst ∧
    memory.ParamOutcome block.params values {afterRead with block := edge.target, instruction := 0} next exhausting

/-- Splitting a value transfers each bound field once. An omitted active field
must be Copy, and every destination has the declared type of its field. -/
def DestructureFields (language : CoreLanguage) (ty : language.operands.Ty)
    (value : language.operands.Val) (bindings : List (FieldId × language.operands.Ty)) : Prop :=
  (bindings.map Prod.fst).Nodup ∧
  ∃ fields, language.activeFields ty value = some fields ∧
    (∀ binding ∈ bindings,
      binding.1 ∈ fields ∧ language.fieldType ty binding.1 = some binding.2) ∧
    ∀ field ∈ fields, field ∉ bindings.map Prod.fst →
      ∃ fieldTy, language.fieldType ty field = some fieldTy ∧
        language.operands.isCopy fieldTy = true

def firstTrap (opcode : Opcode) (values : List language.operands.Val)
    (checks : List OperationCheck) : Option TrapKind :=
  (checks.find? fun check => check.disposition == .residual &&
    language.checkTrap opcode check.discriminator values).map OperationCheck.trap

def instructionTrap (memory : Memory lang profile) (state : MemoryRegisters language profile) :
    MemoryInst lang → Option TrapKind
  | @MemoryInst.op _ _ opcode _ _ _ _ args checks _ => do
    let (values, _) ← memory.readArgs args state
    firstTrap (.operator opcode) values checks
  | .call callee _ args checks => do
    let (values, _) ← memory.readOperands args state
    firstTrap (.call callee) values checks
  | _ => none

/-- Taking an address assigns a fresh identity to the current lifetime on first use.
The identity is independent of the numeric placement and survives suspension. -/
def prepareAddresses (state : MemoryRegisters language profile)
    (operands : List (Sigma (MemoryPlace language.operands.Ty))) :
    MemoryRegisters language profile :=
  operands.foldl (fun state operand =>
    let root := operand.2.logical.root
    if (AList.lookup state.addressIds root).isSome then state
    else {state with
      addressIds := (root, ⟨state.nextAddressId⟩) :: state.addressIds
      nextAddressId := state.nextAddressId + 1}) state

def resolveAddress (memory : Memory lang profile)
    (state : MemoryRegisters language profile) (logical : Bool)
    (operand : Sigma (MemoryPlace language.operands.Ty)) :
    Option (OperationAddress language.operands.Ty) := do
  let place := operand.2
  if !memory.validPlace state place || !state.live.contains place.logical.root ||
      !initializedAt state place.logical.root place.logical.path then none else
  let provenance ← AList.lookup state.addressIds place.logical.root
  if logical then
    return ⟨operand.1, place.logical.root, place.logical.path, none, provenance, .logical⟩
  else
    if place.storage != .materialized then none else
    if memory.zeroRoot state.frame place.logical.root then
      if !place.logical.path.isEmpty then
        let rootType ← memory.rootType state.frame place.logical.root
        let layout ← memory.layoutAt state.frame place.logical.root
        let (offset, field) ← language.fieldRange rootType place.logical.path layout
        if offset + field.size != 0 then none else
          some ⟨operand.1, place.logical.root, place.logical.path, none, provenance, .zero⟩
      else some ⟨operand.1, place.logical.root, place.logical.path, none, provenance, .zero⟩
    else
      let range ← memory.rangeAt state place.logical
      let _ ← state.bytes.access profile.memoryTarget range.ptr range.extent range.align
      return ⟨operand.1, place.logical.root, place.logical.path, some range, provenance, .bytes⟩

private def addressedValues (logical : Bool) (frame : MemoryFrame language.operands.Val) :
    AList Hybrid.Place language.operands.Val :=
  let values := if logical then frame.virtualValues else frame.zeroValues
  frame.addressIds.filterMap fun (root, id) =>
    (AList.lookup values root).map fun value => (id, value)

def operationStore (state : MemoryRegisters language profile) (logical : Bool) :
    OperationStore language.operands.Val :=
  ⟨state.bytes, state.ownership,
    addressedValues logical state.toMemoryFrame ++
      state.callers.flatMap fun continuation => addressedValues logical continuation.caller⟩

private def updateAddressed (logical : Bool) (values : AList Hybrid.Place language.operands.Val)
    (frame : MemoryFrame language.operands.Val) : MemoryFrame language.operands.Val :=
  let update := List.map fun (root, value) =>
    let changed := (AList.lookup frame.addressIds root).bind (AList.lookup values)
    (root, changed.getD value)
  if logical then {frame with virtualValues := update frame.virtualValues}
    else {frame with zeroValues := update frame.zeroValues}

/-- Install logical writes through references in the owning frame. The operation can
only update addressed values that were present before the step. Physical bytes remain
the authority for nonempty roots. -/
def applyOperation (state : MemoryRegisters language profile) (logical : Bool)
    (store : OperationStore language.operands.Val) : MemoryRegisters language profile :=
  {state with
    toMemoryFrame := updateAddressed logical store.addressedValues state.toMemoryFrame
    callers := state.callers.map fun continuation =>
      {continuation with caller := updateAddressed logical store.addressedValues continuation.caller}
    bytes := store.bytes
    ownership := store.ownership}

def InstructionStepWith (memory : Memory lang profile)
    (state next : MemoryRegisters language profile) (logical startDefinitions : Bool) (instruction : MemoryInst lang)
    (exhausting : Bool := false) : Prop := match instruction with
  | .bind dst src =>
    ∃ value afterRead stored, memory.readValue state src = some (value, afterRead) ∧
      (if logical || startDefinitions then memory.ParamOutcome [⟨_, dst⟩] [value] afterRead stored exhausting
        else if exhausting then False else memory.Writes afterRead dst value stored) ∧ next = stored.advance
  | .assign dst src => if exhausting then False else
    ∃ value afterRead stored, memory.readValue state src = some (value, afterRead) ∧
      memory.Writes afterRead dst value stored ∧ next = stored.advance
  | @MemoryInst.op _ _ opcode types ty dst _ args checks addresses =>
    ∃ values afterReads operands result effects changed stored, memory.readArgs args state = some (values, afterReads) ∧
      firstTrap (.operator opcode) values checks = none ∧
      addresses.mapM (memory.resolveAddress (prepareAddresses afterReads addresses) logical) = some operands ∧
      let ready := prepareAddresses afterReads addresses
      let before := operationStore ready logical
      language.operation opcode types ty values profile.memoryTarget operands before result effects changed ∧
      List.map Prod.fst changed.addressedValues = List.map Prod.fst before.addressedValues ∧
      let afterOperation := applyOperation ready logical changed
      (if logical || startDefinitions then memory.ParamOutcome [⟨_, dst⟩] [result] afterOperation stored exhausting
        else if exhausting then False else memory.Writes afterOperation dst result stored) ∧
      next = {stored.advance with effects := stored.effects ++ effects}
  | .beginLifetime place => if exhausting then memory.BeginExhausted state place else
    ∃ started, memory.Begins state place started ∧ next = started.advance
  | .endLifetime place => if exhausting then False else
    ∃ ended, memory.Ends state place ended ∧ next = ended.advance
  | @MemoryInst.destructure _ _ ty src bindings =>
    ∃ value afterRead fields stored,
      memory.readValue state (if language.operands.isCopy ty then .copy src else .move src) =
        some (value, afterRead) ∧
      DestructureFields language ty value (bindings.map fun (field, dst) => (field, dst.1)) ∧
      bindings.mapM (fun binding => language.field value [binding.1]) = some fields ∧
      memory.ParamOutcome (bindings.map Prod.snd) fields afterRead stored exhausting ∧ next = stored.advance
  | .call callee dst args checks =>
    memory.instructionTrap state (.call callee dst args checks) = none ∧
      memory.Call callee dst args state next exhausting

def InstructionStep (memory : Memory lang profile)
    (state next : MemoryRegisters language profile) (logical : Bool := false) (op : MemoryInst lang)
    (exhausting : Bool := false) : Prop :=
  memory.InstructionStepWith state next logical false op exhausting

def ControlStep (memory : Memory lang profile)
    (state next : MemoryRegisters language profile)
    (ending : MemoryTerm lang) (exhausting : Bool := false) : Prop := match ending with
  | .jump edge => memory.Transfer edge state next exhausting
  | .branch test yes no =>
    ∃ value afterRead flag, memory.readValue state test = some (value, afterRead) ∧
      language.operands.test value = some flag ∧
      memory.Transfer (if flag then yes else no) afterRead next exhausting
  | .ret value => memory.Return value state next exhausting
  | .trap .. | .invalid => False

/-- Both outcomes share instruction fetch and all guards preceding a reservation.
Only a normal outcome supplies the successor registers to commit. -/
def StepOutcome (memory : Memory lang profile)
    (state next : MemoryRegisters language profile) (exhausting : Bool) (startDefinitions : Bool := false) : Prop :=
  ∃ block, memory.blockAt state = some block ∧
    match block.body[state.instruction]? with
    | some op => memory.InstructionStepWith state next false startDefinitions op.instruction exhausting
    | none => state.instruction = block.body.length ∧ memory.ControlStep state next block.ending exhausting

/-- An instruction commits its ordinary successor or aborts at its original PC.
A failed reservation inside a compound transfer commits no partial writes or effects. -/
def Step (memory : Memory lang profile) (state next : MemoryRegisters language profile)
    (startDefinitions : Bool := false) : Prop :=
  if state.resourceExhausted then False else if next.resourceExhausted then
    next = {state with resourceExhausted := true} ∧
      ∃ partialState, memory.StepOutcome state partialState true startDefinitions
  else memory.StepOutcome state next false startDefinitions

def Initial (memory : Memory lang profile) (state : MemoryRegisters language profile) : Prop :=
  state.frame = memory.entry ∧ state.callers = [] ∧
  (∃ graph, memory.graphAt state.frame = some graph ∧ graph.entry = state.block) ∧
  (∃ block, memory.blockAt state = some block ∧ block.params = []) ∧
  state.instruction = 0 ∧ state.virtualValues = [] ∧
  state.materialized = [] ∧ state.live = [] ∧ state.initialized = [] ∧ state.zeroValues = [] ∧
  state.bytes = Mem.Memory.empty ∧ state.effects = [] ∧ state.ownership = {} ∧
  state.addressIds = [] ∧ state.nextAddressId = 0 ∧ state.movedFields = [] ∧ state.resourceExhausted = false

end Memory

/-- Render the hosted trap diagnostic from the originating source location. -/
private def trapMessage (kind : TrapKind) (site : SiteId) (span : FileSpan) (file : File) :
    String :=
  s!"concrete: trap: {kind.name} at {file.path.render.text}:{span.line}:{span.col} (site {site.id})\n"

def trapDiagnostic (project : Project) (kind : TrapKind) (site : SiteId)
    (span : FileSpan) : ByteArray :=
  (project.files[span.file.index]?.map fun file => (trapMessage kind site span file).toUTF8)
    |>.getD ByteArray.empty

def coreTrapDiagnostic {language : CoreLanguage} (core : CoreInput language)
    (kind : TrapKind) (site : SiteId) : ByteArray :=
  match AList.lookup core.payload.origins site with
  | some origin => trapDiagnostic core.project kind site origin.lexical
  | none => ByteArray.empty

/-- A hosted observation comes from the terminator at the current PC. Return
operands use the same target-indexed reads as instructions. A callee return
instead steps back to its caller. -/
def Memory.terminalObservation {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (memory : Memory lang profile)
    (state : MemoryRegisters language profile) : Option Observation := do
  if state.resourceExhausted then return .resourceExhausted
  let block ← memory.blockAt state
  if let some op := block.body[state.instruction]? then
    let kind ← memory.instructionTrap state op.instruction
    let .trapped status kind _ ← language.hostedResult (.trapped kind op.origin.operation) | none
    return .trapped status kind op.origin.lexical
      (trapDiagnostic memory.project kind op.origin.operation op.origin.lexical)
  else if state.instruction != block.body.length then none else
  match block.ending with
  | .ret (some ⟨_, operand⟩) => do
    if !state.callers.isEmpty || state.frame != memory.entry then none else
    let (value, afterRead) ← memory.readValue state operand
    if !(afterRead.live.all (memory.canEnd afterRead)) then none else
    let .returned status ← language.hostedResult (.returned value) | none
    return .returned status ByteArray.empty
  | .trap kind origin => do
    let .trapped status kind _ ← language.hostedResult (.trapped kind origin.operation) | none
    return .trapped status kind origin.lexical (trapDiagnostic memory.project kind origin.operation origin.lexical)
  | _ => none

namespace MIR

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
  {profile : Profile}

private def logicalPlace {ty : language.operands.Ty} (place : Place _ ty) :
    MemoryPlace _ ty := ⟨place, .virtual⟩

private def logicalRead {ty : language.operands.Ty} :
    Read _ lang.literal ty → MemoryRead _ lang.literal ty
  | .literal literal => .literal literal
  | .copy place => .copy (logicalPlace place)
  | .move place => .move (logicalPlace place)

private def logicalOperand (operand : Sigma (Read language.operands.Ty lang.literal)) :
    Sigma (MemoryRead _ lang.literal) := ⟨operand.1, logicalRead operand.2⟩

private def logicalParam (param : Sigma (Place language.operands.Ty)) :
    Sigma (MemoryPlace language.operands.Ty) := ⟨param.1, logicalPlace param.2⟩

private def logicalArgs : {types : List language.operands.Ty} →
    Args (Read _ lang.literal) types → Args (MemoryRead _ lang.literal) types
  | _, .nil => .nil
  | _, .cons read rest => .cons (logicalRead read) (logicalArgs rest)

private def logicalInstruction : MIRInst lang → MemoryInst lang
  | .bind dst src => .bind (logicalPlace dst) (logicalRead src)
  | .assign dst src => .assign (logicalPlace dst) (logicalRead src)
  | .op dst op args checks addresses =>
    .op (logicalPlace dst) op (logicalArgs args) checks (addresses.map logicalParam)
  | .endLifetime place => .endLifetime place
  | .destructure src bindings => .destructure (logicalPlace src)
      (bindings.map fun (field, place) => (field, logicalParam place))
  | .call callee dst args checks => .call callee (dst.map logicalParam) (args.map logicalOperand) checks

private def logicalTerminator : Terminator (Sigma (Read _ lang.literal)) (Read _ lang.literal lang.bool) →
    MemoryTerm lang
  | .ret value => .ret (value.map logicalOperand)
  | .trap kind origin => .trap kind origin
  | .invalid => .invalid
  | .jump edge => .jump ⟨edge.target, edge.arguments.map logicalOperand⟩
  | .branch test yes no => .branch (logicalRead test)
      ⟨yes.target, yes.arguments.map logicalOperand⟩ ⟨no.target, no.arguments.map logicalOperand⟩

/-- The logical evaluator shares CFG and operand rules with Memory. All MIR
roots hold logical values, even when their physical representation needs bytes. -/
def logicalView (mir : MIR lang profile) : Memory lang profile where
  project := mir.project
  entry := mir.entry
  signatures := mir.signatures
  functions := mir.functions.map fun (locals, graph) =>
    let body : MemoryGraph lang := {
      entry := graph.entry
      blocks := graph.blocks.map fun block => {
        id := block.id
        params := block.params.map logicalParam
        body := block.body.map fun inst => ⟨inst.origin, logicalInstruction inst.instruction⟩
        ending := logicalTerminator block.ending
        boundaryOrigins := block.boundaryOrigins } }
    (locals, body)
  layouts := []
  mutableLocals := mir.mutableLocals
  storage := mir.functions.map fun (locals, _) =>
    locals.map fun _ => .virtual

private def instructionAt (mir : MIR lang profile) (state : MemoryRegisters language profile) :
    Option (LocatedOp (MIRInst lang)) := do
  let (_, graph) ← AList.lookup mir.functions state.frame
  let block ← graph.blocks.find? (·.id == state.block)
  block.body[state.instruction]?

def Step (mir : MIR lang profile) (state next : MemoryRegisters language profile) : Prop :=
  let view := mir.logicalView
  match mir.instructionAt state with
  | some op => view.instructionTrap state (logicalInstruction op.instruction) = none ∧
    view.InstructionStep state next true (logicalInstruction op.instruction)
  | none => ∃ block, view.blockAt state = some block ∧ state.instruction = block.body.length ∧
    view.ControlStep state next block.ending

def terminalObservation (mir : MIR lang profile) (state : MemoryRegisters language profile) :
    Option Observation :=
  mir.logicalView.terminalObservation state

end MIR

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

private def entryStateFor (memory : Memory lang profile) (arity : Nat := 0) :
    MemoryRegisters language profile :=
  let graph := memory.graphAt memory.entry
  let id := (graph.map CFG.entry).getD ⟨0⟩
  let block := graph.bind (·.blocks.find? (·.id == id))
  let pc := (block.map fun block =>
    if block.params.length = arity then 0 else block.body.length + 1).getD 0
  ⟨⟨memory.entry, id, pc, [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩

private def hostedResultFor (memory : Memory lang profile) (state : MemoryRegisters language profile) :
    Option Concrete.Contracts.Observation := do
  match ← memory.terminalObservation state with
  | .returned status bytes => return .returned status bytes
  | .trapped status kind span bytes =>
    let block ← memory.blockAt state
    let origin ← match block.body[state.instruction]? with
      | some op => some op.origin
      | none => match block.ending with
        | .trap _ origin => some origin
        | _ => none
    let file ← memory.project.files[span.file.index]?
    return .trapped status kind ⟨⟨origin.operation.id⟩, ⟨file.path.render.text, span.line, span.col⟩⟩ bytes
  | .startupRefused status bytes => return .startupRefused status bytes
  | .resourceExhausted => return .resourceExhausted

private def traceFor {A S E : Type}
    (effects : S → List E)
    (result : A → S → Option Concrete.Contracts.Observation)
    (a : A) (state : S) :
    List (ProcessEvent E) :=
  (effects state).map .effect ++
    ((result a state).map fun o => o.stderr.data.toList.map .stderr).getD []

/-- Interpret an invocation of the supplied graph from the selected activation. -/
private def hostedExecution {A S E : Type} (effects : S → List E)
    (step : A → S → S → Prop)
    (start : A → S)
    (observe : A → S → Option Concrete.Contracts.Observation) :
    Semantics A E where
  State := S
  initial a state := state = start a
  initialized _ := ⟨_, rfl⟩
  step a state next :=
    observe a state = none ∧ step a state next ∧
      Concrete.Contracts.TracePrefix (traceFor effects observe a state) (traceFor effects observe a next)
  result a state result := observe a state = some result
  terminal _ _ _ h _ tick := by
    have impossible := h.symm.trans tick.1
    contradiction
  safe a state :=
    (observe a state).isSome = true ∨
    ∃ next, observe a state = none ∧ step a state next ∧
      Concrete.Contracts.TracePrefix (traceFor effects observe a state) (traceFor effects observe a next)
  trace := traceFor effects observe
  permitted _ _ := True
  resultTrace := by
    intro input state result h
    unfold traceFor
    rw [h]
    simp only [Option.map_some, Option.getD_some]
    have stderrExact : ∀ effects : List E, ∀ bytes : List UInt8,
        Concrete.Contracts.stderrBytes (effects.map ProcessEvent.effect ++ bytes.map ProcessEvent.stderr) = bytes := by
      intro effects bytes
      induction effects with
      | nil => induction bytes with
        | nil => rfl
        | cons b rest ih => exact congrArg (b :: ·) ih
      | cons _ _ ih => exact ih
    exact stderrExact _ _
  traceGrows _ _ _ tick := tick.2.2

private def graphExecution {A : Type}
    (step : A → MemoryRegisters language profile → MemoryRegisters language profile → Prop)
    (start : A → MemoryRegisters language profile)
    (observe : A → MemoryRegisters language profile → Option Concrete.Contracts.Observation) :
    Semantics A language.operands.Event :=
  hostedExecution (fun state => state.effects) step start observe

private abbrev CoreState (language : CoreLanguage) :=
  (Σ ownership : AList ConcreteSpec.Var Own.Descriptor, Exec.Config (language.instrumented ownership)) ×
    List language.operands.Event

private def coreHostedResult (core : CoreInput language) (state : CoreState language) :
    Option Concrete.Contracts.Observation := do
  let outcome ← state.1.2.final?
  match ← language.hostedResult outcome with
  | .returned status => return .returned status ByteArray.empty
  | .trapped status kind site =>
    let origin ← AList.lookup core.payload.origins site
    let file ← core.project.files[origin.lexical.file.index]?
    return .trapped status kind ⟨⟨site.id⟩,
      ⟨file.path.render.text, origin.lexical.line, origin.lexical.col⟩⟩
      (coreTrapDiagnostic core kind site)

private def coreStep (core : CoreInput language) (state next : CoreState language) : Prop :=
  ∃ p q label, state.1 = ⟨core.payload.ownership, p⟩ ∧
    next.1 = ⟨core.payload.ownership, q⟩ ∧ Exec.Step core.payload.body p label q ∧
    next.2 = state.2 ++ label.events

def CoreInput.Execution : Semantics (CoreInput language) language.operands.Event :=
  { hostedExecution Prod.snd coreStep
      (fun core => (⟨core.payload.ownership, core.payload.initial⟩, [])) coreHostedResult with
    permitted := fun core event =>
      ∃ state events, Exec.Steps core.payload.body core.payload.initial events state ∧
        (event ∈ ((coreHostedResult core (⟨core.payload.ownership, state⟩, events)).map
          fun result => result.stderr.data.toList.map ProcessEvent.stderr).getD [] ∨
        ∃ label next, Exec.Step core.payload.body state label next ∧
          event ∈ label.events.map ProcessEvent.effect) }

def MIR.Execution : Semantics (MIR lang profile) language.operands.Event :=
  graphExecution MIR.Step (fun mir => entryStateFor mir.logicalView)
    (fun mir => hostedResultFor mir.logicalView)

def Memory.Execution : Semantics (Memory lang profile) language.operands.Event :=
  graphExecution (fun memory state next => memory.Step state next) (fun memory => entryStateFor memory) hostedResultFor

/-- Ordinary hosted results carry the MIR matching obligation. A failed physical
reservation has its own typed observation in Execution. -/
def Memory.MatchingResult (memory : Memory lang profile) (q : MemoryRegisters language profile)
    (o : Concrete.Contracts.Observation) : Prop :=
  if q.resourceExhausted then False else Memory.Execution.result memory q o

theorem Memory.silent_step {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    (ready : memory.terminalObservation q = none) (tick : memory.Step q next)
    (effects : next.effects = q.effects) : Memory.Execution.step memory q next := by
  have absent : hostedResultFor memory q = none := by simp [hostedResultFor, ready]
  refine ⟨absent, tick, ?_⟩
  change Concrete.Contracts.TracePrefix (traceFor _ _ memory q) (traceFor _ _ memory next)
  unfold traceFor
  rw [absent]
  simp only [Option.map_none, Option.getD_none, List.append_nil]
  exact ⟨_, by rw [effects]⟩

theorem Memory.exhausted_result (memory : Memory lang profile) {q : MemoryRegisters language profile}
    (h : q.resourceExhausted = true) : Memory.Execution.result memory q .resourceExhausted := by
  change hostedResultFor memory q = _
  simp [hostedResultFor, Memory.terminalObservation, h]

private def bindsOwner {O : Exec.Operands} (owner : ConcreteSpec.Var) : Exec.Stmt O → Bool
  | .bind var .. => var == owner
  | .call dst .. | .prim dst .. => dst == some owner
  | .seq first second | .cond _ first second _ | .loop _ first _ second _ =>
    bindsOwner owner first || bindsOwner owner second
  | .block _ body => bindsOwner owner body
  | _ => false

private def sourceMutable (core : CoreInput language) (fn : FnId) (root : PlaceId) : Bool :=
  let owner : ConcreteSpec.Var := ⟨root.id⟩
  ((AList.lookup core.payload.ownership owner).any (·.mutable)) &&
    (AList.lookup core.payload.body.fns fn).any (fun function =>
      function.params.any (fun parameter => parameter.1 == owner) || bindsOwner owner function.body)

/-- Retained bindings keep their fresh Core identity as the MIR root number.
Every mutable root must be declared locally and backed by a mutable binding
in the corresponding Core function. Synthetic roots are immutable here. -/
def PreservesOwnerMutability (core : CoreInput language) (mir : MIR lang profile) : Prop :=
  (∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
    ∀ root ty, (root, ty) ∈ locals →
      ((AList.lookup mir.mutableLocals fn).getD []).contains root = sourceMutable core fn root) ∧
  ∀ fn roots root, (fn, roots) ∈ mir.mutableLocals → root ∈ roots →
    sourceMutable core fn root = true ∧
    ∃ locals graph ty, (fn, locals, graph) ∈ mir.functions ∧ (root, ty) ∈ locals

def LowersCore
    (input : Admitted language) (mir : MIR lang profile) : Prop :=
  mir.project = input.core.project ∧ mir.entry = input.core.payload.entry ∧
  PreservesOrigins (List.map Prod.snd input.core.payload.origins) mir.origins ∧
  ConsumesEvidence input.core input.discharges mir ∧
  PreservesOwnerMutability input.core mir ∧
  BehaviorRefinement CoreInput.Execution (MIR.Execution (language := language))
    CoreInput.Execution.result (MIR.Execution (language := language)).result input.core mir

def LowersUnchecked
    (input : CoreInput language) (mir : MIR lang profile) : Prop :=
  mir.project = input.project ∧ mir.entry = input.payload.entry ∧
  PreservesOrigins (List.map Prod.snd input.payload.origins) mir.origins ∧
  ConsumesEvidence input [] mir ∧
  PreservesOwnerMutability input mir ∧
  BehaviorRefinement CoreInput.Execution (MIR.Execution (language := language))
    CoreInput.Execution.result (MIR.Execution (language := language)).result input mir

def Memory.RepresentsActivation (memory : Memory lang profile)
    (logical physical : MemoryFrame language.operands.Val) (bytes : Mem.Memory)
    (ranges : AList Hybrid.Place (Option Hybrid.Range) := []) : Prop :=
  memory.Represents ⟨logical.frame, logical.live, logical.virtualValues, logical.movedFields⟩
    ⟨physical, bytes, [], [], {}, 0, false⟩ ranges

def MemoryFrame.backingRanges (frame : MemoryFrame language.operands.Val) :
    AList Hybrid.Place (Option Hybrid.Range) :=
  List.map (fun (place, provenance) => (provenance, AList.lookup frame.materialized place)) frame.addressIds

private def addressIdsMatch (memory : Memory lang profile)
    (source target : MemoryFrame language.operands.Val) : Prop :=
  source.addressIds = target.addressIds ∧
  ∀ place id, (place, id) ∈ source.addressIds →
    place ∈ source.live ∧
    (memory.zeroRoot target.frame place = true ∨ (AList.lookup target.materialized place).isSome)

def Memory.RepresentsCheckpoint (memory : Memory lang profile)
    (source target : MemoryRegisters language profile) : Prop :=
  let ranges := target.toMemoryFrame.backingRanges ++
    target.callers.flatMap (·.caller.backingRanges)
  memory.RepresentsActivation source.toMemoryFrame target.toMemoryFrame target.bytes ranges ∧
  source.callers.length = target.callers.length ∧
  (∀ pair ∈ source.callers.zip target.callers,
    memory.RepresentsActivation pair.1.caller pair.2.caller target.bytes ranges ∧
      addressIdsMatch memory pair.1.caller pair.2.caller) ∧
  addressIdsMatch memory source.toMemoryFrame target.toMemoryFrame ∧
  (List.map Prod.fst ranges).Nodup ∧ source.nextAddressId = target.nextAddressId ∧
  source.ownership = target.ownership

/-- Execute the two exact graphs and relate their reachable representations. -/
def LowersMemory (mir : MIR lang profile) (memory : Memory lang profile) : Prop :=
  memory.project = mir.project ∧ memory.entry = mir.entry ∧
  PreservesOrigins mir.origins memory.origins ∧ LayoutsMatch language.layout memory ∧
  memory.signatures = mir.signatures ∧ PromotionSound (language := language) mir memory ∧
  memory.TablesWF ∧ mir.logicalView.TablesWF ∧
  let source := MIR.Execution (language := language)
  let target := Memory.Execution (language := language)
  BehaviorRefinement source target source.result (Memory.MatchingResult (language := language)) mir memory ∧
  (ProgramSafe source mir →
    ∃ sim : CheckpointSimulation source target source.result (Memory.MatchingResult (language := language)) mir memory,
      ∀ p q, source.finite mir p → target.finite memory q → sim.related p q →
        memory.RepresentsCheckpoint p q ∧ p.effects = q.effects)

def BIR.TrapsMatch {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (ssa : SSA lang profile) (bir : BIR lang profile) : Prop :=
  ∀ fn ∈ ssa.functions, ∀ block ∈ fn.2.2.blocks.toList, ∀ kind origin,
    block.ending = .trap kind origin →
    ∃ after ∈ bir.functions, after.1 = fn.1 ∧
      ∃ next ∈ after.2.2.blocks.toList, next.id = block.id ∧
        ∃ slot, next.ending = .trap slot ∧
          ∃ entry, bir.sites[slot.index]? = some entry ∧ entry.kind = kind ∧ entry.origin = origin

private def TrapEntry.runtimeFile (entry : TrapEntry) (project : Project) : Option File :=
  project.files[entry.origin.lexical.file.index]?

private def TrapEntry.runtimeMessage (entry : TrapEntry) (file : File) : String :=
  trapMessage entry.kind entry.origin.operation entry.origin.lexical file

def LCBInput.RuntimeMatches (output : LCBInput profile) (project : Project) : Prop :=
  ∃ diagnostics,
    output.sites.toList.mapM (fun entry =>
      (entry.runtimeFile project).map entry.runtimeMessage) = some diagnostics ∧
    output.runtimeInput = (Backend.Runtime.qbeDiagnostics diagnostics).toUTF8

private def LCBInput.runtimeResult (project : Project) (output : LCBInput profile) :
    LeanCompilerBackend.Verified.Source.Value → Option Concrete.Contracts.Observation
  | .word word => do
    if word.toNat < 256 then
      return .returned (UInt8.ofNat word.toNat) ByteArray.empty
    else
      let entry ← output.sites[word.toNat - 256]?
      let file ← entry.runtimeFile project
      let site : Concrete.Contracts.DiagnosticSite := ⟨⟨entry.origin.operation.id⟩,
        ⟨file.path.render.text, entry.origin.lexical.line, entry.origin.lexical.col⟩⟩
      return .trapped 134 entry.kind site (entry.runtimeMessage file).toUTF8
  | _ => none

def LCBInput.Execution (E : Type) (project : Project) : Semantics (LCBInput profile) E :=
  Backend.lcbSemanticsFor LCBInput.body (LCBInput.runtimeResult project)

namespace BIR

private def register (base : Nat) {ty : language.operands.Ty}
    (value : SSAValue _ ty) : MemoryPlace _ ty :=
  ⟨⟨⟨base + value.id⟩, []⟩, .virtual⟩

private def materialized {ty : language.operands.Ty} (place : Place _ ty) : MemoryPlace _ ty :=
  ⟨place, .materialized⟩

private def registerRead (base : Nat) {ty : language.operands.Ty} :
    SSARead _ lang.literal ty → MemoryRead _ lang.literal ty
  | .literal literal => .literal literal
  | .value value =>
    if language.operands.isCopy ty then .copy (register base value) else .move (register base value)

private def operand (base : Nat) (arg : Sigma (SSARead _ lang.literal)) :
    Sigma (MemoryRead language.operands.Ty lang.literal) :=
  ⟨arg.1, registerRead base arg.2⟩

private def arguments (base : Nat) : {types : List language.operands.Ty} →
    Args (SSARead _ lang.literal) types → Args (MemoryRead _ lang.literal) types
  | _, .nil => .nil
  | _, .cons arg rest => .cons (registerRead base arg) (arguments base rest)

private def instruction (base : Nat) : SSAInst lang → MemoryInst lang
  | .op dst op args addresses =>
    .op (register base dst) op (arguments base args) [] (addresses.map fun p => ⟨p.1, materialized p.2⟩)
  | .load (ty := ty) dst src =>
    .bind (register base dst) (if language.operands.isCopy ty then .copy (materialized src)
      else .move (materialized src))
  | .destructure src bindings =>
    .destructure (match src with
      | .register src => register base src
      | .materialized src => materialized src)
      (bindings.map fun (field, dst) => (field, ⟨dst.1, register base dst.2⟩))
  | .store dst src => .assign (materialized dst) (registerRead base src)
  | .beginLifetime place => .beginLifetime place
  | .endLifetime place => .endLifetime place
  | .call fn dst args => .call fn (dst.map fun p => ⟨p.1, register base p.2⟩)
      (args.map (operand base)) []

private def registerDeclarations (base : Nat) : SSAInst lang → LocalTypes language.operands.Ty
  | .op (ty := ty) dst .. | .load (ty := ty) dst .. => [((register base dst).logical.root, ty)]
  | .destructure _ bindings =>
    bindings.map fun (_, dst) => ((register base dst.2).logical.root, dst.1)
  | .call _ (some ⟨ty, dst⟩) _ => [((register base dst).logical.root, ty)]
  | _ => []

private def terminator (bir : BIR lang profile) (base : Nat) :
    BIRTerm (Sigma (SSARead _ lang.literal)) (SSARead _ lang.literal lang.bool) → MemoryTerm lang
  | .ret value => .ret (value.map (operand base))
  | .jump edge => .jump ⟨edge.target, edge.arguments.map (operand base)⟩
  | .branch test yes no => .branch (registerRead base test)
      ⟨yes.target, yes.arguments.map (operand base)⟩ ⟨no.target, no.arguments.map (operand base)⟩
  | .trap slot => match bir.sites[slot.index]? with
    | some entry => .trap entry.kind entry.origin
    | none => .invalid

/-- Interpret the exact BIR graph with the shared CFG rules. Register IDs and
byte-backed roots occupy disjoint namespaces. Trap slots resolve in this graph's table. -/
private def registerView {T : Type} (project : Project) (entry : FnId)
    (signatures : AList FnId (FunctionSignature language.operands.Ty))
    (functions : AList FnId (LocalTypes language.operands.Ty ×
      CFG (Sigma (SSAValue language.operands.Ty)) (LocatedOp (SSAInst lang)) T))
    (layouts : AList FnId (AList PlaceId Layout)) (mutableLocals : AList FnId (List PlaceId))
    (lowerTerm : Nat → T → MemoryTerm lang) :
    Memory lang profile :=
  let views := List.map (fun (fn, locals, graph) =>
    let declaredLimit := List.foldl (fun limit (root, _) => max limit (root.id + 1)) 0 locals
    -- Even an undeclared physical lifetime operand must not alias a virtual register.
    let base := graph.blocks.foldl (fun limit block =>
      block.body.foldl (fun limit op =>
        match op.instruction with
        | .beginLifetime root | .endLifetime root => max limit (root.id + 1)
        | _ => limit) limit) declaredLimit
    let registers := graph.blocks.toList.flatMap fun block =>
      (block.params.map fun p => ((register base p.2).logical.root, p.1)) ++
      (block.body.flatMap fun op => registerDeclarations base op.instruction)
    let body : MemoryGraph lang := ⟨graph.entry, graph.blocks.map fun block =>
      ⟨block.id, block.params.map (fun p => ⟨p.1, register base p.2⟩),
        block.body.map (fun op => ⟨op.origin, instruction base op.instruction⟩),
        lowerTerm base block.ending, block.boundaryOrigins⟩⟩
    ((fn, locals ++ registers, body),
      (fn, List.map (fun p => (p.1, StorageClass.materialized)) locals ++
        List.map (fun p => (p.1, StorageClass.virtual)) registers),
      (fn, registers.map Prod.fst))) functions
  -- Register definitions recur on loop iterations. Their synthetic backing
  -- slots permit replacement of Copy values without granting source mutation.
  let mutableLocals := views.foldl (fun table (_, _, fn, registers) =>
    if registers.isEmpty then table
    else (fn, registers ++ (AList.lookup table fn).getD []) :: AList.erase table fn) mutableLocals
  ⟨project, entry, signatures, views.map Prod.fst, layouts, views.map (·.2.1), mutableLocals⟩


def memoryView (bir : BIR lang profile) : Memory lang profile :=
  registerView bir.project bir.entry bir.signatures bir.functions bir.layouts bir.mutableLocals (terminator bir)

def entryState (bir : BIR lang profile) : MemoryRegisters language profile :=
  entryStateFor (bir.memoryView (language := language))

private def stepFor (memory : Memory lang profile) (state next : MemoryRegisters language profile) : Prop :=
  memory.Step state next (startDefinitions := true)

def step (bir : BIR lang profile) (state next : MemoryRegisters language profile) : Prop :=
  stepFor (bir.memoryView (language := language)) state next

def hostedResult (bir : BIR lang profile) (state : MemoryRegisters language profile) :
    Option Concrete.Contracts.Observation :=
  hostedResultFor (bir.memoryView (language := language)) state

private def registerExecution : Semantics (Memory lang profile) language.operands.Event :=
  graphExecution stepFor (fun memory => entryStateFor memory) hostedResultFor

def Execution : Semantics (BIR lang profile) language.operands.Event :=
  registerExecution.onInput (memoryView (language := language))

end BIR

/-- Structural obligations of the LLVM handoff. Semantic correctness of the
exact printed module is the trusted-base assumption P_print. -/
def LLVMMetadataMatches {Ty : Type} {lang : LoweringLanguage Ty}
    (bir : BIR lang profile) (output : LLVMInput profile) : Prop :=
  profile.backend = .llvm ∧ output.triple = profile.llvmTriple ∧
  output.dataLayout = profile.llvmDataLayout ∧ output.sites = bir.sites

def SSA.memoryView (ssa : SSA lang profile) : Memory lang profile :=
  BIR.registerView ssa.project ssa.entry ssa.signatures ssa.functions ssa.layouts ssa.mutableLocals fun base ending =>
    match ending with
    | .ret value => .ret (value.map (BIR.operand base))
    | .jump edge => .jump ⟨edge.target, edge.arguments.map (BIR.operand base)⟩
    | .branch test yes no => .branch (BIR.registerRead base test)
        ⟨yes.target, yes.arguments.map (BIR.operand base)⟩
        ⟨no.target, no.arguments.map (BIR.operand base)⟩
    | .trap kind origin => .trap kind origin
    | .invalid => .invalid

def SSA.Execution : Semantics (SSA lang profile) language.operands.Event :=
  BIR.registerExecution.onInput (SSA.memoryView (language := language))

def BuildsSSA
    (memory : Memory lang profile) (ssa : SSA lang profile) : Prop :=
  ssa.project = memory.project ∧ ssa.entry = memory.entry ∧ ssa.layouts = memory.layouts ∧
  ssa.signatures = memory.signatures ∧ PreservesOrigins memory.origins ssa.origins ∧
  BehaviorRefinement (Memory.Execution (language := language)) (SSA.Execution (language := language))
    (Memory.Execution (language := language)).result (SSA.Execution (language := language)).result memory ssa

/-- BIR formation re-encodes the entry graph. Linked behavior is checked
separately, so unreachable helper bodies can differ. -/
def BIR.PreservesProgram (ssa : SSA lang profile) (bir : BIR lang profile) : Prop :=
  BehaviorRefinement (SSA.Execution (language := language)) (BIR.Execution (language := language))
    (SSA.Execution (language := language)).result (BIR.Execution (language := language)).result ssa bir

def BIR.EntryMatches (ssa : SSA lang profile) (bir : BIR lang profile) : Prop :=
  bir.entry = ssa.entry ∧
  (ssa.memoryView (language := language)).graphAt ssa.entry =
    (bir.memoryView (language := language)).graphAt bir.entry

theorem BIR.refinesOfViewEq {ssa : SSA lang profile} {bir : BIR lang profile}
    (same : ssa.memoryView (language := language) = bir.memoryView (language := language)) :
    BIR.PreservesProgram (language := language) ssa bir := by
  let exec := BIR.registerExecution (language := language) (lang := lang) (profile := profile)
  change BehaviorRefinement exec exec exec.result exec.result
    (ssa.memoryView (language := language)) (bir.memoryView (language := language))
  rw [same]
  intro safe
  exact ⟨fun state _ reached result => ⟨state, reached, result, rfl⟩,
    fun run infinite => ⟨run, infinite, fun _ => Iff.rfl⟩, safe⟩

def BuildsBIR (ssa : SSA lang profile) (bir : BIR lang profile) : Prop :=
  bir.project = ssa.project ∧ bir.TrapSlotsWF ∧ bir.TrapsMatch ssa ∧ PreservesOrigins ssa.origins bir.origins ∧
  bir.frameSlots ≤ profile.maxBIRSlots ∧ BIR.EntryMatches (language := language) ssa bir ∧
  BIR.PreservesProgram (language := language) ssa bir

theorem BuildsBIR.refines {ssa : SSA lang profile} {bir : BIR lang profile}
    (built : BuildsBIR (language := language) ssa bir) :
    BIR.PreservesProgram (language := language) ssa bir :=
  built.2.2.2.2.2.2

/-- Visit only CFG successors of the entry. Each distinct block is expanded at
most once. Fuel covers the entry plus at most two queued edges per block. -/
private def reachableCall (graph : BIRGraph lang) :
    Nat → List BlockId → List BlockId → Bool
  | 0, _, _ | _, [], _ => false
  | fuel + 1, id :: pending, visited =>
    if visited.contains id then reachableCall graph fuel pending visited
    else
      match graph.blocks.find? (·.id == id) with
      | none => reachableCall graph fuel pending visited
      | some block =>
        let hasCall := block.body.any fun op =>
          match op.instruction with
          | .call .. => true
          | _ => false
        let successors := match block.ending with
          | .jump edge => [edge.target]
          | .branch _ yes no => [yes.target, no.target]
          | .ret _ | .trap _ => []
        hasCall || reachableCall graph fuel (successors ++ pending) (id :: visited)

def BIR.requiresLCBLinking (bir : BIR lang profile) : Bool :=
  match AList.lookup bir.functions bir.entry with
  | none => false
  | some (_, graph) => reachableCall graph (2 * graph.blocks.size + 1) [graph.entry] []

def LCBEncoding (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) (output : LCBInput profile) : Prop :=
  ((Backend.LCB.job output.body :: List.map Prod.snd output.additionalJobs).map
    (fun job => job.code.symbol.name)).Nodup ∧
  output.sites = bir.sites ∧ output.RuntimeMatches bir.project ∧
  BehaviorRefinement (BIR.Execution (language := language)) (LCBInput.Execution _ bir.project)
    (BIR.Execution (language := language)).result (LCBInput.Execution _ bir.project).result bir output ∧
  (List.map Prod.fst bir.functions).Nodup ∧
  (bir.entry :: List.map Prod.fst output.additionalJobs).Nodup ∧
  (∀ fn, fn ∈ List.map Prod.fst bir.functions ↔ fn ∈ bir.entry :: List.map Prod.fst output.additionalJobs) ∧
  bir.requiresLCBLinking = false ∧
  bir.TrapSlotsWF

def EmitsLCB (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) (output : LCBInput profile) : Prop :=
  profile.backend = .lcb ∧
  LeanCompilerBackend.Verified.validateJob (Backend.LCB.job output.body) = .ok () ∧
  (∀ pair ∈ output.additionalJobs, LeanCompilerBackend.Verified.validateJob pair.2 = .ok ()) ∧
  LCBEncoding language bir output

end Concrete.Interfaces
