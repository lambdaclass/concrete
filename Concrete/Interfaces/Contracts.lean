import Concrete.Interfaces.Carriers
import Concrete.Contracts
import Concrete.Backend.Observation

namespace Concrete.Interfaces

open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface
open Concrete.Contracts (ProcessEvent Semantics EvaluatorAdequacy EvidenceKind Graph StutteringSimulation)

inductive Observation where
  | returned (status : UInt8) (stderr : ByteArray)
  | trapped (status : UInt8) (kind : TrapKind) (site : FileSpan) (stderr : ByteArray)
  | startupRefused (status : UInt8) (stderr : ByteArray)

def Observation.stderr : Observation → ByteArray
  | .returned _ bytes | .trapped _ _ _ bytes | .startupRefused _ bytes => bytes

def Observation.SameRuntimeResult (observed : Observation)
    (legacy : Concrete.Contracts.Observation) : Prop :=
  match observed, legacy with
  | .returned a bytes, .returned b other => a = b ∧ bytes = other
  | .trapped a k span bytes, .trapped b j site other =>
    a = b ∧ k = j ∧ span.line = site.location.line ∧ span.col = site.location.col ∧ bytes = other
  | .startupRefused a bytes, .startupRefused b other => a = b ∧ bytes = other
  | _, _ => False

/-- A precise terminal projection over the same transition system used by the composed theorem.
The bridge retains the old runtime diagnostic equality during migration. -/
structure Model (A E : Type) where
  execution : Semantics A E
  result : A → execution.State → Observation → Prop
  projects : A → Observation → Concrete.Contracts.Observation → Prop
  terminalExact : ∀ a state legacy, execution.result a state legacy ↔
    ∃ observed, result a state observed ∧ projects a observed legacy
  completeProjection : ∀ a state observed, result a state observed →
    ∃ legacy, projects a observed legacy
  runtimeExact : ∀ a observed legacy, projects a observed legacy →
    observed.SameRuntimeResult legacy

def Model.MatchesResult {A B E : Type} (source : Model A E) (target : Model B E)
    {a : A} {b : B} (sim : Concrete.Contracts.Simulates source.execution target.execution a b) : Prop :=
  ∀ p q observed, source.execution.finite a p → target.execution.finite b q →
    sim.simulation.stateRel p q → target.result b q observed →
    ∃ p', Concrete.Contracts.Steps (source.execution.step a) p p' ∧
      source.result a p' observed ∧ source.execution.trace a p' = target.execution.trace b q

def Model.Simulates {A B E : Type} (source : Model A E) (target : Model B E)
    (a : A) (b : B) : Prop :=
  ∃ sim : Concrete.Contracts.Simulates source.execution target.execution a b,
    source.MatchesResult target sim

/-- An external edge can state refinement directly, with the same exact terminal projection. -/
def Model.Refines {A B E : Type} (source : Model A E) (target : Model B E)
    (a : A) (b : B) : Prop :=
  Concrete.Contracts.Refines source.execution target.execution a b ∧
  ∀ state observed, target.execution.finite b state → target.result b state observed →
    ∃ before, source.execution.finite a before ∧ source.result a before observed ∧
      source.execution.trace a before = target.execution.trace b state

/-- Evaluator adequacy stays separate from the fuel-free lowering proposition. -/
abbrev EvaluatorContract {A : Type} (model : Model A Empty)
    (evaluate : A → Nat → Concrete.Contracts.Evaluation) :=
  EvaluatorAdequacy model.execution evaluate

/-- The registry owns the function value and uses it for both execution and the proof graph. -/
structure Stage (Input Failure Output : Type) where
  run : Input → Except Failure Output
  kind : EvidenceKind
  inputInvariant : Input → Prop
  outputInvariant : Output → Prop
  success : Input → Output → Prop
  refusal : Input → Failure → Prop

def Stage.Sound {A R B : Type} (edge : Stage A R B) : Prop :=
  (∀ a b, edge.inputInvariant a → Graph edge.run a b →
    edge.outputInvariant b ∧ edge.success a b) ∧
  (∀ a r, edge.inputInvariant a → edge.run a = .error r → edge.refusal a r)

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

structure FrontendSpec (Ast : Type) (language : CoreLanguage) where
  lexes : Source → TokenStream → Prop
  reconstruct : TokenStream → ByteArray
  malformedToken : Source → RuleId → FileSpan → Prop
  lexLimit : Source → ResourceRefusal → Prop
  elaborates : Parsed Ast → Payload language → Prop
  elabRefusal : Parsed Ast → ElabRefusal → Prop

def Lexes {Ast : Type} {language : CoreLanguage} (spec : FrontendSpec Ast language)
    (source : Source) (stream : TokenStream) : Prop :=
  stream.source = source ∧ spec.lexes source stream ∧ stream.parserView.WF ∧
  stream.trivia.size = stream.tokens.size + 1 ∧
  ∃ file, source.project.files[source.entryFile.index]? = some file ∧
    (spec.reconstruct stream).data.toList = file.source.toBytes

def Elaborates {Ast : Type} {language : CoreLanguage} (spec : FrontendSpec Ast language)
    (ast : Parsed Ast) (core : CoreInput language) : Prop :=
  core.project = ast.source.project ∧ core.slice = ast.source.slice ∧
  core.WF ∧ spec.elaborates ast core.payload

/-- The actual admission entry calls exactly the function supplied by ConcreteSpec. -/
def check {language : CoreLanguage} (spec : Admission language) (core : CoreInput language) :
    Except CheckerRefusal (Admitted language) :=
  (spec.check core).map fun discharges => ⟨core, discharges⟩

def Admits {language : CoreLanguage} (spec : Admission language)
    (core : CoreInput language) (out : Admitted language) : Prop :=
  out.core = core ∧ spec.accepted core ∧ spec.statics core ∧
    spec.fragment core.slice core.payload ∧ ValidDischarges core out.discharges

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

def loadValue (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) : Option language.operands.Val := do
  if !(memory.validPlace state place) then none else
  if !state.live.contains place.logical.root || !state.initialized.contains place.logical.root then none else
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

def writeValue (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (value : language.operands.Val) :
    Option (MemoryRegisters language profile) := do
  let root := place.logical.root
  if !state.live.contains root then none else
  if !place.logical.path.isEmpty && !state.initialized.contains root then none else
  let state := if state.initialized.contains root then state
    else {state with initialized := root :: state.initialized}
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
  | .copy place =>
    if language.operands.isCopy ty then
      return (← memory.loadValue state place, state)
    else none
  | .move place => do
    -- Projected moves require per-field initialization, supplied at their first consumer.
    if !place.logical.path.isEmpty then none else
    let value ← memory.loadValue state place
    let root := place.logical.root
    let moved := {state with
      initialized := state.initialized.erase root
      virtualValues := AList.erase state.virtualValues root
      zeroValues := AList.erase state.zeroValues root}
    return (value, moved)

def Writes (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (value : language.operands.Val)
    (next : MemoryRegisters language profile) : Prop :=
  (memory.validPlace state place = true ∧
    (place.logical.root ∈ state.initialized → language.operands.isCopy ty = true)) ∧
  (place.storage = .materialized → ∃ layout,
    memory.layoutAt state.frame place.logical.root = some layout ∧
    (layout.size = 0 ↔ (AList.lookup state.materialized place.logical.root).isNone)) ∧
  memory.writeValue state place value = some next ∧
  memory.StoresValue next place value

def Represents (memory : Memory lang profile)
    (logical : LogicalFrame language.operands.Val) (state : MemoryRegisters language profile)
    (ranges : AList Hybrid.Place (Option Hybrid.Range) := []) : Prop :=
  state.BytesWF ∧ logical.fn = state.frame ∧ (∀ place ∈ logical.live, place ∈ state.live) ∧
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
      ∃ physical, language.representsValue profile.memoryTarget ranges ty value physical ∧
        memory.StoresValue state (ty := ty) ⟨⟨place, []⟩, storage⟩ physical

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

def canEnd (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) : Bool :=
  !state.initialized.contains place ||
    (memory.rootType state.frame place).any language.operands.isCopy

def Ends (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) (next : MemoryRegisters language profile) : Prop :=
  (place ∈ state.live ∧ memory.canEnd state place) ∧
  let dead := {state with
    live := state.live.erase place
    initialized := state.initialized.erase place
    zeroValues := AList.erase state.zeroValues place
    virtualValues := AList.erase state.virtualValues place
    materialized := AList.erase state.materialized place
    addressIds := AList.erase state.addressIds place}
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

inductive WriteParams (memory : Memory lang profile) :
    List (Sigma (MemoryPlace language.operands.Ty)) → List language.operands.Val →
      MemoryRegisters language profile → MemoryRegisters language profile → Prop
  | nil (state) : WriteParams memory [] [] state state
  | cons {ty} {place : MemoryPlace _ ty} {params value values state started stored next} :
      (if state.live.contains place.logical.root then started = state
        else memory.Begins state place.logical.root started) →
      memory.Writes started place value stored →
      WriteParams memory params values stored next →
      WriteParams memory (⟨ty, place⟩ :: params) (value :: values) state next

inductive EndLocals (memory : Memory lang profile) :
    List PlaceId → MemoryRegisters language profile → MemoryRegisters language profile → Prop
  | nil (state) : EndLocals memory [] state state
  | cons {place rest state ended next} :
      memory.Ends state place ended → EndLocals memory rest ended next →
      EndLocals memory (place :: rest) state next

def Call (memory : Memory lang profile) (callee : FnId)
    (dst : Option (Sigma (MemoryPlace language.operands.Ty)))
    (args : List (Sigma (MemoryRead _ lang.literal)))
    (state next : MemoryRegisters language profile) : Prop :=
  ∃ values afterRead graph entry,
    memory.readOperands args state = some (values, afterRead) ∧
    memory.graphAt callee = some graph ∧ graph.blocks.find? (·.id == graph.entry) = some entry ∧
    args.map Sigma.fst = entry.params.map Sigma.fst ∧
    memory.WriteParams entry.params values
      { toMemoryFrame := ⟨callee, graph.entry, 0, [], [], [], [], [], []⟩
        bytes := afterRead.bytes
        ownership := afterRead.ownership
        nextAddressId := afterRead.nextAddressId
        effects := afterRead.effects
        callers := ⟨afterRead.advance.toMemoryFrame, dst⟩ :: afterRead.callers } next

def Return (memory : Memory lang profile) (operand : Option (Sigma (MemoryRead _ lang.literal)))
    (state next : MemoryRegisters language profile) : Prop :=
  ∃ caller rest values afterRead ended,
    state.callers = caller :: rest ∧
    memory.readOperands operand.toList state = some (values, afterRead) ∧
    memory.EndLocals afterRead.live afterRead ended ∧
    operand.toList.map Sigma.fst = caller.destination.toList.map Sigma.fst ∧
    memory.WriteParams caller.destination.toList values
      { toMemoryFrame := caller.caller, bytes := ended.bytes, callers := rest, effects := ended.effects, ownership := ended.ownership, nextAddressId := ended.nextAddressId } next

def Transfer (memory : Memory lang profile) (edge : CFGEdge (Sigma (MemoryRead _ lang.literal)))
    (state next : MemoryRegisters language profile) : Prop :=
  ∃ values afterRead block,
    memory.readOperands edge.arguments state = some (values, afterRead) ∧
    memory.blockAt {state with block := edge.target} = some block ∧
    edge.arguments.map Sigma.fst = block.params.map Sigma.fst ∧
    memory.WriteParams block.params values {afterRead with block := edge.target, instruction := 0} next

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
      !state.initialized.contains place.logical.root then none else
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
    (state next : MemoryRegisters language profile) (logical startDefinitions : Bool) : MemoryInst lang → Prop
  | .bind dst src =>
    ∃ value afterRead stored, memory.readValue state src = some (value, afterRead) ∧
      (if logical || startDefinitions then memory.WriteParams [⟨_, dst⟩] [value] afterRead stored
        else memory.Writes afterRead dst value stored) ∧ next = stored.advance
  | .assign dst src =>
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
      (if logical || startDefinitions then memory.WriteParams [⟨_, dst⟩] [result] afterOperation stored
        else memory.Writes afterOperation dst result stored) ∧
      next = {stored.advance with effects := stored.effects ++ effects}
  | .beginLifetime place => ∃ started, memory.Begins state place started ∧ next = started.advance
  | .endLifetime place => ∃ ended, memory.Ends state place ended ∧ next = ended.advance
  | @MemoryInst.destructure _ _ ty src bindings =>
    ∃ value afterRead fields stored,
      memory.readValue state (.move src) = some (value, afterRead) ∧
      DestructureFields language ty value (bindings.map fun (field, dst) => (field, dst.1)) ∧
      bindings.mapM (fun binding => language.field value [binding.1]) = some fields ∧
      memory.WriteParams (bindings.map Prod.snd) fields afterRead stored ∧ next = stored.advance
  | .call callee dst args checks =>
    memory.instructionTrap state (.call callee dst args checks) = none ∧
      memory.Call callee dst args state next

def InstructionStep (memory : Memory lang profile)
    (state next : MemoryRegisters language profile) (logical : Bool := false) : MemoryInst lang → Prop :=
  memory.InstructionStepWith state next logical false

def ControlStep (memory : Memory lang profile)
    (state next : MemoryRegisters language profile) :
    Terminator (Sigma (MemoryRead _ lang.literal)) (MemoryRead _ lang.literal lang.bool) → Prop
  | .jump edge => memory.Transfer edge state next
  | .branch test yes no =>
    ∃ value afterRead flag, memory.readValue state test = some (value, afterRead) ∧
      language.operands.test value = some flag ∧
      memory.Transfer (if flag then yes else no) afterRead next
  | .ret value => memory.Return value state next
  | .trap .. => False

def Step (memory : Memory lang profile)
    (state next : MemoryRegisters language profile) : Prop :=
  ∃ block, memory.blockAt state = some block ∧
    match block.body[state.instruction]? with
    | some op => memory.InstructionStep state next false op.instruction
    | none => state.instruction = block.body.length ∧ memory.ControlStep state next block.ending

def Initial (memory : Memory lang profile) (state : MemoryRegisters language profile) : Prop :=
  state.frame = memory.entry ∧ state.callers = [] ∧
  (∃ graph, memory.graphAt state.frame = some graph ∧ graph.entry = state.block) ∧
  (∃ block, memory.blockAt state = some block ∧ block.params = []) ∧
  state.instruction = 0 ∧ state.virtualValues = [] ∧
  state.materialized = [] ∧ state.live = [] ∧ state.initialized = [] ∧ state.zeroValues = [] ∧
  state.bytes = Mem.Memory.empty ∧ state.effects = [] ∧ state.ownership = {} ∧
  state.addressIds = [] ∧ state.nextAddressId = 0

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

def coreRuntimeEvents {language : CoreLanguage} (core : CoreInput language)
    (state : Exec.Config core.payload.machine) : List (ProcessEvent language.operands.Event) :=
  match state.final? with
  | some (.trapped kind site) => (coreTrapDiagnostic core kind site).data.toList.map .stderr
  | _ => []

def coreObservation {language : CoreLanguage} (core : CoreInput language)
    (outcome : Exec.Final language.operands.Val) : Option Observation := do
  match ← language.hostedResult outcome with
  | .returned status => return .returned status ByteArray.empty
  | .trapped status kind site =>
    let origin ← AList.lookup core.payload.origins site
    return .trapped status kind origin.lexical (coreTrapDiagnostic core kind site)

/-- Relate the shared theorem model to actual spec configurations and spec transition labels. -/
structure CoreModel (language : CoreLanguage) where
  model : Model (CoreInput language) language.operands.Event
  config : (a : CoreInput language) → model.execution.State → Exec.Config a.payload.machine
  specTrace : CoreInput language → model.execution.State → List language.operands.Event
  initialExact : ∀ a state, model.execution.initial a state →
    config a state = a.payload.initial ∧ specTrace a state = [] ∧
      model.execution.trace a state = []
  traceExact : ∀ a state, model.execution.trace a state =
    (specTrace a state).map ProcessEvent.effect ++ coreRuntimeEvents a (config a state)
  authorityExact : ∀ a e, model.execution.permitted a e ↔
    ∃ state events, Exec.Steps a.payload.body a.payload.initial events state ∧
      (e ∈ coreRuntimeEvents a state ∨
        ∃ label next, Exec.Step a.payload.body state label next ∧
          e ∈ label.events.map ProcessEvent.effect)
  stepsExact : ∀ a state next, model.execution.step a state next →
    ∃ label, Exec.Step a.payload.body (config a state) label (config a next) ∧
      specTrace a next = specTrace a state ++ label.events
  stepsComplete : ∀ a state label q, model.execution.finite a state →
    Exec.Step a.payload.body (config a state) label q →
    ∃ next, model.execution.step a state next ∧ config a next = q ∧
      specTrace a next = specTrace a state ++ label.events
  safetyCoversTraps : ∀ (a : CoreInput language) state events site op label next kind,
    Exec.Steps a.payload.body a.payload.initial events state →
    a.payload.activeOperation state = some (site, op) →
    Exec.Step a.payload.body state label next → next.ctrl = .trapped kind site →
    ∃ condition ∈ language.safety op, condition.trap = kind
  terminalExact : ∀ a state observed, model.result a state observed ↔
    ∃ outcome, (config a state).final? = some outcome ∧ coreObservation a outcome = some observed

structure LoweringModels (language : CoreLanguage) (lang : LoweringLanguage language.operands.Ty)
    (profile : Profile) where
  core : CoreModel language
  mir : Model (MIR lang profile) language.operands.Event
  memory : Model (Memory lang profile) language.operands.Event
  ssa : Model (SSA lang profile) language.operands.Event
  bir : Model (BIR lang profile) language.operands.Event
  llvm : Model (LLVMInput profile) language.operands.Event
  mirInvariant : MIR lang profile → Prop
  memoryInvariant : Memory lang profile → Prop
  ssaInvariant : SSA lang profile → Prop
  birInvariant : BIR lang profile → Prop
  memoryState : memory.execution.State → MemoryRegisters language profile
  mirState : mir.execution.State → MemoryRegisters language profile
  llvmHandoff : BIR lang profile → LLVMInput profile → Prop

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

def LowersCore (m : LoweringModels language lang profile)
    (input : Admitted language) (mir : MIR lang profile) : Prop :=
  mir.project = input.core.project ∧ mir.entry = input.core.payload.entry ∧
  PreservesOrigins (List.map Prod.snd input.core.payload.origins) mir.origins ∧
  ConsumesEvidence input.core input.discharges mir ∧ m.core.model.Simulates m.mir input.core mir

def LowersUnchecked (m : LoweringModels language lang profile)
    (input : CoreInput language) (mir : MIR lang profile) : Prop :=
  mir.project = input.project ∧ mir.entry = input.payload.entry ∧
  PreservesOrigins (List.map Prod.snd input.payload.origins) mir.origins ∧
  ConsumesEvidence input [] mir ∧ m.core.model.Simulates m.mir input mir

private def executionTrace (state : MemoryRegisters language profile)
    (observation : Option Observation) : List (ProcessEvent language.operands.Event) :=
  state.effects.map ProcessEvent.effect ++
    match observation with
    | some (.trapped _ _ _ diagnostic) => diagnostic.data.toList.map ProcessEvent.stderr
    | _ => []

def Memory.ExecutionExact (memory : Memory lang profile)
    (m : LoweringModels language lang profile) : Prop :=
  ((∀ q, m.memory.execution.initial memory q → memory.Initial (m.memoryState q)) ∧
   (∀ q, m.memory.execution.finite memory q → m.memory.execution.trace memory q =
     executionTrace (m.memoryState q) (memory.terminalObservation (m.memoryState q)))) ∧
  (∀ q next, m.memory.execution.finite memory q → m.memory.execution.step memory q next →
    memory.Step (m.memoryState q) (m.memoryState next)) ∧
  (∀ q state, m.memory.execution.finite memory q → memory.Step (m.memoryState q) state →
    ∃ next, m.memory.execution.step memory q next ∧ m.memoryState next = state) ∧
  ∀ q result, m.memory.execution.finite memory q →
    (m.memory.result memory q result ↔
      memory.terminalObservation (m.memoryState q) = some result)

def MIR.ExecutionExact (mir : MIR lang profile)
    (m : LoweringModels language lang profile) : Prop :=
  ((∀ p, m.mir.execution.initial mir p → mir.logicalView.Initial (m.mirState p)) ∧
   (∀ p, m.mir.execution.finite mir p → m.mir.execution.trace mir p =
     executionTrace (m.mirState p) (mir.terminalObservation (m.mirState p)))) ∧
  (∀ p next, m.mir.execution.finite mir p → m.mir.execution.step mir p next →
    mir.Step (m.mirState p) (m.mirState next)) ∧
  (∀ p state, m.mir.execution.finite mir p → mir.Step (m.mirState p) state →
    ∃ next, m.mir.execution.step mir p next ∧ m.mirState next = state) ∧
  ∀ p result, m.mir.execution.finite mir p →
    (m.mir.result mir p result ↔ mir.terminalObservation (m.mirState p) = some result)

def Memory.RepresentsActivation (memory : Memory lang profile)
    (logical physical : MemoryFrame language.operands.Val) (bytes : Mem.Memory)
    (ranges : AList Hybrid.Place (Option Hybrid.Range) := []) : Prop :=
  memory.Represents ⟨logical.frame, logical.live, logical.virtualValues⟩
    ⟨physical, bytes, [], [], {}, 0⟩ ranges

def MemoryFrame.backingRanges (frame : MemoryFrame language.operands.Val) :
    AList Hybrid.Place (Option Hybrid.Range) :=
  List.map (fun (place, provenance) => (provenance, AList.lookup frame.materialized place)) frame.addressIds

private def addressIdsMatch (memory : Memory lang profile)
    (source target : MemoryFrame language.operands.Val) : Prop :=
  source.addressIds = target.addressIds ∧
  ∀ place id, (place, id) ∈ source.addressIds →
    place ∈ source.live ∧
    (memory.zeroRoot target.frame place = true ∨ (AList.lookup target.materialized place).isSome)

/-- The target simulates source behavior. At related prefixes with a source successor,
there is a compatible source successor with a represented target checkpoint.
This preserves materialization while allowing the target to commit to permitted choices early. -/
def LowersMemory (m : LoweringModels language lang profile)
    (mir : MIR lang profile) (memory : Memory lang profile) : Prop :=
  memory.project = mir.project ∧ memory.entry = mir.entry ∧
  PreservesOrigins mir.origins memory.origins ∧ LayoutsMatch language.layout memory ∧
  memory.signatures = mir.signatures ∧ PromotionSound (language := language) mir memory ∧
  memory.ExecutionExact m ∧ mir.ExecutionExact m ∧ memory.TablesWF ∧ mir.logicalView.TablesWF ∧
  ∃ sim : Concrete.Contracts.Simulates m.mir.execution m.memory.execution mir memory,
    m.mir.MatchesResult m.memory sim ∧
    (∀ p, m.mir.execution.initial mir p → ∃ q, m.memory.execution.initial memory q ∧
      sim.simulation.stateRel p q) ∧
    (∀ p q, m.mir.execution.finite mir p → m.memory.execution.finite memory q →
      sim.simulation.stateRel p q → (∃ p', m.mir.execution.step mir p p') →
      ∃ p' q', m.mir.execution.step mir p p' ∧
        Concrete.Contracts.Steps (m.memory.execution.step memory) q q' ∧
        sim.simulation.stateRel p' q') ∧
    ∀ p q, m.mir.execution.finite mir p → m.memory.execution.finite memory q →
      sim.simulation.stateRel p q →
      let source := m.mirState p
      let target := m.memoryState q
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

def BuildsSSA (m : LoweringModels language lang profile)
    (memory : Memory lang profile) (ssa : SSA lang profile) : Prop :=
  ssa.project = memory.project ∧ ssa.entry = memory.entry ∧ ssa.layouts = memory.layouts ∧
  ssa.signatures = memory.signatures ∧ PreservesOrigins memory.origins ssa.origins ∧ m.memory.Simulates m.ssa memory ssa

def BIR.TrapsMatch {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (ssa : SSA lang profile) (bir : BIR lang profile) : Prop :=
  ∀ fn ∈ ssa.functions, ∀ block ∈ fn.2.2.blocks.toList, ∀ kind origin,
    block.ending = .trap kind origin →
    ∃ after ∈ bir.functions, after.1 = fn.1 ∧
      ∃ next ∈ after.2.2.blocks.toList, next.id = block.id ∧
        ∃ slot, next.ending = .trap slot ∧
          ∃ entry, bir.sites[slot.index]? = some entry ∧ entry.kind = kind ∧ entry.origin = origin

def EmitsLLVM (m : LoweringModels language lang profile)
    (bir : BIR lang profile) (output : LLVMInput profile) : Prop :=
  profile.backend = .llvm ∧ output.triple = profile.llvmTriple ∧
  output.dataLayout = profile.llvmDataLayout ∧ output.sites = bir.sites ∧ m.llvmHandoff bir output ∧ m.bir.Refines m.llvm bir output

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

private def register {ty : language.operands.Ty} (value : SSAValue _ ty) : MemoryPlace _ ty :=
  ⟨⟨⟨2 * value.id + 1⟩, []⟩, .virtual⟩

private def materialized {ty : language.operands.Ty} (place : Place _ ty) : MemoryPlace _ ty :=
  ⟨{place with root := ⟨2 * place.root.id⟩}, .materialized⟩

private def registerRead {ty : language.operands.Ty} :
    SSARead _ lang.literal ty → MemoryRead _ lang.literal ty
  | .literal literal => .literal literal
  | .value value => if language.operands.isCopy ty then .copy (register value) else .move (register value)

private def operand (arg : Sigma (SSARead _ lang.literal)) :
    Sigma (MemoryRead language.operands.Ty lang.literal) :=
  ⟨arg.1, registerRead arg.2⟩

private def arguments : {types : List language.operands.Ty} →
    Args (SSARead _ lang.literal) types → Args (MemoryRead _ lang.literal) types
  | _, .nil => .nil
  | _, .cons arg rest => .cons (registerRead arg) (arguments rest)

private def instruction : SSAInst lang → MemoryInst lang
  | .op dst op args addresses =>
    .op (register dst) op (arguments args) [] (addresses.map fun p => ⟨p.1, materialized p.2⟩)
  | .load (ty := ty) dst src =>
    .bind (register dst) (if language.operands.isCopy ty then .copy (materialized src)
      else .move (materialized src))
  | .store dst src => .assign (materialized dst) (registerRead src)
  | .beginLifetime place => .beginLifetime ⟨2 * place.id⟩
  | .endLifetime place => .endLifetime ⟨2 * place.id⟩
  | .call fn dst args => .call fn (dst.map fun p => ⟨p.1, register p.2⟩) (args.map operand) []

private def registerDeclarations : SSAInst lang → LocalTypes language.operands.Ty
  | .op (ty := ty) dst .. | .load (ty := ty) dst .. => [((register dst).logical.root, ty)]
  | .call _ (some ⟨ty, dst⟩) _ => [((register dst).logical.root, ty)]
  | _ => []

private def terminator (bir : BIR lang profile) :
    BIRTerm (Sigma (SSARead _ lang.literal)) (SSARead _ lang.literal lang.bool) → MemoryTerm lang
  | .ret value => .ret (value.map operand)
  | .jump edge => .jump ⟨edge.target, edge.arguments.map operand⟩
  | .branch test yes no => .branch (registerRead test)
      ⟨yes.target, yes.arguments.map operand⟩ ⟨no.target, no.arguments.map operand⟩
  | .trap slot => match bir.sites[slot.index]? with
    | some entry => .trap entry.kind entry.origin
    | none => .ret none

/-- Interpret the exact BIR graph with the shared CFG rules. Register IDs and
byte-backed roots occupy disjoint namespaces. Trap slots resolve in this graph's table. -/
def memoryView (bir : BIR lang profile) : Memory lang profile :=
  let functions := List.map (fun (fn, locals, graph) =>
    let registers := graph.blocks.toList.flatMap fun block =>
      (block.params.map fun p => ((register p.2).logical.root, p.1)) ++
      (block.body.flatMap fun op => registerDeclarations op.instruction)
    let roots : LocalTypes language.operands.Ty :=
      List.map (fun (place, ty) => (⟨2 * place.id⟩, ty)) locals
    let body : MemoryGraph lang := ⟨graph.entry, graph.blocks.map fun block =>
      ⟨block.id, block.params.map (fun p => ⟨p.1, register p.2⟩),
        block.body.map (fun op => ⟨op.origin, instruction op.instruction⟩),
        terminator bir block.ending, block.boundaryOrigins⟩⟩
    ((fn, roots ++ registers, body),
      (fn, List.map (fun p => (p.1, StorageClass.materialized)) roots ++
        List.map (fun p => (p.1, StorageClass.virtual)) registers))) bir.functions
  ⟨bir.project, bir.entry, bir.signatures, functions.map Prod.fst,
    List.map (fun (fn, slots) => (fn, List.map (fun (place, layout) => (PlaceId.mk (2 * place.id), layout)) slots)) bir.layouts,
    functions.map Prod.snd⟩

def entryState (bir : BIR lang profile) : MemoryRegisters language profile :=
  let block := ((bir.memoryView (language := language)).graphAt bir.entry).map CFG.entry
  ⟨⟨bir.entry, block.getD ⟨0⟩, 0, [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0⟩

def step (bir : BIR lang profile) (state next : MemoryRegisters language profile) : Prop :=
  let memory := bir.memoryView (language := language)
  ∃ block, memory.blockAt state = some block ∧
    match block.body[state.instruction]? with
    | some op => memory.InstructionStepWith state next false true op.instruction
    | none => state.instruction = block.body.length ∧ memory.ControlStep state next block.ending

def hostedResult (bir : BIR lang profile) (state : MemoryRegisters language profile) :
    Option Concrete.Contracts.Observation := do
  match ← (bir.memoryView (language := language)).terminalObservation state with
  | .returned status bytes => return .returned status bytes
  | .trapped status kind span bytes =>
    let block ← (bir.memoryView (language := language)).blockAt state
    let .trap _ origin := block.ending | none
    let file ← bir.project.files[span.file.index]?
    return .trapped status kind ⟨⟨origin.operation.id⟩, ⟨file.path.render.text, span.line, span.col⟩⟩ bytes
  | .startupRefused status bytes => return .startupRefused status bytes

private def trace (bir : BIR lang profile) (state : MemoryRegisters language profile) :
    List (ProcessEvent language.operands.Event) :=
  state.effects.map .effect ++
    ((hostedResult bir state).map fun o => o.stderr.data.toList.map .stderr).getD []

/-- Source semantics for backend refinement. Fetch, reads, operations and terminal
observations are determined by the supplied BIR graph. -/
def Execution : Semantics (BIR lang profile) language.operands.Event where
  State := MemoryRegisters language profile
  initial bir state := state = entryState bir
  initialized _ := ⟨_, rfl⟩
  step bir state next :=
    hostedResult bir state = none ∧ bir.step state next ∧
      Concrete.Contracts.TracePrefix (trace bir state) (trace bir next)
  result bir state result := hostedResult bir state = some result
  terminal _ _ _ h _ tick := by
    have impossible := h.symm.trans tick.1
    contradiction
  safe bir state :=
    (hostedResult bir state).isSome = true ∨
    ∃ next, hostedResult bir state = none ∧ bir.step state next ∧
      Concrete.Contracts.TracePrefix (trace bir state) (trace bir next)
  trace := trace
  permitted _ _ := True
  resultTrace := by
    intro bir state result h
    unfold trace
    rw [h]
    simp only [Option.map_some, Option.getD_some]
    have stderrExact : ∀ effects : List language.operands.Event, ∀ bytes : List UInt8,
        Concrete.Contracts.stderrBytes (effects.map ProcessEvent.effect ++ bytes.map ProcessEvent.stderr) = bytes := by
      intro effects bytes
      induction effects with
      | nil => induction bytes with
        | nil => rfl
        | cons b rest ih => exact congrArg (b :: ·) ih
      | cons _ _ ih => exact ih
    exact stderrExact _ _
  traceGrows _ _ _ tick := tick.2.2

end BIR

def BuildsBIR (m : LoweringModels language lang profile)
    (ssa : SSA lang profile) (bir : BIR lang profile) : Prop :=
  bir.project = ssa.project ∧ bir.entry = ssa.entry ∧ bir.layouts = ssa.layouts ∧
  bir.signatures = ssa.signatures ∧ bir.TrapSlotsWF ∧ bir.TrapsMatch ssa ∧ PreservesOrigins ssa.origins bir.origins ∧
  bir.frameSlots ≤ profile.maxBIRSlots ∧
  m.bir.execution = BIR.Execution (language := language) ∧ m.ssa.Simulates m.bir ssa bir

def EmitsLCB (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) (output : LCBInput profile) : Prop :=
  profile.backend = .lcb ∧
  LeanCompilerBackend.Verified.validateJob (Backend.LCB.job output.body) = .ok () ∧
  (∀ job ∈ output.additionalJobs, LeanCompilerBackend.Verified.validateJob job = .ok ()) ∧
  ((Backend.LCB.job output.body :: output.additionalJobs).map
    (fun job => job.code.symbol.name)).Nodup ∧
  output.sites = bir.sites ∧ output.RuntimeMatches bir.project ∧
  Concrete.Contracts.Refines (BIR.Execution (language := language))
    (LCBInput.Execution _ bir.project) bir output

end Concrete.Interfaces
