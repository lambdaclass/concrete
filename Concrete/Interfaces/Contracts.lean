import Concrete.Interfaces.Carriers
import Concrete.Contracts
import Concrete.Backend.LCB

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
        | @MIRInst.op _ _ opcode _ _ _ _ args checks =>
          some ⟨fn, block.id, index, inst.origin, .operator opcode, args.identities, checks⟩
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

def PromotionSound {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (scalar : Ty → Prop) (escapes : MIR lang profile → FnId → PlaceId → Prop)
    (mir : MIR lang profile) (memory : Memory lang profile) : Prop :=
  ∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
    ∀ place ty, (place, ty) ∈ locals →
      ∃ classes, AList.lookup memory.storage fn = some classes ∧
        ((AList.lookup classes place = some .virtual) ↔ scalar ty ∧ ¬ escapes mir fn place)

/-- The logical MIR frame used to verify Memory's representation of live places. -/
structure LogicalFrame (Value : Type) where
  fn : FnId
  live : List PlaceId
  values : AList PlaceId Value

/-- The spec supplies typed literal decoding, field projection and active bytes.
An encoding is a list of offset/byte runs so padding is outside the write footprint. -/
structure MemoryValues (Ty Value : Type) where
  literal : Ty → List UInt8 → Option Value
  field : Value → List FieldId → Option Value
  replaceField : Value → List FieldId → Value → Option Value
  fieldRange : (ty : Ty) → List FieldId → Layout → Option (Nat × Layout)
  encode : Target → Ty → Value → Option (List (Nat × List Mem.MByte))
  decode : Target → Ty → List Mem.MByte → Option Value

abbrev MemoryRegisters (language : CoreLanguage) (profile : Profile) :=
  MemoryState language.operands.Val profile

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

def rangeAt (values : MemoryValues language.operands.Ty language.operands.Val)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty} (place : Place _ ty) :
    Option Hybrid.Range := do
  let root ← AList.lookup state.materialized place.root
  if place.path.isEmpty then return root
  let (offset, field) ← values.fieldRange ty place.path ⟨root.extent, root.align⟩
  if offset + field.size ≤ root.extent then
    some ⟨{root.ptr with addr := root.ptr.addr + offset}, field.size, field.align⟩
  else none

def loadValue (values : MemoryValues language.operands.Ty language.operands.Val)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) : Option language.operands.Val := do
  if !state.initialized.contains place.logical.root then none else
  match place.storage with
  | .virtual =>
    let root ← AList.lookup state.virtualValues place.logical.root
    if place.logical.path.isEmpty then some root else values.field root place.logical.path
  | .materialized =>
    let range ← rangeAt values state place.logical
    let bytes ← state.bytes.load profile.memoryTarget range.ptr range.extent range.align
    values.decode profile.memoryTarget ty bytes

private def byteRuns (values : MemoryValues language.operands.Ty language.operands.Val)
    (target : Target) (ty : language.operands.Ty) (value : language.operands.Val) :
    Option (List (Nat × List Mem.MByte)) :=
  match language.valueShape ▸ value with
  | .scalar scalar => (Hybrid.encode target scalar).map fun bytes => [(0, bytes)]
  | .other _ => values.encode target ty value

private def StoresValue (values : MemoryValues language.operands.Ty language.operands.Val)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (value : language.operands.Val) : Prop :=
  loadValue values state place = some value ∧
  (place.storage = .materialized →
    ∃ range runs, rangeAt values state place.logical = some range ∧
      byteRuns values profile.memoryTarget ty value = some runs ∧
      (0 < range.extent → ∃ offset bytes, (offset, bytes) ∈ runs ∧ bytes ≠ []) ∧
      ∀ offset bytes, (offset, bytes) ∈ runs →
        offset + bytes.length ≤ range.extent ∧ (∀ byte ∈ bytes, byte ≠ .uninit) ∧
        state.bytes.load profile.memoryTarget {range.ptr with addr := range.ptr.addr + offset}
          bytes.length 1 = some bytes)

def writeValue (values : MemoryValues language.operands.Ty language.operands.Val)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (value : language.operands.Val) :
    Option (MemoryRegisters language profile) := do
  let root := place.logical.root
  if !state.live.contains root then none else
  let state := if state.initialized.contains root then state
    else {state with initialized := root :: state.initialized}
  match place.storage with
  | .virtual =>
    let stored ← if place.logical.path.isEmpty then some value else do
      let before ← AList.lookup state.virtualValues root
      values.replaceField before place.logical.path value
    return {state with virtualValues := (root, stored) :: AList.erase state.virtualValues root}
  | .materialized =>
    let range ← rangeAt values state place.logical
    let runs ← byteRuns values profile.memoryTarget ty value
    let bytes ← runs.foldlM (fun mem (offset, bytes) =>
      if offset + bytes.length ≤ range.extent then
        mem.store profile.memoryTarget {range.ptr with addr := range.ptr.addr + offset} bytes 1
      else none) state.bytes
    return {state with bytes := bytes}

def readValue (values : MemoryValues language.operands.Ty language.operands.Val)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (read : MemoryRead _ lang.literal ty) :
    Option (language.operands.Val × MemoryRegisters language profile) := do
  match read with
  | .literal value => return (← values.literal ty value.encoding, state)
  | .copy place => return (← loadValue values state place, state)
  | .move place => do
    -- Projected moves require per-field initialization, supplied at their first consumer.
    if !place.logical.path.isEmpty then none else
    let value ← loadValue values state place
    let root := place.logical.root
    let moved := {state with
      initialized := state.initialized.erase root
      virtualValues := AList.erase state.virtualValues root}
    return (value, moved)

def Writes (memory : Memory lang profile)
    (values : MemoryValues language.operands.Ty language.operands.Val)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (value : language.operands.Val)
    (next : MemoryRegisters language profile) : Prop :=
  memory.classAt state.frame place.logical.root = some place.storage ∧
  writeValue values state place value = some next ∧
  StoresValue values next place value

def Represents (memory : Memory lang profile)
    (values : MemoryValues language.operands.Ty language.operands.Val)
    (logical : LogicalFrame language.operands.Val) (state : MemoryRegisters language profile) : Prop :=
  state.BytesWF ∧ logical.fn = state.frame ∧ logical.live = state.live ∧
  (List.map Prod.fst logical.values).Nodup ∧ (List.map Prod.fst state.materialized).Nodup ∧
  (List.map Prod.fst state.virtualValues).Nodup ∧
  (∀ place, place ∈ state.initialized ↔ (AList.lookup logical.values place).isSome) ∧
  (∀ place value, (place, value) ∈ state.virtualValues →
    place ∈ state.initialized ∧ memory.classAt state.frame place = some .virtual) ∧
  (∀ place range, (place, range) ∈ state.materialized →
    place ∈ state.live ∧ memory.classAt state.frame place = some .materialized ∧
    memory.rootRange state place range) ∧
  ∀ place ∈ state.live, ∃ locals graph ty storage,
    (state.frame, locals, graph) ∈ memory.functions ∧
    AList.lookup locals place = some ty ∧ memory.classAt state.frame place = some storage ∧
    (storage = .materialized → (AList.lookup state.materialized place).isSome) ∧
    ∀ value, AList.lookup logical.values place = some value →
      StoresValue values state (ty := ty) ⟨⟨place, []⟩, storage⟩ value

end Memory

/-- Slice-specific operation values, function-frame transitions and ownership effects.
The fixed storage rules below fetch instructions themselves and handle stores and lifetimes. -/
structure MemoryActions (language : CoreLanguage) (lang : LoweringLanguage language.operands.Ty)
    (profile : Profile) where
  values : MemoryValues language.operands.Ty language.operands.Val
  operation : {opcode : Operator} → {types : List language.operands.Ty} →
    {ty : language.operands.Ty} → lang.operation opcode types ty →
    List language.operands.Val → language.operands.Val → Prop
  destructure : {ty : language.operands.Ty} → MemoryPlace _ ty →
    List (FieldId × Sigma (MemoryPlace language.operands.Ty)) →
    MemoryRegisters language profile → MemoryRegisters language profile → Prop
  call : Memory lang profile → FnId → Option (Sigma (MemoryPlace language.operands.Ty)) →
    List (Sigma (MemoryRead language.operands.Ty lang.literal)) →
    MemoryRegisters language profile → MemoryRegisters language profile → Prop
  transfer : Memory lang profile → CFGEdge (Sigma (MemoryRead _ lang.literal)) →
    MemoryRegisters language profile → MemoryRegisters language profile → Prop
  returnToCaller : Memory lang profile → Option (Sigma (MemoryRead _ lang.literal)) →
    MemoryRegisters language profile → MemoryRegisters language profile → Prop

namespace Memory
variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
  {profile : Profile}

def readArgs (values : MemoryValues language.operands.Ty language.operands.Val) :
    {types : List language.operands.Ty} → Args (MemoryRead _ lang.literal) types →
    MemoryRegisters language profile →
    Option (List language.operands.Val × MemoryRegisters language profile)
  | _, .nil, state => some ([], state)
  | _, .cons read rest, state => do
    let (value, state) ← readValue values state read
    let (tail, state) ← readArgs values rest state
    return (value :: tail, state)

def Begins (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) (next : MemoryRegisters language profile) : Prop :=
  place ∉ state.live ∧
  let live := {state with live := place :: state.live}
  (memory.classAt state.frame place = some .virtual ∧ next = live) ∨
  (memory.classAt state.frame place = some .materialized ∧
    ∃ layout space base block bytes,
      memory.layoutAt state.frame place = some layout ∧
      state.bytes.alloc profile.memoryTarget space base layout.size layout.align = .ok (block, bytes) ∧
      next = {live with bytes := bytes, materialized :=
        (place, ⟨⟨space, base, some block⟩, layout.size, layout.align⟩) :: state.materialized})

def Ends (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) (next : MemoryRegisters language profile) : Prop :=
  place ∈ state.live ∧
  let dead := {state with
    live := state.live.erase place
    initialized := state.initialized.erase place
    virtualValues := AList.erase state.virtualValues place
    materialized := AList.erase state.materialized place}
  (memory.classAt state.frame place = some .virtual ∧ next = dead) ∨
  (memory.classAt state.frame place = some .materialized ∧
    ∃ range block bytes, AList.lookup state.materialized place = some range ∧ range.ptr.prov = some block ∧
      state.bytes.free block = some bytes ∧ next = {dead with bytes := bytes})

def InstructionStep (memory : Memory lang profile) (actions : MemoryActions language lang profile)
    (state next : MemoryRegisters language profile) : MemoryInst lang → Prop
  | .bind dst src | .assign dst src =>
    ∃ value afterRead stored, readValue actions.values state src = some (value, afterRead) ∧
      memory.Writes actions.values afterRead dst value stored ∧ next = stored.advance
  | .op dst op args =>
    ∃ values afterReads result stored, readArgs actions.values args state = some (values, afterReads) ∧
      actions.operation op values result ∧ memory.Writes actions.values afterReads dst result stored ∧
      next = stored.advance
  | .beginLifetime place => ∃ started, memory.Begins state place started ∧ next = started.advance
  | .endLifetime place => ∃ ended, memory.Ends state place ended ∧ next = ended.advance
  | .destructure src bindings =>
    ∃ destructured, actions.destructure src bindings state destructured ∧
      destructured.hasCaller = state.hasCaller ∧ next = destructured.advance
  | .call callee dst args => actions.call memory callee dst args state next

def ControlStep (memory : Memory lang profile) (actions : MemoryActions language lang profile)
    (state next : MemoryRegisters language profile) :
    Terminator (Sigma (MemoryRead _ lang.literal)) (MemoryRead _ lang.literal lang.bool) → Prop
  | .jump edge => next.frame = state.frame ∧ next.hasCaller = state.hasCaller ∧
    next.block = edge.target ∧ next.instruction = 0 ∧
    actions.transfer memory edge state next
  | .branch test yes no =>
    ∃ value read flag, readValue actions.values state test = some (value, read) ∧
      language.operands.test value = some flag ∧
      let edge := if flag then yes else no
      next.frame = state.frame ∧ next.hasCaller = state.hasCaller ∧
      next.block = edge.target ∧ next.instruction = 0 ∧
      actions.transfer memory edge read next
  | .ret value => state.hasCaller = true ∧ actions.returnToCaller memory value state next
  | .trap .. => False

def Step (memory : Memory lang profile) (actions : MemoryActions language lang profile)
    (state next : MemoryRegisters language profile) : Prop :=
  ∃ block, memory.blockAt state = some block ∧
    match block.body[state.instruction]? with
    | some op => memory.InstructionStep actions state next op.instruction
    | none => state.instruction = block.body.length ∧ memory.ControlStep actions state next block.ending

def Initial (memory : Memory lang profile) (state : MemoryRegisters language profile) : Prop :=
  state.frame = memory.entry ∧ state.hasCaller = false ∧ (∃ graph, memory.graphAt state.frame = some graph ∧
    graph.entry = state.block) ∧ state.instruction = 0 ∧ state.virtualValues = [] ∧
  state.materialized = [] ∧ state.live = [] ∧ state.initialized = [] ∧ state.bytes = Mem.Memory.empty

end Memory

/-- Render the hosted trap diagnostic from the originating source location. -/
def trapDiagnostic (project : Project) (kind : TrapKind) (site : SiteId)
    (span : FileSpan) : ByteArray := Id.run do
  let some file := project.files[span.file.index]? | return ByteArray.empty
  return s!"concrete: trap: {kind.name} at {file.path.render.text}:{span.line}:{span.col} (site {site.id})\n".toUTF8

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
    (values : MemoryValues language.operands.Ty language.operands.Val)
    (state : MemoryRegisters language profile) : Option Observation := do
  let block ← memory.blockAt state
  if state.instruction != block.body.length then none else
  match block.ending with
  | .ret (some ⟨_, operand⟩) => do
    if state.hasCaller || state.frame != memory.entry then none else
    let (value, _) ← readValue values state operand
    let .returned status ← language.hostedResult (.returned value) | none
    return .returned status ByteArray.empty
  | .trap kind origin => do
    let .trapped status kind _ ← language.hostedResult (.trapped kind origin.operation) | none
    return .trapped status kind origin.lexical (trapDiagnostic memory.project kind origin.operation origin.lexical)
  | _ => none

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
  lcb : Model (LCBInput profile) language.operands.Event
  layout : LayoutOf language.operands.Ty
  mirInvariant : MIR lang profile → Prop
  memoryInvariant : Memory lang profile → Prop
  ssaInvariant : SSA lang profile → Prop
  birInvariant : BIR lang profile → Prop
  memoryState : memory.execution.State → MemoryState language.operands.Val profile
  mirFrame : mir.execution.State → LogicalFrame language.operands.Val
  memoryActions : MemoryActions language lang profile
  scalar : language.operands.Ty → Prop
  escapes : MIR lang profile → FnId → PlaceId → Prop
  llvmHandoff : BIR lang profile → LLVMInput profile → Prop
  lcbHandoff : BIR lang profile → LCBInput profile → Prop

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

def Memory.ExecutionExact (memory : Memory lang profile)
    (m : LoweringModels language lang profile) : Prop :=
  (∀ q, m.memory.execution.initial memory q → memory.Initial (m.memoryState q)) ∧
  (∀ q next, m.memory.execution.finite memory q → m.memory.execution.step memory q next →
    memory.Step m.memoryActions (m.memoryState q) (m.memoryState next)) ∧
  (∀ q state, m.memory.execution.finite memory q → memory.Step m.memoryActions (m.memoryState q) state →
    ∃ next, m.memory.execution.step memory q next ∧ m.memoryState next = state) ∧
  ∀ q result, m.memory.execution.finite memory q →
    (m.memory.result memory q result ↔
      memory.terminalObservation m.memoryActions.values (m.memoryState q) = some result)

def LowersMemory (m : LoweringModels language lang profile)
    (mir : MIR lang profile) (memory : Memory lang profile) : Prop :=
  memory.project = mir.project ∧ memory.entry = mir.entry ∧
  PreservesOrigins mir.origins memory.origins ∧ LayoutsMatch m.layout memory ∧
  memory.signatures = mir.signatures ∧ PromotionSound m.scalar m.escapes mir memory ∧
  memory.ExecutionExact m ∧
  ∃ sim : Concrete.Contracts.Simulates m.mir.execution m.memory.execution mir memory,
    m.mir.MatchesResult m.memory sim ∧
    ∀ p q, m.mir.execution.finite mir p → m.memory.execution.finite memory q →
      sim.simulation.stateRel p q →
      memory.Represents m.memoryActions.values (m.mirFrame p) (m.memoryState q)

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

def BuildsBIR (m : LoweringModels language lang profile)
    (ssa : SSA lang profile) (bir : BIR lang profile) : Prop :=
  bir.project = ssa.project ∧ bir.entry = ssa.entry ∧ bir.layouts = ssa.layouts ∧
  bir.signatures = ssa.signatures ∧ bir.TrapSlotsWF ∧ bir.TrapsMatch ssa ∧ PreservesOrigins ssa.origins bir.origins ∧
  bir.frameSlots ≤ profile.maxBIRSlots ∧ m.ssa.Simulates m.bir ssa bir

def EmitsLLVM (m : LoweringModels language lang profile)
    (bir : BIR lang profile) (output : LLVMInput profile) : Prop :=
  profile.backend = .llvm ∧ output.triple = profile.llvmTriple ∧
  output.dataLayout = profile.llvmDataLayout ∧ output.sites = bir.sites ∧ m.llvmHandoff bir output ∧ m.bir.Refines m.llvm bir output

def EmitsLCB (m : LoweringModels language lang profile)
    (bir : BIR lang profile) (output : LCBInput profile) : Prop :=
  profile.backend = .lcb ∧
  LeanCompilerBackend.Verified.validateJob (Backend.LCB.job output.body) = .ok () ∧
  (∀ job ∈ output.additionalJobs, LeanCompilerBackend.Verified.validateJob job = .ok ()) ∧
  ((Backend.LCB.job output.body :: output.additionalJobs).map
    (fun job => job.code.symbol.name)).Nodup ∧
  output.sites = bir.sites ∧ m.lcbHandoff bir output ∧ m.bir.Simulates m.lcb bir output

end Concrete.Interfaces
