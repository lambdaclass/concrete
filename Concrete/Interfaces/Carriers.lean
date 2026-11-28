import ConcreteSpec.StageInterface
import Concrete.Contracts
import Concrete.Backend.LLVMIR
import LeanCompilerBackend.Verified.Job

namespace Concrete.Backend.LCB

/-- The targets the gate runs LCB's output on. -/
inductive Target where
  | x86_64Linux | aarch64Linux | aarch64Macos
  deriving DecidableEq, Repr

def Target.ofName : String → Option Target
  | "x86_64-linux" => some .x86_64Linux
  | "aarch64-linux" => some .aarch64Linux
  | "aarch64-macos" => some .aarch64Macos
  | _ => none

end Concrete.Backend.LCB

namespace Concrete.Interfaces

open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface

inductive BackendKind where
  | llvm | lcb
  deriving DecidableEq, Repr

inductive ISA where
  | x86_64 | aarch64
  deriving DecidableEq, Repr

/-- Compilation route: checked statically discharges arithmetic safety before lowering;
unchecked checks static typing and ownership and retains runtime overflow traps. -/
inductive Route where
  | checked
  | unchecked
  deriving DecidableEq, Repr

/-- Each supported platform has one ISA and one spec memory target. All three hosted
platforms currently share the 64-bit little-endian memory layout. -/
structure Profile where
  backend : BackendKind
  backendTarget : Backend.LCB.Target
  maxBIRSlots : Nat
  maxImageStackBytes : Nat
  deriving DecidableEq, Repr

def Profile.isa (profile : Profile) : ISA :=
  match profile.backendTarget with
  | .x86_64Linux => .x86_64
  | .aarch64Linux | .aarch64Macos => .aarch64

def Profile.memoryTarget (profile : Profile) : Target :=
  match profile.backendTarget with
  | .x86_64Linux | .aarch64Linux | .aarch64Macos => linuxX8664

def Profile.byteOrder (profile : Profile) := profile.memoryTarget.byteOrder

theorem Profile.targetWF (profile : Profile) : profile.memoryTarget.WellFormed := by
  cases profile with
  | mk _ platform _ _ => cases platform <;> exact example_existing_profile_is_well_formed

def BackendKind.policy : BackendKind → BackendPolicy.Backend
  | .llvm => .llvm
  | .lcb => .lcb

def Profile.policyPlatform (profile : Profile) : BackendPolicy.Platform :=
  match profile.backendTarget with
  | .x86_64Linux => .x86_64Linux
  | .aarch64Linux => .aarch64Linux
  | .aarch64Macos => .aarch64Macos

/-- Project a compilation profile to the spec backend-policy profile. -/
def Profile.policyProfile (profile : Profile) : BackendPolicy.Profile :=
  ⟨profile.backend.policy, profile.policyPlatform⟩

/-- Project a compiler `Operator` to its spec backend-policy operation kind. -/
def Operator.policyOperation : Operator → BackendPolicy.Operation
  | .read => .read
  | .arithmetic op => .arithmetic op
  | .named name => .named name

/-- Exact position of a backend-feature occurrence inside a BIR function. -/
inductive FeaturePosition where
  | signatureParam (index : Nat)
  | signatureResult
  | localDecl (place : PlaceId)
  | blockParam (block : BlockId) (index : Nat)
  | instruction (block : BlockId) (index : Nat)
  | terminator (block : BlockId)
  deriving DecidableEq, Repr

def FeaturePosition.block? : FeaturePosition → Option BlockId
  | .blockParam block _ | .instruction block _ | .terminator block => some block
  | .signatureParam _ | .signatureResult | .localDecl _ => none

/-- Closed sum of unsupported backend features diagnosed at the selected emitter boundary.
Every cause carries the offending construct, enclosing function and exact position, and
projects to its single registered rule in `ConcreteSpec.Core.BackendPolicy`. -/
inductive FeatureCause (_profile : Profile) where
  | unsupportedWidth (ty : IntTy) (fn : FnId) (position : FeaturePosition)
  | unsupportedOperation (op : BackendPolicy.Operation) (fn : FnId) (position : FeaturePosition)
  | unsupportedStorage (form : BackendPolicy.StorageForm) (fn : FnId) (position : FeaturePosition)
  | unsupportedCall (form : BackendPolicy.CallForm) (callee : Option FnId)
      (fn : FnId) (position : FeaturePosition)
  deriving DecidableEq, Repr

def FeatureCause.feature {profile : Profile} : FeatureCause profile → BackendPolicy.Feature
  | .unsupportedWidth ty .. => .intWidth ty
  | .unsupportedOperation op .. => .operation op
  | .unsupportedStorage form .. => .storage form
  | .unsupportedCall form .. => .call form

def FeatureCause.fn {profile : Profile} : FeatureCause profile → FnId
  | .unsupportedWidth _ fn _ | .unsupportedOperation _ fn _
  | .unsupportedStorage _ fn _ | .unsupportedCall _ _ fn _ => fn

def FeatureCause.position {profile : Profile} : FeatureCause profile → FeaturePosition
  | .unsupportedWidth _ _ pos | .unsupportedOperation _ _ pos
  | .unsupportedStorage _ _ pos | .unsupportedCall _ _ _ pos => pos

/-- Every backend feature cause projects to its single registered specification rule. -/
def FeatureCause.rule {profile : Profile} (cause : FeatureCause profile) : RegisteredRule :=
  match cause with
  | .unsupportedWidth .. => ⟨BackendPolicy.intWidthRule, by decide⟩
  | .unsupportedOperation .. => ⟨BackendPolicy.operationRule, by decide⟩
  | .unsupportedStorage .. => ⟨BackendPolicy.storageFormRule, by decide⟩
  | .unsupportedCall .. => ⟨BackendPolicy.callRule, by decide⟩

theorem FeatureCause.rule_val {profile : Profile} (cause : FeatureCause profile) :
    cause.rule.val = cause.feature.ruleId := by
  cases cause <;> rfl

inductive LexRefusal where
  | missingFile (entry : ConcreteSpec.Core.FileId)
  | source (cause : RefusalCause)

inductive ElabRefusal where
  | unsupportedSlice (slice : Slice)
  | invalidTree (file : ConcreteSpec.Core.FileId)
  | unexpectedExtension (file : ConcreteSpec.Core.FileId)
  | sliceMismatch (expected actual : Slice)
  | source (cause : RefusalCause)

inductive StageFailure where
  | admission (failure : AdmissionFailure)
  | checkerRefused (cause : RefusalCause)
  | resource (failure : ResourceRefusal)
  | unsupportedProfile (rule : RuleId)
  | unsupportedFeature (profile : Profile) (cause : FeatureCause profile)
  | invalidInput (rule : RuleId)
  | lcbLinkingUnavailable
  | invalidLCBJob (reason : LeanCompilerBackend.Verified.CompileError)

/-- Local value types and field projections use the integrated spec type family. -/
structure Place (Ty : Type) (_ty : Ty) where
  root : PlaceId
  path : List FieldId

inductive Read (Ty : Type) (Literal : Ty → Type) : Ty → Type where
  | literal {ty : Ty} (value : Literal ty) : Read Ty Literal ty
  | copy {ty : Ty} (place : Place Ty ty) : Read Ty Literal ty
  | move {ty : Ty} (place : Place Ty ty) : Read Ty Literal ty

structure CFGEdge (Arg : Type) where
  target : BlockId
  arguments : List Arg

inductive Terminator (Arg Cond : Type) where
  | jump (next : CFGEdge Arg)
  | branch (test : Cond) (yes no : CFGEdge Arg)
  | ret (value : Option Arg)
  | trap (kind : TrapKind) (origin : Origin)
  /-- An unresolved backend control reference has no successor or result. -/
  | invalid

structure Block (Param Inst Term : Type) where
  id : BlockId
  params : List Param
  body : List Inst
  ending : Term
  /-- Origins realized at this control boundary, including eliminated value-only
  operations. This metadata never executes and cannot carry an instruction check. -/
  boundaryOrigins : List Origin

structure CFG (Param Inst Term : Type) where
  entry : BlockId
  blocks : Array (Block Param Inst Term)

def Terminator.successors {Arg Cond : Type} :
    Terminator Arg Cond → List (CFGEdge Arg)
  | .jump edge => [edge]
  | .branch _ yes no => [yes, no]
  | .ret _ | .trap _ _ | .invalid => []

/-- Shared well-formedness for control-flow edges, including typed parallel arguments. -/
def CFG.WF {P I T A Ty : Type} (graph : CFG P I T) (successors : T → List (CFGEdge A))
    (paramType : P → Ty) (argType : A → Ty) : Prop :=
  (graph.blocks.toList.map Block.id).Nodup ∧
  (∃ block ∈ graph.blocks.toList, block.id = graph.entry) ∧
  ∀ block ∈ graph.blocks.toList, ∀ edge ∈ successors block.ending,
    ∃ next ∈ graph.blocks.toList, next.id = edge.target ∧
      edge.arguments.map argType = next.params.map paramType

/-- The operation family is indexed by the entire input signature and the result type. -/
structure Literal (Ty : Type) (_ty : Ty) where
  encoding : List UInt8

structure LoweringLanguage (Ty : Type) where
  operation : Operator → List Ty → Ty → Type
  bool : Ty
  intTy : IntTy → Ty

/-- Operation annotations are erased. Their complete signatures are indices,
and their meaning is given by the selected Core instance. -/
def sliceLoweringLanguage (slice : Slice) : LoweringLanguage (sliceLanguage slice).operands.Ty where
  operation _ _ _ := Unit
  bool := .scalar .bool
  intTy t := .scalar (.int t)

theorem sliceLoweringLanguage_intTy_scalar (slice : Slice) (t : IntTy) :
    (sliceLanguage slice).scalarType ((sliceLoweringLanguage slice).intTy t) = some (.int t) :=
  rfl

theorem sliceLoweringLanguage_bool_scalar (slice : Slice) :
    (sliceLanguage slice).scalarType (sliceLoweringLanguage slice).bool = some .bool :=
  rfl

theorem sliceLoweringLanguage_intTy_injective (slice : Slice) {t₁ t₂ : IntTy}
    (h : (sliceLoweringLanguage slice).intTy t₁ = (sliceLoweringLanguage slice).intTy t₂) :
    t₁ = t₂ := by
  cases h; rfl

theorem sliceLoweringLanguage_intTy_ne_bool (slice : Slice) (t : IntTy) :
    (sliceLoweringLanguage slice).intTy t ≠ (sliceLoweringLanguage slice).bool := by
  intro h; cases h

theorem sliceLoweringLanguage_intTy_isCopy (slice : Slice) (t : IntTy) :
    (sliceLanguage slice).operands.isCopy ((sliceLoweringLanguage slice).intTy t) = true :=
  rfl

theorem sliceLoweringLanguage_bool_isCopy (slice : Slice) :
    (sliceLanguage slice).operands.isCopy (sliceLoweringLanguage slice).bool = true :=
  rfl

abbrev LoweringLanguage.literal {Ty : Type} (_lang : LoweringLanguage Ty) := Literal Ty

inductive CheckDisposition where
  | residual
  | discharged
  deriving DecidableEq

/-- Residual conditions are checked as part of executing the operation, before its
unchecked action, on the same evaluated reads. No CFG edge enters between them. -/
structure OperationCheck where
  discriminator : Nat
  trap : TrapKind
  disposition : CheckDisposition
  deriving DecidableEq

inductive Args {Ty : Type} (Read : Ty → Type) : List Ty → Type where
  | nil : Args Read []
  | cons {ty : Ty} {types : List Ty} (arg : Read ty) (rest : Args Read types) :
      Args Read (ty :: types)

inductive MIRInst {Ty : Type} (lang : LoweringLanguage Ty) where
  | bind {ty : Ty} (dst : Place Ty ty) (src : Read Ty lang.literal ty)
  | assign {ty : Ty} (dst : Place Ty ty) (src : Read Ty lang.literal ty)
  | op {opcode : Operator} {types : List Ty} {ty : Ty} (dst : Place Ty ty)
      (op : lang.operation opcode types ty) (args : Args (Read Ty lang.literal) types)
      (checks : List OperationCheck) (addresses : List (Sigma (Place Ty)) := [])
  | endLifetime (place : PlaceId)
  | destructure {ty : Ty} (src : Place Ty ty) (bindings : List (FieldId × Sigma (Place Ty)))
  | call (callee : FnId) (dst : Option (Sigma (Place Ty)))
      (args : List (Sigma (Read Ty lang.literal))) (checks : List OperationCheck)

structure LocatedOp (Inst : Type) where
  origin : Origin
  instruction : Inst

abbrev LocalTypes (Ty : Type) := AList PlaceId Ty

structure FunctionSignature (Ty : Type) where
  parameters : List Ty
  result : Option Ty

def Place.Valid {Ty : Type} {ty : Ty} (locals : LocalTypes Ty)
    (fieldType : Ty → FieldId → Option Ty) (place : Place Ty ty) : Prop :=
  ∃ rootType, (place.root, rootType) ∈ locals ∧
    place.path.foldlM fieldType rootType = some ty

abbrev MIRGraph {Ty : Type} (lang : LoweringLanguage Ty) :=
  CFG (Sigma (Place Ty)) (LocatedOp (MIRInst lang))
    (Terminator (Sigma (Read Ty lang.literal)) (Read Ty lang.literal lang.bool))

structure MIR {Ty : Type} (lang : LoweringLanguage Ty) (_profile : Profile) where
  project : Project
  entry : FnId
  signatures : AList FnId (FunctionSignature Ty)
  functions : AList FnId (LocalTypes Ty × MIRGraph lang)
  mutableLocals : AList FnId (List PlaceId) := []
  declarations : Declarations := []

/-- Virtual scalar storage, or a logical place with a target-materialized representation.
Materialization can have zero byte extent. -/
inductive StorageClass where
  | virtual
  | materialized
  deriving DecidableEq

structure MemoryPlace (Ty : Type) (ty : Ty) where
  logical : Place Ty ty
  storage : StorageClass

inductive MemoryRead (Ty : Type) (Literal : Ty → Type) : Ty → Type where
  | literal {ty : Ty} (value : Literal ty) : MemoryRead Ty Literal ty
  | copy {ty : Ty} (place : MemoryPlace Ty ty) : MemoryRead Ty Literal ty
  | move {ty : Ty} (place : MemoryPlace Ty ty) : MemoryRead Ty Literal ty

inductive MemoryInst {Ty : Type} (lang : LoweringLanguage Ty) where
  | bind {ty : Ty} (dst : MemoryPlace Ty ty) (src : MemoryRead Ty lang.literal ty)
  | assign {ty : Ty} (dst : MemoryPlace Ty ty) (src : MemoryRead Ty lang.literal ty)
  | op {opcode : Operator} {types : List Ty} {ty : Ty} (dst : MemoryPlace Ty ty)
      (op : lang.operation opcode types ty) (args : Args (MemoryRead Ty lang.literal) types)
      (checks : List OperationCheck := []) (addresses : List (Sigma (MemoryPlace Ty)) := [])
  | addWithOverflow (t : IntTy) (dst : MemoryPlace Ty (lang.intTy t))
      (flag : MemoryPlace Ty lang.bool)
      (lhs rhs : MemoryRead Ty lang.literal (lang.intTy t))
  | beginLifetime (place : PlaceId)
  | endLifetime (place : PlaceId)
  | destructure {ty : Ty} (src : MemoryPlace Ty ty)
      (bindings : List (FieldId × Sigma (MemoryPlace Ty)))
  | call (callee : FnId) (dst : Option (Sigma (MemoryPlace Ty)))
      (args : List (Sigma (MemoryRead Ty lang.literal))) (checks : List OperationCheck := [])

abbrev MemoryTerm {Ty : Type} (lang : LoweringLanguage Ty) :=
  Terminator (Sigma (MemoryRead Ty lang.literal)) (MemoryRead Ty lang.literal lang.bool)

abbrev MemoryBlock {Ty : Type} (lang : LoweringLanguage Ty) :=
  Block (Sigma (MemoryPlace Ty)) (LocatedOp (MemoryInst lang)) (MemoryTerm lang)

abbrev MemoryGraph {Ty : Type} (lang : LoweringLanguage Ty) :=
  CFG (Sigma (MemoryPlace Ty)) (LocatedOp (MemoryInst lang)) (MemoryTerm lang)

structure Memory {Ty : Type} (lang : LoweringLanguage Ty) (profile : Profile) where
  project : Project
  entry : FnId
  signatures : AList FnId (FunctionSignature Ty)
  functions : AList FnId (LocalTypes Ty × MemoryGraph lang)
  layouts : AList FnId (AList PlaceId Layout)
  storage : AList FnId (AList PlaceId StorageClass)
  mutableLocals : AList FnId (List PlaceId) := []
  declarations : Declarations := []

/-- An activation owns its locals. Suspended activations share byte memory
with the running activation, but retain their own ranges and logical lifetimes. -/
structure MemoryFrame (Value : Type) where
  frame : FnId
  block : BlockId
  instruction : Nat
  virtualValues : AList PlaceId Value
  materialized : AList PlaceId Hybrid.Range
  live : List PlaceId
  initialized : List PlaceId
  zeroValues : AList PlaceId Value := []
  addressIds : AList PlaceId Hybrid.Place := []
  /-- Moved subtrees of initialized roots. A path is relative to its root. -/
  movedFields : List (PlaceId × List FieldId) := []

structure MemoryContinuation (Ty Value : Type) where
  caller : MemoryFrame Value
  destination : Option (Sigma (MemoryPlace Ty))

structure MemoryState (Ty Value Event : Type) (profile : Profile) extends MemoryFrame Value where
  bytes : Mem.Memory
  callers : List (MemoryContinuation Ty Value) := []
  effects : List Event := []
  ownership : Own.State := {}
  nextAddressId : Nat := 0
  resourceExhausted : Bool := false

def MemoryState.BytesWF {Ty Value Event : Type} {profile : Profile}
    (state : MemoryState Ty Value Event profile) :
    Prop :=
  state.bytes.WellFormed profile.memoryTarget ∧
    state.live.Nodup ∧ state.initialized.Nodup ∧
    ∀ place ∈ state.initialized, place ∈ state.live

structure SSAValue (Ty : Type) (_ty : Ty) where
  id : Nat

inductive SSARead (Ty : Type) (Literal : Ty → Type) : Ty → Type where
  | value {ty : Ty} (value : SSAValue Ty ty) : SSARead Ty Literal ty
  | literal {ty : Ty} (value : Literal ty) : SSARead Ty Literal ty

/-- A destructure reads an entire aggregate, whether promoted or byte-backed. -/
inductive SSAAggregate (Ty : Type) : Ty → Type where
  | register {ty : Ty} (src : SSAValue Ty ty) : SSAAggregate Ty ty
  | materialized {ty : Ty} (src : Place Ty ty) : SSAAggregate Ty ty

/-- SSA only renames virtual values. Materialized locations retain their addressable identity. -/
inductive SSAInst {Ty : Type} (lang : LoweringLanguage Ty) where
  | op {opcode : Operator} {types : List Ty} {ty : Ty} (dst : SSAValue Ty ty) (op : lang.operation opcode types ty)
      (args : Args (SSARead Ty lang.literal) types)
      (addresses : List (Sigma (Place Ty)) := [])
  | addWithOverflow (t : IntTy) (dst : SSAValue Ty (lang.intTy t))
      (flag : SSAValue Ty lang.bool)
      (lhs rhs : SSARead Ty lang.literal (lang.intTy t))
  | load {ty : Ty} (dst : SSAValue Ty ty) (src : Place Ty ty)
  | destructure {ty : Ty} (src : SSAAggregate Ty ty)
      (bindings : List (FieldId × Sigma (SSAValue Ty)))
  | store {ty : Ty} (dst : Place Ty ty) (src : SSARead Ty lang.literal ty)
  | beginLifetime (place : PlaceId)
  | endLifetime (place : PlaceId)
  | call (callee : FnId) (dst : Option (Sigma (SSAValue Ty)))
      (args : List (Sigma (SSARead Ty lang.literal)))

abbrev SSAGraph {Ty : Type} (lang : LoweringLanguage Ty) :=
  CFG (Sigma (SSAValue Ty)) (LocatedOp (SSAInst lang))
    (Terminator (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool))

structure SSA {Ty : Type} (lang : LoweringLanguage Ty) (_profile : Profile) where
  project : Project
  entry : FnId
  signatures : AList FnId (FunctionSignature Ty)
  functions : AList FnId (LocalTypes Ty × SSAGraph lang)
  layouts : AList FnId (AList PlaceId Layout)
  mutableLocals : AList FnId (List PlaceId) := []
  declarations : Declarations := []

structure TrapIndex where
  index : Nat
  deriving DecidableEq, Repr

structure TrapEntry where
  kind : TrapKind
  origin : Origin

abbrev TrapTable := Array TrapEntry

/-- Resolve an operation origin in its project to the single runtime diagnostic site
shared by Core, Memory/SSA/BIR, LLVM launcher, and LCB runtime observations. -/
def runtimeSite (project : Project) (origin : Origin) : Option Concrete.Contracts.DiagnosticSite := do
  let file ← project.fileAt origin.lexical.file
  return ⟨origin.operation, ⟨file.path.render.text, origin.lexical.line, origin.lexical.col⟩⟩

/-- Render the canonical newline-terminated runtime trap diagnostic line from a
resolved diagnostic site. -/
def renderTrapLine (kind : TrapKind) (site : Concrete.Contracts.DiagnosticSite) : String :=
  s!"concrete: trap: {kind.name} at {site.location.path}:{site.location.line}:{site.location.col} (site {site.id.id})\n"

/-- Render the canonical trap diagnostic line for a `TrapEntry` in its issuing project. -/
def TrapEntry.diagnosticLine (entry : TrapEntry) (project : Project) : Option String :=
  (runtimeSite project entry.origin).map (renderTrapLine entry.kind)

theorem runtimeSite_of_binds {project : Project} {origin : Origin} {span : Span}
    (binds : origin.lexical.Binds project span) :
    ∃ file, project.fileAt origin.lexical.file = some file ∧
      runtimeSite project origin =
        some ⟨origin.operation, ⟨file.path.render.text, origin.lexical.line, origin.lexical.col⟩⟩ := by
  obtain ⟨file, hfile, _⟩ := binds
  have hAt : project.fileAt origin.lexical.file = some file := by
    rw [Project.fileAt, ← Array.getElem?_toList]
    exact hfile
  refine ⟨file, hAt, ?_⟩
  simp [runtimeSite, hAt]

inductive BIRTerm (Arg Cond : Type) where
  | jump (next : CFGEdge Arg)
  | branch (test : Cond) (yes no : CFGEdge Arg)
  | ret (value : Option Arg)
  | trap (entry : TrapIndex)

abbrev BIRGraph {Ty : Type} (lang : LoweringLanguage Ty) :=
  CFG (Sigma (SSAValue Ty)) (LocatedOp (SSAInst lang))
    (BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool))

structure BIR {Ty : Type} (lang : LoweringLanguage Ty) (_profile : Profile) where
  project : Project
  entry : FnId
  signatures : AList FnId (FunctionSignature Ty)
  functions : AList FnId (LocalTypes Ty × BIRGraph lang)
  layouts : AList FnId (AList PlaceId Layout)
  sites : TrapTable
  mutableLocals : AList FnId (List PlaceId) := []
  declarations : Declarations := []

/-- Number of register slots defined by a single SSA/BIR instruction.
Two-result `addWithOverflow` occupies two slots (wrapped result and overflow flag). -/
def SSAInst.slotCount {Ty : Type} {lang : LoweringLanguage Ty} : SSAInst lang → Nat
  | .op .. | .load .. => 1
  | .addWithOverflow .. => 2
  | .destructure _ bindings => bindings.length
  | .call _ (some _) _ => 1
  | .store .. | .beginLifetime .. | .endLifetime .. | .call _ none _ => 0

theorem SSAInst.slotCount_addWithOverflow {Ty : Type} {lang : LoweringLanguage Ty}
    (t : IntTy) (dst : SSAValue Ty (lang.intTy t)) (flag : SSAValue Ty lang.bool)
    (lhs rhs : SSARead Ty lang.literal (lang.intTy t)) :
    (SSAInst.addWithOverflow t dst flag lhs rhs).slotCount = 2 :=
  rfl

/-- Each register definition and block parameter occupies one slot; two-result
`addWithOverflow` occupies two slots. Use the largest function frame, including
for the pre-BIR resource refusal. -/
private def registerFrameSlots {Ty T : Type} {lang : LoweringLanguage Ty}
    (functions : AList FnId (LocalTypes Ty × CFG (Sigma (SSAValue Ty)) (LocatedOp (SSAInst lang)) T)) :
    Nat :=
  functions.foldl (fun largest (_, _, graph) =>
    max largest (graph.blocks.foldl (fun slots block =>
      block.body.foldl (fun count inst => count + inst.instruction.slotCount)
        (slots + block.params.length)) 0)) 0

def SSA.frameSlots {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (ssa : SSA lang profile) : Nat :=
  registerFrameSlots ssa.functions

def BIR.frameSlots {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) : Nat :=
  registerFrameSlots bir.functions

/-- The four intermediate representation points where intra-IR passes may be inserted. -/
inductive IRPoint where
  | mir
  | memory
  | ssa
  | bir
  deriving DecidableEq, Repr

/-- Generic IR carrier at an IR insertion point for any lowering language and profile. -/
def AtLang {Ty : Type} (lang : LoweringLanguage Ty) (profile : Profile) : IRPoint → Type
  | .mir => MIR lang profile
  | .memory => Memory lang profile
  | .ssa => SSA lang profile
  | .bir => BIR lang profile

/-- The frozen IR carrier at the repository-selected slice, profile and IR insertion point. -/
def At (slice : Slice) (profile : Profile) : IRPoint → Type :=
  AtLang (sliceLoweringLanguage slice) profile

theorem At.mir_eq (slice : Slice) (profile : Profile) :
    At slice profile .mir = MIR (sliceLoweringLanguage slice) profile := rfl

theorem At.memory_eq (slice : Slice) (profile : Profile) :
    At slice profile .memory = Memory (sliceLoweringLanguage slice) profile := rfl

theorem At.ssa_eq (slice : Slice) (profile : Profile) :
    At slice profile .ssa = SSA (sliceLoweringLanguage slice) profile := rfl

theorem At.bir_eq (slice : Slice) (profile : Profile) :
    At slice profile .bir = BIR (sliceLoweringLanguage slice) profile := rfl

/-- Exact syntactic slot of an origin-bearing occurrence inside a CFG block. -/
inductive OccurrenceSlot where
  | instruction (index : Nat)
  | boundary (index : Nat)
  | trap
  deriving DecidableEq, Repr

/-- Unique syntactic address of one origin-bearing occurrence in a CFG IR
(`MIR`, `Memory`, `SSA`, or `BIR`). Two distinct occurrences can share the same
`Origin`; accounting tracks them individually by this address. -/
structure Occurrence where
  fn : FnId
  block : BlockId
  slot : OccurrenceSlot
  deriving DecidableEq, Repr

/-- Enumerate every origin-bearing occurrence in a single CFG block in deterministic stored order:
its body instructions, its boundary origins, and (when present) its trap terminator. -/
def Block.occurrences {P I T : Type} (fn : FnId)
    (block : Block P (LocatedOp I) T) (trapOrigin : T → Option Origin) :
    List (Occurrence × Origin) :=
  (block.body.zipIdx.map fun (op, idx) => (⟨fn, block.id, .instruction idx⟩, op.origin)) ++
  (block.boundaryOrigins.zipIdx.map fun (origin, idx) => (⟨fn, block.id, .boundary idx⟩, origin)) ++
  match trapOrigin block.ending with
  | some origin => [(⟨fn, block.id, .trap⟩, origin)]
  | none => []

/-- Enumerate every origin-bearing occurrence in a CFG in deterministic stored order:
for each block, its body instructions, its boundary origins, and (when present) its
trap terminator. -/
def CFG.occurrences {P I T : Type} (fn : FnId)
    (graph : CFG P (LocatedOp I) T) (trapOrigin : T → Option Origin) :
    List (Occurrence × Origin) :=
  graph.blocks.toList.flatMap fun block => block.occurrences fn trapOrigin

def Terminator.trapOrigin {A C : Type} : Terminator A C → Option Origin
  | .trap _ origin => some origin
  | _ => none

def MIR.occurrences {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (mir : MIR lang profile) : List (Occurrence × Origin) :=
  mir.functions.flatMap fun (fn, _, graph) => graph.occurrences fn Terminator.trapOrigin

def Memory.occurrences {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (memory : Memory lang profile) : List (Occurrence × Origin) :=
  memory.functions.flatMap fun (fn, _, graph) => graph.occurrences fn Terminator.trapOrigin

def SSA.occurrences {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (ssa : SSA lang profile) : List (Occurrence × Origin) :=
  ssa.functions.flatMap fun (fn, _, graph) => graph.occurrences fn Terminator.trapOrigin

def BIR.occurrences {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) : List (Occurrence × Origin) :=
  bir.functions.flatMap fun (fn, _, graph) =>
    graph.occurrences fn fun
      | .trap slot => (bir.sites[slot.index]?).map TrapEntry.origin
      | _ => none

def AtLang.occurrences {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile} :
    {point : IRPoint} → AtLang lang profile point → List (Occurrence × Origin)
  | .mir, mir => MIR.occurrences mir
  | .memory, memory => Memory.occurrences memory
  | .ssa, ssa => SSA.occurrences ssa
  | .bir, bir => BIR.occurrences bir

def At.occurrences {slice : Slice} {profile : Profile} {point : IRPoint}
    (ir : At slice profile point) : List (Occurrence × Origin) :=
  AtLang.occurrences ir

private theorem map_snd_zipIdx_map {α β : Type} (f : α → β) (g : α × Nat → Occurrence)
    (xs : List α) (k : Nat) :
    ((xs.zipIdx k).map fun p => (g p, f p.1)).map Prod.snd = xs.map f := by
  induction xs generalizing k with
  | nil => rfl
  | cons a rest ih =>
    simp only [List.zipIdx_cons, List.map_cons, ih (k + 1)]

theorem CFG.occurrences_map_snd {P I T : Type} (fn : FnId)
    (graph : CFG P (LocatedOp I) T) (trapOrigin : T → Option Origin) :
    (graph.occurrences fn trapOrigin).map Prod.snd =
      graph.blocks.toList.flatMap fun block =>
        block.body.map LocatedOp.origin ++ block.boundaryOrigins ++ (trapOrigin block.ending).toList := by
  unfold CFG.occurrences Block.occurrences
  rw [List.map_flatMap]
  congr 1
  funext block
  rw [List.map_append, List.map_append]
  have hbody :
      (List.map (fun x => (Occurrence.mk fn block.id (.instruction x.2), x.1.origin))
          (List.zipIdx block.body)).map Prod.snd =
        block.body.map LocatedOp.origin :=
    map_snd_zipIdx_map LocatedOp.origin (fun p => ⟨fn, block.id, .instruction p.2⟩) block.body 0
  have hbound :
      (List.map (fun x => (Occurrence.mk fn block.id (.boundary x.2), x.1))
          (List.zipIdx block.boundaryOrigins)).map Prod.snd =
        block.boundaryOrigins := by
    simpa [List.map_id] using
      map_snd_zipIdx_map id (fun p => ⟨fn, block.id, .boundary p.2⟩) block.boundaryOrigins 0
  rw [hbody, hbound]
  cases trapOrigin block.ending <;> rfl

private theorem nodup_zipIdx_slot {α : Type} (fn : FnId) (block : BlockId)
    (mkSlot : Nat → OccurrenceSlot) (inj : ∀ m n, mkSlot m = mkSlot n → m = n)
    (xs : List α) (k : Nat) :
    ((xs.zipIdx k).map fun p => Occurrence.mk fn block (mkSlot p.2)).Nodup := by
  induction xs generalizing k with
  | nil => exact List.nodup_nil
  | cons _ rest ih =>
    rw [List.zipIdx_cons, List.map_cons, List.nodup_cons]
    refine ⟨?_, ih (k + 1)⟩
    intro hmem
    obtain ⟨p, hp, heq⟩ := List.mem_map.mp hmem
    have hidx : p.2 = k := inj p.2 k (Occurrence.mk.inj heq).2.2
    have hge : k + 1 ≤ p.2 := (List.mem_zipIdx hp).1
    omega

private theorem nodup_append_of_disjoint {α : Type} {xs ys : List α}
    (hx : xs.Nodup) (hy : ys.Nodup) (disj : ∀ a ∈ xs, a ∉ ys) : (xs ++ ys).Nodup := by
  induction xs with
  | nil => exact hy
  | cons a rest ih =>
    rw [List.nodup_cons] at hx
    rw [List.cons_append, List.nodup_cons]
    refine ⟨?_, ih hx.2 (fun b hb => disj b (List.mem_cons_of_mem a hb))⟩
    intro hmem
    rcases List.mem_append.mp hmem with h1 | h2
    · exact hx.1 h1
    · exact disj a List.mem_cons_self h2

private theorem nodup_block_occurrences {P I T : Type} (fn : FnId)
    (block : Block P (LocatedOp I) T) (trapOrigin : T → Option Origin) :
    ((block.occurrences fn trapOrigin).map Prod.fst).Nodup := by
  unfold Block.occurrences
  rw [List.map_append, List.map_append, List.map_map, List.map_map]
  simp only [Function.comp_def]
  have hbody : (block.body.zipIdx.map fun p => Occurrence.mk fn block.id (.instruction p.2)).Nodup :=
    nodup_zipIdx_slot fn block.id OccurrenceSlot.instruction (fun _ _ h => OccurrenceSlot.instruction.inj h)
      block.body 0
  have hbound : (block.boundaryOrigins.zipIdx.map fun p => Occurrence.mk fn block.id (.boundary p.2)).Nodup :=
    nodup_zipIdx_slot fn block.id OccurrenceSlot.boundary (fun _ _ h => OccurrenceSlot.boundary.inj h)
      block.boundaryOrigins 0
  have hbb :
      ((block.body.zipIdx.map fun p => Occurrence.mk fn block.id (.instruction p.2)) ++
        (block.boundaryOrigins.zipIdx.map fun p => Occurrence.mk fn block.id (.boundary p.2))).Nodup := by
    refine nodup_append_of_disjoint hbody hbound ?_
    intro occ h1 h2
    obtain ⟨_, _, rfl⟩ := List.mem_map.mp h1
    obtain ⟨_, _, heq⟩ := List.mem_map.mp h2
    cases heq
  cases trapOrigin block.ending with
  | none =>
    simpa [List.append_nil] using hbb
  | some origin =>
    refine nodup_append_of_disjoint hbb (List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩) ?_
    intro occ h1 h2
    have hEq := List.mem_singleton.mp h2
    subst hEq
    rcases List.mem_append.mp h1 with hI | hB
    · obtain ⟨_, _, heq⟩ := List.mem_map.mp hI
      cases heq
    · obtain ⟨_, _, heq⟩ := List.mem_map.mp hB
      cases heq

private theorem block_occurrences_block_eq {P I T : Type} (fn : FnId)
    (block : Block P (LocatedOp I) T) (trapOrigin : T → Option Origin) {occ : Occurrence}
    (hmem : occ ∈ (block.occurrences fn trapOrigin).map Prod.fst) :
    occ.block = block.id := by
  unfold Block.occurrences at hmem
  rw [List.map_append, List.map_append, List.map_map, List.map_map] at hmem
  rcases List.mem_append.mp hmem with h1 | h2
  · rcases List.mem_append.mp h1 with hI | hB
    · obtain ⟨_, _, rfl⟩ := List.mem_map.mp hI; rfl
    · obtain ⟨_, _, rfl⟩ := List.mem_map.mp hB; rfl
  · cases h : trapOrigin block.ending with
    | none => simp [h] at h2
    | some origin =>
      simp only [h, List.map_cons, List.map_nil, List.mem_singleton] at h2
      subst h2; rfl

/-- Distinct block IDs yield duplicate-free occurrence addresses across the entire CFG. -/
theorem CFG.occurrences_keys_nodup {P I T : Type} (fn : FnId)
    (graph : CFG P (LocatedOp I) T) (trapOrigin : T → Option Origin)
    (blocksUnique : (graph.blocks.toList.map Block.id).Nodup) :
    ((graph.occurrences fn trapOrigin).map Prod.fst).Nodup := by
  unfold CFG.occurrences
  rw [List.map_flatMap]
  generalize graph.blocks.toList = blocks at blocksUnique ⊢
  induction blocks with
  | nil => exact List.nodup_nil
  | cons block rest ih =>
    rw [List.map_cons, List.nodup_cons] at blocksUnique
    rw [List.flatMap_cons]
    refine nodup_append_of_disjoint (nodup_block_occurrences fn block trapOrigin) (ih blocksUnique.2) ?_
    intro occ hhead htail
    have hb1 : occ.block = block.id := block_occurrences_block_eq fn block trapOrigin hhead
    obtain ⟨other, hother, hoccOther⟩ := List.mem_flatMap.mp htail
    have hb2 : occ.block = other.id := block_occurrences_block_eq fn other trapOrigin hoccOther
    exact blocksUnique.1 (List.mem_map.mpr ⟨other, hother, (hb1.symm.trans hb2).symm⟩)

/-- Explicit justification for omitting a source occurrence in an intra-IR pass. -/
inductive EliminationReason where
  | unreachableFunction
  | unreachableBlock
  | deadValue
  | mergedInto (target : Occurrence)
  deriving DecidableEq, Repr

/-- Purely executable occurrence-accounting witness for an intra-IR pass `a → b`.
`mapped` records the source occurrence in `a` behind every output occurrence in `b`.
`eliminated` justifies every source occurrence in `a` not reached by `mapped`. -/
structure Account where
  mapped : AList Occurrence Occurrence
  eliminated : AList Occurrence EliminationReason
  deriving DecidableEq, Repr

/-- Identity accounting witness mapping every occurrence to itself with no eliminations. -/
def Account.identity (occurrences : List Occurrence) : Account where
  mapped := occurrences.map fun occ => (occ, occ)
  eliminated := []

/-- Identity accounting witness for an IR program at any insertion point. -/
def Account.identityFor {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} (ir : AtLang lang profile point) : Account :=
  Account.identity ((AtLang.occurrences ir).map Prod.fst)

private def Account.resolveMerged (second : Account) (target : Occurrence) :
    Option EliminationReason :=
  match second.mapped.find? (fun pair => pair.2 == target) with
  | some (outOcc, _) => some (.mergedInto outOcc)
  | none => AList.lookup second.eliminated target

/-- Compose two sequential occurrence-accounting witnesses `first` (`a → b`) and
`second` (`b → c`) into a finite executable witness for `a → c`. -/
def Account.compose (first second : Account) : Account :=
  let mapped := second.mapped.filterMap fun (occC, occB) =>
    (AList.lookup first.mapped occB).map fun occA => (occC, occA)
  let surviving (occA : Occurrence) : Bool :=
    mapped.any fun pair => pair.2 == occA
  let fromFirst := first.eliminated.filterMap fun (occA, reason) =>
    if surviving occA then none
    else match reason with
      | .mergedInto targetB => (Account.resolveMerged second targetB).map (occA, ·)
      | other => some (occA, other)
  let initialSeen := fromFirst.map Prod.fst
  let (_, fromSecondRev) := first.mapped.foldl
    (fun (acc : List Occurrence × List (Occurrence × EliminationReason)) (pair : Occurrence × Occurrence) =>
      let (seen, out) := acc
      let (occB, occA) := pair
      if surviving occA || seen.contains occA then (seen, out)
      else match AList.lookup second.eliminated occB with
        | some reason => (occA :: seen, (occA, reason) :: out)
        | none => (seen, out))
    (initialSeen, [])
  { mapped := mapped, eliminated := fromFirst ++ fromSecondRev.reverse }

theorem Account.lookup_identity_of_mem {occs : List Occurrence} (nodup : occs.Nodup)
    {occ : Occurrence} (h : occ ∈ occs) :
    AList.lookup (Account.identity occs).mapped occ = some occ := by
  induction occs with
  | nil => cases h
  | cons a rest ih =>
    rw [List.nodup_cons] at nodup
    rcases List.mem_cons.mp h with rfl | hrest
    · simp [Account.identity, AList.lookup]
    · have hne : a ≠ occ := fun heq => nodup.1 (heq ▸ hrest)
      simp [Account.identity, AList.lookup, hne]
      exact ih nodup.2 hrest

theorem Account.mem_compose_mapped_iff (first second : Account) (occC occA : Occurrence) :
    (occC, occA) ∈ (Account.compose first second).mapped ↔
      ∃ occB, (occC, occB) ∈ second.mapped ∧ AList.lookup first.mapped occB = some occA := by
  unfold Account.compose
  dsimp only
  rw [List.mem_filterMap]
  constructor
  · rintro ⟨⟨c, b⟩, hmem, hmap⟩
    cases hlook : AList.lookup first.mapped b with
    | none => simp [hlook] at hmap
    | some a =>
      simp only [hlook, Option.map_some, Option.some.injEq, Prod.mk.injEq] at hmap
      obtain ⟨rfl, rfl⟩ := hmap
      exact ⟨b, hmem, hlook⟩
  · rintro ⟨occB, hmem, hlook⟩
    exact ⟨(occC, occB), hmem, by simp [hlook]⟩

def Profile.llvmTriple (profile : Profile) : String :=
  match profile.backendTarget with
  | .x86_64Linux => "x86_64-unknown-linux-gnu"
  | .aarch64Linux => "aarch64-unknown-linux-gnu"
  | .aarch64Macos => "arm64-apple-macosx15.0.0"

def Profile.llvmDataLayout (profile : Profile) : String :=
  match profile.backendTarget with
  | .x86_64Linux => "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
  | .aarch64Linux => "e-m:e-p270:32:32-p271:32:32-p272:64:64-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32"
  | .aarch64Macos => "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"

/-- The two linked LLVM IR modules emitted for a hosted Concrete program:
the compiled component (`@concrete_body`) and the fixed runtime launcher (`@main`
plus trap diagnostics). -/
inductive LLVMRole where
  | component
  | launcher
  deriving DecidableEq, Repr

/-- One structured LLVM IR module carrying its checked target header and body items.
The structured body has no target directive or raw-text escape hatch. -/
structure LLVMModule where
  triple : String
  dataLayout : String
  items : List Backend.LLVM.Item
  deriving BEq

/-- Render a single LLVM module's checked header and structured items to UTF-8 bytes. -/
def LLVMModule.bytes (mod : LLVMModule) : ByteArray :=
  (s!"target datalayout = \"{mod.dataLayout}\"\ntarget triple = \"{mod.triple}\"\n" ++
    Backend.LLVM.render mod.items).toUTF8

/-- Two-module LLVM handoff: structured component and launcher modules plus the
exact trap table shared between them. -/
structure LLVMInput (_profile : Profile) where
  component : LLVMModule
  launcher : LLVMModule
  sites : TrapTable

/-- Build a two-module `LLVMInput` stamped with `profile`'s target triple and data layout
on both roles. -/
def LLVMInput.ofProfile (profile : Profile)
    (componentItems launcherItems : List Backend.LLVM.Item)
    (sites : TrapTable) : LLVMInput profile where
  component := ⟨profile.llvmTriple, profile.llvmDataLayout, componentItems⟩
  launcher := ⟨profile.llvmTriple, profile.llvmDataLayout, launcherItems⟩
  sites := sites

/-- Select the structured LLVM module for `role`. -/
def LLVMInput.moduleAt {profile : Profile} (input : LLVMInput profile) : LLVMRole → LLVMModule
  | .component => input.component
  | .launcher => input.launcher

/-- Render the exact bytes passed to the LLVM toolchain for the selected role.
The structured body has no target directive or raw-text escape hatch. -/
def LLVMInput.bytesAt {profile : Profile} (input : LLVMInput profile) (role : LLVMRole) : ByteArray :=
  (input.moduleAt role).bytes

/-- The exact pair of rendered LLVM IR byte arrays handed to the external toolchain
and bound by `P_print` / `llvmBehaviors`. -/
structure LLVMPackageBytes where
  component : ByteArray
  launcher : ByteArray

def LLVMPackageBytes.bytesAt (pkg : LLVMPackageBytes) : LLVMRole → ByteArray
  | .component => pkg.component
  | .launcher => pkg.launcher

/-- Package both rendered role byte arrays from the checked `LLVMInput` fields. -/
def LLVMInput.packageBytes {profile : Profile} (input : LLVMInput profile) : LLVMPackageBytes where
  component := input.bytesAt .component
  launcher := input.bytesAt .launcher

theorem LLVMInput.ofProfile_moduleAt (profile : Profile)
    (componentItems launcherItems : List Backend.LLVM.Item)
    (sites : TrapTable) (role : LLVMRole) :
    (LLVMInput.ofProfile profile componentItems launcherItems sites).moduleAt role =
      ⟨profile.llvmTriple, profile.llvmDataLayout,
        match role with | .component => componentItems | .launcher => launcherItems⟩ := by
  cases role <;> rfl

theorem LLVMPackageBytes.bytesAt_mk (component launcher : ByteArray) (role : LLVMRole) :
    (LLVMPackageBytes.mk component launcher).bytesAt role =
      match role with | .component => component | .launcher => launcher := by
  cases role <;> rfl

structure LCBInput (_profile : Profile) where
  body : LeanCompilerBackend.Verified.Source.Function
  additionalJobs : AList FnId LeanCompilerBackend.Verified.FunctionJob
  sites : TrapTable
  runtimeInput : ByteArray

/-- Backend handoff selected by the profile's backend kind. Reduces definitionally
to `LLVMInput profile` for `.llvm` and `LCBInput profile` for `.lcb`. -/
def BackendOutput (profile : Profile) : Type :=
  match profile.backend with
  | .llvm => LLVMInput profile
  | .lcb => LCBInput profile

theorem BackendOutput.llvm_eq (target : Backend.LCB.Target) (maxSlots maxStack : Nat) :
    BackendOutput ⟨.llvm, target, maxSlots, maxStack⟩ =
      LLVMInput ⟨.llvm, target, maxSlots, maxStack⟩ := rfl

theorem BackendOutput.lcb_eq (target : Backend.LCB.Target) (maxSlots maxStack : Nat) :
    BackendOutput ⟨.lcb, target, maxSlots, maxStack⟩ =
      LCBInput ⟨.lcb, target, maxSlots, maxStack⟩ := rfl

theorem BackendOutput.eq_llvm {profile : Profile} (h : profile.backend = .llvm) :
    BackendOutput profile = LLVMInput profile := by
  unfold BackendOutput; rw [h]

theorem BackendOutput.eq_lcb {profile : Profile} (h : profile.backend = .lcb) :
    BackendOutput profile = LCBInput profile := by
  unfold BackendOutput; rw [h]

end Concrete.Interfaces
