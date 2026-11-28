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
projects to its single registered rule in `ConcreteSpec.Core.BackendPolicy`.
The `profile` index is tied to the backend policy (`BackendPolicy.FeatureSupported`)
by F6b's `firstUnsupported` contract rather than by an internal proof field here. -/
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

/-- Failure channel shared by post-elaboration compiler stages. Checker refusals on
both checked and unchecked routes use `.admission (.refused cause)` so there is one
refusal representation across both routes. -/
inductive StageFailure where
  | admission (failure : AdmissionFailure)
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

private theorem nodup_filter {α : Type} {l : List α} (p : α → Bool) (h : l.Nodup) :
    (l.filter p).Nodup := by
  induction l with
  | nil => exact List.nodup_nil
  | cons a rest ih =>
    rw [List.nodup_cons] at h
    rw [List.filter_cons]
    cases hp : p a
    · simp only [Bool.false_eq_true, ↓reduceIte]
      exact ih h.2
    · simp only [↓reduceIte, List.nodup_cons]
      refine ⟨fun hmem => h.1 (List.mem_filter.mp hmem).1, ih h.2⟩

private theorem nodup_eraseDups_aux {α : Type} [BEq α] [LawfulBEq α] :
    ∀ (n : Nat) (l : List α), l.length ≤ n → l.eraseDups.Nodup
  | 0, [], _ => by simp
  | 0, _ :: _, hlen => by cases hlen
  | n + 1, [], _ => by simp
  | n + 1, a :: rest, hlen => by
    rw [List.eraseDups_cons, List.nodup_cons]
    refine ⟨?_, nodup_eraseDups_aux n (rest.filter fun b => !b == a) ?_⟩
    · intro hmem
      have hf := (List.mem_eraseDups.mp hmem)
      have hne := (List.mem_filter.mp hf).2
      simp at hne
    · have hle : (rest.filter fun b => !b == a).length ≤ rest.length := List.length_filter_le _ _
      exact Nat.le_trans hle (Nat.le_of_succ_le_succ hlen)

private theorem nodup_eraseDups {α : Type} [BEq α] [LawfulBEq α] (l : List α) :
    l.eraseDups.Nodup :=
  nodup_eraseDups_aux l.length l (Nat.le_refl _)

private theorem eraseDups_eq_self {α : Type} [BEq α] [LawfulBEq α] {l : List α} (h : l.Nodup) :
    l.eraseDups = l := by
  induction l with
  | nil => rfl
  | cons a rest ih =>
    rw [List.nodup_cons] at h
    rw [List.eraseDups_cons]
    have hfilt : rest.filter (fun b => !b == a) = rest := by
      rw [List.filter_eq_self]
      intro b hb
      have hne : b ≠ a := fun heq => h.1 (heq ▸ hb)
      simp [hne]
    rw [hfilt, ih h.2]

private theorem filter_eraseDups_aux {α : Type} [BEq α] [LawfulBEq α] :
    ∀ (n : Nat) (l : List α) (p : α → Bool), l.length ≤ n →
      l.eraseDups.filter p = (l.filter p).eraseDups
  | 0, [], _, _ => by simp
  | 0, _ :: _, _, hlen => by cases hlen
  | n + 1, [], _, _ => by simp
  | n + 1, a :: rest, p, hlen => by
    have hrest : (rest.filter fun b => !b == a).length ≤ n :=
      Nat.le_trans (List.length_filter_le _ _) (Nat.le_of_succ_le_succ hlen)
    rw [List.eraseDups_cons, List.filter_cons, List.filter_cons]
    cases hp : p a
    · simp only [Bool.false_eq_true, ↓reduceIte]
      rw [filter_eraseDups_aux n (rest.filter fun b => !b == a) p hrest]
      have hEq : (rest.filter fun b => !b == a).filter p = rest.filter p := by
        rw [List.filter_filter]
        refine List.filter_congr fun b _ => ?_
        cases hpb : p b
        · simp
        · have hne : b ≠ a := fun heq => by rw [heq, hp] at hpb; cases hpb
          simp [hne]
      rw [hEq]
    · simp only [↓reduceIte, List.eraseDups_cons]
      rw [filter_eraseDups_aux n (rest.filter fun b => !b == a) p hrest]
      congr 2
      rw [List.filter_filter, List.filter_filter]
      refine List.filter_congr fun b _ => Bool.and_comm _ _

private theorem filter_eraseDups {α : Type} [BEq α] [LawfulBEq α] (l : List α) (p : α → Bool) :
    l.eraseDups.filter p = (l.filter p).eraseDups :=
  filter_eraseDups_aux l.length l p (Nat.le_refl _)

private theorem removeAll_eraseDups {α : Type} [BEq α] [LawfulBEq α] (l s : List α) :
    l.eraseDups.removeAll s = (l.removeAll s).eraseDups :=
  filter_eraseDups l (fun x => !List.elem x s)

private theorem removeAll_eq_of_mem_iff {α : Type} [BEq α] [LawfulBEq α] (l s₁ s₂ : List α)
    (h : ∀ x, x ∈ s₁ ↔ x ∈ s₂) :
    l.removeAll s₁ = l.removeAll s₂ := by
  unfold List.removeAll
  refine List.filter_congr fun x _ => ?_
  congr 1
  cases h1 : List.elem x s₁ <;> cases h2 : List.elem x s₂
  · rfl
  · exfalso
    have hm2 : x ∈ s₂ := List.mem_of_elem_eq_true h2
    have hm1 : x ∈ s₁ := (h x).mpr hm2
    have ht1 : List.elem x s₁ = true := List.elem_eq_true_of_mem hm1
    rw [h1] at ht1; cases ht1
  · exfalso
    have hm1 : x ∈ s₁ := List.mem_of_elem_eq_true h1
    have hm2 : x ∈ s₂ := (h x).mp hm1
    have ht2 : List.elem x s₂ = true := List.elem_eq_true_of_mem hm2
    rw [h2] at ht2; cases ht2
  · rfl

private theorem filter_false_on_mem {α : Type} (p : α → Bool) (xs : List α)
    (h : ∀ x ∈ xs, p x = false) : xs.filter p = [] := by
  induction xs with
  | nil => rfl
  | cons a rest ih =>
    rw [List.filter_cons, h a List.mem_cons_self]
    exact ih fun x hx => h x (List.mem_cons_of_mem a hx)

private theorem removeAll_self_eq_nil {α : Type} [BEq α] [LawfulBEq α] (l : List α) :
    l.removeAll l = [] := by
  unfold List.removeAll
  exact filter_false_on_mem _ l fun x hx => by
    simp only [List.elem_eq_true_of_mem hx, Bool.not_true]

private theorem removeAll_append {α : Type} [BEq α] (l₁ l₂ s : List α) :
    (l₁ ++ l₂).removeAll s = l₁.removeAll s ++ l₂.removeAll s :=
  List.filter_append (l₁ := l₁) (l₂ := l₂)

private theorem removeAll_flatMap_filter_ne {α β : Type} [BEq α] [LawfulBEq α]
    [BEq β] [LawfulBEq β] (a : α) (rest : List α) (f : α → List β) :
    ((rest.filter fun b => !b == a).flatMap f).removeAll (f a) =
      (rest.flatMap f).removeAll (f a) := by
  induction rest with
  | nil => rfl
  | cons b bs ih =>
    rw [List.filter_cons, List.flatMap_cons, removeAll_append]
    cases hba : (b == a)
    · simp only [Bool.not_false, ↓reduceIte, List.flatMap_cons, removeAll_append, ih]
    · have heq : b = a := eq_of_beq hba
      subst heq
      simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte, removeAll_self_eq_nil, List.nil_append, ih]

private theorem eraseDups_flatMap_eraseDups_left_aux {α β : Type} [BEq α] [LawfulBEq α]
    [BEq β] [LawfulBEq β] :
    ∀ (n : Nat) (xs : List α) (f : α → List β), xs.length ≤ n →
      (xs.eraseDups.flatMap f).eraseDups = (xs.flatMap f).eraseDups
  | 0, [], _, _ => by simp
  | 0, _ :: _, _, hlen => by cases hlen
  | n + 1, [], _, _ => by simp
  | n + 1, a :: rest, f, hlen => by
    have hrest : (rest.filter fun b => !b == a).length ≤ n :=
      Nat.le_trans (List.length_filter_le _ _) (Nat.le_of_succ_le_succ hlen)
    rw [List.eraseDups_cons, List.flatMap_cons, List.flatMap_cons,
      List.eraseDups_append, List.eraseDups_append]
    congr 1
    rw [← removeAll_eraseDups,
      eraseDups_flatMap_eraseDups_left_aux n (rest.filter fun b => !b == a) f hrest,
      removeAll_eraseDups, removeAll_flatMap_filter_ne]

private theorem eraseDups_flatMap_eraseDups_left {α β : Type} [BEq α] [LawfulBEq α]
    [BEq β] [LawfulBEq β] (xs : List α) (f : α → List β) :
    (xs.eraseDups.flatMap f).eraseDups = (xs.flatMap f).eraseDups :=
  eraseDups_flatMap_eraseDups_left_aux xs.length xs f (Nat.le_refl _)

private theorem eraseDups_flatMap_eraseDups_right {α β : Type} [BEq β] [LawfulBEq β]
    (xs : List α) (f : α → List β) :
    (xs.flatMap fun x => (f x).eraseDups).eraseDups = (xs.flatMap f).eraseDups := by
  induction xs with
  | nil => rfl
  | cons a rest ih =>
    rw [List.flatMap_cons, List.flatMap_cons, List.eraseDups_append, List.eraseDups_append]
    rw [eraseDups_eq_self (nodup_eraseDups (f a))]
    congr 1
    rw [removeAll_eq_of_mem_iff _ (f a).eraseDups (f a) (fun _ => List.mem_eraseDups)]
    rw [← removeAll_eraseDups, ih, removeAll_eraseDups]

private theorem eraseDups_flatMap_assoc {α β γ : Type} [BEq β] [LawfulBEq β] [BEq γ] [LawfulBEq γ]
    (xs : List α) (g : α → List β) (f : β → List γ) :
    ((xs.flatMap g).eraseDups.flatMap f).eraseDups =
      (xs.flatMap fun x => ((g x).flatMap f).eraseDups).eraseDups := by
  rw [eraseDups_flatMap_eraseDups_left, List.flatMap_assoc, ← eraseDups_flatMap_eraseDups_right]

private theorem alist_lookup_mem {κ α : Type} [DecidableEq κ] {l : AList κ α} {k : κ} {v : α}
    (h : AList.lookup l k = some v) : (k, v) ∈ l := by
  induction l with
  | nil => cases h
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [AList.lookup] at h
    split at h
    · rename_i heq
      subst heq
      cases Option.some.inj h
      exact List.mem_cons_self
    · exact List.mem_cons_of_mem _ (ih h)

private theorem alist_lookup_of_mem {κ α : Type} [DecidableEq κ] {l : AList κ α}
    (hn : (List.map Prod.fst l).Nodup) {k : κ} {v : α} (h : (k, v) ∈ l) :
    AList.lookup l k = some v := by
  induction l with
  | nil => cases h
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    rw [List.map_cons, List.nodup_cons] at hn
    rcases List.mem_cons.mp h with heq | htl
    · cases heq
      simp [AList.lookup]
    · have hne : k' ≠ k := fun eq => hn.1 (eq ▸ List.mem_map.mpr ⟨(k, v), htl, rfl⟩)
      simp [AList.lookup, hne]
      exact ih hn.2 htl

private theorem alist_lookup_none_iff {κ α : Type} [DecidableEq κ] {l : AList κ α} {k : κ} :
    AList.lookup l k = none ↔ ∀ v, (k, v) ∉ l := by
  induction l with
  | nil => simp [AList.lookup]
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [AList.lookup]
    split
    · rename_i heq
      subst heq
      constructor
      · intro h; cases h
      · intro h; exact absurd List.mem_cons_self (h v')
    · rename_i hne
      rw [ih]
      constructor
      · intro h v hmem
        rcases List.mem_cons.mp hmem with heq | htl
        · cases heq; exact hne rfl
        · exact h v htl
      · intro h v htl
        exact h v (List.mem_cons_of_mem _ htl)

private theorem alist_lookup_append {κ α : Type} [DecidableEq κ] (l₁ l₂ : AList κ α) (k : κ) :
    AList.lookup (l₁ ++ l₂) k = match AList.lookup l₁ k with
      | some v => some v
      | none => AList.lookup l₂ k := by
  induction l₁ with
  | nil => rfl
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [List.cons_append, AList.lookup]
    split
    · rfl
    · exact ih

private theorem alist_lookup_map_values {κ α β : Type} [DecidableEq κ]
    (f : κ → α → β) (l : AList κ α) (k : κ) :
    AList.lookup (List.map (fun p => (p.1, f p.1 p.2)) l) k = (AList.lookup l k).map (f k) := by
  induction l with
  | nil => rfl
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [List.map_cons, AList.lookup]
    split
    · rename_i heq; subst heq; rfl
    · exact ih

private theorem alist_lookup_filter_key {κ α : Type} [DecidableEq κ]
    (p : κ → Bool) (l : AList κ α) (k : κ) :
    AList.lookup (l.filter fun entry => p entry.1) k =
      if p k then AList.lookup l k else none := by
  induction l with
  | nil =>
    cases p k <;> rfl
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [List.filter_cons]
    cases hpk' : p k'
    · simp only [Bool.false_eq_true, ↓reduceIte, ih, AList.lookup]
      cases hpk : p k
      · rfl
      · simp only [↓reduceIte]
        split
        · rename_i heq; subst heq; rw [hpk'] at hpk; cases hpk
        · rfl
    · simp only [↓reduceIte, AList.lookup]
      split
      · rename_i heq; subst heq; simp [hpk']
      · exact ih

private theorem mapM_option_some_iff {α β : Type} (f : α → Option β) (xs : List α) :
    (xs.mapM f).isSome = true ↔ ∀ x ∈ xs, (f x).isSome = true := by
  induction xs with
  | nil => simp
  | cons a rest ih =>
    simp only [List.mapM_cons]
    cases hfa : f a with
    | none =>
      simp only [bind, Option.bind, Option.isSome_none, Bool.false_eq_true, false_iff]
      intro hall
      have h := hall a List.mem_cons_self
      simp [hfa] at h
    | some b =>
      cases hm : rest.mapM f with
      | none =>
        simp only [bind, Option.bind, Option.isSome_none, Bool.false_eq_true, false_iff]
        intro hall
        have h := ih.mpr fun x hx => hall x (List.mem_cons_of_mem a hx)
        simp [hm] at h
      | some bs =>
        simp only [bind, Option.bind, pure, Option.isSome_some, true_iff]
        intro x hx
        rcases List.mem_cons.mp hx with rfl | hrest
        · simp [hfa]
        · exact ih.mp (by simp [hm]) x hrest

private theorem mapM_option_length {α β : Type} {f : α → Option β} {xs : List α} {ys : List β}
    (h : xs.mapM f = some ys) : ys.length = xs.length := by
  induction xs generalizing ys with
  | nil =>
    simp only [List.mapM_nil, pure, Option.some.injEq] at h
    subst h; rfl
  | cons a rest ih =>
    simp only [List.mapM_cons, bind, Option.bind] at h
    cases hfa : f a with
    | none => simp [hfa] at h
    | some b =>
      simp only [hfa] at h
      cases hm : rest.mapM f with
      | none => simp [hm] at h
      | some bs =>
        simp only [hm, pure, Option.some.injEq] at h
        subst h
        simp [ih hm]

/-- Explicit justification for omitting a source occurrence in an intra-IR pass.
Elimination applies only to unreachable code or unused, pure, non-trapping computations;
contributors to a fused or merged output remain in `Account.mapped`. -/
inductive EliminationReason where
  | unreachableFunction
  | unreachableBlock
  | deadValue
  deriving DecidableEq, Repr

/-- Purely executable occurrence-accounting witness for an intra-IR pass `a → b`.
`mapped` maps each output occurrence in `b` to its contributing source occurrences in `a`
(supporting both copying — multiple output images — and fusion — multiple contributors),
with unique output keys (`mappedKeysNodup`) and duplicate-free contributor lists
(`mappedSourcesNodup`).
`eliminated` justifies every omitted source occurrence in `a`, with unique source keys
(`eliminatedKeysNodup`) and disjointness from mapped sources (`disjoint`). -/
structure Account where
  mapped : AList Occurrence (List Occurrence)
  eliminated : AList Occurrence EliminationReason
  mappedKeysNodup : (List.map Prod.fst mapped).Nodup := by decide
  mappedSourcesNodup : ∀ entry ∈ mapped, entry.2.Nodup := by decide
  eliminatedKeysNodup : (List.map Prod.fst eliminated).Nodup := by decide
  disjoint : ∀ entry ∈ mapped, ∀ src ∈ entry.2, AList.lookup eliminated src = none := by decide
  deriving DecidableEq

instance : Repr Account where
  reprPrec account prec := reprPrec (account.mapped, account.eliminated) prec

/-- Contributing source occurrences recorded for output occurrence `out`. -/
def Account.sourcesOf (account : Account) (out : Occurrence) : List Occurrence :=
  (AList.lookup account.mapped out).getD []

/-- Output occurrences in `account.mapped` to which `src` contributes. -/
def Account.imagesOf (account : Account) (src : Occurrence) : List Occurrence :=
  account.mapped.filterMap fun (out, srcs) =>
    if srcs.contains src then some out else none

/-- Relational view of `account.mapped`: `src` contributes to output `out`. -/
def Account.MapsTo (account : Account) (out src : Occurrence) : Prop :=
  src ∈ account.sourcesOf out

/-- `src` contributes to at least one output occurrence in `account`. -/
def Account.Mapped (account : Account) (src : Occurrence) : Prop :=
  ∃ out, account.MapsTo out src

/-- `src` is justified as eliminated in `account`. -/
def Account.Eliminated (account : Account) (src : Occurrence) : Prop :=
  ∃ reason, AList.lookup account.eliminated src = some reason

/-- Complete, consistent occurrence-accounting witness for a pass with input occurrences
`inputs` and output occurrences `outputs`:
- every output key of `mapped` belongs to `outputs`, and every output in `outputs` traces
  to at least one contributing source in `inputs`;
- every referenced source and every eliminated key belongs to `inputs`;
- every source in `inputs` is either mapped to some output or explicitly eliminated. -/
def Account.Valid (inputs outputs : List Occurrence) (account : Account) : Prop :=
  (∀ out ∈ List.map Prod.fst account.mapped, out ∈ outputs) ∧
  (∀ out ∈ outputs, ∃ src, account.MapsTo out src) ∧
  (∀ out src, account.MapsTo out src → src ∈ inputs) ∧
  (∀ src ∈ List.map Prod.fst account.eliminated, src ∈ inputs) ∧
  (∀ src ∈ inputs, account.Mapped src ∨ account.Eliminated src)

/-- Extensional equivalence of two accounting witnesses: they induce the exact same
output-to-source relation and justify the exact same set of eliminated sources. -/
def Account.Equiv (a b : Account) : Prop :=
  (∀ out src, a.MapsTo out src ↔ b.MapsTo out src) ∧
  (∀ src, a.Eliminated src ↔ b.Eliminated src)

theorem Account.Equiv.refl (a : Account) : Account.Equiv a a :=
  ⟨fun _ _ => Iff.rfl, fun _ => Iff.rfl⟩

theorem Account.Equiv.symm {a b : Account} (h : Account.Equiv a b) : Account.Equiv b a :=
  ⟨fun out src => (h.1 out src).symm, fun src => (h.2 src).symm⟩

theorem Account.Equiv.trans {a b c : Account}
    (hab : Account.Equiv a b) (hbc : Account.Equiv b c) : Account.Equiv a c :=
  ⟨fun out src => (hab.1 out src).trans (hbc.1 out src),
    fun src => (hab.2 src).trans (hbc.2 src)⟩

theorem Account.sourcesOf_nodup (account : Account) (out : Occurrence) :
    (account.sourcesOf out).Nodup := by
  unfold Account.sourcesOf
  cases h : AList.lookup account.mapped out with
  | none => exact List.nodup_nil
  | some srcs => exact account.mappedSourcesNodup (out, srcs) (alist_lookup_mem h)

theorem Account.mapsTo_iff_mem_mapped (account : Account) (out src : Occurrence) :
    account.MapsTo out src ↔ ∃ srcs, (out, srcs) ∈ account.mapped ∧ src ∈ srcs := by
  unfold Account.MapsTo Account.sourcesOf
  constructor
  · intro h
    cases hlook : AList.lookup account.mapped out with
    | none => simp [hlook] at h
    | some srcs =>
      simp only [hlook, Option.getD_some] at h
      exact ⟨srcs, alist_lookup_mem hlook, h⟩
  · rintro ⟨srcs, hmem, hsrc⟩
    rw [alist_lookup_of_mem account.mappedKeysNodup hmem]
    exact hsrc

theorem Account.mem_imagesOf_iff (account : Account) (src out : Occurrence) :
    out ∈ account.imagesOf src ↔ account.MapsTo out src := by
  rw [Account.mapsTo_iff_mem_mapped]
  unfold Account.imagesOf
  rw [List.mem_filterMap]
  constructor
  · rintro ⟨⟨o, srcs⟩, hmem, hres⟩
    cases hc : srcs.contains src
    · simp only [hc, Bool.false_eq_true, ↓reduceIte] at hres
      cases hres
    · simp only [hc, ↓reduceIte, Option.some.injEq] at hres
      subst hres
      exact ⟨srcs, hmem, List.mem_of_elem_eq_true hc⟩
  · rintro ⟨srcs, hmem, hsrc⟩
    refine ⟨(out, srcs), hmem, ?_⟩
    simp only [List.contains_eq_mem, hsrc, decide_true, ↓reduceIte]

theorem Account.mapped_iff_any (account : Account) (src : Occurrence) :
    account.Mapped src ↔ (account.mapped.any fun entry => entry.2.contains src) = true := by
  unfold Account.Mapped
  rw [List.any_eq_true]
  constructor
  · rintro ⟨out, hmap⟩
    obtain ⟨srcs, hmem, hsrc⟩ := (account.mapsTo_iff_mem_mapped out src).mp hmap
    exact ⟨(out, srcs), hmem, List.elem_eq_true_of_mem hsrc⟩
  · rintro ⟨⟨out, srcs⟩, hmem, hc⟩
    exact ⟨out, (account.mapsTo_iff_mem_mapped out src).mpr ⟨srcs, hmem, List.mem_of_elem_eq_true hc⟩⟩

theorem Account.eliminated_iff_isSome (account : Account) (src : Occurrence) :
    account.Eliminated src ↔ (AList.lookup account.eliminated src).isSome = true := by
  unfold Account.Eliminated
  cases h : AList.lookup account.eliminated src with
  | none => simp
  | some reason => simp

theorem Account.not_eliminated_of_mapped (account : Account) {src : Occurrence}
    (h : account.Mapped src) : ¬ account.Eliminated src := by
  obtain ⟨out, hmap⟩ := h
  obtain ⟨srcs, hmem, hsrc⟩ := (account.mapsTo_iff_mem_mapped out src).mp hmap
  rintro ⟨reason, hlook⟩
  have hnone := account.disjoint (out, srcs) hmem src hsrc
  rw [hnone] at hlook
  cases hlook

/-- Identity accounting witness mapping every occurrence in `occurrences` to itself
with no eliminations. -/
def Account.identity (occurrences : List Occurrence) : Account where
  mapped := occurrences.eraseDups.map fun occ => (occ, [occ])
  eliminated := []
  mappedKeysNodup := by
    rw [List.map_map]
    simpa [Function.comp_def] using nodup_eraseDups occurrences
  mappedSourcesNodup := by
    intro entry hmem
    obtain ⟨occ, _, rfl⟩ := List.mem_map.mp hmem
    exact List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩
  eliminatedKeysNodup := List.nodup_nil
  disjoint := by
    intro _ _ _ _
    rfl

/-- Identity accounting witness for an IR program at any insertion point. -/
def Account.identityFor {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} (ir : AtLang lang profile point) : Account :=
  Account.identity ((AtLang.occurrences ir).map Prod.fst)

theorem Account.lookup_identity_of_mem {occs : List Occurrence} {occ : Occurrence}
    (h : occ ∈ occs) :
    AList.lookup (Account.identity occs).mapped occ = some [occ] := by
  apply alist_lookup_of_mem (Account.identity occs).mappedKeysNodup
  exact List.mem_map.mpr ⟨occ, List.mem_eraseDups.mpr h, rfl⟩

theorem Account.lookup_identity_of_not_mem {occs : List Occurrence} {occ : Occurrence}
    (h : occ ∉ occs) :
    AList.lookup (Account.identity occs).mapped occ = none := by
  rw [alist_lookup_none_iff]
  intro srcs hmem
  obtain ⟨occ', hocc', heq⟩ := List.mem_map.mp hmem
  cases Prod.mk.inj heq |>.1
  exact h (List.mem_eraseDups.mp hocc')

theorem Account.sourcesOf_identity (occs : List Occurrence) (occ : Occurrence) :
    (Account.identity occs).sourcesOf occ = if occ ∈ occs then [occ] else [] := by
  unfold Account.sourcesOf
  split
  · rename_i hmem
    rw [Account.lookup_identity_of_mem hmem]; rfl
  · rename_i hnot
    rw [Account.lookup_identity_of_not_mem hnot]; rfl

theorem Account.mapsTo_identity_iff (occs : List Occurrence) (out src : Occurrence) :
    (Account.identity occs).MapsTo out src ↔ out ∈ occs ∧ src = out := by
  unfold Account.MapsTo
  rw [Account.sourcesOf_identity]
  split
  · rename_i hmem
    simp [hmem]
  · rename_i hnot
    simp [hnot]

theorem Account.identity_valid (occs : List Occurrence) :
    (Account.identity occs).Valid occs occs := by
  refine ⟨?_, ?_, ?_, ?_, ?_⟩
  · intro out hout
    rw [Account.identity, List.map_map] at hout
    obtain ⟨occ, hocc, rfl⟩ := List.mem_map.mp hout
    exact List.mem_eraseDups.mp hocc
  · intro out hout
    exact ⟨out, (Account.mapsTo_identity_iff occs out out).mpr ⟨hout, rfl⟩⟩
  · intro out src hmap
    obtain ⟨hout, rfl⟩ := (Account.mapsTo_identity_iff occs out src).mp hmap
    exact hout
  · intro _ hmem
    cases hmem
  · intro src hsrc
    exact Or.inl ⟨src, (Account.mapsTo_identity_iff occs src src).mpr ⟨hsrc, rfl⟩⟩

private def Account.composeMapped (first second : Account) : AList Occurrence (List Occurrence) :=
  List.map (fun (occC, srcsB) => (occC, (srcsB.flatMap first.sourcesOf).eraseDups)) second.mapped

private def Account.composeEliminated (first second : Account)
    (mapped : AList Occurrence (List Occurrence)) : AList Occurrence EliminationReason :=
  let surviving (occA : Occurrence) : Bool :=
    mapped.any fun entry => entry.2.contains occA
  let fromFirst := first.eliminated.filter fun entry => !surviving entry.1
  let candidateSources := (first.mapped.flatMap Prod.snd).eraseDups
  let fromSecond := candidateSources.filterMap fun occA =>
    if surviving occA || (AList.lookup fromFirst occA).isSome then none
    else match (first.imagesOf occA).mapM (AList.lookup second.eliminated) with
      | some (reason :: _) => some (occA, reason)
      | _ => none
  fromFirst ++ fromSecond

private theorem nodup_filterMap_key {α β : Type} {xs : List α} (f : α → Option β)
    (h : xs.Nodup) :
    ((xs.filterMap fun a => (f a).map (a, ·)).map Prod.fst).Nodup := by
  induction xs with
  | nil => exact List.nodup_nil
  | cons a rest ih =>
    rw [List.nodup_cons] at h
    rw [List.filterMap_cons]
    cases hfa : f a with
    | none =>
      exact ih h.2
    | some b =>
      simp only [Option.map_some, List.map_cons, List.nodup_cons]
      refine ⟨?_, ih h.2⟩
      intro hmem
      obtain ⟨⟨k, v⟩, hkv, heq⟩ := List.mem_map.mp hmem
      dsimp only at heq
      subst heq
      obtain ⟨a', ha', hmap⟩ := List.mem_filterMap.mp hkv
      cases hfa' : f a' with
      | none => simp [hfa'] at hmap
      | some b' =>
        simp only [hfa', Option.map_some, Option.some.injEq, Prod.mk.injEq] at hmap
        exact h.1 (hmap.1 ▸ ha')

private theorem nodup_composeEliminated_keys (first second : Account)
    (mapped : AList Occurrence (List Occurrence)) :
    (List.map Prod.fst (Account.composeEliminated first second mapped)).Nodup := by
  unfold Account.composeEliminated
  dsimp only
  let surviving : Occurrence → Bool := fun occA => mapped.any fun entry => entry.2.contains occA
  let fromFirst := first.eliminated.filter fun entry => !surviving entry.1
  let candidateSources := (first.mapped.flatMap Prod.snd).eraseDups
  let pickReason : Occurrence → Option EliminationReason := fun occA =>
    if surviving occA || (AList.lookup fromFirst occA).isSome then none
    else match (first.imagesOf occA).mapM (AList.lookup second.eliminated) with
      | some (reason :: _) => some reason
      | _ => none
  have hFromSecondEq :
      (candidateSources.filterMap fun occA =>
        if surviving occA || (AList.lookup fromFirst occA).isSome then none
        else match (first.imagesOf occA).mapM (AList.lookup second.eliminated) with
          | some (reason :: _) => some (occA, reason)
          | _ => none) =
      candidateSources.filterMap (fun occA => (pickReason occA).map (occA, ·)) := by
    congr 1
    funext occA
    dsimp only [pickReason]
    cases surviving occA || (AList.lookup fromFirst occA).isSome
    · simp only [Bool.false_eq_true, ↓reduceIte]
      cases (first.imagesOf occA).mapM (AList.lookup second.eliminated) with
      | none => rfl
      | some rs => cases rs <;> rfl
    · rfl
  change (List.map Prod.fst (fromFirst ++ (candidateSources.filterMap fun occA =>
    if surviving occA || (AList.lookup fromFirst occA).isSome then none
    else match (first.imagesOf occA).mapM (AList.lookup second.eliminated) with
      | some (reason :: _) => some (occA, reason)
      | _ => none))).Nodup
  rw [hFromSecondEq, List.map_append]
  have hNodupFirst : (List.map Prod.fst fromFirst).Nodup := by
    have hEq : List.map Prod.fst fromFirst =
        (List.map Prod.fst first.eliminated).filter fun occA => !surviving occA := by
      change List.map Prod.fst (first.eliminated.filter fun entry => !surviving entry.1) = _
      induction first.eliminated with
      | nil => rfl
      | cons hd tl ih =>
        rw [List.filter_cons, List.map_cons, List.filter_cons]
        cases !surviving hd.1 <;> simp [ih]
    rw [hEq]
    exact nodup_filter _ first.eliminatedKeysNodup
  have hNodupSecond : (List.map Prod.fst (candidateSources.filterMap fun occA => (pickReason occA).map (occA, ·))).Nodup :=
    nodup_filterMap_key pickReason (nodup_eraseDups _)
  refine nodup_append_of_disjoint hNodupFirst hNodupSecond ?_
  intro occA h1 h2
  obtain ⟨⟨k, r⟩, hkr, rfl⟩ := List.mem_map.mp h1
  obtain ⟨⟨k2, r2⟩, hkr2, heq⟩ := List.mem_map.mp h2
  dsimp only at heq
  subst heq
  obtain ⟨a', _, hmap⟩ := List.mem_filterMap.mp hkr2
  cases hp : pickReason a' with
  | none => simp [hp] at hmap
  | some r' =>
    simp only [hp, Option.map_some, Option.some.injEq, Prod.mk.injEq] at hmap
    obtain ⟨rfl, rfl⟩ := hmap
    have hlook : AList.lookup fromFirst a' = some r :=
      alist_lookup_of_mem hNodupFirst hkr
    dsimp only [pickReason] at hp
    rw [hlook] at hp
    simp at hp

private theorem composeEliminated_disjoint (first second : Account)
    (mapped : AList Occurrence (List Occurrence)) :
    ∀ entry ∈ mapped, ∀ src ∈ entry.2,
      AList.lookup (Account.composeEliminated first second mapped) src = none := by
  intro entry hEntry src hSrc
  have hSurv : (mapped.any fun e => e.2.contains src) = true :=
    List.any_eq_true.mpr ⟨entry, hEntry, List.elem_eq_true_of_mem hSrc⟩
  rw [alist_lookup_none_iff]
  intro reason hmem
  unfold Account.composeEliminated at hmem
  rcases List.mem_append.mp hmem with hFirst | hSecond
  · have hf := (List.mem_filter.mp hFirst).2
    dsimp only at hf
    rw [hSurv] at hf
    cases hf
  · obtain ⟨a', _, hmap⟩ := List.mem_filterMap.mp hSecond
    cases hs : (mapped.any fun e => e.2.contains a')
    · simp only [hs, Bool.false_or] at hmap
      split at hmap
      · cases hmap
      · split at hmap
        · cases Option.some.inj hmap
          rw [hSurv] at hs; cases hs
        · cases hmap
    · simp only [hs, Bool.true_or, ↓reduceIte] at hmap
      cases hmap

/-- Compose two sequential occurrence-accounting witnesses `first` (`a → b`) and
`second` (`b → c`) by relational composition on `mapped` and full elimination
justification on omitted sources. -/
def Account.compose (first second : Account) : Account :=
  let mapped := Account.composeMapped first second
  { mapped := mapped
    eliminated := Account.composeEliminated first second mapped
    mappedKeysNodup := by
      unfold mapped Account.composeMapped
      rw [List.map_map]
      simpa [Function.comp_def] using second.mappedKeysNodup
    mappedSourcesNodup := by
      intro entry hmem
      obtain ⟨⟨occC, srcsB⟩, _, rfl⟩ := List.mem_map.mp hmem
      exact nodup_eraseDups _
    eliminatedKeysNodup := nodup_composeEliminated_keys first second mapped
    disjoint := composeEliminated_disjoint first second mapped }

theorem Account.sourcesOf_compose (first second : Account) (occC : Occurrence) :
    (Account.compose first second).sourcesOf occC =
      ((second.sourcesOf occC).flatMap first.sourcesOf).eraseDups := by
  unfold Account.sourcesOf Account.compose Account.composeMapped
  dsimp only
  have hLook := alist_lookup_map_values (fun _ (srcsB : List Occurrence) => (srcsB.flatMap first.sourcesOf).eraseDups)
    second.mapped occC
  change (AList.lookup (List.map (fun p => (p.1, (p.2.flatMap first.sourcesOf).eraseDups)) second.mapped) occC).getD [] = _
  rw [hLook]
  cases AList.lookup second.mapped occC <;> rfl

/-- Relational composition law for `Account.compose`: `occA` contributes to `occC` in
`first ∘ second` iff some intermediate `occB` connects them. -/
theorem Account.mapsTo_compose_iff (first second : Account) (occC occA : Occurrence) :
    (Account.compose first second).MapsTo occC occA ↔
      ∃ occB, second.MapsTo occC occB ∧ first.MapsTo occB occA := by
  unfold Account.MapsTo
  rw [Account.sourcesOf_compose, List.mem_eraseDups, List.mem_flatMap]

theorem Account.eliminated_compose_iff (first second : Account) (occA : Occurrence) :
    (Account.compose first second).Eliminated occA ↔
      ¬ (Account.compose first second).Mapped occA ∧
      (first.Eliminated occA ∨
        (first.Mapped occA ∧ ∀ occB, first.MapsTo occB occA → second.Eliminated occB)) := by
  rw [Account.eliminated_iff_isSome]
  let mapped := Account.composeMapped first second
  let surviving : Occurrence → Bool := fun x => mapped.any fun entry => entry.2.contains x
  have hSurvIff : surviving occA = true ↔ (Account.compose first second).Mapped occA :=
    (Account.mapped_iff_any (Account.compose first second) occA).symm
  let fromFirst := first.eliminated.filter fun entry => !surviving entry.1
  let candidateSources := (first.mapped.flatMap Prod.snd).eraseDups
  let fromSecond := candidateSources.filterMap fun x =>
    if surviving x || (AList.lookup fromFirst x).isSome then none
    else match (first.imagesOf x).mapM (AList.lookup second.eliminated) with
      | some (reason :: _) => some (x, reason)
      | _ => none
  change (AList.lookup (fromFirst ++ fromSecond) occA).isSome = true ↔ _
  rw [alist_lookup_append]
  have hLookFirst : AList.lookup fromFirst occA =
      if !surviving occA then AList.lookup first.eliminated occA else none :=
    alist_lookup_filter_key (fun x => !surviving x) first.eliminated occA
  cases hSurv : surviving occA
  · have hNotMapped : ¬ (Account.compose first second).Mapped occA := by
      intro hm; have ht := hSurvIff.mpr hm; rw [hSurv] at ht; cases ht
    rw [hLookFirst]
    simp only [hSurv, Bool.not_false, ↓reduceIte]
    cases hElim1 : AList.lookup first.eliminated occA with
    | some r1 =>
      simp only [Option.isSome_some, true_iff]
      exact ⟨hNotMapped, Or.inl ⟨r1, hElim1⟩⟩
    | none =>
      have hNotElim1 : ¬ first.Eliminated occA := by
        rintro ⟨r, hr⟩; rw [hElim1] at hr; cases hr
      have hFromFirstNone : AList.lookup fromFirst occA = none := by
        rw [hLookFirst]; simp [hSurv, hElim1]
      have hFromSecondIff :
          (AList.lookup fromSecond occA).isSome = true ↔
            first.Mapped occA ∧ ∀ occB, first.MapsTo occB occA → second.Eliminated occB := by
        constructor
        · intro hSome
          cases hLook2 : AList.lookup fromSecond occA with
          | none => simp [hLook2] at hSome
          | some r2 =>
            have hMem := alist_lookup_mem hLook2
            obtain ⟨x, hxCand, hMap⟩ := List.mem_filterMap.mp hMem
            cases hCond : surviving x || (AList.lookup fromFirst x).isSome
            · simp only [hCond, Bool.false_eq_true, ↓reduceIte] at hMap
              cases hM : (first.imagesOf x).mapM (AList.lookup second.eliminated) with
              | none => simp [hM] at hMap
              | some rs =>
                cases rs with
                | nil => simp [hM] at hMap
                | cons r0 rs' =>
                  simp only [hM, Option.some.injEq, Prod.mk.injEq] at hMap
                  obtain ⟨rfl, rfl⟩ := hMap
                  have hxFlat : x ∈ first.mapped.flatMap Prod.snd := List.mem_eraseDups.mp hxCand
                  obtain ⟨⟨outB, srcsA⟩, hMemB, hxIn⟩ := List.mem_flatMap.mp hxFlat
                  have hMapped1 : first.Mapped x :=
                    ⟨outB, (first.mapsTo_iff_mem_mapped outB x).mpr ⟨srcsA, hMemB, hxIn⟩⟩
                  refine ⟨hMapped1, fun occB hMapsTo => ?_⟩
                  have hImg : occB ∈ first.imagesOf x := (first.mem_imagesOf_iff x occB).mpr hMapsTo
                  have hAll := (mapM_option_some_iff (AList.lookup second.eliminated) (first.imagesOf x)).mp
                    (by simp [hM]) occB hImg
                  exact (second.eliminated_iff_isSome occB).mpr hAll
            · simp only [hCond, ↓reduceIte] at hMap
              cases hMap
        · rintro ⟨hMapped1, hAllElim⟩
          obtain ⟨out0, hMaps0⟩ := hMapped1
          obtain ⟨srcs0, hMem0, hIn0⟩ := (first.mapsTo_iff_mem_mapped out0 occA).mp hMaps0
          have hCand : occA ∈ candidateSources :=
            List.mem_eraseDups.mpr (List.mem_flatMap.mpr ⟨(out0, srcs0), hMem0, hIn0⟩)
          have hImg0 : out0 ∈ first.imagesOf occA := (first.mem_imagesOf_iff occA out0).mpr hMaps0
          have hMapMSome : ((first.imagesOf occA).mapM (AList.lookup second.eliminated)).isSome = true := by
            rw [mapM_option_some_iff]
            intro occB hImg
            exact (second.eliminated_iff_isSome occB).mp
              (hAllElim occB ((first.mem_imagesOf_iff occA occB).mp hImg))
          cases hM : (first.imagesOf occA).mapM (AList.lookup second.eliminated) with
          | none => simp [hM] at hMapMSome
          | some rs =>
            cases rs with
            | nil =>
              have hLen := mapM_option_length hM
              cases hImgs : first.imagesOf occA with
              | nil => rw [hImgs] at hImg0; cases hImg0
              | cons _ _ => rw [hImgs] at hLen; cases hLen
            | cons r0 rs' =>
              have hMem2 : (occA, r0) ∈ fromSecond := by
                refine List.mem_filterMap.mpr ⟨occA, hCand, ?_⟩
                simp only [hSurv, hFromFirstNone, Option.isSome_none, Bool.or_self,
                  Bool.false_eq_true, ↓reduceIte, hM]
              cases hLook2 : AList.lookup fromSecond occA with
              | none =>
                exact absurd hMem2 ((alist_lookup_none_iff.mp hLook2) r0)
              | some _ => rfl
      rw [hFromSecondIff]
      constructor
      · intro h; exact ⟨hNotMapped, Or.inr h⟩
      · rintro ⟨_, h1 | h2⟩
        · exact absurd h1 hNotElim1
        · exact h2
  · have hMapped : (Account.compose first second).Mapped occA := hSurvIff.mp hSurv
    have hLookFirstNone : AList.lookup fromFirst occA = none := by
      rw [hLookFirst]; simp [hSurv]
    rw [hLookFirstNone]
    have hLookSecondNone : AList.lookup fromSecond occA = none := by
      rw [alist_lookup_none_iff]
      intro r hmem
      obtain ⟨x, _, hmap⟩ := List.mem_filterMap.mp hmem
      cases hCond : surviving x || (AList.lookup fromFirst x).isSome
      · simp only [hCond, Bool.false_eq_true, ↓reduceIte] at hmap
        cases hM : (first.imagesOf x).mapM (AList.lookup second.eliminated) with
        | none => simp [hM] at hmap
        | some rs =>
          cases rs with
          | nil => simp [hM] at hmap
          | cons _ _ =>
            simp only [hM, Option.some.injEq, Prod.mk.injEq] at hmap
            obtain ⟨rfl, _⟩ := hmap
            simp [hSurv] at hCond
      · simp only [hCond, ↓reduceIte] at hmap
        cases hmap
    rw [hLookSecondNone]
    simp [hMapped]

/-- Composition preserves every source occurrence: if every source of `first` is mapped
or eliminated and every output of `first` is mapped or eliminated by `second`, then
every source is mapped or eliminated in `Account.compose first second`. -/
theorem Account.compose_preserves_sources {occsA occsB : List Occurrence}
    {first second : Account}
    (hFirstCovers : ∀ src ∈ occsA, first.Mapped src ∨ first.Eliminated src)
    (hFirstOutputs : ∀ out ∈ List.map Prod.fst first.mapped, out ∈ occsB)
    (hSecondCovers : ∀ src ∈ occsB, second.Mapped src ∨ second.Eliminated src) :
    ∀ src ∈ occsA,
      (Account.compose first second).Mapped src ∨
        (Account.compose first second).Eliminated src := by
  intro src hSrc
  cases hMapped : ((Account.compose first second).mapped.any fun e => e.2.contains src)
  · right
    have hNotMapped : ¬ (Account.compose first second).Mapped src := by
      intro hm
      have ht := (Account.mapped_iff_any (Account.compose first second) src).mp hm
      rw [hMapped] at ht; cases ht
    rw [Account.eliminated_compose_iff]
    refine ⟨hNotMapped, ?_⟩
    rcases hFirstCovers src hSrc with hMap1 | hElim1
    · right
      refine ⟨hMap1, fun occB hMaps1 => ?_⟩
      obtain ⟨srcsA, hMemB, _⟩ := (first.mapsTo_iff_mem_mapped occB src).mp hMaps1
      have hOutB : occB ∈ occsB := hFirstOutputs occB (List.mem_map.mpr ⟨(occB, srcsA), hMemB, rfl⟩)
      rcases hSecondCovers occB hOutB with ⟨occC, hMaps2⟩ | hElim2
      · exfalso
        exact hNotMapped ⟨occC, (Account.mapsTo_compose_iff first second occC src).mpr
          ⟨occB, hMaps2, hMaps1⟩⟩
      · exact hElim2
    · exact Or.inl hElim1
  · left
    exact (Account.mapped_iff_any (Account.compose first second) src).mpr hMapped

/-- Composition of two valid accounting witnesses is a valid accounting witness. -/
theorem Account.compose_valid {occsA occsB occsC : List Occurrence}
    {first second : Account}
    (hFirst : first.Valid occsA occsB) (hSecond : second.Valid occsB occsC) :
    (Account.compose first second).Valid occsA occsC := by
  refine ⟨?_, ?_, ?_, ?_, ?_⟩
  · intro out hOut
    unfold Account.compose Account.composeMapped at hOut
    dsimp only at hOut
    rw [List.map_map] at hOut
    exact hSecond.1 out (by simpa [Function.comp_def] using hOut)
  · intro outC hOutC
    obtain ⟨occB, hMaps2⟩ := hSecond.2.1 outC hOutC
    have hInB : occB ∈ occsB := hSecond.2.2.1 outC occB hMaps2
    obtain ⟨srcA, hMaps1⟩ := hFirst.2.1 occB hInB
    exact ⟨srcA, (Account.mapsTo_compose_iff first second outC srcA).mpr ⟨occB, hMaps2, hMaps1⟩⟩
  · intro out src hMaps
    obtain ⟨occB, _, hMaps1⟩ := (Account.mapsTo_compose_iff first second out src).mp hMaps
    exact hFirst.2.2.1 occB src hMaps1
  · intro src hMem
    obtain ⟨⟨k, r⟩, hkr, rfl⟩ := List.mem_map.mp hMem
    have hElim : (Account.compose first second).Eliminated k :=
      ⟨r, alist_lookup_of_mem (Account.compose first second).eliminatedKeysNodup hkr⟩
    obtain ⟨_, hElim1 | ⟨⟨occB, hMaps1⟩, _⟩⟩ := (Account.eliminated_compose_iff first second k).mp hElim
    · obtain ⟨r1, hr1⟩ := hElim1
      exact hFirst.2.2.2.1 k (List.mem_map.mpr ⟨(k, r1), alist_lookup_mem hr1, rfl⟩)
    · exact hFirst.2.2.1 occB k hMaps1
  · exact Account.compose_preserves_sources hFirst.2.2.2.2 hFirst.1 hSecond.2.2.2.2

/-- Exact structural associativity of `Account.compose` on `mapped`. -/
theorem Account.compose_assoc_mapped (first second third : Account) :
    (Account.compose (Account.compose first second) third).mapped =
      (Account.compose first (Account.compose second third)).mapped := by
  change Account.composeMapped (Account.compose first second) third =
    Account.composeMapped first (Account.compose second third)
  unfold Account.composeMapped
  change _ = List.map (fun (occC, srcsB) => (occC, (srcsB.flatMap first.sourcesOf).eraseDups))
    (Account.composeMapped second third)
  unfold Account.composeMapped
  rw [List.map_map]
  refine List.map_congr_left fun entry _ => ?_
  rcases entry with ⟨occD, srcsC⟩
  dsimp only [Function.comp_def]
  congr 1
  have hFun :
      (Account.compose first second).sourcesOf =
      (fun occC => ((second.sourcesOf occC).flatMap first.sourcesOf).eraseDups) := by
    funext occC
    exact Account.sourcesOf_compose first second occC
  rw [hFun]
  exact (eraseDups_flatMap_assoc srcsC second.sourcesOf first.sourcesOf).symm

theorem Account.compose_assoc_mapsTo (first second third : Account) (out src : Occurrence) :
    (Account.compose (Account.compose first second) third).MapsTo out src ↔
      (Account.compose first (Account.compose second third)).MapsTo out src := by
  unfold Account.MapsTo Account.sourcesOf
  rw [Account.compose_assoc_mapped]

theorem Account.compose_assoc_eliminated (first second third : Account)
    (hSecond : ∀ out, (∃ src, first.MapsTo out src) → second.Mapped out ∨ second.Eliminated out)
    (src : Occurrence) :
    (Account.compose (Account.compose first second) third).Eliminated src ↔
      (Account.compose first (Account.compose second third)).Eliminated src := by
  have hMappedEq :
      (Account.compose (Account.compose first second) third).Mapped src ↔
        (Account.compose first (Account.compose second third)).Mapped src := by
    unfold Account.Mapped
    constructor
    · rintro ⟨out, h⟩; exact ⟨out, (Account.compose_assoc_mapsTo first second third out src).mp h⟩
    · rintro ⟨out, h⟩; exact ⟨out, (Account.compose_assoc_mapsTo first second third out src).mpr h⟩
  rw [Account.eliminated_compose_iff (Account.compose first second) third,
    Account.eliminated_compose_iff first (Account.compose second third)]
  constructor
  · rintro ⟨hNotL, hElimAB | ⟨hMapAB, hAllC⟩⟩
    · have hNotR : ¬ (Account.compose first (Account.compose second third)).Mapped src :=
        fun h => hNotL (hMappedEq.mpr h)
      refine ⟨hNotR, ?_⟩
      obtain ⟨hNotAB, hElimA | ⟨hMapA, hAllB⟩⟩ := (Account.eliminated_compose_iff first second src).mp hElimAB
      · exact Or.inl hElimA
      · right
        refine ⟨hMapA, fun occB hMapsA => ?_⟩
        rw [Account.eliminated_compose_iff]
        have hNotBC : ¬ (Account.compose second third).Mapped occB := by
          rintro ⟨occD, hMapsBC⟩
          exact hNotR ⟨occD, (Account.mapsTo_compose_iff first (Account.compose second third) occD src).mpr
            ⟨occB, hMapsBC, hMapsA⟩⟩
        exact ⟨hNotBC, Or.inl (hAllB occB hMapsA)⟩
    · have hNotR : ¬ (Account.compose first (Account.compose second third)).Mapped src :=
        fun h => hNotL (hMappedEq.mpr h)
      obtain ⟨occC0, hMapsAB0⟩ := hMapAB
      obtain ⟨occB0, _, hMapsA0⟩ := (Account.mapsTo_compose_iff first second occC0 src).mp hMapsAB0
      refine ⟨hNotR, Or.inr ⟨⟨occB0, hMapsA0⟩, fun occB hMapsA => ?_⟩⟩
      rw [Account.eliminated_compose_iff]
      have hNotBC : ¬ (Account.compose second third).Mapped occB := by
        rintro ⟨occD, hMapsBC⟩
        exact hNotR ⟨occD, (Account.mapsTo_compose_iff first (Account.compose second third) occD src).mpr
          ⟨occB, hMapsBC, hMapsA⟩⟩
      refine ⟨hNotBC, ?_⟩
      rcases hSecond occB ⟨src, hMapsA⟩ with hMapB | hElimB
      · right
        refine ⟨hMapB, fun occC hMapsB => ?_⟩
        exact hAllC occC ((Account.mapsTo_compose_iff first second occC src).mpr ⟨occB, hMapsB, hMapsA⟩)
      · exact Or.inl hElimB
  · rintro ⟨hNotR, hElimA | ⟨hMapA, hAllBC⟩⟩
    · have hNotL : ¬ (Account.compose (Account.compose first second) third).Mapped src :=
        fun h => hNotR (hMappedEq.mp h)
      refine ⟨hNotL, Or.inl ?_⟩
      rw [Account.eliminated_compose_iff]
      have hNotAB : ¬ (Account.compose first second).Mapped src := by
        rintro ⟨occC, hMapsAB⟩
        obtain ⟨occB, _, hMapsA⟩ := (Account.mapsTo_compose_iff first second occC src).mp hMapsAB
        exact first.not_eliminated_of_mapped ⟨occB, hMapsA⟩ hElimA
      exact ⟨hNotAB, Or.inl hElimA⟩
    · have hNotL : ¬ (Account.compose (Account.compose first second) third).Mapped src :=
        fun h => hNotR (hMappedEq.mp h)
      refine ⟨hNotL, ?_⟩
      cases hAB : ((Account.compose first second).mapped.any fun e => e.2.contains src)
      · left
        have hNotAB : ¬ (Account.compose first second).Mapped src := by
          intro hm
          have ht := (Account.mapped_iff_any (Account.compose first second) src).mp hm
          rw [hAB] at ht; cases ht
        rw [Account.eliminated_compose_iff]
        refine ⟨hNotAB, Or.inr ⟨hMapA, fun occB hMapsA => ?_⟩⟩
        obtain ⟨_, hElimB | ⟨⟨occC, hMapsB⟩, _⟩⟩ :=
          (Account.eliminated_compose_iff second third occB).mp (hAllBC occB hMapsA)
        · exact hElimB
        · exfalso
          exact hNotAB ⟨occC, (Account.mapsTo_compose_iff first second occC src).mpr ⟨occB, hMapsB, hMapsA⟩⟩
      · right
        have hMapAB : (Account.compose first second).Mapped src :=
          (Account.mapped_iff_any (Account.compose first second) src).mpr hAB
        refine ⟨hMapAB, fun occC hMapsAB => ?_⟩
        obtain ⟨occB, hMapsB, hMapsA⟩ := (Account.mapsTo_compose_iff first second occC src).mp hMapsAB
        obtain ⟨_, hElimB | ⟨_, hAllC⟩⟩ :=
          (Account.eliminated_compose_iff second third occB).mp (hAllBC occB hMapsA)
        · exact absurd hElimB (second.not_eliminated_of_mapped ⟨occC, hMapsB⟩)
        · exact hAllC occC hMapsB

/-- Composition is associative up to `Account.Equiv` whenever `second` accounts for every
output reached in `first`. -/
theorem Account.compose_assoc (first second third : Account)
    (hSecond : ∀ out, (∃ src, first.MapsTo out src) → second.Mapped out ∨ second.Eliminated out) :
    Account.Equiv
      (Account.compose (Account.compose first second) third)
      (Account.compose first (Account.compose second third)) :=
  ⟨Account.compose_assoc_mapsTo first second third,
    Account.compose_assoc_eliminated first second third hSecond⟩

theorem Account.compose_assoc_of_valid {occsA occsB occsC : List Occurrence}
    {first second : Account} (third : Account)
    (hFirst : first.Valid occsA occsB) (hSecond : second.Valid occsB occsC) :
    Account.Equiv
      (Account.compose (Account.compose first second) third)
      (Account.compose first (Account.compose second third)) := by
  refine Account.compose_assoc first second third fun out hReach => ?_
  obtain ⟨src, hMaps⟩ := hReach
  obtain ⟨srcs, hMem, _⟩ := (first.mapsTo_iff_mem_mapped out src).mp hMaps
  exact hSecond.2.2.2.2 out (hFirst.1 out (List.mem_map.mpr ⟨(out, srcs), hMem, rfl⟩))

/-- `Account.identity` is an exact left unit on `mapped` whenever `occsA` covers every
source referenced by `account.mapped`. -/
theorem Account.compose_identity_left_mapped {occsA : List Occurrence} {account : Account}
    (hSrc : ∀ out src, account.MapsTo out src → src ∈ occsA) :
    (Account.compose (Account.identity occsA) account).mapped = account.mapped := by
  change Account.composeMapped (Account.identity occsA) account = account.mapped
  unfold Account.composeMapped
  refine (List.map_congr_left fun entry hEntry => ?_).trans (List.map_id account.mapped)
  rcases entry with ⟨out, srcs⟩
  dsimp only [id]
  have hFlat : srcs.flatMap (Account.identity occsA).sourcesOf = srcs := by
    have hAll : ∀ s ∈ srcs, (Account.identity occsA).sourcesOf s = [s] := by
      intro s hs
      have hMem : s ∈ occsA :=
        hSrc out s ((account.mapsTo_iff_mem_mapped out s).mpr ⟨srcs, hEntry, hs⟩)
      rw [Account.sourcesOf_identity, ite_eq_left hMem]
    clear hEntry hSrc
    induction srcs with
    | nil => rfl
    | cons s rest ih =>
      rw [List.flatMap_cons, hAll s List.mem_cons_self, List.singleton_append,
        ih (fun s' hs' => hAll s' (List.mem_cons_of_mem s hs'))]
  rw [hFlat, eraseDups_eq_self (account.mappedSourcesNodup (out, srcs) hEntry)]

/-- `Account.identity` is an exact left unit on `eliminated` lookups whenever `occsA` covers
every source mapped or eliminated by `account`. -/
theorem Account.compose_identity_left_eliminated_lookup {occsA : List Occurrence} {account : Account}
    (hSrc : ∀ out src, account.MapsTo out src → src ∈ occsA)
    (hElim : ∀ src, account.Eliminated src → src ∈ occsA)
    (src : Occurrence) :
    AList.lookup (Account.compose (Account.identity occsA) account).eliminated src =
      AList.lookup account.eliminated src := by
  have hMappedEq : (Account.compose (Account.identity occsA) account).mapped = account.mapped :=
    Account.compose_identity_left_mapped hSrc
  cases hLook : AList.lookup account.eliminated src with
  | none =>
    cases hLookC : AList.lookup (Account.compose (Account.identity occsA) account).eliminated src with
    | none => rfl
    | some r =>
      exfalso
      have hElimC : (Account.compose (Account.identity occsA) account).Eliminated src := ⟨r, hLookC⟩
      obtain ⟨_, hElimId | ⟨hMappedId, hAll⟩⟩ :=
        (Account.eliminated_compose_iff (Account.identity occsA) account src).mp hElimC
      · obtain ⟨_, hr⟩ := hElimId; cases hr
      · obtain ⟨out, hMapsId⟩ := hMappedId
        obtain ⟨hOutIn, rfl⟩ := (Account.mapsTo_identity_iff occsA out src).mp hMapsId
        obtain ⟨r0, hr0⟩ := hAll src ((Account.mapsTo_identity_iff occsA src src).mpr ⟨hOutIn, rfl⟩)
        rw [hLook] at hr0; cases hr0
  | some reason =>
    have hSrcIn : src ∈ occsA := hElim src ⟨reason, hLook⟩
    have hNotSurv : ((Account.composeMapped (Account.identity occsA) account).any
        fun e => e.2.contains src) = false := by
      cases hAny : (Account.composeMapped (Account.identity occsA) account).any fun e => e.2.contains src
      · rfl
      · exfalso
        have hMapC : (Account.compose (Account.identity occsA) account).Mapped src :=
          (Account.mapped_iff_any (Account.compose (Account.identity occsA) account) src).mpr hAny
        obtain ⟨out, hMapsC⟩ := hMapC
        have hMapsAcc : account.MapsTo out src := by
          unfold Account.MapsTo Account.sourcesOf at hMapsC ⊢
          rwa [hMappedEq] at hMapsC
        exact account.not_eliminated_of_mapped ⟨out, hMapsAcc⟩ ⟨reason, hLook⟩
    have hImgEq : (Account.identity occsA).imagesOf src = [src] := by
      unfold Account.imagesOf Account.identity
      dsimp only
      have hNodup := nodup_eraseDups occsA
      have hMem : src ∈ occsA.eraseDups := List.mem_eraseDups.mpr hSrcIn
      generalize occsA.eraseDups = xs at hNodup hMem ⊢
      induction xs with
      | nil => cases hMem
      | cons a rest ih =>
        rw [List.nodup_cons] at hNodup
        rw [List.map_cons, List.filterMap_cons]
        rcases List.mem_cons.mp hMem with rfl | hrest
        · simp only [List.contains_cons, beq_self_eq_true, Bool.true_or, ↓reduceIte]
          congr 1
          have hNil : (rest.map fun occ => (occ, [occ])).filterMap
              (fun x => if x.2.contains src then some x.1 else none) = [] := by
            rw [List.filterMap_eq_nil_iff]
            intro entry hentry
            obtain ⟨occ, hocc, rfl⟩ := List.mem_map.mp hentry
            have hne : src ≠ occ := fun eq => hNodup.1 (eq ▸ hocc)
            simp [hne]
          exact hNil
        · have hne : src ≠ a := fun eq => hNodup.1 (eq ▸ hrest)
          simp only [List.contains_cons, beq_iff_eq, hne, List.contains_nil, Bool.or_false,
            ↓reduceIte]
          exact ih hNodup.2 hrest
    have hCand : src ∈ ((Account.identity occsA).mapped.flatMap Prod.snd).eraseDups := by
      rw [List.mem_eraseDups, List.mem_flatMap]
      exact ⟨(src, [src]), List.mem_map.mpr ⟨src, List.mem_eraseDups.mpr hSrcIn, rfl⟩,
        List.mem_singleton_self src⟩
    have hMemComp : (src, reason) ∈ (Account.compose (Account.identity occsA) account).eliminated := by
      unfold Account.compose Account.composeEliminated
      dsimp only
      apply List.mem_append_right
      refine List.mem_filterMap.mpr ⟨src, hCand, ?_⟩
      change (if ((Account.composeMapped (Account.identity occsA) account).any fun e => e.2.contains src) ||
          (AList.lookup ([] : AList Occurrence EliminationReason) src).isSome
        then none
        else match ((Account.identity occsA).imagesOf src).mapM (AList.lookup account.eliminated) with
          | some (r :: _) => some (src, r)
          | _ => none) = some (src, reason)
      rw [hNotSurv, hImgEq]
      simp only [AList.lookup, Option.isSome_none, Bool.or_self, Bool.false_eq_true, ↓reduceIte,
        List.mapM_cons, List.mapM_nil, hLook, bind, Option.bind, pure]
    exact alist_lookup_of_mem (Account.compose (Account.identity occsA) account).eliminatedKeysNodup hMemComp

/-- `Account.identity` is a left unit for `Account.compose`. -/
theorem Account.compose_identity_left {occsA : List Occurrence} {account : Account}
    (hSrc : ∀ out src, account.MapsTo out src → src ∈ occsA)
    (hElim : ∀ src, account.Eliminated src → src ∈ occsA) :
    Account.Equiv (Account.compose (Account.identity occsA) account) account := by
  constructor
  · intro out src
    unfold Account.MapsTo Account.sourcesOf
    rw [Account.compose_identity_left_mapped hSrc]
  · intro src
    unfold Account.Eliminated
    rw [Account.compose_identity_left_eliminated_lookup hSrc hElim]

theorem Account.compose_identity_left_of_valid {occsA occsB : List Occurrence} {account : Account}
    (hValid : account.Valid occsA occsB) :
    Account.Equiv (Account.compose (Account.identity occsA) account) account :=
  Account.compose_identity_left hValid.2.2.1 fun src ⟨r, hr⟩ =>
    hValid.2.2.2.1 src (List.mem_map.mpr ⟨(src, r), alist_lookup_mem hr, rfl⟩)

/-- `Account.identity` is an exact right unit on `sourcesOf` whenever `occsB` covers every
output key of `account.mapped`. -/
theorem Account.compose_identity_right_sourcesOf {occsB : List Occurrence} {account : Account}
    (hOut : ∀ out ∈ List.map Prod.fst account.mapped, out ∈ occsB) (out : Occurrence) :
    (Account.compose account (Account.identity occsB)).sourcesOf out = account.sourcesOf out := by
  rw [Account.sourcesOf_compose, Account.sourcesOf_identity]
  split
  · simp only [List.flatMap_cons, List.flatMap_nil, List.append_nil]
    exact eraseDups_eq_self (account.sourcesOf_nodup out)
  · rename_i hNot
    simp only [List.flatMap_nil, List.eraseDups_nil]
    symm
    unfold Account.sourcesOf
    cases hLook : AList.lookup account.mapped out with
    | none => rfl
    | some srcs =>
      exfalso
      exact hNot (hOut out (List.mem_map.mpr ⟨(out, srcs), alist_lookup_mem hLook, rfl⟩))

/-- `Account.identity` is an exact right unit on `eliminated` whenever `occsB` covers every
output key of `account.mapped`. -/
theorem Account.compose_identity_right_eliminated {occsB : List Occurrence} {account : Account}
    (hOut : ∀ out ∈ List.map Prod.fst account.mapped, out ∈ occsB) :
    (Account.compose account (Account.identity occsB)).eliminated = account.eliminated := by
  unfold Account.compose Account.composeEliminated
  dsimp only
  have hFromSecond :
      ((account.mapped.flatMap Prod.snd).eraseDups.filterMap fun occA =>
        if (Account.composeMapped account (Account.identity occsB)).any (fun e => e.2.contains occA) ||
            (AList.lookup (account.eliminated.filter fun e =>
              !(Account.composeMapped account (Account.identity occsB)).any fun m => m.2.contains e.1) occA).isSome
        then none
        else match (account.imagesOf occA).mapM (AList.lookup (Account.identity occsB).eliminated) with
          | some (reason :: _) => some (occA, reason)
          | _ => none) = [] := by
    rw [List.filterMap_eq_nil_iff]
    intro occA _
    split
    · rfl
    · cases hImgs : account.imagesOf occA with
      | nil => rfl
      | cons b rest =>
        simp [Account.identity, AList.lookup]
  rw [hFromSecond, List.append_nil]
  rw [List.filter_eq_self]
  intro entry hEntry
  rcases entry with ⟨src, reason⟩
  simp only [Bool.not_eq_true']
  cases hAny : (Account.composeMapped account (Account.identity occsB)).any fun e => e.2.contains src
  · rfl
  · exfalso
    have hMapC : (Account.compose account (Account.identity occsB)).Mapped src :=
      (Account.mapped_iff_any (Account.compose account (Account.identity occsB)) src).mpr hAny
    obtain ⟨out, hMapsC⟩ := hMapC
    have hMapsAcc : account.MapsTo out src := by
      unfold Account.MapsTo at hMapsC ⊢
      rwa [Account.compose_identity_right_sourcesOf hOut out] at hMapsC
    exact account.not_eliminated_of_mapped ⟨out, hMapsAcc⟩
      ⟨reason, alist_lookup_of_mem account.eliminatedKeysNodup hEntry⟩

/-- `Account.identity` is a right unit for `Account.compose`. -/
theorem Account.compose_identity_right {occsB : List Occurrence} {account : Account}
    (hOut : ∀ out ∈ List.map Prod.fst account.mapped, out ∈ occsB) :
    Account.Equiv (Account.compose account (Account.identity occsB)) account := by
  constructor
  · intro out src
    unfold Account.MapsTo
    rw [Account.compose_identity_right_sourcesOf hOut out]
  · intro src
    unfold Account.Eliminated
    rw [Account.compose_identity_right_eliminated hOut]

theorem Account.compose_identity_right_of_valid {occsA occsB : List Occurrence} {account : Account}
    (hValid : account.Valid occsA occsB) :
    Account.Equiv (Account.compose account (Account.identity occsB)) account :=
  Account.compose_identity_right hValid.1

/-- When `identity` is built on `account`'s exact output key list, right composition is
structurally equal to `account`. -/
theorem Account.compose_identity_right_eq (account : Account) :
    Account.compose account (Account.identity (List.map Prod.fst account.mapped)) = account := by
  have hOut : ∀ out ∈ List.map Prod.fst account.mapped, out ∈ List.map Prod.fst account.mapped :=
    fun _ h => h
  have hElim := Account.compose_identity_right_eliminated hOut
  have hMapped : (Account.compose account (Account.identity (List.map Prod.fst account.mapped))).mapped =
      account.mapped := by
    change Account.composeMapped account (Account.identity (List.map Prod.fst account.mapped)) = account.mapped
    unfold Account.composeMapped Account.identity
    dsimp only
    rw [eraseDups_eq_self account.mappedKeysNodup, List.map_map, List.map_map]
    refine (List.map_congr_left fun entry hEntry => ?_).trans (List.map_id account.mapped)
    rcases entry with ⟨out, srcs⟩
    dsimp only [Function.comp_def, id]
    simp only [List.flatMap_cons, List.flatMap_nil, List.append_nil]
    have hSrc : account.sourcesOf out = srcs := by
      unfold Account.sourcesOf
      rw [alist_lookup_of_mem account.mappedKeysNodup hEntry]; rfl
    rw [hSrc, eraseDups_eq_self (account.mappedSourcesNodup (out, srcs) hEntry)]
  cases hC : Account.compose account (Account.identity (List.map Prod.fst account.mapped))
  cases account
  simp only [hC] at hMapped hElim
  simp only [Account.mk.injEq]
  exact ⟨hMapped, hElim⟩

/-- Composition respects `Account.Equiv` in both arguments. -/
theorem Account.compose_congr {a₁ a₂ b₁ b₂ : Account}
    (ha : Account.Equiv a₁ a₂) (hb : Account.Equiv b₁ b₂) :
    Account.Equiv (Account.compose a₁ b₁) (Account.compose a₂ b₂) := by
  have hMaps : ∀ out src, (Account.compose a₁ b₁).MapsTo out src ↔ (Account.compose a₂ b₂).MapsTo out src := by
    intro out src
    rw [Account.mapsTo_compose_iff, Account.mapsTo_compose_iff]
    constructor
    · rintro ⟨mid, h2, h1⟩; exact ⟨mid, (hb.1 out mid).mp h2, (ha.1 mid src).mp h1⟩
    · rintro ⟨mid, h2, h1⟩; exact ⟨mid, (hb.1 out mid).mpr h2, (ha.1 mid src).mpr h1⟩
  refine ⟨hMaps, fun src => ?_⟩
  rw [Account.eliminated_compose_iff, Account.eliminated_compose_iff]
  have hMapC : (Account.compose a₁ b₁).Mapped src ↔ (Account.compose a₂ b₂).Mapped src :=
    ⟨fun ⟨o, h⟩ => ⟨o, (hMaps o src).mp h⟩, fun ⟨o, h⟩ => ⟨o, (hMaps o src).mpr h⟩⟩
  have hMapA : a₁.Mapped src ↔ a₂.Mapped src :=
    ⟨fun ⟨o, h⟩ => ⟨o, (ha.1 o src).mp h⟩, fun ⟨o, h⟩ => ⟨o, (ha.1 o src).mpr h⟩⟩
  constructor
  · rintro ⟨hNot, hElim | ⟨hM, hAll⟩⟩
    · exact ⟨fun h => hNot (hMapC.mpr h), Or.inl ((ha.2 src).mp hElim)⟩
    · exact ⟨fun h => hNot (hMapC.mpr h), Or.inr ⟨hMapA.mp hM,
        fun occB hB => (hb.2 occB).mp (hAll occB ((ha.1 occB src).mpr hB))⟩⟩
  · rintro ⟨hNot, hElim | ⟨hM, hAll⟩⟩
    · exact ⟨fun h => hNot (hMapC.mp h), Or.inl ((ha.2 src).mpr hElim)⟩
    · exact ⟨fun h => hNot (hMapC.mp h), Or.inr ⟨hMapA.mpr hM,
        fun occB hB => (hb.2 occB).mpr (hAll occB ((ha.1 occB src).mp hB))⟩⟩

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

/-- One structured LLVM IR module carrying its body items.
The target datalayout and triple header is derived exclusively from the resolved
`Profile` inside `LLVMInput.bytesAt`; the structured body has no target directive
or raw-text escape hatch, so a mismatched header is unrepresentable. -/
structure LLVMModule where
  items : List Backend.LLVM.Item
  deriving BEq

/-- Two-module LLVM handoff: structured component and launcher modules plus the
exact trap table shared between them. -/
structure LLVMInput (_profile : Profile) where
  component : LLVMModule
  launcher : LLVMModule
  sites : TrapTable

/-- Select the structured LLVM module for `role`. -/
def LLVMInput.moduleAt {profile : Profile} (input : LLVMInput profile) : LLVMRole → LLVMModule
  | .component => input.component
  | .launcher => input.launcher

/-- Render the exact bytes passed to the LLVM toolchain for the selected role,
rendering the target header from the resolved `profile`'s data layout and triple.
The structured body has no target directive or raw-text escape hatch. -/
def LLVMInput.bytesAt {profile : Profile} (input : LLVMInput profile) (role : LLVMRole) : ByteArray :=
  (s!"target datalayout = \"{profile.llvmDataLayout}\"\ntarget triple = \"{profile.llvmTriple}\"\n" ++
    Backend.LLVM.render (input.moduleAt role).items).toUTF8

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

theorem LLVMInput.moduleAt_mk {profile : Profile} (component launcher : LLVMModule)
    (sites : TrapTable) (role : LLVMRole) :
    ({ component, launcher, sites } : LLVMInput profile).moduleAt role =
      match role with | .component => component | .launcher => launcher := by
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
