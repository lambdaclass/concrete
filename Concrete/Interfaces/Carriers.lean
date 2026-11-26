import ConcreteSpec.StageInterface
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

/-- Each supported platform has one ISA and one spec memory target. All three hosted
platforms currently share the 64-bit little-endian memory layout. -/
structure Profile where
  backend : BackendKind
  backendTarget : Backend.LCB.Target
  maxBIRSlots : Nat
  maxImageStackBytes : Nat

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

inductive LexRefusal where
  | malformed (rule : RuleId) (atSpan : FileSpan)
  | resource (failure : ResourceRefusal)

structure ElabRefusal where
  rule : RuleId
  primary : FileSpan
  related : List (ConcreteSpec.Name × FileSpan)

inductive StageFailure where
  | resource (failure : ResourceRefusal)
  | unsupportedProfile (rule : RuleId)
  | invalidInput (rule : RuleId)

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
  | .ret _ | .trap _ _ => []

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
      (checks : List OperationCheck)
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
  | beginLifetime (place : PlaceId)
  | endLifetime (place : PlaceId)
  | destructure {ty : Ty} (src : MemoryPlace Ty ty)
      (bindings : List (FieldId × Sigma (MemoryPlace Ty)))
  | call (callee : FnId) (dst : Option (Sigma (MemoryPlace Ty)))
      (args : List (Sigma (MemoryRead Ty lang.literal)))

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

structure MemoryState (Value : Type) (profile : Profile) where
  frame : FnId
  block : BlockId
  instruction : Nat
  virtualValues : AList PlaceId Value
  materialized : AList PlaceId Hybrid.Range
  live : List PlaceId
  initialized : List PlaceId
  bytes : Mem.Memory
  /-- Whether the current activation has a caller, including recursive calls to the entry. -/
  hasCaller : Bool := false

def MemoryState.BytesWF {Value : Type} {profile : Profile} (state : MemoryState Value profile) :
    Prop :=
  state.bytes.WellFormed profile.memoryTarget ∧
    state.live.Nodup ∧ state.initialized.Nodup ∧
    ∀ place ∈ state.initialized, place ∈ state.live

structure SSAValue (Ty : Type) (_ty : Ty) where
  id : Nat

inductive SSARead (Ty : Type) (Literal : Ty → Type) : Ty → Type where
  | value {ty : Ty} (value : SSAValue Ty ty) : SSARead Ty Literal ty
  | literal {ty : Ty} (value : Literal ty) : SSARead Ty Literal ty

/-- SSA only renames virtual values. Materialized locations retain their addressable identity. -/
inductive SSAInst {Ty : Type} (lang : LoweringLanguage Ty) where
  | op {opcode : Operator} {types : List Ty} {ty : Ty} (dst : SSAValue Ty ty) (op : lang.operation opcode types ty)
      (args : Args (SSARead Ty lang.literal) types)
  | load {ty : Ty} (dst : SSAValue Ty ty) (src : Place Ty ty)
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

structure TrapIndex where
  index : Nat
  deriving DecidableEq, Repr

structure TrapEntry where
  kind : TrapKind
  origin : Origin

abbrev TrapTable := Array TrapEntry

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

/-- The register frame allocates a slot per definition, including block parameters.
The ceiling is per activation, so the program uses the largest function frame. -/
def BIR.frameSlots {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) : Nat :=
  bir.functions.foldl (fun largest function =>
    max largest (function.2.2.blocks.foldl (fun slots block =>
      block.body.foldl (fun count inst => match inst.instruction with
        | .op .. | .load .. => count + 1
        | .call _ (some _) _ => count + 1
        | _ => count) (slots + block.params.length)) 0)) 0

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

structure LLVMInput (_profile : Profile) where
  triple : String
  dataLayout : String
  items : List Backend.LLVM.Item
  sites : TrapTable

/-- The bytes passed to the toolchain are rendered from the checked fields.
The structured body has no target directive or raw-text escape hatch. -/
def LLVMInput.moduleBytes {profile : Profile} (input : LLVMInput profile) : ByteArray :=
  (s!"target datalayout = \"{input.dataLayout}\"\ntarget triple = \"{input.triple}\"\n" ++
    Backend.LLVM.render input.items).toUTF8

structure LCBInput (_profile : Profile) where
  body : LeanCompilerBackend.Verified.Source.Function
  additionalJobs : List LeanCompilerBackend.Verified.FunctionJob
  sites : TrapTable
  runtimeInput : ByteArray

end Concrete.Interfaces
