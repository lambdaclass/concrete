import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Scalar

/-!
# Backend profile policy rules

Pure specification predicates for backend profile support judgments: integer widths,
operations, storage/lifetime/destructure forms, and call/linking support. This module
depends only on specification vocabularies and imports no compiler implementation.
Each registered rule has a concrete, decidable judgment and decision provenance.
-/

namespace ConcreteSpec.Core.BackendPolicy

/-- The backend family selected by a compilation profile. -/
inductive Backend where
  | llvm
  | lcb
  deriving DecidableEq, Repr

/-- Hosted platforms supported by the current backend profiles. -/
inductive Platform where
  | x86_64Linux
  | aarch64Linux
  | aarch64Macos
  deriving DecidableEq, Repr

/-- Read-only backend policy selector (backend family and hosted platform). -/
structure Profile where
  backend : Backend
  platform : Platform
  deriving DecidableEq, Repr

def Profile.llvm (platform : Platform) : Profile := ⟨.llvm, platform⟩
def Profile.lcb (platform : Platform) : Profile := ⟨.lcb, platform⟩

/-- Operation forms that can appear in BIR instructions. Integer operand widths
are judged separately by `IntWidthSupported`. -/
inductive Operation where
  | read
  | addWithOverflow
  | arithmetic (op : MachineOp)
  | checkedNeg
  | bitwise (op : BitOp)
  | bitNot
  | compare (op : CmpOp)
  | intConvert
  | named (operation : Name)
  deriving DecidableEq, Repr

/-- Storage, address, lifetime and aggregate-destructure forms in BIR. -/
inductive StorageForm where
  | registerOnly
  | addressOperand
  | load
  | store
  | beginLifetime
  | endLifetime
  | destructureRegister
  | destructureMaterialized
  deriving DecidableEq, Repr

/-- Call and linking forms in BIR. A call inside a block unreachable from the
entry does not require cross-job linking on entry execution. -/
inductive CallForm where
  | noReachableCall
  | unreachableCall
  | reachableInternalCall
  | foreignCall
  deriving DecidableEq, Repr

/-- Closed backend feature query, dispatched to its single owning policy rule. -/
inductive Feature where
  | intWidth (ty : IntTy)
  | operation (op : Operation)
  | storage (form : StorageForm)
  | call (form : CallForm)
  deriving DecidableEq, Repr

/-- Executable backend integer-width policy. LLVM covers all fixed-width Core
integer types (8 to 64 bits); the slice-0 LCB profile covers widths up to 16 bits
so exact binary results fit in a 32-bit LCB word. -/
def supportsIntWidth (profile : Profile) (ty : IntTy) : Bool :=
  match profile.backend with
  | .llvm => true
  | .lcb => decide (ty.bits ≤ 16)

/-- Integer width policy for the selected backend profile: LLVM supports all
fixed-width Core integer types (8 to 64 bits), while the slice-0 LCB profile
supports widths up to 16 bits so exact binary results fit in a 32-bit word.
Decision: Q8.1, R-0581, R-0579, docs/decisions.md#decided-project-proof-infrastructure,
docs/decisions.md#decided-2026-11-23-slice0-and-roadmap-answers. -/
def IntWidthSupported (input : Profile × IntTy) : Prop :=
  supportsIntWidth input.1 input.2 = true

instance (input : Profile × IntTy) : Decidable (IntWidthSupported input) := by
  unfold IntWidthSupported
  infer_instance

def intWidthRule : RuleId := ⟨``IntWidthSupported, 1⟩

/-- Executable backend operation policy for the current slice-0 BIR surface:
value reads, checked addition with overflow, plain and wrapping add/sub/mul,
and integer width conversions are supported; remaining operations require later
backend increments. -/
def supportsOperation (profile : Profile) (op : Operation) : Bool :=
  match profile.backend, op with
  | _, .read
  | _, .addWithOverflow
  | _, .intConvert
  | _, .arithmetic (.plain .add)
  | _, .arithmetic (.plain .sub)
  | _, .arithmetic (.plain .mul)
  | _, .arithmetic (.wrapping _) => true
  | _, _ => false

/-- Operation support policy for the selected backend profile: admits value reads,
checked addition with overflow, plain and wrapping add/sub/mul, and integer width
conversions; refuses unimplemented arithmetic, bitwise, comparison and named
operations until their backend lowering lands.
Decision: Q8.1, R-0581, R-0579, docs/decisions.md#decided-project-proof-infrastructure. -/
def OperationSupported (input : Profile × Operation) : Prop :=
  supportsOperation input.1 input.2 = true

instance (input : Profile × Operation) : Decidable (OperationSupported input) := by
  unfold OperationSupported
  infer_instance

def operationRule : RuleId := ⟨``OperationSupported, 1⟩

/-- Executable backend storage, lifetime and destructure policy: the slice-0
backends admit register-only SSA values and refuse materialized memory operations,
address operands, explicit lifetime instructions and aggregate destructuring. -/
def supportsStorageForm (profile : Profile) (form : StorageForm) : Bool :=
  match profile.backend, form with
  | _, .registerOnly => true
  | _, _ => false

/-- Storage, lifetime and destructure policy for the selected backend profile:
slice-0 backends admit register-only SSA values and refuse materialized loads,
stores, address operands, explicit lifetime instructions and aggregate
destructuring until memory-backed backend lowering lands.
Decision: Q8.1, R-0581, R-0580, docs/decisions.md#decided-project-proof-infrastructure. -/
def StorageFormSupported (input : Profile × StorageForm) : Prop :=
  supportsStorageForm input.1 input.2 = true

instance (input : Profile × StorageForm) : Decidable (StorageFormSupported input) := by
  unfold StorageFormSupported
  infer_instance

def storageFormRule : RuleId := ⟨``StorageFormSupported, 1⟩

/-- Executable backend call and linking policy: functions with no entry-reachable
call (including calls confined to unreachable blocks) are admitted; entry-reachable
internal calls and foreign calls are refused until linked multi-function and FFI
lowering land. -/
def supportsCall (profile : Profile) (form : CallForm) : Bool :=
  match profile.backend, form with
  | _, .noReachableCall
  | _, .unreachableCall => true
  | _, .reachableInternalCall
  | _, .foreignCall => false

/-- Call and linking policy for the selected backend profile: admits programs
without entry-reachable calls (including disconnected unreachable helper calls)
and refuses entry-reachable internal calls and foreign calls until linked
multi-function semantics and FFI lowering land.
Decision: Q8.1, R-0581, R-0579, docs/decisions.md#decided-project-proof-infrastructure. -/
def CallSupported (input : Profile × CallForm) : Prop :=
  supportsCall input.1 input.2 = true

instance (input : Profile × CallForm) : Decidable (CallSupported input) := by
  unfold CallSupported
  infer_instance

def callRule : RuleId := ⟨``CallSupported, 1⟩

/-- Every backend policy rule registered in `ConcreteSpec.Core.registeredRules`. -/
def rules : List RuleId :=
  [intWidthRule, operationRule, storageFormRule, callRule]

/-- Each backend feature maps to its single owning policy rule identity. -/
def Feature.ruleId : Feature → RuleId
  | .intWidth _ => intWidthRule
  | .operation _ => operationRule
  | .storage _ => storageFormRule
  | .call _ => callRule

/-- Unified backend policy judgment delegating to each feature's owning rule. -/
def FeatureSupported (profile : Profile) : Feature → Prop
  | .intWidth ty => IntWidthSupported (profile, ty)
  | .operation op => OperationSupported (profile, op)
  | .storage form => StorageFormSupported (profile, form)
  | .call form => CallSupported (profile, form)

/-- Executable unified backend feature predicate matching `FeatureSupported`. -/
def supportsFeature (profile : Profile) : Feature → Bool
  | .intWidth ty => supportsIntWidth profile ty
  | .operation op => supportsOperation profile op
  | .storage form => supportsStorageForm profile form
  | .call form => supportsCall profile form

/-- Executable backend feature check returning the exact owning `RuleId` on refusal. -/
def checkFeature (profile : Profile) (feature : Feature) : Except RuleId Unit :=
  if supportsFeature profile feature then .ok () else .error feature.ruleId

theorem supportsIntWidth_llvm (platform : Platform) (ty : IntTy) :
    IntWidthSupported (Profile.llvm platform, ty) := rfl

theorem supportsIntWidth_lcb_iff (platform : Platform) (ty : IntTy) :
    IntWidthSupported (Profile.lcb platform, ty) ↔ ty.bits ≤ 16 := by
  simp [IntWidthSupported, supportsIntWidth, Profile.lcb]

theorem featureRule_mem (feature : Feature) : feature.ruleId ∈ rules := by
  cases feature <;> simp [Feature.ruleId, rules]

theorem supportsFeature_eq_true_iff (profile : Profile) (feature : Feature) :
    supportsFeature profile feature = true ↔ FeatureSupported profile feature := by
  cases feature <;> rfl

theorem checkFeature_ok_iff (profile : Profile) (feature : Feature) :
    checkFeature profile feature = .ok () ↔ FeatureSupported profile feature := by
  unfold checkFeature
  cases h : supportsFeature profile feature <;>
    simp [← supportsFeature_eq_true_iff, h]

theorem checkFeature_error_iff (profile : Profile) (feature : Feature) (rule : RuleId) :
    checkFeature profile feature = .error rule ↔
      ¬ FeatureSupported profile feature ∧ rule = feature.ruleId := by
  unfold checkFeature
  cases h : supportsFeature profile feature
  · simp [← supportsFeature_eq_true_iff, h]
    exact eq_comm
  · simp [← supportsFeature_eq_true_iff, h]

end ConcreteSpec.Core.BackendPolicy
