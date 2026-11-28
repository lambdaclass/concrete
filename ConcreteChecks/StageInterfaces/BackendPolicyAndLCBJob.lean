import ConcreteSpec.Spec.Rules
import Concrete.Backend.LCBJob
import Concrete.Backend.LCB

/-!
Focused controls for pre-freeze Round 1a (backend policy rules) and Round 1b
(pure LCB job leaf).
-/

namespace StageInterfaceControls.PreFreeze

open ConcreteSpec.Core
open ConcreteSpec.Core.BackendPolicy
open Concrete.Backend

-- Every backend policy rule is registered at its literal declaration name and
-- literal revision 1; any other revision (0, 2, 3, or any rev ≠ 1) and
-- unregistered rule names are rejected.
example :
    BackendPolicy.rules =
      [⟨`ConcreteSpec.Core.BackendPolicy.IntWidthSupported, 1⟩,
       ⟨`ConcreteSpec.Core.BackendPolicy.OperationSupported, 1⟩,
       ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 1⟩,
       ⟨`ConcreteSpec.Core.BackendPolicy.CallSupported, 1⟩] ∧
    (registeredRules.filter (·.name == `ConcreteSpec.Core.BackendPolicy.IntWidthSupported)).map (·.rev) = [1] ∧
    (registeredRules.filter (·.name == `ConcreteSpec.Core.BackendPolicy.OperationSupported)).map (·.rev) = [1] ∧
    (registeredRules.filter (·.name == `ConcreteSpec.Core.BackendPolicy.StorageFormSupported)).map (·.rev) = [1] ∧
    (registeredRules.filter (·.name == `ConcreteSpec.Core.BackendPolicy.CallSupported)).map (·.rev) = [1] ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.IntWidthSupported, 1⟩).isSome = true ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.OperationSupported, 1⟩).isSome = true ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 1⟩).isSome = true ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.CallSupported, 1⟩).isSome = true ∧
    ([0, 2, 3].all fun rev =>
      (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.IntWidthSupported, rev⟩).isNone &&
      (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.OperationSupported, rev⟩).isNone &&
      (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, rev⟩).isNone &&
      (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.CallSupported, rev⟩).isNone) = true ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.Unregistered, 1⟩).isNone = true := by
  decide

-- Width policy: LLVM supports all 8 integer widths; LCB supports widths up to 16 bits
-- and refuses 32- and 64-bit widths with the exact literal declaration name and revision 1.
example :
    IntTy.all.all (fun ty => supportsIntWidth (Profile.llvm .x86_64Linux) ty) = true ∧
    [.i8, .u8, .i16, .u16].all (fun ty => supportsIntWidth (Profile.lcb .aarch64Macos) ty) = true ∧
    [.i32, .u32, .i64, .u64].all (fun ty => !supportsIntWidth (Profile.lcb .aarch64Macos) ty) = true ∧
    checkFeature (Profile.llvm .x86_64Linux) (.intWidth .u32) = .ok () ∧
    checkFeature (Profile.lcb .x86_64Linux) (.intWidth .u32) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.IntWidthSupported, 1⟩ ∧
    checkFeature (Profile.lcb .aarch64Macos) (.intWidth .i64) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.IntWidthSupported, 1⟩ ∧
    checkFeature (Profile.lcb .x86_64Linux) (.intWidth .u32) ≠
      .error ⟨`ConcreteSpec.Core.BackendPolicy.IntWidthSupported, 3⟩ ∧
    checkFeature (Profile.lcb .x86_64Linux) (.intWidth .u32) ≠
      .error ⟨`ConcreteSpec.Core.BackendPolicy.OperationSupported, 1⟩ := by
  decide

-- Operation, storage/lifetime/destructure, and call/linking policies map each
-- unsupported construct to its single literal owning rule at revision 1.
example :
    checkFeature (Profile.lcb .x86_64Linux) (.operation .addWithOverflow) = .ok () ∧
    checkFeature (Profile.lcb .x86_64Linux) (.operation (.arithmetic (.plain .div))) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.OperationSupported, 1⟩ ∧
    checkFeature (Profile.llvm .aarch64Linux) (.operation (.named ⟨"unknown"⟩)) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.OperationSupported, 1⟩ ∧
    checkFeature (Profile.lcb .x86_64Linux) (.operation (.arithmetic (.plain .div))) ≠
      .error ⟨`ConcreteSpec.Core.BackendPolicy.OperationSupported, 3⟩ ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .registerOnly) = .ok () ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .addressOperand) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 1⟩ ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .load) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 1⟩ ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .store) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 1⟩ ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .beginLifetime) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 1⟩ ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .endLifetime) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 1⟩ ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .destructureRegister) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 1⟩ ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .destructureMaterialized) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 1⟩ ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .load) ≠
      .error ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 3⟩ ∧
    checkFeature (Profile.lcb .aarch64Macos) (.call .noReachableCall) = .ok () ∧
    checkFeature (Profile.lcb .aarch64Macos) (.call .unreachableCall) = .ok () ∧
    checkFeature (Profile.lcb .aarch64Macos) (.call .reachableInternalCall) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.CallSupported, 1⟩ ∧
    checkFeature (Profile.llvm .x86_64Linux) (.call .foreignCall) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.CallSupported, 1⟩ ∧
    checkFeature (Profile.lcb .aarch64Macos) (.call .reachableInternalCall) ≠
      .error ⟨`ConcreteSpec.Core.BackendPolicy.CallSupported, 3⟩ := by
  decide

-- Pure LCB job leaf: symbol and job constructor are definitionally equal to LCB's
-- re-exports, validate a legal body, and reject mutated job headers.
private def trivialBody : LeanCompilerBackend.Verified.Source.Function :=
  { resultWidth := .word, parameters := #[], valueWidths := #[],
    blocks := #[{ phis := #[], instructions := #[], terminator := .ret (.literal (.word (BitVec.ofInt 32 42))) }] }

example :
    LCBJob.bodySymbol = LCB.bodySymbol ∧
    LCBJob.bodySymbol = { namespaceName := "concrete", name := "concrete_body", exported := true } ∧
    LCBJob.lcbFunctionJob trivialBody = LCB.lcbFunctionJob trivialBody ∧
    LCBJob.lcbFunctionJob trivialBody = LCB.job trivialBody ∧
    (LeanCompilerBackend.Verified.validateJob (LCBJob.lcbFunctionJob trivialBody)).isOk = true ∧
    (LeanCompilerBackend.Verified.validateJob
      { LCBJob.lcbFunctionJob trivialBody with
        code := { (LCBJob.lcbFunctionJob trivialBody).code with version := 0 } }).isOk = false := by
  with_unfolding_all exact ⟨rfl, rfl, rfl, rfl, rfl, rfl⟩

end StageInterfaceControls.PreFreeze
