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

-- Every backend policy rule is registered at revision 1; higher revisions and
-- unrelated rule names are rejected.
example :
    (validateRule intWidthRule).isSome = true ∧
    (validateRule operationRule).isSome = true ∧
    (validateRule storageFormRule).isSome = true ∧
    (validateRule callRule).isSome = true ∧
    (validateRule ⟨``IntWidthSupported, 2⟩).isNone = true ∧
    (validateRule ⟨``OperationSupported, 2⟩).isNone = true ∧
    (validateRule ⟨``StorageFormSupported, 2⟩).isNone = true ∧
    (validateRule ⟨``CallSupported, 2⟩).isNone = true := by
  decide

-- Width policy: LLVM supports all 8 integer widths; LCB supports widths up to 16 bits
-- and refuses 32- and 64-bit widths with the exact registered width rule.
example :
    IntTy.all.all (fun ty => supportsIntWidth (Profile.llvm .x86_64Linux) ty) = true ∧
    [.i8, .u8, .i16, .u16].all (fun ty => supportsIntWidth (Profile.lcb .aarch64Macos) ty) = true ∧
    [.i32, .u32, .i64, .u64].all (fun ty => !supportsIntWidth (Profile.lcb .aarch64Macos) ty) = true ∧
    checkFeature (Profile.llvm .x86_64Linux) (.intWidth .u32) = .ok () ∧
    checkFeature (Profile.lcb .x86_64Linux) (.intWidth .u32) = .error intWidthRule ∧
    checkFeature (Profile.lcb .x86_64Linux) (.intWidth .u32) ≠ .error operationRule := by
  decide

-- Operation, storage/lifetime/destructure, and call/linking policies map each
-- unsupported construct to its single owning rule.
example :
    checkFeature (Profile.lcb .x86_64Linux) (.operation .addWithOverflow) = .ok () ∧
    checkFeature (Profile.lcb .x86_64Linux) (.operation (.arithmetic (.plain .div))) = .error operationRule ∧
    checkFeature (Profile.llvm .aarch64Linux) (.operation (.named ⟨"unknown"⟩)) = .error operationRule ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .registerOnly) = .ok () ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .addressOperand) = .error storageFormRule ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .load) = .error storageFormRule ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .store) = .error storageFormRule ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .beginLifetime) = .error storageFormRule ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .endLifetime) = .error storageFormRule ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .destructureRegister) = .error storageFormRule ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .destructureMaterialized) = .error storageFormRule ∧
    checkFeature (Profile.lcb .aarch64Macos) (.call .noReachableCall) = .ok () ∧
    checkFeature (Profile.lcb .aarch64Macos) (.call .unreachableCall) = .ok () ∧
    checkFeature (Profile.lcb .aarch64Macos) (.call .reachableInternalCall) = .error callRule ∧
    checkFeature (Profile.llvm .x86_64Linux) (.call .foreignCall) = .error callRule := by
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
