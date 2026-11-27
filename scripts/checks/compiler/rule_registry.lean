import ConcreteSpec.Spec.Slice0Layout
import Concrete.Refusal

open Concrete ConcreteSpec.Core
open ConcreteSpec.Project
open Slice0Layout

private def refusal : Refusal := .checker "unreachable code"
example : ((refusal.diagnostic ``Slice0Rules.ReachableBody).map (fun d => d.rule.val.name)) =
    some ``Slice0Rules.ReachableBody := by decide
example : (refusal.diagnostic `unregistered).isNone = true := by decide
example : (refusal.diagnosticAt ⟨`unregistered, 1⟩).isNone = true := by decide
example : (refusal.diagnosticAt ⟨``Slice0Rules.ReachableBody, 2⟩).isNone = true := by decide
example : (refusal.diagnosticAt ⟨``Slice0Rules.ReachableBody, 1⟩).isSome = true := by decide

example : (refusal.diagnostic ``Slice0Rules.Lexed).isNone = true := by decide
example : (refusal.diagnosticAt ⟨``Slice0Rules.Lexed, 2⟩).isNone = true := by decide

example : ((Refusal.syntaxError "invalid source").diagnostic ``Slice0Rules.Lexed).isSome =
    true := by decide

example : ((Refusal.syntaxError "invalid source").diagnosticAt ⟨``Slice0Rules.Lexed, 1⟩).isNone =
    true := by decide
example : (refusal.diagnosticAt ⟨``Slice0Rules.UnreservedLocal, 2⟩).isSome = true := by decide
example : (refusal.diagnosticAt ⟨``Slice0Rules.UnreservedLocal, 1⟩).isNone = true := by decide

example : ((Refusal.lexerDigitLimit 0 1024).diagnostic ``Slice0Rules.WithinLimit).isSome =
    true := by decide
example : ((Refusal.lexerDigitLimit 0 1024).diagnostic ``Slice0Rules.Lexed).isNone =
    true := by decide

example : ((Refusal.manifest .size).diagnosticAt Manifest.sizeRule).isSome = true :=
  by decide
example : ((Refusal.manifest .size).diagnostic ``Manifest.SizeAccepted).isSome = true :=
  by decide
example : ((Refusal.manifest .size).diagnosticAt {Manifest.sizeRule with rev := 2}).isNone =
    true := by decide
example : ((Refusal.manifest .size).diagnosticAt Manifest.depthRule).isNone = true :=
  by decide
example : ((Refusal.manifest .schemaPending).diagnosticAt Manifest.sizeRule).isNone = true :=
  by decide

-- A literal-range or generic admission example cannot stand in for OWN-5.
example : ((rulesForSlice ⟨0⟩).getD []).any
    (fun rule => rule.val.name == `ConcreteSpec.Core.Slice0Rules.LocalsUsed) = true := by decide

private def storageFacts (profile : Profile) (ty : IntTy) : Option (Nat × Nat × ByteOrder) :=
  match query profile (.integer ty) with
  | some (.stored storage) => some (storage.byteSize, storage.align, storage.byteOrder)
  | _ => none

example : [.i8, .u8, .i16, .u16, .i32, .u32, .i64, .u64].map
    (storageFacts linuxX8664Profile) =
    [some (1, 1, .little), some (1, 1, .little),
     some (2, 2, .little), some (2, 2, .little),
     some (4, 4, .little), some (4, 4, .little),
     some (8, 8, .little), some (8, 8, .little)] := rfl

-- A valid small profile with distinct byte order and ABI alignment.
private def smallProfile : Profile where
  target := { linuxX8664 with
    addrBits := 16
    ptrSize := 2
    ptrAlign := 2
    maxObject := 4
    byteOrder := .big }
  wellFormed := by decide
  intAlign := fun ty => min 2 ty.byteSize
  intAlignPow2 := by intro ty; cases ty <;> decide
  intSizeAligned := by intro ty; cases ty <;> decide

example : storageFacts smallProfile .u32 = some (4, 2, .big) := rfl
example : query smallProfile (.integer .u64) = none := rfl
example : query smallProfile .logical = some .logical := rfl
example : (query smallProfile .logical).map Layout.byteSize = some 0 := rfl
