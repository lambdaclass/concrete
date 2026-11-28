import ConcreteSpec.Spec.Slice0Layout
import Concrete.Stages

open Concrete ConcreteSpec.Core
open ConcreteSpec.Project
open Slice0Layout

private def span : ConcreteSpec.Surface0.Span := ⟨0, 1, 1, 1, ⟨0⟩⟩
private def refusal : Refusal := .source (.surface (.unreachable span))
example : ((refusal.diagnostic ``Slice0Rules.ReachableBody).map (fun d => d.rule.val.name)) =
    some ``Slice0Rules.ReachableBody := by decide
example : (refusal.diagnostic `unregistered).isNone = true := by decide
example : (refusal.diagnosticAt ⟨`unregistered, 1⟩).isNone = true := by decide
example : (refusal.diagnosticAt ⟨``Slice0Rules.ReachableBody, 2⟩).isNone = true := by decide
example : (refusal.diagnosticAt ⟨``Slice0Rules.ReachableBody, 1⟩).isSome = true := by decide

example : (refusal.diagnostic ``Slice0Rules.Lexed).isNone = true := by decide
example : (refusal.diagnosticAt ⟨``Slice0Rules.Lexed, 2⟩).isNone = true := by decide

example : ((Refusal.source (.lexical (.encoding .nul) span)).diagnostic ``Slice0Rules.Lexed).isSome =
    true := by decide

example : ((Refusal.source (.lexical (.encoding .nul) span)).diagnosticAt ⟨``Slice0Rules.Lexed, 1⟩).isNone =
    true := by decide
example : (refusal.diagnosticAt ⟨``Slice0Rules.UnreservedLocal, 2⟩).isNone = true := by decide
example : (refusal.diagnosticAt ⟨``Slice0Rules.UnreservedLocal, 1⟩).isNone = true := by decide

example : ((Refusal.source (.lexical (.digitLimit 1024) span)).diagnostic ``Slice0Rules.WithinLimit).isSome =
    true := by decide
example : ((Refusal.source (.lexical (.digitLimit 1024) span)).diagnostic ``Slice0Rules.Lexed).isNone =
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

-- The changed judgments have new live revisions. Older diagnostics remain identifiable.
example : Slice0Rules.AdmittedProduction (.decls, []) ∧
    ¬ Manifest.DepthAccepted "[a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p]\nx = \"oops" := by
  cbv <;> decide

example : (validateRule ⟨``Slice0Rules.AdmittedProduction, 3⟩).isSome ∧
    (validateRule ⟨``Slice0Rules.AdmittedProduction, 2⟩).isNone := by decide
example : (validateRule ⟨``Manifest.DepthAccepted, 2⟩).isSome ∧
    (validateRule ⟨``Manifest.DepthAccepted, 1⟩).isNone := by decide
example : ⟨``Slice0Rules.AdmittedProduction, 2⟩ ∈ retiredRuleIds ∧
    ⟨``Manifest.DepthAccepted, 1⟩ ∈ retiredRuleIds := by decide

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

-- The location belongs to the original path bytes, including an empty boundary.
private def pathWitness (path : String) (rule : RuleId) : Option (Nat × Nat × Bool) := do
  let .error (failure@(.path r)) := Concrete.Stages.Input.parsePath path | none
  let loc := r.violation.location
  return (loc.offset, loc.stop, (failure.diagnosticAt rule).isSome)

example : pathWitness "a b.con" ⟨``SourcePath.NoSpaces, 1⟩ = some (1, 7, true) ∧
    pathWitness "a b.con" ⟨``SourcePath.NoReservedStem, 1⟩ = some (1, 7, false) ∧
    pathWitness "con.con" ⟨``SourcePath.NoReservedStem, 1⟩ = some (0, 7, true) ∧
    pathWitness "" ⟨``SourcePath.Nonempty, 1⟩ = some (0, 0, true) := by
  with_unfolding_all exact ⟨rfl, rfl, rfl, rfl⟩

-- Backend policy rules are pinned to their literal declaration names and literal
-- revision 1, never compared against the constants supplying the IDs.
open BackendPolicy in
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
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.IntWidthSupported, 1⟩).isSome ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.IntWidthSupported, 2⟩).isNone ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.IntWidthSupported, 3⟩).isNone ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.OperationSupported, 1⟩).isSome ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.OperationSupported, 2⟩).isNone ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.OperationSupported, 3⟩).isNone ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 1⟩).isSome ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 2⟩).isNone ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 3⟩).isNone ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.CallSupported, 1⟩).isSome ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.CallSupported, 2⟩).isNone ∧
    (validateRule ⟨`ConcreteSpec.Core.BackendPolicy.CallSupported, 3⟩).isNone ∧
    checkFeature (Profile.lcb .x86_64Linux) (.intWidth .u8) = .ok () ∧
    checkFeature (Profile.llvm .x86_64Linux) (.intWidth .u32) = .ok () ∧
    checkFeature (Profile.lcb .x86_64Linux) (.intWidth .u32) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.IntWidthSupported, 1⟩ ∧
    checkFeature (Profile.lcb .x86_64Linux) (.operation .addWithOverflow) = .ok () ∧
    checkFeature (Profile.lcb .x86_64Linux) (.operation (.arithmetic (.plain .div))) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.OperationSupported, 1⟩ ∧
    checkFeature (Profile.lcb .x86_64Linux) (.storage .registerOnly) = .ok () ∧
    checkFeature (Profile.llvm .x86_64Linux) (.storage .load) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.StorageFormSupported, 1⟩ ∧
    checkFeature (Profile.lcb .aarch64Macos) (.call .unreachableCall) = .ok () ∧
    checkFeature (Profile.lcb .aarch64Macos) (.call .reachableInternalCall) =
      .error ⟨`ConcreteSpec.Core.BackendPolicy.CallSupported, 1⟩ := by
  decide

-- Source encoding controls: CR / CRLF is valid whitespace on both routes, whereas
-- raw NUL, raw BOM (U+FEFF) and invalid UTF-8 fail with Slice0Rules.Lexed@2.
private def crlfSource : ByteArray := "fn main() -> u8 {\r\n  return 42;\r\n}\r\n".toUTF8
private def bomSource : ByteArray := "\uFEFFfn main() -> u8 { return 42; }".toUTF8
private def nulSource : ByteArray := ByteArray.mk (#[0] ++ "fn main() -> u8 { return 42; }".toUTF8.data)
private def badUtf8Source : ByteArray := ByteArray.mk (#[0xFF] ++ "fn main() -> u8 { return 42; }".toUTF8.data)

example :
    ((Concrete.Stages.Input.load "main.con" crlfSource).bind Concrete.Stages.compile).isOk = true ∧
    ((Concrete.Stages.Input.load "main.con" crlfSource).bind Concrete.Stages.unchecked).isOk = true ∧
    ((Concrete.Stages.Input.load "main.con" bomSource).bind Concrete.Stages.compile) =
      .error (.source (.lexical (.encoding .bom) ⟨0, 1, 1, 1, ⟨0⟩⟩)) ∧
    ((Concrete.Stages.Input.load "main.con" nulSource).bind Concrete.Stages.compile) =
      .error (.source (.lexical (.encoding .nul) ⟨0, 1, 1, 1, ⟨0⟩⟩)) ∧
    ((Concrete.Stages.Input.load "main.con" badUtf8Source).bind Concrete.Stages.compile) =
      .error (.source (.lexical (.encoding .tooLarge) ⟨0, 1, 1, 1, ⟨0⟩⟩)) := by
  refine ⟨Concrete.Stages.KernelEval.load_compile_isOk_of_sparse (by decide +kernel), ?_,
    Concrete.Stages.KernelEval.load_compile_eq_error_of_sparse (by decide +kernel),
    Concrete.Stages.KernelEval.load_compile_eq_error_of_sparse (by decide +kernel),
    Concrete.Stages.KernelEval.load_compile_eq_error_of_sparse (by decide +kernel)⟩
  rw [Concrete.Stages.KernelEval.load_unchecked_eq_sparse]
  decide +kernel
