import Concrete.Interfaces.Carriers

/-!
Carrier controls migrated from `Concrete/Interfaces/Carriers.lean` (F-1, F-2, and N-1).
Every negative control is name-checked against the exact carrier definitions (exhaustive
constructor/field proofs plus `#guard_msgs` matching the exact diagnostic) so a missing or
renamed identifier fails the control rather than passing vacuously.
-/

namespace StageInterfaceControls.CarrierControls

open Concrete
open Concrete.Interfaces
open ConcreteSpec.Core
open ConcreteSpec.StageInterface

private def ctrlFn : FnId :=
  ⟨⟨⟨⟨"pkg"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"mod"⟩], by decide⟩, ⟨"main"⟩⟩

private def ctrlOcc (idx : Nat) : Occurrence := ⟨ctrlFn, ⟨0⟩, .instruction idx⟩

-- F-2 counterexample control: fusion (`first` maps `o10` to `[o1, o2]`) composed with
-- `second` (`o20` from `[o10]`) preserves both contributors; `o2` appears among `o20`'s sources.
example :
    let o1 := ctrlOcc 1
    let o2 := ctrlOcc 2
    let o10 := ctrlOcc 10
    let o20 := ctrlOcc 20
    let first : Account := { mapped := [(o10, [o1, o2])], eliminated := [] }
    let second : Account := { mapped := [(o20, [o10])], eliminated := [] }
    let composed := Account.compose first second
    composed.sourcesOf o20 = [o1, o2] ∧
    o2 ∈ composed.sourcesOf o20 ∧
    composed.MapsTo o20 o1 ∧
    composed.MapsTo o20 o2 ∧
    composed.eliminated = [] ∧
    composed.Valid [o1, o2] [o20] := by
  refine ⟨rfl, ?_, ?_, ?_, rfl, ?_⟩
  · exact List.mem_cons_of_mem _ List.mem_cons_self
  · exact List.mem_cons_self
  · exact List.mem_cons_of_mem _ List.mem_cons_self
  · refine ⟨?_, ?_, ?_, ?_, ?_⟩
    · intro out hout
      rcases List.mem_singleton.mp hout with rfl
      exact List.mem_singleton_self _
    · intro out hout
      rcases List.mem_singleton.mp hout with rfl
      exact ⟨ctrlOcc 1, List.mem_cons_self⟩
    · intro out src hmaps
      have hmapped :
          (Account.compose { mapped := [(ctrlOcc 10, [ctrlOcc 1, ctrlOcc 2])], eliminated := [] }
            { mapped := [(ctrlOcc 20, [ctrlOcc 10])], eliminated := [] }).mapped =
            [(ctrlOcc 20, [ctrlOcc 1, ctrlOcc 2])] := rfl
      change src ∈ ((Account.compose { mapped := [(ctrlOcc 10, [ctrlOcc 1, ctrlOcc 2])], eliminated := [] }
        { mapped := [(ctrlOcc 20, [ctrlOcc 10])], eliminated := [] }).mapped.lookup out).getD [] at hmaps
      rw [hmapped] at hmaps
      by_cases h : out = ctrlOcc 20
      · subst h
        exact hmaps
      · simp [AList.lookup, Ne.symm h] at hmaps
    · intro _ hmem; cases hmem
    · intro src hsrc
      rcases List.mem_cons.mp hsrc with rfl | hrest
      · exact Or.inl ⟨ctrlOcc 20, List.mem_cons_self⟩
      · rcases List.mem_singleton.mp hrest with rfl
        exact Or.inl ⟨ctrlOcc 20, List.mem_cons_of_mem _ List.mem_cons_self⟩

-- F-2 negative controls: no `Account` value can exist with duplicate output keys or with a
-- mapped contributor simultaneously listed in `eliminated`; `.mergedInto` is removed.
example :
    let o1 := ctrlOcc 1
    let o2 := ctrlOcc 2
    let o10 := ctrlOcc 10
    (¬ ∃ a : Account, a.mapped = [(o10, [o1]), (o10, [o2])]) ∧
    (¬ ∃ a : Account, a.mapped = [(o10, [o1, o2])] ∧ a.eliminated = [(o2, .deadValue)]) := by
  refine ⟨?_, ?_⟩
  · rintro ⟨a, h⟩
    have hnd := a.mappedKeysNodup
    rw [h] at hnd
    exact (show ¬ (List.map Prod.fst ([(ctrlOcc 10, [ctrlOcc 1]), (ctrlOcc 10, [ctrlOcc 2])] :
      AList Occurrence (List Occurrence))).Nodup from by decide) hnd
  · rintro ⟨a, hm, he⟩
    have hd := a.disjoint
    rw [hm, he] at hd
    exact (show ¬ (∀ entry ∈ ([(ctrlOcc 10, [ctrlOcc 1, ctrlOcc 2])] : AList Occurrence (List Occurrence)),
      ∀ src ∈ entry.2, AList.lookup [(ctrlOcc 2, EliminationReason.deadValue)] src = none) from by decide) hd

-- Name-checked F-2 negative control: `EliminationReason` has only `.deadValue`,
-- `.unreachableBlock`, and `.unreachableFunction` (no `.mergedInto`).
theorem eliminationReason_exhaustive (r : EliminationReason) :
    r = .deadValue ∨ r = .unreachableBlock ∨ r = .unreachableFunction := by
  cases r with
  | deadValue => exact Or.inl rfl
  | unreachableBlock => exact Or.inr (Or.inl rfl)
  | unreachableFunction => exact Or.inr (Or.inr rfl)

/--
error: Unknown constant `Concrete.Interfaces.EliminationReason.mergedInto`
-/
#guard_msgs in
example : EliminationReason :=
  Concrete.Interfaces.EliminationReason.mergedInto (ctrlOcc 10)

-- F-2 copy + partial elimination + diamond fusion + full elimination + incomplete pass controls.
example :
    let o1 := ctrlOcc 1
    let o10 := ctrlOcc 10
    let o11 := ctrlOcc 11
    let o20 := ctrlOcc 20
    let copyPass : Account := { mapped := [(o10, [o1]), (o11, [o1])], eliminated := [] }
    let partialDead : Account := { mapped := [(o20, [o11])], eliminated := [(o10, .deadValue)] }
    let diamondFuse : Account := { mapped := [(o20, [o10, o11])], eliminated := [] }
    let bothDead : Account :=
      { mapped := [], eliminated := [(o10, .deadValue), (o11, .unreachableBlock)] }
    let incompleteDrop : Account := { mapped := [], eliminated := [(o10, .deadValue)] }
    (Account.compose copyPass partialDead).sourcesOf o20 = [o1] ∧
    (Account.compose copyPass partialDead).eliminated = [] ∧
    (Account.compose copyPass diamondFuse).sourcesOf o20 = [o1] ∧
    AList.lookup (Account.compose copyPass bothDead).eliminated o1 = some .deadValue ∧
    AList.lookup (Account.compose copyPass incompleteDrop).eliminated o1 = none := by
  decide

-- Name-checked F-1 negative controls: `LLVMModule` carries only `items`, `LLVMInput` carries
-- only `(component, launcher, sites)`, and `LLVMInput.ofProfile` does not exist.
theorem llvmModule_single_field (m : LLVMModule) : m = ⟨m.items⟩ := by
  cases m with
  | mk items => rfl

theorem llvmInput_three_fields (p : Profile) (inp : LLVMInput p) :
    inp = ⟨inp.component, inp.launcher, inp.sites⟩ := by
  cases inp with
  | mk component launcher sites => rfl

/--
error: Invalid `⟨...⟩` notation: The expected type `List Backend.LLVM.Item` has more than one constructor

Note: This notation can only be used when the expected type is an inductive type with a single constructor
-/
#guard_msgs in
example (p : Profile) : LLVMModule :=
  ⟨"wrong", p.llvmDataLayout, []⟩

/--
error: Invalid `⟨...⟩` notation: The expected type `List Backend.LLVM.Item` has more than one constructor

Note: This notation can only be used when the expected type is an inductive type with a single constructor
---
error: Invalid `⟨...⟩` notation: The expected type `List Backend.LLVM.Item` has more than one constructor

Note: This notation can only be used when the expected type is an inductive type with a single constructor
-/
#guard_msgs in
example (p : Profile) : LLVMInput p :=
  ⟨⟨"wrong", p.llvmDataLayout, []⟩, ⟨p.llvmTriple, p.llvmDataLayout, []⟩, #[]⟩

/--
error: Unknown constant `Concrete.Interfaces.LLVMInput.ofProfile`
-/
#guard_msgs in
example (p : Profile) : LLVMInput p :=
  Concrete.Interfaces.LLVMInput.ofProfile p [] [] #[]

-- F-1 positive control: `LLVMInput.bytesAt` renders the resolved profile's data layout and triple
-- for both roles, and changing the profile changes the rendered bytes for identical module items.
example :
    let linuxP : Profile := ⟨.llvm, .x86_64Linux, 64, 4096⟩
    let macP : Profile := ⟨.llvm, .aarch64Macos, 64, 4096⟩
    let compMod : LLVMModule := ⟨[.blank]⟩
    let launchMod : LLVMModule := ⟨[]⟩
    let linuxIn : LLVMInput linuxP := ⟨compMod, launchMod, #[]⟩
    let macIn : LLVMInput macP := ⟨compMod, launchMod, #[]⟩
    linuxIn.bytesAt .component =
      (s!"target datalayout = \"{linuxP.llvmDataLayout}\"\ntarget triple = \"{linuxP.llvmTriple}\"\n\n").toUTF8 ∧
    linuxIn.bytesAt .launcher =
      (s!"target datalayout = \"{linuxP.llvmDataLayout}\"\ntarget triple = \"{linuxP.llvmTriple}\"\n").toUTF8 ∧
    macIn.bytesAt .launcher =
      (s!"target datalayout = \"{macP.llvmDataLayout}\"\ntarget triple = \"{macP.llvmTriple}\"\n").toUTF8 ∧
    linuxIn.packageBytes.bytesAt .component = linuxIn.bytesAt .component ∧
    linuxIn.packageBytes.bytesAt .launcher = linuxIn.bytesAt .launcher ∧
    linuxIn.bytesAt .launcher ≠ macIn.bytesAt .launcher := by
  intro linuxP macP compMod launchMod linuxIn macIn
  refine ⟨rfl, rfl, rfl, rfl, rfl, ?_⟩
  intro h
  have hBytes : (linuxIn.bytesAt .launcher).data.toList = (macIn.bytesAt .launcher).data.toList :=
    congrArg (fun b => b.data.toList) h
  exact (show (linuxIn.bytesAt .launcher).data.toList ≠ (macIn.bytesAt .launcher).data.toList from by decide) hBytes

-- Name-checked N-1 control: checker refusals on both routes use `.admission (.refused cause)`;
-- `StageFailure` has no duplicate `.checkerRefused` constructor.
theorem stageFailure_exhaustive (f : StageFailure) :
    (∃ a, f = .admission a) ∨
    (∃ r, f = .resource r) ∨
    (∃ r, f = .unsupportedProfile r) ∨
    (∃ p c, f = .unsupportedFeature p c) ∨
    (∃ r, f = .invalidInput r) ∨
    f = .lcbLinkingUnavailable ∨
    (∃ r, f = .invalidLCBJob r) := by
  cases f with
  | admission a => exact Or.inl ⟨a, rfl⟩
  | resource r => exact Or.inr (Or.inl ⟨r, rfl⟩)
  | unsupportedProfile r => exact Or.inr (Or.inr (Or.inl ⟨r, rfl⟩))
  | unsupportedFeature p c => exact Or.inr (Or.inr (Or.inr (Or.inl ⟨p, c, rfl⟩)))
  | invalidInput r => exact Or.inr (Or.inr (Or.inr (Or.inr (Or.inl ⟨r, rfl⟩))))
  | lcbLinkingUnavailable => exact Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inl rfl)))))
  | invalidLCBJob r => exact Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (Or.inr ⟨r, rfl⟩)))))

/--
error: Unknown constant `Concrete.Interfaces.StageFailure.checkerRefused`
-/
#guard_msgs in
example (cause : RefusalCause) : StageFailure :=
  Concrete.Interfaces.StageFailure.checkerRefused cause

example (cause : RefusalCause) : StageFailure :=
  .admission (.refused cause)

end StageInterfaceControls.CarrierControls
