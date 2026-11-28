import Concrete.Backend.ValidateEmit

/-!
# D5 production backend witnesses, width-policy controls (`ISS-0115`/`ISS-0117`), and mutants

Standalone controls on the fixed Slice 0 instance (`slice0`, `sliceLanguage slice0`,
`sliceLoweringLanguage slice0`) exercising the unified production emitter
`Concrete.Backend.ValidateEmit.emit` through the registered `(registry passes).emit` stage:

1. **Universal registered validation and soundness on fixed `slice0`**:
   - `(registry (prodPasses p)).emit.Validated` across LCB and LLVM profiles;
   - unconditional `Stage.Sound (prodPasses p) .emit` on LCB (`.kind = .directProof`);
   - conditional `Stage.Sound (prodPasses p) .emit` on LLVM under `P_print`
     (`.kind = .namedPremise "P_print"`).
2. **Reachable whole-program witnesses**:
   - straight-line `return 42` on LCB (`EmitsLCB`, `validateJob`, observes exit 42 in both
     `BIR.Execution` and `LCBInput.Execution`) and LLVM (`LLVMMetadataMatches`, `LauncherMatches`,
     `ComponentOwnsSymbols`, distinct `x86_64Linux` vs `aarch64Macos` component and launcher headers);
   - two-result `u8` `addWithOverflow`:
     - `20 + 22` → exit 42 on LCB (`LCBEncoding` + `LCBInput.Execution`) and LLVM
       (`@llvm.uadd.with.overflow.i8` declaration + `LLVMMetadataMatches`);
     - `255 + 1` → trap 134 with exact stderr `concrete: trap: overflow at src/main.con:1:1 (site 0)\n`
       on LCB (`LCBEncoding` + `LCBInput.Execution`) and LLVM (`launcherItems`);
   - **`ISS-0117` width-policy contrast**: well-formed `u32` arithmetic accepted by LLVM
     (`LLVMMetadataMatches`) and refused by LCB with `.unsupportedFeature` citing
     `BackendPolicy.intWidthRule` (`IntWidthSupported@1`), paired with passing LCB controls for
     `u8`, `i8`, `u16`, and `i16`;
   - entry zero-slot self-loop (trampoline block 0 inserted, diverges in both `BIR.Execution`
     and `LCBInput.Execution`);
   - same-target parameterized branch (split block inserted, observes exit 42 on `true` and
     exit 7 on `false` in `LCBInput.Execution`);
   - unreachable helper function containing a `.call` (`requiresLCBLinking = false`,
     `entry_no_reachable_calls`, `additionalJobs` gets validated `concrete_fn_0`, `EmitsLCB`);
   - LCB refusal precedence:
     - `requiresLCBLinking = true` beats `firstUnsupported` (`.lcbLinkingUnavailable`);
     - `.load` refused with `BackendPolicy.storageFormRule` (`StorageFormSupported@1`);
     - Case 4 `.invalidLCBJob` on a 9-parameter well-formed entry BIR where `emitUnvalidated`
       succeeds with `LCBEncoding` and `validateJobs` rejects `parameters > 8`.
3. **Eleven rejected wrong-compiler mutants**, proving `¬ (registry mutant).emit.Validated`
   (and refusal-cause rejection on fixed `slice0`) across both LCB and LLVM profiles.
-/

namespace StageInterfaceControls.D5Backends

open Concrete.Backend
open Concrete.Backend.LCBWordLemmas
open Concrete.Backend.ValidateEmit
open Concrete.Contracts
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface
open LeanCompilerBackend.Verified

private abbrev s0Lang : CoreLanguage := sliceLanguage slice0
private abbrev s0Low : LoweringLanguage s0Lang.operands.Ty := sliceLoweringLanguage slice0
private abbrev u8Ty : s0Lang.operands.Ty := s0Low.intTy .u8

private def mainFn : FnId := ⟨⟨⟨⟨"d5"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩, ⟨"main"⟩⟩
private def helperFn : FnId := ⟨⟨⟨⟨"d5"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩, ⟨"helper"⟩⟩

private theorem helperFn_ne_mainFn : helperFn ≠ mainFn := by
  intro h
  have hn := congrArg (fun f => f.name.text) h
  exact (show "helper" ≠ "main" from by decide) hn

private abbrev lcbLinux : Profile := ⟨.lcb, .x86_64Linux, 16, 4096⟩
private abbrev lcbMac : Profile := ⟨.lcb, .aarch64Macos, 16, 4096⟩
private abbrev llvmLinux : Profile := ⟨.llvm, .x86_64Linux, 16, 4096⟩
private abbrev llvmMac : Profile := ⟨.llvm, .aarch64Macos, 16, 4096⟩

private def span0 : FileSpan := ⟨⟨0⟩, 0, 1, 1, 1⟩
private def checkedSpan0 : ConcreteSpec.Core.Span :=
  ⟨⟨0⟩, 0, 1, by decide, 1, 1, by decide, by decide⟩

private theorem span0_binds : ∃ s, span0.Binds tabProject s :=
  ⟨checkedSpan0, File.load mainSourcePath tabSource, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private def orig0 : Origin := ⟨⟨0⟩, span0, ⟨.function mainFn, ⟨[]⟩⟩⟩
private def origHelper : Origin := ⟨⟨1⟩, span0, ⟨.function helperFn, ⟨[]⟩⟩⟩

private def expectedTrapSite : DiagnosticSite :=
  ⟨⟨0⟩, ⟨mainSourcePath.render.text, 1, 1⟩⟩

private def expectedOverflowLine : String :=
  s!"concrete: trap: overflow at {mainSourcePath.render.text}:1:1 (site 0)\n"

private theorem runtimeSite_orig0 : runtimeSite tabProject orig0 = some expectedTrapSite := rfl

private theorem renderTrapLine_overflow0 :
    renderTrapLine .overflow expectedTrapSite = expectedOverflowLine := rfl

private def dummyRule : RuleId := ⟨`StageInterfaceControls.D5Backends.unused, 1⟩

/-- Build a Slice 0 `Passes` bundle whose `emit` field is `fn`. -/
private def passesWithEmit (profile : Profile)
    (fn : BIR s0Low profile → Except StageFailure (BackendOutput profile)) :
    Passes slice0 profile where
  lex _ := .error (.missingFile ⟨0⟩)
  elaborate _ := .error (.invalidTree ⟨0⟩)
  checkCore _ := .error (.fragment (.unsupportedSlice slice0))
  lowerCore _ := .error (.invalidInput dummyRule)
  lowerUnchecked _ := .error (.invalidInput dummyRule)
  lowerMemory _ := .error (.invalidInput dummyRule)
  buildSSA _ := .error (.invalidInput dummyRule)
  buildBIR _ := .error (.invalidInput dummyRule)
  emit := fn

private def prodPasses (profile : Profile) : Passes slice0 profile :=
  passesWithEmit profile (ValidateEmit.emit (language := s0Lang))

/-! ### 1. Universal registered validation and soundness on fixed `slice0` -/

theorem d5_emit_validated_lcbLinux : (registry (prodPasses lcbLinux)).emit.Validated :=
  emit_validated (prodPasses lcbLinux) rfl

theorem d5_emit_validated_lcbMac : (registry (prodPasses lcbMac)).emit.Validated :=
  emit_validated (prodPasses lcbMac) rfl

theorem d5_emit_validated_llvmLinux : (registry (prodPasses llvmLinux)).emit.Validated :=
  emit_llvm_validated (prodPasses llvmLinux) rfl

theorem d5_emit_validated_llvmMac : (registry (prodPasses llvmMac)).emit.Validated :=
  emit_llvm_validated (prodPasses llvmMac) rfl

theorem d5_emit_sound_lcbLinux :
    (registry (prodPasses lcbLinux)).emit.kind = .directProof ∧
    Stage.Sound (prodPasses lcbLinux) .emit :=
  ⟨rfl, emit_sound_lcb (prodPasses lcbLinux) rfl rfl⟩

theorem d5_emit_sound_llvmMac
    (hPrint : ∀ (bir : BIR s0Low llvmMac) (output : LLVMInput llvmMac),
      BIR.WF (language := s0Lang) bir →
      EmitLLVM.emit (language := s0Lang) bir = .ok output →
      P_print (language := s0Lang) bir output) :
    (registry (prodPasses llvmMac)).emit.kind = .namedPremise "P_print" ∧
    Stage.Sound (prodPasses llvmMac) .emit :=
  ⟨rfl, emit_sound_llvm_of_print (prodPasses llvmMac) rfl hPrint⟩

/-! ### Shared Slice 0 BIR fixtures -/

private def returnBIR (value : UInt8) (profile : Profile) : BIR s0Low profile :=
  let blk : EmitLCB.BIRBlock s0Low :=
    ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[value]⟩⟩), [orig0]⟩
  ⟨tabProject, mainFn, [(mainFn, ⟨[], some u8Ty⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[blk]⟩)], [], #[], [], []⟩

private def checkedAddBIR (a b : UInt8) (profile : Profile) : BIR s0Low profile :=
  let blk0 : EmitLCB.BIRBlock s0Low :=
    ⟨⟨0⟩, [],
      [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[a]⟩) (.literal ⟨[b]⟩)⟩],
      .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
  let blk1 : EmitLCB.BIRBlock s0Low :=
    ⟨⟨1⟩, [], [], .trap ⟨0⟩, []⟩
  let blk2 : EmitLCB.BIRBlock s0Low :=
    ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
  ⟨tabProject, mainFn, [(mainFn, ⟨[], some u8Ty⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[blk0, blk1, blk2]⟩)], [], #[⟨.overflow, orig0⟩], [], []⟩

private def intLitBytes (t : Core.IntTy) (n : UInt8) : List UInt8 :=
  n :: List.replicate (t.byteSize - 1) 0

private def widthArithBIR (t : Core.IntTy) (profile : Profile) : BIR s0Low profile :=
  let ty := s0Low.intTy t
  let lit1 : SSARead s0Lang.operands.Ty s0Low.literal ty := .literal ⟨intLitBytes t 1⟩
  let lit2 : SSARead s0Lang.operands.Ty s0Low.literal ty := .literal ⟨intLitBytes t 2⟩
  let inst : SSAInst s0Low :=
    @SSAInst.op _ s0Low (.arithmetic (.wrapping .add)) [ty, ty] ty ⟨0⟩ ()
      (.cons lit1 (.cons lit2 .nil)) []
  let blk0 : EmitLCB.BIRBlock s0Low :=
    ⟨⟨0⟩, [], [⟨orig0, inst⟩], .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩
  ⟨tabProject, mainFn, [(mainFn, ⟨[], some u8Ty⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[blk0]⟩)], [], #[], [], []⟩

private def entryLoopBIR (profile : Profile) : BIR s0Low profile :=
  let blk0 : EmitLCB.BIRBlock s0Low := ⟨⟨0⟩, [], [], .jump ⟨⟨0⟩, []⟩, []⟩
  ⟨tabProject, mainFn, [(mainFn, ⟨[], none⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[blk0]⟩)], [], #[], [], []⟩

private def sameTargetBranchBIR (flag : Bool) (profile : Profile) : BIR s0Low profile :=
  let aByte : UInt8 := if flag then 255 else 0
  let blk0 : EmitLCB.BIRBlock s0Low :=
    ⟨⟨0⟩, [],
      [⟨orig0, .addWithOverflow .u8 ⟨1⟩ ⟨2⟩ (.literal ⟨[aByte]⟩) (.literal ⟨[1]⟩)⟩],
      .branch (.value ⟨2⟩)
        ⟨⟨1⟩, [⟨u8Ty, .literal ⟨[42]⟩⟩]⟩
        ⟨⟨1⟩, [⟨u8Ty, .literal ⟨[7]⟩⟩]⟩, []⟩
  let blk1 : EmitLCB.BIRBlock s0Low :=
    ⟨⟨1⟩, [⟨u8Ty, ⟨0⟩⟩], [], .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
  ⟨tabProject, mainFn, [(mainFn, ⟨[], some u8Ty⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[blk0, blk1]⟩)], [], #[], [], []⟩

private def unreachableHelperBIR (profile : Profile) : BIR s0Low profile :=
  let mainBlk : EmitLCB.BIRBlock s0Low :=
    ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩
  let helperBlk : EmitLCB.BIRBlock s0Low :=
    ⟨⟨0⟩, [],
      [⟨origHelper, .call mainFn (some ⟨u8Ty, ⟨0⟩⟩) []⟩],
      .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
  ⟨tabProject, mainFn,
    [(mainFn, ⟨[], some u8Ty⟩), (helperFn, ⟨[], some u8Ty⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[mainBlk]⟩), (helperFn, [], ⟨⟨0⟩, #[helperBlk]⟩)],
    [], #[], [], []⟩

private def linkingAndU32BIR (profile : Profile) : BIR s0Low profile :=
  let ty32 := s0Low.intTy .u32
  let lit1 : SSARead s0Lang.operands.Ty s0Low.literal ty32 := .literal ⟨intLitBytes .u32 1⟩
  let lit2 : SSARead s0Lang.operands.Ty s0Low.literal ty32 := .literal ⟨intLitBytes .u32 2⟩
  let addInst : SSAInst s0Low :=
    @SSAInst.op _ s0Low (.arithmetic (.wrapping .add)) [ty32, ty32] ty32 ⟨0⟩ ()
      (.cons lit1 (.cons lit2 .nil)) []
  let mainBlk : EmitLCB.BIRBlock s0Low :=
    ⟨⟨0⟩, [],
      [⟨orig0, addInst⟩,
       ⟨orig0, .call helperFn (some ⟨u8Ty, ⟨1⟩⟩) []⟩],
      .ret (some ⟨u8Ty, .value ⟨1⟩⟩), []⟩
  let helperBlk : EmitLCB.BIRBlock s0Low :=
    ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩
  ⟨tabProject, mainFn,
    [(mainFn, ⟨[], some u8Ty⟩), (helperFn, ⟨[], some u8Ty⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[mainBlk]⟩), (helperFn, [], ⟨⟨0⟩, #[helperBlk]⟩)],
    [], #[], [], []⟩

private def loadBIR (profile : Profile) : BIR s0Low profile :=
  let blk0 : EmitLCB.BIRBlock s0Low :=
    ⟨⟨0⟩, [],
      [⟨orig0, @SSAInst.load _ s0Low u8Ty ⟨0⟩ ⟨⟨0⟩, []⟩⟩],
      .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
  ⟨tabProject, mainFn, [(mainFn, ⟨[], some u8Ty⟩)],
    [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[blk0]⟩)],
    [(mainFn, [(⟨0⟩, ⟨1, 1, .nil⟩)])], #[], [], []⟩

private def nineParamBIR (profile : Profile) : BIR s0Low profile :=
  let params : List (Sigma (SSAValue s0Lang.operands.Ty)) :=
    [⟨u8Ty, ⟨0⟩⟩, ⟨u8Ty, ⟨1⟩⟩, ⟨u8Ty, ⟨2⟩⟩, ⟨u8Ty, ⟨3⟩⟩, ⟨u8Ty, ⟨4⟩⟩,
     ⟨u8Ty, ⟨5⟩⟩, ⟨u8Ty, ⟨6⟩⟩, ⟨u8Ty, ⟨7⟩⟩, ⟨u8Ty, ⟨8⟩⟩]
  let blk0 : EmitLCB.BIRBlock s0Low :=
    ⟨⟨0⟩, params, [], .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), [orig0]⟩
  ⟨tabProject, mainFn, [(mainFn, ⟨List.replicate 9 u8Ty, some u8Ty⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[blk0]⟩)], [], #[], [], []⟩

/-! ### Standalone `validateJob`, LLVM header, runtime-bytes, and constructive LCB step helpers -/

private def retBody (n : Int) : Source.Function :=
  { resultWidth := .word
    parameters := #[]
    valueWidths := #[]
    blocks := #[{ phis := #[]
                  instructions := #[]
                  terminator := .ret (.literal (.word (BitVec.ofInt 32 n))) }] }

private theorem validateJob_ret42 : validateJob (LCBJob.lcbFunctionJob (retBody 42)) = .ok () := by
  unfold validateJob validateJobWithWork LCBJob.lcbFunctionJob LCBJob.bodySymbol
  dsimp only
  simp only [bne_self_eq_false]
  with_unfolding_all rfl

private theorem validateJob_ret43 : validateJob (LCBJob.lcbFunctionJob (retBody 43)) = .ok () := by
  unfold validateJob validateJobWithWork LCBJob.lcbFunctionJob LCBJob.bodySymbol
  dsimp only
  simp only [bne_self_eq_false]
  with_unfolding_all rfl

private theorem validateJob_ret134 : validateJob (LCBJob.lcbFunctionJob (retBody 134)) = .ok () := by
  unfold validateJob validateJobWithWork LCBJob.lcbFunctionJob LCBJob.bodySymbol
  dsimp only
  simp only [bne_self_eq_false]
  with_unfolding_all rfl

private theorem validateJob_dupHelper :
    validateJob { EmitLCB.helperJob 0 with
      code := { (EmitLCB.helperJob 0).code with symbol := LCBJob.bodySymbol } } = .ok () := by
  unfold validateJob validateJobWithWork EmitLCB.helperJob LCBJob.bodySymbol
  dsimp only
  simp only [bne_self_eq_false]
  with_unfolding_all rfl

private theorem validateJob_nineParam_err :
    validateJob (LCBJob.lcbFunctionJob
      { resultWidth := .word
        parameters := Array.replicate 9 .word
        valueWidths := Array.replicate 9 .word
        blocks := #[{ phis := #[]
                      instructions := #[]
                      terminator := .ret (.literal (.word (BitVec.ofInt 32 0))) }] }) =
      .error (.admission (.resourceLimit "parameters" 9 8)) := by
  unfold validateJob validateJobWithWork LCBJob.lcbFunctionJob LCBJob.bodySymbol
  dsimp only
  simp only [bne_self_eq_false]
  with_unfolding_all rfl

private theorem prefixByte (head tail : String) (idx : Nat) (h : idx < head.toUTF8.data.size) :
    (head ++ tail).toUTF8.data[idx]? = head.toUTF8.data[idx]? := by
  change (head.toUTF8 ++ tail.toUTF8).data[idx]? = _
  rw [ByteArray.data_append, Array.getElem?_append_left h]

private theorem suffixByte (head tail : String) (idx : Nat) (h : head.toUTF8.data.size ≤ idx) :
    (head ++ tail).toUTF8.data[idx]? = tail.toUTF8.data[idx - head.toUTF8.data.size]? := by
  change (head.toUTF8 ++ tail.toUTF8).data[idx]? = _
  rw [ByteArray.data_append, Array.getElem?_append_right h]

private theorem llvm_linux_ne_mac_bytesAt (in₁ : LLVMInput llvmLinux) (in₂ : LLVMInput llvmMac)
    (role : LLVMRole) : in₁.bytesAt role ≠ in₂.bytesAt role := by
  intro heq
  have hPrefSz : ("target datalayout = \"").toUTF8.data.size = 21 := by decide
  have h1 : 25 < ("target datalayout = \"" ++ llvmLinux.llvmDataLayout).toUTF8.data.size := by
    change 25 < (("target datalayout = \"").toUTF8 ++ llvmLinux.llvmDataLayout.toUTF8).data.size
    rw [ByteArray.data_append, Array.size_append, hPrefSz]
    have : 4 < llvmLinux.llvmDataLayout.toUTF8.data.size := by decide
    omega
  have h2 : 25 < ("target datalayout = \"" ++ llvmMac.llvmDataLayout).toUTF8.data.size := by
    change 25 < (("target datalayout = \"").toUTF8 ++ llvmMac.llvmDataLayout.toUTF8).data.size
    rw [ByteArray.data_append, Array.size_append, hPrefSz]
    have : 4 < llvmMac.llvmDataLayout.toUTF8.data.size := by decide
    omega
  have hPre : ∀ (a b c d e : String), 25 < a.toUTF8.data.size →
      (((((a ++ b) ++ c) ++ d) ++ e).toUTF8).data[25]? = a.toUTF8.data[25]? := by
    intro a b c d e ha
    have hSz1 : 25 < (a ++ b).toUTF8.data.size := by
      change 25 < (a.toUTF8 ++ b.toUTF8).data.size; rw [ByteArray.data_append, Array.size_append]; omega
    have hSz2 : 25 < ((a ++ b) ++ c).toUTF8.data.size := by
      change 25 < ((a ++ b).toUTF8 ++ c.toUTF8).data.size; rw [ByteArray.data_append, Array.size_append]; omega
    have hSz3 : 25 < (((a ++ b) ++ c) ++ d).toUTF8.data.size := by
      change 25 < (((a ++ b) ++ c).toUTF8 ++ d.toUTF8).data.size; rw [ByteArray.data_append, Array.size_append]; omega
    rw [prefixByte _ e 25 hSz3, prefixByte _ d 25 hSz2, prefixByte _ c 25 hSz1, prefixByte _ b 25 ha]
  have hb := congrArg (fun ba => ba.data[25]?) heq
  unfold LLVMInput.bytesAt at hb
  dsimp only [toString] at hb
  rw [hPre _ _ _ _ _ h1, hPre _ _ _ _ _ h2] at hb
  rw [suffixByte _ _ 25 (by omega), suffixByte _ _ 25 (by omega), hPrefSz] at hb
  exact (show llvmLinux.llvmDataLayout.toUTF8.data[4]? ≠ llvmMac.llvmDataLayout.toUTF8.data[4]? by decide) hb

private theorem runtimeBytes_nil_size_ne_zero : (EmitLCB.runtimeBytes []).size ≠ 0 := by
  unfold EmitLCB.runtimeBytes Runtime.qbeDiagnostics
  dsimp only
  change (_ ++ "}\n").toUTF8.size ≠ 0
  change (_ ++ ("}\n").toUTF8).size ≠ 0
  rw [ByteArray.size_append]
  have h2 : ("}\n").toUTF8.size = 2 := by decide
  omega

private theorem sourceSteps_to_lcbSteps {E : Type} {profile : Profile}
    (project : Project) (out : LCBInput profile) :
    ∀ {k : Nat} {s s' : Source.State}, Source.Steps out.body k s s' →
      Steps ((LCBInput.Execution E project).step out) (some s) (some s')
  | _, _, _, .zero _ => .nil _
  | _, _, _, .succ h1 hRest => .cons ⟨_, _, rfl, rfl, h1⟩ (sourceSteps_to_lcbSteps project out hRest)

private theorem lcb_observes_of_steps {E : Type} {profile : Profile}
    (project : Project) (out : LCBInput profile) {k : Nat} {s0 sEnd : Source.State}
    {w : BitVec 32} {obs : Concrete.Contracts.Observation}
    (hInit : Source.initial out.body #[] = .ok s0)
    (hSteps : Source.Steps out.body k s0 sEnd)
    (hRet : Source.step out.body sEnd = .ok (.returned (.word w)))
    (hRes : (if w.toNat < 256 then some (.returned (UInt8.ofNat w.toNat) ByteArray.empty)
             else do
               let entry ← out.sites[w.toNat - 256]?
               let site ← runtimeSite project entry.origin
               pure (.trapped 134 entry.kind site (renderTrapLine entry.kind site).toUTF8)) = some obs) :
    (LCBInput.Execution E project).observes out obs := by
  refine ⟨some sEnd, ⟨some s0, ?_, sourceSteps_to_lcbSteps project out hSteps⟩, ?_⟩
  · change some s0 = (Source.initial out.body #[]).toOption
    rw [hInit]; rfl
  · change (match Source.step out.body sEnd with | .ok (.returned v) => _ | _ => none) = some obs
    rw [hRet]
    with_unfolding_all exact hRes

private theorem lcb_observes_u8 {E : Type} {profile : Profile}
    (project : Project) (out : LCBInput profile) {k : Nat} {s0 sEnd : Source.State}
    {n : Int} (hn : Core.IntTy.u8.InRange n)
    (hInit : Source.initial out.body #[] = .ok s0)
    (hSteps : Source.Steps out.body k s0 sEnd)
    (hRet : Source.step out.body sEnd = .ok (.returned (word n))) :
    (LCBInput.Execution E project).observes out (.returned (UInt8.ofNat n.toNat) ByteArray.empty) := by
  refine lcb_observes_of_steps project out hInit hSteps hRet ?_
  have hLt : (wordBits n).toNat < 256 := u8_wordBits_lt hn
  have hEq : (wordBits n).toNat = n.toNat := u8_wordBits_toNat hn
  rw [ite_eq_left hLt, hEq]

private theorem lcb_observes_trap {E : Type} {profile : Profile}
    (project : Project) (out : LCBInput profile) {k : Nat} {s0 sEnd : Source.State}
    {slot : Nat} {entry : TrapEntry} {site : DiagnosticSite}
    (hSlot : slot < LCBWordLemmas.trapCapacity)
    (hInit : Source.initial out.body #[] = .ok s0)
    (hSteps : Source.Steps out.body k s0 sEnd)
    (hRet : Source.step out.body sEnd = .ok (.returned (trapWord slot)))
    (hSite : out.sites[slot]? = some entry)
    (hRt : runtimeSite project entry.origin = some site) :
    (LCBInput.Execution E project).observes out
      (.trapped 134 entry.kind site (renderTrapLine entry.kind site).toUTF8) := by
  refine lcb_observes_of_steps project out hInit hSteps hRet ?_
  have hNotLt : ¬ (trapWordBits slot).toNat < 256 := trapWordBits_not_lt hSlot
  have hSub : (trapWordBits slot).toNat - 256 = slot := trapWordBits_sub hSlot
  simp only [hNotLt, ↓reduceIte, hSub, hSite, hRt, bind, Option.bind, pure]

private theorem transfer_one_phi {f : Source.Function} {pred target : Source.BlockId}
    {tb : Source.Block} {phi : Source.Phi} {opnd : Source.Operand} {v : Source.Value}
    {env env' : Source.Environment}
    (hb : f.blocks[target]? = some tb)
    (hphi : tb.phis = #[phi])
    (hinc : phi.incoming.filter (·.1 == pred) = #[(pred, opnd)])
    (hrd : Source.read env phi.width opnd = .ok v)
    (hwr : Source.write f env phi.destination v = .ok env') :
    Source.transfer f pred target env = .ok ⟨target, 0, env'⟩ := by
  unfold Source.transfer
  rw [hb]
  dsimp only
  rw [hphi, ← Array.forIn_toList]
  have hSz : (#[(pred, opnd)] : Array (Source.BlockId × Source.Operand)).size = 1 := rfl
  have hGet0 : (#[(pred, opnd)] : Array (Source.BlockId × Source.Operand))[0]? = some (pred, opnd) := rfl
  simp only [List.forIn_cons, List.forIn_nil, hinc, hSz, bne_self_eq_false, Array.any_empty,
    Bool.false_or, Bool.false_eq_true, ↓reduceIte, hGet0, hrd, Array.push_empty,
    bind, Except.bind, pure, Except.pure, ← Array.forIn_toList, hwr]

/-! ### Well-formedness proofs for all Slice 0 BIR fixtures -/

private theorem declsOk_nil : ConcreteSpec.DeclsOk [] := by decide

private theorem mutableLocalsWF_single {G : Type} (fn : FnId)
    (regs : List (PlaceId × s0Lang.operands.Ty)) (g : G) :
    MutableLocalsWF [(fn, [] ++ regs, g)]
      (if (List.map Prod.fst regs).isEmpty then [] else [(fn, List.map Prod.fst regs ++ [])]) := by
  cases hE : (List.map Prod.fst regs).isEmpty with
  | true => exact ⟨.nil, nofun⟩
  | false =>
    refine ⟨.cons nofun .nil, fun f roots hm => ?_⟩
    cases List.mem_singleton.mp hm
    refine ⟨[] ++ regs, g, List.mem_singleton_self _, fun r hr => ?_⟩
    simp only [List.append_nil, List.nil_append] at hr ⊢
    obtain ⟨⟨r', ty⟩, hMem, hEq⟩ := List.mem_map.mp hr
    subst hEq
    exact ⟨ty, hMem⟩

private theorem bir_wf_single_simple {profile : Profile}
    (sig : FunctionSignature s0Lang.operands.Ty)
    (blocks : Array (EmitLCB.BIRBlock s0Low))
    (sites : TrapTable)
    (hReg : (BIR.mk tabProject mainFn [(mainFn, sig)] [(mainFn, [], ⟨⟨0⟩, blocks⟩)]
        [] sites [] [] : BIR s0Low profile).checkRegisterFormation (language := s0Lang) = true)
    (hTraps : (BIR.mk tabProject mainFn [(mainFn, sig)] [(mainFn, [], ⟨⟨0⟩, blocks⟩)]
        [] sites [] [] : BIR s0Low profile).TrapSlotsWF)
    (hSlots : (BIR.mk tabProject mainFn [(mainFn, sig)] [(mainFn, [], ⟨⟨0⟩, blocks⟩)]
        [] sites [] [] : BIR s0Low profile).frameSlots ≤ profile.maxBIRSlots)
    (hOrig : ∀ origin ∈ (BIR.mk tabProject mainFn [(mainFn, sig)] [(mainFn, [], ⟨⟨0⟩, blocks⟩)]
        [] sites [] [] : BIR s0Low profile).origins, ∃ s, origin.lexical.Binds tabProject s)
    (hSitesBind : ∀ site ∈ sites.toList, ∃ s, site.origin.lexical.Binds tabProject s)
    (hMemOrig : ∀ origin ∈ ((BIR.mk tabProject mainFn [(mainFn, sig)] [(mainFn, [], ⟨⟨0⟩, blocks⟩)]
        [] sites [] [] : BIR s0Low profile).memoryView (language := s0Lang)).origins,
        ∃ s, origin.lexical.Binds tabProject s)
    (hStorNd : ∀ cls, (mainFn, cls) ∈ ((BIR.mk tabProject mainFn [(mainFn, sig)] [(mainFn, [], ⟨⟨0⟩, blocks⟩)]
        [] sites [] [] : BIR s0Low profile).memoryView (language := s0Lang)).storage →
        (List.map Prod.fst cls).Nodup)
    (hLocNd : ∀ locals graph, (mainFn, locals, graph) ∈ ((BIR.mk tabProject mainFn [(mainFn, sig)]
        [(mainFn, [], ⟨⟨0⟩, blocks⟩)] [] sites [] [] : BIR s0Low profile).memoryView (language := s0Lang)).functions →
        (List.map Prod.fst locals).Nodup)
    (hBlocksNd : (blocks.toList.map Block.id).Nodup)
    (hEntryBlk : ∃ b ∈ blocks.toList, b.id = ⟨0⟩ ∧ b.params.map Sigma.fst = sig.parameters)
    (hEdges : ∀ b ∈ blocks.toList, ∀ e ∈ b.ending.successors,
      ∃ b' ∈ blocks.toList, b'.id = e.target ∧ e.arguments.map Sigma.fst = b'.params.map Sigma.fst)
    (hRets : ∀ b ∈ blocks.toList,
      (∀ v, b.ending = .ret v → v.map Sigma.fst = sig.result))
    (hNoCalls : ∀ b ∈ blocks.toList, ∀ op ∈ b.body,
      ∀ callee dst args, op.instruction ≠ .call callee dst args) :
    BIR.WF (language := s0Lang)
      (⟨tabProject, mainFn, [(mainFn, sig)], [(mainFn, [], ⟨⟨0⟩, blocks⟩)],
        [], sites, [], []⟩ : BIR s0Low profile) := by
  refine ⟨(BIR.checkRegisterFormation_eq_true_iff _).mp hReg, hTraps, hSlots, hOrig, hSitesBind, ?_⟩
  refine ⟨⟨.cons nofun .nil, .cons nofun .nil, .cons nofun .nil, .nil, ?_, nofun, ?_⟩,
    List.mem_singleton_self _, hMemOrig, nofun, ⟨.nil, declsOk_nil, nofun⟩, ?_⟩
  · intro fn cls hm
    have hEq := List.mem_singleton.mp hm
    cases congrArg Prod.fst hEq
    exact hStorNd cls hm
  · intro fn locals graph hm
    have hEq := List.mem_singleton.mp hm
    cases congrArg Prod.fst hEq
    have hLoc := hLocNd locals graph hm
    cases congrArg (fun p => p.2.2) hEq
    obtain ⟨eb, hebMem, hebId, hebParams⟩ := hEntryBlk
    refine ⟨hLoc, ⟨?_, ?_, ?_⟩, ⟨sig, rfl, ?_, ?_⟩⟩
    · rw [Array.toList_map, List.map_map]
      exact hBlocksNd
    · exact ⟨_, by rw [Array.toList_map]; exact List.mem_map.mpr ⟨eb, hebMem, rfl⟩, hebId⟩
    · intro mb hmb e he
      rw [Array.toList_map] at hmb
      obtain ⟨b, hbMem, rfl⟩ := List.mem_map.mp hmb
      have hSucc : ∃ e0 ∈ b.ending.successors,
          e0.target = e.target ∧ e.arguments.map Sigma.fst = e0.arguments.map Sigma.fst := by
        cases hEnd : b.ending with
        | jump j =>
          rw [hEnd] at he
          cases List.mem_singleton.mp he
          exact ⟨j, List.mem_singleton_self _, rfl, by rw [List.map_map]; rfl⟩
        | branch t y n =>
          rw [hEnd] at he
          cases he with
          | head => exact ⟨y, List.mem_cons_self, rfl, by rw [List.map_map]; rfl⟩
          | tail _ htl =>
            cases List.mem_singleton.mp htl
            exact ⟨n, List.mem_cons_of_mem _ List.mem_cons_self, rfl, by rw [List.map_map]; rfl⟩
        | ret _ => rw [hEnd] at he; cases he
        | trap slot =>
          rw [hEnd] at he
          with_unfolding_all
          change e ∈ (match sites[slot.index]? with
            | some entry => Terminator.trap entry.kind entry.origin | none => .invalid : MemoryTerm s0Low).successors at he
          cases hS : sites[slot.index]? with
          | none => rw [hS] at he; cases he
          | some entry => rw [hS] at he; cases he
      obtain ⟨e0, he0Mem, he0Tgt, he0Args⟩ := hSucc
      obtain ⟨b', hb'Mem, hb'Id, hb'Args⟩ := hEdges b hbMem e0 he0Mem
      refine ⟨_, by rw [Array.toList_map]; exact List.mem_map.mpr ⟨b', hb'Mem, rfl⟩,
        hb'Id.trans he0Tgt, ?_⟩
      dsimp only
      rw [List.map_map]
      exact he0Args.trans hb'Args
    · refine ⟨_, by rw [Array.toList_map]; exact List.mem_map.mpr ⟨eb, hebMem, rfl⟩, hebId, ?_⟩
      dsimp only
      rw [List.map_map]
      exact hebParams
    · intro mb hmb
      rw [Array.toList_map] at hmb
      obtain ⟨b, hbMem, hMb⟩ := List.mem_map.mp hmb
      refine ⟨?_, ?_⟩
      · cases hBEnd : b.ending with
        | jump _ => rw [← hMb, hBEnd]; trivial
        | branch _ _ _ => rw [← hMb, hBEnd]; trivial
        | trap slot =>
          obtain ⟨entry, hEntry⟩ := hTraps _ (List.mem_singleton_self _) b hbMem slot hBEnd
          have hEndEq : mb.ending = .trap entry.kind entry.origin := by
            rw [← hMb, hBEnd]
            with_unfolding_all
              change (match sites[slot.index]? with
                | some e => Terminator.trap e.kind e.origin | none => .invalid : MemoryTerm s0Low) = _
              rw [hEntry]
          rw [hEndEq]; trivial
        | ret ov =>
          rw [← hMb, hBEnd]
          have hR := hRets b hbMem ov hBEnd
          cases ov with
          | none => exact hR
          | some pair => exact hR
      · rw [← hMb]
        intro op hop
        obtain ⟨raw, hRawMem, rfl⟩ := List.mem_map.mp hop
        cases hInst : raw.instruction with
        | call callee dst args => exact (hNoCalls b hbMem raw hRawMem callee dst args hInst).elim
        | op _ _ _ _ | addWithOverflow _ _ _ _ _ | load _ _ | destructure _ _
        | store _ _ | beginLifetime _ | endLifetime _ => trivial
  · exact mutableLocalsWF_single mainFn _ _

private theorem returnBIR_wf (value : UInt8) (profile : Profile)
    (hReg : (returnBIR value profile).checkRegisterFormation (language := s0Lang) = true) :
    BIR.WF (language := s0Lang) (returnBIR value profile) := by
  refine bir_wf_single_simple ⟨[], some u8Ty⟩ _ #[]
    hReg
    (by intro fn hm blk hb slot ht; cases List.mem_singleton.mp hm; cases List.mem_singleton.mp hb; cases ht)
    (Nat.zero_le _)
    (by intro o ho; have h : o ∈ [orig0] := (by with_unfolding_all exact ho); cases List.mem_singleton.mp h; exact span0_binds)
    (by intro s hs; cases hs)
    (by intro o ho; have h : o ∈ [orig0] := (by with_unfolding_all exact ho); cases List.mem_singleton.mp h; exact span0_binds)
    (by intro cls h; cases List.mem_singleton.mp h; exact .nil)
    (by intro locs g h; cases List.mem_singleton.mp h; exact .nil)
    (.cons nofun .nil)
    ⟨_, List.mem_singleton_self _, rfl, rfl⟩
    (by intro b hb e he; cases List.mem_singleton.mp hb; cases he)
    (by intro b hb v hv; cases List.mem_singleton.mp hb; cases hv; rfl)
    (by intro b hb op hop; cases List.mem_singleton.mp hb; cases hop)

private theorem checkedAddBIR_wf (a b : UInt8) (profile : Profile)
    (hReg : (checkedAddBIR a b profile).checkRegisterFormation (language := s0Lang) = true)
    (hSlots : 2 ≤ profile.maxBIRSlots) :
    BIR.WF (language := s0Lang) (checkedAddBIR a b profile) := by
  refine bir_wf_single_simple ⟨[], some u8Ty⟩ _ #[⟨.overflow, orig0⟩]
    hReg
    (by
      intro fn hm blk hb slot ht
      cases List.mem_singleton.mp hm
      simp only [List.mem_cons, List.not_mem_nil, or_false] at hb
      rcases hb with rfl | rfl | rfl
      · cases ht
      · cases BIRTerm.trap.inj ht; exact ⟨⟨.overflow, orig0⟩, rfl⟩
      · cases ht)
    (by change 2 ≤ profile.maxBIRSlots; exact hSlots)
    (by
      intro o ho
      have h : o ∈ [orig0, orig0] := (by with_unfolding_all exact ho)
      simp only [List.mem_cons, List.not_mem_nil, or_false] at h
      rcases h with rfl | rfl <;> exact span0_binds)
    (by intro s hs; cases List.mem_singleton.mp hs; exact span0_binds)
    (by
      intro o ho
      have h : o ∈ [orig0, orig0] := (by with_unfolding_all exact ho)
      simp only [List.mem_cons, List.not_mem_nil, or_false] at h
      rcases h with rfl | rfl <;> exact span0_binds)
    (by
      intro cls h; cases List.mem_singleton.mp h
      show ([⟨0⟩, ⟨1⟩] : List PlaceId).Nodup; decide)
    (by
      intro locs g h; cases List.mem_singleton.mp h
      show ([⟨0⟩, ⟨1⟩] : List PlaceId).Nodup; decide)
    (by show ([⟨0⟩, ⟨1⟩, ⟨2⟩] : List BlockId).Nodup; decide)
    ⟨_, List.mem_cons_self, rfl, rfl⟩
    (by
      intro blk hblk e he
      simp only [List.mem_cons, List.not_mem_nil, or_false] at hblk
      rcases hblk with rfl | rfl | rfl
      · simp only [BIRTerm.successors, List.mem_cons, List.not_mem_nil, or_false] at he
        rcases he with rfl | rfl
        · exact ⟨_, List.mem_cons_of_mem _ List.mem_cons_self, rfl, rfl⟩
        · exact ⟨_, List.mem_cons_of_mem _ (List.mem_cons_of_mem _ List.mem_cons_self), rfl, rfl⟩
      · cases he
      · cases he)
    (by
      intro blk hblk v hv
      simp only [List.mem_cons, List.not_mem_nil, or_false] at hblk
      rcases hblk with rfl | rfl | rfl
      · cases hv
      · cases hv
      · cases hv; rfl)
    (by
      intro blk hblk op hop _ _ _ hC
      simp only [List.mem_cons, List.not_mem_nil, or_false] at hblk
      rcases hblk with rfl | rfl | rfl
      · cases List.mem_singleton.mp hop; cases hC
      · cases hop
      · cases hop)

private theorem widthArithBIR_wf (t : Core.IntTy) (profile : Profile)
    (hReg : (widthArithBIR t profile).checkRegisterFormation (language := s0Lang) = true)
    (hSlots : 1 ≤ profile.maxBIRSlots) :
    BIR.WF (language := s0Lang) (widthArithBIR t profile) := by
  refine bir_wf_single_simple ⟨[], some u8Ty⟩ _ #[]
    hReg
    (by intro fn hm blk hb slot ht; cases List.mem_singleton.mp hm; cases List.mem_singleton.mp hb; cases ht)
    (by change 1 ≤ profile.maxBIRSlots; exact hSlots)
    (by intro o ho; have h : o ∈ [orig0] := (by with_unfolding_all exact ho); cases List.mem_singleton.mp h; exact span0_binds)
    (by intro s hs; cases hs)
    (by intro o ho; have h : o ∈ [orig0] := (by with_unfolding_all exact ho); cases List.mem_singleton.mp h; exact span0_binds)
    (by intro cls h; cases List.mem_singleton.mp h; exact .cons nofun .nil)
    (by intro locs g h; cases List.mem_singleton.mp h; exact .cons nofun .nil)
    (.cons nofun .nil)
    ⟨_, List.mem_singleton_self _, rfl, rfl⟩
    (by intro blk hblk e he; cases List.mem_singleton.mp hblk; cases he)
    (by intro blk hblk v hv; cases List.mem_singleton.mp hblk; cases hv; rfl)
    (by intro blk hblk op hop _ _ _ hC; cases List.mem_singleton.mp hblk; cases List.mem_singleton.mp hop; cases hC)

private theorem entryLoopBIR_wf (profile : Profile)
    (hReg : (entryLoopBIR profile).checkRegisterFormation (language := s0Lang) = true) :
    BIR.WF (language := s0Lang) (entryLoopBIR profile) := by
  refine bir_wf_single_simple ⟨[], none⟩ _ #[]
    hReg
    (by intro fn hm blk hb slot ht; cases List.mem_singleton.mp hm; cases List.mem_singleton.mp hb; cases ht)
    (Nat.zero_le _)
    (by intro o ho; have h : o ∈ ([] : List Origin) := (by with_unfolding_all exact ho); cases h)
    (by intro s hs; cases hs)
    (by intro o ho; have h : o ∈ ([] : List Origin) := (by with_unfolding_all exact ho); cases h)
    (by intro cls h; cases List.mem_singleton.mp h; exact .nil)
    (by intro locs g h; cases List.mem_singleton.mp h; exact .nil)
    (.cons nofun .nil)
    ⟨_, List.mem_singleton_self _, rfl, rfl⟩
    (by
      intro blk hblk e he
      cases List.mem_singleton.mp hblk
      cases List.mem_singleton.mp he
      exact ⟨_, List.mem_singleton_self _, rfl, rfl⟩)
    (by intro blk hblk v hv; cases List.mem_singleton.mp hblk; cases hv)
    (by intro blk hblk op hop; cases List.mem_singleton.mp hblk; cases hop)

private theorem sameTargetBranchBIR_wf (flag : Bool) (profile : Profile)
    (hReg : (sameTargetBranchBIR flag profile).checkRegisterFormation (language := s0Lang) = true)
    (hSlots : 3 ≤ profile.maxBIRSlots) :
    BIR.WF (language := s0Lang) (sameTargetBranchBIR flag profile) := by
  refine bir_wf_single_simple ⟨[], some u8Ty⟩ _ #[]
    hReg
    (by
      intro fn hm blk hb slot ht
      cases List.mem_singleton.mp hm
      simp only [List.mem_cons, List.not_mem_nil, or_false] at hb
      rcases hb with rfl | rfl <;> cases ht)
    (by change 3 ≤ profile.maxBIRSlots; exact hSlots)
    (by intro o ho; have h : o ∈ [orig0] := (by with_unfolding_all exact ho); cases List.mem_singleton.mp h; exact span0_binds)
    (by intro s hs; cases hs)
    (by intro o ho; have h : o ∈ [orig0] := (by with_unfolding_all exact ho); cases List.mem_singleton.mp h; exact span0_binds)
    (by
      intro cls h; cases List.mem_singleton.mp h
      show ([⟨1⟩, ⟨2⟩, ⟨0⟩] : List PlaceId).Nodup; decide)
    (by
      intro locs g h; cases List.mem_singleton.mp h
      show ([⟨1⟩, ⟨2⟩, ⟨0⟩] : List PlaceId).Nodup; decide)
    (by show ([⟨0⟩, ⟨1⟩] : List BlockId).Nodup; decide)
    ⟨_, List.mem_cons_self, rfl, rfl⟩
    (by
      intro blk hblk e he
      simp only [List.mem_cons, List.not_mem_nil, or_false] at hblk
      rcases hblk with rfl | rfl
      · simp only [BIRTerm.successors, List.mem_cons, List.not_mem_nil, or_false] at he
        rcases he with rfl | rfl
        · exact ⟨_, List.mem_cons_of_mem _ List.mem_cons_self, rfl, rfl⟩
        · exact ⟨_, List.mem_cons_of_mem _ List.mem_cons_self, rfl, rfl⟩
      · cases he)
    (by
      intro blk hblk v hv
      simp only [List.mem_cons, List.not_mem_nil, or_false] at hblk
      rcases hblk with rfl | rfl
      · cases hv
      · cases hv; rfl)
    (by
      intro blk hblk op hop _ _ _ hC
      simp only [List.mem_cons, List.not_mem_nil, or_false] at hblk
      rcases hblk with rfl | rfl
      · cases List.mem_singleton.mp hop; cases hC
      · cases hop)

private theorem nineParamBIR_wf (profile : Profile)
    (hReg : (nineParamBIR profile).checkRegisterFormation (language := s0Lang) = true)
    (hSlots : 9 ≤ profile.maxBIRSlots) :
    BIR.WF (language := s0Lang) (nineParamBIR profile) := by
  refine bir_wf_single_simple ⟨List.replicate 9 u8Ty, some u8Ty⟩ _ #[]
    hReg
    (by intro fn hm blk hb slot ht; cases List.mem_singleton.mp hm; cases List.mem_singleton.mp hb; cases ht)
    (by change 9 ≤ profile.maxBIRSlots; exact hSlots)
    (by intro o ho; have h : o ∈ [orig0] := (by with_unfolding_all exact ho); cases List.mem_singleton.mp h; exact span0_binds)
    (by intro s hs; cases hs)
    (by intro o ho; have h : o ∈ [orig0] := (by with_unfolding_all exact ho); cases List.mem_singleton.mp h; exact span0_binds)
    (by
      intro cls h; cases List.mem_singleton.mp h
      show ([⟨0⟩, ⟨1⟩, ⟨2⟩, ⟨3⟩, ⟨4⟩, ⟨5⟩, ⟨6⟩, ⟨7⟩, ⟨8⟩] : List PlaceId).Nodup; decide)
    (by
      intro locs g h; cases List.mem_singleton.mp h
      show ([⟨0⟩, ⟨1⟩, ⟨2⟩, ⟨3⟩, ⟨4⟩, ⟨5⟩, ⟨6⟩, ⟨7⟩, ⟨8⟩] : List PlaceId).Nodup; decide)
    (.cons nofun .nil)
    ⟨_, List.mem_singleton_self _, rfl, rfl⟩
    (by intro blk hblk e he; cases List.mem_singleton.mp hblk; cases he)
    (by intro blk hblk v hv; cases List.mem_singleton.mp hblk; cases hv; rfl)
    (by intro blk hblk op hop; cases List.mem_singleton.mp hblk; cases hop)

private theorem unreachableHelperBIR_wf (profile : Profile)
    (hReg : (unreachableHelperBIR profile).checkRegisterFormation (language := s0Lang) = true)
    (hSlots : 1 ≤ profile.maxBIRSlots) :
    BIR.WF (language := s0Lang) (unreachableHelperBIR profile) := by
  have hNdFns : [mainFn, helperFn].Nodup :=
    .cons (fun a ha hEq => helperFn_ne_mainFn ((List.mem_singleton.mp ha).symm.trans hEq.symm))
      (.cons nofun .nil)
  refine ⟨(BIR.checkRegisterFormation_eq_true_iff _).mp hReg,
    (by
      intro fn hm blk hb slot ht
      simp only [unreachableHelperBIR, List.mem_cons, List.not_mem_nil, or_false] at hm
      rcases hm with rfl | rfl <;> (cases List.mem_singleton.mp hb; cases ht)),
    (by change 1 ≤ profile.maxBIRSlots; exact hSlots),
    (by
      intro o ho
      have h : o ∈ [orig0, origHelper] := (by with_unfolding_all exact ho)
      simp only [List.mem_cons, List.not_mem_nil, or_false] at h
      rcases h with rfl | rfl <;> exact span0_binds),
    (by intro s hs; cases hs), ?_⟩
  refine ⟨⟨hNdFns, hNdFns, hNdFns, .nil, ?_, nofun, ?_⟩,
    List.mem_cons_self, ?_, nofun, ⟨.nil, declsOk_nil, nofun⟩, ?_⟩
  · intro fn cls hm
    have hm' : (fn, cls) ∈ [(mainFn, []), (helperFn, [(⟨0⟩, StorageClass.virtual)])] := by
      with_unfolding_all exact hm
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hm'
    rcases hm' with h | h
    · cases h; exact .nil
    · cases h; exact .cons nofun .nil
  · intro fn locals graph hm
    have hm' : (fn, locals, graph) ∈
        ((unreachableHelperBIR profile).memoryView (language := s0Lang)).functions := hm
    have hFns : ((unreachableHelperBIR profile).memoryView (language := s0Lang)).functions =
        [(mainFn, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩]⟩),
         (helperFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[⟨⟨0⟩, [],
            [⟨origHelper, .call mainFn (some ⟨u8Ty, ⟨⟨⟨0⟩, []⟩, .virtual⟩⟩) []⟩],
            .ret (some ⟨u8Ty, .copy ⟨⟨⟨0⟩, []⟩, .virtual⟩⟩), []⟩]⟩)] := by
      with_unfolding_all rfl
    rw [hFns] at hm'
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hm'
    rcases hm' with h | h
    · cases h
      refine ⟨.nil, ⟨.cons nofun .nil, ⟨_, List.mem_singleton_self _, rfl⟩,
          fun b hb e he => by cases List.mem_singleton.mp hb; cases he⟩,
        ⟨⟨[], some u8Ty⟩, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩,
          fun b hb => by
            cases List.mem_singleton.mp hb
            exact ⟨rfl, fun op hop => by cases hop⟩⟩⟩
    · cases h
      refine ⟨.cons nofun .nil, ⟨.cons nofun .nil, ⟨_, List.mem_singleton_self _, rfl⟩,
          fun b hb e he => by cases List.mem_singleton.mp hb; cases he⟩,
        ⟨⟨[], some u8Ty⟩, by with_unfolding_all rfl,
          ⟨_, List.mem_singleton_self _, rfl, rfl⟩,
          fun b hb => by
            cases List.mem_singleton.mp hb
            refine ⟨rfl, fun op hop => ?_⟩
            cases List.mem_singleton.mp hop
            exact ⟨⟨[], some u8Ty⟩, rfl, rfl, rfl⟩⟩⟩
  · intro o ho
    have h : o ∈ [orig0, origHelper] := by with_unfolding_all exact ho
    simp only [List.mem_cons, List.not_mem_nil, or_false] at h
    rcases h with rfl | rfl <;> exact span0_binds
  · refine ⟨.cons nofun .nil, fun fn roots hm => ?_⟩
    cases List.mem_singleton.mp hm
    exact ⟨_, _, List.mem_cons_of_mem _ List.mem_cons_self, fun r hr => by
      cases List.mem_singleton.mp hr
      exact ⟨u8Ty, List.mem_singleton_self _⟩⟩

/-! ### Explicit emitted LCB bodies for fast constructive execution proofs -/

private def checkedAddLCBBody (x y : Int) : Source.Function :=
  { resultWidth := .word
    parameters := #[]
    valueWidths := Array.replicate 6 .word
    blocks := #[
      { phis := #[]
        instructions := (checkedOpExpansion .add .u8 0
          (.literal (word x)) (.literal (word y))).toArray
        terminator := .branch (.local 5) 1 2 },
      { phis := #[]
        instructions := #[]
        terminator := .ret (.literal (trapWord 0)) },
      { phis := #[]
        instructions := #[]
        terminator := .ret (.local 3) }] }

private def entryLoopLCBBody : Source.Function :=
  { resultWidth := .word
    parameters := #[]
    valueWidths := #[]
    blocks := #[
      { phis := #[], instructions := #[], terminator := .jump 1 },
      { phis := #[], instructions := #[], terminator := .jump 1 }] }

private def sameTargetBranchLCBBody (x : Int) : Source.Function :=
  { resultWidth := .word
    parameters := #[]
    valueWidths := Array.replicate 7 .word
    blocks := #[
      { phis := #[]
        instructions := (checkedOpExpansion .add .u8 0
          (.literal (word x)) (.literal (word 1))).toArray
        terminator := .branch (.local 5) 1 2 },
      { phis := #[{ destination := 6, width := .word,
                    incoming := #[(0, .literal (word 42)), (2, .literal (word 7))] }]
        instructions := #[]
        terminator := .ret (.local 6) },
      { phis := #[]
        instructions := #[]
        terminator := .jump 1 }] }

private theorem retBody_observes {profile : Profile} (out : LCBInput profile)
    {n : Int} (hn : Core.IntTy.u8.InRange n) (hBody : out.body = retBody n) :
    (LCBInput.Execution s0Lang.operands.Event tabProject).observes out
      (.returned (UInt8.ofNat n.toNat) ByteArray.empty) := by
  have hInit : Source.initial out.body #[] = .ok ⟨0, 0, #[]⟩ := by
    rw [hBody]
    exact initial_no_params (f := retBody n)
      (entry := { phis := #[], instructions := #[], terminator := .ret (.literal (word n)) })
      rfl rfl rfl
  have hRet : Source.step out.body ⟨0, 0, #[]⟩ = .ok (.returned (word n)) := by
    rw [hBody]
    exact step_ret (f := retBody n) (s := ⟨0, 0, #[]⟩)
      (lb := { phis := #[], instructions := #[], terminator := .ret (.literal (word n)) })
      rfl rfl rfl (read_word_literal #[] n)
  exact lcb_observes_u8 tabProject out hn hInit (.zero _) hRet

private theorem checkedAddLCB_block0_steps {x y : Int}
    (hx : Core.IntTy.u8.InRange x) (hy : Core.IntTy.u8.InRange y) :
    ∃ u₄, Source.Steps (checkedAddLCBBody x y) 6
      ⟨0, 0, Array.replicate 6 none⟩
      ⟨0, 6,
        ((set4 (Array.replicate 6 none) 0
          (some (.word (raw .add x y)))
          (some (.word (biased .u8 (raw .add x y))))
          (some (.word (masked .u8 (biased .u8 (raw .add x y)))))
          (some (word (wrap .u8 (x + y))))).set! 4 u₄).set!
          5 (some (boolWord (decide (checked .u8 .add x y = .error .overflow))))⟩ := by
  let f := checkedAddLCBBody x y
  let lb0 : Source.Block :=
    { phis := #[]
      instructions := (checkedOpExpansion .add .u8 0
        (.literal (word x)) (.literal (word y))).toArray
      terminator := .branch (.local 5) 1 2 }
  have hq : ∀ k, k < 6 → lb0.instructions[0 + k]? =
      (checkedOpExpansion .add .u8 0 (.literal (word x)) (.literal (word y)))[k]? := by
    intro k _
    rw [Nat.zero_add]
    exact List.getElem?_toArray
  have hW : ∀ m, m < (Array.replicate 6 (none : Option Source.Value)).size →
      f.valueWidths[m]? = some .word := by
    intro m hm
    simp only [Array.size_replicate] at hm
    simp [f, checkedAddLCBBody, hm]
  exact checkedAdd_steps (f := f) (s := ⟨0, 0, Array.replicate 6 none⟩) (lb := lb0) (n := 0)
    (t := .u8) rfl hx hy rfl hq (read_word_literal _ x) (read_word_literal _ y) hW
    (by simp)

/-! ### 2. Reachable whole-program witnesses through `(registry (prodPasses p)).emit` -/

-- Witness 1a: `return 42` through registered LCB emitter observes exit 42 in both BIR and LCB
-- and establishes `EmitsLCB`.
theorem d5_witness_1a_return42_lcb :
    ∃ out : LCBInput lcbLinux,
      (registry (prodPasses lcbLinux)).emit.run (returnBIR 42 lcbLinux) = .ok out ∧
      EmitsLCB s0Lang (returnBIR 42 lcbLinux) out ∧
      (BIR.Execution (language := s0Lang)).observes (returnBIR 42 lcbLinux) (.returned 42 ByteArray.empty) ∧
      (LCBInput.Execution s0Lang.operands.Event tabProject).observes out (.returned 42 ByteArray.empty) := by
  let out : LCBInput lcbLinux := ⟨retBody 42, [], #[], EmitLCB.runtimeBytes []⟩
  have hUnval : EmitLCB.emitUnvalidated (language := s0Lang) (returnBIR 42 lcbLinux) = .ok out := rfl
  have hVal : EmitLCB.validateJobs out = .ok () := by
    unfold EmitLCB.validateJobs
    change validateJob (LCBJob.lcbFunctionJob (retBody 42)) >>= (fun _ => .ok ()) = .ok ()
    rw [validateJob_ret42]; rfl
  have hEmit : EmitLCB.emit (language := s0Lang) (returnBIR 42 lcbLinux) = .ok out := by
    unfold EmitLCB.emit
    rw [hUnval]
    dsimp only [bind, Except.bind, Except.mapError, pure, Except.pure]
    rw [hVal]
  have hRun : (registry (prodPasses lcbLinux)).emit.run (returnBIR 42 lcbLinux) = .ok out := by
    change emitGuarded (language := s0Lang) (prodPasses lcbLinux).emit (returnBIR 42 lcbLinux) = .ok out
    rw [show (prodPasses lcbLinux).emit = ValidateEmit.emit (language := s0Lang) from rfl,
      emitGuarded_emit_lcb]
    exact hEmit
  have hEmits : EmitsLCB s0Lang (returnBIR 42 lcbLinux) out :=
    (emitLCB_emitsLCB (returnBIR_wf 42 lcbLinux (by decide)) hEmit).2
  have hBirObs : (BIR.Execution (language := s0Lang)).observes (returnBIR 42 lcbLinux)
      (.returned 42 ByteArray.empty) :=
    ⟨BIR.entryState (language := s0Lang) (returnBIR 42 lcbLinux),
      ⟨_, rfl, .nil _⟩, by with_unfolding_all rfl⟩
  have hLcbObs : (LCBInput.Execution s0Lang.operands.Event tabProject).observes out
      (.returned 42 ByteArray.empty) :=
    retBody_observes out (by decide) rfl
  exact ⟨out, hRun, hEmits, hBirObs, hLcbObs⟩

-- Witness 1b: `return 42` through registered LLVM emitter on `x86_64Linux` vs `aarch64Macos`
-- establishes `LLVMMetadataMatches`, `LauncherMatches`, `ComponentOwnsSymbols`, and distinct
-- target headers on BOTH `.component` and `.launcher`.
theorem d5_witness_1b_return42_llvm_targets :
    ∃ (outLinux : LLVMInput llvmLinux) (outMac : LLVMInput llvmMac),
      (registry (prodPasses llvmLinux)).emit.run (returnBIR 42 llvmLinux) = .ok outLinux ∧
      (registry (prodPasses llvmMac)).emit.run (returnBIR 42 llvmMac) = .ok outMac ∧
      LLVMMetadataMatches (returnBIR 42 llvmLinux) outLinux ∧
      LLVMMetadataMatches (returnBIR 42 llvmMac) outMac ∧
      outLinux.packageBytes.component ≠ outMac.packageBytes.component ∧
      outLinux.packageBytes.launcher ≠ outMac.packageBytes.launcher := by
  obtain ⟨outLinux, hOutL⟩ : ∃ o, EmitLLVM.emit (language := s0Lang) (returnBIR 42 llvmLinux) = .ok o :=
    ⟨_, rfl⟩
  obtain ⟨outMac, hOutM⟩ : ∃ o, EmitLLVM.emit (language := s0Lang) (returnBIR 42 llvmMac) = .ok o :=
    ⟨_, rfl⟩
  have hRunL : (registry (prodPasses llvmLinux)).emit.run (returnBIR 42 llvmLinux) = .ok outLinux := by
    change emitGuarded (language := s0Lang) (prodPasses llvmLinux).emit (returnBIR 42 llvmLinux) = .ok outLinux
    rw [show (prodPasses llvmLinux).emit = ValidateEmit.emit (language := s0Lang) from rfl,
      emitGuarded_emit_llvm]
    exact hOutL
  have hRunM : (registry (prodPasses llvmMac)).emit.run (returnBIR 42 llvmMac) = .ok outMac := by
    change emitGuarded (language := s0Lang) (prodPasses llvmMac).emit (returnBIR 42 llvmMac) = .ok outMac
    rw [show (prodPasses llvmMac).emit = ValidateEmit.emit (language := s0Lang) from rfl,
      emitGuarded_emit_llvm]
    exact hOutM
  exact ⟨outLinux, outMac, hRunL, hRunM,
    EmitLLVM.emit_metadataMatches hOutL, EmitLLVM.emit_metadataMatches hOutM,
    llvm_linux_ne_mac_bytesAt outLinux outMac .component,
    llvm_linux_ne_mac_bytesAt outLinux outMac .launcher⟩

-- Witness 2a: Two-result `u8` `addWithOverflow` (`20 + 22`) on LCB (`LCBEncoding` + exact exit 42)
-- and LLVM (`LLVMMetadataMatches` + `@llvm.uadd.with.overflow.i8` declaration).
theorem d5_witness_2a_checkedAdd_ok :
    ∃ (outLCB : LCBInput lcbLinux) (outLLVM : LLVMInput llvmMac),
      EmitLCB.emitUnvalidated (language := s0Lang) (checkedAddBIR 20 22 lcbLinux) = .ok outLCB ∧
      LCBEncoding s0Lang (checkedAddBIR 20 22 lcbLinux) outLCB ∧
      (LCBInput.Execution s0Lang.operands.Event tabProject).observes outLCB
        (.returned 42 ByteArray.empty) ∧
      (registry (prodPasses llvmMac)).emit.run (checkedAddBIR 20 22 llvmMac) = .ok outLLVM ∧
      LLVMMetadataMatches (checkedAddBIR 20 22 llvmMac) outLLVM ∧
      (.declare (.overflowPair .u8) (.overflow .add .u8) [.integer .u8, .integer .u8] ∈
        outLLVM.component.items) := by
  let outLCB : LCBInput lcbLinux :=
    ⟨checkedAddLCBBody 20 22, [], #[⟨.overflow, orig0⟩], EmitLCB.runtimeBytes [expectedOverflowLine]⟩
  have hOutLCB : EmitLCB.emitUnvalidated (language := s0Lang) (checkedAddBIR 20 22 lcbLinux) = .ok outLCB := rfl
  have hEnc := emitLCB_encoding (checkedAddBIR_wf 20 22 lcbLinux (by decide) (by decide)) hOutLCB
  have hLcbObs : (LCBInput.Execution s0Lang.operands.Event tabProject).observes outLCB
      (.returned 42 ByteArray.empty) := by
    have hInit : Source.initial outLCB.body #[] = .ok ⟨0, 0, Array.replicate 6 none⟩ :=
      initial_no_params (f := checkedAddLCBBody 20 22) rfl rfl rfl
    obtain ⟨u₄, hSteps6⟩ := checkedAddLCB_block0_steps (x := 20) (y := 22) (by decide) (by decide)
    let env6 := ((set4 (Array.replicate 6 none) 0
      (some (.word (raw .add 20 22)))
      (some (.word (biased .u8 (raw .add 20 22))))
      (some (.word (masked .u8 (biased .u8 (raw .add 20 22)))))
      (some (word (wrap .u8 (20 + 22))))).set! 4 u₄).set!
      5 (some (boolWord (decide (checked .u8 .add 20 22 = .error .overflow))))
    have hFlagFalse : decide (checked .u8 .add 20 22 = .error .overflow) = false := by decide
    have hWrap42 : wrap .u8 (20 + 22) = 42 := by decide
    have hEnvSz4 : (set4 (Array.replicate 6 none) 0
      (some (.word (raw .add 20 22)))
      (some (.word (biased .u8 (raw .add 20 22))))
      (some (.word (masked .u8 (biased .u8 (raw .add 20 22)))))
      (some (word (wrap .u8 (20 + 22))))).size = 6 := by
      rw [size_set4]; simp
    have hRdFlag : Source.read env6 .word (.local 5) = .ok (boolWord false) := by
      dsimp only [env6]
      rw [hFlagFalse]
      exact read_set_self (by rw [size_set!, hEnvSz4]; omega) rfl
    have hBr : Source.Step outLCB.body ⟨0, 6, env6⟩ ⟨2, 0, env6⟩ :=
      step_branch (f := checkedAddLCBBody 20 22) (s := ⟨0, 6, env6⟩) rfl rfl rfl hRdFlag
        (by
          rw [boolWord_branch]
          exact transfer_no_phis (f := checkedAddLCBBody 20 22) rfl rfl)
    have hRdRes : Source.read env6 .word (.local 3) = .ok (word 42) := by
      dsimp only [env6, set4]
      rw [read_set_other (by omega), read_set_other (by omega), hWrap42]
      exact read_set_self (by rw [size_set!, size_set!, size_set!]; simp) rfl
    have hRet : Source.step outLCB.body ⟨2, 0, env6⟩ = .ok (.returned (word 42)) :=
      step_ret (f := checkedAddLCBBody 20 22) (s := ⟨2, 0, env6⟩) rfl rfl rfl hRdRes
    exact lcb_observes_u8 tabProject outLCB (n := 42) (by decide) hInit
      (steps_append hSteps6 (.succ hBr (.zero _))) hRet
  obtain ⟨outLLVM, hOutLLVM⟩ :
      ∃ o, EmitLLVM.emit (language := s0Lang) (checkedAddBIR 20 22 llvmMac) = .ok o :=
    ⟨_, rfl⟩
  have hRunLLVM : (registry (prodPasses llvmMac)).emit.run (checkedAddBIR 20 22 llvmMac) = .ok outLLVM := by
    change emitGuarded (language := s0Lang) (prodPasses llvmMac).emit (checkedAddBIR 20 22 llvmMac) = .ok outLLVM
    rw [show (prodPasses llvmMac).emit = ValidateEmit.emit (language := s0Lang) from rfl,
      emitGuarded_emit_llvm]
    exact hOutLLVM
  have hDeclMem : .declare (.overflowPair .u8) (.overflow .add .u8) [.integer .u8, .integer .u8] ∈
      outLLVM.component.items := by
    cases Except.ok.inj hOutLLVM
    exact List.mem_cons_self
  exact ⟨outLCB, outLLVM, hOutLCB, hEnc, hLcbObs, hRunLLVM,
    EmitLLVM.emit_metadataMatches hOutLLVM, hDeclMem⟩

-- Witness 2b: Overflowing `u8` `addWithOverflow` (`255 + 1`) observes trap 134 with exact stderr
-- `concrete: trap: overflow at src/main.con:1:1 (site 0)\n` on LCB and exact `launcherItems` on LLVM.
theorem d5_witness_2b_checkedAdd_overflow_trap :
    ∃ (outLCB : LCBInput lcbLinux) (outLLVM : LLVMInput llvmMac),
      EmitLCB.emitUnvalidated (language := s0Lang) (checkedAddBIR 255 1 lcbLinux) = .ok outLCB ∧
      LCBEncoding s0Lang (checkedAddBIR 255 1 lcbLinux) outLCB ∧
      (LCBInput.Execution s0Lang.operands.Event tabProject).observes outLCB
        (.trapped 134 .overflow expectedTrapSite expectedOverflowLine.toUTF8) ∧
      (registry (prodPasses llvmMac)).emit.run (checkedAddBIR 255 1 llvmMac) = .ok outLLVM ∧
      LLVMMetadataMatches (checkedAddBIR 255 1 llvmMac) outLLVM ∧
      outLLVM.launcher.items = launcherItems [expectedOverflowLine] := by
  let outLCB : LCBInput lcbLinux :=
    ⟨checkedAddLCBBody 255 1, [], #[⟨.overflow, orig0⟩], EmitLCB.runtimeBytes [expectedOverflowLine]⟩
  have hOutLCB : EmitLCB.emitUnvalidated (language := s0Lang) (checkedAddBIR 255 1 lcbLinux) = .ok outLCB := rfl
  have hEnc := emitLCB_encoding (checkedAddBIR_wf 255 1 lcbLinux (by decide) (by decide)) hOutLCB
  have hLcbObs : (LCBInput.Execution s0Lang.operands.Event tabProject).observes outLCB
      (.trapped 134 .overflow expectedTrapSite expectedOverflowLine.toUTF8) := by
    have hInit : Source.initial outLCB.body #[] = .ok ⟨0, 0, Array.replicate 6 none⟩ :=
      initial_no_params (f := checkedAddLCBBody 255 1) rfl rfl rfl
    obtain ⟨u₄, hSteps6⟩ := checkedAddLCB_block0_steps (x := 255) (y := 1) (by decide) (by decide)
    let env6 := ((set4 (Array.replicate 6 none) 0
      (some (.word (raw .add 255 1)))
      (some (.word (biased .u8 (raw .add 255 1))))
      (some (.word (masked .u8 (biased .u8 (raw .add 255 1)))))
      (some (word (wrap .u8 (255 + 1))))).set! 4 u₄).set!
      5 (some (boolWord (decide (checked .u8 .add 255 1 = .error .overflow))))
    have hFlagTrue : decide (checked .u8 .add 255 1 = .error .overflow) = true := by decide
    have hEnvSz4 : (set4 (Array.replicate 6 none) 0
      (some (.word (raw .add 255 1)))
      (some (.word (biased .u8 (raw .add 255 1))))
      (some (.word (masked .u8 (biased .u8 (raw .add 255 1)))))
      (some (word (wrap .u8 (255 + 1))))).size = 6 := by
      rw [size_set4]; simp
    have hRdFlag : Source.read env6 .word (.local 5) = .ok (boolWord true) := by
      dsimp only [env6]
      rw [hFlagTrue]
      exact read_set_self (by rw [size_set!, hEnvSz4]; omega) rfl
    have hBr : Source.Step outLCB.body ⟨0, 6, env6⟩ ⟨1, 0, env6⟩ :=
      step_branch (f := checkedAddLCBBody 255 1) (s := ⟨0, 6, env6⟩) rfl rfl rfl hRdFlag
        (by
          rw [boolWord_branch]
          exact transfer_no_phis (f := checkedAddLCBBody 255 1) rfl rfl)
    have hRet : Source.step outLCB.body ⟨1, 0, env6⟩ = .ok (.returned (trapWord 0)) :=
      step_ret (f := checkedAddLCBBody 255 1) (s := ⟨1, 0, env6⟩) rfl rfl rfl
        (read_literal env6 (trapWordBits 0))
    have hObs := lcb_observes_trap (E := s0Lang.operands.Event) tabProject outLCB (slot := 0)
      (entry := ⟨.overflow, orig0⟩) (site := expectedTrapSite) (by decide) hInit
      (steps_append hSteps6 (.succ hBr (.zero _))) hRet rfl runtimeSite_orig0
    simpa only [renderTrapLine_overflow0] using hObs
  obtain ⟨outLLVM, hOutLLVM⟩ :
      ∃ o, EmitLLVM.emit (language := s0Lang) (checkedAddBIR 255 1 llvmMac) = .ok o :=
    ⟨_, rfl⟩
  have hRunLLVM : (registry (prodPasses llvmMac)).emit.run (checkedAddBIR 255 1 llvmMac) = .ok outLLVM := by
    change emitGuarded (language := s0Lang) (prodPasses llvmMac).emit (checkedAddBIR 255 1 llvmMac) = .ok outLLVM
    rw [show (prodPasses llvmMac).emit = ValidateEmit.emit (language := s0Lang) from rfl,
      emitGuarded_emit_llvm]
    exact hOutLLVM
  have hMeta := EmitLLVM.emit_metadataMatches hOutLLVM
  have hLaunch : outLLVM.launcher.items = launcherItems [expectedOverflowLine] := by
    obtain ⟨diags, hDiags, _, hItems, _⟩ := hMeta.launcherMatches
    have hD : diags = [expectedOverflowLine] := Option.some.inj (hDiags.symm.trans rfl)
    rw [hItems, hD]
  exact ⟨outLCB, outLLVM, hOutLCB, hEnc, hLcbObs, hRunLLVM, hMeta, hLaunch⟩

-- Witness 3 (`ISS-0117` width-policy contrast): well-formed `u32` arithmetic is ACCEPTED by LLVM
-- and REFUSED by LCB with `.unsupportedFeature` citing `BackendPolicy.intWidthRule`, while
-- `u8`, `i8`, `u16`, and `i16` are all ACCEPTED by LCB with `LCBEncoding`.
theorem d5_witness_3_width_policy_iss0117 :
    (∃ outLLVM, (registry (prodPasses llvmLinux)).emit.run (widthArithBIR .u32 llvmLinux) = .ok outLLVM ∧
      LLVMMetadataMatches (widthArithBIR .u32 llvmLinux) outLLVM) ∧
    (registry (prodPasses lcbLinux)).emit.run (widthArithBIR .u32 lcbLinux) =
      .error (.unsupportedFeature lcbLinux (.unsupportedWidth .u32 mainFn (.instruction ⟨0⟩ 0))) ∧
    FeatureCause.rule (profile := lcbLinux) (.unsupportedWidth .u32 mainFn (.instruction ⟨0⟩ 0)) =
      BackendPolicy.intWidthRule ∧
    (∀ t ∈ ([.u8, .i8, .u16, .i16] : List Core.IntTy),
      ∃ outLCB, EmitLCB.emitUnvalidated (language := s0Lang) (widthArithBIR t lcbLinux) = .ok outLCB ∧
        LCBEncoding s0Lang (widthArithBIR t lcbLinux) outLCB) := by
  obtain ⟨outLLVM, hOutLLVM⟩ :
      ∃ o, EmitLLVM.emit (language := s0Lang) (widthArithBIR .u32 llvmLinux) = .ok o :=
    ⟨_, rfl⟩
  have hRunLLVM : (registry (prodPasses llvmLinux)).emit.run (widthArithBIR .u32 llvmLinux) = .ok outLLVM := by
    change emitGuarded (language := s0Lang) (prodPasses llvmLinux).emit (widthArithBIR .u32 llvmLinux) = .ok outLLVM
    rw [show (prodPasses llvmLinux).emit = ValidateEmit.emit (language := s0Lang) from rfl,
      emitGuarded_emit_llvm]
    exact hOutLLVM
  have hRefuseLCB : (registry (prodPasses lcbLinux)).emit.run (widthArithBIR .u32 lcbLinux) =
      .error (.unsupportedFeature lcbLinux (.unsupportedWidth .u32 mainFn (.instruction ⟨0⟩ 0))) := by
    change emitGuarded (language := s0Lang) (prodPasses lcbLinux).emit (widthArithBIR .u32 lcbLinux) = _
    rw [show (prodPasses lcbLinux).emit = ValidateEmit.emit (language := s0Lang) from rfl,
      emitGuarded_emit_lcb]
    rfl
  refine ⟨⟨outLLVM, hRunLLVM, EmitLLVM.emit_metadataMatches hOutLLVM⟩, hRefuseLCB, rfl, ?_⟩
  intro t ht
  simp only [List.mem_cons, List.not_mem_nil, or_false] at ht
  rcases ht with rfl | rfl | rfl | rfl
  · exact ⟨_, rfl, emitLCB_encoding (widthArithBIR_wf .u8 lcbLinux (by decide) (by decide)) rfl⟩
  · exact ⟨_, rfl, emitLCB_encoding (widthArithBIR_wf .i8 lcbLinux (by decide) (by decide)) rfl⟩
  · exact ⟨_, rfl, emitLCB_encoding (widthArithBIR_wf .u16 lcbLinux (by decide) (by decide)) rfl⟩
  · exact ⟨_, rfl, emitLCB_encoding (widthArithBIR_wf .i16 lcbLinux (by decide) (by decide)) rfl⟩

-- Witness 4: Entry zero-slot self-loop inserts synthetic trampoline block 0 and diverges in both
-- `BIR.Execution` and `LCBInput.Execution`.
theorem d5_witness_4_entry_self_loop_diverges :
    ∃ outLCB : LCBInput lcbLinux,
      EmitLCB.emitUnvalidated (language := s0Lang) (entryLoopBIR lcbLinux) = .ok outLCB ∧
      LCBEncoding s0Lang (entryLoopBIR lcbLinux) outLCB ∧
      outLCB.body.blocks.size = 2 ∧
      (∃ run, (LCBInput.Execution s0Lang.operands.Event tabProject).diverges outLCB run) ∧
      (∃ run, (BIR.Execution (language := s0Lang)).diverges (entryLoopBIR lcbLinux) run) := by
  let outLCB : LCBInput lcbLinux := ⟨entryLoopLCBBody, [], #[], EmitLCB.runtimeBytes []⟩
  have hOutLCB : EmitLCB.emitUnvalidated (language := s0Lang) (entryLoopBIR lcbLinux) = .ok outLCB := rfl
  have hEnc := emitLCB_encoding (entryLoopBIR_wf lcbLinux (by decide)) hOutLCB
  have hLcbDiv : ∃ run, (LCBInput.Execution s0Lang.operands.Event tabProject).diverges outLCB run := by
    let run : Nat → Option Source.State := fun n =>
      match n with
      | 0 => some ⟨0, 0, #[]⟩
      | _ + 1 => some ⟨1, 0, #[]⟩
    have hInit : Source.initial entryLoopLCBBody #[] = .ok ⟨0, 0, #[]⟩ :=
      initial_no_params (f := entryLoopLCBBody) rfl rfl rfl
    have hStep0 : Source.Step entryLoopLCBBody ⟨0, 0, #[]⟩ ⟨1, 0, #[]⟩ :=
      step_jump (f := entryLoopLCBBody) (s := ⟨0, 0, #[]⟩) rfl rfl rfl
        (transfer_no_phis (f := entryLoopLCBBody) rfl rfl)
    have hStep1 : Source.Step entryLoopLCBBody ⟨1, 0, #[]⟩ ⟨1, 0, #[]⟩ :=
      step_jump (f := entryLoopLCBBody) (s := ⟨1, 0, #[]⟩) rfl rfl rfl
        (transfer_no_phis (f := entryLoopLCBBody) rfl rfl)
    refine ⟨run, ?_, ?_⟩
    · change some ⟨0, 0, #[]⟩ = (Source.initial entryLoopLCBBody #[]).toOption
      rw [hInit]; rfl
    · intro i
      cases i with
      | zero => exact ⟨⟨0, 0, #[]⟩, ⟨1, 0, #[]⟩, rfl, rfl, hStep0⟩
      | succ _ => exact ⟨⟨1, 0, #[]⟩, ⟨1, 0, #[]⟩, rfl, rfl, hStep1⟩
  have hBirDiv : ∃ run, (BIR.Execution (language := s0Lang)).diverges (entryLoopBIR lcbLinux) run := by
    let p0 := BIR.entryState (language := s0Lang) (entryLoopBIR lcbLinux)
    refine ⟨fun _ => p0, rfl, fun _ => ?_⟩
    refine ⟨by with_unfolding_all rfl, ?_, ⟨[], List.append_nil _⟩⟩
    with_unfolding_all
    refine ⟨_, rfl, rfl, [], p0, _, rfl, rfl, rfl, ?_⟩
    exact .nil p0
  exact ⟨outLCB, hOutLCB, hEnc, rfl, hLcbDiv, hBirDiv⟩

private theorem sameTargetBranch_block0_steps {x : Int} (hx : Core.IntTy.u8.InRange x) :
    ∃ u₄, Source.Steps (sameTargetBranchLCBBody x) 6
      ⟨0, 0, Array.replicate 7 none⟩
      ⟨0, 6,
        ((set4 (Array.replicate 7 none) 0
          (some (.word (raw .add x 1)))
          (some (.word (biased .u8 (raw .add x 1))))
          (some (.word (masked .u8 (biased .u8 (raw .add x 1)))))
          (some (word (wrap .u8 (x + 1))))).set! 4 u₄).set!
          5 (some (boolWord (decide (checked .u8 .add x 1 = .error .overflow))))⟩ := by
  let f := sameTargetBranchLCBBody x
  let lb0 : Source.Block :=
    { phis := #[]
      instructions := (checkedOpExpansion .add .u8 0
        (.literal (word x)) (.literal (word 1))).toArray
      terminator := .branch (.local 5) 1 2 }
  have hq : ∀ k, k < 6 → lb0.instructions[0 + k]? =
      (checkedOpExpansion .add .u8 0 (.literal (word x)) (.literal (word 1)))[k]? := by
    intro k _
    rw [Nat.zero_add]
    exact List.getElem?_toArray
  have hW : ∀ m, m < (Array.replicate 7 (none : Option Source.Value)).size →
      f.valueWidths[m]? = some .word := by
    intro m hm
    simp only [Array.size_replicate] at hm
    simp [f, sameTargetBranchLCBBody, hm]
  exact checkedAdd_steps (f := f) (s := ⟨0, 0, Array.replicate 7 none⟩) (lb := lb0) (n := 0)
    (t := .u8) rfl hx (by decide) rfl hq (read_word_literal _ x) (read_word_literal _ 1) hW
    (by simp)

private theorem sameTargetBranch_observes_true :
    (LCBInput.Execution s0Lang.operands.Event tabProject).observes
      (⟨sameTargetBranchLCBBody 255, [], #[], EmitLCB.runtimeBytes []⟩ : LCBInput lcbLinux)
      (.returned 42 ByteArray.empty) := by
  let outT : LCBInput lcbLinux := ⟨sameTargetBranchLCBBody 255, [], #[], EmitLCB.runtimeBytes []⟩
  have hInit : Source.initial outT.body #[] = .ok ⟨0, 0, Array.replicate 7 none⟩ :=
    initial_no_params (f := sameTargetBranchLCBBody 255) rfl rfl rfl
  obtain ⟨u₄, hSteps6⟩ := sameTargetBranch_block0_steps (x := 255) (by decide)
  let env6 := ((set4 (Array.replicate 7 none) 0
    (some (.word (raw .add 255 1)))
    (some (.word (biased .u8 (raw .add 255 1))))
    (some (.word (masked .u8 (biased .u8 (raw .add 255 1)))))
    (some (word (wrap .u8 (255 + 1))))).set! 4 u₄).set!
    5 (some (boolWord (decide (checked .u8 .add 255 1 = .error .overflow))))
  have hFlagTrue : decide (checked .u8 .add 255 1 = .error .overflow) = true := by decide
  have hEnv6Sz : env6.size = 7 := by
    dsimp only [env6]; rw [size_set!, size_set!, size_set4]; simp
  have hRdFlag : Source.read env6 .word (.local 5) = .ok (boolWord true) := by
    dsimp only [env6]; rw [hFlagTrue]
    exact read_set_self (by rw [size_set!, size_set4]; simp) rfl
  let env7 := env6.set! 6 (some (word 42))
  have hTr1 : Source.transfer outT.body 0 1 env6 = .ok ⟨1, 0, env7⟩ :=
    transfer_one_phi (f := sameTargetBranchLCBBody 255) (pred := 0) (target := 1)
      rfl rfl rfl (read_word_literal env6 42)
      (write_ok (f := sameTargetBranchLCBBody 255) (n := 6) (by simp [sameTargetBranchLCBBody])
        (by omega) rfl)
  have hBr : Source.Step outT.body ⟨0, 6, env6⟩ ⟨1, 0, env7⟩ :=
    step_branch (f := sameTargetBranchLCBBody 255) (s := ⟨0, 6, env6⟩) rfl rfl rfl hRdFlag
      (by rw [boolWord_branch]; exact hTr1)
  have hRd6 : Source.read env7 .word (.local 6) = .ok (word 42) :=
    read_set_self (env := env6) (n := 6) (v := word 42) (by omega) rfl
  have hRet : Source.step outT.body ⟨1, 0, env7⟩ = .ok (.returned (word 42)) :=
    step_ret (f := sameTargetBranchLCBBody 255) (s := ⟨1, 0, env7⟩)
      (lb := { phis := #[{ destination := 6, width := .word,
                           incoming := #[(0, .literal (word 42)), (2, .literal (word 7))] }]
               instructions := #[]
               terminator := .ret (.local 6) })
      rfl rfl rfl hRd6
  exact lcb_observes_u8 tabProject outT (n := 42) (by decide) hInit
    (steps_append hSteps6 (.succ hBr (.zero _))) hRet

private theorem sameTargetBranch_observes_false :
    (LCBInput.Execution s0Lang.operands.Event tabProject).observes
      (⟨sameTargetBranchLCBBody 0, [], #[], EmitLCB.runtimeBytes []⟩ : LCBInput lcbLinux)
      (.returned 7 ByteArray.empty) := by
  let outF : LCBInput lcbLinux := ⟨sameTargetBranchLCBBody 0, [], #[], EmitLCB.runtimeBytes []⟩
  have hInit : Source.initial outF.body #[] = .ok ⟨0, 0, Array.replicate 7 none⟩ :=
    initial_no_params (f := sameTargetBranchLCBBody 0) rfl rfl rfl
  obtain ⟨u₄, hSteps6⟩ := sameTargetBranch_block0_steps (x := 0) (by decide)
  let env6 := ((set4 (Array.replicate 7 none) 0
    (some (.word (raw .add 0 1)))
    (some (.word (biased .u8 (raw .add 0 1))))
    (some (.word (masked .u8 (biased .u8 (raw .add 0 1)))))
    (some (word (wrap .u8 (0 + 1))))).set! 4 u₄).set!
    5 (some (boolWord (decide (checked .u8 .add 0 1 = .error .overflow))))
  have hFlagFalse : decide (checked .u8 .add 0 1 = .error .overflow) = false := by decide
  have hEnv6Sz : env6.size = 7 := by
    dsimp only [env6]; rw [size_set!, size_set!, size_set4]; simp
  have hRdFlag : Source.read env6 .word (.local 5) = .ok (boolWord false) := by
    dsimp only [env6]; rw [hFlagFalse]
    exact read_set_self (by rw [size_set!, size_set4]; simp) rfl
  have hBr : Source.Step outF.body ⟨0, 6, env6⟩ ⟨2, 0, env6⟩ :=
    step_branch (f := sameTargetBranchLCBBody 0) (s := ⟨0, 6, env6⟩) rfl rfl rfl hRdFlag
      (by
        rw [boolWord_branch]
        exact transfer_no_phis (f := sameTargetBranchLCBBody 0) rfl rfl)
  let env7 := env6.set! 6 (some (word 7))
  have hTr1 : Source.transfer outF.body 2 1 env6 = .ok ⟨1, 0, env7⟩ :=
    transfer_one_phi (f := sameTargetBranchLCBBody 0) (pred := 2) (target := 1)
      rfl rfl rfl (read_word_literal env6 7)
      (write_ok (f := sameTargetBranchLCBBody 0) (n := 6) (by simp [sameTargetBranchLCBBody])
        (by omega) rfl)
  have hJmp : Source.Step outF.body ⟨2, 0, env6⟩ ⟨1, 0, env7⟩ :=
    step_jump (f := sameTargetBranchLCBBody 0) (s := ⟨2, 0, env6⟩) rfl rfl rfl hTr1
  have hRd6 : Source.read env7 .word (.local 6) = .ok (word 7) :=
    read_set_self (env := env6) (n := 6) (v := word 7) (by omega) rfl
  have hRet : Source.step outF.body ⟨1, 0, env7⟩ = .ok (.returned (word 7)) :=
    step_ret (f := sameTargetBranchLCBBody 0) (s := ⟨1, 0, env7⟩)
      (lb := { phis := #[{ destination := 6, width := .word,
                           incoming := #[(0, .literal (word 42)), (2, .literal (word 7))] }]
               instructions := #[]
               terminator := .ret (.local 6) })
      rfl rfl rfl hRd6
  exact lcb_observes_u8 tabProject outF (n := 7) (by decide) hInit
    (steps_append hSteps6 (.succ hBr (.succ hJmp (.zero _)))) hRet

-- Witness 5: Same-target parameterized branch inserts a split block (`blocks.size = 3`) and
-- observes exit 42 on `true` and exit 7 on `false` in `LCBInput.Execution`.
theorem d5_witness_5_same_target_branch_split :
    ∃ (outT outF : LCBInput lcbLinux),
      EmitLCB.emitUnvalidated (language := s0Lang) (sameTargetBranchBIR true lcbLinux) = .ok outT ∧
      EmitLCB.emitUnvalidated (language := s0Lang) (sameTargetBranchBIR false lcbLinux) = .ok outF ∧
      LCBEncoding s0Lang (sameTargetBranchBIR true lcbLinux) outT ∧
      LCBEncoding s0Lang (sameTargetBranchBIR false lcbLinux) outF ∧
      outT.body.blocks.size = 3 ∧
      (LCBInput.Execution s0Lang.operands.Event tabProject).observes outT (.returned 42 ByteArray.empty) ∧
      (LCBInput.Execution s0Lang.operands.Event tabProject).observes outF (.returned 7 ByteArray.empty) := by
  let outT : LCBInput lcbLinux := ⟨sameTargetBranchLCBBody 255, [], #[], EmitLCB.runtimeBytes []⟩
  let outF : LCBInput lcbLinux := ⟨sameTargetBranchLCBBody 0, [], #[], EmitLCB.runtimeBytes []⟩
  have hOutT : EmitLCB.emitUnvalidated (language := s0Lang) (sameTargetBranchBIR true lcbLinux) = .ok outT := rfl
  have hOutF : EmitLCB.emitUnvalidated (language := s0Lang) (sameTargetBranchBIR false lcbLinux) = .ok outF := rfl
  have hEncT := emitLCB_encoding (sameTargetBranchBIR_wf true lcbLinux (by decide) (by decide)) hOutT
  have hEncF := emitLCB_encoding (sameTargetBranchBIR_wf false lcbLinux (by decide) (by decide)) hOutF
  exact ⟨outT, outF, hOutT, hOutF, hEncT, hEncF, rfl,
    sameTargetBranch_observes_true, sameTargetBranch_observes_false⟩

-- Witness 6: Unreachable helper function containing a `.call` has `requiresLCBLinking = false`,
-- satisfies `entry_no_reachable_calls` and `EmitsLCB`, and emits uniquely named `concrete_fn_0`
-- in `additionalJobs`.
theorem d5_witness_6_unreachable_helper_call :
    (unreachableHelperBIR lcbLinux).requiresLCBLinking = false ∧
    ∃ out : LCBInput lcbLinux,
      (registry (prodPasses lcbLinux)).emit.run (unreachableHelperBIR lcbLinux) = .ok out ∧
      EmitsLCB s0Lang (unreachableHelperBIR lcbLinux) out ∧
      out.additionalJobs.length = 1 ∧
      List.map (fun p => p.2.code.symbol.name) out.additionalJobs = ["concrete_fn_0"] := by
  let out : LCBInput lcbLinux :=
    ⟨retBody 42, [(helperFn, EmitLCB.helperJob 0)], #[], EmitLCB.runtimeBytes []⟩
  have hOut : EmitLCB.emitUnvalidated (language := s0Lang) (unreachableHelperBIR lcbLinux) = .ok out := rfl
  have hVal : EmitLCB.validateJobs out = .ok () := by
    unfold EmitLCB.validateJobs
    change validateJob (LCBJob.lcbFunctionJob (retBody 42)) >>= (fun _ =>
      validateJob (EmitLCB.helperJob 0) >>= (fun _ => .ok ())) = .ok ()
    rw [validateJob_ret42, EmitLCB.helperJob_validateJob 0]; rfl
  have hEmit : EmitLCB.emit (language := s0Lang) (unreachableHelperBIR lcbLinux) = .ok out := by
    unfold EmitLCB.emit
    rw [hOut]
    dsimp only [bind, Except.bind, Except.mapError, pure, Except.pure]
    rw [hVal]
  have hRun : (registry (prodPasses lcbLinux)).emit.run (unreachableHelperBIR lcbLinux) = .ok out := by
    change emitGuarded (language := s0Lang) (prodPasses lcbLinux).emit (unreachableHelperBIR lcbLinux) = .ok out
    rw [show (prodPasses lcbLinux).emit = ValidateEmit.emit (language := s0Lang) from rfl,
      emitGuarded_emit_lcb]
    exact hEmit
  have hEmits := (emitLCB_emitsLCB (unreachableHelperBIR_wf lcbLinux (by decide) (by decide)) hEmit).2
  exact ⟨rfl, out, hRun, hEmits, rfl, rfl⟩

-- Witness 7: LCB refusal precedence (`requiresLCBLinking` beats `firstUnsupported`; `.load` cites
-- `StorageFormSupported@1`; Case 4 `.invalidLCBJob` on 9-parameter entry BIR with `LCBEncoding`).
theorem d5_witness_7_lcb_refusal_precedence :
    (linkingAndU32BIR lcbLinux).requiresLCBLinking = true ∧
    (firstUnsupported (language := s0Lang) (linkingAndU32BIR lcbLinux)).isSome = true ∧
    (registry (prodPasses lcbLinux)).emit.run (linkingAndU32BIR lcbLinux) =
      .error .lcbLinkingUnavailable ∧
    (registry (prodPasses lcbLinux)).emit.run (loadBIR lcbLinux) =
      .error (.unsupportedFeature lcbLinux (.unsupportedStorage .load mainFn (.instruction ⟨0⟩ 0))) ∧
    FeatureCause.rule (profile := lcbLinux) (.unsupportedStorage .load mainFn (.instruction ⟨0⟩ 0)) =
      BackendPolicy.storageFormRule ∧
    (∃ reason out,
      EmitLCB.emitUnvalidated (language := s0Lang) (nineParamBIR lcbLinux) = .ok out ∧
      LCBEncoding s0Lang (nineParamBIR lcbLinux) out ∧
      (registry (prodPasses lcbLinux)).emit.run (nineParamBIR lcbLinux) =
        .error (.invalidLCBJob reason)) := by
  have hRunLink : (registry (prodPasses lcbLinux)).emit.run (linkingAndU32BIR lcbLinux) =
      .error .lcbLinkingUnavailable := by
    change emitGuarded (language := s0Lang) (prodPasses lcbLinux).emit (linkingAndU32BIR lcbLinux) = _
    rw [show (prodPasses lcbLinux).emit = ValidateEmit.emit (language := s0Lang) from rfl,
      emitGuarded_emit_lcb]
    rfl
  have hRunLoad : (registry (prodPasses lcbLinux)).emit.run (loadBIR lcbLinux) =
      .error (.unsupportedFeature lcbLinux (.unsupportedStorage .load mainFn (.instruction ⟨0⟩ 0))) := by
    change emitGuarded (language := s0Lang) (prodPasses lcbLinux).emit (loadBIR lcbLinux) = _
    rw [show (prodPasses lcbLinux).emit = ValidateEmit.emit (language := s0Lang) from rfl,
      emitGuarded_emit_lcb]
    rfl
  let out9 : LCBInput lcbLinux :=
    ⟨{ resultWidth := .word
       parameters := Array.replicate 9 .word
       valueWidths := Array.replicate 9 .word
       blocks := #[{ phis := #[]
                     instructions := #[]
                     terminator := .ret (.literal (.word (BitVec.ofInt 32 0))) }] },
     [], #[], EmitLCB.runtimeBytes []⟩
  have hOut9 : EmitLCB.emitUnvalidated (language := s0Lang) (nineParamBIR lcbLinux) = .ok out9 := rfl
  have hEnc9 := emitLCB_encoding (nineParamBIR_wf lcbLinux (by decide) (by decide)) hOut9
  have hVal9 : EmitLCB.validateJobs out9 = .error (.admission (.resourceLimit "parameters" 9 8)) := by
    unfold EmitLCB.validateJobs
    change validateJob (LCBJob.lcbFunctionJob _) >>= (fun _ => .ok ()) = _
    rw [validateJob_nineParam_err]; rfl
  have hRun9 : (registry (prodPasses lcbLinux)).emit.run (nineParamBIR lcbLinux) =
      .error (.invalidLCBJob (.admission (.resourceLimit "parameters" 9 8))) := by
    change emitGuarded (language := s0Lang) (prodPasses lcbLinux).emit (nineParamBIR lcbLinux) = _
    rw [show (prodPasses lcbLinux).emit = ValidateEmit.emit (language := s0Lang) from rfl,
      emitGuarded_emit_lcb]
    unfold EmitLCB.emit
    rw [hOut9]
    dsimp only [bind, Except.bind, Except.mapError, pure, Except.pure]
    rw [hVal9]; rfl
  exact ⟨rfl, rfl, hRunLink, hRunLoad, rfl, _, out9, hOut9, hEnc9, hRun9⟩

/-! ### 3. Rejected wrong-compiler mutants (`¬ (registry mutant).emit.Validated`) -/

private theorem returnBIR_safe42 (profile : Profile) :
    ProgramSafe (BIR.Execution (language := s0Lang)) (returnBIR 42 profile) := by
  intro st hStuck
  obtain ⟨⟨start, hInit, hSteps⟩, _, hNoRes⟩ := hStuck
  change start = BIR.entryState (language := s0Lang) (returnBIR 42 profile) at hInit
  subst start
  have hRes0 : (BIR.Execution (language := s0Lang)).result (returnBIR 42 profile)
      (BIR.entryState (language := s0Lang) (returnBIR 42 profile))
      (.returned 42 ByteArray.empty) := by with_unfolding_all rfl
  cases hSteps with
  | nil => exact hNoRes _ hRes0
  | cons hS _ => exact (BIR.Execution.terminal _ _ _ hRes0 _ hS).elim

-- Mutant 1 (LCB): emits return literal `43` instead of `42` (passes `validateJob`, caught by
-- `BehaviorRefinement` inside `EmitsLCB`).
theorem d5_mutant_01_lcb_wrong_return_literal :
    let mutant := passesWithEmit lcbLinux fun _ =>
      EmitLCB.emitUnvalidated (language := s0Lang) (returnBIR 43 lcbLinux)
    ¬ (registry mutant).emit.Validated := by
  intro mutant hVal
  let out43 : LCBInput lcbLinux := ⟨retBody 43, [], #[], EmitLCB.runtimeBytes []⟩
  have hOut43 : EmitLCB.emitUnvalidated (language := s0Lang) (returnBIR 43 lcbLinux) = .ok out43 := rfl
  have hVal43 : EmitLCB.validateJobs out43 = .ok () := by
    unfold EmitLCB.validateJobs
    change validateJob (LCBJob.lcbFunctionJob (retBody 43)) >>= (fun _ => .ok ()) = .ok ()
    rw [validateJob_ret43]; rfl
  have hRun : (registry mutant).emit.run (returnBIR 42 lcbLinux) = .ok out43 := by
    change emitGuarded (language := s0Lang) mutant.emit (returnBIR 42 lcbLinux) = .ok out43
    unfold emitGuarded
    simp only [show ¬ (LCBInput.trapCapacity < (returnBIR 42 lcbLinux).sites.size) from by decide,
      show (returnBIR 42 lcbLinux).requiresLCBLinking = false from rfl,
      show firstUnsupported (language := s0Lang) (returnBIR 42 lcbLinux) = none from rfl,
      ↓reduceIte, mutant, passesWithEmit, hOut43, bind, Except.bind]
    change (EmitLCB.validateJobs out43).mapError StageFailure.invalidLCBJob >>= (fun _ => .ok out43) = .ok out43
    rw [hVal43]; rfl
  have hEmits : EmitsLCB s0Lang (returnBIR 42 lcbLinux) out43 :=
    (hVal.1 (returnBIR 42 lcbLinux) out43 (returnBIR_wf 42 lcbLinux (by decide)) hRun).2.2
  have hRef := hEmits.2.2.2.2.2.2.1 (returnBIR_safe42 lcbLinux)
  have hObs43 : (LCBInput.Execution s0Lang.operands.Event tabProject).observes out43
      (.returned 43 ByteArray.empty) :=
    retBody_observes out43 (by decide) rfl
  obtain ⟨q, hFinQ, hResQ⟩ := hObs43
  obtain ⟨p, hFinP, hResP, _⟩ := hRef.1 q _ hFinQ hResQ
  obtain ⟨start, hInit, hSteps⟩ := hFinP
  change start = BIR.entryState (language := s0Lang) (returnBIR 42 lcbLinux) at hInit
  subst start
  have hRes42 : (BIR.Execution (language := s0Lang)).result (returnBIR 42 lcbLinux)
      (BIR.entryState (language := s0Lang) (returnBIR 42 lcbLinux))
      (.returned 42 ByteArray.empty) := by with_unfolding_all rfl
  cases hSteps with
  | nil => cases Option.some.inj (hRes42.symm.trans hResP)
  | cons hS _ => exact (BIR.Execution.terminal _ _ _ hRes42 _ hS).elim

-- Mutant 2 (LCB): encodes a return body with exit `134` instead of `42` (passes `validateJob`,
-- caught by `BehaviorRefinement` inside `EmitsLCB`).
theorem d5_mutant_02_lcb_wrong_body_exit_134 :
    let mutant := passesWithEmit lcbLinux fun bir =>
      (EmitLCB.emitUnvalidated (language := s0Lang) bir).map fun out =>
        { out with body := retBody 134 }
    ¬ (registry mutant).emit.Validated := by
  intro mutant hVal
  let bir := returnBIR 42 lcbLinux
  let out : LCBInput lcbLinux := ⟨retBody 42, [], #[], EmitLCB.runtimeBytes []⟩
  have hOut : EmitLCB.emitUnvalidated (language := s0Lang) bir = .ok out := rfl
  let bad : LCBInput lcbLinux := ⟨retBody 134, [], #[], EmitLCB.runtimeBytes []⟩
  have hValBad : EmitLCB.validateJobs bad = .ok () := by
    unfold EmitLCB.validateJobs
    change validateJob (LCBJob.lcbFunctionJob (retBody 134)) >>= (fun _ => .ok ()) = .ok ()
    rw [validateJob_ret134]; rfl
  have hRun : (registry mutant).emit.run bir = .ok bad := by
    change emitGuarded (language := s0Lang) mutant.emit bir = .ok bad
    unfold emitGuarded
    simp only [show ¬ (LCBInput.trapCapacity < bir.sites.size) from by decide,
      show bir.requiresLCBLinking = false from rfl,
      show firstUnsupported (language := s0Lang) bir = none from rfl,
      ↓reduceIte, mutant, passesWithEmit, hOut, Except.map, bind, Except.bind]
    change (EmitLCB.validateJobs bad).mapError StageFailure.invalidLCBJob >>= (fun _ => .ok bad) = .ok bad
    rw [hValBad]; rfl
  have hEmits := (hVal.1 bir bad (returnBIR_wf 42 lcbLinux (by decide)) hRun).2.2
  have hRef := hEmits.2.2.2.2.2.2.1 (returnBIR_safe42 lcbLinux)
  have hObs134 : (LCBInput.Execution s0Lang.operands.Event tabProject).observes bad
      (.returned 134 ByteArray.empty) :=
    retBody_observes bad (by decide) rfl
  obtain ⟨q, hFinQ, hResQ⟩ := hObs134
  obtain ⟨p, hFinP, hResP, _⟩ := hRef.1 q _ hFinQ hResQ
  obtain ⟨start, hInit, hSteps⟩ := hFinP
  change start = BIR.entryState (language := s0Lang) bir at hInit
  subst start
  have hRes42 : (BIR.Execution (language := s0Lang)).result bir
      (BIR.entryState (language := s0Lang) bir) (.returned 42 ByteArray.empty) := by
    with_unfolding_all rfl
  cases hSteps with
  | nil => cases Option.some.inj (hRes42.symm.trans hResP)
  | cons hS _ => exact (BIR.Execution.terminal _ _ _ hRes42 _ hS).elim

-- Mutant 3 (LCB): corrupts `output.runtimeInput` to `ByteArray.empty` (caught by `LCBEncoding`).
theorem d5_mutant_03_lcb_empty_runtime_input :
    let mutant := passesWithEmit lcbLinux fun bir =>
      (EmitLCB.emitUnvalidated (language := s0Lang) bir).map fun out =>
        { out with runtimeInput := ByteArray.empty }
    ¬ (registry mutant).emit.Validated := by
  intro mutant hVal
  let bir := returnBIR 42 lcbLinux
  let out : LCBInput lcbLinux := ⟨retBody 42, [], #[], EmitLCB.runtimeBytes []⟩
  have hOut : EmitLCB.emitUnvalidated (language := s0Lang) bir = .ok out := rfl
  let bad : LCBInput lcbLinux := ⟨retBody 42, [], #[], ByteArray.empty⟩
  have hValBad : EmitLCB.validateJobs bad = .ok () := by
    unfold EmitLCB.validateJobs
    change validateJob (LCBJob.lcbFunctionJob (retBody 42)) >>= (fun _ => .ok ()) = .ok ()
    rw [validateJob_ret42]; rfl
  have hRun : (registry mutant).emit.run bir = .ok bad := by
    change emitGuarded (language := s0Lang) mutant.emit bir = .ok bad
    unfold emitGuarded
    simp only [show ¬ (LCBInput.trapCapacity < bir.sites.size) from by decide,
      show bir.requiresLCBLinking = false from rfl,
      show firstUnsupported (language := s0Lang) bir = none from rfl,
      ↓reduceIte, mutant, passesWithEmit, hOut, Except.map, bind, Except.bind]
    change (EmitLCB.validateJobs bad).mapError StageFailure.invalidLCBJob >>= (fun _ => .ok bad) = .ok bad
    rw [hValBad]; rfl
  have hRt := (hVal.1 bir bad (returnBIR_wf 42 lcbLinux (by decide)) hRun).2.2.2.2.2.2.2.1
  obtain ⟨lines, hLines, hBytes⟩ := hRt
  have hNil : lines = [] := Option.some.inj hLines.symm
  subst hNil
  exact runtimeBytes_nil_size_ne_zero (congrArg ByteArray.size hBytes.symm)

-- Mutant 4 (LCB): drops `additionalJobs` on `unreachableHelperBIR` (caught by function bijection).
theorem d5_mutant_04_lcb_dropped_additional_jobs :
    let mutant := passesWithEmit lcbLinux fun bir =>
      (EmitLCB.emitUnvalidated (language := s0Lang) bir).map fun out =>
        { out with additionalJobs := [] }
    ¬ (registry mutant).emit.Validated := by
  intro mutant hVal
  let bir := unreachableHelperBIR lcbLinux
  let out : LCBInput lcbLinux :=
    ⟨retBody 42, [(helperFn, EmitLCB.helperJob 0)], #[], EmitLCB.runtimeBytes []⟩
  have hOut : EmitLCB.emitUnvalidated (language := s0Lang) bir = .ok out := rfl
  let bad : LCBInput lcbLinux := ⟨retBody 42, [], #[], EmitLCB.runtimeBytes []⟩
  have hValBad : EmitLCB.validateJobs bad = .ok () := by
    unfold EmitLCB.validateJobs
    change validateJob (LCBJob.lcbFunctionJob (retBody 42)) >>= (fun _ => .ok ()) = .ok ()
    rw [validateJob_ret42]; rfl
  have hRun : (registry mutant).emit.run bir = .ok bad := by
    change emitGuarded (language := s0Lang) mutant.emit bir = .ok bad
    unfold emitGuarded
    simp only [show ¬ (LCBInput.trapCapacity < bir.sites.size) from by decide,
      show bir.requiresLCBLinking = false from rfl,
      show firstUnsupported (language := s0Lang) bir = none from rfl,
      ↓reduceIte, mutant, passesWithEmit, hOut, Except.map, bind, Except.bind]
    change (EmitLCB.validateJobs bad).mapError StageFailure.invalidLCBJob >>= (fun _ => .ok bad) = .ok bad
    rw [hValBad]; rfl
  have hBij := (hVal.1 bir bad (unreachableHelperBIR_wf lcbLinux (by decide) (by decide)) hRun).2.2.2.2.2.2.2.2.2.2.2.1
  have hMem : helperFn ∈ bir.entry :: List.map Prod.fst bad.additionalJobs :=
    (hBij helperFn).mp (List.mem_cons_of_mem _ List.mem_cons_self)
  rcases List.mem_cons.mp hMem with hEq | hNil
  · exact helperFn_ne_mainFn hEq
  · cases hNil

-- Mutant 5 (LCB): gives the helper job the duplicate symbol `LCBJob.bodySymbol` (caught by
-- `symbol_nodup` in `LCBEncoding`).
theorem d5_mutant_05_lcb_duplicate_helper_symbol :
    let mutant := passesWithEmit lcbLinux fun bir =>
      (EmitLCB.emitUnvalidated (language := s0Lang) bir).map fun out =>
        { out with additionalJobs := List.map (fun (fn, j) =>
            (fn, { j with code := { j.code with symbol := LCBJob.bodySymbol } })) out.additionalJobs }
    ¬ (registry mutant).emit.Validated := by
  intro mutant hVal
  let bir := unreachableHelperBIR lcbLinux
  let out : LCBInput lcbLinux :=
    ⟨retBody 42, [(helperFn, EmitLCB.helperJob 0)], #[], EmitLCB.runtimeBytes []⟩
  have hOut : EmitLCB.emitUnvalidated (language := s0Lang) bir = .ok out := rfl
  let dupJob : FunctionJob :=
    { EmitLCB.helperJob 0 with code := { (EmitLCB.helperJob 0).code with symbol := LCBJob.bodySymbol } }
  let bad : LCBInput lcbLinux := ⟨retBody 42, [(helperFn, dupJob)], #[], EmitLCB.runtimeBytes []⟩
  have hValBad : EmitLCB.validateJobs bad = .ok () := by
    unfold EmitLCB.validateJobs
    change validateJob (LCBJob.lcbFunctionJob (retBody 42)) >>= (fun _ =>
      validateJob dupJob >>= (fun _ => .ok ())) = .ok ()
    rw [validateJob_ret42, validateJob_dupHelper]; rfl
  have hRun : (registry mutant).emit.run bir = .ok bad := by
    change emitGuarded (language := s0Lang) mutant.emit bir = .ok bad
    unfold emitGuarded
    simp only [show ¬ (LCBInput.trapCapacity < bir.sites.size) from by decide,
      show bir.requiresLCBLinking = false from rfl,
      show firstUnsupported (language := s0Lang) bir = none from rfl,
      ↓reduceIte, mutant, passesWithEmit, hOut, Except.map, bind, Except.bind]
    change (EmitLCB.validateJobs bad).mapError StageFailure.invalidLCBJob >>= (fun _ => .ok bad) = .ok bad
    rw [hValBad]; rfl
  have hSymNd := (hVal.1 bir bad (unreachableHelperBIR_wf lcbLinux (by decide) (by decide)) hRun).2.2.2.2.2.1
  change (["concrete_body", "concrete_body"]).Nodup at hSymNd
  rw [List.nodup_cons] at hSymNd
  exact hSymNd.1 (List.mem_singleton_self _)

-- Mutant 6 (LCB): spuriously refuses a valid `u8` BIR inside the inner emitter.
theorem d5_mutant_06_lcb_spurious_u8_refusal :
    let mutant := passesWithEmit lcbLinux fun _ =>
      .error (.unsupportedFeature lcbLinux (.unsupportedWidth .u8 mainFn (.instruction ⟨0⟩ 0)))
    ¬ (registry mutant).emit.Validated := by
  intro mutant hVal
  let bir := returnBIR 42 lcbLinux
  have hRun : (registry mutant).emit.run bir =
      .error (.unsupportedFeature lcbLinux (.unsupportedWidth .u8 mainFn (.instruction ⟨0⟩ 0))) := by
    change emitGuarded (language := s0Lang) mutant.emit bir = _
    unfold emitGuarded
    simp only [show ¬ (LCBInput.trapCapacity < bir.sites.size) from by decide,
      show bir.requiresLCBLinking = false from rfl,
      show firstUnsupported (language := s0Lang) bir = none from rfl,
      Bool.false_eq_true, ↓reduceIte, mutant, passesWithEmit, bind, Except.bind]
  rcases hVal.2 bir _ (returnBIR_wf 42 lcbLinux (by decide)) hRun with
    ⟨hCap, _⟩ | ⟨_, hLink, _⟩ | ⟨_, _, cause, hSup, _⟩ | ⟨_, _, _, _, _, _, _, _, hFail⟩
  · exact (by decide : ¬ (LCBInput.trapCapacity < bir.sites.size)) hCap
  · cases hLink
  · have hNone : firstUnsupported (language := s0Lang) bir = none := rfl
    rw [hNone] at hSup; cases hSup
  · cases hFail

-- Mutant 7 (LCB): returns `.invalidInput` from the inner emitter on valid `returnBIR 42`
-- (violates the closed 4-branch LCB refusal contract).
theorem d5_mutant_07_lcb_invalid_input_refusal :
    let mutant := passesWithEmit lcbLinux fun _ => .error (.invalidInput dummyRule)
    ¬ (registry mutant).emit.Validated := by
  intro mutant hVal
  let bir := returnBIR 42 lcbLinux
  have hRun : (registry mutant).emit.run bir = .error (.invalidInput dummyRule) := by
    change emitGuarded (language := s0Lang) mutant.emit bir = _
    unfold emitGuarded
    simp only [show ¬ (LCBInput.trapCapacity < bir.sites.size) from by decide,
      show bir.requiresLCBLinking = false from rfl,
      show firstUnsupported (language := s0Lang) bir = none from rfl,
      Bool.false_eq_true, ↓reduceIte, mutant, passesWithEmit, bind, Except.bind]
  rcases hVal.2 bir _ (returnBIR_wf 42 lcbLinux (by decide)) hRun with
    ⟨_, hFail⟩ | ⟨_, _, hFail⟩ | ⟨_, _, _, _, hFail⟩ | ⟨_, _, _, _, _, _, _, _, hFail⟩ <;>
    cases hFail

-- Mutant 8 (LCB `ISS-0117` width-policy mismatch): wrong `FeatureCause` (`OperationSupported`
-- instead of `IntWidthSupported`) on `widthArithBIR .u32` is rejected by `.emit.refusal`.
theorem d5_mutant_08_lcb_wrong_feature_cause_iss0117 :
    let bir := widthArithBIR .u32 lcbLinux
    ¬ (registry (prodPasses lcbLinux)).emit.refusal bir
        (.unsupportedFeature lcbLinux
          (.unsupportedOperation (.arithmetic (.wrapping .add)) mainFn (.instruction ⟨0⟩ 0))) := by
  intro bir hRef
  rcases hRef with ⟨_, hFail⟩ | ⟨_, _, hFail⟩ | ⟨_, _, cause, hSup, hFail⟩ | ⟨_, _, _, _, _, _, _, _, hFail⟩
  · cases hFail
  · cases hFail
  · have hActual : firstUnsupported (language := s0Lang) bir =
        some (.unsupportedWidth .u32 mainFn (.instruction ⟨0⟩ 0)) := rfl
    rw [hActual] at hSup
    cases Option.some.inj hSup
    cases (StageFailure.unsupportedFeature.inj hFail).2
  · cases hFail

-- Mutant 9 (LLVM): emits an empty launcher module (caught by `LauncherMatches` in
-- `LLVMMetadataMatches`).
theorem d5_mutant_09_llvm_empty_launcher :
    let mutant := passesWithEmit llvmMac fun bir =>
      (EmitLLVM.emit (language := s0Lang) bir).map fun out =>
        { out with launcher := ⟨[]⟩ }
    ¬ (registry mutant).emit.Validated := by
  intro mutant hVal
  let bir := returnBIR 42 llvmMac
  obtain ⟨out, hOut⟩ : ∃ out, EmitLLVM.emit (language := s0Lang) bir = .ok out := ⟨_, rfl⟩
  let bad : LLVMInput llvmMac := { out with launcher := ⟨[]⟩ }
  have hRun : (registry mutant).emit.run bir = .ok bad := by
    change emitGuarded (language := s0Lang) mutant.emit bir = .ok bad
    unfold emitGuarded
    simp only [show firstUnsupported (language := s0Lang) bir = none from rfl,
      mutant, passesWithEmit, hOut, Except.map]
    rfl
  obtain ⟨_, _, _, hLaunch, _⟩ :=
    (hVal.1 bir bad (returnBIR_wf 42 llvmMac (by decide)) hRun).2.2.launcherMatches
  cases hLaunch

-- Mutant 10 (LLVM): component defines launcher-owned symbol `.main` (caught by
-- `ComponentOwnsSymbols` in `LLVMMetadataMatches`).
theorem d5_mutant_10_llvm_component_defines_main :
    let mutant := passesWithEmit llvmMac fun bir =>
      (EmitLLVM.emit (language := s0Lang) bir).map fun out =>
        { out with component := ⟨out.component.items ++ [.define false (.integer .u32) .main []]⟩ }
    ¬ (registry mutant).emit.Validated := by
  intro mutant hVal
  let bir := returnBIR 42 llvmMac
  obtain ⟨out, hOut⟩ : ∃ out, EmitLLVM.emit (language := s0Lang) bir = .ok out := ⟨_, rfl⟩
  let bad : LLVMInput llvmMac :=
    { out with component := ⟨out.component.items ++ [.define false (.integer .u32) .main []]⟩ }
  have hRun : (registry mutant).emit.run bir = .ok bad := by
    change emitGuarded (language := s0Lang) mutant.emit bir = .ok bad
    unfold emitGuarded
    simp only [show firstUnsupported (language := s0Lang) bir = none from rfl,
      mutant, passesWithEmit, hOut, Except.map]
    rfl
  obtain ⟨diagnostics, _, _, hLaunch, hOwn⟩ :=
    (hVal.1 bir bad (returnBIR_wf 42 llvmMac (by decide)) hRun).2.2.launcherMatches
  have hDefComp : LLVM.Global.main ∈ llvmModuleDefines bad.component.items := by
    simp [bad, llvmModuleDefines, llvmItemDefines]
  have hDefLaunch : LLVM.Global.main ∈ llvmModuleDefines bad.launcher.items := by
    rw [hLaunch, llvmModuleDefines_launcherItems]
    exact List.mem_append_right _ (by simp)
  exact hOwn.1 .main hDefComp hDefLaunch

-- Mutant 11 (LLVM `ISS-0117` width-policy alignment): spuriously refuses valid `u32` arithmetic
-- on LLVM (caught by `Validated.2` because LLVM supports all 8 integer widths).
theorem d5_mutant_11_llvm_spurious_u32_refusal :
    let mutant := passesWithEmit llvmLinux fun _ =>
      .error (.unsupportedFeature llvmLinux (.unsupportedWidth .u32 mainFn (.instruction ⟨0⟩ 0)))
    ¬ (registry mutant).emit.Validated := by
  intro mutant hVal
  let bir := widthArithBIR .u32 llvmLinux
  have hRun : (registry mutant).emit.run bir =
      .error (.unsupportedFeature llvmLinux (.unsupportedWidth .u32 mainFn (.instruction ⟨0⟩ 0))) := by
    change emitGuarded (language := s0Lang) mutant.emit bir = _
    unfold emitGuarded
    simp only [show firstUnsupported (language := s0Lang) bir = none from rfl,
      mutant, passesWithEmit]
  obtain ⟨cause, hSup, _⟩ := hVal.2 bir _ (widthArithBIR_wf .u32 llvmLinux (by decide) (by decide)) hRun
  have hNone : firstUnsupported (language := s0Lang) bir = none := rfl
  rw [hNone] at hSup; cases hSup

end StageInterfaceControls.D5Backends
