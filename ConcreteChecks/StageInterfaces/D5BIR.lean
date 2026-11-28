import Concrete.BIR.Build

/-!
# D5 BIR formation witnesses, ceiling/loop controls, and wrong-compiler mutants

Standalone controls on the fixed Slice 0 instance (`slice0`, `sliceLanguage slice0`,
`sliceLoweringLanguage slice0`) exercising the production `Concrete.BIR.Build.buildBIR`
pass through the registered `(registry passes).buildBIR` stage:
1. universal validation and `Stage.Sound` across LLVM and LCB profiles (including `maxBIRSlots = 0`);
2. reachable whole-program witnesses:
   - straight-line `return 42` (`frameSlots = 0`, empty trap table, observes exit 42),
   - two-result `addWithOverflow` (`frameSlots = 2`, exact ceiling refusal at 1 and acceptance at 2,
     observes exit 42 on `20 + 22` and trap 134 with exact stderr at slot 0 on `255 + 1`),
   - multi-site trap table including an unreachable trap block (`frameSlots = 4`, deterministic
     sequential slots 0/1/2, observes trap 134 at site 1 on `100 + 100 + 100`),
   - zero-slot self-loop accepted at `maxBIRSlots = 0` with diverging execution in both SSA and BIR,
   - materialized local with non-empty `mutableLocals` preserved across BIR formation,
   - off-domain `.invalid` terminator cleanly refused with `.invalidInput`;
3. ten rejected wrong-compiler mutants, each proving `¬ (registry mutant).buildBIR.Validated` on
   the fixed Slice 0 instance.
-/

namespace StageInterfaceControls.D5BIR

open Concrete.BIR.Build
open Concrete.Contracts
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface

private abbrev s0Lang : CoreLanguage := sliceLanguage slice0
private abbrev s0Low : LoweringLanguage s0Lang.operands.Ty := sliceLoweringLanguage slice0
private abbrev u8Ty : s0Lang.operands.Ty := s0Low.intTy .u8
private abbrev boolTy : s0Lang.operands.Ty := s0Low.bool

private def mainFn : FnId := ⟨⟨⟨⟨"d5"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩, ⟨"main"⟩⟩
private def helperFn : FnId := ⟨⟨⟨⟨"d5"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩, ⟨"helper"⟩⟩

private def lcbProfile (slots : Nat) : Profile := ⟨.lcb, .x86_64Linux, slots, 4096⟩
private def llvmProfile (slots : Nat) : Profile := ⟨.llvm, .aarch64Macos, slots, 4096⟩

private def otherProject : Project :=
  Project.singleton mainSourcePath (SourceText.ofBytes [97, 9, 99])

private theorem otherProject_ne_tabProject : otherProject ≠ tabProject := by
  intro h
  have hb := congrArg (fun p => (p.fileAt ⟨0⟩).map fun f => f.source.bytes[2]?) h
  have hDiff : some (some (99 : Nat)) ≠ some (some 98) := by decide
  exact hDiff hb

private def span0 : FileSpan := ⟨⟨0⟩, 0, 1, 1, 1⟩
private def span1 : FileSpan := ⟨⟨0⟩, 2, 3, 1, 3⟩
private def span2 : FileSpan := ⟨⟨0⟩, 0, 2, 1, 1⟩

private def checkedSpan0 : ConcreteSpec.Core.Span :=
  ⟨⟨0⟩, 0, 1, by decide, 1, 1, by decide, by decide⟩
private def checkedSpan1 : ConcreteSpec.Core.Span :=
  ⟨⟨0⟩, 2, 3, by decide, 1, 3, by decide, by decide⟩
private def checkedSpan2 : ConcreteSpec.Core.Span :=
  ⟨⟨0⟩, 0, 2, by decide, 1, 1, by decide, by decide⟩

private theorem span0_binds : ∃ s, span0.Binds tabProject s :=
  ⟨checkedSpan0, File.load mainSourcePath tabSource, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private theorem span1_binds : ∃ s, span1.Binds tabProject s :=
  ⟨checkedSpan1, File.load mainSourcePath tabSource, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private theorem span2_binds : ∃ s, span2.Binds tabProject s :=
  ⟨checkedSpan2, File.load mainSourcePath tabSource, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private def orig0 : Origin := ⟨⟨0⟩, span0, ⟨.function mainFn, ⟨[]⟩⟩⟩
private def orig1 : Origin := ⟨⟨1⟩, span1, ⟨.function mainFn, ⟨[]⟩⟩⟩
private def orig2 : Origin := ⟨⟨2⟩, span2, ⟨.function mainFn, ⟨[]⟩⟩⟩

private def dummyRule : RuleId := ⟨`StageInterfaceControls.D5BIR.unused, 1⟩

/-- Build a Slice 0 `Passes` bundle whose `buildBIR` field is `fn`. -/
private def passesWithBuildBIR (profile : Profile)
    (fn : SSA s0Low profile → Except StageFailure (BIR s0Low profile)) : Passes slice0 profile where
  lex _ := .error (.missingFile ⟨0⟩)
  elaborate _ := .error (.invalidTree ⟨0⟩)
  checkCore _ := .error (.fragment (.unsupportedSlice slice0))
  lowerCore _ := .error (.invalidInput dummyRule)
  lowerUnchecked _ := .error (.invalidInput dummyRule)
  lowerMemory _ := .error (.invalidInput dummyRule)
  buildSSA _ := .error (.invalidInput dummyRule)
  buildBIR := fn
  emit _ := .error (.invalidInput dummyRule)

private def prodPasses (profile : Profile) : Passes slice0 profile :=
  passesWithBuildBIR profile buildBIR

/-! ### 1. Universal registered validation and soundness on fixed `slice0` -/

example : (registry (prodPasses (lcbProfile 0))).buildBIR.Validated :=
  buildBIR_validated (prodPasses (lcbProfile 0)) rfl

example : Stage.Sound (prodPasses (lcbProfile 2)) .buildBIR :=
  buildBIR_sound (prodPasses (lcbProfile 2)) rfl

example : Stage.Sound (prodPasses (llvmProfile 4)) .buildBIR :=
  buildBIR_sound (prodPasses (llvmProfile 4)) rfl

/-! ### Shared fixtures on `slice0` -/

private def returnSSA (value : UInt8) (profile : Profile) : SSA s0Low profile :=
  let blk : SSABlock s0Low :=
    ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[value]⟩⟩), [orig0]⟩
  ⟨tabProject, mainFn, [(mainFn, ⟨[], some u8Ty⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[blk]⟩)], [], [], []⟩

private def checkedAddSSA (a b : UInt8) (profile : Profile) : SSA s0Low profile :=
  let blk0 : SSABlock s0Low :=
    ⟨⟨0⟩, [],
      [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[a]⟩) (.literal ⟨[b]⟩)⟩],
      .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
  let blk1 : SSABlock s0Low :=
    ⟨⟨1⟩, [], [], .trap .overflow orig0, []⟩
  let blk2 : SSABlock s0Low :=
    ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
  ⟨tabProject, mainFn, [(mainFn, ⟨[], some u8Ty⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[blk0, blk1, blk2]⟩)], [], [], []⟩

private def twoSiteSSA (a b c : UInt8) (profile : Profile) : SSA s0Low profile :=
  let blk0 : SSABlock s0Low :=
    ⟨⟨0⟩, [],
      [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[a]⟩) (.literal ⟨[b]⟩)⟩],
      .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
  let blk1 : SSABlock s0Low :=
    ⟨⟨1⟩, [], [], .trap .overflow orig0, []⟩
  let blk2 : SSABlock s0Low :=
    ⟨⟨2⟩, [],
      [⟨orig1, .addWithOverflow .u8 ⟨2⟩ ⟨3⟩ (.value ⟨0⟩) (.literal ⟨[c]⟩)⟩],
      .branch (.value ⟨3⟩) ⟨⟨3⟩, []⟩ ⟨⟨4⟩, []⟩, []⟩
  let blk3 : SSABlock s0Low :=
    ⟨⟨3⟩, [], [], .trap .overflow orig1, []⟩
  let blk4 : SSABlock s0Low :=
    ⟨⟨4⟩, [], [], .ret (some ⟨u8Ty, .value ⟨2⟩⟩), []⟩
  let blk5 : SSABlock s0Low :=
    ⟨⟨5⟩, [], [], .trap .abort orig2, []⟩
  ⟨tabProject, mainFn, [(mainFn, ⟨[], some u8Ty⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[blk0, blk1, blk2, blk3, blk4, blk5]⟩)], [], [], []⟩

private def zeroSlotLoopSSA (profile : Profile) : SSA s0Low profile :=
  let blk : SSABlock s0Low := ⟨⟨0⟩, [], [], .jump ⟨⟨0⟩, []⟩, []⟩
  ⟨tabProject, mainFn, [(mainFn, ⟨[], none⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[blk]⟩)], [], [], []⟩

private def mutableLocalSSA (profile : Profile) : SSA s0Low profile :=
  let blk : SSABlock s0Low :=
    ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩
  ⟨tabProject, mainFn, [(mainFn, ⟨[], some u8Ty⟩)],
    [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[blk]⟩)],
    [(mainFn, [(⟨0⟩, ⟨1, 1, .nil⟩)])],
    [(mainFn, [⟨0⟩])], []⟩

private def invalidTermSSA (profile : Profile) : SSA s0Low profile :=
  let blk : SSABlock s0Low := ⟨⟨0⟩, [], [], .invalid, []⟩
  ⟨tabProject, mainFn, [(mainFn, ⟨[], none⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[blk]⟩)], [], [], []⟩

private def multiFnSSA (profile : Profile) : SSA s0Low profile :=
  let mainBlk : SSABlock s0Low := ⟨⟨0⟩, [], [], .trap .abort orig0, []⟩
  let hBlk0 : SSABlock s0Low :=
    ⟨⟨0⟩, [],
      [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[10]⟩) (.literal ⟨[20]⟩)⟩],
      .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
  let hBlk1 : SSABlock s0Low := ⟨⟨1⟩, [], [], .trap .overflow orig1, []⟩
  let hBlk2 : SSABlock s0Low := ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
  ⟨tabProject, mainFn,
    [(mainFn, ⟨[], some u8Ty⟩), (helperFn, ⟨[], some u8Ty⟩)],
    [(mainFn, [], ⟨⟨0⟩, #[mainBlk]⟩),
     (helperFn, [], ⟨⟨0⟩, #[hBlk0, hBlk1, hBlk2]⟩)],
    [], [], []⟩

/-! ### Well-formedness proofs for the Slice 0 SSA fixtures -/

private theorem returnSSA_wf (value : UInt8) (profile : Profile)
    (hReg : (returnSSA value profile).checkRegisterFormation (language := s0Lang) = true) :
    SSA.WF (language := s0Lang) (returnSSA value profile) := by
  have hFns : ((returnSSA value profile).memoryView (language := s0Lang)).functions =
      [(mainFn, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[value]⟩⟩), [orig0]⟩]⟩)] := by
    with_unfolding_all rfl
  have hStor : ((returnSSA value profile).memoryView (language := s0Lang)).storage =
      [(mainFn, [])] := by
    with_unfolding_all rfl
  have hSig : ((returnSSA value profile).memoryView (language := s0Lang)).signatures =
      [(mainFn, ⟨[], some u8Ty⟩)] := by
    with_unfolding_all rfl
  have hLay : ((returnSSA value profile).memoryView (language := s0Lang)).layouts = [] := by
    with_unfolding_all rfl
  have hMut : ((returnSSA value profile).memoryView (language := s0Lang)).mutableLocals = [] := by
    with_unfolding_all rfl
  have hDecl : ((returnSSA value profile).memoryView (language := s0Lang)).declarations = [] := by
    with_unfolding_all rfl
  refine ⟨(SSA.checkRegisterFormation_eq_true_iff _).mp hReg, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · refine ⟨by rw [hFns]; simp, by rw [hSig]; simp, by rw [hStor]; simp, by rw [hLay]; exact .nil,
      ?_, by rw [hLay]; exact nofun, ?_⟩
    · intro fn cls h
      rw [hStor] at h
      cases List.mem_singleton.mp h
      exact .nil
    · intro fn locals graph h
      rw [hFns] at h
      cases List.mem_singleton.mp h
      refine ⟨.nil, ⟨by simp, ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
        ⟨⟨[], some u8Ty⟩, by rw [hSig]; rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩⟩
      · intro blk hblk edge hedge
        cases List.mem_singleton.mp hblk
        cases hedge
      · intro blk hblk
        cases List.mem_singleton.mp hblk
        exact ⟨rfl, fun _ h => (List.not_mem_nil h).elim⟩
  · rw [hFns]
    exact List.mem_singleton_self _
  · intro origin ho
    rw [ssa_memoryView_origins_eq (language := s0Lang) (returnSSA value profile)] at ho
    have hEq : origin = orig0 := List.mem_singleton.mp ho
    subst hEq
    exact span0_binds
  · intro table ht
    rw [hLay] at ht
    cases ht
  · rw [hDecl]
    exact ⟨.nil, by decide, nofun⟩
  · rw [hMut]
    exact ⟨.nil, nofun⟩

private theorem checkedAddSSA_wf (a b : UInt8) (profile : Profile)
    (hReg : (checkedAddSSA a b profile).checkRegisterFormation (language := s0Lang) = true) :
    SSA.WF (language := s0Lang) (checkedAddSSA a b profile) := by
  have hFns : ((checkedAddSSA a b profile).memoryView (language := s0Lang)).functions =
      [(mainFn, [(⟨0⟩, u8Ty), (⟨1⟩, boolTy)],
        ⟨⟨0⟩, #[
          ⟨⟨0⟩, [], [⟨orig0, .addWithOverflow .u8 ⟨⟨⟨0⟩, []⟩, .virtual⟩ ⟨⟨⟨1⟩, []⟩, .virtual⟩
            (.literal ⟨[a]⟩) (.literal ⟨[b]⟩)⟩],
            .branch (.copy ⟨⟨⟨1⟩, []⟩, .virtual⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩,
          ⟨⟨1⟩, [], [], .trap .overflow orig0, []⟩,
          ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .copy ⟨⟨⟨0⟩, []⟩, .virtual⟩⟩), []⟩]⟩)] := by
    with_unfolding_all rfl
  have hStor : ((checkedAddSSA a b profile).memoryView (language := s0Lang)).storage =
      [(mainFn, [(⟨0⟩, .virtual), (⟨1⟩, .virtual)])] := by
    with_unfolding_all rfl
  have hSig : ((checkedAddSSA a b profile).memoryView (language := s0Lang)).signatures =
      [(mainFn, ⟨[], some u8Ty⟩)] := by
    with_unfolding_all rfl
  have hLay : ((checkedAddSSA a b profile).memoryView (language := s0Lang)).layouts = [] := by
    with_unfolding_all rfl
  have hMut : ((checkedAddSSA a b profile).memoryView (language := s0Lang)).mutableLocals =
      [(mainFn, [⟨0⟩, ⟨1⟩])] := by
    with_unfolding_all rfl
  have hDecl : ((checkedAddSSA a b profile).memoryView (language := s0Lang)).declarations = [] := by
    with_unfolding_all rfl
  refine ⟨(SSA.checkRegisterFormation_eq_true_iff _).mp hReg, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · refine ⟨by rw [hFns]; simp, by rw [hSig]; simp, by rw [hStor]; simp, by rw [hLay]; exact .nil,
      ?_, by rw [hLay]; exact nofun, ?_⟩
    · intro fn cls h
      rw [hStor] at h
      cases List.mem_singleton.mp h
      decide
    · intro fn locals graph h
      rw [hFns] at h
      cases List.mem_singleton.mp h
      refine ⟨by decide, ⟨by simp, ⟨_, List.mem_cons_self, rfl⟩, ?_⟩,
        ⟨⟨[], some u8Ty⟩, by rw [hSig]; rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩, ?_⟩⟩
      · intro blk hblk edge hedge
        simp only [List.mem_cons, List.not_mem_nil, or_false] at hblk
        rcases hblk with rfl | rfl | rfl
        · simp only [Terminator.successors, List.mem_cons, List.not_mem_nil, or_false] at hedge
          rcases hedge with rfl | rfl
          · exact ⟨_, List.mem_cons_of_mem _ List.mem_cons_self, rfl, rfl⟩
          · exact ⟨_, List.mem_cons_of_mem _ (List.mem_cons_of_mem _ List.mem_cons_self), rfl, rfl⟩
        · cases hedge
        · cases hedge
      · intro blk hblk
        simp only [List.mem_cons, List.not_mem_nil, or_false] at hblk
        rcases hblk with rfl | rfl | rfl
        · exact ⟨trivial, fun op hop => by cases List.mem_singleton.mp hop; trivial⟩
        · exact ⟨trivial, fun _ h => (List.not_mem_nil h).elim⟩
        · exact ⟨rfl, fun _ h => (List.not_mem_nil h).elim⟩
  · rw [hFns]
    exact List.mem_singleton_self _
  · intro origin ho
    rw [ssa_memoryView_origins_eq (language := s0Lang) (checkedAddSSA a b profile)] at ho
    have hOrigs : (checkedAddSSA a b profile).origins = [orig0, orig0] := rfl
    rw [hOrigs] at ho
    simp only [List.mem_cons, List.not_mem_nil, or_false] at ho
    rcases ho with rfl | rfl <;> exact span0_binds
  · intro table ht
    rw [hLay] at ht
    cases ht
  · rw [hDecl]
    exact ⟨.nil, by decide, nofun⟩
  · refine ⟨by rw [hMut]; simp, ?_⟩
    intro fn roots h
    rw [hMut] at h
    cases List.mem_singleton.mp h
    refine ⟨_, _, by rw [hFns]; exact List.mem_cons_self, ?_⟩
    intro root hr
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hr
    rcases hr with rfl | rfl
    · exact ⟨u8Ty, List.mem_cons_self⟩
    · exact ⟨boolTy, List.mem_cons_of_mem _ List.mem_cons_self⟩

private theorem twoSiteSSA_wf (a b c : UInt8) (profile : Profile)
    (hReg : (twoSiteSSA a b c profile).checkRegisterFormation (language := s0Lang) = true) :
    SSA.WF (language := s0Lang) (twoSiteSSA a b c profile) := by
  have hFns : ((twoSiteSSA a b c profile).memoryView (language := s0Lang)).functions =
      [(mainFn, [(⟨0⟩, u8Ty), (⟨1⟩, boolTy), (⟨2⟩, u8Ty), (⟨3⟩, boolTy)],
        ⟨⟨0⟩, #[
          ⟨⟨0⟩, [], [⟨orig0, .addWithOverflow .u8 ⟨⟨⟨0⟩, []⟩, .virtual⟩ ⟨⟨⟨1⟩, []⟩, .virtual⟩
            (.literal ⟨[a]⟩) (.literal ⟨[b]⟩)⟩],
            .branch (.copy ⟨⟨⟨1⟩, []⟩, .virtual⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩,
          ⟨⟨1⟩, [], [], .trap .overflow orig0, []⟩,
          ⟨⟨2⟩, [], [⟨orig1, .addWithOverflow .u8 ⟨⟨⟨2⟩, []⟩, .virtual⟩ ⟨⟨⟨3⟩, []⟩, .virtual⟩
            (.copy ⟨⟨⟨0⟩, []⟩, .virtual⟩) (.literal ⟨[c]⟩)⟩],
            .branch (.copy ⟨⟨⟨3⟩, []⟩, .virtual⟩) ⟨⟨3⟩, []⟩ ⟨⟨4⟩, []⟩, []⟩,
          ⟨⟨3⟩, [], [], .trap .overflow orig1, []⟩,
          ⟨⟨4⟩, [], [], .ret (some ⟨u8Ty, .copy ⟨⟨⟨2⟩, []⟩, .virtual⟩⟩), []⟩,
          ⟨⟨5⟩, [], [], .trap .abort orig2, []⟩]⟩)] := by
    with_unfolding_all rfl
  have hStor : ((twoSiteSSA a b c profile).memoryView (language := s0Lang)).storage =
      [(mainFn, [(⟨0⟩, .virtual), (⟨1⟩, .virtual), (⟨2⟩, .virtual), (⟨3⟩, .virtual)])] := by
    with_unfolding_all rfl
  have hSig : ((twoSiteSSA a b c profile).memoryView (language := s0Lang)).signatures =
      [(mainFn, ⟨[], some u8Ty⟩)] := by
    with_unfolding_all rfl
  have hLay : ((twoSiteSSA a b c profile).memoryView (language := s0Lang)).layouts = [] := by
    with_unfolding_all rfl
  have hMut : ((twoSiteSSA a b c profile).memoryView (language := s0Lang)).mutableLocals =
      [(mainFn, [⟨0⟩, ⟨1⟩, ⟨2⟩, ⟨3⟩])] := by
    with_unfolding_all rfl
  have hDecl : ((twoSiteSSA a b c profile).memoryView (language := s0Lang)).declarations = [] := by
    with_unfolding_all rfl
  refine ⟨(SSA.checkRegisterFormation_eq_true_iff _).mp hReg, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · refine ⟨by rw [hFns]; simp, by rw [hSig]; simp, by rw [hStor]; simp, by rw [hLay]; exact .nil,
      ?_, by rw [hLay]; exact nofun, ?_⟩
    · intro fn cls h
      rw [hStor] at h
      cases List.mem_singleton.mp h
      decide
    · intro fn locals graph h
      rw [hFns] at h
      cases List.mem_singleton.mp h
      refine ⟨by decide, ⟨by simp, ⟨_, List.mem_cons_self, rfl⟩, ?_⟩,
        ⟨⟨[], some u8Ty⟩, by rw [hSig]; rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩, ?_⟩⟩
      · intro blk hblk edge hedge
        simp only [List.mem_cons, List.not_mem_nil, or_false] at hblk
        rcases hblk with rfl | rfl | rfl | rfl | rfl | rfl
        · simp only [Terminator.successors, List.mem_cons, List.not_mem_nil, or_false] at hedge
          rcases hedge with rfl | rfl
          · exact ⟨_, List.mem_cons_of_mem _ List.mem_cons_self, rfl, rfl⟩
          · exact ⟨_, List.mem_cons_of_mem _ (List.mem_cons_of_mem _ List.mem_cons_self), rfl, rfl⟩
        · cases hedge
        · simp only [Terminator.successors, List.mem_cons, List.not_mem_nil, or_false] at hedge
          rcases hedge with rfl | rfl
          · exact ⟨_, List.mem_cons_of_mem _ (List.mem_cons_of_mem _ (List.mem_cons_of_mem _ List.mem_cons_self)), rfl, rfl⟩
          · exact ⟨_, List.mem_cons_of_mem _ (List.mem_cons_of_mem _ (List.mem_cons_of_mem _ (List.mem_cons_of_mem _ List.mem_cons_self))), rfl, rfl⟩
        · cases hedge
        · cases hedge
        · cases hedge
      · intro blk hblk
        simp only [List.mem_cons, List.not_mem_nil, or_false] at hblk
        rcases hblk with rfl | rfl | rfl | rfl | rfl | rfl
        · exact ⟨trivial, fun op hop => by cases List.mem_singleton.mp hop; trivial⟩
        · exact ⟨trivial, fun _ h => (List.not_mem_nil h).elim⟩
        · exact ⟨trivial, fun op hop => by cases List.mem_singleton.mp hop; trivial⟩
        · exact ⟨trivial, fun _ h => (List.not_mem_nil h).elim⟩
        · exact ⟨rfl, fun _ h => (List.not_mem_nil h).elim⟩
        · exact ⟨trivial, fun _ h => (List.not_mem_nil h).elim⟩
  · rw [hFns]
    exact List.mem_singleton_self _
  · intro origin ho
    rw [ssa_memoryView_origins_eq (language := s0Lang) (twoSiteSSA a b c profile)] at ho
    have hOrigs : (twoSiteSSA a b c profile).origins = [orig0, orig0, orig1, orig1, orig2] := rfl
    rw [hOrigs] at ho
    simp only [List.mem_cons, List.not_mem_nil, or_false] at ho
    rcases ho with rfl | rfl | rfl | rfl | rfl
    · exact span0_binds
    · exact span0_binds
    · exact span1_binds
    · exact span1_binds
    · exact span2_binds
  · intro table ht
    rw [hLay] at ht
    cases ht
  · rw [hDecl]
    exact ⟨.nil, by decide, nofun⟩
  · refine ⟨by rw [hMut]; simp, ?_⟩
    intro fn roots h
    rw [hMut] at h
    cases List.mem_singleton.mp h
    refine ⟨_, _, by rw [hFns]; exact List.mem_cons_self, ?_⟩
    intro root hr
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hr
    rcases hr with rfl | rfl | rfl | rfl
    · exact ⟨u8Ty, List.mem_cons_self⟩
    · exact ⟨boolTy, List.mem_cons_of_mem _ List.mem_cons_self⟩
    · exact ⟨u8Ty, List.mem_cons_of_mem _ (List.mem_cons_of_mem _ List.mem_cons_self)⟩
    · exact ⟨boolTy, List.mem_cons_of_mem _ (List.mem_cons_of_mem _ (List.mem_cons_of_mem _ List.mem_cons_self))⟩

private theorem zeroSlotLoopSSA_wf (profile : Profile)
    (hReg : (zeroSlotLoopSSA profile).checkRegisterFormation (language := s0Lang) = true) :
    SSA.WF (language := s0Lang) (zeroSlotLoopSSA profile) := by
  have hFns : ((zeroSlotLoopSSA profile).memoryView (language := s0Lang)).functions =
      [(mainFn, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .jump ⟨⟨0⟩, []⟩, []⟩]⟩)] := by
    with_unfolding_all rfl
  have hStor : ((zeroSlotLoopSSA profile).memoryView (language := s0Lang)).storage =
      [(mainFn, [])] := by
    with_unfolding_all rfl
  have hSig : ((zeroSlotLoopSSA profile).memoryView (language := s0Lang)).signatures =
      [(mainFn, ⟨[], none⟩)] := by
    with_unfolding_all rfl
  have hLay : ((zeroSlotLoopSSA profile).memoryView (language := s0Lang)).layouts = [] := by
    with_unfolding_all rfl
  have hMut : ((zeroSlotLoopSSA profile).memoryView (language := s0Lang)).mutableLocals = [] := by
    with_unfolding_all rfl
  have hDecl : ((zeroSlotLoopSSA profile).memoryView (language := s0Lang)).declarations = [] := by
    with_unfolding_all rfl
  refine ⟨(SSA.checkRegisterFormation_eq_true_iff _).mp hReg, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · refine ⟨by rw [hFns]; simp, by rw [hSig]; simp, by rw [hStor]; simp, by rw [hLay]; exact .nil,
      ?_, by rw [hLay]; exact nofun, ?_⟩
    · intro fn cls h
      rw [hStor] at h
      cases List.mem_singleton.mp h
      exact .nil
    · intro fn locals graph h
      rw [hFns] at h
      cases List.mem_singleton.mp h
      refine ⟨.nil, ⟨by simp, ⟨_, List.mem_cons_self, rfl⟩, ?_⟩,
        ⟨⟨[], none⟩, by rw [hSig]; rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩, ?_⟩⟩
      · intro blk hblk edge hedge
        cases List.mem_singleton.mp hblk
        cases List.mem_singleton.mp hedge
        exact ⟨_, List.mem_cons_self, rfl, rfl⟩
      · intro blk hblk
        cases List.mem_singleton.mp hblk
        exact ⟨trivial, fun _ h => (List.not_mem_nil h).elim⟩
  · rw [hFns]
    exact List.mem_singleton_self _
  · intro origin ho
    rw [ssa_memoryView_origins_eq (language := s0Lang) (zeroSlotLoopSSA profile)] at ho
    cases ho
  · intro table ht
    rw [hLay] at ht
    cases ht
  · rw [hDecl]
    exact ⟨.nil, by decide, nofun⟩
  · rw [hMut]
    exact ⟨.nil, nofun⟩

private theorem mutableLocalSSA_wf (profile : Profile)
    (hReg : (mutableLocalSSA profile).checkRegisterFormation (language := s0Lang) = true) :
    SSA.WF (language := s0Lang) (mutableLocalSSA profile) := by
  have hFns : ((mutableLocalSSA profile).memoryView (language := s0Lang)).functions =
      [(mainFn, [(⟨0⟩, u8Ty)],
        ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), [orig0]⟩]⟩)] := by
    with_unfolding_all rfl
  have hStor : ((mutableLocalSSA profile).memoryView (language := s0Lang)).storage =
      [(mainFn, [(⟨0⟩, .materialized)])] := by
    with_unfolding_all rfl
  have hSig : ((mutableLocalSSA profile).memoryView (language := s0Lang)).signatures =
      [(mainFn, ⟨[], some u8Ty⟩)] := by
    with_unfolding_all rfl
  have hLay : ((mutableLocalSSA profile).memoryView (language := s0Lang)).layouts =
      [(mainFn, [(⟨0⟩, ⟨1, 1, .nil⟩)])] := by
    with_unfolding_all rfl
  have hMut : ((mutableLocalSSA profile).memoryView (language := s0Lang)).mutableLocals =
      [(mainFn, [⟨0⟩])] := by
    with_unfolding_all rfl
  have hDecl : ((mutableLocalSSA profile).memoryView (language := s0Lang)).declarations = [] := by
    with_unfolding_all rfl
  refine ⟨(SSA.checkRegisterFormation_eq_true_iff _).mp hReg, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · refine ⟨by rw [hFns]; simp, by rw [hSig]; simp, by rw [hStor]; simp, by rw [hLay]; simp,
      ?_, ?_, ?_⟩
    · intro fn cls h
      rw [hStor] at h
      cases List.mem_singleton.mp h
      decide
    · intro fn lays h
      rw [hLay] at h
      cases List.mem_singleton.mp h
      decide
    · intro fn locals graph h
      rw [hFns] at h
      cases List.mem_singleton.mp h
      refine ⟨by decide, ⟨by simp, ⟨_, List.mem_cons_self, rfl⟩, ?_⟩,
        ⟨⟨[], some u8Ty⟩, by rw [hSig]; rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩, ?_⟩⟩
      · intro blk hblk edge hedge
        cases List.mem_singleton.mp hblk
        cases hedge
      · intro blk hblk
        cases List.mem_singleton.mp hblk
        exact ⟨rfl, fun _ h => (List.not_mem_nil h).elim⟩
  · rw [hFns]
    exact List.mem_singleton_self _
  · intro origin ho
    rw [ssa_memoryView_origins_eq (language := s0Lang) (mutableLocalSSA profile)] at ho
    cases List.mem_singleton.mp ho
    exact span0_binds
  · intro table ht slot hslot
    rw [hLay] at ht
    cases List.mem_singleton.mp ht
    cases List.mem_singleton.mp hslot
    cases profile with
    | mk bk tg slots stk => cases tg <;> dsimp only [Profile.memoryTarget] <;> decide
  · rw [hDecl]
    exact ⟨.nil, by decide, nofun⟩
  · refine ⟨by rw [hMut]; simp, ?_⟩
    intro fn roots h
    rw [hMut] at h
    cases List.mem_singleton.mp h
    refine ⟨_, _, by rw [hFns]; exact List.mem_cons_self, ?_⟩
    intro root hr
    cases List.mem_singleton.mp hr
    exact ⟨u8Ty, List.mem_cons_self⟩

/-! ### 2. D5 reachable whole-program witnesses on fixed `slice0` -/

private theorem onInput_observes_eq {A C B E : Type} {S : Semantics B E}
    {f : A → B} {g : C → B} {a : A} {c : C} {o : Concrete.Contracts.Observation}
    (hEq : g c = f a) (hObs : (S.onInput f).observes a o) :
    (S.onInput g).observes c o := by
  unfold Semantics.observes Semantics.finite Semantics.onInput at hObs ⊢
  dsimp only at hObs ⊢
  rw [hEq]
  exact hObs

private theorem onInput_diverges_eq {A C B E : Type} {S : Semantics B E}
    {f : A → B} {g : C → B} {a : A} {c : C} {run : Nat → S.State}
    (hEq : g c = f a) (hDiv : (S.onInput f).diverges a run) :
    (S.onInput g).diverges c run := by
  unfold Semantics.diverges Semantics.onInput at hDiv ⊢
  dsimp only at hDiv ⊢
  rw [hEq]
  exact hDiv

private theorem bir_observes_of_ssa {profile : Profile} {ssa : SSA s0Low profile}
    {bir : BIR s0Low profile} {obs : Concrete.Contracts.Observation}
    (hBir : buildBIR ssa = .ok bir)
    (hObs : (SSA.Execution (language := s0Lang)).observes ssa obs) :
    (BIR.Execution (language := s0Lang)).observes bir obs :=
  onInput_observes_eq (buildBIR_memoryView_eq (language := s0Lang) hBir) hObs

private theorem bir_diverges_of_ssa {profile : Profile} {ssa : SSA s0Low profile}
    {bir : BIR s0Low profile} {run : Nat → MemoryRegisters s0Lang profile}
    (hBir : buildBIR ssa = .ok bir)
    (hDiv : (SSA.Execution (language := s0Lang)).diverges ssa run) :
    (BIR.Execution (language := s0Lang)).diverges bir run :=
  onInput_diverges_eq (buildBIR_memoryView_eq (language := s0Lang) hBir) hDiv

-- Witness 1: straight-line `return 42` at `maxBIRSlots = 0`.
example :
    let p := lcbProfile 0
    let ssa := returnSSA 42 p
    let edge := (registry (prodPasses p)).buildBIR
    ∃ bir,
      edge.inputInvariant ssa ∧
      edge.run ssa = .ok bir ∧
      edge.outputInvariant bir ∧
      edge.success ssa bir ∧
      bir.frameSlots = 0 ∧
      bir.sites = #[] ∧
      (SSA.Execution (language := s0Lang)).observes ssa (.returned 42 ByteArray.empty) ∧
      (BIR.Execution (language := s0Lang)).observes bir (.returned 42 ByteArray.empty) := by
  intro p ssa edge
  have hWF : SSA.WF (language := s0Lang) ssa := returnSSA_wf 42 p (by decide)
  obtain ⟨bir, hBir⟩ := buildBIR_ok_of_wf hWF (by decide)
  let s0 : MemoryRegisters s0Lang p :=
    ⟨⟨mainFn, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩
  have hInit : (SSA.Execution (language := s0Lang)).initial ssa s0 := by
    with_unfolding_all rfl
  have hObsSSA : (SSA.Execution (language := s0Lang)).observes ssa (.returned 42 ByteArray.empty) := by
    refine ⟨s0, ⟨s0, hInit, .nil _⟩, ?_⟩
    with_unfolding_all rfl
  refine ⟨bir, hWF, hBir, buildBIR_wf hWF hBir, ⟨buildBIR_buildsBIR hBir, buildBIR_mutableLocals_eq hBir⟩,
    buildBIR_frameSlots_eq hBir, ?_, hObsSSA, bir_observes_of_ssa hBir hObsSSA⟩
  have hEmpty : bir.sites.toList = [] := by
    cases hList : bir.sites.toList with
    | nil => rfl
    | cons entry _ =>
      have hMem : entry ∈ bir.sites.toList := hList ▸ List.mem_cons_self
      obtain ⟨fn, hfn, blk, hblk, hEnd⟩ := buildBIR_sites_from_ssa hBir entry hMem
      cases List.mem_singleton.mp hfn
      cases List.mem_singleton.mp hblk
      cases hEnd
  exact Array.ext' hEmpty

-- Witness 2a: `addWithOverflow` frame-slot ceiling boundary (2 slots: refused at 1, accepted at 2)
-- and reachable exit-42 observation for `20 + 22`.
set_option maxRecDepth 4096 in
example :
    let p1 := lcbProfile 1
    let p2 := lcbProfile 2
    let ssa1 := checkedAddSSA 20 22 p1
    let ssa2 := checkedAddSSA 20 22 p2
    let edge1 := (registry (prodPasses p1)).buildBIR
    let edge2 := (registry (prodPasses p2)).buildBIR
    let expectedFailure := StageFailure.resource (frameSlotRefusal 2 1)
    ssa1.frameSlots = 2 ∧
    edge1.inputInvariant ssa1 ∧
    edge1.run ssa1 = .error expectedFailure ∧
    edge1.refusal ssa1 expectedFailure ∧
    ∃ bir,
      edge2.inputInvariant ssa2 ∧
      edge2.run ssa2 = .ok bir ∧
      edge2.outputInvariant bir ∧
      edge2.success ssa2 bir ∧
      bir.frameSlots = 2 ∧
      bir.sites = #[⟨.overflow, orig0⟩] ∧
      (SSA.Execution (language := s0Lang)).observes ssa2 (.returned 42 ByteArray.empty) ∧
      (BIR.Execution (language := s0Lang)).observes bir (.returned 42 ByteArray.empty) := by
  intro p1 p2 ssa1 ssa2 edge1 edge2 expectedFailure
  have hWF1 : SSA.WF (language := s0Lang) ssa1 := checkedAddSSA_wf 20 22 p1 (by decide)
  have hWF2 : SSA.WF (language := s0Lang) ssa2 := checkedAddSSA_wf 20 22 p2 (by decide)
  have hSlots1 : ssa1.frameSlots = 2 := rfl
  have hRef1 : edge1.run ssa1 = .error expectedFailure :=
    buildBIR_refusal_of_gt (by decide : p1.maxBIRSlots < ssa1.frameSlots)
  obtain ⟨bir, hBir⟩ := buildBIR_ok_of_wf hWF2 (by decide : ssa2.frameSlots ≤ p2.maxBIRSlots)
  have hSites : bir.sites = #[⟨.overflow, orig0⟩] := by
    have hEval : buildBIR ssa2 = .ok ⟨tabProject, mainFn, [(mainFn, ⟨[], some u8Ty⟩)],
        [(mainFn, [], ⟨⟨0⟩, #[
          ⟨⟨0⟩, [], [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)⟩],
            .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩,
          ⟨⟨1⟩, [], [], .trap ⟨0⟩, []⟩,
          ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩]⟩)],
        [], #[⟨.overflow, orig0⟩], [], []⟩ := rfl
    rw [hEval] at hBir
    cases Except.ok.inj hBir
    rfl
  let start : MemoryRegisters s0Lang p2 :=
    ⟨⟨mainFn, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩
  let mid : MemoryRegisters s0Lang p2 :=
    {start with
      instruction := 1
      live := [⟨1⟩, ⟨0⟩]
      initialized := [⟨1⟩, ⟨0⟩]
      virtualValues := [(⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 42))]}
  let atRet : MemoryRegisters s0Lang p2 := {mid with block := ⟨2⟩, instruction := 0}
  have hInit : (SSA.Execution (language := s0Lang)).initial ssa2 start := by
    with_unfolding_all rfl
  have hStep0 : (SSA.Execution (language := s0Lang)).step ssa2 start mid := by
    let st0 : MemoryRegisters s0Lang p2 := {start with live := [⟨0⟩]}
    let st1 : MemoryRegisters s0Lang p2 :=
      {st0 with initialized := [⟨0⟩], virtualValues := [(⟨0⟩, .scalar (.int .u8 42))]}
    let st2 : MemoryRegisters s0Lang p2 := {st1 with live := [⟨1⟩, ⟨0⟩]}
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, rfl, rfl, Or.inl (by decide),
        .scalar (.int .u8 20), start, .scalar (.int .u8 22), start,
        20, 22, {mid with instruction := 0}, rfl, rfl, rfl, rfl, ?_, rfl⟩, [], rfl⟩
    apply Memory.WriteParams.cons (started := st0) (stored := st1)
    · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
    · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := st2) (stored := {mid with instruction := 0})
      · with_unfolding_all exact ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
  have hStep1 : (SSA.Execution (language := s0Lang)).step ssa2 mid atRet := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, rfl, .scalar (.bool false), mid, false, rfl, rfl,
        [], mid, _, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  have hObsSSA : (SSA.Execution (language := s0Lang)).observes ssa2 (.returned 42 ByteArray.empty) := by
    refine ⟨atRet, ⟨start, hInit, .cons hStep0 (.cons hStep1 (.nil _))⟩, ?_⟩
    with_unfolding_all rfl
  refine ⟨hSlots1, hWF1, hRef1, ⟨rfl, rfl, hSlots1.symm, by decide⟩,
    bir, hWF2, hBir, buildBIR_wf hWF2 hBir, ⟨buildBIR_buildsBIR hBir, buildBIR_mutableLocals_eq hBir⟩,
    buildBIR_frameSlots_eq hBir, hSites, hObsSSA, bir_observes_of_ssa hBir hObsSSA⟩

-- Witness 2b: `addWithOverflow` overflow path (`255 + 1`) traps at allocated slot 0
-- with exact diagnostic site and stderr bytes in both SSA and BIR.
set_option maxRecDepth 4096 in
example :
    let p := lcbProfile 2
    let ssa := checkedAddSSA 255 1 p
    let site0 : DiagnosticSite := ⟨⟨0⟩, ⟨mainSourcePath.render.text, 1, 1⟩⟩
    let expectedObs : Concrete.Contracts.Observation :=
      .trapped 134 .overflow site0 (renderTrapLine .overflow site0).toUTF8
    ∃ bir,
      buildBIR ssa = .ok bir ∧
      bir.sites = #[⟨.overflow, orig0⟩] ∧
      (SSA.Execution (language := s0Lang)).observes ssa expectedObs ∧
      (BIR.Execution (language := s0Lang)).observes bir expectedObs := by
  intro p ssa site0 expectedObs
  have hWF : SSA.WF (language := s0Lang) ssa := checkedAddSSA_wf 255 1 p (by decide)
  obtain ⟨bir, hBir⟩ := buildBIR_ok_of_wf hWF (by decide : ssa.frameSlots ≤ p.maxBIRSlots)
  have hSites : bir.sites = #[⟨.overflow, orig0⟩] := by
    have hEval : buildBIR ssa = .ok ⟨tabProject, mainFn, [(mainFn, ⟨[], some u8Ty⟩)],
        [(mainFn, [], ⟨⟨0⟩, #[
          ⟨⟨0⟩, [], [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[255]⟩) (.literal ⟨[1]⟩)⟩],
            .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩,
          ⟨⟨1⟩, [], [], .trap ⟨0⟩, []⟩,
          ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩]⟩)],
        [], #[⟨.overflow, orig0⟩], [], []⟩ := rfl
    rw [hEval] at hBir
    cases Except.ok.inj hBir
    rfl
  let start : MemoryRegisters s0Lang p :=
    ⟨⟨mainFn, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩
  let mid : MemoryRegisters s0Lang p :=
    {start with
      instruction := 1
      live := [⟨1⟩, ⟨0⟩]
      initialized := [⟨1⟩, ⟨0⟩]
      virtualValues := [(⟨1⟩, .scalar (.bool true)), (⟨0⟩, .scalar (.int .u8 0))]}
  let atTrap : MemoryRegisters s0Lang p := {mid with block := ⟨1⟩, instruction := 0}
  have hInit : (SSA.Execution (language := s0Lang)).initial ssa start := by
    with_unfolding_all rfl
  have hStep0 : (SSA.Execution (language := s0Lang)).step ssa start mid := by
    let st0 : MemoryRegisters s0Lang p := {start with live := [⟨0⟩]}
    let st1 : MemoryRegisters s0Lang p :=
      {st0 with initialized := [⟨0⟩], virtualValues := [(⟨0⟩, .scalar (.int .u8 0))]}
    let st2 : MemoryRegisters s0Lang p := {st1 with live := [⟨1⟩, ⟨0⟩]}
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, rfl, rfl, Or.inl (by decide),
        .scalar (.int .u8 255), start, .scalar (.int .u8 1), start,
        255, 1, {mid with instruction := 0}, rfl, rfl, rfl, rfl, ?_, rfl⟩, [], rfl⟩
    apply Memory.WriteParams.cons (started := st0) (stored := st1)
    · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
    · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := st2) (stored := {mid with instruction := 0})
      · with_unfolding_all exact ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
  have hStep1 : (SSA.Execution (language := s0Lang)).step ssa mid atTrap := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, rfl, .scalar (.bool true), mid, true, rfl, rfl,
        [], mid, _, rfl, rfl, rfl, .nil _⟩, _, rfl⟩
  have hObsSSA : (SSA.Execution (language := s0Lang)).observes ssa expectedObs := by
    refine ⟨atTrap, ⟨start, hInit, .cons hStep0 (.cons hStep1 (.nil _))⟩, ?_⟩
    with_unfolding_all rfl
  exact ⟨bir, hBir, hSites, hObsSSA, bir_observes_of_ssa hBir hObsSSA⟩

-- Witness 3: multi-site trap table including an unreachable trap block (`twoSiteSSA`).
-- Verifies deterministic sequential trap-slot allocation (`sites = [orig0, orig1, orig2]`),
-- ceiling boundary (`frameSlots = 4`: refused at 3, accepted at 4), project-bound unreachable
-- trap site, and reachable execution trapping at the SECOND site (`orig1`).
set_option maxRecDepth 4096 in
example :
    let p3 := llvmProfile 3
    let p4 := llvmProfile 4
    let ssa3 := twoSiteSSA 100 100 100 p3
    let ssa4 := twoSiteSSA 100 100 100 p4
    let edge3 := (registry (prodPasses p3)).buildBIR
    let edge4 := (registry (prodPasses p4)).buildBIR
    let site1 : DiagnosticSite := ⟨⟨1⟩, ⟨mainSourcePath.render.text, 1, 3⟩⟩
    let expectedObs : Concrete.Contracts.Observation :=
      .trapped 134 .overflow site1 (renderTrapLine .overflow site1).toUTF8
    ssa3.frameSlots = 4 ∧
    edge3.inputInvariant ssa3 ∧
    edge3.run ssa3 = .error (.resource (frameSlotRefusal 4 3)) ∧
    edge3.refusal ssa3 (.resource (frameSlotRefusal 4 3)) ∧
    ∃ bir,
      edge4.inputInvariant ssa4 ∧
      edge4.run ssa4 = .ok bir ∧
      edge4.outputInvariant bir ∧
      edge4.success ssa4 bir ∧
      bir.frameSlots = 4 ∧
      bir.sites = #[⟨.overflow, orig0⟩, ⟨.overflow, orig1⟩, ⟨.abort, orig2⟩] ∧
      (BIR.Execution (language := s0Lang)).observes bir expectedObs := by
  intro p3 p4 ssa3 ssa4 edge3 edge4 site1 expectedObs
  have hWF3 : SSA.WF (language := s0Lang) ssa3 := twoSiteSSA_wf 100 100 100 p3 (by decide)
  have hWF4 : SSA.WF (language := s0Lang) ssa4 := twoSiteSSA_wf 100 100 100 p4 (by decide)
  have hRef3 : edge3.run ssa3 = .error (.resource (frameSlotRefusal 4 3)) :=
    buildBIR_refusal_of_gt (by decide : p3.maxBIRSlots < ssa3.frameSlots)
  obtain ⟨bir, hBir⟩ := buildBIR_ok_of_wf hWF4 (by decide : ssa4.frameSlots ≤ p4.maxBIRSlots)
  have hSites : bir.sites = #[⟨.overflow, orig0⟩, ⟨.overflow, orig1⟩, ⟨.abort, orig2⟩] := by
    have hEval : buildBIR ssa4 = .ok ⟨tabProject, mainFn, [(mainFn, ⟨[], some u8Ty⟩)],
        [(mainFn, [], ⟨⟨0⟩, #[
          ⟨⟨0⟩, [], [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[100]⟩) (.literal ⟨[100]⟩)⟩],
            .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩,
          ⟨⟨1⟩, [], [], .trap ⟨0⟩, []⟩,
          ⟨⟨2⟩, [], [⟨orig1, .addWithOverflow .u8 ⟨2⟩ ⟨3⟩ (.value ⟨0⟩) (.literal ⟨[100]⟩)⟩],
            .branch (.value ⟨3⟩) ⟨⟨3⟩, []⟩ ⟨⟨4⟩, []⟩, []⟩,
          ⟨⟨3⟩, [], [], .trap ⟨1⟩, []⟩,
          ⟨⟨4⟩, [], [], .ret (some ⟨u8Ty, .value ⟨2⟩⟩), []⟩,
          ⟨⟨5⟩, [], [], .trap ⟨2⟩, []⟩]⟩)],
        [], #[⟨.overflow, orig0⟩, ⟨.overflow, orig1⟩, ⟨.abort, orig2⟩], [], []⟩ := rfl
    rw [hEval] at hBir
    cases Except.ok.inj hBir
    rfl
  let s0 : MemoryRegisters s0Lang p4 :=
    ⟨⟨mainFn, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩
  let s1 : MemoryRegisters s0Lang p4 :=
    {s0 with
      instruction := 1
      live := [⟨1⟩, ⟨0⟩]
      initialized := [⟨1⟩, ⟨0⟩]
      virtualValues := [(⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 200))]}
  let s2 : MemoryRegisters s0Lang p4 := {s1 with block := ⟨2⟩, instruction := 0}
  let s3 : MemoryRegisters s0Lang p4 :=
    {s2 with
      instruction := 1
      live := [⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩]
      initialized := [⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩]
      virtualValues := [(⟨3⟩, .scalar (.bool true)), (⟨2⟩, .scalar (.int .u8 44)),
        (⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 200))]}
  let s4 : MemoryRegisters s0Lang p4 := {s3 with block := ⟨3⟩, instruction := 0}
  have hInit : (SSA.Execution (language := s0Lang)).initial ssa4 s0 := by
    with_unfolding_all rfl
  have h01 : (SSA.Execution (language := s0Lang)).step ssa4 s0 s1 := by
    let st0 : MemoryRegisters s0Lang p4 := {s0 with live := [⟨0⟩]}
    let st1 : MemoryRegisters s0Lang p4 :=
      {st0 with initialized := [⟨0⟩], virtualValues := [(⟨0⟩, .scalar (.int .u8 200))]}
    let st2 : MemoryRegisters s0Lang p4 := {st1 with live := [⟨1⟩, ⟨0⟩]}
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, rfl, rfl, Or.inl (by decide),
        .scalar (.int .u8 100), s0, .scalar (.int .u8 100), s0,
        100, 100, {s1 with instruction := 0}, rfl, rfl, rfl, rfl, ?_, rfl⟩, [], rfl⟩
    apply Memory.WriteParams.cons (started := st0) (stored := st1)
    · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
    · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := st2) (stored := {s1 with instruction := 0})
      · with_unfolding_all exact ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
  have h12 : (SSA.Execution (language := s0Lang)).step ssa4 s1 s2 := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, rfl, .scalar (.bool false), s1, false, rfl, rfl,
        [], s1, _, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  have h23 : (SSA.Execution (language := s0Lang)).step ssa4 s2 s3 := by
    let st0 : MemoryRegisters s0Lang p4 := {s2 with live := [⟨2⟩, ⟨1⟩, ⟨0⟩]}
    let st1 : MemoryRegisters s0Lang p4 :=
      {st0 with
        initialized := [⟨2⟩, ⟨1⟩, ⟨0⟩]
        virtualValues := [(⟨2⟩, .scalar (.int .u8 44)),
          (⟨1⟩, .scalar (.bool false)), (⟨0⟩, .scalar (.int .u8 200))]}
    let st2 : MemoryRegisters s0Lang p4 := {st1 with live := [⟨3⟩, ⟨2⟩, ⟨1⟩, ⟨0⟩]}
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, rfl, rfl, Or.inl (by decide),
        .scalar (.int .u8 200), s2, .scalar (.int .u8 100), s2,
        200, 100, {s3 with instruction := 0}, rfl, rfl, rfl, rfl, ?_, rfl⟩, [], rfl⟩
    apply Memory.WriteParams.cons (started := st0) (stored := st1)
    · with_unfolding_all exact ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
    · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
    · apply Memory.WriteParams.cons (started := st2) (stored := {s3 with instruction := 0})
      · with_unfolding_all exact ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
  have h34 : (SSA.Execution (language := s0Lang)).step ssa4 s3 s4 := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, rfl, .scalar (.bool true), s3, true, rfl, rfl,
        [], s3, _, rfl, rfl, rfl, .nil _⟩, _, rfl⟩
  have hObsSSA : (SSA.Execution (language := s0Lang)).observes ssa4 expectedObs := by
    refine ⟨s4, ⟨s0, hInit, .cons h01 (.cons h12 (.cons h23 (.cons h34 (.nil _))))⟩, ?_⟩
    with_unfolding_all rfl
  refine ⟨rfl, hWF3, hRef3, ⟨rfl, rfl, rfl, by decide⟩,
    bir, hWF4, hBir, buildBIR_wf hWF4 hBir, ⟨buildBIR_buildsBIR hBir, buildBIR_mutableLocals_eq hBir⟩,
    buildBIR_frameSlots_eq hBir, hSites, bir_observes_of_ssa hBir hObsSSA⟩

-- Witness 4: zero-slot self-loop accepted at `maxBIRSlots = 0` with diverging execution
-- in both `SSA.Execution` and `BIR.Execution`.
set_option maxRecDepth 4096 in
example :
    let p := lcbProfile 0
    let ssa := zeroSlotLoopSSA p
    let edge := (registry (prodPasses p)).buildBIR
    ∃ bir,
      ssa.frameSlots = 0 ∧
      edge.inputInvariant ssa ∧
      edge.run ssa = .ok bir ∧
      edge.outputInvariant bir ∧
      edge.success ssa bir ∧
      bir.frameSlots = 0 ∧
      bir.sites = #[] ∧
      (∃ run, (SSA.Execution (language := s0Lang)).diverges ssa run) ∧
      (∃ run, (BIR.Execution (language := s0Lang)).diverges bir run) := by
  intro p ssa edge
  have hWF : SSA.WF (language := s0Lang) ssa := zeroSlotLoopSSA_wf p (by decide)
  obtain ⟨bir, hBir, hZero, hBirWF, hBuilds⟩ := buildBIR_zero_slot_ok hWF rfl
  let s0 : MemoryRegisters s0Lang p :=
    ⟨⟨mainFn, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩
  have hInit : (SSA.Execution (language := s0Lang)).initial ssa s0 := by
    with_unfolding_all rfl
  have hLoopStep : (SSA.Execution (language := s0Lang)).step ssa s0 s0 := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, rfl, [], s0, _, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  have hDivSSA : (SSA.Execution (language := s0Lang)).diverges ssa (fun _ => s0) :=
    ⟨hInit, fun _ => hLoopStep⟩
  have hEmpty : bir.sites = #[] := by
    apply Array.ext'
    cases hList : bir.sites.toList with
    | nil => rfl
    | cons entry _ =>
      obtain ⟨fn, hfn, blk, hblk, hEnd⟩ := buildBIR_sites_from_ssa hBir entry (hList ▸ List.mem_cons_self)
      cases List.mem_singleton.mp hfn
      cases List.mem_singleton.mp hblk
      cases hEnd
  exact ⟨bir, rfl, hWF, hBir, hBirWF, ⟨hBuilds, buildBIR_mutableLocals_eq hBir⟩,
    hZero, hEmpty, ⟨fun _ => s0, hDivSSA⟩, ⟨fun _ => s0, bir_diverges_of_ssa hBir hDivSSA⟩⟩

-- Witness 5: materialized local with non-empty `mutableLocals` preserved across BIR formation.
example :
    let p := lcbProfile 0
    let ssa := mutableLocalSSA p
    let edge := (registry (prodPasses p)).buildBIR
    ∃ bir,
      edge.inputInvariant ssa ∧
      edge.run ssa = .ok bir ∧
      edge.outputInvariant bir ∧
      edge.success ssa bir ∧
      bir.mutableLocals = [(mainFn, [⟨0⟩])] := by
  intro p ssa edge
  have hWF : SSA.WF (language := s0Lang) ssa := mutableLocalSSA_wf p (by decide)
  obtain ⟨bir, hBir⟩ := buildBIR_ok_of_wf hWF (by decide)
  exact ⟨bir, hWF, hBir, buildBIR_wf hWF hBir,
    ⟨buildBIR_buildsBIR hBir, buildBIR_mutableLocals_eq hBir⟩, buildBIR_mutableLocals_eq hBir⟩

-- Witness 6: off-domain `.invalid` terminator is refused cleanly with `.invalidInput`.
example :
    buildBIR (invalidTermSSA (lcbProfile 4)) = .error (.invalidInput invalidTerminatorRule) :=
  rfl

-- Witness 7: multi-function SSA verifies `frameSlots = max(main=0, helper=2) = 2` ceiling
-- enforcement and deterministic cross-function sequential trap-slot allocation (`sites = [orig0, orig1]`).
private theorem multiFnSSA_wf (profile : Profile)
    (hReg : (multiFnSSA profile).checkRegisterFormation (language := s0Lang) = true) :
    SSA.WF (language := s0Lang) (multiFnSSA profile) := by
  have hFns : ((multiFnSSA profile).memoryView (language := s0Lang)).functions =
      [(mainFn, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .trap .abort orig0, []⟩]⟩),
       (helperFn, [(⟨0⟩, u8Ty), (⟨1⟩, boolTy)],
         ⟨⟨0⟩, #[
           ⟨⟨0⟩, [], [⟨orig1, .addWithOverflow .u8 ⟨⟨⟨0⟩, []⟩, .virtual⟩ ⟨⟨⟨1⟩, []⟩, .virtual⟩
             (.literal ⟨[10]⟩) (.literal ⟨[20]⟩)⟩],
             .branch (.copy ⟨⟨⟨1⟩, []⟩, .virtual⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩,
           ⟨⟨1⟩, [], [], .trap .overflow orig1, []⟩,
           ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .copy ⟨⟨⟨0⟩, []⟩, .virtual⟩⟩), []⟩]⟩)] := by
    with_unfolding_all rfl
  have hStor : ((multiFnSSA profile).memoryView (language := s0Lang)).storage =
      [(mainFn, []), (helperFn, [(⟨0⟩, .virtual), (⟨1⟩, .virtual)])] := by
    with_unfolding_all rfl
  have hSig : ((multiFnSSA profile).memoryView (language := s0Lang)).signatures =
      [(mainFn, ⟨[], some u8Ty⟩), (helperFn, ⟨[], some u8Ty⟩)] := by
    with_unfolding_all rfl
  have hLay : ((multiFnSSA profile).memoryView (language := s0Lang)).layouts = [] := by
    with_unfolding_all rfl
  have hMut : ((multiFnSSA profile).memoryView (language := s0Lang)).mutableLocals =
      [(helperFn, [⟨0⟩, ⟨1⟩])] := by
    with_unfolding_all rfl
  have hDecl : ((multiFnSSA profile).memoryView (language := s0Lang)).declarations = [] := by
    with_unfolding_all rfl
  refine ⟨(SSA.checkRegisterFormation_eq_true_iff _).mp hReg, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · refine ⟨by rw [hFns]; decide, by rw [hSig]; decide, by rw [hStor]; decide, by rw [hLay]; exact .nil,
      ?_, by rw [hLay]; exact nofun, ?_⟩
    · intro fn cls h
      rw [hStor] at h
      simp only [List.mem_cons, List.not_mem_nil, or_false] at h
      rcases h with heq | heq <;> cases heq <;> decide
    · intro fn locals graph h
      rw [hFns] at h
      simp only [List.mem_cons, List.not_mem_nil, or_false] at h
      rcases h with heq | heq
      · cases heq
        refine ⟨.nil, ⟨by simp, ⟨_, List.mem_cons_self, rfl⟩, ?_⟩,
          ⟨⟨[], some u8Ty⟩, by rw [hSig]; rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩, ?_⟩⟩
        · intro blk hblk edge hedge
          cases List.mem_singleton.mp hblk
          cases hedge
        · intro blk hblk
          cases List.mem_singleton.mp hblk
          exact ⟨trivial, fun _ h => (List.not_mem_nil h).elim⟩
      · cases heq
        refine ⟨by decide, ⟨by simp, ⟨_, List.mem_cons_self, rfl⟩, ?_⟩,
          ⟨⟨[], some u8Ty⟩, by rw [hSig]; rfl, ⟨_, List.mem_cons_self, rfl, rfl⟩, ?_⟩⟩
        · intro blk hblk edge hedge
          simp only [List.mem_cons, List.not_mem_nil, or_false] at hblk
          rcases hblk with rfl | rfl | rfl
          · simp only [Terminator.successors, List.mem_cons, List.not_mem_nil, or_false] at hedge
            rcases hedge with rfl | rfl
            · exact ⟨_, List.mem_cons_of_mem _ List.mem_cons_self, rfl, rfl⟩
            · exact ⟨_, List.mem_cons_of_mem _ (List.mem_cons_of_mem _ List.mem_cons_self), rfl, rfl⟩
          · cases hedge
          · cases hedge
        · intro blk hblk
          simp only [List.mem_cons, List.not_mem_nil, or_false] at hblk
          rcases hblk with rfl | rfl | rfl
          · exact ⟨trivial, fun op hop => by cases List.mem_singleton.mp hop; trivial⟩
          · exact ⟨trivial, fun _ h => (List.not_mem_nil h).elim⟩
          · exact ⟨rfl, fun _ h => (List.not_mem_nil h).elim⟩
  · rw [hFns]
    exact List.mem_cons_self
  · intro origin ho
    rw [ssa_memoryView_origins_eq (language := s0Lang) (multiFnSSA profile)] at ho
    have hOrigs : (multiFnSSA profile).origins = [orig0, orig1, orig1] := rfl
    rw [hOrigs] at ho
    simp only [List.mem_cons, List.not_mem_nil, or_false] at ho
    rcases ho with rfl | rfl | rfl
    · exact span0_binds
    · exact span1_binds
    · exact span1_binds
  · intro table ht
    rw [hLay] at ht
    cases ht
  · rw [hDecl]
    exact ⟨.nil, by decide, nofun⟩
  · refine ⟨by rw [hMut]; simp, ?_⟩
    intro fn roots h
    rw [hMut] at h
    cases List.mem_singleton.mp h
    refine ⟨_, _, by rw [hFns]; exact List.mem_cons_of_mem _ List.mem_cons_self, ?_⟩
    intro root hr
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hr
    rcases hr with rfl | rfl
    · exact ⟨u8Ty, List.mem_cons_self⟩
    · exact ⟨boolTy, List.mem_cons_of_mem _ List.mem_cons_self⟩

example :
    let p1 := lcbProfile 1
    let p2 := lcbProfile 2
    let ssa1 := multiFnSSA p1
    let ssa2 := multiFnSSA p2
    let edge1 := (registry (prodPasses p1)).buildBIR
    let edge2 := (registry (prodPasses p2)).buildBIR
    ssa1.frameSlots = 2 ∧
    edge1.inputInvariant ssa1 ∧
    edge1.run ssa1 = .error (.resource (frameSlotRefusal 2 1)) ∧
    edge1.refusal ssa1 (.resource (frameSlotRefusal 2 1)) ∧
    ∃ bir,
      edge2.inputInvariant ssa2 ∧
      edge2.run ssa2 = .ok bir ∧
      edge2.outputInvariant bir ∧
      edge2.success ssa2 bir ∧
      bir.frameSlots = 2 ∧
      bir.sites = #[⟨.abort, orig0⟩, ⟨.overflow, orig1⟩] := by
  intro p1 p2 ssa1 ssa2 edge1 edge2
  have hWF1 : SSA.WF (language := s0Lang) ssa1 := multiFnSSA_wf p1 (by decide)
  have hWF2 : SSA.WF (language := s0Lang) ssa2 := multiFnSSA_wf p2 (by decide)
  have hRef1 : edge1.run ssa1 = .error (.resource (frameSlotRefusal 2 1)) :=
    buildBIR_refusal_of_gt (by decide : p1.maxBIRSlots < ssa1.frameSlots)
  obtain ⟨bir, hBir⟩ := buildBIR_ok_of_wf hWF2 (by decide : ssa2.frameSlots ≤ p2.maxBIRSlots)
  have hSites : bir.sites = #[⟨.abort, orig0⟩, ⟨.overflow, orig1⟩] := by
    have hEval : buildBIR ssa2 = .ok ⟨tabProject, mainFn,
        [(mainFn, ⟨[], some u8Ty⟩), (helperFn, ⟨[], some u8Ty⟩)],
        [(mainFn, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .trap ⟨0⟩, []⟩]⟩),
         (helperFn, [], ⟨⟨0⟩, #[
           ⟨⟨0⟩, [], [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[10]⟩) (.literal ⟨[20]⟩)⟩],
             .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩,
           ⟨⟨1⟩, [], [], .trap ⟨1⟩, []⟩,
           ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩]⟩)],
        [], #[⟨.abort, orig0⟩, ⟨.overflow, orig1⟩], [], []⟩ := rfl
    rw [hEval] at hBir
    cases Except.ok.inj hBir
    rfl
  exact ⟨rfl, hWF1, hRef1, ⟨rfl, rfl, rfl, by decide⟩,
    bir, hWF2, hBir, buildBIR_wf hWF2 hBir, ⟨buildBIR_buildsBIR hBir, buildBIR_mutableLocals_eq hBir⟩,
    buildBIR_frameSlots_eq hBir, hSites⟩

/-! ### 3. D5 rejected wrong-compiler mutants on fixed `slice0` -/

-- Mutant 1: alters the returned literal `42` to `43`.
example :
    let p := lcbProfile 0
    let mutant := passesWithBuildBIR p fun _ =>
      buildBIR (returnSSA 43 p)
    ¬ (registry mutant).buildBIR.Validated := by
  intro p mutant hVal
  have hWF := returnSSA_wf 42 p (by decide)
  obtain ⟨bir43, hOk⟩ := buildBIR_ok_of_wf (returnSSA_wf 43 p (by decide)) (by decide)
  have hSucc := (hVal.1 (returnSSA 42 p) bir43 hWF hOk).2.1
  have hEntry := hSucc.2.2.2.2.2.1.2.2
  have hBir43Entry : bir43.entry = (returnSSA 43 p).entry :=
    (buildBIR_buildsBIR (language := s0Lang) hOk).2.2.2.2.2.1.1
  have hView43 := buildBIR_memoryView_eq (language := s0Lang) hOk
  rw [hView43, hBir43Entry] at hEntry
  have hRet := congrArg (fun g => do
    let graph ← g
    let blk ← graph.blocks[0]?
    match blk.ending with
    | .ret (some ⟨_, .literal lit⟩) => some lit.encoding
    | _ => none) hEntry
  have hWrong : some ([42] : List UInt8) = some [43] := by with_unfolding_all exact hRet
  cases Option.some.inj hWrong

-- Mutant 2: corrupts `bir.sites[0].kind` from `.overflow` to `.abort`.
example :
    let p := lcbProfile 2
    let mutant := passesWithBuildBIR p fun ssa =>
      (buildBIR ssa).map fun bir =>
        { bir with sites := bir.sites.map fun e => { e with kind := .abort } }
    ¬ (registry mutant).buildBIR.Validated := by
  intro p mutant hVal
  let ssa := checkedAddSSA 255 1 p
  have hWF := checkedAddSSA_wf 255 1 p (by decide)
  obtain ⟨bir, hOk⟩ := buildBIR_ok_of_wf hWF (by decide : ssa.frameSlots ≤ p.maxBIRSlots)
  let bad : BIR s0Low p := { bir with sites := bir.sites.map fun e => { e with kind := .abort } }
  have hRun : (registry mutant).buildBIR.run ssa = .ok bad := by
    dsimp only [registry, passesWithBuildBIR, mutant]
    rw [hOk]; rfl
  have hTraps := (hVal.1 ssa bad hWF hRun).2.1.2.2.1
  obtain ⟨_, _, _, _, _, _, slot, _, entry, hGet, hKind, _⟩ :=
    hTraps (mainFn, [], _) List.mem_cons_self ⟨⟨1⟩, [], [], .trap .overflow orig0, []⟩
      (List.mem_cons_of_mem _ List.mem_cons_self) .overflow orig0 rfl
  have hAbort : entry.kind = .abort := by
    simp only [bad, Array.getElem?_map] at hGet
    cases hOrig : bir.sites[slot.index]? with
    | none => rw [hOrig] at hGet; cases hGet
    | some e =>
      rw [hOrig] at hGet
      cases Option.some.inj hGet
      rfl
  rw [hKind] at hAbort
  cases hAbort

-- Mutant 3: swaps `bir.sites[0]` and `bir.sites[1]` on `twoSiteSSA`.
example :
    let p := llvmProfile 4
    let mutant := passesWithBuildBIR p fun ssa =>
      (buildBIR ssa).map fun bir =>
        { bir with sites := #[⟨.overflow, orig1⟩, ⟨.overflow, orig0⟩, ⟨.abort, orig2⟩] }
    ¬ (registry mutant).buildBIR.Validated := by
  intro p mutant hVal
  let ssa := twoSiteSSA 100 100 100 p
  have hWF := twoSiteSSA_wf 100 100 100 p (by decide)
  obtain ⟨bir, hOk⟩ := buildBIR_ok_of_wf hWF (by decide : ssa.frameSlots ≤ p.maxBIRSlots)
  let bad : BIR s0Low p :=
    { bir with sites := #[⟨.overflow, orig1⟩, ⟨.overflow, orig0⟩, ⟨.abort, orig2⟩] }
  have hRun : (registry mutant).buildBIR.run ssa = .ok bad := by
    dsimp only [registry, passesWithBuildBIR, mutant]
    rw [hOk]; rfl
  have hTraps := (hVal.1 ssa bad hWF hRun).2.1.2.2.1
  obtain ⟨fn', hfn', _, blk', hblk', hBlkId, slot, hEnd, entry, hGet, _, hOrig⟩ :=
    hTraps (mainFn, [], _) List.mem_cons_self ⟨⟨1⟩, [], [], .trap .overflow orig0, []⟩
      (List.mem_cons_of_mem _ List.mem_cons_self) .overflow orig0 rfl
  have hEval : buildBIR ssa = .ok ⟨tabProject, mainFn, [(mainFn, ⟨[], some u8Ty⟩)],
      [(mainFn, [], ⟨⟨0⟩, #[
        ⟨⟨0⟩, [], [⟨orig0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[100]⟩) (.literal ⟨[100]⟩)⟩],
          .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩,
        ⟨⟨1⟩, [], [], .trap ⟨0⟩, []⟩,
        ⟨⟨2⟩, [], [⟨orig1, .addWithOverflow .u8 ⟨2⟩ ⟨3⟩ (.value ⟨0⟩) (.literal ⟨[100]⟩)⟩],
          .branch (.value ⟨3⟩) ⟨⟨3⟩, []⟩ ⟨⟨4⟩, []⟩, []⟩,
        ⟨⟨3⟩, [], [], .trap ⟨1⟩, []⟩,
        ⟨⟨4⟩, [], [], .ret (some ⟨u8Ty, .value ⟨2⟩⟩), []⟩,
        ⟨⟨5⟩, [], [], .trap ⟨2⟩, []⟩]⟩)],
      [], #[⟨.overflow, orig0⟩, ⟨.overflow, orig1⟩, ⟨.abort, orig2⟩], [], []⟩ := rfl
  rw [hEval] at hOk
  cases Except.ok.inj hOk
  cases List.mem_singleton.mp hfn'
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hblk'
  rcases hblk' with rfl | rfl | rfl | rfl | rfl | rfl
  · cases hBlkId
  · cases BIRTerm.trap.inj hEnd
    cases Option.some.inj hGet
    cases hOrig
  · cases hBlkId
  · cases hBlkId
  · cases hBlkId
  · cases hBlkId

-- Mutant 4: drops `bir.sites` to empty `#[]` leaving dangling trap slot `⟨0⟩`.
example :
    let p := lcbProfile 2
    let mutant := passesWithBuildBIR p fun ssa =>
      (buildBIR ssa).map fun bir => { bir with sites := #[] }
    ¬ (registry mutant).buildBIR.Validated := by
  intro p mutant hVal
  let ssa := checkedAddSSA 255 1 p
  have hWF := checkedAddSSA_wf 255 1 p (by decide)
  obtain ⟨bir, hOk⟩ := buildBIR_ok_of_wf hWF (by decide : ssa.frameSlots ≤ p.maxBIRSlots)
  let bad : BIR s0Low p := { bir with sites := #[] }
  have hRun : (registry mutant).buildBIR.run ssa = .ok bad := by
    dsimp only [registry, passesWithBuildBIR, mutant]
    rw [hOk]; rfl
  have hTraps := (hVal.1 ssa bad hWF hRun).2.1.2.2.1
  obtain ⟨_, _, _, _, _, _, slot, _, _, hGet, _, _⟩ :=
    hTraps (mainFn, [], _) List.mem_cons_self ⟨⟨1⟩, [], [], .trap .overflow orig0, []⟩
      (List.mem_cons_of_mem _ List.mem_cons_self) .overflow orig0 rfl
  cases hGet

-- Mutant 5: substitutes `bir.project` with `otherProject ≠ tabProject`.
example :
    let p := lcbProfile 0
    let mutant := passesWithBuildBIR p fun ssa =>
      (buildBIR ssa).map fun bir => { bir with project := otherProject }
    ¬ (registry mutant).buildBIR.Validated := by
  intro p mutant hVal
  let ssa := returnSSA 42 p
  have hWF := returnSSA_wf 42 p (by decide)
  obtain ⟨bir, hOk⟩ := buildBIR_ok_of_wf hWF (by decide)
  let bad : BIR s0Low p := { bir with project := otherProject }
  have hRun : (registry mutant).buildBIR.run ssa = .ok bad := by
    dsimp only [registry, passesWithBuildBIR, mutant]
    rw [hOk]; rfl
  have hProj := (hVal.1 ssa bad hWF hRun).2.1.1
  exact otherProject_ne_tabProject hProj

-- Mutant 6: drops `bir.mutableLocals` to `[]` on `mutableLocalSSA`.
example :
    let p := lcbProfile 0
    let mutant := passesWithBuildBIR p fun ssa =>
      (buildBIR ssa).map fun bir => { bir with mutableLocals := [] }
    ¬ (registry mutant).buildBIR.Validated := by
  intro p mutant hVal
  let ssa := mutableLocalSSA p
  have hWF := mutableLocalSSA_wf p (by decide)
  obtain ⟨bir, hOk⟩ := buildBIR_ok_of_wf hWF (by decide)
  let bad : BIR s0Low p := { bir with mutableLocals := [] }
  have hRun : (registry mutant).buildBIR.run ssa = .ok bad := by
    dsimp only [registry, passesWithBuildBIR, mutant]
    rw [hOk]; rfl
  have hMut := (hVal.1 ssa bad hWF hRun).2.2
  cases hMut

-- Mutant 7: spuriously refuses a valid zero-slot loop at `maxBIRSlots = 0`.
example :
    let p := lcbProfile 0
    let mutant := passesWithBuildBIR p fun ssa =>
      .error (.resource (frameSlotRefusal ssa.frameSlots p.maxBIRSlots))
    ¬ (registry mutant).buildBIR.Validated := by
  intro p mutant hVal
  let ssa := zeroSlotLoopSSA p
  have hWF := zeroSlotLoopSSA_wf p (by decide)
  have hRef := hVal.2 ssa (.resource (frameSlotRefusal ssa.frameSlots p.maxBIRSlots)) hWF rfl
  exact Nat.lt_irrefl 0 hRef.2.2.2

-- Mutant 8: reports wrong rule revision (rev 2 instead of rev 1) on an over-budget SSA.
example :
    let p := lcbProfile 1
    let badRefusal (measured limit : Nat) : ResourceRefusal := ⟨⟨``BIR.frameSlots, 2⟩, measured, limit⟩
    let mutant := passesWithBuildBIR p fun ssa =>
      if p.maxBIRSlots < ssa.frameSlots then
        .error (.resource (badRefusal ssa.frameSlots p.maxBIRSlots))
      else buildBIR ssa
    ¬ (registry mutant).buildBIR.Validated := by
  intro p badRefusal mutant hVal
  let ssa := checkedAddSSA 20 22 p
  have hWF := checkedAddSSA_wf 20 22 p (by decide)
  have hRef := hVal.2 ssa (.resource (badRefusal 2 1)) hWF rfl
  have hRev : (2 : Nat) = 1 := congrArg RuleId.rev hRef.1
  cases hRev

-- Mutant 9: ignores the frame-slot ceiling and lowers an over-budget 2-slot SSA at `maxBIRSlots = 1`.
example :
    let p := lcbProfile 1
    let uncheckedLower (ssa : SSA s0Low p) : Except StageFailure (BIR s0Low p) :=
      match lowerFunctions ssa.functions #[] with
      | .error e => .error e
      | .ok (fns, sites) =>
        .ok ⟨ssa.project, ssa.entry, ssa.signatures, fns, ssa.layouts, sites,
          ssa.mutableLocals, ssa.declarations⟩
    let mutant := passesWithBuildBIR p uncheckedLower
    ¬ (registry mutant).buildBIR.Validated := by
  intro p uncheckedLower mutant hVal
  let ssa := checkedAddSSA 20 22 p
  have hWF := checkedAddSSA_wf 20 22 p (by decide)
  have hRun : ∃ bir, (registry mutant).buildBIR.run ssa = .ok bir ∧ bir.frameSlots = 2 :=
    ⟨_, rfl, rfl⟩
  obtain ⟨bir, hOk, hSlots⟩ := hRun
  have hLe := (hVal.1 ssa bir hWF hOk).1.frameSlots_le
  rw [hSlots] at hLe
  exact (by decide : ¬ (2 ≤ 1)) hLe

-- Mutant 10: returns a non-resource `.invalidInput` failure on a well-formed SSA.
example :
    let p := lcbProfile 4
    let mutant := passesWithBuildBIR p fun _ => .error (.invalidInput dummyRule)
    ¬ (registry mutant).buildBIR.Validated := by
  intro p mutant hVal
  have hWF := returnSSA_wf 42 p (by decide)
  exact hVal.2 (returnSSA 42 p) (.invalidInput dummyRule) hWF rfl

end StageInterfaceControls.D5BIR
