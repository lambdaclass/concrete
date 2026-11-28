import Concrete.Memory.Validate
import Concrete.Memory.Controls

/-!
# D5 MIR-to-Memory lowering witnesses, ISS-0125/U02 controls, and wrong-compiler mutants

Standalone controls on the fixed Slice 0 instance (`slice0`, `sliceLanguage slice0`,
`sliceLoweringLanguage slice0`) exercising the production `Concrete.Memory.Validate.lowerMemory`
pass through the registered `(registry passes).lowerMemory` stage:
1. universal validation and `Stage.Sound` across LCB and LLVM profiles (and `sliceOne`);
2. reachable whole-program witnesses:
   - `return 42` (`bindReturnControl`: fast-path `lowerGraph`, observes exit 42 in MIR and
     Memory, `RetainsTypedRoots`, `ResidentHandles`, `RepresentsCheckpoint`, `TransferCheckpoint`,
     and `ZeroUnbacked` via `lowerMemory_reachable_represents_resident`),
   - checked addition `20 + 22` and residual overflow `20 + 250` (exit 42 and trap 134 with
     exact `trapDiagnostic` stderr),
   - escaped materialized `u8` local (`.materialized`, layout `.scalar 1 1`, non-escaping
     local `.virtual`),
   - zero-byte materialized empty linear nominal (`constructEmpty` + `destructure`, layout
     `.scalar 0 1`),
   - non-empty `mutableLocals` preserved across `lowerMemory`,
   - interprocedural `call`/`ret` connecting `lowerMemory_reachable_return_exhausted_iff`
     (U02 `ReturnAt.exhausted_iff` + suspended caller `ResidentHandles`),
   - ambiguous lifetime join loop (`badJoinMIR`): rejected by raw `lowerGraph`, lowered
     cleanly to `.ok mem` by `lowerMemory` via `specializeMIR` with `Memory.HandoffWF`,
     `LowersMemory`, `RetainsTypedRoots`, and diverging runs in both MIR and Memory,
   - generic `CoreLanguage` operation control exercising `ResidentHandles.of_operation`
     (`ISS-0125` active + suspended-caller residency across a generic spec operation);
3. ten rejected wrong-compiler mutants, each proving `¬ (registry mutant).lowerMemory.Validated`
   on the fixed Slice 0 instance.
-/

namespace StageInterfaceControls.D5Memory

open Concrete Concrete.Memory Concrete.Memory.Validate
open Concrete.Contracts (Steps Semantics)
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface

private abbrev s0Lang : CoreLanguage := sliceLanguage slice0
private abbrev s0Low : LoweringLanguage s0Lang.operands.Ty := sliceLoweringLanguage slice0
private abbrev u8Ty : s0Lang.operands.Ty := .scalar (.int .u8)
private abbrev boolTy : s0Lang.operands.Ty := .scalar (.bool)

private def lcbProfile : Profile := ⟨.lcb, .x86_64Linux, 0, 4096⟩
private def llvmProfile : Profile := ⟨.llvm, .aarch64Macos, 4, 4096⟩

private abbrev State := MemoryRegisters s0Lang lcbProfile
private abbrev M := Interfaces.Memory s0Low lcbProfile
private abbrev Src := Interfaces.MIR.Execution (language := s0Lang) (lang := s0Low) (profile := lcbProfile)
private abbrev Dst := Interfaces.Memory.Execution (language := s0Lang) (lang := s0Low) (profile := lcbProfile)
private abbrev MBlock := Block (Sigma (Place s0Lang.operands.Ty)) (LocatedOp (MIRInst s0Low))
  (Terminator (Sigma (Read s0Lang.operands.Ty s0Low.literal))
    (Read s0Lang.operands.Ty s0Low.literal s0Low.bool))

private abbrev base : MIR s0Low lcbProfile := bindReturnControl slice0
private abbrev mainFn : FnId := base.entry
private def helperFn : FnId := ⟨mainFn.module, ⟨"helper"⟩⟩
private def emptyDeclId : DeclId := ⟨mainFn.module, ⟨"Empty"⟩⟩
private def emptyDecls : Declarations := [(emptyDeclId, ⟨emptyDeclId.name, false, []⟩)]
private abbrev emptyTy : s0Lang.operands.Ty := .nominal emptyDeclId

private def orig : Origin := base.functions[0].2.2.blocks[0].body[0].origin
private def p0 : Place s0Lang.operands.Ty u8Ty := ⟨⟨0⟩, []⟩
private def p1 : Place s0Lang.operands.Ty u8Ty := ⟨⟨1⟩, []⟩
private def p2 : Place s0Lang.operands.Ty u8Ty := ⟨⟨2⟩, []⟩
private def pe : Place s0Lang.operands.Ty emptyTy := ⟨⟨0⟩, []⟩
private def val42 : s0Lang.operands.Val := .scalar (.int .u8 42)

private def startState : State :=
  ⟨⟨mainFn, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, .empty, [], [], {}, 0, false⟩

private def overflowTrapObs : Concrete.Contracts.Observation :=
  .trapped 134 .overflow
    ⟨⟨orig.operation.id⟩, ⟨mainSourcePath.render.text, orig.lexical.line, orig.lexical.col⟩⟩
    (trapDiagnostic base.project .overflow orig.operation orig.lexical)

private def otherProject : Project :=
  Project.singleton mainSourcePath (SourceText.ofBytes [97, 9, 99])

private theorem otherProject_ne_base : otherProject ≠ base.project := by
  intro h
  have hb := congrArg (fun p => (p.fileAt ⟨0⟩).map fun f => f.source.bytes[2]?) h
  have hDiff : Option.map (fun f => f.source.bytes[2]?) (otherProject.fileAt ⟨0⟩) ≠
      Option.map (fun f => f.source.bytes[2]?) (base.project.fileAt ⟨0⟩) := by
    with_unfolding_all decide
  exact hDiff hb

private def dummyRule : RuleId := ⟨`StageInterfaceControls.D5Memory.unused, 1⟩

/-- Build a Slice 0 `Passes` bundle whose `lowerMemory` field is `fn`. -/
private def passesWithLowerMemory (profile : Profile)
    (fn : MIR s0Low profile → Except StageFailure (Interfaces.Memory s0Low profile)) :
    Passes slice0 profile where
  lex _ := .error (.missingFile ⟨0⟩)
  elaborate _ := .error (.invalidTree ⟨0⟩)
  checkCore _ := .error (.fragment (.unsupportedSlice slice0))
  lowerCore _ := .error (.invalidInput dummyRule)
  lowerUnchecked _ := .error (.invalidInput dummyRule)
  lowerMemory := fn
  buildSSA _ := .error (.invalidInput dummyRule)
  buildBIR _ := .error (.invalidInput dummyRule)
  emit _ := .error (.invalidInput dummyRule)

private def prodPasses (profile : Profile) : Passes slice0 profile :=
  passesWithLowerMemory profile lowerMemory

private def prodPassesSliceOne (profile : Profile) : Passes sliceOne profile where
  lex _ := .error (.missingFile ⟨0⟩)
  elaborate _ := .error (.invalidTree ⟨0⟩)
  checkCore _ := .error (.fragment (.unsupportedSlice sliceOne))
  lowerCore _ := .error (.invalidInput dummyRule)
  lowerUnchecked _ := .error (.invalidInput dummyRule)
  lowerMemory := lowerMemory
  buildSSA _ := .error (.invalidInput dummyRule)
  buildBIR _ := .error (.invalidInput dummyRule)
  emit _ := .error (.invalidInput dummyRule)

/-! ### 1. Universal registered validation and soundness -/

example : (registry (prodPasses lcbProfile)).lowerMemory.Validated :=
  lowerMemory_validated (prodPasses lcbProfile) rfl

example : Stage.Sound (prodPasses lcbProfile) .lowerMemory :=
  lowerMemory_sound (prodPasses lcbProfile) rfl

example : Stage.Sound (prodPasses llvmProfile) .lowerMemory :=
  lowerMemory_sound (prodPasses llvmProfile) rfl

example : (registry (prodPassesSliceOne llvmProfile)).lowerMemory.Validated :=
  lowerMemory_validated (prodPassesSliceOne llvmProfile) rfl

example : Stage.Sound (prodPassesSliceOne llvmProfile) .lowerMemory :=
  lowerMemory_sound (prodPassesSliceOne llvmProfile) rfl

/-! ### Helpers for discharging `MIR.HandoffWF` on Slice 0 fixtures -/

private theorem u8_layout_ok (decls : Declarations) :
    ∃ d, s0Lang.layout decls lcbProfile.memoryTarget u8Ty = some d ∧
      d.WellFormed lcbProfile.memoryTarget = true :=
  ⟨.scalar 1 1, by with_unfolding_all rfl, by with_unfolding_all decide⟩

private theorem empty_layout_ok :
    ∃ d, s0Lang.layout emptyDecls lcbProfile.memoryTarget emptyTy = some d ∧
      d.WellFormed lcbProfile.memoryTarget = true :=
  ⟨.scalar 0 1, by with_unfolding_all rfl, by with_unfolding_all decide⟩

private theorem materializable_all_u8 {mir : MIR s0Low lcbProfile}
    (hOnly : ∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
      ∀ r ty, (r, ty) ∈ locals → ty = u8Ty) :
    Materializable (language := s0Lang) mir := by
  intro fn locals graph hMem r ty hLoc _
  rw [hOnly fn locals graph hMem r ty hLoc]
  exact u8_layout_ok mir.declarations

private theorem mir_handoffWF_of_u8 {mir : MIR s0Low lcbProfile}
    (hLog : (mir.logicalView (language := s0Lang)).HandoffWF)
    (hOnly : ∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
      ∀ r ty, (r, ty) ∈ locals → ty = u8Ty) :
    mir.HandoffWF (language := s0Lang) :=
  ⟨hLog, materializable_all_u8 hOnly⟩

private theorem base_wf : base.HandoffWF (language := s0Lang) := by
  letI := s0Lang.typeDecidableEq
  refine mir_handoffWF_of_u8 ((checkHandoff_iff (base.logicalView (language := s0Lang))).mp (by with_unfolding_all decide)) ?_
  intro fn locals graph hm r ty hloc
  cases List.mem_singleton.mp hm
  cases List.mem_singleton.mp hloc
  rfl

/-! ### 2. Reachable whole-program witnesses through `(registry (prodPasses p)).lowerMemory` -/

-- Witness 1: `bindReturnControl` (`return 42`), fast-path `lowerGraph`, exit 42,
-- `RetainsTypedRoots`, and `lowerMemory_reachable_represents_resident` on the terminal state.
example :
    let edge := (registry (prodPasses lcbProfile)).lowerMemory
    ∃ mem q,
      edge.inputInvariant base ∧
      edge.run base = .ok mem ∧
      edge.outputInvariant mem ∧
      edge.success base mem ∧
      RetainsTypedRoots base mem ∧
      Src.observes base (.returned 42 ByteArray.empty) ∧
      Dst.observes mem (.returned 42 ByteArray.empty) ∧
      Dst.finite mem q ∧
      ResidentHandles mem q ∧
      (∀ ceiling, lcbProfile.memoryTarget.addressCeiling ≤ ceiling →
        mem.TransferCheckpoint q ceiling) ∧
      ∃ p, Src.finite base p ∧ mem.RepresentsCheckpoint p q ∧ mem.ZeroUnbacked q := by
  intro edge
  obtain ⟨mem, hGraph, hSafe, _hLow0, hSrcObs, hDstObs⟩ := Concrete.Memory.BindReturn.refines_42
  have hRun : edge.run base = .ok mem := lowerMemory_eq_lowerGraph_of_ok hGraph
  have hVal := lowerMemory_validated (prodPasses lcbProfile) rfl
  obtain ⟨hOutWF, hLow, hMut⟩ := hVal.1 base mem base_wf hRun
  obtain ⟨q, hFinQ, hResQ⟩ := hDstObs
  obtain ⟨hRes, hTC, p, hFinP, hRep, hZero, _⟩ :=
    lowerMemory_reachable_represents_resident base_wf hRun hSafe q hFinQ
  exact ⟨mem, q, base_wf, hRun, hOutWF, ⟨hLow, hMut⟩, hLow.retainsTypedRoots,
    hSrcObs, ⟨q, hFinQ, hResQ⟩, hFinQ, hRes, hTC, p, hFinP, hRep, hZero⟩

-- Witness 2: checked addition `20 + 22` and residual overflow `20 + 250` (trap 134 + exact stderr).
private def checkedAddMIR : MIR s0Low lcbProfile :=
  let b0 : MBlock :=
    ⟨⟨0⟩, [],
      [⟨orig, .bind p0 (.literal ⟨[20]⟩)⟩,
       ⟨orig, .bind p1 (.literal ⟨[22]⟩)⟩,
       ⟨orig, .op (opcode := .arithmetic (.plain .add)) (types := [u8Ty, u8Ty]) p2 ()
         (.cons (.copy p0) (.cons (.copy p1) .nil)) [⟨0, .overflow, .residual⟩] []⟩],
      .ret (some ⟨u8Ty, .copy p2⟩), []⟩
  { base with functions := [(mainFn, [(⟨0⟩, u8Ty), (⟨1⟩, u8Ty), (⟨2⟩, u8Ty)], ⟨⟨0⟩, #[b0]⟩)] }

private def checkedAddTrapMIR : MIR s0Low lcbProfile :=
  let op : MIRInst s0Low :=
    .op (opcode := .arithmetic (.plain .add)) (types := [u8Ty, u8Ty]) p0 ()
      (.cons (.literal ⟨[20]⟩) (.cons (.literal ⟨[250]⟩) .nil)) [⟨0, .overflow, .residual⟩] []
  { base with
    functions := [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨orig, op⟩], .ret (some ⟨u8Ty, .copy p0⟩), []⟩]⟩)] }

example :
    let edge := (registry (prodPasses lcbProfile)).lowerMemory
    (∃ mem,
      edge.inputInvariant checkedAddMIR ∧
      edge.run checkedAddMIR = .ok mem ∧
      edge.outputInvariant mem ∧
      edge.success checkedAddMIR mem ∧
      RetainsTypedRoots checkedAddMIR mem) ∧
    (∃ mem,
      edge.inputInvariant checkedAddTrapMIR ∧
      edge.run checkedAddTrapMIR = .ok mem ∧
      edge.outputInvariant mem ∧
      edge.success checkedAddTrapMIR mem ∧
      Src.observes checkedAddTrapMIR overflowTrapObs ∧
      Dst.observes mem overflowTrapObs) := by
  intro edge
  letI := s0Lang.typeDecidableEq
  have hVal := lowerMemory_validated (prodPasses lcbProfile) rfl
  have hAddWF : checkedAddMIR.HandoffWF (language := s0Lang) := by
    refine mir_handoffWF_of_u8
      ((checkHandoff_iff (checkedAddMIR.logicalView (language := s0Lang))).mp (by with_unfolding_all decide)) ?_
    intro fn locals graph hm r ty hloc
    cases List.mem_singleton.mp hm
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hloc
    rcases hloc with h | h | h <;> cases h <;> rfl
  have hTrapWF : checkedAddTrapMIR.HandoffWF (language := s0Lang) := by
    refine mir_handoffWF_of_u8
      ((checkHandoff_iff (checkedAddTrapMIR.logicalView (language := s0Lang))).mp (by with_unfolding_all decide)) ?_
    intro fn locals graph hm r ty hloc
    cases List.mem_singleton.mp hm
    cases List.mem_singleton.mp hloc
    rfl
  obtain ⟨memAdd, hRunAdd⟩ := lowerMemory_ok_of_wf hAddWF
  obtain ⟨hOutAdd, hLowAdd, hMutAdd⟩ := hVal.1 checkedAddMIR memAdd hAddWF hRunAdd
  obtain ⟨memTrap, hGraphTrap, hInitTrap, hStepTrap, hResTrap⟩ := Concrete.Memory.BindReturn.declared_add_traps
  have hRunTrap : edge.run checkedAddTrapMIR = .ok memTrap := lowerMemory_eq_lowerGraph_of_ok hGraphTrap
  obtain ⟨hOutTrap, hLowTrap, hMutTrap⟩ := hVal.1 checkedAddTrapMIR memTrap hTrapWF hRunTrap
  exact ⟨⟨memAdd, hAddWF, hRunAdd, hOutAdd, ⟨hLowAdd, hMutAdd⟩, hLowAdd.retainsTypedRoots⟩,
    ⟨memTrap, hTrapWF, hRunTrap, hOutTrap, ⟨hLowTrap, hMutTrap⟩,
      ⟨startState, ⟨startState, by with_unfolding_all rfl, .nil _⟩, by with_unfolding_all rfl⟩,
      ⟨_, ⟨startState, hInitTrap, .cons hStepTrap (.nil _)⟩, hResTrap⟩⟩⟩

-- Witness 3: escaped materialized `u8` local (`.materialized` + `.scalar 1 1`) alongside
-- a virtual non-escaping `u8` local.
private def escapedU8MIR : MIR s0Low lcbProfile :=
  let b0 : MBlock :=
    ⟨⟨0⟩, [], [⟨orig, .bind p0 (.literal ⟨[42]⟩)⟩], .ret (some ⟨u8Ty, .copy p0⟩), []⟩
  let b1 : MBlock :=
    ⟨⟨1⟩, [], [⟨orig, .op (opcode := .read) (types := []) p1 () .nil [] [⟨u8Ty, p0⟩]⟩],
      .trap .abort orig, []⟩
  { base with functions := [(mainFn, [(⟨0⟩, u8Ty), (⟨1⟩, u8Ty)], ⟨⟨0⟩, #[b0, b1]⟩)] }

private theorem escapedU8MIR_wf : escapedU8MIR.HandoffWF (language := s0Lang) := by
  letI := s0Lang.typeDecidableEq
  refine mir_handoffWF_of_u8
    ((checkHandoff_iff (escapedU8MIR.logicalView (language := s0Lang))).mp (by with_unfolding_all decide)) ?_
  intro fn locals graph hm r ty hloc
  cases List.mem_singleton.mp hm
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hloc
  rcases hloc with h | h <;> cases h <;> rfl

example :
    let edge := (registry (prodPasses lcbProfile)).lowerMemory
    ∃ mem,
      edge.inputInvariant escapedU8MIR ∧
      edge.run escapedU8MIR = .ok mem ∧
      edge.outputInvariant mem ∧
      edge.success escapedU8MIR mem ∧
      RetainsTypedRoots escapedU8MIR mem ∧
      mem.storage = [(mainFn, [(⟨0⟩, .materialized), (⟨1⟩, .virtual)])] ∧
      mem.layouts = [(mainFn, [(⟨0⟩, .scalar 1 1)])] := by
  intro edge
  have hVal := lowerMemory_validated (prodPasses lcbProfile) rfl
  have hGraphOk : (lowerGraph (language := s0Lang) escapedU8MIR).isOk = true := by
    with_unfolding_all decide
  cases hG : lowerGraph (language := s0Lang) escapedU8MIR with
  | error e => rw [hG] at hGraphOk; cases hGraphOk
  | ok mem =>
    have hRun : edge.run escapedU8MIR = .ok mem := lowerMemory_eq_lowerGraph_of_ok hG
    obtain ⟨hOutWF, hLow, hMut⟩ := hVal.1 escapedU8MIR mem escapedU8MIR_wf hRun
    have hsl : ((lowerGraph (language := s0Lang) escapedU8MIR).toOption.map
        fun x => (x.storage, x.layouts)) =
        some ([(mainFn, [(⟨0⟩, .materialized), (⟨1⟩, .virtual)])],
          [(mainFn, [(⟨0⟩, .scalar 1 1)])]) := by
      with_unfolding_all rfl
    rw [hG] at hsl
    simp only [Except.toOption, Option.map_some, Option.some.injEq, Prod.mk.injEq] at hsl
    exact ⟨mem, escapedU8MIR_wf, hRun, hOutWF, ⟨hLow, hMut⟩, hLow.retainsTypedRoots, hsl.1, hsl.2⟩

-- Witness 4: zero-byte materialized empty linear nominal (`constructEmpty` + `destructure`).
private def emptyNominalMIR : MIR s0Low lcbProfile :=
  let b0 : MBlock :=
    ⟨⟨0⟩, [],
      [⟨orig, .op (opcode := .named ⟨"constructEmpty"⟩) (types := []) pe () .nil [] []⟩,
       ⟨orig, .destructure pe []⟩,
       ⟨orig, .endLifetime ⟨0⟩⟩],
      .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
  { base with
    declarations := emptyDecls
    functions := [(mainFn, [(⟨0⟩, emptyTy)], ⟨⟨0⟩, #[b0]⟩)] }

example :
    let edge := (registry (prodPasses lcbProfile)).lowerMemory
    ∃ mem,
      edge.inputInvariant emptyNominalMIR ∧
      edge.run emptyNominalMIR = .ok mem ∧
      edge.outputInvariant mem ∧
      edge.success emptyNominalMIR mem ∧
      RetainsTypedRoots emptyNominalMIR mem ∧
      mem.storage = [(mainFn, [(⟨0⟩, .materialized)])] ∧
      mem.layouts = [(mainFn, [(⟨0⟩, .scalar 0 1)])] := by
  intro edge
  letI := s0Lang.typeDecidableEq
  have hWF : emptyNominalMIR.HandoffWF (language := s0Lang) := by
    refine ⟨(checkHandoff_iff (emptyNominalMIR.logicalView (language := s0Lang))).mp (by with_unfolding_all decide), ?_⟩
    intro fn locals graph hm r ty hloc _
    cases List.mem_singleton.mp hm
    cases List.mem_singleton.mp hloc
    exact empty_layout_ok
  have hVal := lowerMemory_validated (prodPasses lcbProfile) rfl
  have hGraphOk : (lowerGraph (language := s0Lang) emptyNominalMIR).isOk = true := by
    with_unfolding_all decide
  cases hG : lowerGraph (language := s0Lang) emptyNominalMIR with
  | error e => rw [hG] at hGraphOk; cases hGraphOk
  | ok mem =>
    have hRun : edge.run emptyNominalMIR = .ok mem := lowerMemory_eq_lowerGraph_of_ok hG
    obtain ⟨hOutWF, hLow, hMut⟩ := hVal.1 emptyNominalMIR mem hWF hRun
    have hsl : ((lowerGraph (language := s0Lang) emptyNominalMIR).toOption.map
        fun x => (x.storage, x.layouts)) =
        some ([(mainFn, [(⟨0⟩, .materialized)])], [(mainFn, [(⟨0⟩, .scalar 0 1)])]) := by
      with_unfolding_all rfl
    rw [hG] at hsl
    simp only [Except.toOption, Option.map_some, Option.some.injEq, Prod.mk.injEq] at hsl
    exact ⟨mem, hWF, hRun, hOutWF, ⟨hLow, hMut⟩, hLow.retainsTypedRoots, hsl.1, hsl.2⟩

-- Witness 5: non-empty `mutableLocals` preserved across `lowerMemory`.
private def mutableLocalMIR : MIR s0Low lcbProfile :=
  { base with mutableLocals := [(mainFn, [⟨0⟩])] }

private theorem mutableLocalMIR_wf : mutableLocalMIR.HandoffWF (language := s0Lang) := by
  letI := s0Lang.typeDecidableEq
  refine mir_handoffWF_of_u8
    ((checkHandoff_iff (mutableLocalMIR.logicalView (language := s0Lang))).mp (by with_unfolding_all decide)) ?_
  intro fn locals graph hm r ty hloc
  cases List.mem_singleton.mp hm
  cases List.mem_singleton.mp hloc
  rfl

example :
    let edge := (registry (prodPasses lcbProfile)).lowerMemory
    ∃ mem,
      edge.inputInvariant mutableLocalMIR ∧
      edge.run mutableLocalMIR = .ok mem ∧
      edge.outputInvariant mem ∧
      edge.success mutableLocalMIR mem ∧
      mem.mutableLocals = [(mainFn, [⟨0⟩])] := by
  intro edge
  have hVal := lowerMemory_validated (prodPasses lcbProfile) rfl
  obtain ⟨mem, hRun⟩ := lowerMemory_ok_of_wf mutableLocalMIR_wf
  obtain ⟨hOutWF, hLow, hMut⟩ := hVal.1 mutableLocalMIR mem mutableLocalMIR_wf hRun
  exact ⟨mem, mutableLocalMIR_wf, hRun, hOutWF, ⟨hLow, hMut⟩, hMut⟩

-- Witness 6: interprocedural `call`/`ret` exercising `lowerMemory_reachable_return_exhausted_iff`
-- (U02 `ReturnAt.exhausted_iff` + suspended caller `ResidentHandles`).
private def callRetSimpleMIR : MIR s0Low lcbProfile :=
  let mainBlk : MBlock :=
    ⟨⟨0⟩, [],
      [⟨orig, .call helperFn (some ⟨u8Ty, p0⟩) [] []⟩],
      .ret (some ⟨u8Ty, .copy p0⟩), []⟩
  let helperBlk : MBlock :=
    ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
  { base with
    signatures := [(mainFn, ⟨[], some u8Ty⟩), (helperFn, ⟨[], some u8Ty⟩)]
    functions :=
      [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[mainBlk]⟩),
       (helperFn, [], ⟨⟨0⟩, #[helperBlk]⟩)] }

private def callRetSimpleMem : M :=
  let vp0 : MemoryPlace _ u8Ty := ⟨p0, .virtual⟩
  let mainBlk : MemoryBlock s0Low :=
    ⟨⟨0⟩, [],
      [⟨orig, .call helperFn (some ⟨u8Ty, vp0⟩) [] []⟩],
      .ret (some ⟨u8Ty, .copy vp0⟩), []⟩
  let helperBlk : MemoryBlock s0Low :=
    ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
  { project := base.project
    entry := mainFn
    signatures := [(mainFn, ⟨[], some u8Ty⟩), (helperFn, ⟨[], some u8Ty⟩)]
    functions :=
      [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[mainBlk]⟩),
       (helperFn, [], ⟨⟨0⟩, #[helperBlk]⟩)]
    storage := [(mainFn, [(⟨0⟩, .virtual)]), (helperFn, [])]
    layouts := [(mainFn, []), (helperFn, [])] }

private def callRetCaller : MemoryContinuation s0Lang.operands.Ty s0Lang.operands.Val :=
  ⟨startState.advance.toMemoryFrame, some ⟨u8Ty, ⟨p0, .virtual⟩⟩⟩

private def callRetInHelper : State :=
  ⟨⟨helperFn, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, .empty, [callRetCaller], [], {}, 0, false⟩

private def callRetReturned : State :=
  { startState with
    instruction := 1
    live := [⟨0⟩]
    initialized := [⟨0⟩]
    virtualValues := [(⟨0⟩, val42)] }

private theorem callRetSimple_safe : ProgramSafe Src callRetSimpleMIR := by
  have hStep01 : Src.step callRetSimpleMIR startState callRetInHelper := by
    with_unfolding_all
      exact ⟨rfl, ⟨rfl, rfl, [], startState, _, _, rfl, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  have hStep12 : Src.step callRetSimpleMIR callRetInHelper callRetReturned := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, rfl, callRetCaller, [], [val42], callRetInHelper, callRetInHelper,
        rfl, rfl, .nil _, rfl,
        .cons ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩ ⟨⟨rfl, nofun⟩, nofun, rfl, ⟨rfl, nofun⟩⟩ (.nil _)⟩, _, rfl⟩
  have hRes2 : Src.result callRetSimpleMIR callRetReturned (.returned 42 ByteArray.empty) := by
    with_unfolding_all rfl
  have hCases : ∀ p, Src.finite callRetSimpleMIR p →
      p = startState ∨ p = callRetInHelper ∨ p = callRetReturned := by
    intro p hp
    apply finite_invariant Src callRetSimpleMIR _
      (fun _ h => .inl (by with_unfolding_all exact h)) _ hp
    rintro _ next (rfl | rfl | rfl) tick
    · have hCall : callRetSimpleMIR.logicalView.Call helperFn (some ⟨u8Ty, ⟨p0, .virtual⟩⟩) []
          startState next false := by
        with_unfolding_all exact tick.2.1.2.2
      obtain ⟨vals, ar, g, eb, hrd, hg, hfind, _, hpo⟩ := hCall
      have e1 : some ([], startState) = some (vals, ar) := by with_unfolding_all exact hrd
      cases e1
      have e2 : some ((callRetSimpleMIR.logicalView.functions[1]'(by with_unfolding_all decide)).2.2) = some g := by
        with_unfolding_all exact hg
      cases Option.some.inj e2
      have e3 : some ((callRetSimpleMIR.logicalView.functions[1]'(by with_unfolding_all decide)).2.2.blocks[0]'(by with_unfolding_all decide)) = some eb := by
        with_unfolding_all exact hfind
      cases Option.some.inj e3
      have hp0 : ((callRetSimpleMIR.logicalView.functions[1]'(by with_unfolding_all decide)).2.2.blocks[0]'(by with_unfolding_all decide)).params = [] := by
        with_unfolding_all rfl
      rw [hp0] at hpo
      cases hpo
      exact .inr (.inl rfl)
    · obtain ⟨blk, hblk, _, hcs⟩ := tick.2.1
      have eb : some ((callRetSimpleMIR.logicalView.functions[1]'(by with_unfolding_all decide)).2.2.blocks[0]'(by with_unfolding_all decide)) = some blk := by
        with_unfolding_all exact hblk
      cases Option.some.inj eb
      have hendEq : ((callRetSimpleMIR.logicalView.functions[1]'(by with_unfolding_all decide)).2.2.blocks[0]'(by with_unfolding_all decide)).ending =
          .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩) := by
        with_unfolding_all rfl
      rw [hendEq] at hcs
      obtain ⟨c, rest, vals, ar, en, hcs', hrd, hend, _, hpo⟩ := hcs
      cases hcs'
      have e1 : some ([val42], callRetInHelper) = some (vals, ar) := by with_unfolding_all exact hrd
      cases e1
      cases hend
      cases hpo with
      | cons hbeg hwr hnil =>
        cases hnil
        rcases hbeg.2 with ⟨_, rfl⟩ | ⟨hmat, _⟩
        · have hst : some callRetReturned = some next := by with_unfolding_all exact hwr.2.2.1
          cases hst
          exact .inr (.inr rfl)
        · have bad : some StorageClass.virtual = some StorageClass.materialized := by with_unfolding_all exact hmat
          cases bad
    · exact (Src.terminal callRetSimpleMIR _ _ hRes2 next tick).elim
  intro p stuck
  rcases hCases p stuck.1 with rfl | rfl | rfl
  · exact stuck.2.1 _ hStep01
  · exact stuck.2.1 _ hStep12
  · exact stuck.2.2 _ hRes2

example :
    let edge := (registry (prodPasses lcbProfile)).lowerMemory
    let resume := Interfaces.Memory.ReturnAt.resume callRetCaller [] callRetInHelper
    edge.inputInvariant callRetSimpleMIR ∧
    edge.run callRetSimpleMIR = .ok callRetSimpleMem ∧
    edge.outputInvariant callRetSimpleMem ∧
    edge.success callRetSimpleMIR callRetSimpleMem ∧
    ResidentHandles callRetSimpleMem resume ∧
    ((Dst.step callRetSimpleMem callRetInHelper { callRetInHelper with resourceExhausted := true } ∧
      Dst.result callRetSimpleMem { callRetInHelper with resourceExhausted := true } .resourceExhausted) ↔
      (∃ ceiling, callRetSimpleMem.ParamsValid callRetCaller.destination.toList [val42] resume ceiling) ∧
        ¬ callRetSimpleMem.ParamSpaceFits callRetCaller.destination.toList resume) := by
  intro edge resume
  letI := s0Lang.typeDecidableEq
  have hWF : callRetSimpleMIR.HandoffWF (language := s0Lang) := by
    refine mir_handoffWF_of_u8
      ((checkHandoff_iff (callRetSimpleMIR.logicalView (language := s0Lang))).mp (by with_unfolding_all decide)) ?_
    intro fn locals graph hm r ty hloc
    simp only [callRetSimpleMIR, List.mem_cons, List.not_mem_nil, or_false] at hm
    rcases hm with h | h
    · cases h; cases List.mem_singleton.mp hloc; rfl
    · cases h; cases hloc
  have hGraph : lowerGraph (language := s0Lang) callRetSimpleMIR = .ok callRetSimpleMem := by
    with_unfolding_all rfl
  have hRun : edge.run callRetSimpleMIR = .ok callRetSimpleMem := lowerMemory_eq_lowerGraph_of_ok hGraph
  have hVal := lowerMemory_validated (prodPasses lcbProfile) rfl
  obtain ⟨hOutWF, hLow, hMut⟩ := hVal.1 callRetSimpleMIR callRetSimpleMem hWF hRun
  have hDstStep01 : Dst.step callRetSimpleMem startState callRetInHelper := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, rfl, [], startState, _, _, rfl, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  have hFinInHelper : Dst.finite callRetSimpleMem callRetInHelper :=
    ⟨startState, by with_unfolding_all rfl, .cons hDstStep01 (.nil _)⟩
  have hRetAt : callRetSimpleMem.ReturnAt (language := s0Lang) callRetInHelper callRetInHelper callRetInHelper
      (some ⟨u8Ty, .literal ⟨[42]⟩⟩) callRetCaller [] [val42] := by
    refine ⟨rfl, ⟨_, by with_unfolding_all rfl, rfl, rfl⟩, rfl, by with_unfolding_all rfl, .nil _, rfl⟩
  obtain ⟨hResResume, hExhIff⟩ :=
    lowerMemory_reachable_return_exhausted_iff hWF hRun callRetSimple_safe hFinInHelper hRetAt
  exact ⟨hWF, hRun, hOutWF, ⟨hLow, hMut⟩, hResResume, hExhIff⟩

-- Witness 7: ambiguous lifetime join loop (`badJoinMIR`) — raw `lowerGraph` fails with `.lifetime`,
-- while production `lowerMemory` succeeds via `specializeMIR` with full `Memory.HandoffWF`,
-- `LowersMemory`, `RetainsTypedRoots`, and diverging executions on both MIR and Memory.
private def badJoinMIR : MIR s0Low lcbProfile :=
  let b0 : MBlock := ⟨⟨0⟩, [], [⟨orig, .bind p0 (.literal ⟨[42]⟩)⟩], .jump ⟨⟨0⟩, []⟩, []⟩
  { base with
    functions := [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[b0]⟩)]
    mutableLocals := [(mainFn, [⟨0⟩])] }

private def badJoinBlock : MemoryBlock s0Low :=
  ⟨⟨0⟩, [], [⟨orig, .bind ⟨p0, .virtual⟩ (.literal ⟨[42]⟩)⟩], .jump ⟨⟨0⟩, []⟩, []⟩

private def badJoinGraph : MemoryGraph s0Low := ⟨⟨0⟩, #[badJoinBlock]⟩

private def badJoinPrepared : PreparedGraphs s0Low lcbProfile :=
  { functions := [(mainFn, [(⟨0⟩, u8Ty)], badJoinGraph)]
    storage := [(mainFn, [(⟨0⟩, .virtual)])]
    layouts := [(mainFn, [])] }

private theorem badJoinMIR_wf : badJoinMIR.HandoffWF (language := s0Lang) := by
  letI := s0Lang.typeDecidableEq
  refine mir_handoffWF_of_u8
    ((checkHandoff_iff (badJoinMIR.logicalView (language := s0Lang))).mp (by with_unfolding_all decide)) ?_
  intro fn locals graph hm r ty hloc
  cases List.mem_singleton.mp hm
  cases List.mem_singleton.mp hloc
  rfl

private theorem badJoinMIR_raw_lowerGraph_err :
    ∃ j, lowerGraph (language := s0Lang) badJoinMIR = .error (.lifetime j) := by
  have hprepExact : prepareGraph (language := s0Lang) badJoinMIR = .ok badJoinPrepared := by
    with_unfolding_all rfl
  cases h : lowerGraph (language := s0Lang) badJoinMIR with
  | error e =>
    cases e with
    | lifetime j => exact ⟨j, rfl⟩
    | layout _ =>
      unfold lowerGraph at h
      rw [hprepExact] at h
      simp only [Except.mapError, bind, Except.bind] at h
      split at h
      · rename_i hinner
        split at hinner
        · cases hinner; cases h
        · cases hinner
      · cases h
  | ok m =>
    obtain ⟨graphs, hprep, hplaced⟩ := (lowerGraph_handoff h badJoinMIR_wf.1).placement
    cases Except.ok.inj (hprep.symm.trans hprepExact)
    obtain ⟨placed, _, hpg⟩ := hplaced mainFn [(⟨0⟩, u8Ty)] badJoinGraph (.head _)
    obtain ⟨pb0, _, hblk⟩ := hpg.blocks.forward badJoinBlock (.head _)
    have hbody := hblk.body
    have hdef : definitionRoot [(⟨0⟩, u8Ty)] (MemoryInst.bind (lang := s0Low) ⟨p0, .virtual⟩ (.literal ⟨[42]⟩)) = some ⟨0⟩ := by
      with_unfolding_all rfl
    have hfp : EntryPath badJoinGraph ⟨0⟩ (⟨0⟩, false) := .initial (by with_unfolding_all decide)
    have htp : EntryPath badJoinGraph ⟨0⟩ (⟨0⟩, true) :=
      .edge hfp (by with_unfolding_all decide) (by with_unfolding_all decide)
    generalize pb0.body = dstBody at hbody
    cases hbody with
    | keep hkeep _ =>
      have hch := hkeep ⟨0⟩ hdef
      have h1 := entryLifetime_on_path badJoinGraph badJoinBlock ⟨0⟩ hfp hch
      cases h1
    | start hdef' hch _ =>
      cases Option.some.inj (hdef'.symm.trans hdef)
      have h2 := entryLifetime_on_path badJoinGraph badJoinBlock ⟨0⟩ htp hch
      cases h2

private def badJoinSpecMIR : MIR s0Low lcbProfile :=
  let b0 : MBlock := ⟨⟨0⟩, [], [⟨orig, .bind p0 (.literal ⟨[42]⟩)⟩], .jump ⟨⟨1⟩, []⟩, []⟩
  let b1 : MBlock := ⟨⟨1⟩, [], [⟨orig, .bind p0 (.literal ⟨[42]⟩)⟩], .jump ⟨⟨1⟩, []⟩, []⟩
  { badJoinMIR with functions := [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[b0, b1]⟩)] }

private def badJoinSpecMem : M :=
  let vp0 : MemoryPlace _ u8Ty := ⟨p0, .virtual⟩
  let blk0 : MemoryBlock s0Low :=
    ⟨⟨0⟩, [], [⟨orig, .beginLifetime ⟨0⟩⟩, ⟨orig, .bind vp0 (.literal ⟨[42]⟩)⟩], .jump ⟨⟨1⟩, []⟩, []⟩
  let blk1 : MemoryBlock s0Low :=
    ⟨⟨1⟩, [], [⟨orig, .bind vp0 (.literal ⟨[42]⟩)⟩], .jump ⟨⟨1⟩, []⟩, []⟩
  { project := base.project, entry := mainFn, signatures := base.signatures
    functions := [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[blk0, blk1]⟩)]
    storage := [(mainFn, [(⟨0⟩, .virtual)])]
    layouts := [(mainFn, [])]
    mutableLocals := [(mainFn, [⟨0⟩])] }

private theorem specializeMIR_badJoin_eq :
    specializeMIR badJoinMIR = badJoinSpecMIR := by
  have hAmb : hasAmbiguousJoin badJoinGraph ⟨0⟩ = true := by
    cases h : hasAmbiguousJoin badJoinGraph ⟨0⟩ with
    | true => rfl
    | false =>
      have hUnamb := rootUnambiguousAt_of_hasAmbiguousJoin_false h (block := badJoinBlock) List.mem_cons_self
      have hfp : EntryPath badJoinGraph ⟨0⟩ (⟨0⟩, false) := .initial (by with_unfolding_all decide)
      have htp : EntryPath badJoinGraph ⟨0⟩ (⟨0⟩, true) :=
        .edge hfp (by with_unfolding_all decide) (by with_unfolding_all decide)
      rcases hUnamb with hP | hNoPred | hNotBoth
      · cases hP
      · exact False.elim (by
          have hHas : badJoinGraph.blocks.any (fun b => b.ending.successors.any (·.target == badJoinBlock.id)) = true := by
            with_unfolding_all decide
          rw [hHas] at hNoPred; cases hNoPred)
      · exact False.elim (hNotBoth ⟨htp, hfp⟩)
  have hRoots : ambiguousRoots [(⟨0⟩, u8Ty)] (badJoinMIR.functions[0].2.2) = [⟨0⟩] := by
    unfold ambiguousRoots
    have hToLog : toLogicalGraph (badJoinMIR.functions[0].2.2) = badJoinGraph := by with_unfolding_all rfl
    simp only [List.map_cons, List.map_nil, List.filter_cons, List.filter_nil, hToLog, hAmb]
    with_unfolding_all rfl
  have hSplit : splitGraphForRoots [⟨0⟩] (badJoinMIR.functions[0].2.2) =
      badJoinSpecMIR.functions[0].2.2 := by
    with_unfolding_all rfl
  show { badJoinMIR with
    functions := [(mainFn, [(⟨0⟩, u8Ty)],
      splitGraphForRoots (ambiguousRoots [(⟨0⟩, u8Ty)] (badJoinMIR.functions[0].2.2))
        (badJoinMIR.functions[0].2.2))] } = badJoinSpecMIR
  rw [hRoots, hSplit]
  rfl

example :
    let edge := (registry (prodPasses lcbProfile)).lowerMemory
    (∃ j, lowerGraph (language := s0Lang) badJoinMIR = .error (.lifetime j)) ∧
    ∃ mem,
      edge.inputInvariant badJoinMIR ∧
      edge.run badJoinMIR = .ok mem ∧
      edge.outputInvariant mem ∧
      edge.success badJoinMIR mem ∧
      RetainsTypedRoots badJoinMIR mem ∧
      (∃ run, Src.diverges badJoinMIR run) ∧
      (∃ run, Dst.diverges mem run) := by
  intro edge
  have hVal := lowerMemory_validated (prodPasses lcbProfile) rfl
  obtain ⟨mem, hRun⟩ := lowerMemory_ok_of_wf badJoinMIR_wf
  obtain ⟨hOutWF, hLow, hMut⟩ := hVal.1 badJoinMIR mem badJoinMIR_wf hRun
  -- Construct an explicit diverging run of `badJoinMIR` in `Src`.
  let boundSt : State :=
    { startState with
      instruction := 1
      live := [⟨0⟩]
      initialized := [⟨0⟩]
      virtualValues := [(⟨0⟩, val42)] }
  let loopHead : State := { boundSt with instruction := 0 }
  have hInitS : Src.initial badJoinMIR startState := by with_unfolding_all rfl
  have hBind0 : Src.step badJoinMIR startState boundSt := by
    with_unfolding_all
      exact ⟨rfl, ⟨rfl, val42, startState, { boundSt with instruction := 0 }, rfl,
        .cons ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩ ⟨⟨rfl, nofun⟩, nofun, rfl, ⟨rfl, nofun⟩⟩ (.nil _), rfl⟩, [], rfl⟩
  have hJump : Src.step badJoinMIR boundSt loopHead := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, rfl, [], boundSt, _, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  have hRebind : Src.step badJoinMIR loopHead boundSt := by
    with_unfolding_all
      exact ⟨rfl, ⟨rfl, val42, loopHead, loopHead, rfl,
        .cons rfl ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, ⟨rfl, nofun⟩⟩ (.nil _), rfl⟩, [], rfl⟩
  let srcRun : Nat → State
    | 0 => startState
    | n + 1 => if n % 2 = 0 then boundSt else loopHead
  have hDivSrc : Src.diverges badJoinMIR srcRun := by
    refine ⟨hInitS, ?_⟩
    intro i
    cases i with
    | zero => exact hBind0
    | succ k =>
      change Src.step badJoinMIR (if k % 2 = 0 then boundSt else loopHead)
        (if (k + 1) % 2 = 0 then boundSt else loopHead)
      by_cases hmod : k % 2 = 0
      · have hnext : (k + 1) % 2 ≠ 0 := by omega
        simp only [hmod, ↓reduceIte, hnext]
        exact hJump
      · have hnext : (k + 1) % 2 = 0 := by omega
        simp only [hmod, ↓reduceIte, hnext]
        exact hRebind
  -- Identify `mem = badJoinSpecMem` directly from `lowerGraph_handoff`'s `PlacedGraph`.
  have hSpecOk : lowerGraph (language := s0Lang) badJoinSpecMIR = .ok mem := by
    have hRun' := hRun
    change lowerMemory badJoinMIR = .ok mem at hRun'
    unfold lowerMemory at hRun'
    obtain ⟨j, hRaw⟩ := badJoinMIR_raw_lowerGraph_err
    rw [hRaw, specializeMIR_badJoin_eq] at hRun'
    cases hSp : lowerGraph (language := s0Lang) badJoinSpecMIR with
    | error _ => rw [hSp] at hRun'; cases hRun'
    | ok m' => rw [hSp] at hRun'; cases Except.ok.inj hRun'; rfl
  letI := s0Lang.typeDecidableEq
  have hSpecWF : (badJoinSpecMIR.logicalView (language := s0Lang)).HandoffWF :=
    (checkHandoff_iff (badJoinSpecMIR.logicalView (language := s0Lang))).mp (by with_unfolding_all decide)
  have hSpecHand := lowerGraph_handoff hSpecOk hSpecWF
  let specBlk0 : MemoryBlock s0Low :=
    ⟨⟨0⟩, [], [⟨orig, .bind ⟨p0, .virtual⟩ (.literal ⟨[42]⟩)⟩], .jump ⟨⟨1⟩, []⟩, []⟩
  let specBlk1 : MemoryBlock s0Low :=
    ⟨⟨1⟩, [], [⟨orig, .bind ⟨p0, .virtual⟩ (.literal ⟨[42]⟩)⟩], .jump ⟨⟨1⟩, []⟩, []⟩
  let specGraph : MemoryGraph s0Low := ⟨⟨0⟩, #[specBlk0, specBlk1]⟩
  let specPrepared : PreparedGraphs s0Low lcbProfile :=
    { functions := [(mainFn, [(⟨0⟩, u8Ty)], specGraph)]
      storage := [(mainFn, [(⟨0⟩, .virtual)])]
      layouts := [(mainFn, [])] }
  have hPrepExact : prepareGraph (language := s0Lang) badJoinSpecMIR = .ok specPrepared := by
    with_unfolding_all rfl
  obtain ⟨graphs, hPrep, hPlaced⟩ := hSpecHand.placement
  cases Except.ok.inj (hPrep.symm.trans hPrepExact)
  obtain ⟨placedG, hMemFn, hPG⟩ := hPlaced mainFn [(⟨0⟩, u8Ty)] specGraph (.head _)
  have hDef0 : definitionRoot [(⟨0⟩, u8Ty)] (MemoryInst.bind (lang := s0Low) ⟨p0, .virtual⟩ (.literal ⟨[42]⟩)) = some ⟨0⟩ := by
    with_unfolding_all rfl
  -- Block 0 (`id = 0`) starts with `p0` dead (`EntryPath (0, false)`), so `pb0` inserts `beginLifetime 0`.
  obtain ⟨pb0, hpb0Mem, hpb0Blk⟩ := hPG.blocks.forward specBlk0 (.head _)
  have hfp0 : EntryPath specGraph ⟨0⟩ (⟨0⟩, false) := .initial (by with_unfolding_all decide)
  have hpb0Body : pb0.body = [⟨orig, .beginLifetime ⟨0⟩⟩, ⟨orig, .bind ⟨p0, .virtual⟩ (.literal ⟨[42]⟩)⟩] := by
    have hb := hpb0Blk.body
    generalize pb0.body = db at hb
    cases hb with
    | keep hkeep hrest =>
      have h1 := entryLifetime_on_path specGraph specBlk0 ⟨0⟩ hfp0 (hkeep ⟨0⟩ hDef0)
      cases h1
    | start hdef' _ hrest =>
      cases Option.some.inj (hdef'.symm.trans hDef0)
      cases hrest
      rfl
  have hpb0Eq : pb0 = ⟨⟨0⟩, [], [⟨orig, .beginLifetime ⟨0⟩⟩, ⟨orig, .bind ⟨p0, .virtual⟩ (.literal ⟨[42]⟩)⟩], .jump ⟨⟨1⟩, []⟩, []⟩ := by
    cases pb0
    cases hpb0Blk.id; cases hpb0Blk.params; cases hpb0Blk.ending; cases hpb0Blk.boundary
    cases hpb0Body
    rfl
  -- Block 1 (`id = 1`) enters with `p0` live (`EntryPath (1, true)`), so `pb1` keeps `[bind p0 42]`.
  obtain ⟨pb1, hpb1Mem, hpb1Blk⟩ := hPG.blocks.forward specBlk1 (.tail _ (.head _))
  have htp1 : EntryPath specGraph ⟨0⟩ (⟨1⟩, true) :=
    .edge hfp0 (by with_unfolding_all decide) (by with_unfolding_all decide)
  have hpb1Body : pb1.body = [⟨orig, .bind ⟨p0, .virtual⟩ (.literal ⟨[42]⟩)⟩] := by
    have hb := hpb1Blk.body
    generalize pb1.body = db at hb
    cases hb with
    | start hdef' hch _ =>
      cases Option.some.inj (hdef'.symm.trans hDef0)
      have h2 := entryLifetime_on_path specGraph specBlk1 ⟨0⟩ htp1 hch
      cases h2
    | keep _ hrest =>
      cases hrest
      rfl
  have hpb1Eq : pb1 = ⟨⟨1⟩, [], [⟨orig, .bind ⟨p0, .virtual⟩ (.literal ⟨[42]⟩)⟩], .jump ⟨⟨1⟩, []⟩, []⟩ := by
    cases pb1
    cases hpb1Blk.id; cases hpb1Blk.params; cases hpb1Blk.ending; cases hpb1Blk.boundary
    cases hpb1Body
    rfl
  have hPlacedEq : placedG = ⟨⟨0⟩, #[pb0, pb1]⟩ := by
    have hEntry : placedG.entry = ⟨0⟩ := hPG.entry.symm
    have hKeys : specGraph.blocks.toList.map Block.id = placedG.blocks.toList.map Block.id :=
      hPG.blocks.keys Block.id Block.id (fun _ _ h => h.id)
    have hLen : placedG.blocks.toList.length = 2 := by
      have := congrArg List.length hKeys
      simpa [specGraph] using this.symm
    have hListEq : placedG.blocks.toList = [pb0, pb1] := by
      cases hBlks : placedG.blocks.toList with
      | nil => rw [hBlks] at hLen; cases hLen
      | cons a rest =>
        cases rest with
        | nil => rw [hBlks] at hLen; cases hLen
        | cons b rest' =>
          cases rest' with
          | cons _ _ => rw [hBlks] at hLen; cases hLen
          | nil =>
            rw [hBlks] at hKeys hpb0Mem hpb1Mem
            have hKeys' : [⟨0⟩, ⟨1⟩] = [a.id, b.id] := by
              simpa [specGraph, specBlk0, specBlk1] using hKeys
            injection hKeys' with haId hTailKeys
            injection hTailKeys with hbId _
            have hA : a = pb0 := by
              rcases List.mem_cons.mp hpb0Mem with rfl | hb0
              · rfl
              · cases List.mem_singleton.mp hb0
                rw [hpb0Eq] at hbId; cases hbId
            have hB : b = pb1 := by
              rcases List.mem_cons.mp hpb1Mem with rfl | hb1
              · rw [hpb1Eq] at haId; cases haId
              · exact (List.mem_singleton.mp hb1).symm
            rw [hA, hB]
    rcases placedG with ⟨ent, ⟨blks⟩⟩
    dsimp only at hEntry hListEq
    subst hEntry
    subst hListEq
    rfl
  have hMemShape : mem = { badJoinSpecMem with functions := [(mainFn, [(⟨0⟩, u8Ty)], placedG)] } := by
    have hUnf := hSpecOk
    unfold lowerGraph at hUnf
    rw [hPrepExact] at hUnf
    simp only [Except.mapError, bind, Except.bind, pure, Except.pure, specPrepared,
      List.mapM_cons, List.mapM_nil] at hUnf
    split at hUnf
    · cases hUnf
    · rename_i fns hFns
      cases Except.ok.inj hUnf
      have hFnOnly : fns = [(mainFn, [(⟨0⟩, u8Ty)], placedG)] := by
        split at hFns
        · cases hFns
        · rename_i res hRes
          cases Except.ok.inj hFns
          split at hRes
          · cases hRes
          · rename_i pg hPg
            cases Except.ok.inj hRes
            cases List.mem_singleton.mp hMemFn
            rfl
      rw [hFnOnly]
      rfl
  -- Build the diverging target run directly on `mem = badJoinSpecMem`.
  let mInit : State := startState
  let mOpened : State := { mInit with instruction := 1, live := [⟨0⟩] }
  let mBound0 : State :=
    { mInit with instruction := 2, live := [⟨0⟩], initialized := [⟨0⟩], virtualValues := [(⟨0⟩, val42)] }
  let mHead1 : State := { mBound0 with block := ⟨1⟩, instruction := 0 }
  let mBound1 : State := { mHead1 with instruction := 1 }
  let dstRun : Nat → State
    | 0 => mInit
    | 1 => mOpened
    | 2 => mBound0
    | n + 3 => if n % 2 = 0 then mHead1 else mBound1
  have hDivDst : Dst.diverges mem dstRun := by
    rw [hMemShape, hPlacedEq, hpb0Eq, hpb1Eq]
    refine ⟨by with_unfolding_all rfl, fun i => ?_⟩
    match i with
    | 0 =>
      with_unfolding_all
        exact ⟨rfl, ⟨_, rfl, { mOpened with instruction := 0 }, ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩, [], rfl⟩
    | 1 =>
      with_unfolding_all
        exact ⟨rfl, ⟨_, rfl, val42, mOpened, { mBound0 with instruction := 1 }, rfl,
          ⟨⟨rfl, nofun⟩, nofun, rfl, ⟨rfl, nofun⟩⟩, rfl⟩, [], rfl⟩
    | 2 =>
      with_unfolding_all
        exact ⟨rfl, ⟨_, rfl, rfl, [], mBound0, _, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
    | k + 3 =>
      change Dst.step _ (if k % 2 = 0 then mHead1 else mBound1)
        (if (k + 1) % 2 = 0 then mHead1 else mBound1)
      by_cases hmod : k % 2 = 0
      · have hnext : (k + 1) % 2 ≠ 0 := by omega
        simp only [hmod, ↓reduceIte, hnext]
        with_unfolding_all
          exact ⟨rfl, ⟨_, rfl, val42, mHead1, mHead1, rfl,
            ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, ⟨rfl, nofun⟩⟩, rfl⟩, [], rfl⟩
      · have hnext : (k + 1) % 2 = 0 := by omega
        simp only [hmod, ↓reduceIte, hnext]
        with_unfolding_all
          exact ⟨rfl, ⟨_, rfl, rfl, [], mBound1, _, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  exact ⟨badJoinMIR_raw_lowerGraph_err, mem, badJoinMIR_wf, hRun, hOutWF, ⟨hLow, hMut⟩,
    hLow.retainsTypedRoots, ⟨srcRun, hDivSrc⟩, ⟨dstRun, hDivDst⟩⟩

-- Witness 8: generic `CoreLanguage` operation control for `ISS-0125` `ResidentHandles.of_operation`:
-- for ANY `CoreLanguage`, any operation step preserves both active and suspended-caller handle residency.
example {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {memory : Interfaces.Memory lang profile} {q : MemoryRegisters language profile}
    {logical : Bool} {env op inputs output values addresses result effects changed}
    (hRes : ResidentHandles memory q)
    (hOp : language.operation env op inputs output values profile.memoryTarget addresses
      (Interfaces.Memory.operationStore q logical) result effects changed) :
    ResidentHandles memory (Interfaces.Memory.applyOperation q logical changed) ∧
    (∀ ceiling, profile.memoryTarget.addressCeiling ≤ ceiling →
      memory.TransferCheckpoint (Interfaces.Memory.applyOperation q logical changed) ceiling) :=
  let hNext := ResidentHandles.of_operation hRes hOp
  ⟨hNext, fun _ hge => hNext.transferCheckpoint hge⟩

/-! ### 3. Ten D5 rejected wrong-compiler mutants on fixed `slice0` -/

-- Mutant 1: alters the lowered return literal `42` to `43`.
private def wrongReturnMemory : M :=
  let vp0 : MemoryPlace _ u8Ty := ⟨p0, .virtual⟩
  let blk : MemoryBlock s0Low :=
    ⟨⟨0⟩, [], [⟨orig, .beginLifetime ⟨0⟩⟩, ⟨orig, .bind vp0 (.literal ⟨[43]⟩)⟩],
      .ret (some ⟨u8Ty, .copy vp0⟩), []⟩
  { project := base.project, entry := mainFn, signatures := base.signatures
    functions := [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[blk]⟩)]
    storage := [(mainFn, [(⟨0⟩, .virtual)])], layouts := [(mainFn, [])] }

example :
    let mutant := passesWithLowerMemory lcbProfile fun _ => .ok wrongReturnMemory
    ¬ (registry mutant).lowerMemory.Validated := by
  intro mutant hVal
  have hLow := (hVal.1 base wrongReturnMemory base_wf rfl).2.1
  obtain ⟨_, _, hSafe, _, _, _⟩ := Concrete.Memory.BindReturn.refines_42
  let opened : State := { startState with live := [⟨0⟩] }
  let val43 : s0Lang.operands.Val := .scalar (.int .u8 43)
  let stored : State := { opened with initialized := [⟨0⟩], virtualValues := [(⟨0⟩, val43)] }
  let bound42 : State := { startState with live := [⟨0⟩], initialized := [⟨0⟩], virtualValues := [(⟨0⟩, val42)] }
  have hStep1 : Dst.step wrongReturnMemory startState opened.advance := by
    with_unfolding_all exact ⟨rfl, ⟨_, rfl, opened, ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩, [], rfl⟩
  have hStep2 : Dst.step wrongReturnMemory opened.advance { stored with instruction := 2 } := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, val43, opened.advance, _, rfl, ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩, rfl⟩, [], rfl⟩
  have hFin : Dst.finite wrongReturnMemory { stored with instruction := 2 } :=
    ⟨startState, by with_unfolding_all rfl, .cons hStep1 (.cons hStep2 (.nil _))⟩
  have hRes : Dst.result wrongReturnMemory { stored with instruction := 2 } (.returned 43 ByteArray.empty) := by
    with_unfolding_all rfl
  rcases LowersMemory.result_or_exhausted hLow hSafe hFin hRes with ⟨p, hpFin, hpRes, _⟩ | hBad
  · have hCases : p = startState ∨ p = { bound42 with instruction := 1 } := by
      apply finite_invariant Src base _ (fun _ h => .inl (by with_unfolding_all exact h)) _ hpFin
      rintro _ next (rfl | rfl) tick
      · have step : base.logicalView.InstructionStep startState next true (.bind ⟨p0, .virtual⟩ (.literal ⟨[42]⟩)) := by
          with_unfolding_all exact tick.2.1.2
        obtain ⟨v, ar, s, hrd, hw, rfl⟩ := step
        have e : some (val42, startState) = some (v, ar) := by with_unfolding_all exact hrd
        cases e
        cases hw with
        | cons hbeg hwr hnil =>
          cases hnil
          rcases hbeg.2 with ⟨_, rfl⟩ | ⟨hmat, _⟩
          · have hst : some bound42 = some s := by with_unfolding_all exact hwr.2.2.1
            cases hst; exact .inr rfl
          · have bad : some StorageClass.virtual = some StorageClass.materialized := by with_unfolding_all exact hmat
            cases bad
      · exact (Src.terminal base _ (.returned 42 ByteArray.empty) (by with_unfolding_all rfl) next tick).elim
    rcases hCases with rfl | rfl
    · have bad : none = some (Concrete.Contracts.Observation.returned 43 ByteArray.empty) := by with_unfolding_all exact hpRes
      cases bad
    · have bad : some (Concrete.Contracts.Observation.returned 42 ByteArray.empty) =
          some (Concrete.Contracts.Observation.returned 43 ByteArray.empty) := by
        with_unfolding_all exact hpRes
      cases bad
  · cases hBad

-- Mutant 2: unplaced mutant (drops inserted `beginLifetime`, leaving target stuck on safe `base`).
example :
    let unplaced : M := { base.logicalView with layouts := [(mainFn, [])] }
    let mutant := passesWithLowerMemory lcbProfile fun _ => .ok unplaced
    ¬ (registry mutant).lowerMemory.Validated := by
  intro unplaced mutant hVal
  have hLow := (hVal.1 base unplaced base_wf rfl).2.1
  obtain ⟨_, _, hSafe, _, _, _⟩ := Concrete.Memory.BindReturn.refines_42
  exact (hLow.2.2.2.2.2.2.2.2.2.2.1 hSafe).2.2 startState Concrete.Memory.BindReturn.unplaced_stuck

-- Mutant 3: drops declared local root `⟨0⟩` from `mem.functions`, violating frozen `RetainsTypedRoots`.
example :
    let mutant := passesWithLowerMemory lcbProfile fun mir =>
      (lowerMemory mir).map fun mem =>
        { mem with functions := List.map (fun (fn, _, g) => (fn, [], g)) mem.functions }
    ¬ (registry mutant).lowerMemory.Validated := by
  intro mutant hVal
  obtain ⟨mem, hOk⟩ := lowerMemory_ok_of_wf base_wf
  let bad : M := { mem with functions := List.map (fun (fn, _, g) => (fn, [], g)) mem.functions }
  have hRun : (registry mutant).lowerMemory.run base = .ok bad := by
    dsimp only [registry, passesWithLowerMemory, mutant]
    rw [hOk]; rfl
  have hRet := (hVal.1 base bad base_wf hRun).2.1.retainsTypedRoots
  obtain ⟨dstLocs, _, hMemDst, hIff⟩ := hRet.2.2.2.2.1 mainFn [(⟨0⟩, u8Ty)] _ List.mem_cons_self
  have hEmpty : dstLocs = [] := by
    simp only [bad, List.mem_map] at hMemDst
    obtain ⟨⟨_, _, _⟩, _, heq⟩ := hMemDst
    cases heq
    rfl
  have hIn := (hIff ⟨0⟩ u8Ty).mp List.mem_cons_self
  rw [hEmpty] at hIn
  cases hIn

-- Mutant 4: retypes declared local root `⟨0⟩` from `u8` to `bool`, violating frozen `RetainsTypedRoots`.
example :
    let mutant := passesWithLowerMemory lcbProfile fun mir =>
      (lowerMemory mir).map fun mem =>
        { mem with functions := List.map (fun (fn, locs, g) =>
            (fn, List.map (fun (r, _) => (r, boolTy)) locs, g)) mem.functions }
    ¬ (registry mutant).lowerMemory.Validated := by
  intro mutant hVal
  obtain ⟨mem, hOk⟩ := lowerMemory_ok_of_wf base_wf
  let bad : M :=
    { mem with functions := List.map (fun (fn, locs, g) =>
        (fn, List.map (fun (r, _) => (r, boolTy)) locs, g)) mem.functions }
  have hRun : (registry mutant).lowerMemory.run base = .ok bad := by
    dsimp only [registry, passesWithLowerMemory, mutant]
    rw [hOk]; rfl
  have hRet := (hVal.1 base bad base_wf hRun).2.1.retainsTypedRoots
  obtain ⟨dstLocs, _, hMemDst, hIff⟩ := hRet.2.2.2.2.1 mainFn [(⟨0⟩, u8Ty)] _ List.mem_cons_self
  have hIn := (hIff ⟨0⟩ u8Ty).mp List.mem_cons_self
  simp only [bad, List.mem_map] at hMemDst
  obtain ⟨⟨_, origLocs, _⟩, _, heq⟩ := hMemDst
  cases heq
  simp only [List.mem_map] at hIn
  obtain ⟨⟨_, _⟩, _, hPair⟩ := hIn
  cases hPair

-- Mutant 5: promotes escaping `u8` root `⟨0⟩` to `.virtual`, violating `PromotionSound`.
example :
    let mutant := passesWithLowerMemory lcbProfile fun mir =>
      (lowerMemory mir).map fun mem =>
        { mem with storage := List.map (fun (fn, cls) =>
            (fn, List.map (fun (r, _) => (r, .virtual)) cls)) mem.storage }
    ¬ (registry mutant).lowerMemory.Validated := by
  intro mutant hVal
  obtain ⟨mem, hOk⟩ := lowerMemory_ok_of_wf escapedU8MIR_wf
  let bad : M :=
    { mem with storage := List.map (fun (fn, cls) =>
        (fn, List.map (fun (r, _) => (r, .virtual)) cls)) mem.storage }
  have hRun : (registry mutant).lowerMemory.run escapedU8MIR = .ok bad := by
    dsimp only [registry, passesWithLowerMemory, mutant]
    rw [hOk]; rfl
  have hProm := (hVal.1 escapedU8MIR bad escapedU8MIR_wf hRun).2.1.2.2.2.2.2.1
  obtain ⟨cls, st, hLuFn, hLuR, hIff⟩ :=
    hProm.1 mainFn [(⟨0⟩, u8Ty), (⟨1⟩, u8Ty)] _ List.mem_cons_self ⟨0⟩ u8Ty List.mem_cons_self
  have hVirt : st = .virtual := by
    have hMemFn := AList.lookup_mem hLuFn
    simp only [bad, List.mem_map] at hMemFn
    obtain ⟨⟨_, origCls⟩, _, heq⟩ := hMemFn
    cases heq
    have hMemR := AList.lookup_mem hLuR
    simp only [List.mem_map] at hMemR
    obtain ⟨⟨_, _⟩, _, hrEq⟩ := hMemR
    cases hrEq
    rfl
  have hNoEsc : ¬ escapedU8MIR.Escapes (language := s0Lang) mainFn ⟨0⟩ := (hIff.mp hVirt).2
  have hEsc : escapedU8MIR.Escapes (language := s0Lang) mainFn ⟨0⟩ := by
    exact (escapes_iff escapedU8MIR mainFn ⟨0⟩).mp (by with_unfolding_all decide)
  exact hNoEsc hEsc

-- Mutant 6: drops `memory.mutableLocals` to `[]` on `mutableLocalMIR`.
example :
    let mutant := passesWithLowerMemory lcbProfile fun mir =>
      (lowerMemory mir).map fun mem => { mem with mutableLocals := [] }
    ¬ (registry mutant).lowerMemory.Validated := by
  intro mutant hVal
  obtain ⟨mem, hOk⟩ := lowerMemory_ok_of_wf mutableLocalMIR_wf
  let bad : M := { mem with mutableLocals := [] }
  have hRun : (registry mutant).lowerMemory.run mutableLocalMIR = .ok bad := by
    dsimp only [registry, passesWithLowerMemory, mutant]
    rw [hOk]; rfl
  have hMut := (hVal.1 mutableLocalMIR bad mutableLocalMIR_wf hRun).2.2
  cases hMut

-- Mutant 7: substitutes `memory.project` with `otherProject ≠ base.project`.
example :
    let mutant := passesWithLowerMemory lcbProfile fun mir =>
      (lowerMemory mir).map fun mem => { mem with project := otherProject }
    ¬ (registry mutant).lowerMemory.Validated := by
  intro mutant hVal
  obtain ⟨mem, hOk⟩ := lowerMemory_ok_of_wf base_wf
  let bad : M := { mem with project := otherProject }
  have hRun : (registry mutant).lowerMemory.run base = .ok bad := by
    dsimp only [registry, passesWithLowerMemory, mutant]
    rw [hOk]; rfl
  have hProj := (hVal.1 base bad base_wf hRun).2.1.1
  exact otherProject_ne_base hProj

-- Mutant 8: drops block origins from an unreachable block, violating `PreservesOrigins`.
private def deadOrigin : Origin := ⟨⟨99⟩, orig.lexical, orig.evidenceKey⟩

private def deadOriginMIR : MIR s0Low lcbProfile :=
  let b0 : MBlock := base.functions[0].2.2.blocks[0]
  let b1 : MBlock := ⟨⟨1⟩, [], [], .trap .abort deadOrigin, [deadOrigin]⟩
  { base with functions := [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[b0, b1]⟩)] }

example :
    let mutant := passesWithLowerMemory lcbProfile fun _ => lowerMemory base
    ¬ (registry mutant).lowerMemory.Validated := by
  intro mutant hVal
  letI := s0Lang.typeDecidableEq
  have hDeadWF : deadOriginMIR.HandoffWF (language := s0Lang) := by
    refine mir_handoffWF_of_u8
      ((checkHandoff_iff (deadOriginMIR.logicalView (language := s0Lang))).mp (by with_unfolding_all decide)) ?_
    intro fn locals graph hm r ty hloc
    cases List.mem_singleton.mp hm
    cases List.mem_singleton.mp hloc
    rfl
  obtain ⟨memBase, hOk⟩ := lowerMemory_ok_of_wf base_wf
  have hRun : (registry mutant).lowerMemory.run deadOriginMIR = .ok memBase := hOk
  have hOrigs := (hVal.1 deadOriginMIR memBase hDeadWF hRun).2.1.2.2.1
  have hInSrc : deadOrigin ∈ deadOriginMIR.origins := by
    with_unfolding_all decide
  have hInBase : deadOrigin ∈ memBase.origins := hOrigs.2 deadOrigin hInSrc
  have hOrigBase := (lowerMemory_lowersMemory base_wf hOk).2.2.1
  have hInBaseSrc : deadOrigin ∈ base.origins := hOrigBase.1 deadOrigin hInBase
  have hNotIn : deadOrigin ∉ base.origins := by
    with_unfolding_all decide
  exact hNotIn hInBaseSrc

-- Mutant 9: wraps raw `lowerGraph` without ambiguous-join specialization, refusing well-formed
-- `badJoinMIR` against the frozen `refusal := fun _ _ => False` contract.
example :
    let rawPass (mir : MIR s0Low lcbProfile) : Except StageFailure M :=
      match lowerGraph (language := s0Lang) mir with
      | .ok m => .ok m
      | .error _ => .error (.invalidInput invalidMIRRule)
    let mutant := passesWithLowerMemory lcbProfile rawPass
    ¬ (registry mutant).lowerMemory.Validated := by
  intro rawPass mutant hVal
  have hErr : (registry mutant).lowerMemory.run badJoinMIR = .error (.invalidInput invalidMIRRule) := by
    change rawPass badJoinMIR = .error (.invalidInput invalidMIRRule)
    unfold rawPass
    obtain ⟨j, hRaw⟩ := badJoinMIR_raw_lowerGraph_err
    rw [hRaw]
  exact hVal.2 badJoinMIR (.invalidInput invalidMIRRule) badJoinMIR_wf hErr

-- Mutant 10: corrupts materialized root layout from `.scalar 1 1` to `.scalar 4 4` on `escapedU8MIR`,
-- violating `LayoutsMatch`.
example :
    let mutant := passesWithLowerMemory lcbProfile fun mir =>
      (lowerMemory mir).map fun mem =>
        { mem with layouts := List.map (fun (fn, ls) =>
            (fn, List.map (fun (r, _) => (r, .scalar 4 4)) ls)) mem.layouts }
    ¬ (registry mutant).lowerMemory.Validated := by
  intro mutant hVal
  have hGraphOk : (lowerGraph (language := s0Lang) escapedU8MIR).isOk = true := by
    with_unfolding_all decide
  cases hG : lowerGraph (language := s0Lang) escapedU8MIR with
  | error _ => rw [hG] at hGraphOk; cases hGraphOk
  | ok mem =>
    have hOk : lowerMemory escapedU8MIR = .ok mem := lowerMemory_eq_lowerGraph_of_ok hG
    let bad : M :=
      { mem with layouts := List.map (fun (fn, ls) =>
          (fn, List.map (fun (r, _) => (r, .scalar 4 4)) ls)) mem.layouts }
    have hRun : (registry mutant).lowerMemory.run escapedU8MIR = .ok bad := by
      dsimp only [registry, passesWithLowerMemory, mutant]
      rw [hOk]; rfl
    have hLayMatch := (hVal.1 escapedU8MIR bad escapedU8MIR_wf hRun).2.1.2.2.2.1
    have hRet := (hVal.1 escapedU8MIR bad escapedU8MIR_wf hRun).2.1.retainsTypedRoots
    have hLayEq : ((lowerGraph (language := s0Lang) escapedU8MIR).toOption.map (·.layouts)) =
        some [(mainFn, [(⟨0⟩, .scalar 1 1)])] := by
      with_unfolding_all rfl
    rw [hG] at hLayEq
    have hMemLay : mem.layouts = [(mainFn, [(⟨0⟩, .scalar 1 1)])] := Option.some.inj hLayEq
    have hLayIn : (mainFn, [(⟨0⟩, Layout.scalar 4 4)]) ∈ bad.layouts := by
      simp only [bad, hMemLay, List.map_cons, List.map_nil]
      exact List.mem_cons_self
    obtain ⟨dstLocs, dstGraph, ty, _, hMemFn, hRootTy, _, _, hBadMatch⟩ :=
      hLayMatch.2 mainFn [(⟨0⟩, Layout.scalar 4 4)] (⟨0⟩ : PlaceId) (Layout.scalar 4 4) hLayIn List.mem_cons_self
    obtain ⟨srcLocs, _, hMemSrc, hIff⟩ := hRet.2.2.2.2.2 mainFn dstLocs dstGraph hMemFn
    cases List.mem_singleton.mp hMemSrc
    have hInSrc : ((⟨0⟩ : PlaceId), ty) ∈ [(⟨0⟩, u8Ty), (⟨1⟩, u8Ty)] := (hIff ⟨0⟩ ty).mp hRootTy
    have hTyEq : ty = u8Ty := by
      simp only [List.mem_cons, List.not_mem_nil, or_false] at hInSrc
      rcases hInSrc with h | h
      · exact (Prod.mk.inj h).2
      · exact False.elim (by cases (Prod.mk.inj h).1)
    subst hTyEq
    have hTrue : s0Lang.layout bad.declarations lcbProfile.memoryTarget u8Ty = some (.scalar 1 1) := by
      with_unfolding_all rfl
    rw [hTrue] at hBadMatch
    cases Option.some.inj hBadMatch

end StageInterfaceControls.D5Memory
