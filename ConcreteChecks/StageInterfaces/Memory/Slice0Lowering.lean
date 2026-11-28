import Concrete.Memory.StepSimulation
import Concrete.Memory.Controls

namespace StageInterfaceControls.Slice0Lowering

open Concrete Concrete.Memory Concrete.Interfaces
open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface
open Concrete.Contracts (Steps Semantics)

private abbrev Language := sliceLanguage slice0
private abbrev Lowering := sliceLoweringLanguage slice0
private abbrev profile : Profile := ⟨.lcb, .x86_64Linux, 0, 4096⟩
private abbrev State := MemoryRegisters Language profile
private abbrev M := Interfaces.Memory Lowering profile
private abbrev Src := Interfaces.MIR.Execution (language := Language) (lang := Lowering) (profile := profile)
private abbrev Dst := Interfaces.Memory.Execution (language := Language) (lang := Lowering) (profile := profile)
private abbrev MBlock := Block (Sigma (Place Language.operands.Ty)) (LocatedOp (MIRInst Lowering))
  (Terminator (Sigma (Read Language.operands.Ty Lowering.literal))
    (Read Language.operands.Ty Lowering.literal Lowering.bool))

private abbrev base := bindReturnControl slice0
private abbrev mainFn : FnId := base.entry
private def helperFn : FnId := ⟨mainFn.module, ⟨"helper"⟩⟩
private def emptyDeclId : DeclId := ⟨mainFn.module, ⟨"Empty"⟩⟩
private def emptyDecls : Declarations := [(emptyDeclId, ⟨emptyDeclId.name, false, []⟩)]

private abbrev u8Ty : Language.operands.Ty := .inl (.scalar (.int .u8))
private abbrev boolTy : Language.operands.Ty := .inl (.scalar .bool)
private abbrev emptyTy : Language.operands.Ty := .inl (.nominal emptyDeclId)

private def orig : Origin := base.functions[0].2.2.blocks[0].body[0].origin
private def p0 : Place _ u8Ty := ⟨⟨0⟩, []⟩
private def p1 : Place _ u8Ty := ⟨⟨1⟩, []⟩
private def p2 : Place _ u8Ty := ⟨⟨2⟩, []⟩
private def pb : Place _ boolTy := ⟨⟨0⟩, []⟩
private def pe : Place _ emptyTy := ⟨⟨0⟩, []⟩

private def startState : State :=
  ⟨⟨mainFn, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, .empty, [], [], {}, 0, false⟩

private def overflowTrapObs : Concrete.Contracts.Observation :=
  .trapped 134 .overflow
    ⟨⟨orig.operation.id⟩, ⟨mainSourcePath.render.text, orig.lexical.line, orig.lexical.col⟩⟩
    (trapDiagnostic base.project .overflow orig.operation orig.lexical)

private theorem lowers_of_checks {mir : MIR Lowering profile}
    (hok : (lowerGraph (language := Language) mir).isOk = true)
    (hwf : mir.logicalView.HandoffWF) :
    ∃ m, lowerGraph (language := Language) mir = .ok m ∧
      LowersMemory (language := Language) mir m := by
  cases h : lowerGraph (language := Language) mir with
  | error e => rw [h] at hok; cases hok
  | ok m => exact ⟨m, rfl, slice_lowerGraph_lowersMemory h hwf⟩

/-! ### Positive Control 1: `bindReturnControl` via `slice_lowerGraph_lowersMemory` and `slice_lowerGraph_correspondence` -/

theorem bindReturn_lowersMemory :
    ∃ m, lowerGraph (language := Language) base = .ok m ∧
      LowersMemory (language := Language) base m ∧
      LocalCorrespondence (language := Language) base m := by
  letI := Language.typeDecidableEq
  have hwf : base.logicalView.HandoffWF := (checkHandoff_iff _).mp (by with_unfolding_all decide)
  obtain ⟨m, hok, hsafe, _, _, _⟩ := Concrete.Memory.BindReturn.refines_42
  exact ⟨m, hok, slice_lowerGraph_lowersMemory hok hwf, slice_lowerGraph_correspondence hok hwf hsafe⟩

/-! ### Positive Control 2: Multi-binding Slice 0 checked addition returning 42 -/

private def checkedAddMIR : MIR Lowering profile :=
  let b0 : MBlock :=
    ⟨⟨0⟩, [],
      [⟨orig, .bind p0 (.literal ⟨[20]⟩)⟩,
       ⟨orig, .bind p1 (.literal ⟨[22]⟩)⟩,
       ⟨orig, .op (opcode := .arithmetic (.plain .add)) (types := [u8Ty, u8Ty]) p2 ()
         (.cons (.copy p0) (.cons (.copy p1) .nil)) [⟨0, .overflow, .residual⟩] []⟩],
      .ret (some ⟨u8Ty, .copy p2⟩), []⟩
  { base with
    functions := [(mainFn, [(⟨0⟩, u8Ty), (⟨1⟩, u8Ty), (⟨2⟩, u8Ty)], ⟨⟨0⟩, #[b0]⟩)] }

theorem checkedAdd_lowersMemory :
    ∃ m, lowerGraph (language := Language) checkedAddMIR = .ok m ∧
      LowersMemory (language := Language) checkedAddMIR m ∧
      (m.functions.head?.bind fun f => f.2.2.blocks[0]?.map (·.body.length)) = some 6 := by
  letI := Language.typeDecidableEq
  have hwf : checkedAddMIR.logicalView.HandoffWF := (checkHandoff_iff _).mp (by with_unfolding_all decide)
  have hok : (lowerGraph (language := Language) checkedAddMIR).isOk = true := by with_unfolding_all decide
  cases h : lowerGraph (language := Language) checkedAddMIR with
  | error e => rw [h] at hok; cases hok
  | ok m =>
    refine ⟨m, rfl, slice_lowerGraph_lowersMemory h hwf, ?_⟩
    have hlen : ((lowerGraph (language := Language) checkedAddMIR).toOption.bind fun x =>
        x.functions.head?.bind fun f => f.2.2.blocks[0]?.map (·.body.length)) = some 6 := by
      with_unfolding_all rfl
    rw [h] at hlen
    exact hlen

/-! ### Positive Control 3: Residual overflow traps (declared and undeclared destinations) -/

private def checkedAddTrapMIR (declareResult : Bool) : MIR Lowering profile :=
  let op : MIRInst Lowering :=
    .op (opcode := .arithmetic (.plain .add)) (types := [u8Ty, u8Ty]) p0 ()
      (.cons (.literal ⟨[20]⟩) (.cons (.literal ⟨[250]⟩) .nil)) [⟨0, .overflow, .residual⟩] []
  { base with
    functions := [(mainFn, if declareResult then [(⟨0⟩, u8Ty)] else [],
      ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨orig, op⟩], .ret (some ⟨u8Ty, .copy p0⟩), []⟩]⟩)] }

theorem checkedAdd_declared_trap_lowersMemory :
    ∃ m, lowerGraph (language := Language) (checkedAddTrapMIR true) = .ok m ∧
      LowersMemory (language := Language) (checkedAddTrapMIR true) m ∧
      Src.observes (checkedAddTrapMIR true) overflowTrapObs ∧
      Dst.observes m overflowTrapObs := by
  letI := Language.typeDecidableEq
  have hwf : (checkedAddTrapMIR true).logicalView.HandoffWF :=
    (checkHandoff_iff _).mp (by with_unfolding_all decide)
  obtain ⟨m, hok, hinit, hstep, hres⟩ := Concrete.Memory.BindReturn.declared_add_traps
  refine ⟨m, hok, slice_lowerGraph_lowersMemory hok hwf,
    ⟨startState, ⟨startState, by with_unfolding_all rfl, .nil _⟩, by with_unfolding_all rfl⟩,
    ⟨_, ⟨startState, hinit, .cons hstep (.nil _)⟩, hres⟩⟩

private def undeclaredTrapMemory : M :=
  let op : MemoryInst Lowering :=
    .op (opcode := .arithmetic (.plain .add)) (types := [u8Ty, u8Ty]) ⟨p0, .materialized⟩ ()
      (.cons (.literal ⟨[20]⟩) (.cons (.literal ⟨[250]⟩) .nil)) [⟨0, .overflow, .residual⟩] []
  { project := base.project, entry := mainFn, signatures := base.signatures
    functions := [(mainFn, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨orig, op⟩],
      .ret (some ⟨u8Ty, .copy ⟨p0, .materialized⟩⟩), []⟩]⟩)]
    storage := [(mainFn, [])], layouts := [(mainFn, [])] }

theorem checkedAdd_undeclared_trap_lowersMemory :
    ∃ m, lowerGraph (language := Language) (checkedAddTrapMIR false) = .ok m ∧
      LowersMemory (language := Language) (checkedAddTrapMIR false) m ∧
      Src.observes (checkedAddTrapMIR false) overflowTrapObs ∧
      Dst.observes m overflowTrapObs := by
  letI := Language.typeDecidableEq
  have hwf : (checkedAddTrapMIR false).logicalView.HandoffWF :=
    (checkHandoff_iff _).mp (by with_unfolding_all decide)
  have hok : lowerGraph (language := Language) (checkedAddTrapMIR false) = .ok undeclaredTrapMemory := by
    with_unfolding_all rfl
  exact ⟨undeclaredTrapMemory, hok, slice_lowerGraph_lowersMemory hok hwf,
    ⟨startState, ⟨startState, by with_unfolding_all rfl, .nil _⟩, by with_unfolding_all rfl⟩,
    ⟨startState, ⟨startState, by with_unfolding_all rfl, .nil _⟩, by with_unfolding_all rfl⟩⟩

/-! ### Positive Control 4: `bind`, `assign`, `endLifetime`, and re-`bind` -/

private def assignEndRebindMIR : MIR Lowering profile :=
  let b0 : MBlock :=
    ⟨⟨0⟩, [],
      [⟨orig, .bind p0 (.literal ⟨[10]⟩)⟩,
       ⟨orig, .assign p0 (.literal ⟨[21]⟩)⟩,
       ⟨orig, .endLifetime ⟨0⟩⟩,
       ⟨orig, .bind p0 (.literal ⟨[42]⟩)⟩],
      .ret (some ⟨u8Ty, .copy p0⟩), []⟩
  { base with functions := [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[b0]⟩)] }

theorem assign_endLifetime_rebind_lowersMemory :
    ∃ m, lowerGraph (language := Language) assignEndRebindMIR = .ok m ∧
      LowersMemory (language := Language) assignEndRebindMIR m ∧
      (m.functions.head?.bind fun f => f.2.2.blocks[0]?.map (·.body.length)) = some 6 := by
  letI := Language.typeDecidableEq
  have hwf : assignEndRebindMIR.logicalView.HandoffWF := (checkHandoff_iff _).mp (by with_unfolding_all decide)
  have hok : (lowerGraph (language := Language) assignEndRebindMIR).isOk = true := by with_unfolding_all decide
  cases h : lowerGraph (language := Language) assignEndRebindMIR with
  | error e => rw [h] at hok; cases hok
  | ok m =>
    refine ⟨m, rfl, slice_lowerGraph_lowersMemory h hwf, ?_⟩
    have hlen : ((lowerGraph (language := Language) assignEndRebindMIR).toOption.bind fun x =>
        x.functions.head?.bind fun f => f.2.2.blocks[0]?.map (·.body.length)) = some 6 := by
      with_unfolding_all rfl
    rw [h] at hlen
    exact hlen

/-! ### Positive Control 5: `branch` with block parameters and explicit `trap` -/

private def branchParamsMIR : MIR Lowering profile :=
  let b0 : MBlock :=
    ⟨⟨0⟩, [],
      [⟨orig, .bind pb (.literal ⟨[1]⟩)⟩],
      .branch (.copy pb)
        ⟨⟨1⟩, [⟨u8Ty, .literal ⟨[42]⟩⟩]⟩
        ⟨⟨2⟩, [⟨u8Ty, .literal ⟨[0]⟩⟩]⟩, []⟩
  let b1 : MBlock := ⟨⟨1⟩, [⟨u8Ty, p1⟩], [], .ret (some ⟨u8Ty, .copy p1⟩), []⟩
  let b2 : MBlock := ⟨⟨2⟩, [⟨u8Ty, p1⟩], [], .trap .abort orig, []⟩
  { base with functions := [(mainFn, [(⟨0⟩, boolTy), (⟨1⟩, u8Ty)], ⟨⟨0⟩, #[b0, b1, b2]⟩)] }

theorem branch_blockParams_lowersMemory :
    ∃ m, lowerGraph (language := Language) branchParamsMIR = .ok m ∧
      LowersMemory (language := Language) branchParamsMIR m := by
  letI := Language.typeDecidableEq
  exact lowers_of_checks (by with_unfolding_all decide) ((checkHandoff_iff _).mp (by with_unfolding_all decide))

/-! ### Positive Control 6: Interprocedural `call` and callee `ret` -/

private def callRetMIR : MIR Lowering profile :=
  let mainBlock : MBlock :=
    ⟨⟨0⟩, [],
      [⟨orig, .call helperFn (some ⟨u8Ty, p0⟩)
        [⟨u8Ty, .literal ⟨[20]⟩⟩, ⟨u8Ty, .literal ⟨[22]⟩⟩] []⟩],
      .ret (some ⟨u8Ty, .copy p0⟩), []⟩
  let helperBlock : MBlock :=
    ⟨⟨0⟩, [⟨u8Ty, p0⟩, ⟨u8Ty, p1⟩],
      [⟨orig, .op (opcode := .arithmetic (.plain .add)) (types := [u8Ty, u8Ty]) p2 ()
        (.cons (.copy p0) (.cons (.copy p1) .nil)) [⟨0, .overflow, .residual⟩] []⟩],
      .ret (some ⟨u8Ty, .copy p2⟩), []⟩
  { base with
    signatures := [(mainFn, ⟨[], some u8Ty⟩), (helperFn, ⟨[u8Ty, u8Ty], some u8Ty⟩)]
    functions :=
      [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[mainBlock]⟩),
       (helperFn, [(⟨0⟩, u8Ty), (⟨1⟩, u8Ty), (⟨2⟩, u8Ty)], ⟨⟨0⟩, #[helperBlock]⟩)] }

theorem call_ret_lowersMemory :
    ∃ m, lowerGraph (language := Language) callRetMIR = .ok m ∧
      LowersMemory (language := Language) callRetMIR m := by
  letI := Language.typeDecidableEq
  exact lowers_of_checks (by with_unfolding_all decide) ((checkHandoff_iff _).mp (by with_unfolding_all decide))

/-! ### Positive Control 7: Diverging control-flow loop (`jump` back-edge) -/

private def loopDivergeMIR : MIR Lowering profile :=
  let b0 : MBlock := ⟨⟨0⟩, [], [⟨orig, .bind p0 (.literal ⟨[42]⟩)⟩], .jump ⟨⟨1⟩, []⟩, []⟩
  let b1 : MBlock := ⟨⟨1⟩, [], [], .jump ⟨⟨1⟩, []⟩, []⟩
  { base with functions := [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[b0, b1]⟩)] }

theorem loop_divergence_lowersMemory :
    ∃ m, lowerGraph (language := Language) loopDivergeMIR = .ok m ∧
      LowersMemory (language := Language) loopDivergeMIR m := by
  letI := Language.typeDecidableEq
  exact lowers_of_checks (by with_unfolding_all decide) ((checkHandoff_iff _).mp (by with_unfolding_all decide))

/-! ### Positive Control 8: Zero-byte materialized empty linear nominal (`constructEmpty` + `destructure`) -/

private def emptyNominalMIR : MIR Lowering profile :=
  let b0 : MBlock :=
    ⟨⟨0⟩, [],
      [⟨orig, .op (opcode := .named ⟨"constructEmpty"⟩) (types := []) pe () .nil [] []⟩,
       ⟨orig, .destructure pe []⟩,
       ⟨orig, .endLifetime ⟨0⟩⟩],
      .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
  { base with
    declarations := emptyDecls
    functions := [(mainFn, [(⟨0⟩, emptyTy)], ⟨⟨0⟩, #[b0]⟩)] }

theorem emptyNominal_destructure_lowersMemory :
    ∃ m, lowerGraph (language := Language) emptyNominalMIR = .ok m ∧
      LowersMemory (language := Language) emptyNominalMIR m ∧
      m.storage = [(mainFn, [(⟨0⟩, .materialized)])] ∧
      m.layouts = [(mainFn, [(⟨0⟩, .scalar 0 1)])] := by
  letI := Language.typeDecidableEq
  have hwf : emptyNominalMIR.logicalView.HandoffWF := (checkHandoff_iff _).mp (by with_unfolding_all decide)
  have hok : (lowerGraph (language := Language) emptyNominalMIR).isOk = true := by with_unfolding_all decide
  cases h : lowerGraph (language := Language) emptyNominalMIR with
  | error e => rw [h] at hok; cases hok
  | ok m =>
    have hsl : ((lowerGraph (language := Language) emptyNominalMIR).toOption.map
        fun x => (x.storage, x.layouts)) =
        some ([(mainFn, [(⟨0⟩, .materialized)])], [(mainFn, [(⟨0⟩, .scalar 0 1)])]) := by
      with_unfolding_all rfl
    rw [h] at hsl
    simp only [Except.toOption, Option.map_some, Option.some.injEq, Prod.mk.injEq] at hsl
    exact ⟨m, rfl, slice_lowerGraph_lowersMemory h hwf, hsl.1, hsl.2⟩

/-! ### Positive Control 9: Nonempty 1-byte materialized escaped `u8` local -/

private def escapedU8MIR : MIR Lowering profile :=
  let b0 : MBlock :=
    ⟨⟨0⟩, [], [⟨orig, .bind p0 (.literal ⟨[42]⟩)⟩], .ret (some ⟨u8Ty, .copy p0⟩), []⟩
  let b1 : MBlock :=
    ⟨⟨1⟩, [], [⟨orig, .op (opcode := .read) (types := []) p1 () .nil [] [⟨u8Ty, p0⟩]⟩],
      .trap .abort orig, []⟩
  { base with functions := [(mainFn, [(⟨0⟩, u8Ty), (⟨1⟩, u8Ty)], ⟨⟨0⟩, #[b0, b1]⟩)] }

theorem escaped_materialized_u8_lowersMemory :
    ∃ m, lowerGraph (language := Language) escapedU8MIR = .ok m ∧
      LowersMemory (language := Language) escapedU8MIR m ∧
      m.storage = [(mainFn, [(⟨0⟩, .materialized), (⟨1⟩, .virtual)])] ∧
      m.layouts = [(mainFn, [(⟨0⟩, .scalar 1 1)])] := by
  letI := Language.typeDecidableEq
  have hwf : escapedU8MIR.logicalView.HandoffWF := (checkHandoff_iff _).mp (by with_unfolding_all decide)
  have hok : (lowerGraph (language := Language) escapedU8MIR).isOk = true := by with_unfolding_all decide
  cases h : lowerGraph (language := Language) escapedU8MIR with
  | error e => rw [h] at hok; cases hok
  | ok m =>
    have hsl : ((lowerGraph (language := Language) escapedU8MIR).toOption.map
        fun x => (x.storage, x.layouts)) =
        some ([(mainFn, [(⟨0⟩, .materialized), (⟨1⟩, .virtual)])], [(mainFn, [(⟨0⟩, .scalar 1 1)])]) := by
      with_unfolding_all rfl
    rw [h] at hsl
    simp only [Except.toOption, Option.map_some, Option.some.injEq, Prod.mk.injEq] at hsl
    exact ⟨m, rfl, slice_lowerGraph_lowersMemory h hwf, hsl.1, hsl.2⟩

/-! ### Negative / Mutant Controls -/

private def unplacedMutant : M := { base.logicalView with layouts := [(mainFn, [])] }

/-- Dropping inserted `beginLifetime` instructions produces a stuck target on a safe source,
so the `LowersMemory` contract rejects the unplaced mutant. -/
theorem unplaced_mutant_not_lowersMemory :
    ¬ LowersMemory (language := Language) base unplacedMutant := by
  intro contract
  obtain ⟨_, _, hsafe, _, _, _⟩ := Concrete.Memory.BindReturn.refines_42
  exact (contract.2.2.2.2.2.2.2.2.1 hsafe).2.2 startState Concrete.Memory.BindReturn.unplaced_stuck

private def wrongReturnMemory : M :=
  let vRoot : MemoryPlace _ u8Ty := ⟨p0, .virtual⟩
  let blk : MemoryBlock Lowering :=
    ⟨⟨0⟩, [], [⟨orig, .beginLifetime ⟨0⟩⟩, ⟨orig, .bind vRoot (.literal ⟨[43]⟩)⟩],
      .ret (some ⟨u8Ty, .copy vRoot⟩), []⟩
  { project := base.project, entry := mainFn, signatures := base.signatures
    functions := [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[blk]⟩)]
    storage := [(mainFn, [(⟨0⟩, .virtual)])], layouts := [(mainFn, [])] }

/-- Mutating the lowered literal from 42 to 43 preserves `beginLifetime` placement and
progress, yet fails `LowersMemory` because the target observes 43 which `base` never returns. -/
theorem wrongValue_mutant_not_lowersMemory :
    ¬ LowersMemory (language := Language) base wrongReturnMemory := by
  intro contract
  obtain ⟨_, _, hsafe, _, _, _⟩ := Concrete.Memory.BindReturn.refines_42
  let opened : State := { startState with live := [⟨0⟩] }
  let val43 : Language.operands.Val := .scalar (.int .u8 43)
  let stored : State := { opened with initialized := [⟨0⟩], virtualValues := [(⟨0⟩, val43)] }
  let bound42 : State := { startState with
    live := [⟨0⟩], initialized := [⟨0⟩], virtualValues := [(⟨0⟩, .scalar (.int .u8 42))] }
  have hstep1 : Dst.step wrongReturnMemory startState opened.advance := by
    with_unfolding_all exact ⟨rfl, ⟨_, rfl, opened, ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩, rfl⟩, [], rfl⟩
  have hstep2 : Dst.step wrongReturnMemory opened.advance { stored with instruction := 2 } := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, val43, opened.advance, _, rfl, ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩, rfl⟩, [], rfl⟩
  have hfin : Dst.finite wrongReturnMemory { stored with instruction := 2 } :=
    ⟨startState, by with_unfolding_all rfl, .cons hstep1 (.cons hstep2 (.nil _))⟩
  have hres : Dst.result wrongReturnMemory { stored with instruction := 2 } (.returned 43 ByteArray.empty) := by
    with_unfolding_all rfl
  rcases LowersMemory.result_or_exhausted contract hsafe hfin hres with ⟨p, hpFin, hpRes, _⟩ | hbad
  · have hcases : p = startState ∨ p = { bound42 with instruction := 1 } := by
      apply finite_invariant Src base _ (fun _ h => .inl (by with_unfolding_all exact h)) _ hpFin
      rintro _ next (rfl | rfl) tick
      · have step : base.logicalView.InstructionStep startState next true (.bind ⟨p0, .virtual⟩ (.literal ⟨[42]⟩)) := by
          with_unfolding_all exact tick.2.1.2
        obtain ⟨v, ar, s, hrd, hw, rfl⟩ := step
        have e : some (.scalar (.int .u8 42), startState) = some (v, ar) := by with_unfolding_all exact hrd
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
    rcases hcases with rfl | rfl
    · have bad : none = some (Concrete.Contracts.Observation.returned 43 ByteArray.empty) := by
        with_unfolding_all exact hpRes
      cases bad
    · have bad : some (Concrete.Contracts.Observation.returned 42 ByteArray.empty) =
          some (Concrete.Contracts.Observation.returned 43 ByteArray.empty) := by
        with_unfolding_all exact hpRes
      cases bad
  · cases hbad

private def badJoinMIR : MIR Lowering profile :=
  let b0 : MBlock := ⟨⟨0⟩, [], [⟨orig, .bind p0 (.literal ⟨[42]⟩)⟩], .jump ⟨⟨0⟩, []⟩, []⟩
  { base with functions := [(mainFn, [(⟨0⟩, u8Ty)], ⟨⟨0⟩, #[b0]⟩)] }

private def badJoinBlock : MemoryBlock Lowering :=
  ⟨⟨0⟩, [], [⟨orig, .bind ⟨p0, .virtual⟩ (.literal ⟨[42]⟩)⟩], .jump ⟨⟨0⟩, []⟩, []⟩

private def badJoinGraph : MemoryGraph Lowering := ⟨⟨0⟩, #[badJoinBlock]⟩

private def badJoinPrepared : PreparedGraphs Lowering profile :=
  { functions := [(mainFn, [(⟨0⟩, u8Ty)], badJoinGraph)]
    storage := [(mainFn, [(⟨0⟩, .virtual)])]
    layouts := [(mainFn, [])] }

/-- A back-edge re-entering a defining block without `endLifetime` has both incoming
lifetime facts (`false` on activation entry and `true` around the back-edge), so
`lowerGraph` rejects it with a `.lifetime` failure. -/
theorem ambiguousJoin_rejected_by_lowerGraph :
    ∃ j, lowerGraph (language := Language) badJoinMIR = .error (.lifetime j) := by
  have hprepExact : prepareGraph (language := Language) badJoinMIR = .ok badJoinPrepared := by
    with_unfolding_all rfl
  cases h : lowerGraph (language := Language) badJoinMIR with
  | error e =>
    cases e with
    | lifetime j => exact ⟨j, rfl⟩
    | layout miss =>
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
    letI := Language.typeDecidableEq
    have hwf : badJoinMIR.logicalView.HandoffWF := (checkHandoff_iff _).mp (by with_unfolding_all decide)
    obtain ⟨graphs, hprep, hplaced⟩ := (lowerGraph_handoff h hwf).placement
    cases Except.ok.inj (hprep.symm.trans hprepExact)
    obtain ⟨placed, _, hpg⟩ := hplaced mainFn [(⟨0⟩, u8Ty)] badJoinGraph (.head _)
    obtain ⟨pb0, _, hblk⟩ := hpg.blocks.forward badJoinBlock (.head _)
    have hbody := hblk.body
    have hdef : definitionRoot [(⟨0⟩, u8Ty)] (MemoryInst.bind (lang := Lowering) ⟨p0, .virtual⟩ (.literal ⟨[42]⟩)) = some ⟨0⟩ := by
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

end StageInterfaceControls.Slice0Lowering
