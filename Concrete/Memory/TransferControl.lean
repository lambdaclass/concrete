import Concrete.Memory.Refinement

namespace Concrete.Memory.TransferControl

open Concrete.Interfaces ConcreteSpec.Core ConcreteSpec.StageInterface

-- A single live backing object leaves addresses 1, 2 and 3 available. Supply
-- its layout through the language interface so the fixture fits in two steps.
private def backingSize := 2^64 - 5
private abbrev Language : CoreLanguage := {sliceLanguage slice0 with
  layout := fun target ty => match ty with
    | .inr false => some ⟨backingSize, 1⟩
    | _ => (sliceLanguage slice0).layout target ty}
private abbrev Lowering := sliceLoweringLanguage slice0
private abbrev profile : Profile := ⟨.lcb, .x86_64Linux, 4, 4096⟩
private abbrev State := MemoryRegisters Language profile
private abbrev M := Interfaces.Memory Lowering profile
private abbrev exec := Interfaces.Memory.Execution (language := Language) (lang := Lowering) (profile := profile)
private abbrev mir := bindReturnControl slice0
private abbrev u8Ty : Language.operands.Ty := .inl (.scalar (.int .u8))
private abbrev u16Ty : Language.operands.Ty := .inl (.scalar (.int .u16))
private def byte : MemoryPlace _ u8Ty := ⟨⟨⟨0⟩, []⟩, .materialized⟩
private def word : MemoryPlace _ u16Ty := ⟨⟨⟨1⟩, []⟩, .materialized⟩
private def byteValue : Language.operands.Val := .scalar (.int .u8 42)
private def wordValue : Language.operands.Val := .scalar (.int .u16 42)
private def backing : PlaceId := ⟨2⟩
private def origin := mir.functions[0].2.2.blocks[0].body[0].origin
private def locals : LocalTypes Language.operands.Ty :=
  [(byte.logical.root, u8Ty), (word.logical.root, u16Ty), (backing, .inr false)]
private def layouts : AList FnId (AList PlaceId Layout) :=
  [(mir.entry, [(byte.logical.root, ⟨1, 1⟩), (word.logical.root, ⟨2, 2⟩), (backing, ⟨backingSize, 1⟩)])]
private def params : List (Sigma (MemoryPlace Language.operands.Ty)) := [⟨u8Ty, byte⟩, ⟨u16Ty, word⟩]
private def edge : CFGEdge (Sigma (MemoryRead _ Lowering.literal)) :=
  ⟨⟨1⟩, [⟨u8Ty, .literal ⟨[42]⟩⟩, ⟨u16Ty, .literal ⟨[42, 0]⟩⟩]⟩

-- In the rollback case a preceding begin reserves the first parameter's byte.
private def entryBlock (preopen : Bool) : MemoryBlock Lowering :=
  let body := [⟨origin, .beginLifetime backing⟩] ++
    (if preopen then [⟨origin, .beginLifetime byte.logical.root⟩] else [])
  ⟨⟨0⟩, [], body, .jump edge, []⟩

private def destination : MemoryBlock Lowering := ⟨⟨1⟩, params, [], .ret (some ⟨u8Ty, .copy byte⟩), []⟩

private def program (preopen : Bool) : M :=
  {project := mir.project, entry := mir.entry, signatures := mir.signatures
   functions := [(mir.entry, locals, ⟨⟨0⟩, #[entryBlock preopen, destination]⟩)]
   storage := [(mir.entry, List.map (fun decl => (decl.1, .materialized)) locals)]
   layouts}

private def start : State := ⟨⟨mir.entry, ⟨0⟩, 0, [], [], [], [], [], [], []⟩, .empty, [], [], {}, 0, false⟩
private def reserved (s : State) (root : PlaceId) (base size align : Nat) : State :=
  let ptr : Mem.Ptr := ⟨⟨0⟩, base, some ⟨s.bytes.next⟩⟩
  {s with live := root :: s.live, materialized := (root, ⟨ptr, size, align⟩) :: s.materialized
          bytes := ⟨(⟨s.bytes.next⟩, Mem.candidate ⟨0⟩ base size align) :: s.bytes.blocks, s.bytes.next + 1⟩}
private def jumping := (reserved start backing 4 backingSize 1).advance
private def arriving : State := {jumping with block := edge.target, instruction := 0}
private def openByte (base : Nat) := reserved arriving byte.logical.root base 1 1
private def writtenByte (base : Nat) : State :=
  ((program false).writeValue (language := Language) (openByte base) byte byteValue).getD arriving
private def openWord := reserved (writtenByte 1) word.logical.root 2 2 2
private def completed : State :=
  ((program false).writeValue (language := Language) openWord word wordValue).getD arriving
private def forced := (reserved jumping byte.logical.root 2 1 1).advance

private theorem handoff (preopen : Bool) : (program preopen).HandoffWF := by
  letI := Language.typeDecidableEq
  cases preopen <;> exact (checkHandoff_iff _).mp (by with_unfolding_all decide)

private theorem reach_jump (preopen : Bool) : exec.finite (program preopen) jumping := by
  refine ⟨start, by cases preopen <;> with_unfolding_all rfl, .cons (b := jumping) ?_ (.nil _)⟩
  cases preopen <;> with_unfolding_all
    exact ⟨rfl, ⟨_, rfl, _, ⟨nofun, .inr ⟨rfl, _, rfl, .inr
      ⟨by decide, ⟨0⟩, 4, ⟨0⟩, _, rfl, rfl⟩⟩⟩, rfl⟩, [], rfl⟩

private theorem begin_byte (base : Nat) (h : base = 1 ∨ base = 2) :
    (program false).Begins (language := Language) arriving byte.logical.root (openByte base) := by
  rcases h with rfl | rfl <;> with_unfolding_all exact
    ⟨by decide, .inr ⟨rfl, _, rfl, .inr ⟨by decide, ⟨0⟩, _, ⟨1⟩, _, rfl, rfl⟩⟩⟩

private theorem write_byte (base : Nat) (h : base = 1 ∨ base = 2) :
    (program false).Writes (language := Language) (openByte base) byte byteValue (writtenByte base) := by
  rcases h with rfl | rfl <;> with_unfolding_all
    refine ⟨⟨rfl, nofun⟩, fun _ => ⟨_, rfl, nofun, nofun⟩, rfl, rfl,
      fun _ => .inr ⟨_, [(0, [.init 42 none])], rfl, rfl, fun _ => ⟨0, _, .head _, by decide⟩, ?_⟩⟩
    intro off bytes present
    cases List.mem_singleton.mp present
    exact ⟨by decide, by decide, rfl⟩

private theorem fits_together :
    (program false).WriteParams (language := Language) params [byteValue, wordValue] arriving completed := by
  refine .cons (begin_byte 1 (.inl rfl)) (write_byte 1 (.inl rfl)) (.cons (started := openWord) ?_ ?_ (.nil _))
  · with_unfolding_all exact
      ⟨by decide, .inr ⟨rfl, _, rfl, .inr ⟨by decide, ⟨0⟩, 2, ⟨2⟩, _, rfl, rfl⟩⟩⟩
  · with_unfolding_all
      refine ⟨⟨rfl, nofun⟩, fun _ => ⟨_, rfl, nofun, nofun⟩, rfl, rfl,
        fun _ => .inr ⟨_, [(0, [.init 42 none, .init 0 none])], rfl, rfl, fun _ => ⟨0, _, .head _, by decide⟩, ?_⟩⟩
      intro off bytes present
      cases List.mem_singleton.mp present
      exact ⟨by decide, by decide, rfl⟩

-- Once byte 2 is occupied, the sole aligned candidate below the backing
-- object overlaps that byte. Every larger candidate overlaps the backing.
private theorem fragmented {m : M} {s : State}
    (fresh : word.logical.root ∉ s.live)
    (cls : m.classAt s.frame word.logical.root = some .materialized)
    (shape : m.layoutAt s.frame word.logical.root = some ⟨2, 2⟩)
    (bytes : s.bytes = (writtenByte 2).bytes) : m.BeginExhausted (language := Language) s word.logical.root := by
  refine ⟨fresh, cls, ⟨_, shape, by decide⟩, ?_⟩
  rintro ⟨_, _, ⟨hv, _⟩ | ⟨_, layout, hl, zero | placed⟩⟩
  · cases cls.symm.trans hv
  all_goals cases Option.some.inj (hl.symm.trans shape)
  · exact (by decide : (2 : Nat) ≠ 0) zero.1
  · obtain ⟨_, space, base, _, _, alloc, _⟩ := placed
    rw [bytes] at alloc
    unfold Mem.Memory.alloc at alloc
    dsimp only at alloc
    split at alloc
    · rename_i fits
      obtain rfl : space = ⟨0⟩ := List.mem_singleton.mp fits.2.2.2.2.2.2
      have aligned : base % 2 = 0 := fits.2.1
      have nonnull : base ≠ 0 := fits.2.2.2.2.1 (by decide : 0 < 2)
      have top : base + 2 ≤ 2^64 - 1 := fits.2.2.1
      by_cases atByte : base = 2
      · subst base
        apply fits.2.2.2.2.2.1 _ (by with_unfolding_all exact List.mem_cons_self ..) rfl
        exact ⟨rfl, by decide, by decide, by decide, by decide⟩
      · apply fits.2.2.2.2.2.1 (⟨0⟩, Mem.candidate ⟨0⟩ 4 backingSize 1)
          (by with_unfolding_all exact List.mem_cons_of_mem _ (List.mem_cons_self ..)) rfl
        refine ⟨rfl, by decide, (by decide : 0 < 2), ?_, ?_⟩
        · change 4 < base + 2
          omega
        · change base < 4 + (2^64 - 5)
          omega
    · contradiction

private theorem jump_outcome (preopen : Bool) {q next : State} {mode}
    (atPC : q = (if preopen then forced else jumping))
    (raw : (program preopen).StepOutcome (language := Language) q next mode) :
    (program preopen).ParamOutcome (language := Language) params [byteValue, wordValue]
      {q with block := edge.target, instruction := 0} next mode := by
  have transfer := raw.terminatorAt (language := Language)
    (block := entryBlock preopen)
    (by subst q; cases preopen <;> with_unfolding_all rfl)
    (by subst q; cases preopen <;> with_unfolding_all rfl)
  change (program preopen).Transfer (language := Language) edge q next mode at transfer
  obtain ⟨values, read, block, hr, hb, _, outcome⟩ := transfer
  have args : some ([byteValue, wordValue], q) = some (values, read) := by with_unfolding_all exact hr
  cases args
  have lookup : some destination = some block := by
    subst q
    cases preopen <;> with_unfolding_all exact hb
  cases lookup
  exact outcome

/-- A bad prefix really can strand the u16 here. The legal joint placement
commits, and the same reachable jump admits no exhausted successor. -/
theorem fitting_transfer :
    (program false).HandoffWF ∧ exec.finite (program false) jumping ∧
    (program false).Begins (language := Language) arriving byte.logical.root (openByte 2) ∧
    (program false).Writes (language := Language) (openByte 2) byte byteValue (writtenByte 2) ∧
    (program false).BeginExhausted (language := Language) (writtenByte 2) word.logical.root ∧
    exec.step (program false) jumping completed ∧
    ¬ exec.step (program false) jumping {jumping with resourceExhausted := true} := by
  refine ⟨handoff false, reach_jump false, begin_byte 2 (.inr rfl), write_byte 2 (.inr rfl),
    by with_unfolding_all exact fragmented (by decide) rfl rfl rfl, ?_, ?_⟩
  · with_unfolding_all exact ⟨rfl, ⟨_, rfl, rfl, _, _, _, rfl, rfl, rfl, fits_together⟩, [], rfl⟩
  · rintro ⟨_, tick, _⟩
    obtain ⟨_, partialState, raw⟩ := tick
    exact (jump_outcome false rfl raw).2 ⟨completed, fits_together⟩

private theorem blocked_transfer : (program true).ParamOutcome (language := Language) params [byteValue, wordValue]
    (openByte 2) (writtenByte 2) true := by
  have wrote : (program true).Writes (language := Language) (openByte 2) byte byteValue (writtenByte 2) := by
    with_unfolding_all exact write_byte 2 (.inr rfl)
  have blocked : (program true).BeginExhausted (language := Language) (writtenByte 2) word.logical.root := by
    with_unfolding_all exact fragmented (by decide) rfl rfl rfl
  constructor
  · exact .cons rfl wrote (.exhausted blocked)
  · rintro ⟨_, path⟩
    cases path with
    | cons first stores tail =>
      obtain rfl := first
      cases Option.some.inj (wrote.2.2.1.symm.trans stores.2.2.1)
      cases tail with
      | cons beginWord _ _ => exact blocked.unavailable ⟨_, beginWord⟩

/-- A live first destination forces the u8 write at 2. The later reservation
fails for every placement. Execution terminates at the original jump with the
entire register state restored, including the bytes written by that prefix. -/
theorem rollback_after_write :
    (program true).HandoffWF ∧ exec.finite (program true) forced ∧
    (writtenByte 2).bytes ≠ forced.bytes ∧
    exec.step (program true) forced {forced with resourceExhausted := true} ∧
    exec.result (program true) {forced with resourceExhausted := true} .resourceExhausted ∧
    ∀ next, exec.step (program true) forced next → next = {forced with resourceExhausted := true} := by
  refine ⟨handoff true, finite_next (reach_jump true) ?_, ?_, ?_, by with_unfolding_all rfl, ?_⟩
  · with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, _, ⟨by decide, .inr ⟨rfl, _, rfl, .inr
        ⟨by decide, ⟨0⟩, 2, ⟨1⟩, _, rfl, rfl⟩⟩⟩, rfl⟩, [], rfl⟩
  · intro eq
    have contents := congrArg (fun m : Mem.Memory => m.blocks.head?.bind (fun b => b.2.bytes.head?)) eq
    have impossible : some (Mem.MByte.init 42 none) = some .uninit := by with_unfolding_all exact contents
    contradiction
  · with_unfolding_all exact ⟨rfl, ⟨rfl, writtenByte 2, _, rfl, rfl, _, _, _, rfl, rfl, rfl, blocked_transfer⟩, [], rfl⟩
  · intro next step
    have tick := step.2.1
    change (program true).Step (language := Language) forced next at tick
    unfold Interfaces.Memory.Step at tick
    rw [show forced.resourceExhausted = false from rfl] at tick
    simp only [Bool.false_eq_true, ite_false] at tick
    split at tick
    · exact tick.1
    · exact (blocked_transfer.2 ⟨next, jump_outcome true rfl tick⟩).elim

-- The register interpreters reach the same fragmented allocator through two
-- explicit begins and a store. Only the terminator encoding differs.
private def registerGraph {Term : Type} (ret : Term) :
    CFG (Sigma (SSAValue Language.operands.Ty)) (LocatedOp (SSAInst Lowering)) Term :=
  ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨origin, .beginLifetime backing⟩, ⟨origin, .beginLifetime byte.logical.root⟩,
    ⟨origin, .store byte.logical (.literal ⟨[42]⟩)⟩, ⟨origin, .beginLifetime word.logical.root⟩], ret, []⟩]⟩

private def ssa : SSA Lowering profile :=
  {project := mir.project, entry := mir.entry, signatures := mir.signatures, layouts
   functions := [(mir.entry, locals, registerGraph (.ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩)))]}

private def bir : BIR Lowering profile :=
  {project := mir.project, entry := mir.entry, signatures := mir.signatures, layouts, sites := #[]
   functions := [(mir.entry, locals, registerGraph (.ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩)))]}

private def atBegin : State := {writtenByte 2 with block := ⟨0⟩, instruction := 3}
private def halted : State := {atBegin with resourceExhausted := true}
private abbrev ssaExec := Interfaces.SSA.Execution (language := Language) (lang := Lowering) (profile := profile)
private abbrev birExec := Interfaces.BIR.Execution (language := Language) (lang := Lowering) (profile := profile)

theorem ssa_exhaustion : ssa.WF (language := Language) ∧ ssaExec.finite ssa atBegin ∧
    ssaExec.step ssa atBegin halted ∧ ssaExec.result ssa halted .resourceExhausted := by
  refine ⟨?_, ?_, ?_, by with_unfolding_all rfl⟩
  · letI := Language.typeDecidableEq
    exact (checkHandoff_iff _).mp (by with_unfolding_all decide)
  · refine ⟨start, by with_unfolding_all rfl,
      .cons (b := jumping) ?_ (.cons (b := forced) ?_ (.cons ?_ (.nil _)))⟩
    · with_unfolding_all exact ⟨rfl, ⟨_, rfl, _, ⟨nofun, .inr ⟨rfl, _, rfl, .inr
        ⟨by decide, ⟨0⟩, 4, ⟨0⟩, _, rfl, rfl⟩⟩⟩, rfl⟩, [], rfl⟩
    · with_unfolding_all exact ⟨rfl, ⟨_, rfl, _, ⟨by decide, .inr ⟨rfl, _, rfl, .inr
        ⟨by decide, ⟨0⟩, 2, ⟨1⟩, _, rfl, rfl⟩⟩⟩, rfl⟩, [], rfl⟩
    · with_unfolding_all
        refine ⟨rfl, ⟨_, rfl, byteValue, forced, {atBegin with instruction := 2}, rfl, ?_, rfl⟩, [], rfl⟩
        obtain ⟨valid, layout, _, stored⟩ := write_byte 2 (.inr rfl)
        exact ⟨valid, layout, rfl, stored⟩
  · with_unfolding_all
      refine ⟨rfl, ⟨rfl, atBegin, _, rfl, ?_⟩, [], rfl⟩
      exact fragmented (by decide) rfl rfl rfl

theorem bir_exhaustion : bir.WF (language := Language) ∧ birExec.finite bir atBegin ∧
    birExec.step bir atBegin halted ∧ birExec.result bir halted .resourceExhausted := by
  refine ⟨?_, ?_⟩
  · refine ⟨?_, by with_unfolding_all decide, by with_unfolding_all decide, by with_unfolding_all decide,
      fun _ h => (List.not_mem_nil h).elim, ?_⟩
    · rintro _ member block present _ _
      cases List.mem_singleton.mp member
      change block ∈ [_] at present
      cases List.mem_singleton.mp present
      contradiction
    · with_unfolding_all exact ssa_exhaustion.1.1
  · with_unfolding_all exact ssa_exhaustion.2

end Concrete.Memory.TransferControl
