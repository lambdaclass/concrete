import Concrete.Memory.Refinement

namespace Concrete.Memory.TransferControl

open Concrete.Interfaces ConcreteSpec.Core ConcreteSpec.StageInterface

-- A single live backing object leaves addresses 1, 2 and 3 available. Supply
-- its layout through the language interface so the fixture fits in two steps.
private def backingSize := 2^64 - 5
private def aggregateId : DeclId := ⟨(bindReturnControl slice0).entry.module, ⟨"suffix"⟩⟩
private def suffixRuns : List (Nat × List Mem.MByte) := [(0, [.init 42 none]), (4, [])]
private abbrev Language : CoreLanguage := {sliceLanguage slice0 with
  layout := fun env target ty => match ty with
    | .inr false => some ⟨backingSize, 1, .nil⟩
    | .inr true => some ⟨4, 1, .nil⟩
    | _ => (sliceLanguage slice0).layout env target ty
  literal := fun ty bytes => match ty with
    | .inr true => if bytes == [42] then some (.other aggregateId) else none
    | _ => (sliceLanguage slice0).literal ty bytes
  encodeOther := fun _ ty (v : DeclId) => match ty with
    | .inr true => if v == aggregateId then some suffixRuns else none
    | _ => none
  decodeOther := fun _ ty bytes => match ty with
    | .inr true => if bytes.head? == some (.init 42 none) then some aggregateId else none
    | _ => none}
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
  [(mir.entry, [(byte.logical.root, ⟨1, 1, .nil⟩), (word.logical.root, ⟨2, 2, .nil⟩), (backing, ⟨backingSize, 1, .nil⟩)])]
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

private theorem write_byte (base : Nat) (h : base = 1 ∨ base = 2) (ceiling : Nat := profile.memoryTarget.addressCeiling) :
    (program false).Writes (language := Language) (openByte base) byte byteValue (writtenByte base) ceiling := by
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
private theorem no_word_allocation {bytes : Mem.Memory} {contents : List Mem.MByte}
    (byteBlock : (⟨1⟩, {Mem.candidate ⟨0⟩ 2 1 1 with bytes := contents}) ∈ bytes.blocks)
    (backingBlock : (⟨0⟩, Mem.candidate ⟨0⟩ 4 backingSize 1) ∈ bytes.blocks)
    {space base block reservedBytes} :
    bytes.alloc profile.memoryTarget space base 2 2 ≠ .ok (block, reservedBytes) := by
  intro alloc
  unfold Mem.Memory.alloc at alloc
  dsimp only at alloc
  split at alloc
  · rename_i fits
    obtain rfl : space = ⟨0⟩ := List.mem_singleton.mp fits.2.2.2.2.2.2
    have aligned : base % 2 = 0 := fits.2.1
    have nonnull : base ≠ 0 := fits.2.2.2.2.1 (by decide : 0 < 2)
    have top : base + 2 < 2^64 := fits.2.2.1
    by_cases atByte : base = 2
    · subst base
      apply fits.2.2.2.2.2.1 _ byteBlock rfl
      exact ⟨rfl, (by decide : 0 < 1), (by decide : 0 < 2), (by decide : 2 < 2 + 2), (by decide : 2 < 2 + 1)⟩
    · apply fits.2.2.2.2.2.1 (⟨0⟩, Mem.candidate ⟨0⟩ 4 backingSize 1)
        backingBlock rfl
      refine ⟨rfl, by decide, (by decide : 0 < 2), ?_, ?_⟩
      · change 4 < base + 2
        omega
      · change base < 4 + (2^64 - 5)
        omega
  · contradiction

private theorem fragmented {m : M} {s : State}
    (fresh : word.logical.root ∉ s.live)
    (cls : m.classAt (language := Language) s.frame word.logical.root = some .materialized)
    (shape : m.layoutAt (language := Language) s.frame word.logical.root = some ⟨2, 2, .nil⟩)
    (bytes : s.bytes = (writtenByte 2).bytes) : m.BeginExhausted (language := Language) s word.logical.root := by
  refine ⟨⟨reserved s word.logical.root (2^64) 2 2, fresh, .inr ⟨cls, _, shape, .inr
    ⟨by decide, ⟨0⟩, 2^64, ⟨s.bytes.next⟩, _, ?_, rfl⟩⟩⟩, ?_⟩
  · change s.bytes.alloc _ _ _ _ _ _ = .ok (_, ⟨_ :: s.bytes.blocks, s.bytes.next + 1⟩)
    have bound : m.transferCeiling (language := Language) s [word.logical.root] = 2^64 + 4 := by
      unfold Interfaces.Memory.transferCeiling Interfaces.Memory.freshLayouts
      simp [Interfaces.Memory.freshLayoutsFrom, fresh, cls, shape]
      rfl
    rw [bound, bytes]
    with_unfolding_all rfl
  rintro ⟨_, _, ⟨hv, _⟩ | ⟨_, layout, hl, zero | placed⟩⟩
  · cases cls.symm.trans hv
  all_goals cases Option.some.inj (hl.symm.trans shape)
  · exact (by decide : (2 : Nat) ≠ 0) zero.1
  · obtain ⟨_, space, base, _, _, alloc, _⟩ := placed
    rw [bytes] at alloc
    exact no_word_allocation (by with_unfolding_all exact List.mem_cons_self ..)
      (by with_unfolding_all exact List.mem_cons_of_mem _ (List.mem_cons_self ..)) alloc

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

private theorem joint_space : (program false).ParamSpaceFits (language := Language) params arriving := by
  with_unfolding_all exact ⟨⟨0⟩, 1, ⟨1⟩, _, rfl, ⟨0⟩, 2, ⟨2⟩, _, rfl, trivial⟩

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
    exact Interfaces.Memory.ParamSpaceFits.not_exhausted joint_space (jump_outcome false rfl raw)

private theorem blocked_transfer : (program true).ParamOutcome (language := Language) params [byteValue, wordValue]
    (openByte 2) (writtenByte 2) true := by
  have wrote : (program true).Writes (language := Language) (openByte 2) byte byteValue (writtenByte 2) := by
    with_unfolding_all exact write_byte 2 (.inr rfl)
  have noWord : ¬ (program true).ParamSpaceFits (language := Language) [⟨u16Ty, word⟩] (writtenByte 2) := by
    rintro ⟨space, base, block, mem, alloc, _⟩
    exact no_word_allocation (by with_unfolding_all exact List.mem_cons_self ..)
      (by with_unfolding_all exact List.mem_cons_of_mem _ (List.mem_cons_self ..)) alloc
  constructor
  · exact .cons rfl wrote (.exhausted noWord)
  · let opened := reserved (writtenByte 2) word.logical.root (2^64) 2 2
    let limit := (program true).transferCeiling (language := Language) (openByte 2) (params.map fun p => p.2.logical.root)
    let stored := ((program true).writeValue (language := Language) opened word wordValue limit).getD opened
    refine ⟨⟨by with_unfolding_all decide, stored, .cons (stored := writtenByte 2) rfl (by with_unfolding_all exact write_byte 2 (.inr rfl) _) (.cons (started := opened) ?_ ?_ (.nil _))⟩, ?_⟩
    · with_unfolding_all exact ⟨by decide, .inr ⟨rfl, _, rfl, .inr
        ⟨by decide, ⟨0⟩, 2^64, ⟨2⟩, _, rfl, rfl⟩⟩⟩
    · with_unfolding_all
        refine ⟨⟨rfl, nofun⟩, fun _ => ⟨_, rfl, nofun, nofun⟩, rfl, rfl,
          fun _ => .inr ⟨_, [(0, [.init 42 none, .init 0 none])], rfl, rfl,
            fun _ => ⟨0, _, .head _, by decide⟩, ?_⟩⟩
        intro off bytes present
        cases List.mem_singleton.mp present
        exact ⟨by decide, by decide, rfl⟩
    · rintro ⟨space, base, block, mem, alloc, _⟩
      exact no_word_allocation (by with_unfolding_all exact List.mem_cons_self ..)
        (by with_unfolding_all exact List.mem_cons_of_mem _ (List.mem_cons_self ..)) alloc

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
    · have path := jump_outcome true rfl tick
      change (program true).WriteParams (language := Language) params [byteValue, wordValue] (openByte 2) next false at path
      cases path with
      | cons first stores tail =>
        obtain rfl := first
        have eq := Option.some.inj ((write_byte 2 (.inr rfl)).2.2.1.symm.trans stores.2.2.1)
        cases eq
        cases tail with
        | cons beginWord _ _ =>
          have blocked : (program true).BeginExhausted (language := Language) (writtenByte 2) word.logical.root :=
            fragmented (by decide) rfl rfl rfl
          exact (blocked.unavailable ⟨_, beginWord⟩).elim

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
    · with_unfolding_all exact ⟨ssa_exhaustion.1.1, ssa_exhaustion.1.2.2.2⟩
  · with_unfolding_all exact ssa_exhaustion.2

namespace TrailingParam

private inductive Fault where
  | type | immutable | tag
  deriving DecidableEq

private def virtualLocal : MemoryPlace _ u8Ty := ⟨⟨⟨3⟩, []⟩, .virtual⟩
private def declarations := locals ++ [(virtualLocal.logical.root, u8Ty)]
private def lastParam : Fault → Sigma (MemoryPlace Language.operands.Ty)
  | .type => ⟨u16Ty, ⟨⟨virtualLocal.logical.root, []⟩, .virtual⟩⟩
  | .immutable => ⟨u8Ty, virtualLocal⟩
  | .tag => ⟨u8Ty, {virtualLocal with storage := .materialized}⟩
private def lastArg : Fault → Sigma (MemoryRead _ Lowering.literal)
  | .type => ⟨u16Ty, .literal ⟨[42, 0]⟩⟩
  | _ => ⟨u8Ty, .literal ⟨[99]⟩⟩
private def jump (fault : Fault) := {edge with arguments := edge.arguments ++ [lastArg fault]}
private def first (fault : Fault) : MemoryBlock Lowering :=
  ⟨⟨0⟩, [], [⟨origin, .beginLifetime backing⟩, ⟨origin, .beginLifetime virtualLocal.logical.root⟩,
    ⟨origin, .bind virtualLocal (.literal ⟨[0]⟩)⟩], .jump (jump fault), []⟩
private def dest (fault : Fault) : MemoryBlock Lowering :=
  ⟨⟨1⟩, params ++ [lastParam fault], [], .ret (some ⟨u8Ty, .copy virtualLocal⟩), []⟩

-- The unreachable address operands force materialization of both scalar roots.
private def addressed : MIRInst Lowering := .op (opcode := .read) byte.logical ()
  (.cons (.literal (ty := u8Ty) ⟨[42]⟩) .nil) [] [⟨u8Ty, byte.logical⟩, ⟨u16Ty, word.logical⟩]
private def sourceMir : MIR Lowering profile :=
  {mir with mutableLocals := [(mir.entry, [virtualLocal.logical.root])]
            functions := [(mir.entry, declarations, ⟨⟨0⟩, #[
              ⟨⟨0⟩, [], [⟨origin, .bind virtualLocal.logical (.literal ⟨[0]⟩)⟩],
                .jump ⟨⟨1⟩, [⟨u8Ty, .literal ⟨[42]⟩⟩, ⟨u16Ty, .literal ⟨[42, 0]⟩⟩,
                  ⟨u8Ty, .literal ⟨[99]⟩⟩]⟩, []⟩,
              ⟨⟨1⟩, [⟨u8Ty, byte.logical⟩, ⟨u16Ty, word.logical⟩, ⟨u8Ty, virtualLocal.logical⟩],
                [], .ret (some ⟨u8Ty, .copy virtualLocal.logical⟩), []⟩,
              ⟨⟨2⟩, [], [⟨origin, addressed⟩], .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩]⟩)]}

private def physical (fault : Fault) : M :=
  let dead : MemoryBlock Lowering := ⟨⟨2⟩, [], [⟨origin, .op (opcode := .read) byte ()
    (.cons (.literal (ty := u8Ty) ⟨[42]⟩) .nil) [] [⟨u8Ty, byte⟩, ⟨u16Ty, word⟩]⟩],
    .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩
  {program false with
    mutableLocals := if fault = .immutable then [] else sourceMir.mutableLocals
    functions := [(mir.entry, declarations, ⟨⟨0⟩, #[first fault, dest fault, dead]⟩)]
    storage := [(mir.entry, List.map (fun decl => (decl.1, StorageClass.materialized)) locals ++ [(virtualLocal.logical.root, .virtual)])]}

private def zero : Language.operands.Val := .scalar (.int .u8 0)
private def replacement : Language.operands.Val := .scalar (.int .u8 99)
private def lastValue : Fault → Language.operands.Val
  | .type => wordValue
  | _ => replacement
private def openedLocal : State := {jumping.advance with live := virtualLocal.logical.root :: jumping.live}
private def atJump : State := {openedLocal.advance with
  initialized := [virtualLocal.logical.root], virtualValues := [(virtualLocal.logical.root, zero)]}
private def checkpoint : State := {atJump with block := ⟨1⟩, instruction := 0}

private theorem reaches (fault : Fault) : (physical fault).HandoffWF ∧ exec.finite (physical fault) atJump := by
  constructor
  · letI := Language.typeDecidableEq
    cases fault <;> exact (checkHandoff_iff _).mp (by with_unfolding_all decide)
  · refine ⟨start, by cases fault <;> with_unfolding_all rfl,
      .cons (b := jumping) ?_ (.cons (b := openedLocal) ?_ (.cons ?_ (.nil _)))⟩
    · cases fault <;> with_unfolding_all exact ⟨rfl, ⟨_, rfl, _, ⟨nofun, .inr ⟨rfl, _, rfl, .inr
        ⟨by decide, ⟨0⟩, 4, ⟨0⟩, _, rfl, rfl⟩⟩⟩, rfl⟩, [], rfl⟩
    · cases fault <;> with_unfolding_all exact ⟨rfl, ⟨_, rfl, _, ⟨by decide, .inl ⟨rfl, rfl⟩⟩, rfl⟩, [], rfl⟩
    · cases fault <;> with_unfolding_all exact ⟨rfl, ⟨_, rfl, zero, openedLocal, _, rfl,
        ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩, rfl⟩, [], rfl⟩

private theorem outcome (fault : Fault) {s : State} {mode}
    (raw : (physical fault).StepOutcome (language := Language) atJump s mode) :
    (physical fault).ParamOutcome (language := Language) (dest fault).params
      [byteValue, wordValue, lastValue fault] checkpoint s mode := by
  have transfer := raw.terminatorAt (language := Language) (block := first fault)
    (by cases fault <;> with_unfolding_all rfl) (by rfl)
  obtain ⟨values, read, block, hr, hb, _, result⟩ := transfer
  have args : some ([byteValue, wordValue, lastValue fault], atJump) = some (values, read) := by
    cases fault <;> with_unfolding_all exact hr
  cases args
  have lookup : some (dest fault) = some block := by cases fault <;> with_unfolding_all exact hb
  cases lookup
  exact result

private theorem stuck_of_rejected (fault : Fault)
    (reject : ∀ s, ¬ (physical fault).WriteParams (language := Language) (dest fault).params
      [byteValue, wordValue, lastValue fault] checkpoint s false) : exec.stuck (physical fault) atJump := by
  refine ⟨(reaches fault).2, ?_, ?_⟩
  · intro next step
    obtain ⟨s, mode, raw⟩ := Interfaces.Memory.Step.exists_outcome step.2.1
    cases mode
    · exact reject s (outcome fault raw)
    · exact Interfaces.Memory.ParamSpaceFits.not_exhausted (by cases fault <;> with_unfolding_all exact joint_space) (outcome fault raw)
  · intro o obs
    have impossible : none = some o := by cases fault <;> with_unfolding_all exact obs
    contradiction

private theorem uncons {m : M} {ty} {p : MemoryPlace _ ty} {ps v vs q next}
    (path : m.WriteParams (language := Language) (⟨ty, p⟩ :: ps) (v :: vs) q next false) :
    ∃ started stored, (if q.live.contains p.logical.root then started = q else m.Begins (language := Language) q p.logical.root started) ∧
      m.Writes (language := Language) started p v stored ∧ m.WriteParams (language := Language) ps vs stored next false := by
  cases path with
  | cons opened write tail => exact ⟨_, _, opened, write, tail⟩

private theorem rejected_annotation (fault : Fault) (bad : fault = .type ∨ fault = .tag) (s : State) :
    ¬ (physical fault).WriteParams (language := Language) (dest fault).params
      [byteValue, wordValue, lastValue fault] checkpoint s false := by
  have invalid (q : State) : (physical fault).validPlace (language := Language) q (lastParam fault).2 = false := by
    by_cases frame : q.frame = mir.entry
    · unfold Interfaces.Memory.validPlace
      rw [frame]
      rcases bad with rfl | rfl <;> with_unfolding_all rfl
    · have absent : (physical fault).classAt (language := Language) q.frame (lastParam fault).2.logical.root = none := by
        simp [Interfaces.Memory.classAt, physical, AList.lookup, Ne.symm frame, bind, Option.bind]
      simp [Interfaces.Memory.validPlace, absent]
  intro path
  obtain ⟨_, _, _, _, tail⟩ := uncons path
  obtain ⟨_, _, _, _, tail⟩ := uncons tail
  obtain ⟨started, _, _, write, _⟩ := uncons tail
  exact Bool.noConfusion ((invalid started).symm.trans write.1.1)

/-- The last edge argument and parameter both say u16, but its virtual root is declared u8. -/
theorem mixed_type_stuck : (physical .type).HandoffWF ∧ exec.stuck (physical .type) atJump :=
  ⟨(reaches .type).1, stuck_of_rejected .type (rejected_annotation .type (.inl rfl))⟩

-- Starting a nonempty materialized parameter changes only its live entry,
-- backing range and allocator state. The chosen address can remain arbitrary.
private theorem materialized_begin {m : M} {q started : State} {root layout}
    (opened : m.Begins (language := Language) q root started)
    (cls : m.classAt q.frame root = some .materialized)
    (shape : m.layoutAt q.frame root = some layout) (nonempty : layout.size ≠ 0) :
    ∃ range bytes, started = {q with
      live := root :: q.live, bytes, materialized := (root, range) :: q.materialized} := by
  rcases opened.2 with ⟨virtual, _⟩ | ⟨_, _, layoutEq, zero | placed⟩
  · cases cls.symm.trans virtual
  all_goals cases Option.some.inj (shape.symm.trans layoutEq)
  · exact (nonempty zero.1).elim
  · obtain ⟨_, _, _, _, bytes, _, eq⟩ := placed
    exact ⟨_, bytes, eq⟩

-- The write may select any backing range and write any resulting bytes.
-- Its frame update still initializes just the fresh root.
private theorem materialized_write {m : M} {q stored : State} {ty value} {p : MemoryPlace _ ty}
    (write : m.Writes (language := Language) q p value stored)
    (cls : p.storage = .materialized) (nonzero : m.zeroRoot (language := Language) q.frame p.logical.root = false)
    (fresh : q.initialized.contains p.logical.root = false) (plain : q.addressIds = [])
    (whole : p.logical.path = []) (moved : q.movedFields = []) :
    ∃ bytes, stored = {q with bytes, initialized := p.logical.root :: q.initialized} := by
  have put := write.2.2.1
  simp only [Interfaces.Memory.writeValue, Interfaces.Memory.markWritten, Interfaces.Memory.storeRuns, cls, whole, List.isEmpty_nil, fresh, nonzero,
    moved, List.filter_nil, Bool.false_eq_true, ite_true, ite_false] at put
  split at put
  · contradiction
  obtain ⟨owner, _, put⟩ := Option.bind_eq_some_iff.mp put
  obtain ⟨_, _, put⟩ := Option.bind_eq_some_iff.mp put
  obtain ⟨_, _, put⟩ := Option.bind_eq_some_iff.mp put
  obtain ⟨bytes, _, eq⟩ := Option.bind_eq_some_iff.mp put
  cases eq
  refine ⟨bytes, ?_⟩
  revert plain moved
  cases q with
  | mk frame => cases frame; rintro rfl rfl; rfl

private theorem immutable_rejected (s : State) :
    ¬ (physical .immutable).WriteParams (language := Language) (dest .immutable).params
      [byteValue, wordValue, lastValue .immutable] checkpoint s false := by
  intro path
  obtain ⟨_, _, open₁, write₁, tail⟩ := uncons path
  obtain ⟨_, _, open₂, write₂, tail⟩ := uncons tail
  obtain ⟨_, stored, open₃, write₃, _⟩ := uncons tail
  obtain ⟨r₁, b₁, rfl⟩ := materialized_begin open₁ rfl (layout := ⟨1, 1, .nil⟩) rfl (by decide)
  obtain ⟨b₁', rfl⟩ := materialized_write write₁ rfl rfl rfl rfl rfl rfl
  obtain ⟨r₂, b₂, rfl⟩ := materialized_begin open₂ rfl (layout := ⟨2, 2, .nil⟩) rfl (by decide)
  obtain ⟨b₂', rfl⟩ := materialized_write write₂ rfl rfl rfl rfl rfl rfl
  obtain rfl := open₃
  have denied : none = some stored := by with_unfolding_all exact write₃.2.2.1
  contradiction

/-- Both reservations fit, but replacing the initialized immutable u8 is refused. -/
theorem immutable_stuck : (physical .immutable).HandoffWF ∧ exec.stuck (physical .immutable) atJump :=
  ⟨(reaches .immutable).1, stuck_of_rejected .immutable immutable_rejected⟩

private abbrev source := Interfaces.MIR.Execution (language := Language) (lang := Lowering) (profile := profile)
private abbrev logical := sourceMir.logicalView (language := Language)

-- Every root in the logical view is virtual, so a successful list of bindings
-- has a unique successor. Only the physical view chooses addresses.
private theorem logical_unique {ps vs q s t}
    (left : logical.WriteParams (language := Language) ps vs q s false)
    (right : logical.WriteParams (language := Language) ps vs q t false) : s = t := by
  have opens {q s : State} {root} (h : logical.Begins (language := Language) q root s) :
      s = {q with live := root :: q.live} := by
    rcases h.2 with ⟨_, eq⟩ | ⟨cls, _⟩
    · exact eq
    · obtain ⟨classes, table, entry⟩ := Option.bind_eq_some_iff.mp cls
      have tableMem := AList.lookup_mem table
      change (_, classes) ∈ [_] at tableMem
      cases congrArg Prod.snd (List.mem_singleton.mp tableMem)
      have member := AList.lookup_mem entry
      change (_, StorageClass.materialized) ∈ [_, _, _, _] at member
      simp at member
  induction ps generalizing vs q s t with
  | nil => cases left; cases right; rfl
  | cons p ps ih =>
    obtain ⟨ty, place⟩ := p
    cases vs with
    | nil => cases left
    | cons v vs =>
      obtain ⟨a, b, startA, writeA, restA⟩ := uncons left
      obtain ⟨c, d, startC, writeC, restC⟩ := uncons right
      have eq : a = c := by
        split at startA
        · rename_i live
          rw [ite_eq_left live] at startC
          exact startA.trans startC.symm
        · rename_i fresh
          rw [ite_eq_right fresh] at startC
          exact (opens startA).trans (opens startC).symm
      cases eq
      cases Option.some.inj (writeA.2.2.1.symm.trans writeC.2.2.1)
      exact ih restA restC

private def logicalInit : State := {start with
  live := [virtualLocal.logical.root], initialized := [virtualLocal.logical.root], virtualValues := [(virtualLocal.logical.root, zero)]}
private def logicalJump := logicalInit.advance
private def logicalEntry : State := {logicalJump with block := ⟨1⟩, instruction := 0}
private def logicalStore (q : State) (root : PlaceId) (v : Language.operands.Val) : State :=
  {q with live := root :: q.live, initialized := root :: q.initialized, virtualValues := (root, v) :: q.virtualValues}
private def logicalByte := logicalStore logicalEntry byte.logical.root byteValue
private def logicalWord := logicalStore logicalByte word.logical.root wordValue
private def returned : State := {logicalWord with virtualValues :=
  (virtualLocal.logical.root, replacement) :: AList.erase logicalWord.virtualValues virtualLocal.logical.root}

private theorem initial_binding : logical.WriteParams (language := Language) [⟨u8Ty, virtualLocal⟩] [zero] start logicalInit := by
  with_unfolding_all
    exact .cons (started := {start with live := [virtualLocal.logical.root]})
      ⟨nofun, .inl ⟨rfl, rfl⟩⟩ ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩ (.nil _)

private def logicalParams : List (Sigma (MemoryPlace Language.operands.Ty)) :=
  [⟨u8Ty, {byte with storage := .virtual}⟩, ⟨u16Ty, {word with storage := .virtual}⟩, ⟨u8Ty, virtualLocal⟩]
private theorem jump_bindings : logical.WriteParams (language := Language) logicalParams
    [byteValue, wordValue, replacement] logicalEntry returned := by
  with_unfolding_all
    refine .cons (started := {logicalEntry with live := byte.logical.root :: logicalEntry.live}) (stored := logicalByte)
      ⟨by decide, .inl ⟨rfl, rfl⟩⟩ ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩ ?_
    refine .cons (started := {logicalByte with live := word.logical.root :: logicalByte.live}) (stored := logicalWord)
      ⟨by decide, .inl ⟨rfl, rfl⟩⟩ ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩ ?_
    exact .cons rfl ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩ (.nil _)

private theorem bind_iff (q : State) : source.step sourceMir start q ↔ q = logicalJump := by
  constructor
  · intro step
    obtain ⟨v, read, stored, hr, writes, rfl⟩ := step.2.1.2
    have eq : some (zero, start) = some (v, read) := by with_unfolding_all exact hr
    cases eq
    exact congrArg MemoryRegisters.advance (logical_unique writes initial_binding)
  · rintro rfl
    with_unfolding_all exact ⟨rfl, ⟨rfl, zero, start, logicalInit, rfl, initial_binding, rfl⟩, [], rfl⟩

private theorem jump_iff (q : State) : source.step sourceMir logicalJump q ↔ q = returned := by
  let entry := logical.functions[0].2.2.blocks[0]'(by with_unfolding_all decide)
  let dst := logical.functions[0].2.2.blocks[1]'(by with_unfolding_all decide)
  constructor
  · intro step
    obtain ⟨block, hb, _, tick⟩ := step.2.1
    have lookup : some entry = some block := by with_unfolding_all exact hb
    cases lookup
    rw [show entry.ending = .jump (jump .tag) by with_unfolding_all rfl] at tick
    obtain ⟨vs, read, block, hr, hb, _, writes⟩ := tick
    have args : some ([byteValue, wordValue, replacement], logicalJump) = some (vs, read) := by with_unfolding_all exact hr
    cases args
    have found : some dst = some block := by with_unfolding_all exact hb
    cases found
    with_unfolding_all exact logical_unique writes jump_bindings
  · rintro rfl
    with_unfolding_all exact ⟨rfl, ⟨entry, rfl, rfl, _, _, dst, rfl, rfl, rfl, jump_bindings⟩, [], rfl⟩

private theorem source_safe : ProgramSafe source sourceMir := by
  have result : source.result sourceMir returned (.returned 99 ByteArray.empty) := by with_unfolding_all rfl
  intro q stuck
  have reachable := finite_invariant source sourceMir (fun p => p = start ∨ p = logicalJump ∨ p = returned)
    (fun _ initial => .inl (by with_unfolding_all exact initial)) (by
      rintro _ next (rfl | rfl | rfl) tick
      · exact .inr (.inl ((bind_iff _).mp tick))
      · exact .inr (.inr ((jump_iff _).mp tick))
      · exact (source.terminal _ _ _ result _ tick).elim) stuck.1
  rcases reachable with rfl | rfl | rfl
  · exact stuck.2.1 _ ((bind_iff _).mpr rfl)
  · exact stuck.2.1 _ ((jump_iff _).mpr rfl)
  · exact stuck.2.2 _ result

/-- Corrupting the mutable virtual parameter's storage tag passes the handoff
check. The MIR returns 99, but the reachable stuck jump violates target safety
in the behavior clause of the frozen lowering contract. -/
theorem corrupted_tag_rejected :
    sourceMir.logicalView.HandoffWF ∧ (physical .tag).HandoffWF ∧
    (physical .tag).mutableLocals = sourceMir.mutableLocals ∧ ProgramSafe source sourceMir ∧
    source.observes sourceMir (.returned 99 ByteArray.empty) ∧
    exec.stuck (physical .tag) atJump ∧ ¬ LowersMemory (language := Language) sourceMir (physical .tag) := by
  letI := Language.typeDecidableEq
  have stuck := stuck_of_rejected .tag (rejected_annotation .tag (.inr rfl))
  refine ⟨(checkHandoff_iff _).mp (by with_unfolding_all decide), (reaches .tag).1, rfl, source_safe,
    ⟨returned, ⟨start, by with_unfolding_all rfl,
      .cons ((bind_iff _).mpr rfl) (.cons ((jump_iff _).mpr rfl) (.nil _))⟩, by with_unfolding_all rfl⟩,
    stuck, ?_⟩
  intro contract
  obtain ⟨_, _, _, _, _, _, _, _, refinement, _⟩ := contract
  exact (refinement source_safe).2.2 atJump stuck

end TrailingParam


namespace SingleParam

-- Every fixture jumps with the same literal. A valid outcome must complete
-- the single binding at the transfer ceiling.
private def jump : CFGEdge (Sigma (MemoryRead _ Lowering.literal)) :=
  ⟨⟨1⟩, [⟨u8Ty, .literal ⟨[42]⟩⟩]⟩

private def first (body : List (LocatedOp (MemoryInst Lowering))) : MemoryBlock Lowering :=
  ⟨⟨0⟩, [], body, .jump jump, []⟩
private def last (p : MemoryPlace _ u8Ty) : MemoryBlock Lowering :=
  ⟨⟨1⟩, [⟨u8Ty, p⟩], [], .ret (some ⟨u8Ty, .copy byte⟩), []⟩

private theorem stuck_at_jump {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {m : Interfaces.Memory lang profile} {q : MemoryRegisters language profile}
    {edge : CFGEdge (Sigma (MemoryRead _ lang.literal))} {entry dst : MemoryBlock lang} {values}
    (reachable : (Interfaces.Memory.Execution (language := language)).finite m q)
    (lookup : m.blockAt q = some entry) (pc : entry.body[q.instruction]? = none)
    (ending : entry.ending = .jump edge) (args : m.readOperands edge.arguments q = some (values, q))
    (target : m.blockAt {q with block := edge.target} = some dst)
    (invalid : ∀ ceiling next, ¬ m.WriteParams dst.params values
      {q with block := edge.target, instruction := 0} next false ceiling)
    (running : m.terminalObservation q = none) :
    (Interfaces.Memory.Execution (language := language)).stuck m q := by
  refine ⟨reachable, ?_, ?_⟩
  · intro next step
    obtain ⟨s, mode, raw⟩ := Interfaces.Memory.Step.exists_outcome step.2.1
    have transfer := raw.terminatorAt lookup pc
    rw [ending] at transfer
    obtain ⟨vs, read, block, hr, hb, _, outcome⟩ := transfer
    cases Option.some.inj (args.symm.trans hr)
    cases Option.some.inj (target.symm.trans hb)
    cases mode
    · exact invalid _ s outcome
    · obtain ⟨_, valid⟩ := outcome.valid_of_exhausted.2
      exact invalid _ _ valid
  · intro o obs
    obtain ⟨_, observed, _⟩ := Option.bind_eq_some_iff.mp obs
    cases running.symm.trans observed

-- Byte 2 is already occupied. The u16 and a four-byte aggregate both need
-- widened placement. The two graphs differ only in the last parameter's tag.
private abbrev suffixTy : Language.operands.Ty := .inr true
private def lastPlace (tag : StorageClass) : MemoryPlace _ suffixTy := ⟨⟨⟨3⟩, []⟩, tag⟩
private def suffixEdge : CFGEdge (Sigma (MemoryRead _ Lowering.literal)) :=
  {edge with arguments := edge.arguments ++ [⟨suffixTy, .literal ⟨[42]⟩⟩]}
private def suffixEntry : MemoryBlock Lowering := {entryBlock true with ending := .jump suffixEdge}
private def suffixDest (tag : StorageClass) : MemoryBlock Lowering :=
  {destination with params := params ++ [⟨suffixTy, lastPlace tag⟩]}
private def suffixProgram (tag : StorageClass) : M :=
  let decls := locals ++ [(⟨3⟩, suffixTy)]
  {program true with functions := [(mir.entry, decls, ⟨⟨0⟩, #[suffixEntry, suffixDest tag]⟩)]
                     storage := [(mir.entry, List.map (fun d => (d.1, .materialized)) decls)]
                     layouts := [(mir.entry, layouts[0].2 ++ [(⟨3⟩, ⟨4, 1, .nil⟩)])]}
private def suffixValues : List Language.operands.Val := [byteValue, wordValue, .other aggregateId]

private theorem suffix_no_space (tag : StorageClass) :
    ¬ (suffixProgram tag).ParamSpaceFits (language := Language) (suffixDest tag).params (openByte 2) := by
  rintro ⟨space, base, block, mem, alloc, _⟩
  exact no_word_allocation (by with_unfolding_all exact List.mem_cons_self ..)
    (by with_unfolding_all exact List.mem_cons_of_mem _ (List.mem_cons_self ..)) alloc

private theorem reach_suffix (tag : StorageClass) : exec.finite (suffixProgram tag) forced := by
  refine ⟨start, by cases tag <;> with_unfolding_all rfl, .cons (b := jumping) ?_ (.cons ?_ (.nil _))⟩
  · cases tag <;> with_unfolding_all exact ⟨rfl, ⟨_, rfl, _, ⟨nofun, .inr ⟨rfl, _, rfl, .inr
      ⟨by decide, ⟨0⟩, 4, ⟨0⟩, _, rfl, rfl⟩⟩⟩, rfl⟩, [], rfl⟩
  · cases tag <;> with_unfolding_all exact ⟨rfl, ⟨_, rfl, _, ⟨by decide, .inr ⟨rfl, _, rfl, .inr
      ⟨by decide, ⟨0⟩, 2, ⟨1⟩, _, rfl, rfl⟩⟩⟩, rfl⟩, [], rfl⟩

private def opened := reserved (writtenByte 2) word.logical.root (2^64) 2 2
private def stored : State := {opened with
    initialized := word.logical.root :: opened.initialized
    bytes := ⟨(⟨2⟩, {Mem.candidate ⟨0⟩ (2^64) 2 2 with bytes := [.init 42 none, .init 0 none]}) ::
      (writtenByte 2).bytes.blocks, 3⟩}
private def lastRoot := lastPlace .materialized
private def lastOpen := reserved stored lastRoot.logical.root (2^64 + 2) 4 1
private def lastDone : State := {lastOpen with
    initialized := lastRoot.logical.root :: lastOpen.initialized
    bytes := ⟨(⟨3⟩, {Mem.candidate ⟨0⟩ (2^64 + 2) 4 1 with bytes := [.init 42 none, .uninit, .uninit, .uninit]}) ::
      stored.bytes.blocks, 4⟩}

private theorem suffixWrite : (suffixProgram .materialized).Writes (language := Language)
    lastOpen lastRoot (.other aggregateId) lastDone (2^64 + 8) := by
  with_unfolding_all
    refine ⟨⟨rfl, nofun⟩, fun _ => ⟨_, rfl, nofun, nofun⟩, rfl, rfl,
      fun _ => .inr ⟨_, suffixRuns, rfl, rfl,
        fun _ => ⟨0, [.init 42 none], by decide, by decide⟩, ?_⟩⟩
    intro off bytes present
    rcases List.mem_cons.mp present with eq | tail
    · cases eq; exact ⟨by decide, by decide, rfl⟩
    · cases List.mem_singleton.mp tail; exact ⟨by decide, nofun, rfl⟩

private theorem suffix_valid : (suffixProgram .materialized).ParamsValid (language := Language)
    (suffixDest .materialized).params suffixValues (openByte 2) := by
  refine ⟨by with_unfolding_all decide, ?_⟩
  change ∃ next, (suffixProgram .materialized).WriteParams (language := Language)
    (suffixDest .materialized).params suffixValues (openByte 2) next false (2^64 + 8)
  refine ⟨lastDone, .cons (stored := writtenByte 2) rfl
    (by with_unfolding_all exact write_byte 2 (.inr rfl) _) (.cons (started := opened) (stored := stored) ?_ ?_
      (.cons (started := lastOpen) ?_ suffixWrite (.nil _)))⟩
  · with_unfolding_all exact ⟨by decide, .inr ⟨rfl, _, rfl, .inr
      ⟨by decide, ⟨0⟩, 2^64, ⟨2⟩, _, rfl, rfl⟩⟩⟩
  · with_unfolding_all
      refine ⟨⟨rfl, nofun⟩, fun _ => ⟨_, rfl, nofun, nofun⟩, rfl, rfl,
        fun _ => .inr ⟨_, [(0, [.init 42 none, .init 0 none])], rfl, rfl,
          fun _ => ⟨0, _, .head _, by decide⟩, ?_⟩⟩
      intro off bytes present
      cases List.mem_singleton.mp present
      exact ⟨by decide, by decide, rfl⟩
  · with_unfolding_all exact ⟨by decide, .inr ⟨rfl, _, rfl, .inr
      ⟨by decide, ⟨0⟩, 2^64 + 2, ⟨3⟩, _, rfl, rfl⟩⟩⟩

/-- At the same exhausted rollback checkpoint the correct final parameter
exhausts. Corrupting just its tag leaves the reachable jump stuck. -/
theorem invalid_suffix_at_exhaustion :
    (suffixProgram .virtual).HandoffWF ∧
    ¬ (suffixProgram .virtual).ParamSpaceFits (language := Language) (suffixDest .virtual).params (openByte 2) ∧
    exec.stuck (suffixProgram .virtual) forced ∧
    exec.observes (suffixProgram .materialized) .resourceExhausted := by
  letI := Language.typeDecidableEq
  refine ⟨(checkHandoff_iff _).mp (by with_unfolding_all decide), suffix_no_space _, ?_, ?_⟩
  · apply stuck_at_jump (language := Language) (m := suffixProgram .virtual) (q := forced)
      (edge := suffixEdge) (entry := suffixEntry) (dst := suffixDest .virtual)
      (values := suffixValues) (reach_suffix _) (by with_unfolding_all rfl)
      (by rfl) rfl (by with_unfolding_all rfl) (by with_unfolding_all rfl) _ (by with_unfolding_all rfl)
    intro ceiling next path
    cases path with
    | cons _ _ rest =>
      cases rest with
      | cons _ _ last =>
        cases last with
        | cons _ write _ =>
          obtain ⟨cls, _⟩ := decide_eq_true_iff.mp write.1.1
          obtain ⟨classes, table, slot⟩ := Option.bind_eq_some_iff.mp cls
          have member := AList.lookup_mem table
          cases congrArg Prod.snd (List.mem_singleton.mp member)
          cases (by with_unfolding_all exact slot : some StorageClass.materialized = some .virtual)
  · obtain ⟨tick, result⟩ := Interfaces.Memory.jump_exhausted_of_no_space (language := Language)
      (memory := suffixProgram .materialized) (q := forced) (entry := suffixEntry)
      (edge := suffixEdge) (dst := suffixDest .materialized) (read := forced) (values := suffixValues)
      rfl (by with_unfolding_all rfl) rfl rfl (by with_unfolding_all rfl) (by with_unfolding_all rfl)
      rfl suffix_valid (suffix_no_space _)
    exact ⟨_, finite_next (reach_suffix _) tick, result⟩

-- validPlace reads the frame's declarations. A begin may allocate anywhere,
-- but its successor always retains that frame.
private theorem invalid_root {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {m : Interfaces.Memory lang profile} {q : MemoryRegisters language profile}
    {ty} {p : MemoryPlace _ ty} {v}
    (invalid : m.validPlace q p = false) : ∀ ceiling next, ¬ m.WriteParams [⟨ty, p⟩] [v] q next false ceiling := by
  intro ceiling next path
  cases path with
  | cons opened write tail =>
    rename_i started stored
    have frame : started.frame = q.frame := by
      split at opened
      · cases opened; rfl
      · rcases opened.2 with ⟨_, rfl⟩ | ⟨_, _, _, ⟨_, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩ <;> rfl
    have valid := write.1.1
    unfold Interfaces.Memory.validPlace at valid invalid
    rw [frame, invalid] at valid
    contradiction

private abbrev u32Ty : Language.operands.Ty := .inl (.scalar (.int .u32))
private def wrong : MemoryPlace _ u8Ty := ⟨⟨⟨3⟩, []⟩, .materialized⟩
private def opening : List (LocatedOp (MemoryInst Lowering)) := [⟨origin, MemoryInst.beginLifetime (lang := Lowering) backing⟩]
private def threeBytes (p : MemoryPlace _ u8Ty) : M :=
  let decls := locals ++ [(⟨3⟩, u32Ty)]
  {program false with functions := [(mir.entry, decls, ⟨⟨0⟩, #[first opening, last p]⟩)]
                      storage := [(mir.entry, List.map (fun d => (d.1, .materialized)) decls)]
                      layouts := [(mir.entry, layouts[0].2 ++ [(⟨3⟩, ⟨4, 4, .nil⟩)])]}

private theorem at_jump (p : MemoryPlace _ u8Ty) : exec.finite (threeBytes p) jumping := by
  refine ⟨start, by with_unfolding_all rfl, .cons ?_ (.nil _)⟩
  with_unfolding_all exact ⟨rfl, ⟨_, rfl, _, ⟨nofun, .inr ⟨rfl, _, rfl, .inr
    ⟨by decide, ⟨0⟩, 4, ⟨0⟩, _, rfl, rfl⟩⟩⟩, rfl⟩, [], rfl⟩

/-- Three free bytes suffice for the real u8. Redirecting its parameter to a
declared u32 passes handoff, but the reachable jump has no successor or result. -/
theorem wrong_u32_root :
    (threeBytes wrong).HandoffWF ∧ exec.stuck (threeBytes wrong) jumping ∧
    (threeBytes byte).ParamSpaceFits (language := Language) [⟨u8Ty, byte⟩] arriving ∧
    exec.observes (threeBytes byte) (.returned 42 ByteArray.empty) := by
  letI := Language.typeDecidableEq
  refine ⟨(checkHandoff_iff _).mp (by with_unfolding_all decide), ?_,
    by with_unfolding_all exact ⟨⟨0⟩, 1, ⟨1⟩, _, rfl, trivial⟩, ?_⟩
  · apply stuck_at_jump (language := Language) (m := threeBytes wrong) (q := jumping)
      (edge := jump) (entry := first opening) (dst := last wrong) (values := [byteValue])
      (at_jump wrong) (by with_unfolding_all rfl) (by rfl) rfl (by with_unfolding_all rfl)
      (by with_unfolding_all rfl) _ (by with_unfolding_all rfl)
    exact invalid_root (by with_unfolding_all rfl)
  · refine ⟨writtenByte 1, finite_next (at_jump byte) ?_, by with_unfolding_all rfl⟩
    with_unfolding_all exact ⟨rfl, ⟨_, rfl, rfl, _, _, _, rfl, rfl, rfl,
      .cons (begin_byte 1 (.inl rfl)) (write_byte 1 (.inl rfl)) (.nil _)⟩, [], rfl⟩

private def misaligned (align : Nat) : M :=
  {program false with functions := [(mir.entry, [(byte.logical.root, u8Ty)], ⟨⟨0⟩, #[first [], last byte]⟩)]
                      storage := [(mir.entry, [(byte.logical.root, .materialized)])]
                      layouts := [(mir.entry, [(byte.logical.root, ⟨1, align, .nil⟩)])]}

private abbrev Plain := sliceLanguage slice0
private def atParam : MemoryRegisters Plain profile := {start with block := jump.target}

private theorem no_begin (ceiling : Nat) (next : MemoryRegisters Plain profile) :
    ¬ (misaligned 3).Begins atParam byte.logical.root next ceiling := by
  rintro ⟨_, ⟨virtual, _⟩ | ⟨_, layout, found, zero | placed⟩⟩
  · cases virtual
  all_goals have eq : some (⟨1, 3, .nil⟩ : Layout) = some layout := by with_unfolding_all exact found
  all_goals cases eq
  · exact Nat.one_ne_zero zero.1
  · obtain ⟨_, _, _, _, _, alloc, _⟩ := placed
    unfold Mem.Memory.alloc at alloc
    dsimp only at alloc
    split at alloc
    · rename_i fits
      exact (by decide : ¬ IsPow2 3) fits.1
    · contradiction

/-- Both the standalone handoff and execution reject the malformed layout. -/
theorem alignment_three_stuck :
    ¬ (misaligned 3).HandoffWF ∧ (Interfaces.Memory.Execution (language := Plain)).stuck (misaligned 3) start ∧
    ¬ (misaligned 3).BeginExhausted atParam byte.logical.root := by
  letI := Plain.typeDecidableEq
  refine ⟨fun h => (by with_unfolding_all decide : checkHandoff (misaligned 3) ≠ true) ((checkHandoff_iff _).mpr h), ?_, ?_⟩
  · apply stuck_at_jump (language := Plain) (m := misaligned 3) (q := start)
      (edge := jump) (entry := first []) (dst := last byte) (values := [byteValue])
      ⟨start, by with_unfolding_all rfl, .nil _⟩ (by with_unfolding_all rfl) (by rfl) rfl
      (by with_unfolding_all rfl) (by with_unfolding_all rfl) _ (by with_unfolding_all rfl)
    intro ceiling next path
    cases path with
    | cons opened _ _ => exact no_begin _ _ opened
  · rintro ⟨⟨next, began⟩, _⟩
    exact no_begin _ next began


/-- A one-byte object cannot have alignment 2^64, even at a handoff
whose jump would otherwise be able to widen the address space. -/
theorem alignment_ceiling_refused : ¬ (misaligned (2^64)).HandoffWF := by
  letI := Plain.typeDecidableEq
  intro h
  exact (by with_unfolding_all decide : checkHandoff (misaligned (2^64)) ≠ true) ((checkHandoff_iff _).mpr h)

-- The unused local has a legal layout that cannot be placed in empty target
-- memory. Both source and healthy target return 42 from a one-byte parameter.
-- In the corrupted target only the parameter's root changes.
private abbrev Huge : CoreLanguage := {sliceLanguage slice0 with
  layout := fun env target ty => match ty with
    | .inr false => some ⟨2^64 - 1, 1, .nil⟩
    | _ => (sliceLanguage slice0).layout env target ty}
private abbrev source := Interfaces.MIR.Execution (language := Huge) (lang := Lowering) (profile := profile)
private abbrev target := Interfaces.Memory.Execution (language := Huge) (lang := Lowering) (profile := profile)
private abbrev registers := MemoryRegisters Huge profile
private def unused : MemoryPlace _ u8Ty := ⟨⟨⟨1⟩, []⟩, .materialized⟩
private def decls : LocalTypes Huge.operands.Ty := [(byte.logical.root, u8Ty), (unused.logical.root, .inr false)]
private def addressByte : MIRInst Lowering := .op (opcode := .read) byte.logical ()
  (.cons (.literal (ty := u8Ty) ⟨[42]⟩) .nil) [] [⟨u8Ty, byte.logical⟩]
private def sourceMir : MIR Lowering profile :=
  {mir with functions := [(mir.entry, decls, ⟨⟨0⟩, #[
    ⟨⟨0⟩, [], [], .jump ⟨⟨1⟩, [⟨u8Ty, .literal ⟨[42]⟩⟩]⟩, []⟩,
    ⟨⟨1⟩, [⟨u8Ty, byte.logical⟩], [], .ret (some ⟨u8Ty, .copy byte.logical⟩), []⟩,
    ⟨⟨2⟩, [], [⟨origin, addressByte⟩], .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩]⟩)]}
private def hugeTarget (p : MemoryPlace _ u8Ty) : M :=
  let op : MemoryInst Lowering := .op (opcode := .read) byte ()
    (.cons (.literal (ty := u8Ty) ⟨[42]⟩) .nil) [] [⟨u8Ty, byte⟩]
  let dead : MemoryBlock Lowering := ⟨⟨2⟩, [], [⟨origin, .beginLifetime byte.logical.root⟩, ⟨origin, op⟩],
    .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩
  {program false with functions := [(mir.entry, decls, ⟨⟨0⟩, #[first [], last p, dead]⟩)]
                      storage := [(mir.entry, [(byte.logical.root, .materialized), (unused.logical.root, .materialized)])]
                      layouts := [(mir.entry, [(byte.logical.root, ⟨1, 1, .nil⟩), (unused.logical.root, ⟨2^64 - 1, 1, .nil⟩)])]}

private def entering : registers := {start with block := jump.target}
private def bound : registers := {entering with live := [⟨0⟩], initialized := [⟨0⟩], virtualValues := [(⟨0⟩, byteValue)]}

private theorem source_jump (q : registers) : source.step sourceMir start q ↔ q = bound := by
  constructor
  · intro step
    let logical := sourceMir.logicalView (language := Huge)
    let graph := logical.functions[0].2.2
    let init := graph.blocks[0]'(by with_unfolding_all decide)
    let dest := graph.blocks[1]'(by with_unfolding_all decide)
    obtain ⟨block, hb, _, tick⟩ := step.2.1
    have lookup : some init = some block := by with_unfolding_all exact hb
    cases lookup
    with_unfolding_all change logical.Transfer (language := Huge) jump start q at tick
    obtain ⟨vs, read, block, hr, hb, _, writes⟩ := tick
    have args : some ([byteValue], (start : registers)) = some (vs, read) := by with_unfolding_all exact hr
    cases args
    have found : some dest = some block := by with_unfolding_all exact hb
    cases found
    with_unfolding_all
      change logical.WriteParams (language := Huge) [⟨u8Ty, {byte with storage := .virtual}⟩] [byteValue] entering q false at writes
    cases writes with
    | cons opened write tail =>
      cases tail
      rcases opened.2 with ⟨_, rfl⟩ | ⟨cls, _⟩
      · have eq : some bound = some q := by with_unfolding_all exact write.2.2.1
        exact (Option.some.inj eq).symm
      · cases cls
  · rintro rfl
    with_unfolding_all exact ⟨rfl, ⟨_, rfl, rfl, _, _, _, rfl, rfl, rfl,
      .cons (started := {entering with live := [⟨0⟩]}) ⟨nofun, .inl ⟨rfl, rfl⟩⟩
        ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩ (.nil _)⟩, [], rfl⟩

private theorem safe : ProgramSafe source sourceMir := by
  have result : source.result sourceMir bound (.returned 42 ByteArray.empty) := by with_unfolding_all rfl
  intro q stuck
  have reached := finite_invariant source sourceMir (fun p => p = start ∨ p = bound)
    (fun _ hi => .inl (by with_unfolding_all exact hi)) (by
      rintro _ next (rfl | rfl) step
      · exact .inr ((source_jump _).mp step)
      · exact (source.terminal _ _ _ result _ step).elim) stuck.1
  rcases reached with rfl | rfl
  · exact stuck.2.1 _ ((source_jump _).mpr rfl)
  · exact stuck.2.2 _ result

/-- From empty memory, the safe source and its actual lowering return 42.
Redirecting only the destination to the unused root produces a reachable stuck
state, violating the target-safety clause of the full lowering contract. -/
theorem corrupted_target_rejected :
    ProgramSafe source sourceMir ∧ source.observes sourceMir (.returned 42 ByteArray.empty) ∧
    lowerGraph (language := Huge) sourceMir = .ok (hugeTarget byte) ∧
    target.observes (hugeTarget byte) (.returned 42 ByteArray.empty) ∧
    (hugeTarget unused).HandoffWF ∧ target.stuck (hugeTarget unused) start ∧
    ¬ LowersMemory (language := Huge) sourceMir (hugeTarget unused) := by
  letI := Huge.typeDecidableEq
  have stuck : target.stuck (hugeTarget unused) start := by
    apply stuck_at_jump (language := Huge) (m := hugeTarget unused) (q := start)
      (edge := jump) (entry := first []) (dst := last unused) (values := [byteValue])
      ⟨start, by with_unfolding_all rfl, .nil _⟩ (by with_unfolding_all rfl) (by rfl) rfl
      (by with_unfolding_all rfl) (by with_unfolding_all rfl) _ (by with_unfolding_all rfl)
    exact invalid_root (by with_unfolding_all rfl)
  refine ⟨safe, ⟨bound, ⟨start, by with_unfolding_all rfl, .cons ((source_jump _).mpr rfl) (.nil _)⟩,
    by with_unfolding_all rfl⟩, by with_unfolding_all rfl, ?_,
    (checkHandoff_iff _).mp (by with_unfolding_all decide), stuck, ?_⟩
  · let opened : registers := reserved entering byte.logical.root 1 1 1
    let doneState := ((hugeTarget byte).writeValue (language := Huge) opened byte byteValue).getD entering
    have write : (hugeTarget byte).Writes (language := Huge) opened byte byteValue doneState := by
      with_unfolding_all
        refine ⟨⟨rfl, nofun⟩, fun _ => ⟨_, rfl, nofun, nofun⟩, rfl, rfl,
          fun _ => .inr ⟨_, [(0, [.init 42 none])], rfl, rfl, fun _ => ⟨0, _, .head _, by decide⟩, ?_⟩⟩
        intro offset bytes member
        cases List.mem_singleton.mp member
        exact ⟨by decide, by decide, rfl⟩
    have bindings : (hugeTarget byte).WriteParams (language := Huge) [⟨u8Ty, byte⟩] [byteValue] entering doneState := by
      refine Interfaces.Memory.WriteParams.cons (language := Huge) (started := opened) ?_ write (.nil _)
      with_unfolding_all exact ⟨nofun, .inr ⟨rfl, _, rfl, .inr ⟨by decide, ⟨0⟩, 1, ⟨0⟩, _, rfl, rfl⟩⟩⟩
    refine ⟨doneState, ⟨start, by with_unfolding_all rfl, .cons ?_ (.nil _)⟩, by with_unfolding_all rfl⟩
    with_unfolding_all exact ⟨rfl, ⟨_, rfl, rfl, _, _, _, rfl, rfl, rfl, bindings⟩, [], rfl⟩
  · intro contract
    obtain ⟨_, _, _, _, _, _, _, _, refinement, _⟩ := contract
    exact (refinement safe).2.2 start stuck

end SingleParam

end Concrete.Memory.TransferControl
