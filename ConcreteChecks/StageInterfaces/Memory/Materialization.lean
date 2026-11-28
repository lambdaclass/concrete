import ConcreteChecks.StageInterfaces.Memory.Backing
import ConcreteChecks.StageInterfaces.Memory.Erasure

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

private def materializedBindingBlock : MemoryBlock testLowering :=
  ⟨⟨0⟩, [],
    [⟨testOrigin, .beginLifetime storeRoot⟩, ⟨testOrigin, .bind storePlace (.literal ⟨[]⟩)⟩],
    .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩

private def materializedBinding (project : Project) : Memory testLowering testProfile :=
  {(immediateReturn project) with
    functions := [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[materializedBindingBlock]⟩)]}

private theorem backingAccess (space : AddrSpace) (base : Nat) :
    (backingBefore space base).bytes.access testProfile.memoryTarget (backingRange space base).ptr 1 1 =
      some (some (⟨0⟩, 0)) := by
  simp [backingBefore, backingRange, Mem.Memory.access, Mem.Memory.block, AList.lookup, Mem.candidate, Nat.mod_one]

private theorem aggregateWrite (project : Project) (space : AddrSpace) (base : Nat) :
    (materializedBinding project).Writes (language := erasureLanguage) (backingBefore space base) storePlace
      (.other ()) {backingAfter space base with instruction := 1} := by
  have stored : (backingBefore space base).bytes.store testProfile.memoryTarget
      (backingRange space base).ptr [.init 42 none] 1 = some (backingAfter space base).bytes := by
    unfold Mem.Memory.store
    rw [show ([Mem.MByte.init 42 none]).length = 1 from rfl, backingAccess]
    simp [backingBefore, backingAfter, Mem.Memory.modify, AList.update, Mem.candidate]
  have loaded : (backingAfter space base).bytes.load testProfile.memoryTarget
      (backingRange space base).ptr 1 1 = some [.init 42 none] := by
    unfold Mem.Memory.load
    rw [show (backingAfter space base).bytes.access testProfile.memoryTarget
      (backingRange space base).ptr 1 1 = some (some (⟨0⟩, 0)) by
        simp [backingAfter, backingBefore, backingRange, Mem.Memory.access, Mem.Memory.block,
          AList.lookup, Mem.candidate, Nat.mod_one]]
    rfl
  refine ⟨⟨rfl, fun _ => rfl⟩, (fun _ => ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩),
    ?_, ?_, fun _ => Or.inr ⟨backingRange space base, [(0, [.init 42 none])], rfl, rfl, ?_, ?_⟩⟩
  · simp [Memory.writeValue, Memory.markWritten, Memory.storeRuns, Memory.atOffset, Memory.mayInitialize, Memory.authorizeWrite, Memory.writeOwnerView, Memory.ownerView, Memory.zeroRoot, Memory.rangeAt, Memory.byteRuns,
      Memory.layoutAt, materializedBinding, immediateReturn, zeroMemory, storePlace,
      backingBefore, backingAfter, backingRange, storeBefore, storeInitial, storeRoot,
      Mem.Memory.store, Mem.Memory.access, Mem.Memory.block, Mem.Memory.modify,
      AList.lookup, AList.update, Mem.candidate, Nat.mod_one, erasureLanguage] <;> rfl
  · with_unfolding_all
      change (do
        let bytes ← (backingAfter space base).bytes.load testProfile.memoryTarget
          (backingRange space base).ptr 1 1
        let value ← erasureLanguage.decodeOther testProfile.memoryTarget () bytes
        pure (CoreValue.other value)) = _
    rw [loaded]
    rfl
  · exact fun _ => ⟨0, [.init 42 none], List.mem_cons_self .., by decide⟩
  · intro offset bytes member
    cases List.mem_singleton.mp member
    refine ⟨Nat.le_refl 1, ?_, loaded⟩
    intro byte included
    cases List.mem_singleton.mp included
    decide

private theorem materializedRepresents (project : Project) (space : AddrSpace) (base : Nat)
    (fits : backingFits space base) :
    (materializedBinding project).RepresentsActivation (language := erasureLanguage)
      erasureBound.toMemoryFrame (backingAfter space base).toMemoryFrame (backingAfter space base).bytes := by
  refine ⟨⟨⟨storedBytesWF space base fits, (by simp : [storeRoot].Nodup), (by simp : [storeRoot].Nodup), fun _ h => h⟩, rfl⟩, rfl, fun _ h => h,
    (by simp : [storeRoot].Nodup), (by simp : [storeRoot].Nodup), List.nodup_nil, List.nodup_nil, nofun, ?_, ?_, nofun, ?_, ?_⟩
  · intro place
    change (place ∈ [storeRoot]) ↔ (AList.lookup [(storeRoot, CoreValue.other ())] place :
      Option (CoreValue Unit)).isSome = true
    by_cases equal : storeRoot = place
    · subst place
      decide
    · simp [AList.lookup, equal, Ne.symm equal]
  · intro place value member
    exact List.mem_singleton.mpr (congrArg Prod.fst (List.mem_singleton.mp member))
  · intro place range member
    cases List.mem_singleton.mp member
    exact ⟨List.mem_singleton_self _, rfl,
      ⟨⟨1, 1, .nil⟩, ⟨0⟩, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩
  · intro place member
    have eq := List.mem_singleton.mp member
    subst place
    refine ⟨_, _, (), .materialized, List.mem_singleton_self _, rfl, rfl, ?_, ?_⟩
    · intro _
      exact ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩
    · intro value lookedUp
      have same : some (CoreValue.other ()) = some value := lookedUp
      cases same
      exact ⟨_, rfl, (aggregateWrite project space base).2.2.2⟩

private inductive Materialization where
  | initial
  | allocated (space : AddrSpace) (base : Nat) (fits : backingFits space base)
  | bound (space : AddrSpace) (base : Nat) (fits : backingFits space base)

private def Materialization.state : Materialization → MemoryRegisters erasureLanguage testProfile
  | .initial => storeInitial
  | .allocated space base _ => backingBefore space base
  | .bound space base _ => backingAfter space base

private def Materialization.finished : Materialization → Bool
  | .bound .. => true
  | _ => false

private inductive Materialization.Tick : Materialization → Materialization → Prop
  | allocate (space base) (fits : backingFits space base) : Tick .initial (.allocated space base fits)
  | bind (space base) (fits : backingFits space base) : Tick (.allocated space base fits) (.bound space base fits)

private def materializedSourceState (done : Bool) : MemoryRegisters erasureLanguage testProfile :=
  if done then erasureBound else storeInitial

private theorem materializationNormal (project : Project) (p : Materialization) (next : MemoryRegisters erasureLanguage testProfile) :
    (materializedBinding project).StepOutcome (language := erasureLanguage) p.state next false ↔
      ∃ q, p.Tick q ∧ q.state = next := by
  constructor
  · intro step
    cases p with
    | initial =>
      obtain ⟨block, fetched, tick⟩ := step
      have same : some _ = some block := fetched
      cases same
      with_unfolding_all
        change ∃ started, (materializedBinding project).Begins
          (language := erasureLanguage) storeInitial storeRoot started ∧ next = started.advance at tick
      obtain ⟨started, begins, rfl⟩ := tick
      rcases begins.2 with ⟨impossible, _⟩ | ⟨_, layout, lookup, isZero | allocated⟩
      · have wrong : some StorageClass.materialized = some .virtual := impossible
        cases wrong
      · have same : some (⟨1, 1, .nil⟩ : Layout) = some layout := lookup
        cases same
        exact (Nat.one_ne_zero isZero.1).elim
      · have same : some (⟨1, 1, .nil⟩ : Layout) = some layout := lookup
        cases same
        obtain ⟨_, space, base, id, bytes, alloc, rfl⟩ := allocated
        have result : Mem.Memory.empty.alloc testProfile.memoryTarget space base 1 1 = .ok (id, bytes) := alloc
        unfold Mem.Memory.alloc at result
        dsimp only at result
        split at result
        · rename_i fits
          cases result
          exact ⟨.allocated space base fits, .allocate space base fits, rfl⟩
        · contradiction
    | allocated space base fits =>
      obtain ⟨block, fetched, tick⟩ := step
      have same : some _ = some block := fetched
      cases same
      with_unfolding_all
        change (materializedBinding project).InstructionStep (language := erasureLanguage)
          (backingBefore space base) next false (.bind storePlace (.literal ⟨[]⟩)) at tick
      obtain ⟨value, afterRead, stored, read, writes, rfl⟩ := tick
      have readExact : some (CoreValue.other (), backingBefore space base) = some (value, afterRead) := read
      cases readExact
      have writeExact := (aggregateWrite project space base).2.2.1
      have stores := writeExact.symm.trans writes.2.2.1
      cases stores
      exact ⟨.bound space base fits, .bind space base fits, rfl⟩
    | bound space base fits =>
      obtain ⟨block, fetched, tick⟩ := step
      have same : some _ = some block := fetched
      cases same
      with_unfolding_all
        change _ ∧ (materializedBinding project).Return (language := erasureLanguage)
          (some ⟨(), .literal ⟨[0]⟩⟩) (backingAfter space base) next at tick
      obtain ⟨_, caller, rest, _, _, _, callers, _⟩ := tick
      change [] = caller :: rest at callers
      cases callers
  · rintro ⟨q, tick, rfl⟩
    cases tick with
    | allocate space base fits =>
      refine ⟨_, rfl, {backingBefore space base with instruction := 0}, ⟨by decide, Or.inr ?_⟩, rfl⟩
      refine ⟨rfl, ⟨⟨1, 1, .nil⟩, rfl, Or.inr ⟨by decide, space, base, ⟨0⟩, _, ?_, rfl⟩⟩⟩
      change Mem.Memory.empty.alloc testProfile.memoryTarget space base 1 1 = _
      unfold Mem.Memory.alloc
      have ok : Mem.Memory.empty.Fits testProfile.memoryTarget (Mem.candidate space base 1 1) := fits
      simp only [ok, ite_true]
      rfl
    | bind space base fits =>
      exact ⟨_, rfl, .other (), backingBefore space base, {backingAfter space base with instruction := 1},
        rfl, aggregateWrite project space base, rfl⟩

private theorem materializationExact (project : Project) (p : Materialization) (next : MemoryRegisters erasureLanguage testProfile) :
    (materializedBinding project).Step (language := erasureLanguage) p.state next ↔
      ∃ q, p.Tick q ∧ q.state = next := by
  constructor
  · intro tick
    apply (materializationNormal project p next).mp
    apply tick.normal (language := erasureLanguage)
    intro partialState failed
    have block : (materializedBinding project).blockAt p.state = some materializedBindingBlock := by cases p <;> with_unfolding_all rfl
    cases p with
    | initial =>
      have ok := (materializationNormal project .initial (.allocated ⟨0⟩ 1 (by unfold backingFits; decide) : Materialization).state).mpr
        ⟨_, .allocate ⟨0⟩ 1 _, rfl⟩
      have exhausted : (materializedBinding project).BeginExhausted (language := erasureLanguage) storeInitial storeRoot :=
        failed.instructionAt (op := ⟨testOrigin, .beginLifetime storeRoot⟩) block (by with_unfolding_all rfl)
      obtain ⟨started, begins, _⟩ := ok.instructionAt (op := ⟨testOrigin, .beginLifetime storeRoot⟩) block (by with_unfolding_all rfl)
      exact exhausted.unavailable ⟨started, begins⟩
    | allocated space base fits =>
      exact Memory.bind_cannot_exhaust (language := erasureLanguage)
        (failed.instructionAt (op := ⟨testOrigin, .bind storePlace (.literal ⟨[]⟩)⟩) block (by with_unfolding_all rfl))
    | bound space base fits =>
      obtain ⟨caller, rest, _, _, _, callers, _⟩ := failed.terminatorAt block (by with_unfolding_all rfl)
      change [] = caller :: rest at callers
      cases callers
  · rintro ⟨q, tick, rfl⟩
    have normal := (materializationNormal project p q.state).mpr ⟨q, tick, rfl⟩
    cases tick <;> exact normal

private def logicalBindingBlock : MemoryBlock testLowering :=
  ⟨⟨0⟩, [], [⟨testOrigin, .bind (ty := ()) ⟨⟨storeRoot, []⟩, .virtual⟩ (.literal ⟨[]⟩)⟩],
    .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩

private theorem logicalBindingFunctions (project : Project) :
    ((erasureSource project).logicalView (language := erasureLanguage)).functions =
      [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[logicalBindingBlock]⟩)] := by
  with_unfolding_all rfl

private theorem materializedSourceExact (project : Project) (p : Bool)
    (next : MemoryRegisters erasureLanguage testProfile) :
    (erasureSource project).Step (language := erasureLanguage) (materializedSourceState p) next ↔
      ∃ q : Bool, (p = false ∧ q = true) ∧ materializedSourceState q = next := by
  constructor
  · intro step
    cases p
    · exact ⟨true, ⟨rfl, rfl⟩, (erasureBindingUnique project _ step).symm⟩
    · obtain ⟨block, fetched, tick⟩ := step
      have same : some logicalBindingBlock = some block := by with_unfolding_all exact fetched
      cases same
      with_unfolding_all
        change _ ∧ (erasureSource project).logicalView.Return (language := erasureLanguage)
          (some ⟨(), .literal ⟨[0]⟩⟩) erasureBound next at tick
      obtain ⟨_, caller, rest, _, _, _, callers, _⟩ := tick
      change [] = caller :: rest at callers
      cases callers
  · rintro ⟨_, ⟨rfl, rfl⟩, rfl⟩
    exact erasureBinding project

private theorem materializedTables (project : Project) :
    (materializedBinding project).TablesWF ∧
      ((erasureSource project).logicalView (language := erasureLanguage)).TablesWF := by
  constructor
  all_goals
    with_unfolding_all
      refine ⟨(by simp : [_].Nodup), (by simp : [_].Nodup), (by simp : [_].Nodup), ?_, ?_, ?_, ?_⟩
    · first | exact (by simp : [_].Nodup) | exact List.nodup_nil
    · intro fn classes h
      cases List.mem_singleton.mp h
      exact (by simp : [_].Nodup)
    · intro fn layouts h
      first | (cases List.mem_singleton.mp h; exact (by simp : [_].Nodup)) | exact (List.not_mem_nil h).elim
    · intro fn locals graph member
      try rw [logicalBindingFunctions] at member
      cases List.mem_singleton.mp member
      refine ⟨(by simp : [_].Nodup), ⟨(by simp : [_].Nodup), ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
        _, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩
      · intro block included
        cases List.mem_singleton.mp included
        exact fun _ => nofun
      · intro block included
        cases List.mem_singleton.mp included
        refine ⟨rfl, ?_⟩
        intro op h
        first
        | rcases List.mem_cons.mp h with rfl | last
          · trivial
          · cases List.mem_singleton.mp last; trivial
        | cases List.mem_singleton.mp h; trivial

private theorem materializationRepresents (project : Project) (q : Materialization) :
    (materializedBinding project).RepresentsActivation (language := erasureLanguage)
      (materializedSourceState q.finished).toMemoryFrame q.state.toMemoryFrame q.state.bytes := by
  cases q with
  | initial =>
    exact ⟨⟨⟨emptyBytesWF, List.nodup_nil, List.nodup_nil, nofun⟩, rfl⟩,
      rfl, nofun, List.nodup_nil, List.nodup_nil, List.nodup_nil, List.nodup_nil,
      nofun, (fun _ => ⟨nofun, nofun⟩), nofun, nofun, nofun, nofun⟩
  | allocated space base fits =>
    refine ⟨⟨⟨allocatedBytesWF space base fits, (by simp : [_].Nodup), List.nodup_nil, nofun⟩, rfl⟩,
      rfl, nofun, List.nodup_nil, (by simp : [_].Nodup), List.nodup_nil, List.nodup_nil,
      nofun, (fun _ => ⟨nofun, nofun⟩), nofun, nofun, ?_, ?_⟩
    · intro place range member
      cases List.mem_singleton.mp member
      exact ⟨List.mem_singleton_self _, rfl,
        ⟨⟨1, 1, .nil⟩, ⟨0⟩, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩
    · intro place member
      cases List.mem_singleton.mp member
      exact ⟨_, _, (), .materialized, List.mem_singleton_self _, rfl, rfl,
        fun _ => ⟨⟨1, 1, .nil⟩, rfl, ⟨nofun, nofun⟩⟩, fun _ => nofun⟩
  | bound space base fits => exact materializedRepresents project space base fits

private theorem materializationStatic (project : Project) :
    PreservesOrigins (erasureSource project).origins (materializedBinding project).origins ∧
    LayoutsMatch erasureLanguage.layout (materializedBinding project) ∧
    PromotionSound (language := erasureLanguage) (erasureSource project) (materializedBinding project) := by
  refine ⟨⟨?_, ?_⟩, ?_, ?_⟩
  · intro x h
    have member : x ∈ [testOrigin, testOrigin] := h
    rcases List.mem_cons.mp member with equal | rest
    · exact List.mem_singleton.mpr equal
    · exact rest
  · intro x h
    have member : x ∈ [testOrigin] := h
    exact List.mem_cons_of_mem _ member
  · constructor
    · intro fn locals graph member place ty localMember
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp localMember
      exact ⟨_, rfl, Or.inr ⟨rfl, _, _, rfl, rfl, rfl⟩⟩
    · intro fn layouts place layout member slot
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp slot
      exact ⟨_, _, (), _, List.mem_singleton_self _, List.mem_singleton_self _, rfl, rfl, rfl⟩
  · constructor
    · intro fn locals graph member place ty localMember
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp localMember
      exact ⟨_, .materialized, rfl, rfl, ⟨nofun, fun h => Bool.noConfusion (show false = true from h.1)⟩⟩
    · intro fn classes place member localMember
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp localMember

private abbrev bindingMIR := MIR.Execution (language := erasureLanguage)
  (lang := testLowering) (profile := testProfile)

private abbrev bindingMemory := Memory.Execution (language := erasureLanguage)
  (lang := testLowering) (profile := testProfile)

private def bindingResult : Concrete.Contracts.Observation := .returned 0 ByteArray.empty

private theorem bindingMIRTick (project : Project) :
    bindingMIR.step (erasureSource project) storeInitial erasureBound := by
  exact ⟨by with_unfolding_all rfl, erasureBinding project, ⟨[], by with_unfolding_all rfl⟩⟩

private theorem materializationTick (project : Project) {p q : Materialization} (tick : p.Tick q) :
    bindingMemory.step (materializedBinding project) p.state q.state := by
  refine ⟨?_, (materializationExact project p q.state).mpr ⟨q, tick, rfl⟩, ?_⟩
  · cases tick <;> with_unfolding_all rfl
  · cases tick <;> exact ⟨[], by with_unfolding_all rfl⟩

private theorem materializationReachable (project : Project) (q : bindingMemory.State)
    (finite : bindingMemory.finite (materializedBinding project) q) :
    ∃ phase : Materialization, phase.state = q :=
  reachedPhase bindingMemory _ Materialization.state .initial (fun _ h => by with_unfolding_all exact h.symm)
    (fun p next h => by
      obtain ⟨phase, _, state⟩ := (materializationExact project p next).mp h.2.1
      exact ⟨phase, state⟩) finite

-- Both terminal traces reduce to []. Give rfl that value so the kernel checks each
-- interpreter against the empty trace without comparing the two interpreters.
set_option maxRecDepth 4096 in
set_option maxHeartbeats 2000000 in
example (project : Project) :
    LowersMemory (language := erasureLanguage) (erasureSource project) (materializedBinding project) := by
  have static := materializationStatic project
  have hretains : RetainsTypedRoots (erasureSource project) (materializedBinding project) := by
    refine ⟨by simp [erasureSource, shadowMIR], by simp [materializedBinding], ?_, ?_, ?_, ?_⟩
    · intro _ _ _ h; cases List.mem_singleton.mp h; simp
    · intro _ _ _ h; cases List.mem_singleton.mp h; simp
    · intro _ _ _ h; cases List.mem_singleton.mp h
      exact ⟨_, _, List.mem_singleton_self _, fun _ _ => Iff.rfl⟩
    · intro _ _ _ h; cases List.mem_singleton.mp h
      exact ⟨_, _, List.mem_singleton_self _, fun _ _ => Iff.rfl⟩
  have hlayouts : (materializedBinding project).LayoutsWF := by
    intro tbl htbl slot hslot
    cases List.mem_singleton.mp htbl
    cases List.mem_singleton.mp hslot
    decide
  refine ⟨rfl, rfl, static.1, static.2.1, ⟨rfl, rfl⟩, static.2.2, hretains, hlayouts,
    (materializedTables project).1, (materializedTables project).2, ?_, ?_⟩
  · apply matchingRefinement (language := erasureLanguage)
    refine finiteProgressRefines bindingMIR bindingMemory _ _ bindingResult erasureBound
      ⟨storeInitial, by with_unfolding_all rfl, .cons (bindingMIRTick project) (.nil _)⟩
      (by with_unfolding_all rfl) ?_ ?_ (fun state => 2 - state.instruction) ?_
    · intro q finite
      obtain ⟨phase, rfl⟩ := materializationReachable project q finite
      cases phase with
      | initial =>
        let fits : backingFits ⟨0⟩ 1 := by unfold backingFits; decide
        exact .inl ⟨_, materializationTick project (.allocate ⟨0⟩ 1 fits)⟩
      | allocated space base fits =>
        exact .inl ⟨_, materializationTick project (.bind space base fits)⟩
      | bound => exact .inr (by with_unfolding_all rfl)
    · intro q o finite result
      obtain ⟨phase, rfl⟩ := materializationReachable project q finite
      cases phase
      all_goals first
        | (have impossible : none = some o := by with_unfolding_all exact result
           cases impossible)
        | (have same : some bindingResult = some o := by with_unfolding_all exact result
           exact ⟨(Option.some.inj same).symm, by with_unfolding_all exact (rfl : [] = [])⟩)
    · intro q q' finite step
      obtain ⟨phase, rfl⟩ := materializationReachable project q finite
      obtain ⟨next, tick, rfl⟩ := (materializationExact project phase q').mp step.2.1
      cases tick <;> first
        | exact (by decide : 0 < 1)
        | exact (by decide : 1 < 2)
        | exact (by decide : 2 < 3)
        | exact (by decide : 3 < 4)
        | exact (by decide : 4 < 5)
  · intro _
    let related : bindingMIR.State → bindingMemory.State → Prop :=
      fun p q => ∃ phase : Materialization, materializedSourceState phase.finished = p ∧ phase.state = q
    refine ⟨⟨related, (fun _ q => 2 - q.instruction), ?_, ?_, ?_⟩, ?_⟩
    · intro q initial
      have same : q = storeInitial := initial
      subst q
      exact ⟨storeInitial, by with_unfolding_all rfl, .initial, rfl, rfl⟩
    · rintro _ _ q' _ _ ⟨phase, rfl, rfl⟩ step
      obtain ⟨next, tick, rfl⟩ := (materializationExact project phase q').mp step.2.1
      cases tick with
      | allocate space base fits =>
        exact .inr ⟨⟨.allocated space base fits, rfl, rfl⟩, (by decide : 1 < 2)⟩
      | bind space base fits =>
        exact .inl ⟨_, _, .nil _, bindingMIRTick project, .bound space base fits, rfl, rfl⟩
    · rintro _ _ o _ _ ⟨phase, rfl, rfl⟩ observed
      cases phase
      all_goals first
        | (have absent : none = some o := by with_unfolding_all exact observed
           cases absent)
        | (have same : some bindingResult = some o := by with_unfolding_all exact observed
           obtain rfl := Option.some.inj same
           exact ⟨_, .nil _, by with_unfolding_all rfl, by with_unfolding_all exact (rfl : [] = [])⟩)
    · rintro _ _ _ _ ⟨phase, rfl, rfl⟩
      refine ⟨⟨materializationRepresents project phase, ?_, ?_, ?_, ?_, ?_, ?_⟩, ?_⟩
      all_goals cases phase <;> first | rfl | exact ⟨rfl, nofun⟩ | exact List.nodup_nil | exact nofun

-- This mutant returns the right value and retains the declaration and layout.
-- The actual checkpoint relation rejects removing the live aggregate.
set_option maxRecDepth 4096 in
example (project : Project) :
    ¬ LowersMemory (language := erasureLanguage) (erasureSource project) (immediateReturn project) := by
  intro lowers
  have starts : bindingMIR.initial (erasureSource project) storeInitial := by with_unfolding_all rfl
  have terminal : bindingMIR.result (erasureSource project) erasureBound bindingResult := by
    with_unfolding_all rfl
  have safe : ProgramSafe bindingMIR (erasureSource project) := by
    intro q stuck
    obtain ⟨phase, rfl⟩ := reachedPhase bindingMIR (erasureSource project) materializedSourceState false
      (fun _ h => by with_unfolding_all exact h.symm) (fun p next step => by
        obtain ⟨finish, _, state⟩ := (materializedSourceExact project p next).mp step.2.1
        exact ⟨finish, state⟩) stuck.1
    cases phase
    · exact stuck.2.1 _ (bindingMIRTick project)
    · exact stuck.2.2 _ terminal
  obtain ⟨sim, represents⟩ := lowers.2.2.2.2.2.2.2.2.2.2.2 safe
  have targetInitial : bindingMemory.initial (immediateReturn project) storeInitial := by with_unfolding_all rfl
  have targetReach : bindingMemory.finite (immediateReturn project) storeInitial :=
    ⟨_, targetInitial, .nil _⟩
  obtain ⟨p, pInitial, related⟩ := sim.targetInitial storeInitial targetInitial
  have initial : p = storeInitial := by with_unfolding_all exact pInitial
  rw [initial] at related
  have observed : bindingMemory.result (immediateReturn project) storeInitial bindingResult := by
    with_unfolding_all rfl
  obtain ⟨finish, path, result, _⟩ := sim.targetFinish storeInitial storeInitial bindingResult
    ⟨_, starts, .nil _⟩ targetReach related observed
  cases path with
  | nil =>
    have absent : none = some bindingResult := by with_unfolding_all exact result
    cases absent
  | @cons _ next _ tick _ =>
    have bound := erasureBindingUnique project next tick.1.2.1
    have physical := (represents next storeInitial ⟨_, starts, .cons tick.1 (.nil _)⟩
      targetReach tick.2).1.1
    rw [bound] at physical
    exact List.not_mem_nil (physical.2.2.1 storeRoot (List.mem_cons_self ..))

end StageInterfaceControls
