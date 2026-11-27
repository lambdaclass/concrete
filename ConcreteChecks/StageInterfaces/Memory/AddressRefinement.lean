import ConcreteChecks.StageInterfaces.Memory.AddressSteps
import ConcreteChecks.StageInterfaces.Memory.Backing
import ConcreteChecks.StageInterfaces.Refinement

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

private theorem addressStatic (project : Project) :
    PreservesOrigins (logicalAddressProgram project).origins (addressMemory project).origins ∧
    LayoutsMatch addressLanguage.layout (addressMemory project) ∧
    PromotionSound (language := addressLanguage) (logicalAddressProgram project) (addressMemory project) ∧
    (addressMemory project).TablesWF ∧ (logicalAddressView project).TablesWF := by
  refine ⟨⟨?_, ?_⟩, ⟨?_, ?_⟩, ?_, ?_, ?_⟩
  · intro origin member
    change origin ∈ [testOrigin, testOrigin, testOrigin, testOrigin, testOrigin] at member
    have equal : origin = testOrigin := by simpa using member
    exact List.mem_cons.mpr (Or.inl equal)
  · intro origin member
    have list : origin ∈ [testOrigin, testOrigin, testOrigin] := member
    have equal : origin = testOrigin := by simpa using list
    exact List.mem_cons.mpr (Or.inl equal)
  · intro fn locals graph included root ty declared
    cases List.mem_singleton.mp included
    rcases List.mem_cons.mp declared with h | h
    · cases h
      exact ⟨_, rfl, Or.inr ⟨rfl, _, _, rfl, rfl, rfl⟩⟩
    · cases List.mem_singleton.mp h
      exact ⟨_, rfl, Or.inl rfl⟩
  · intro fn slots root layout listed slot
    cases List.mem_singleton.mp listed
    cases List.mem_singleton.mp slot
    exact ⟨_, _, (), _, List.mem_singleton_self _, List.mem_cons_self .., rfl, rfl, rfl⟩
  · have occurrences : (logicalAddressProgram project).operations =
        [⟨Exec.Demo.mainF, ⟨0⟩, 1, testOrigin, .operator (.named ⟨"address"⟩),
          [((), .address storeRoot [])], []⟩,
         ⟨Exec.Demo.mainF, ⟨0⟩, 2, testOrigin, .operator (.named ⟨"indirectReadWrite"⟩),
          [((), .copy shadowRoot [])], []⟩] := by with_unfolding_all rfl
    have escapes : (logicalAddressProgram project).Escapes (language := addressLanguage)
        Exec.Demo.mainF storeRoot := by
      refine ⟨_, occurrences ▸ List.mem_cons_self .., rfl, (), [], List.mem_singleton_self _⟩
    have noEscape : ¬ (logicalAddressProgram project).Escapes (language := addressLanguage)
        Exec.Demo.mainF shadowRoot := by
      rintro ⟨op, member, _, ty, path, argument⟩
      rw [occurrences] at member
      rcases List.mem_cons.mp member with rfl | rest
      · have same := congrArg Prod.snd (List.mem_singleton.mp argument)
        cases same
      · cases List.mem_singleton.mp rest
        have same := congrArg Prod.snd (List.mem_singleton.mp argument)
        cases same
    constructor
    · intro fn locals graph known place ty declared
      cases List.mem_singleton.mp known
      rcases List.mem_cons.mp declared with h | h
      · cases h
        exact ⟨_, .materialized, rfl, rfl, ⟨nofun, fun h => False.elim (h.2 escapes)⟩⟩
      · cases List.mem_singleton.mp h
        exact ⟨_, .virtual, rfl, rfl, ⟨fun _ => ⟨rfl, noEscape⟩, fun _ => rfl⟩⟩
    · intro fn classes place listed declared
      cases List.mem_singleton.mp listed
      rcases List.mem_cons.mp declared with h | h
      · cases h
      · cases List.mem_singleton.mp h
        exact ⟨(), _, _, _, _, List.mem_singleton_self _, List.mem_cons_of_mem _ (List.mem_singleton_self _),
          List.mem_singleton_self _, List.mem_cons_of_mem _ (List.mem_singleton_self _), rfl, noEscape⟩
  all_goals
    refine ⟨(by simp : [_].Nodup), (by simp : [_].Nodup), (by simp : [_].Nodup), ?_, ?_, ?_, ?_⟩
    · first | exact (by simp : [_].Nodup) | exact List.nodup_nil
    · intro fn classes membership
      cases List.mem_singleton.mp membership
      exact (by decide : [storeRoot, shadowRoot].Nodup)
    · intro fn slots membership
      first | (cases List.mem_singleton.mp membership; exact (by simp : [_].Nodup)) | exact (List.not_mem_nil membership).elim
    · intro fn locals graph member
      have logicalFunctions : (logicalAddressView project).functions =
          [(Exec.Demo.mainF, [(storeRoot, ()), (shadowRoot, ())], ⟨⟨0⟩, #[logicalAddressBlock]⟩)] := by
        with_unfolding_all rfl
      try rw [logicalFunctions] at member
      cases List.mem_singleton.mp member
      refine ⟨(by decide : [storeRoot, shadowRoot].Nodup),
        ⟨(by simp : [_].Nodup), ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
        _, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩
      · intro block included
        cases List.mem_singleton.mp included
        exact fun _ => nofun
      · intro block included
        cases List.mem_singleton.mp included
        refine ⟨rfl, ?_⟩
        intro inst present
        first
        | have list : inst ∈ addressBlock.body := present
          simp only [addressBlock, List.mem_cons, List.mem_nil_iff, or_false] at list
          rcases list with rfl | rfl | rfl | rfl | rfl <;> trivial
        | have list : inst ∈ logicalAddressBlock.body := present
          simp only [logicalAddressBlock, List.mem_cons, List.mem_nil_iff, or_false] at list
          rcases list with rfl | rfl | rfl <;> trivial

theorem AddressAllocation.loadUpdated (a : AddressAllocation) :
    a.updatedBytes.load testProfile.memoryTarget a.range.ptr 1 1 = some [.init 99 none] := by
  have access : a.updatedBytes.access testProfile.memoryTarget a.range.ptr 1 1 = some (some (⟨0⟩, 0)) :=
    a.access 99
  unfold Mem.Memory.load
  rw [access]
  rfl

private theorem addressRepresents (project : Project) (q : AddressTargetPhase) :
    (addressMemory project).RepresentsActivation (language := addressLanguage)
      q.source.state.toMemoryFrame q.state.toMemoryFrame q.state.bytes q.state.toMemoryFrame.backingRanges := by
  refine ⟨⟨⟨?_, ?_, ?_, ?_⟩, ?_⟩, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · cases q with
    | start => exact emptyBytesWF
    | allocated a => exact allocatedBytesWF a.space a.base a.fits
    | done a => exact storedBytesWF a.space a.base a.fits 99
    | _ a => exact storedBytesWF a.space a.base a.fits
  · cases q <;> first
    | exact List.nodup_nil
    | exact (by simp : [storeRoot].Nodup)
    | exact (by decide : [shadowRoot, storeRoot].Nodup)
  · cases q <;> first
    | exact List.nodup_nil
    | exact (by simp : [storeRoot].Nodup)
    | exact (by decide : [shadowRoot, storeRoot].Nodup)
  · cases q <;> first
    | exact fun _ h => (List.not_mem_nil h).elim
    | exact fun _ h => h
    | exact fun _ h => List.mem_cons_of_mem _ h
  · cases q <;> rfl
  · cases q <;> rfl
  · cases q <;> first
    | exact fun _ h => h
    | exact fun _ h => List.mem_cons_of_mem _ h
  · cases q <;> first
    | exact List.nodup_nil
    | exact (by simp : [storeRoot].Nodup)
    | exact (by decide : [shadowRoot, storeRoot].Nodup)
  · cases q <;> first | exact List.nodup_nil | exact (by simp : [storeRoot].Nodup)
  · cases q <;> first | exact List.nodup_nil | exact (by simp : [shadowRoot].Nodup)
  · cases q <;> exact List.nodup_nil
  · cases q <;> exact nofun
  · intro place
    cases q <;> first
    | exact ⟨fun h => (List.not_mem_nil h).elim, fun h => Bool.noConfusion (show false = true from h)⟩
    | (change (place ∈ [storeRoot]) ↔
          (AList.lookup [(storeRoot, storeValue)] place).isSome = true
       by_cases equal : storeRoot = place
       · subst place; exact ⟨fun _ => rfl, fun _ => List.mem_singleton_self _⟩
       · simp [AList.lookup, equal, Ne.symm equal])
    | (by_cases eqShadow : shadowRoot = place
       · subst place; exact ⟨fun _ => rfl, fun _ => List.mem_cons_self ..⟩
       · by_cases eqRoot : storeRoot = place
         · subst place; exact ⟨fun _ => rfl, fun _ => List.mem_cons_of_mem _ (List.mem_singleton_self _)⟩
         · with_unfolding_all
             simp [AddressSourcePhase.state, AddressTargetPhase.state, AddressTargetPhase.source,
               logicalAddressAfter, logicalAddressFinal, physicalAddressFinal, addressWritten, addressReady, MemoryRegisters.advance,
               AddressAllocation.bound, backingAfter, backingBefore,
               storeBefore, storeInitial, AList.lookup, eqRoot, eqShadow, Ne.symm eqRoot, Ne.symm eqShadow])
  · intro place value member
    have live : place ∈ List.map Prod.fst q.source.state.virtualValues :=
      List.mem_map.mpr ⟨(place, value), member, rfl⟩
    cases q <;> exact live
  · intro place value member
    cases q <;> first
    | exact (List.not_mem_nil member).elim
    | (cases List.mem_singleton.mp member
       exact ⟨List.mem_cons_self .., rfl⟩)
  · intro place range member
    cases q
    · exact (List.not_mem_nil member).elim
    all_goals
      cases List.mem_singleton.mp member
      refine ⟨?_, rfl, ⟨⟨1, 1⟩, ⟨0⟩, _, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩
      first | exact List.mem_singleton_self _ | exact List.mem_cons_of_mem _ (List.mem_singleton_self _)
  · intro place member
    cases q with
    | start => exact (List.not_mem_nil member).elim
    | allocated a =>
      cases List.mem_singleton.mp member
      exact ⟨_, _, (), .materialized, List.mem_singleton_self _, rfl, rfl,
        fun _ => ⟨⟨1, 1⟩, rfl, ⟨nofun, nofun⟩⟩, fun _ => nofun⟩
    | bound a | ready a | reference a | done a =>
      have membership : place = storeRoot ∨ place = shadowRoot := by
        first
        | exact Or.inl (List.mem_singleton.mp member)
        | rcases List.mem_cons.mp member with eq | tail
          · exact Or.inr eq
          · exact Or.inl (List.mem_singleton.mp tail)
      rcases membership with rfl | rfl
      all_goals
        first
        | (refine ⟨_, _, (), .materialized, List.mem_singleton_self _, rfl, rfl,
              fun _ => ⟨⟨1, 1⟩, rfl, ⟨nofun, nofun⟩⟩, ?_⟩
           intro value found
           first
           | (have same : some storeValue = some value := found
              cases same
              exact ⟨storeValue, rfl, (a.storeScalar project).2.2.2⟩)
           | (have same : some (CoreValue.scalar (.int .u8 99)) = some value := found
              cases same
              refine ⟨CoreValue.scalar (.int .u8 99), rfl, ?_, ?_⟩
              · change (do
                  let bytes ← a.updatedBytes.load testProfile.memoryTarget a.range.ptr 1 1
                  let value ← Hybrid.decode testProfile.memoryTarget (.int .u8) bytes
                  pure (CoreValue.scalar value : CoreValue Unit)) = some (CoreValue.scalar (.int .u8 99))
                rw [a.loadUpdated]
                rfl
              · intro _
                refine Or.inr ⟨a.range, [(0, [.init 99 none])], rfl, rfl,
                  fun _ => ⟨0, [.init 99 none], List.mem_cons_self .., by decide⟩, ?_⟩
                intro offset bytes written
                cases List.mem_singleton.mp written
                refine ⟨Nat.le_refl 1, ?_, a.loadUpdated⟩
                intro byte included
                cases List.mem_singleton.mp included
                decide))
        | (refine ⟨_, _, (), .virtual, List.mem_singleton_self _, rfl, rfl, nofun, ?_⟩
           intro value found
           first
           | exact (List.not_mem_nil member).elim
           | (have impossible : (none : Option (CoreValue Unit)) = some value := found; cases impossible)
           | (have same : some (CoreValue.other ()) = some value := found
              cases same
              exact ⟨CoreValue.scalar (.int .u8 a.base), rfl, rfl, nofun⟩)
           | (have same : some storeValue = some value := found
              cases same
              exact ⟨storeValue, rfl, rfl, nofun⟩))


private theorem addressCheckpoint (project : Project) (q : AddressTargetPhase) :
    (addressMemory project).RepresentsCheckpoint q.source.state q.state := by
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
  · have empty : q.state.callers = [] := by cases q <;> rfl
    change (addressMemory project).RepresentsActivation (language := addressLanguage)
      q.source.state.toMemoryFrame q.state.toMemoryFrame q.state.bytes
      (q.state.toMemoryFrame.backingRanges ++ q.state.callers.flatMap (·.caller.backingRanges))
    rw [empty, List.flatMap_nil, List.append_nil]
    exact addressRepresents project q
  · cases q <;> rfl
  · cases q <;> exact nofun
  · cases q <;> first
    | exact ⟨rfl, fun _ _ h => (List.not_mem_nil h).elim⟩
    | (refine ⟨rfl, ?_⟩
       intro root id member
       cases List.mem_singleton.mp member
       exact ⟨List.mem_cons_of_mem _ (List.mem_singleton_self _), Or.inr rfl⟩)
  · cases q <;> first | exact List.nodup_nil | exact (by simp : [(⟨0⟩ : Hybrid.Place)].Nodup)
  · cases q <;> rfl
  · cases q <;> rfl

private abbrev addressMIRExecution := MIR.Execution (language := addressLanguage)
  (lang := testLowering) (profile := testProfile)

private abbrev addressMemoryExecution := Memory.Execution (language := addressLanguage)
  (lang := testLowering) (profile := testProfile)

private def addressResult : Concrete.Contracts.Observation := .returned 42 ByteArray.empty

private theorem addressMIRTick (project : Project) {p q : AddressSourcePhase} (next : p.next = some q) :
    addressMIRExecution.step (logicalAddressProgram project) p.state q.state := by
  refine ⟨?_, (logicalAddressExact project p q.state).mpr ⟨q, next, rfl⟩, ?_⟩
  · cases p <;> first | (with_unfolding_all rfl) | cases next
  · cases p <;> cases q <;> first | (exact ⟨[], by with_unfolding_all rfl⟩) | cases next

private theorem addressMemoryTick (project : Project) {p q : AddressTargetPhase} (tick : p.Tick q) :
    addressMemoryExecution.step (addressMemory project) p.state q.state := by
  refine ⟨?_, (addressTargetExact project p q.state).mpr ⟨q, tick, rfl⟩, ?_⟩
  · cases tick <;> with_unfolding_all rfl
  · cases tick <;> exact ⟨[], by with_unfolding_all rfl⟩

private theorem addressMemoryReachable (project : Project) (q : addressMemoryExecution.State)
    (finite : addressMemoryExecution.finite (addressMemory project) q) :
    ∃ phase : AddressTargetPhase, phase.state = q :=
  reachedPhase addressMemoryExecution _ AddressTargetPhase.state .start (fun _ h => by with_unfolding_all exact h.symm)
    (fun p next h => by
      obtain ⟨phase, _, state⟩ := (addressTargetExact project p next).mp h.2.1
      exact ⟨phase, state⟩) finite

-- Both terminal traces reduce to []. Give rfl that value so the kernel checks each
-- interpreter against the empty trace without comparing the two interpreters.
set_option maxRecDepth 4096 in
set_option maxHeartbeats 3000000 in
example (project : Project) :
    LowersMemory (language := addressLanguage) (logicalAddressProgram project) (addressMemory project) := by
  have static := addressStatic project
  refine ⟨rfl, rfl, static.1, static.2.1, ⟨rfl, rfl⟩, static.2.2.1, static.2.2.2.1, static.2.2.2.2, ?_, ?_⟩
  · refine finiteProgressRefines addressMIRExecution addressMemoryExecution _ _ addressResult
      AddressSourcePhase.done.state
      ⟨AddressSourcePhase.start.state, by with_unfolding_all rfl, .cons (addressMIRTick project (p := .start) (q := .bound) rfl)
        (.cons (addressMIRTick project (p := .bound) (q := .reference) rfl)
          (.cons (addressMIRTick project (p := .reference) (q := .done) rfl) (.nil _)))⟩
      (by with_unfolding_all rfl) ?_ ?_ (fun state => 5 - state.instruction) ?_
    · intro q finite
      obtain ⟨phase, rfl⟩ := addressMemoryReachable project q finite
      cases phase
      · let a : AddressAllocation := ⟨⟨0⟩, 1, by unfold backingFits; decide⟩
        exact .inl ⟨_, addressMemoryTick project (.allocate a)⟩
      · exact .inl ⟨_, addressMemoryTick project (.bind _)⟩
      · exact .inl ⟨_, addressMemoryTick project (.beginDestination _)⟩
      · exact .inl ⟨_, addressMemoryTick project (.takeAddress _)⟩
      · exact .inl ⟨_, addressMemoryTick project (.indirect _)⟩
      · exact .inr (by with_unfolding_all rfl)
    · intro q o finite result
      obtain ⟨phase, rfl⟩ := addressMemoryReachable project q finite
      cases phase
      all_goals first
        | (have absent : none = some o := by with_unfolding_all exact result
           cases absent)
        | (have same : some addressResult = some o := by with_unfolding_all exact result
           exact ⟨(Option.some.inj same).symm, by with_unfolding_all exact (rfl : [] = [])⟩)
    · intro q q' finite step
      obtain ⟨phase, rfl⟩ := addressMemoryReachable project q finite
      obtain ⟨next, tick, rfl⟩ := (addressTargetExact project phase q').mp step.2.1
      cases tick <;> first
        | exact (by decide : 0 < 1)
        | exact (by decide : 1 < 2)
        | exact (by decide : 2 < 3)
        | exact (by decide : 3 < 4)
        | exact (by decide : 4 < 5)
  · intro _
    let related : addressMIRExecution.State → addressMemoryExecution.State → Prop :=
      fun p q => ∃ phase : AddressTargetPhase, phase.source.state = p ∧ phase.state = q
    refine ⟨⟨related, (fun _ q => 5 - q.instruction), ?_, ?_, ?_⟩, ?_⟩
    · intro q initial
      have same : q = storeInitial := initial
      subst q
      exact ⟨storeInitial, by with_unfolding_all rfl, .start, rfl, rfl⟩
    · rintro _ _ q' _ _ ⟨phase, rfl, rfl⟩ step
      obtain ⟨next, tick, rfl⟩ := (addressTargetExact project phase q').mp step.2.1
      cases tick with
      | allocate a => exact .inr ⟨⟨.allocated a, rfl, rfl⟩, (by decide : 4 < 5)⟩
      | bind a =>
        exact .inl ⟨_, _, .nil _, addressMIRTick project (p := .start) (q := .bound) rfl, .bound a, rfl, rfl⟩
      | beginDestination a => exact .inr ⟨⟨.ready a, rfl, rfl⟩, (by decide : 2 < 3)⟩
      | takeAddress a =>
        exact .inl ⟨_, _, .nil _, addressMIRTick project (p := .bound) (q := .reference) rfl, .reference a, rfl, rfl⟩
      | indirect a =>
        exact .inl ⟨_, _, .nil _, addressMIRTick project (p := .reference) (q := .done) rfl, .done a, rfl, rfl⟩
    · rintro _ _ o _ _ ⟨phase, rfl, rfl⟩ observed
      cases phase
      all_goals first
        | (have absent : none = some o := by with_unfolding_all exact observed
           cases absent)
        | (have same : some addressResult = some o := by with_unfolding_all exact observed
           obtain rfl := Option.some.inj same
           exact ⟨_, .nil _, by with_unfolding_all rfl, by with_unfolding_all exact (rfl : [] = [])⟩)
    · rintro _ _ _ _ ⟨q, rfl, rfl⟩
      exact addressCheckpoint project q


private def addressWrongReturn (project : Project) : Memory testLowering testProfile :=
  let block := {addressBlock with
    body := []
    ending := .ret (some ⟨(), .literal ⟨[0]⟩⟩)
    boundaryOrigins := [testOrigin]}
  {addressMemory project with
    functions := [(Exec.Demo.mainF, [(storeRoot, ()), (shadowRoot, ())], ⟨⟨0⟩, #[block]⟩)]}

-- Keep the declarations, layout and origins while replacing the reached
-- read/write computation with a return of the wrong value.
set_option maxRecDepth 4096 in
set_option maxHeartbeats 2500000 in
example (project : Project) :
    ¬ LowersMemory (language := addressLanguage) (logicalAddressProgram project) (addressWrongReturn project) := by
  intro lowered
  let input := logicalAddressProgram project
  let output := addressWrongReturn project
  have reachable (s : addressMIRExecution.State) (finite : addressMIRExecution.finite input s) :
      ∃ phase : AddressSourcePhase, phase.state = s :=
    reachedPhase addressMIRExecution input AddressSourcePhase.state .start
      (fun _ h => by with_unfolding_all exact h.symm)
      (fun p next h => by
        obtain ⟨phase, _, state⟩ := (logicalAddressExact project p next).mp h.2.1
        exact ⟨phase, state⟩) finite
  have safe : ProgramSafe addressMIRExecution input := by
    intro s stuck
    obtain ⟨phase, equal⟩ := reachable s stuck.1
    rw [← equal] at stuck
    cases phase
    · exact stuck.2.1 _ (addressMIRTick project (p := .start) (q := .bound) rfl)
    · exact stuck.2.1 _ (addressMIRTick project (p := .bound) (q := .reference) rfl)
    · exact stuck.2.1 _ (addressMIRTick project (p := .reference) (q := .done) rfl)
    · exact stuck.2.2 addressResult (by with_unfolding_all rfl)
  have initial : addressMemoryExecution.initial output storeInitial := by with_unfolding_all rfl
  let wrong : Concrete.Contracts.Observation := .returned 0 ByteArray.empty
  have result : addressMemoryExecution.result output storeInitial wrong := by with_unfolding_all rfl
  obtain ⟨s, finite, observed, _⟩ :=
    (lowered.2.2.2.2.2.2.2.2.1 safe).1 storeInitial wrong ⟨_, initial, .nil _⟩ result
  obtain ⟨phase, equal⟩ := reachable s finite
  rw [← equal] at observed
  cases phase
  all_goals first
    | (have absent : none = some wrong := by with_unfolding_all exact observed
       cases absent)
    | (have mismatch : some addressResult = some wrong := by with_unfolding_all exact observed
       have incompatible := Option.some.inj mismatch
       change Concrete.Contracts.Observation.returned 42 _ =
         Concrete.Contracts.Observation.returned 0 _ at incompatible
       cases incompatible)

end StageInterfaceControls
