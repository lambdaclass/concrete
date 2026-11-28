import ConcreteChecks.StageInterfaces.SSACalls

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

private def abortingCore : CoreInput testLanguage := testCore tabProject (.abort testSite)

private abbrev coreExecution := CoreInput.Execution (language := testLanguage)

private def coreStart : coreExecution.State :=
  (⟨(abortingCore.machineProgram.ownership, abortingCore.machineProgram.declarations), abortingCore.machineProgram.initial⟩, [])

private def coreTrapped : coreExecution.State :=
  (⟨(abortingCore.machineProgram.ownership, abortingCore.machineProgram.declarations),
    {abortingCore.machineProgram.initial with ctrl := .trapped .abort testSite}⟩, [])

private theorem coreAbortTick : coreExecution.step abortingCore coreStart coreTrapped := by
  with_unfolding_all
    exact ⟨rfl, ⟨_, _, .silent, rfl, rfl, Exec.Step.silent rfl, rfl⟩, _, rfl⟩

private theorem coreAbortTickUnique (q : coreExecution.State)
    (tick : coreExecution.step abortingCore coreStart q) : q = coreTrapped := by
  obtain ⟨_, ⟨p, next, label, initial, endState, step, effects⟩, _⟩ := tick
  have same : (⟨(abortingCore.machineProgram.ownership, abortingCore.machineProgram.declarations), abortingCore.machineProgram.initial⟩ :
      Σ env : AList Var Own.Descriptor × Declarations, Exec.Config (testLanguage.instrumented env.1 env.2)) =
        ⟨(abortingCore.machineProgram.ownership, abortingCore.machineProgram.declarations), p⟩ := initial
  cases eq_of_heq (Sigma.mk.inj same).2
  have expected : Exec.step abortingCore.machineProgram.body abortingCore.machineProgram.initial =
      some (Exec.Out.next {abortingCore.machineProgram.initial with ctrl := .trapped .abort testSite}) := by
    with_unfolding_all rfl
  cases step with
  | silent h =>
    rw [expected] at h
    have equal := Exec.Out.next.inj (Option.some.inj h)
    subst next
    exact Prod.ext endState effects
  | prim h =>
    rw [expected] at h
    cases h

-- The target's immediate return is a real MIR result. The wrong body
-- survives no Core-to-MIR proof even if a supplied model would call it a trap.
set_option maxRecDepth 4096 in
private def wrongCoreMIR : MIR testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨(), .literal ⟨[42]⟩⟩), [testOrigin]⟩
  {(returningMIR tabProject) with functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)]}

private theorem coreWrongBody : ¬ LowersCore (language := testLanguage) ⟨abortingCore, []⟩ wrongCoreMIR := by
  intro lowered
  have terminal : coreExecution.result abortingCore coreTrapped (helperTrap .abort) := by
    with_unfolding_all rfl
  have reached : ∀ q, coreExecution.finite abortingCore q → q = coreStart ∨ q = coreTrapped := by
    rintro q ⟨p, starts, path⟩
    have eq : p = coreStart := starts
    rw [eq] at path
    cases path with
    | nil => exact .inl rfl
    | cons tick rest =>
      obtain rfl := coreAbortTickUnique _ tick
      cases rest with
      | nil => exact .inr rfl
      | cons next _ => exact (coreExecution.terminal _ _ _ terminal _ next).elim
  have safe : ProgramSafe coreExecution abortingCore := by
    intro q stuck
    rcases reached q stuck.1 with rfl | rfl
    · exact stuck.2.1 coreTrapped coreAbortTick
    · exact stuck.2.2 _ terminal
  let target := MIR.Execution (language := testLanguage) (lang := testLowering) (profile := testProfile)
  let answer := Concrete.Contracts.Observation.returned 42 ByteArray.empty
  have returns : target.result wrongCoreMIR storeInitial answer := by with_unfolding_all rfl
  obtain ⟨q, finite, result, _⟩ := (lowered.2.2.2.2.2 safe).1 storeInitial answer
    ⟨storeInitial, by with_unfolding_all rfl, .nil _⟩ returns
  rcases reached q finite with rfl | rfl
  · have absent : (none : Option Concrete.Contracts.Observation) = some answer := by
      with_unfolding_all exact result
    cases absent
  · have wrong : some (helperTrap .abort) = some answer := by with_unfolding_all exact result
    have eq := Option.some.inj wrong
    change Concrete.Contracts.Observation.trapped _ _ _ _ = .returned 42 ByteArray.empty at eq
    cases eq

def abortMIR (kind : TrapKind) : MIR testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], [], .trap kind testOrigin, []⟩
  ⟨tabProject, Exec.Demo.mainF, [(Exec.Demo.mainF, ⟨[], none⟩)], [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)], [], []⟩

abbrev mirExecution := MIR.Execution (language := testLanguage)
  (lang := testLowering) (profile := testProfile)

private theorem abortLowering :
    LowersCore (language := testLanguage) ⟨abortingCore, []⟩ (abortMIR .abort) := by
  refine ⟨rfl, rfl, ⟨fun _ h => h, fun _ h => h⟩, ?_, ⟨⟨?_, nofun⟩, rfl⟩, ?_⟩
  · refine ⟨List.nodup_nil, ?_, nofun, ?_, ?_⟩
    · rintro site _ _ ⟨op, present, notAbort, _, _⟩ _
      have member : (testSite, CoreOperation.abort) = (site, op) := by
        apply Eq.symm
        exact List.mem_singleton.mp present
      have same := congrArg Prod.snd member
      cases same
      exact False.elim notAbort
    · rintro site evidence ⟨bucket, member, _⟩
      exact (List.not_mem_nil member).elim
    · intro _ op present
      have member := List.mem_singleton.mp present
      cases member
      trivial
  · intro fn locals graph present
    cases List.mem_singleton.mp present
    exact nofun
  · intro _
    have terminal : mirExecution.result (abortMIR .abort) storeInitial (helperTrap .abort) := by
      with_unfolding_all rfl
    have sourceResult : coreExecution.result abortingCore coreTrapped (helperTrap .abort) := by
      with_unfolding_all rfl
    have reached := reachableTerminal mirExecution (abortMIR .abort) storeInitial (helperTrap .abort)
      (fun _ h => by with_unfolding_all exact h) terminal
    refine ⟨?_, ?_, ?_⟩
    · intro state result finite observed
      obtain rfl := reached state finite
      have same : some (helperTrap .abort) = some result := by with_unfolding_all exact observed
      obtain rfl := Option.some.inj same
      exact ⟨coreTrapped, ⟨coreStart, rfl, .cons coreAbortTick (.nil _)⟩, sourceResult,
        by with_unfolding_all rfl⟩
    · intro run infinite
      have initial : run 0 = storeInitial := by with_unfolding_all exact infinite.1
      exact (mirExecution.terminal _ _ _ terminal _ (initial ▸ infinite.2 0)).elim
    · intro state stuck
      obtain rfl := reached state stuck.1
      exact stuck.2.2 _ terminal

example :
    LowersCore (language := testLanguage) ⟨abortingCore, []⟩ (abortMIR .abort) ∧
    ¬ LowersCore (language := testLanguage) ⟨abortingCore, []⟩ wrongCoreMIR := by
  exact ⟨abortLowering, coreWrongBody⟩

private def unsafeMIR : MIR testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], [], .invalid, []⟩
  {(returningMIR tabProject) with functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)]}

private theorem unsafeEntryStuck : mirExecution.stuck unsafeMIR storeInitial := by
  refine ⟨⟨storeInitial, by with_unfolding_all rfl, .nil _⟩, ?_, ?_⟩
  · intro next tick
    obtain ⟨_, ⟨block, found, step⟩, _⟩ := tick
    let entry : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .invalid, []⟩
    have same : some entry = some block := by with_unfolding_all exact found
    have equal := Option.some.inj same
    subst block
    exact step.2
  · intro o returned
    have impossible : none = some o := by with_unfolding_all exact returned
    cases impossible

-- An invalid terminator remains executable only as a malformed input to the
-- semantics. It is excluded from the registered lowering handoff.
example : ¬ LowersMemory (language := testLanguage) unsafeMIR (returningMemory tabProject) := by
  rintro ⟨_, _, _, _, _, _, _, _, _, tables, _, _⟩
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .invalid, []⟩
  have present : (Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩) ∈
      (unsafeMIR.logicalView (language := testLanguage)).functions :=
    by with_unfolding_all exact List.mem_singleton_self _
  exact noInvalidEnding tables present (List.mem_singleton_self block) rfl

-- Requiring an unconditional checkpoint simulation would reject the same
-- program after its confirmed startup fault.
example :
    ¬ ∃ _ : CheckpointSimulation mirExecution memoryExecution mirExecution.result memoryExecution.result
      unsafeMIR (returningMemory tabProject), True := by
  rintro ⟨sim, _⟩
  let target := returningMemory tabProject
  have targetInitial : memoryExecution.initial target storeInitial := by with_unfolding_all rfl
  obtain ⟨source, sourceInitial, related⟩ := sim.targetInitial storeInitial targetInitial
  have entry : source = storeInitial := by with_unfolding_all exact sourceInitial
  rw [entry] at related
  have observed : memoryExecution.result target storeInitial (.returned 0 ByteArray.empty) := by
    with_unfolding_all rfl
  obtain ⟨state, path, result, _⟩ := sim.targetFinish storeInitial storeInitial _
    unsafeEntryStuck.1 ⟨storeInitial, targetInitial, .nil _⟩ related observed
  cases path with
  | nil => exact unsafeEntryStuck.2.2 _ result
  | cons tick _ => exact unsafeEntryStuck.2.1 _ tick.1

end StageInterfaceControls
