import ConcreteChecks.StageInterfaces.Memory.Fixtures
import ConcreteChecks.StageInterfaces.Refinement

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

theorem erasureBinding (project : Project) :
    (erasureSource project).Step (language := erasureLanguage) storeInitial erasureBound := by
  with_unfolding_all
    refine ⟨rfl, CoreValue.other (), storeInitial, {erasureBound with instruction := 0}, rfl, ?_, rfl⟩
  refine Memory.WriteParams.cons (language := erasureLanguage)
    (started := {storeInitial with live := [storeRoot]}) ?_ ?_ (.nil _)
  · exact ⟨by decide, Or.inl ⟨rfl, rfl⟩⟩
  · exact ⟨⟨rfl, fun _ => rfl⟩, nofun, rfl, rfl, nofun⟩

theorem erasureBindingUnique (project : Project)
    (next : MemoryRegisters erasureLanguage testProfile)
    (step : (erasureSource project).Step (language := erasureLanguage) storeInitial next) :
    next = erasureBound := by
  obtain ⟨_, value, afterRead, stored, read, writes, rfl⟩ := step
  have readExact : some (CoreValue.other (), storeInitial) = some (value, afterRead) := read
  cases readExact
  have params : @Memory.WriteParams erasureLanguage testLowering testProfile (@MIR.logicalView erasureLanguage testLowering testProfile (erasureSource project))
      [⟨(), ⟨⟨storeRoot, []⟩, .virtual⟩⟩] [CoreValue.other ()] storeInitial stored := by
    simp only [Bool.true_or, ite_true] at writes
    with_unfolding_all exact writes
  cases params with
  | cons started write rest =>
    cases rest
    rcases started.2 with ⟨_, rfl⟩ | ⟨impossible, _, _, _⟩
    · have written : some {erasureBound with instruction := 0} = some stored := write.2.2.1
      cases written
      rfl
    · have wrong : some StorageClass.virtual = some .materialized := impossible
      cases wrong

-- Retaining a materialized declaration, its layout and its origin metadata
-- cannot justify an immediate return with no live target local.
example (project : Project) (m : LoweringModels erasureLanguage testLowering testProfile)
    (initial : ∀ p, m.mir.execution.initial (erasureSource project) p →
      m.mirState p = storeInitial) :
    ¬ MemorySimulationWitness {m with memory := returnsStatus _ 0, memoryState := fun _ => storeInitial}
      (erasureSource project) (immediateReturn project) := by
  intro lowers
  have exactSource := lowers.2.2.2.2.2.2.2.1
  obtain ⟨_, sim, represents⟩ := lowers.2.2.2.2.2.2.2.2.2.2
  have hq : (returnsStatus (Memory testLowering testProfile) 0).execution.initial
      (immediateReturn project) () := trivial
  obtain ⟨start, starts, relation⟩ := sim.targetInitial () hq
  have sourceReachable : m.mir.execution.finite (erasureSource project) start :=
    ⟨start, starts, .nil _⟩
  have targetReachable : (returnsStatus (Memory testLowering testProfile) 0).execution.finite
      (immediateReturn project) () := ⟨(), hq, .nil _⟩
  obtain ⟨finish, path, result, _⟩ :=
    sim.targetFinish start () (.returned 0 ByteArray.empty) sourceReachable targetReachable relation rfl
  cases path with
  | nil =>
      have impossible := (exactSource.2.2.2 start _ sourceReachable).mp result
      rw [initial start starts] at impossible
      have absent : (erasureSource project).terminalObservation (language := erasureLanguage) storeInitial = none := by
        with_unfolding_all rfl
      rw [absent] at impossible
      cases impossible
  | @cons _ next _ tick _ =>
      have step := exactSource.2.1 start next sourceReachable tick.1
      rw [initial start starts] at step
      have physical := (represents next () ⟨start, starts, .cons tick.1 (.nil _)⟩
        targetReachable tick.2).1
      rw [erasureBindingUnique project _ step] at physical
      exact List.not_mem_nil (physical.2.2.1 storeRoot (List.mem_cons_self ..))

private def finiteReturnModel (A S : Type) (start : S) (step : S → S → Prop)
    (finished : S → Bool) (continues : ∀ p q, step p q → finished p = false)
    (status : UInt8 := 0) : Model A Nat where
  execution := {
    State := S
    initial := fun _ p => p = start
    initialized := fun _ => ⟨start, rfl⟩
    step := fun _ => step
    result := fun _ p result => finished p = true ∧ result = .returned status ByteArray.empty
    terminal := fun _ p _ result q tick => Bool.noConfusion ((continues p q tick).symm.trans result.1)
    safe := fun _ _ => True
    trace := fun _ _ => []
    permitted := fun _ _ => True
    resultTrace := by rintro _ _ _ ⟨_, rfl⟩; rfl
    traceGrows := fun _ _ _ _ => ⟨[], rfl⟩
  }
  result := fun _ p o => finished p = true ∧ o = .returned status ByteArray.empty
  projects := fun _ o result => o = .returned status ByteArray.empty ∧ result = .returned status ByteArray.empty
  terminalExact := by
    intro _ _ _
    exact ⟨fun ⟨h, eq⟩ => ⟨_, ⟨h, rfl⟩, rfl, eq⟩, fun ⟨_, ⟨h, _⟩, _, eq⟩ => ⟨h, eq⟩⟩
  completeProjection := fun _ _ _ h => ⟨_, h.2, rfl⟩
  runtimeExact := by rintro _ _ _ ⟨rfl, rfl⟩; exact ⟨rfl, rfl⟩

-- A target self-loop cannot keep a checkpoint before the source's binding
-- while excluding the state that contains it.
example :
    let source := finiteReturnModel Unit Bool false (fun p q => p = false ∧ q = true) id
      (fun _ _ tick => tick.1)
    let target := finiteReturnModel Unit Unit () (fun _ _ => True) (fun _ => false)
      (fun _ _ _ => rfl)
    ∀ sim : CheckpointSimulation source.execution target.execution source.result target.result () (),
      sim.related false () → ¬ sim.related true () → False := by
  dsimp only
  intro sim related missing
  obtain progresses | stutters := sim.targetAdvance false () ()
    ⟨false, rfl, .nil _⟩ ⟨(), rfl, .nil _⟩ related trivial
  · obtain ⟨before, after, _, step, matched⟩ := progresses
    rw [step.2] at matched
    exact missing matched
  · exact (Nat.lt_irrefl _) stutters.2

private def branchOrLoop : Model Unit Nat :=
  finiteReturnModel Unit Bool false (fun p _ => p = false) id (fun _ _ tick => tick)

-- The source can loop or take the exit. A compiler may select the exit.
example : branchOrLoop.Refines (returnsStatus Unit 0) () () := by
  intro _
  refine ⟨?_, ?_, ?_⟩
  · intro _ _ _ result
    exact ⟨true, ⟨false, rfl, .cons rfl (.nil _)⟩, ⟨rfl, result⟩, rfl⟩
  · exact fun _ infinite => (infinite.2 0).elim
  · exact fun _ stuck => stuck.2.2 (.returned 0 ByteArray.empty) rfl

-- Selecting the exit does not license changing its status.
example : ¬ branchOrLoop.Refines (returnsStatus Unit 42) () () := by
  intro refines
  have safe : ProgramSafe branchOrLoop.execution () := by
    intro p stuck
    cases p
    · exact stuck.2.1 false rfl
    · exact stuck.2.2 (.returned 0 ByteArray.empty) ⟨rfl, rfl⟩
  obtain ⟨_, _, result, _⟩ := (refines safe).1 () (.returned 42 ByteArray.empty)
    ⟨(), trivial, .nil _⟩ rfl
  cases result.2

private def allocationChoices (addresses : List UInt8) : Concrete.Contracts.Semantics Unit where
  State := Option UInt8
  initial _ state := state = none
  initialized _ := ⟨none, rfl⟩
  step _ before after := before = none ∧ ∃ address ∈ addresses, after = some address
  result _ state observed := ∃ address, state = some address ∧ observed = .returned address ByteArray.empty
  terminal := by rintro _ _ _ ⟨a, rfl, _⟩ _ ⟨e, _⟩; cases e
  safe _ _ := True
  trace _ _ := []
  permitted _ _ := True
  resultTrace := by rintro _ _ _ ⟨_, _, rfl⟩; rfl
  traceGrows := fun _ _ _ _ => ⟨[], rfl⟩

private theorem allocationSafe (addresses : List UInt8) (existsAddress : ∃ a, a ∈ addresses) :
    ProgramSafe (allocationChoices addresses) () := by
  intro state stuck
  cases state with
  | none =>
      obtain ⟨a, allowed⟩ := existsAddress
      exact stuck.2.1 (some a) ⟨rfl, a, allowed, rfl⟩
  | some a => exact stuck.2.2 (.returned a ByteArray.empty) ⟨a, rfl, rfl⟩

-- An early allocation may select one of the possible exposed addresses.
-- Selecting an address outside the source choices changes the behavior.
example :
    let source := allocationChoices [1, 2]
    let chosen := allocationChoices [1]
    let wrong := allocationChoices [3]
    BehaviorRefinement source chosen source.result chosen.result () () ∧
    ¬ BehaviorRefinement source wrong source.result wrong.result () () := by
  dsimp
  constructor
  · intro _
    refine ⟨?_, ?_, allocationSafe [1] ⟨1, by simp⟩⟩
    · rintro state _ ⟨start, rfl, path⟩ ⟨address, rfl, rfl⟩
      cases path with
      | cons first rest =>
          obtain ⟨_, a, allowed, equal⟩ := first
          subst equal
          cases rest with
          | nil =>
              have member : address ∈ ([1, 2] : List UInt8) :=
                List.mem_cons.mpr (Or.inl (List.mem_singleton.mp allowed))
              exact ⟨some address, ⟨none, rfl, .cons ⟨rfl, address, member, rfl⟩ (.nil _)⟩,
                ⟨address, rfl, rfl⟩, rfl⟩
          | cons second _ => cases second.1
    · intro run divergent
      obtain ⟨_, a, _, equal⟩ := divergent.2 0
      have impossible := (divergent.2 1).1
      rw [equal] at impossible
      cases impossible
  · intro refines
    obtain ⟨state, ⟨start, rfl, path⟩, ⟨address, rfl, returned⟩, _⟩ :=
      (refines (allocationSafe [1, 2] ⟨1, by simp⟩)).1 (some 3) (.returned 3 ByteArray.empty)
        ⟨none, rfl, .cons ⟨rfl, 3, by simp, rfl⟩ (.nil _)⟩ ⟨3, rfl, rfl⟩
    cases returned
    cases path with
    | cons first rest =>
        obtain ⟨_, a, allowed, equal⟩ := first
        subst equal
        cases rest with
        | nil => exact (show (3 : UInt8) ∉ [1, 2] from by decide) allowed
        | cons second _ => cases second.1

-- A reachable fault in the source defeats the safety premise instead of
-- requiring every successful lowering to prove progress for an unsafe input.
example :
    let source := allocationChoices []
    let target := allocationChoices [1]
    BehaviorRefinement source target source.result target.result () () := by
  dsimp
  intro safe
  exact (safe none ⟨⟨none, rfl, .nil _⟩,
    by rintro _ ⟨_, _, member, _⟩; exact List.not_mem_nil member,
    by rintro _ ⟨_, equality, _⟩; cases equality⟩).elim

end StageInterfaceControls
