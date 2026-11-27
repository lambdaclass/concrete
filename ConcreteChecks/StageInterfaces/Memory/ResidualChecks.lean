import ConcreteChecks.StageInterfaces.Memory.Fixtures
import ConcreteChecks.StageInterfaces.Refinement

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

private abbrev residualLanguage : CoreLanguage :=
  {testLanguage with
    checkTrap := fun _ _ values =>
      match values with
      | [.scalar (.int _ 0)] => true
      | _ => false}

private def runtimeCheck : OperationCheck := ⟨0, .divisionByZero, .residual⟩

private def residualMIR (project : Project) : MIR testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [],
    [⟨testOrigin, .op (opcode := .arithmetic (.plain .div)) (types := [()]) (ty := ()) ⟨storeRoot, []⟩ ()
      (.cons (.literal ⟨[0]⟩) .nil) [runtimeCheck]⟩], .trap .abort testOrigin, []⟩
  ⟨project, Exec.Demo.mainF, [(Exec.Demo.mainF, ⟨[], none⟩)],
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)], [], []⟩

private def residualMemory (project : Project) (checks : List OperationCheck) :
    Memory testLowering testProfile :=
  let block : MemoryBlock testLowering := ⟨⟨0⟩, [],
    [⟨testOrigin, .op (opcode := .arithmetic (.plain .div)) (types := [()]) (ty := ())
      ⟨⟨storeRoot, []⟩, .virtual⟩ ()
      (.cons (.literal ⟨[0]⟩) .nil) checks⟩], .trap .abort testOrigin, []⟩
  ⟨project, Exec.Demo.mainF, (residualMIR project).signatures,
    [(Exec.Demo.mainF, [(storeRoot, ())], ⟨⟨0⟩, #[block]⟩)], [],
    [(Exec.Demo.mainF, [(storeRoot, .virtual)])], [], []⟩

-- The residual trap takes place at the same operation in both IRs, with
-- no synthetic boolean local. Deleting the check loses the observation.
example (project : Project) :
    let trap := .trapped 134 .divisionByZero testOrigin.lexical
      (trapDiagnostic project .divisionByZero testOrigin.operation testOrigin.lexical)
    (residualMIR project).terminalObservation (language := residualLanguage) storeInitial = some trap ∧
    (residualMemory project [runtimeCheck]).terminalObservation
      (language := residualLanguage) storeInitial = some trap ∧
    (residualMemory project []).terminalObservation
      (language := residualLanguage) storeInitial = none := by
  with_unfolding_all exact ⟨rfl, rfl, rfl⟩

private def residualObserved (project : Project) : Observation :=
  .trapped 134 .divisionByZero testOrigin.lexical
    (trapDiagnostic project .divisionByZero testOrigin.operation testOrigin.lexical)

private def residualRuntime (project : Project) : Concrete.Contracts.Observation :=
  .trapped 134 .divisionByZero
    ⟨⟨testOrigin.operation.id⟩, ⟨"", testOrigin.lexical.line, testOrigin.lexical.col⟩⟩
    (residualObserved project).stderr

private theorem residualSame (project : Project) :
    (residualObserved project).SameRuntimeResult (residualRuntime project) :=
  ⟨rfl, rfl, rfl, rfl, rfl⟩

private def residualModel (A : Type) (project : Project) : Model A Nat :=
  terminalModel A (residualObserved project) (residualRuntime project) (residualSame project)

private def residualModels (m : LoweringModels residualLanguage testLowering testProfile)
    (project : Project) : LoweringModels residualLanguage testLowering testProfile :=
  {m with
    mir := residualModel _ project
    memory := residualModel _ project
    mirState := fun _ => storeInitial
    memoryState := fun _ => storeInitial}

private theorem residualStops (project : Project) (next : MemoryRegisters residualLanguage testProfile) :
    ¬ (residualMemory project [runtimeCheck]).Step (language := residualLanguage) storeInitial next := by
  intro tick
  have noOutcome (s : MemoryRegisters residualLanguage testProfile) (mode : Bool)
      (raw : (residualMemory project [runtimeCheck]).StepOutcome (language := residualLanguage) storeInitial s mode) : False := by
    obtain ⟨block, fetched, step⟩ := raw
    have equality : some _ = some block := fetched
    cases equality
    obtain ⟨values, state, _, _, _, _, _, read, checked, _⟩ := step
    have readExact : some ([.scalar (.int .u8 0)], storeInitial) = some (values, state) := read
    cases readExact
    have impossible : some TrapKind.divisionByZero = none := by with_unfolding_all exact checked
    contradiction
  exact noOutcome _ false (tick.normal (language := residualLanguage) fun s => noOutcome s true)

private theorem residualLogicalStops (project : Project)
    (next : MemoryRegisters residualLanguage testProfile) :
    ¬ (residualMIR project).Step (language := residualLanguage) storeInitial next := by
  intro step
  have impossible : some TrapKind.divisionByZero = none := step.1
  contradiction

private theorem residualTables (project : Project) :
    (residualMemory project [runtimeCheck]).TablesWF := by
  refine ⟨?_, ?_, ?_, ?_, ?_, nofun, ?_⟩
  · change [Exec.Demo.mainF].Nodup
    simp
  · change [Exec.Demo.mainF].Nodup
    simp
  · change [Exec.Demo.mainF].Nodup
    simp
  · change ([] : List FnId).Nodup
    simp
  · intro _ _ member
    cases List.mem_singleton.mp member
    decide
  · intro fn locals graph member
    cases List.mem_singleton.mp member
    refine ⟨by decide, ⟨by decide, ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩, ?_⟩
    · intro block inGraph
      cases List.mem_singleton.mp inGraph
      exact fun _ => nofun
    · refine ⟨_, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩
      intro block inGraph
      cases List.mem_singleton.mp inGraph
      refine ⟨trivial, ?_⟩
      intro op inBlock
      cases List.mem_singleton.mp inBlock
      trivial

-- The complete contract has a model for a branch-free operation that traps at
-- its residual check. The same models reject dropping the check.
example (project : Project) (m : LoweringModels residualLanguage testLowering testProfile) :
    MemorySimulationWitness (residualModels m project) (residualMIR project) (residualMemory project [runtimeCheck]) ∧
    ¬ MemorySimulationWitness (residualModels m project) (residualMIR project) (residualMemory project []) := by
  constructor
  · refine ⟨rfl, rfl, ⟨fun _ h => h, fun _ h => h⟩, ?_, rfl, ?_, ?_, ?_, ?_, ?_, ?_⟩
    · constructor
      · intro fn locals graph member place ty localMember
        cases List.mem_singleton.mp member
        have localEntry : (place, ty) ∈ ([(storeRoot, ())] : LocalTypes Unit) := localMember
        cases List.mem_singleton.mp localEntry
        exact ⟨_, rfl, Or.inl rfl⟩
      · nofun
    · have noEscape : ¬ (residualMIR project).Escapes (language := residualLanguage)
          Exec.Demo.mainF storeRoot := by
        have ops : (residualMIR project).operations =
            [⟨Exec.Demo.mainF, ⟨0⟩, 0, testOrigin, .operator (.arithmetic (.plain .div)),
              [((), .literal [0])], [runtimeCheck]⟩] := by with_unfolding_all rfl
        rintro ⟨op, member, _, ty, path, addressed⟩
        have singleton : op ∈ [⟨Exec.Demo.mainF, ⟨0⟩, 0, testOrigin,
            .operator (.arithmetic (.plain .div)), [((), .literal [0])], [runtimeCheck]⟩] :=
          ops ▸ member
        cases List.mem_singleton.mp singleton
        exact OperandRead.noConfusion (congrArg Prod.snd (List.mem_singleton.mp addressed))
      constructor
      · intro fn locals graph member place ty localMember
        cases List.mem_singleton.mp member
        have localEntry : (place, ty) ∈ ([(storeRoot, ())] : LocalTypes Unit) := localMember
        cases List.mem_singleton.mp localEntry
        exact ⟨_, .virtual, rfl, rfl, ⟨fun _ => ⟨rfl, noEscape⟩, fun _ => rfl⟩⟩
      · intro fn classes place member localMember
        simp only [residualMemory, List.mem_singleton, Prod.mk.injEq] at member
        rcases member with ⟨rfl, rfl⟩
        simp only [List.mem_singleton, Prod.mk.injEq, and_true] at localMember
        subst place
        exact ⟨(), _, _, _, _, List.mem_singleton_self _, List.mem_singleton_self _,
          List.mem_singleton_self _, List.mem_singleton_self _, rfl, noEscape⟩
    · refine ⟨⟨fun _ _ => ?_, fun _ _ => ?_⟩,
        fun _ _ _ h => h.elim, fun _ next _ step => (residualStops project next step).elim, ?_⟩
      · exact ⟨rfl, rfl, ⟨_, rfl, rfl⟩, ⟨_, rfl, rfl⟩, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
      · with_unfolding_all rfl
      · intro _ _ _
        with_unfolding_all exact ⟨fun h => congrArg some h.symm, fun h => (Option.some.inj h).symm⟩
    · refine ⟨⟨fun _ _ => ?_, fun _ _ => ?_⟩,
        fun _ _ _ h => h.elim, fun _ next _ step => (residualLogicalStops project next step).elim, ?_⟩
      · with_unfolding_all
          exact ⟨rfl, rfl, ⟨_, rfl, rfl⟩, ⟨_, rfl, rfl⟩, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
      · with_unfolding_all rfl
      · intro _ _ _
        with_unfolding_all exact ⟨fun h => congrArg some h.symm, fun h => (Option.some.inj h).symm⟩
    · exact residualTables project
    · have view : (residualMIR project).logicalView (language := residualLanguage) =
          residualMemory project [runtimeCheck] := by
        with_unfolding_all rfl
      exact view.symm ▸ residualTables project
    · refine ⟨terminalRefines (same := residualSame project), ?_, ?_⟩
      · refine ⟨fun _ _ => True, fun _ _ => 0, fun _ _ => ⟨(), trivial, trivial⟩, ?_, ?_⟩
        · exact fun _ _ _ _ _ _ tick => tick.elim
        · exact fun p _ _ _ _ _ result => ⟨p, .nil _, result, rfl⟩
      · intro _ _ _ _ _
        refine ⟨?_, rfl, nofun, ⟨rfl, nofun⟩, List.nodup_nil, rfl, rfl⟩
        change (residualMemory project [runtimeCheck]).RepresentsActivation
          (language := residualLanguage) storeInitial.toMemoryFrame storeInitial.toMemoryFrame Mem.Memory.empty
        exact ⟨⟨⟨⟨nofun, by decide, nofun, nofun, nofun, nofun, nofun, nofun⟩,
          by decide, by decide, nofun⟩, rfl⟩, rfl, nofun, by decide, by decide, by decide, by decide,
          nofun, (fun _ => ⟨nofun, nofun⟩), nofun, nofun, nofun, nofun⟩
  · intro lowers
    have exactTarget := lowers.2.2.2.2.2.2.1.2.2.2 () (residualObserved project)
      ⟨(), trivial, .nil _⟩
    have impossible : none = some (residualObserved project) := exactTarget.mp rfl
    contradiction

end StageInterfaceControls
