import ConcreteChecks.StageInterfaces.BackendFixtures
import ConcreteChecks.StageInterfaces.Core.Lowering

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

private def abortMemory (kind : TrapKind) : Memory testLowering testProfile :=
  (abortMIR kind).logicalView (language := testLanguage)

private def abortSSA (kind : TrapKind) : SSA testLowering testProfile :=
  let block : Block _ _ _ := ⟨⟨0⟩, [], [], .trap kind testOrigin, []⟩
  ⟨tabProject, Exec.Demo.mainF, (abortMIR kind).signatures,
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)], [], [], []⟩

private def abortBIR (kind : TrapKind) : BIR testLowering testProfile :=
  let block := {birReturnBlock false with ending := .trap ⟨0⟩}
  ⟨tabProject, Exec.Demo.mainF, (abortMIR kind).signatures,
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)], [], #[⟨kind, testOrigin⟩], [], []⟩

private def abortJob (kind : TrapKind) : LCBInput testProfile :=
  let diagnostic :=
    s!"concrete: trap: {kind.name} at {mainSourcePath.render.text}:{span.line}:{span.col} (site {testSite.id})\n"
  {lcbReturnJob 256 with
    sites := (abortBIR kind).sites
    runtimeInput := (Backend.Runtime.qbeDiagnostics [diagnostic]).toUTF8}

private theorem abortTables (kind : TrapKind) : (abortMemory kind).TablesWF := by
  refine ⟨?_, ?_, ?_, List.nodup_nil, ?_, nofun, ?_⟩
  · with_unfolding_all change [Exec.Demo.mainF].Nodup
    simp
  · change [Exec.Demo.mainF].Nodup
    simp
  · with_unfolding_all change [Exec.Demo.mainF].Nodup
    simp
  · intro fn cls included
    with_unfolding_all change (fn, cls) ∈ [(Exec.Demo.mainF, [])] at included
    cases List.mem_singleton.mp included
    exact List.nodup_nil
  · intro fn locals graph included
    let block : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .trap kind testOrigin, []⟩
    have expected : (fn, locals, graph) ∈ [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)] := by
      with_unfolding_all exact included
    cases List.mem_singleton.mp expected
    refine ⟨List.nodup_nil, ⟨by simp, ⟨block, List.mem_singleton_self _, rfl⟩, ?_⟩,
      _, rfl, ⟨block, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩
    · intro _ member _ edge
      cases List.mem_singleton.mp member
      exact (List.not_mem_nil edge).elim
    · intro _ member
      cases List.mem_singleton.mp member
      exact ⟨trivial, nofun⟩

private theorem abortMemoryCheckpoint :
    (abortMemory .abort).RepresentsCheckpoint storeInitial storeInitial := by
  refine ⟨?_, rfl, nofun, ⟨rfl, nofun⟩, List.nodup_nil, rfl, rfl⟩
  change (abortMemory .abort).RepresentsActivation
    storeInitial.toMemoryFrame storeInitial.toMemoryFrame Mem.Memory.empty
  exact ⟨⟨⟨⟨nofun, by decide, nofun, nofun, nofun, nofun, nofun, nofun⟩,
    by decide, by decide, nofun⟩, rfl⟩, rfl, nofun, by decide, by decide, by decide, by decide,
    nofun, (fun _ => ⟨nofun, nofun⟩), nofun, nofun, nofun, nofun⟩

-- The same abort used in the Core→MIR pair is terminal at each of the
-- remaining stages. Each mutant changes only the output of its own edge.
set_option maxRecDepth 4096 in
set_option maxHeartbeats 1500000 in
example :
    LowersMemory (language := testLanguage) (abortMIR .abort) (abortMemory .abort) ∧
    ¬ LowersMemory (language := testLanguage) (abortMIR .abort) (abortMemory .divisionByZero) := by
  have source : mirExecution.result (abortMIR .abort) storeInitial (helperTrap .abort) :=
    by with_unfolding_all rfl
  have target : memoryExecution.result (abortMemory .abort) storeInitial (helperTrap .abort) :=
    by with_unfolding_all rfl
  have atSource : ∀ p, mirExecution.initial (abortMIR .abort) p → p = storeInitial :=
    fun _ h => by with_unfolding_all exact h
  have atTarget : ∀ q, memoryExecution.initial (abortMemory .abort) q → q = storeInitial :=
    fun _ h => by with_unfolding_all exact h
  constructor
  · refine ⟨rfl, rfl, ?_, ?_, ⟨rfl, rfl⟩, ?_, abortTables _, abortTables _, ?_, ?_⟩
    · with_unfolding_all exact ⟨fun _ x => x, fun _ x => x⟩
    · refine ⟨?_, nofun⟩
      intro fn ls graph included place ty member
      let block : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .trap .abort testOrigin, []⟩
      have original : (fn, ls, graph) ∈ [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)] := by
        with_unfolding_all exact included
      have empty : ls = [] := congrArg (fun fn => fn.2.1) (List.mem_singleton.mp original)
      exact (List.not_mem_nil (empty ▸ member)).elim
    · constructor
      · intro fn ls graph included place ty member
        let entry : MIRGraph testLowering := ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .trap .abort testOrigin, []⟩]⟩
        have original : (fn, ls, graph) ∈ [(Exec.Demo.mainF, [], entry)] := included
        have empty : ls = [] := congrArg (fun fn => fn.2.1) (List.mem_singleton.mp original)
        exact (List.not_mem_nil (empty ▸ member)).elim
      · intro fn classes place included member
        have original : (fn, classes) ∈ [(Exec.Demo.mainF, [])] := by
          with_unfolding_all exact included
        cases List.mem_singleton.mp original
        exact (List.not_mem_nil member).elim
    · refine stationaryRefines _ _ _ _ storeInitial storeInitial
        (helperTrap .abort) (helperTrap .abort)
        (fun _ => by with_unfolding_all rfl) (fun _ => by with_unfolding_all rfl)
        source target source ?_ ?_
      · intro o
        with_unfolding_all exact ⟨fun h => (Option.some.inj h).symm, fun h => congrArg some h.symm⟩
      · with_unfolding_all rfl
    · intro _
      let related : mirExecution.State → memoryExecution.State → Prop :=
        fun p q => p = storeInitial ∧ q = storeInitial
      refine ⟨⟨related, fun _ _ => 0, ?_, ?_, ?_⟩, ?_⟩
      · exact fun q starts => ⟨storeInitial, by with_unfolding_all rfl, rfl, atTarget q starts⟩
      · rintro _ _ q' _ _ ⟨rfl, rfl⟩ tick
        exact (memoryExecution.terminal _ _ _ target q' tick).elim
      · rintro _ _ o _ _ ⟨rfl, rfl⟩ observes
        have equal := Option.some.inj (target.symm.trans observes)
        exact ⟨_, .nil _, equal ▸ source, by with_unfolding_all rfl⟩
      · rintro _ _ _ _ ⟨rfl, rfl⟩
        exact ⟨abortMemoryCheckpoint, rfl⟩
  · intro lowered
    apply stationaryRejects (b := abortMemory .divisionByZero) mirExecution memoryExecution
      (Memory.MatchingResult (language := testLanguage)) storeInitial storeInitial
      (helperTrap .abort) (helperTrap .divisionByZero) atSource source
      (by with_unfolding_all rfl) (by with_unfolding_all rfl) ?_
      lowered.2.2.2.2.2.2.2.2.1
    intro observes
    have wrong : some (helperTrap .abort) = some (helperTrap .divisionByZero) := by
      with_unfolding_all exact observes
    have equal := Option.some.inj wrong
    change Concrete.Contracts.Observation.trapped _ .abort _ _ =
      Concrete.Contracts.Observation.trapped _ .divisionByZero _ _ at equal
    cases equal

set_option maxRecDepth 4096 in
example :
    BuildsSSA (language := testLanguage) (abortMemory .abort) (abortSSA .abort) ∧
    ¬ BuildsSSA (language := testLanguage) (abortMemory .abort) (abortSSA .divisionByZero) := by
  constructor
  · refine ⟨rfl, rfl, rfl, ⟨rfl, rfl⟩, by with_unfolding_all exact ⟨fun _ h => h, fun _ h => h⟩, ?_⟩
    refine stationaryRefines _ _ _ _ storeInitial storeInitial
      (helperTrap .abort) (helperTrap .abort)
      (fun _ => by with_unfolding_all rfl) (fun _ => by with_unfolding_all rfl) ?_ ?_ ?_ ?_ ?_
    all_goals first
      | (intro o; with_unfolding_all exact ⟨fun h => (Option.some.inj h).symm, fun h => congrArg some h.symm⟩)
      | with_unfolding_all rfl
  · intro formed
    apply stationaryRejects memoryExecution ssaExecution ssaExecution.result storeInitial storeInitial
      (helperTrap .abort) (helperTrap .divisionByZero) (fun _ h => by with_unfolding_all exact h)
      (by with_unfolding_all rfl) (by with_unfolding_all rfl) (by with_unfolding_all rfl) ?_
      formed.2.2.2.2.2
    intro observes
    have wrong : some (helperTrap .abort) = some (helperTrap .divisionByZero) := by
      with_unfolding_all exact observes
    have equal := Option.some.inj wrong
    change Concrete.Contracts.Observation.trapped _ .abort _ _ =
      Concrete.Contracts.Observation.trapped _ .divisionByZero _ _ at equal
    cases equal

set_option maxRecDepth 4096 in
example :
    BuildsBIR (language := testLanguage) (abortSSA .abort) (abortBIR .abort) ∧
    ¬ BuildsBIR (language := testLanguage) (abortSSA .abort) (abortBIR .divisionByZero) := by
  constructor
  · refine ⟨rfl, ?_, ?_, ?_, by decide, ⟨rfl, rfl, by with_unfolding_all rfl⟩,
      BIR.refinesOfViewEq (by with_unfolding_all rfl)⟩
    · intro fn member block included slot trapped
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp included
      cases BIRTerm.trap.inj trapped
      exact ⟨⟨.abort, testOrigin⟩, rfl⟩
    · intro fn member block included kind origin trapped
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp included
      cases Terminator.trap.inj trapped with
      | intro kindEq originEq =>
        subst kind
        subst origin
        exact ⟨_, List.mem_singleton_self _, rfl, _, List.mem_singleton_self _, rfl,
          ⟨0⟩, rfl, ⟨.abort, testOrigin⟩, rfl, rfl, rfl⟩
    · with_unfolding_all exact ⟨fun _ h => h, fun _ h => h⟩
  · intro formed
    have same := congrArg (fun block => do
      let graph ← block
      let entry ← graph.blocks[0]?
      match entry.ending with
      | Terminator.trap kind _ => some kind
      | _ => none) formed.2.2.2.2.2.1.2.2
    have wrong : some TrapKind.abort = some TrapKind.divisionByZero := by with_unfolding_all exact same
    cases wrong

set_option maxRecDepth 4096 in
example :
    EmitsLCB testLanguage (abortBIR .abort) (abortJob .abort) ∧
    ¬ EmitsLCB testLanguage (abortBIR .abort) {abortJob .abort with body := (lcbReturnJob 0).body} := by
  constructor
  · refine ⟨rfl, by with_unfolding_all rfl, nofun, by simp [abortJob, lcbReturnJob], rfl,
      ⟨_, by with_unfolding_all rfl, by with_unfolding_all rfl⟩, ?_,
      by simp [abortBIR], by simp [abortJob, lcbReturnJob], fun _ => Iff.rfl,
      by with_unfolding_all rfl, ?_⟩
    · refine stationaryRefines _ _ _ _ _ _ (helperTrap .abort) (helperTrap .abort)
        (fun _ => Iff.rfl) (fun _ => Iff.rfl) ?_ ?_ ?_ ?_ ?_
      all_goals first
        | (intro o; with_unfolding_all exact ⟨fun h => (Option.some.inj h).symm, fun h => congrArg some h.symm⟩)
        | with_unfolding_all rfl
    · intro fn member block included slot trapped
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp included
      cases BIRTerm.trap.inj trapped
      exact ⟨⟨.abort, testOrigin⟩, rfl⟩
  · intro emitted
    apply stationaryRejects birExecution (LCBInput.Execution Nat tabProject) (LCBInput.Execution Nat tabProject).result
      (BIR.entryState (language := testLanguage) (abortBIR .abort)) (some ⟨0, 0, #[]⟩)
      (helperTrap .abort) (.returned 0 ByteArray.empty) (fun _ h => h)
      (by with_unfolding_all rfl) (by with_unfolding_all rfl) (by with_unfolding_all rfl) ?_
      emitted.2.2.2.2.2.2.1
    intro observed
    have wrong : some (helperTrap .abort) = some (.returned 0 ByteArray.empty) := by
      with_unfolding_all exact observed
    have equal := Option.some.inj wrong
    change Concrete.Contracts.Observation.trapped _ .abort _ _ =
      Concrete.Contracts.Observation.returned 0 _ at equal
    cases equal

end StageInterfaceControls
