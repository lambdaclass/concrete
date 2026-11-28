import ConcreteChecks.StageInterfaces.Memory.Fixtures
import ConcreteChecks.StageInterfaces.Refinement

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

private def callingSSA (kind : TrapKind) : SSA testLowering testProfile :=
  let entry : Block _ _ _ := ⟨⟨0⟩, [], [⟨testOrigin, .call callee none []⟩],
    .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩
  let helper : Block _ _ _ := ⟨⟨0⟩, [], [], .trap kind testOrigin, []⟩
  ⟨tabProject, Exec.Demo.mainF, [(callee, ⟨[], none⟩)],
    [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[entry]⟩), (callee, [], ⟨⟨0⟩, #[helper]⟩)], [], [], []⟩

abbrev memoryExecution := Memory.Execution (language := testLanguage)
  (lang := testLowering) (profile := testProfile)

abbrev ssaExecution := SSA.Execution (language := testLanguage)
  (lang := testLowering) (profile := testProfile)

private def helperState : MemoryRegisters testLanguage testProfile :=
  {storeInitial with frame := callee, callers := [⟨storeInitial.advance.toMemoryFrame, none⟩]}

def helperTrap (kind : TrapKind) : Concrete.Contracts.Observation :=
  .trapped 134 kind ⟨⟨testSite.id⟩, ⟨mainSourcePath.render.text, span.line, span.col⟩⟩
    (trapDiagnostic tabProject kind testSite span)

private theorem callAtEntry (kind : TrapKind) :
    memoryExecution.step ((callingSSA kind).memoryView (language := testLanguage)) storeInitial helperState := by
  with_unfolding_all
    exact ⟨rfl, ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩, _, rfl⟩

private theorem callOutcomeUnique {startDefinitions : Bool} (kind : TrapKind) (q : memoryExecution.State) (mode : Bool)
    (raw : ((callingSSA kind).memoryView (language := testLanguage)).StepOutcome storeInitial q mode startDefinitions) :
    q = helperState ∧ mode = false := by
  obtain ⟨block, found, call⟩ := raw
  let entry : MemoryBlock testLowering := ⟨⟨0⟩, [], [⟨testOrigin, .call callee none []⟩],
    .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩
  have fetched : some entry = some block := by with_unfolding_all exact found
  have sameBlock := Option.some.inj fetched
  subst block
  change _ ∧ ((callingSSA kind).memoryView (language := testLanguage)).Call callee none [] storeInitial q mode at call
  obtain ⟨values, afterRead, graph, calleeEntry, read, graphFound, blockFound, _, params⟩ := call.2
  have read' : some ([], storeInitial) = some (values, afterRead) := read
  obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj read')
  let helper : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .trap kind testOrigin, []⟩
  have graphFound' : some ⟨⟨0⟩, #[helper]⟩ = some graph := by with_unfolding_all exact graphFound
  have sameGraph := Option.some.inj graphFound'
  subst graph
  have found' : some helper = some calleeEntry := by with_unfolding_all exact blockFound
  have sameEntry := Option.some.inj found'
  subst calleeEntry
  with_unfolding_all exact Memory.ParamOutcome.empty params

private theorem callAtEntryUnique {startDefinitions : Bool} (kind : TrapKind) (q : memoryExecution.State)
    (tick : ((callingSSA kind).memoryView (language := testLanguage)).Step storeInitial q startDefinitions) :
    q = helperState := by
  exact (callOutcomeUnique kind _ _ (Memory.Step.normal tick fun _ h => Bool.noConfusion (callOutcomeUnique kind _ _ h).2)).1

-- The mutation changes only the reached helper's trap. All metadata is retained.
set_option maxRecDepth 4096 in
example :
    let source := (callingSSA .divisionByZero).memoryView (language := testLanguage)
    ¬ BuildsSSA (language := testLanguage) source (callingSSA .abort) := by
  dsimp only
  intro lowered
  let source := (callingSSA .divisionByZero).memoryView (language := testLanguage)
  have terminal : memoryExecution.result source helperState (helperTrap .divisionByZero) := by
    with_unfolding_all rfl
  have limits : ∀ q, memoryExecution.finite source q → q = storeInitial ∨ q = helperState := by
    rintro q ⟨p, starts, path⟩
    have eq : p = storeInitial := by with_unfolding_all exact starts
    rw [eq] at path
    cases path with
    | nil => exact .inl rfl
    | cons tick rest =>
      obtain rfl := callAtEntryUnique .divisionByZero _ tick.2.1
      cases rest with
      | nil => exact .inr rfl
      | cons next _ => exact (memoryExecution.terminal _ _ _ terminal _ next).elim
  have safe : ProgramSafe memoryExecution source := by
    intro q stuck
    rcases limits q stuck.1 with rfl | rfl
    · exact stuck.2.1 helperState (callAtEntry .divisionByZero)
    · exact stuck.2.2 _ terminal
  have targetTick : ssaExecution.step (callingSSA .abort) storeInitial helperState := by
    with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩, _, rfl⟩
  have targetResult : ssaExecution.result (callingSSA .abort) helperState (helperTrap .abort) := by
    with_unfolding_all rfl
  obtain ⟨q, reached, result, _⟩ := (lowered.2.2.2.2.2.2 safe).1 helperState (helperTrap .abort)
    ⟨storeInitial, by with_unfolding_all rfl, .cons targetTick (.nil _)⟩ targetResult
  rcases limits q reached with rfl | rfl
  · have absent : (none : Option Concrete.Contracts.Observation) = some (helperTrap .abort) := by
      with_unfolding_all exact result
    cases absent
  · have wrong : some (helperTrap .divisionByZero) = some (helperTrap .abort) := by
      with_unfolding_all exact result
    have eq := Option.some.inj wrong
    change Concrete.Contracts.Observation.trapped _ _ _ _ = _ at eq
    cases eq

-- The reachable-helper trap substitution above has a positive counterpart
-- on the very same Memory → SSA edge.
set_option maxRecDepth 4096 in
set_option maxHeartbeats 1400000 in
example :
    let ssa := callingSSA .divisionByZero
    BuildsSSA (language := testLanguage) (ssa.memoryView (language := testLanguage)) ssa := by
  let ssa := callingSSA .divisionByZero
  let memory := ssa.memoryView (language := testLanguage)
  let outcome := helperTrap .divisionByZero
  have hres : ResidualAddsMatch memory ssa := by
    constructor
    · intro fn mLocals mGraph hFn ⟨mBlock, hMB, mOp, hMO, hRes⟩
      have hMem : (fn, mLocals, mGraph) ∈
          [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨testOrigin, .call callee none []⟩],
              .ret (some ⟨(), .literal ⟨[0]⟩⟩), []⟩]⟩),
           (callee, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .trap .divisionByZero testOrigin, []⟩]⟩)] := by
        with_unfolding_all exact hFn
      rcases List.mem_cons.mp hMem with h1 | h2
      · cases h1
        cases List.mem_singleton.mp hMB
        cases List.mem_singleton.mp hMO
        cases hRes
      · cases List.mem_singleton.mp h2
        cases List.mem_singleton.mp hMB
        cases hMO
    · intro fn sLocals sGraph hFn sBlock hSB sOp hSO
      simp only [ssa, callingSSA, List.mem_cons, List.not_mem_nil, or_false, Prod.mk.injEq] at hFn
      rcases hFn with ⟨rfl, rfl, rfl⟩ | ⟨rfl, rfl, rfl⟩
      · cases List.mem_singleton.mp hSB
        cases List.mem_singleton.mp hSO
        trivial
      · cases List.mem_singleton.mp hSB
        cases hSO
  refine ⟨rfl, rfl, rfl, ⟨rfl, rfl⟩, by with_unfolding_all exact ⟨fun _ h => h, fun _ h => h⟩, hres, ?_⟩
  have targetTick : ssaExecution.step ssa storeInitial helperState := by
    with_unfolding_all exact ⟨rfl, ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩, _, rfl⟩
  have targetOnly (q : ssaExecution.State) (tick : ssaExecution.step ssa storeInitial q) :
      q = helperState := by
    exact callAtEntryUnique (startDefinitions := true) .divisionByZero q tick.2.1
  have terminal : ssaExecution.result ssa helperState outcome := by with_unfolding_all rfl
  have reachable (q : ssaExecution.State) (finite : ssaExecution.finite ssa q) :
      q = storeInitial ∨ q = helperState := by
    obtain ⟨start, initial, path⟩ := finite
    have equal : start = storeInitial := by with_unfolding_all exact initial
    rw [equal] at path
    cases path with
    | nil => exact .inl rfl
    | cons step rest =>
      obtain rfl := targetOnly _ step
      cases rest with
      | nil => exact .inr rfl
      | cons step _ => exact (ssaExecution.terminal _ _ _ terminal _ step).elim
  refine finiteProgressRefines memoryExecution ssaExecution memory ssa outcome helperState
    ⟨storeInitial, by with_unfolding_all rfl, .cons (callAtEntry .divisionByZero) (.nil _)⟩
    (by with_unfolding_all rfl) ?_ ?_ (fun q => if q.frame == callee then 0 else 1) ?_
  · intro q finite
    rcases reachable q finite with rfl | rfl
    · exact .inl ⟨_, targetTick⟩
    · exact .inr terminal
  · intro q o finite result
    rcases reachable q finite with rfl | rfl
    · have absent : none = some o := by with_unfolding_all exact result
      cases absent
    · have same : some outcome = some o := by with_unfolding_all exact result
      exact ⟨(Option.some.inj same).symm, by with_unfolding_all rfl⟩
  · intro q q' finite step
    rcases reachable q finite with rfl | rfl
    · obtain rfl := targetOnly q' step
      exact (by decide : 0 < 1)
    · exact (ssaExecution.terminal _ _ _ terminal _ step).elim

end StageInterfaceControls
