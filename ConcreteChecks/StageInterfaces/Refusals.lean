import ConcreteChecks.StageInterfaces.Backends
import ConcreteChecks.StageInterfaces.LiteralPipeline

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

private def invalidTrapBlock : Block (Sigma (SSAValue Unit)) (LocatedOp (SSAInst testLowering))
    (BIRTerm (Sigma (SSARead Unit testLowering.literal)) (SSARead Unit testLowering.literal ())) :=
  ⟨⟨0⟩, [], [], .trap ⟨0⟩, []⟩

private def invalidTrapCallee (project : Project) : BIR testLowering testProfile :=
  let entry := {birReturnBlock false with body := [⟨testOrigin, .call callee none []⟩]}
  {(birReturn project false) with
    functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[entry]⟩), (callee, [], ⟨⟨0⟩, #[invalidTrapBlock]⟩)]
    signatures := [(callee, ⟨[], none⟩)]}

-- A call to a void helper with a valid trap slot is refused before emission.
example (m : LoweringModels testLanguage testLowering testProfile) (parser : Parser grammar) :
    let bir := {invalidTrapCallee tabProject with sites := #[trapEntry]}
    (r m parser).emitLCB.run bir = .error .lcbLinkingUnavailable := by
  with_unfolding_all rfl

-- The same call block is accepted when disconnected and refused when its
-- predecessor jumps to it. The array order differs from traversal order.
example (parser : Parser grammar) :
    let callBlock := {birReturnBlock false with
      id := ⟨1⟩
      body := [⟨testOrigin, .call callee none []⟩]}
    let entry (linked : Bool) :=
      if linked then {birReturnBlock false with ending := .jump ⟨⟨1⟩, []⟩}
      else birReturnBlock false
    let input (linked : Bool) : BIR testLowering testProfile :=
      {bundledBIR tabProject with
        functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[callBlock, entry linked]⟩),
          (callee, [], ⟨⟨0⟩, #[birReturnBlock true]⟩)]}
    let stages := fixtureEntries parser {testPasses [] with emitLCB := fun _ => .ok (bundledJobs 7)}
    stages.emitLCB.run (input false) = .ok (bundledJobs 7) ∧
      stages.emitLCB.run (input true) = .error .lcbLinkingUnavailable := by
  with_unfolding_all exact ⟨rfl, rfl⟩

-- The verified job edge requires no external behavior hypothesis.
example (m : LoweringModels testLanguage testLowering testProfile) (parser : Parser grammar) :
    (r m parser).emitLCB.Trusted := fun _ _ _ _ => trivial

private theorem oversizedLCBRefused (passes : Passes slice0 testProfile)
    (bir : BIR returnLowering testProfile) (over : LCBInput.trapCapacity < bir.sites.size) :
    let edge := (registry passes).emitLCB
    let failure := StageFailure.resource (LCBInput.trapCapacityRefusal bir.sites.size)
    edge.run bir = .error failure ∧ edge.refusal bir failure := by
  refine ⟨?_, rfl, over⟩
  change passes.emitSingleLCB bir = .error _
  simp [PassFunctions.emitSingleLCB, over]

private def singletonTrapBIR (sites : TrapTable) (slot : TrapIndex) :
    BIR returnLowering testProfile :=
  ⟨tabProject, earlyEntry, [(earlyEntry, ⟨[], none⟩)],
    [(earlyEntry, [], singleBlockGraph (.trap slot) [])], [], sites, [], []⟩

private theorem singletonTrapWF (sites : TrapTable) (slot : TrapIndex)
    (resolves : ∃ entry, sites[slot.index]? = some entry)
    (locations : ∀ site ∈ sites.toList, ∃ span, site.origin.lexical.Binds tabProject span) :
    BIR.WF (language := earlyFixtureLanguage) (singletonTrapBIR sites slot) := by
  obtain ⟨entry, found⟩ := resolves
  refine ⟨?_, Nat.zero_le _, .cons nofun .nil, List.mem_singleton_self _, locations, ?_, nofun⟩
  · intro fn member block inside index trapped
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp inside
    cases BIRTerm.trap.inj trapped
    exact ⟨entry, found⟩
  · with_unfolding_all
      refine ⟨.cons nofun .nil, .cons nofun .nil, .cons nofun .nil, .nil, ?_, nofun, ?_⟩
      · intro fn classes member
        cases List.mem_singleton.mp member
        exact .nil
      · intro fn locals graph member
        cases List.mem_singleton.mp member
        refine ⟨.nil, ⟨.cons nofun .nil, ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
          ⟨_, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩⟩
        · intro block inside edge follows
          cases List.mem_singleton.mp inside
          change edge ∈ (match sites[slot.index]? with
            | some site => (.trap site.kind site.origin : MemoryTerm returnLowering)
            | none => .invalid).successors at follows
          rw [found] at follows
          cases follows
        · intro block inside
          let terminator : MemoryTerm returnLowering :=
            match sites[slot.index]? with
            | some site => .trap site.kind site.origin
            | none => .invalid
          let decoded : MemoryBlock returnLowering := ⟨⟨0⟩, [], [], terminator, []⟩
          have blockEq : block = decoded := List.mem_singleton.mp inside
          have endingEq : decoded.ending = .trap entry.kind entry.origin := by
            change terminator = _
            simp only [terminator, found]
          rw [blockEq]
          constructor
          · rw [endingEq]
            trivial
          · exact nofun

-- Prove the table facts for a symbolic length so the kernel never constructs
-- the oversized array.
private def syntheticTrapSites (n : Nat) : TrapTable :=
  (Array.replicate n ⟨.divisionByZero, testOrigin⟩).push ⟨.abort, testOrigin⟩

private theorem syntheticTrapSize (n : Nat) : (syntheticTrapSites n).size = n + 1 := by
  simp [syntheticTrapSites]

private theorem syntheticTrapWF (n : Nat) :
    BIR.WF (language := earlyFixtureLanguage) (singletonTrapBIR (syntheticTrapSites n) ⟨n⟩) := by
  apply singletonTrapWF
  · change ∃ entry, (syntheticTrapSites n)[n]? = some entry
    exact ⟨⟨.abort, testOrigin⟩, by
      simp only [syntheticTrapSites, Array.getElem?_push, Array.size_replicate, ite_true]⟩
  · intro site member
    have same : site.origin = testOrigin := by
      simp only [syntheticTrapSites, Array.toList_push, Array.toList_replicate, List.mem_append,
        List.mem_singleton] at member
      rcases member with repeated | rfl
      · rw [List.eq_of_mem_replicate repeated]
      · rfl
    rw [same]
    let checkedSpan : ConcreteSpec.Core.Span :=
      ⟨⟨0⟩, 0, 1, by decide, 1, 1, by decide, by decide⟩
    exact ⟨checkedSpan, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private def OversizedTrapGate (n : Nat) : Prop :=
  let bir := singletonTrapBIR (syntheticTrapSites n) ⟨n⟩
  let passes := {literalReturnPasses with emitLCB := fun _ => .ok (lcbReturnJob 42)}
  let edge := (registry passes).emitLCB
  let failure := StageFailure.resource (LCBInput.trapCapacityRefusal bir.sites.size)
  bir.sites.size = LCBInput.trapCapacity + 1 ∧ edge.inputInvariant bir ∧
    edge.run bir = .error failure ∧ edge.refusal bir failure

set_option maxRecDepth 4096 in
private theorem oversizedTrapGate (n : Nat) (capacity : LCBInput.trapCapacity = n) :
    OversizedTrapGate n := by
  unfold OversizedTrapGate
  intro bir passes edge failure
  have size : bir.sites.size = LCBInput.trapCapacity + 1 := syntheticTrapSize n |>.trans
    (congrArg (· + 1) capacity.symm)
  exact ⟨size, syntheticTrapWF n, oversizedLCBRefused passes bir (size.symm ▸ Nat.lt_succ_self _)⟩

example : OversizedTrapGate LCBInput.trapCapacity := oversizedTrapGate _ rfl

-- A zero-slot self-loop remains a real graph and needs no result slot.
set_option maxRecDepth 4096 in
example :
    let ssa : SSA returnLowering testProfile :=
      ⟨tabProject, earlyEntry, [(earlyEntry, ⟨[], none⟩)],
        [(earlyEntry, [], singleBlockGraph (.jump ⟨⟨0⟩, []⟩) [])], [], [], []⟩
    let bir : BIR returnLowering testProfile :=
      ⟨tabProject, ssa.entry, ssa.signatures,
        [(earlyEntry, [], singleBlockGraph (.jump ⟨⟨0⟩, []⟩) [])], [], #[], [], []⟩
    let edge := (registry {literalReturnPasses with buildBIR := fun _ => .ok bir}).buildBIR
    ssa.frameSlots = 0 ∧ edge.inputInvariant ssa ∧ edge.outputInvariant bir ∧
      edge.run ssa = .ok bir ∧ edge.success ssa bir := by
  intro ssa bir edge
  have tables : (ssa.memoryView (language := earlyFixtureLanguage)).TablesWF := by
    with_unfolding_all
      refine ⟨.cons nofun .nil, .cons nofun .nil, .cons nofun .nil, .nil, ?_, nofun, ?_⟩
      · intro fn classes present
        cases List.mem_singleton.mp present
        exact .nil
      · intro fn locals graph present
        cases List.mem_singleton.mp present
        refine ⟨.nil, ⟨.cons nofun .nil, ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
          ⟨_, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩⟩
        · intro block inside edge target
          cases List.mem_singleton.mp inside
          cases List.mem_singleton.mp target
          exact ⟨_, List.mem_singleton_self _, rfl, rfl⟩
        · intro block inside
          cases List.mem_singleton.mp inside
          exact ⟨trivial, nofun⟩
  have noTrap : bir.TrapSlotsWF := by
    intro fn present block inside site trap
    cases List.mem_singleton.mp present
    cases List.mem_singleton.mp inside
    cases trap
  refine ⟨rfl, ⟨tables, List.mem_singleton_self _, by with_unfolding_all exact ⟨nofun, nofun⟩⟩,
    ⟨noTrap, by decide, .cons nofun .nil,
    List.mem_singleton_self _, nofun, ?_⟩, rfl,
    ⟨rfl, noTrap, ?_, ?_, by decide, ⟨rfl, rfl, by with_unfolding_all rfl⟩, ?_⟩, rfl⟩
  · with_unfolding_all exact ⟨tables, nofun⟩
  · intro fn present block inside kind site trap
    cases List.mem_singleton.mp present
    cases List.mem_singleton.mp inside
    cases trap
  · with_unfolding_all exact ⟨nofun, nofun⟩
  · exact BIR.refinesOfViewEq (by with_unfolding_all rfl)

-- A signature missing from an otherwise eligible return graph is rejected
-- by the decoded Memory tables at both backend entries.
example :
    ¬ BIR.WF (language := testLanguage) (birReturn tabProject false) := by
  intro wf
  have checked := wf.2.2.2.2.2.1
  obtain ⟨_, _, signature⟩ := checked.2.2.2.2.2.2 _ _ _ (List.mem_singleton_self _)
  obtain ⟨_, found, _⟩ := signature
  cases found

-- Missing and duplicate entries cannot enter the backend stage.
example :
    let input := birReturn tabProject false
    (¬ BIR.WF (language := testLanguage) ({input with functions := []} : BIR testLowering testProfile)) ∧
    (¬ BIR.WF (language := testLanguage) ({input with functions := input.functions ++ input.functions} : BIR testLowering testProfile)) := by
  refine ⟨fun h => ?_, fun h => ?_⟩
  · exact List.not_mem_nil h.2.2.2.1
  · simpa [birReturn] using h.2.2.1

example :
    let input := birReturn tabProject false
    let unbound := {testOrigin with lexical := {testOrigin.lexical with file := ⟨99⟩}}
    ¬ BIR.WF (language := testLanguage) ({input with sites := #[⟨.abort, unbound⟩]} : BIR testLowering testProfile) := by
  dsimp only
  intro invalid
  obtain ⟨_, _, found, _⟩ := invalid.2.2.2.2.1 _ (List.mem_singleton_self _)
  cases found

-- A spurious typed linking refusal cannot validate on the registered edge.
example (parser : Parser grammar) :
    let stages := fixtureEntries parser
      {testPasses (profile := testProfile) [] with emitLCB := fun _ => .error .lcbLinkingUnavailable}
    ¬ stages.emitLCB.Validated := by
  intro stages valid
  let input : BIR testLowering testProfile := {birReturn tabProject false with signatures := [(Exec.Demo.mainF, ⟨[], some ()⟩)]}
  have eligible : stages.emitLCB.inputInvariant input := by
    refine ⟨?_, (by decide : 0 ≤ 3), by simp [input, birReturn, testBIR], by simp [input, birReturn, testBIR], nofun, ?_⟩
    · intro fn member block included slot trap
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp included
      cases trap
    · exact ⟨birReturnTables tabProject testProfile, nofun⟩
  have wrong : false = true := by
    with_unfolding_all exact valid.2 input .lcbLinkingUnavailable eligible rfl
  cases wrong

private def atInvalidTrap (project : Project) : MemoryRegisters testLanguage testProfile :=
  {(BIR.entryState (invalidTrapCallee project)) with
    frame := callee
    callers := [⟨((BIR.entryState (invalidTrapCallee project)).advance).toMemoryFrame, none⟩]}

-- The void callee reaches the malformed terminator and can neither return to
-- its caller nor manufacture a terminal observation. The job edge refuses it.
set_option maxHeartbeats 1000000 in
example (project : Project) :
    birExecution.step (invalidTrapCallee project) (BIR.entryState (invalidTrapCallee project))
      (atInvalidTrap project) ∧
    (∀ q, ¬ birExecution.step (invalidTrapCallee project) (atInvalidTrap project) q) ∧
    (∀ o, ¬ birExecution.result (invalidTrapCallee project) (atInvalidTrap project) o) ∧
    ¬ EmitsLCB testLanguage (invalidTrapCallee project) (bundledJobs 42) := by
  let physical : MemoryBlock testLowering := ⟨⟨0⟩, [], [], .invalid, []⟩
  refine ⟨?_, ?_, ?_, ?_⟩
  · with_unfolding_all
      exact ⟨rfl, ⟨_, rfl, rfl, [], _, _, _, rfl, rfl, rfl, rfl, .nil _⟩, [], rfl⟩
  · intro q execution
    obtain ⟨_, _, block, fetched, tick⟩ := Memory.Step.exists_outcome execution.2.1
    have eq : some physical = some block := by with_unfolding_all exact fetched
    obtain rfl := Option.some.inj eq
    exact tick.2
  · intro observed result
    have impossible : none = some observed := by with_unfolding_all exact result
    cases impossible
  · intro emits
    obtain ⟨entry, found⟩ := emits.2.2.2.2.2.2.2.2.2.2.2
      (callee, [], ⟨⟨0⟩, #[invalidTrapBlock]⟩)
      (List.mem_cons_of_mem _ (List.mem_singleton_self _)) invalidTrapBlock
      (List.mem_singleton_self _) ⟨0⟩ rfl
    cases found

end StageInterfaceControls
