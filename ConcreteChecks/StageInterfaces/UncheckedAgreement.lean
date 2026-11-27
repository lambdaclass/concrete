import ConcreteSpec.Proofs.Memory
import ConcreteChecks.StageInterfaces.Core.Ownership
import ConcreteChecks.StageInterfaces.LiteralPipeline
import ConcreteChecks.StageInterfaces.Refinement

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

set_option maxRecDepth 4096 in
example :
    let edge := (registry literalReturnPasses).admission
    let core := elaboratedReturn 256
    edge.inputInvariant (.core core) ∧
      ∃ failure, edge.run (.core core) = .error failure ∧ edge.refusal (.core core) failure := by
  let reason := ConcreteSpec.Admission.Refusal.atPosition 0 (.literalOutOfRange .u8 256)
  refine ⟨⟨rfl, returnedCoreWF 256, _, representsReturn 256⟩, _, rfl, rfl, ?_, _, reason, representsReturn 256, rfl, rfl⟩
  exact fun accepted => return256IllTyped accepted.1.2.1

-- Compose both registered routes through LCB job validation, then observe the exact
-- emitted body with the fixed LCB semantics. This control exercises a single source.
set_option maxRecDepth 4096 in
example (unchecked : Bool) :
    (Registry.pipeline literalReturnPasses unchecked).run elaborationSource = .ok (lcbReturnJob 42) ∧
    ((registry literalReturnPasses).lcbPipeline unchecked).run elaborationSource =
      .ok (lcbReturnJob 42) ∧
    (LCBInput.Execution Empty elaborationSource.project).observes
      (lcbReturnJob 42) (.returned 42 ByteArray.empty) := by
  have ran : ((registry literalReturnPasses).lcbPipeline unchecked).run elaborationSource =
      .ok (lcbReturnJob 42) := by cases unchecked <;> with_unfolding_all rfl
  refine ⟨ran, ran, ?_⟩
  · with_unfolding_all exact ⟨_, ⟨_, rfl, .nil _⟩, rfl⟩

set_option maxRecDepth 4096 in
example (unchecked : Bool) :
    let mutant := { literalReturnPasses with
      lowerCore := fun _ => .error (.invalidInput rule)
      lowerUnchecked := fun _ => .error (.invalidInput rule) }
    ¬ ∃ output, (Registry.pipeline mutant unchecked).run elaborationSource = .ok output := by
  intro mutant
  have refused : (Registry.pipeline mutant unchecked).run elaborationSource =
      .error ⟨StageFailure, .invalidInput rule⟩ := by
    cases unchecked <;> with_unfolding_all rfl
  rintro ⟨_, ran⟩
  rw [refused] at ran
  cases ran

set_option maxRecDepth 4096 in
example (unchecked : Bool) :
    let mutant : Passes slice0 testProfile :=
      { literalReturnPasses with elaborate := fun _ => .ok (pendingLiteral 43) }
    (Registry.pipeline mutant unchecked).run elaborationSource = .ok (lcbReturnJob 43) ∧
    ¬ (registry mutant).elaborator.Validated := by
  refine ⟨?_, wrongLiteralElaboratorInvalid _ rfl⟩
  cases unchecked <;> with_unfolding_all rfl

-- Structural job validation accepts both functions. Imported execution
-- observes their different returns from the supplied body and runtime input.
example (project : Project) :
    let valid := fun status => LeanCompilerBackend.Verified.validateJob
      (Backend.LCB.job (lcbReturnJob status).body) = .ok ()
    let execution := LCBInput.Execution (profile := testProfile) Nat project
    valid 0 ∧ valid 42 ∧ (lcbReturnJob 0).RuntimeMatches project ∧
    execution.observes (lcbReturnJob 0) (.returned 0 ByteArray.empty) ∧
    execution.observes (lcbReturnJob 42) (.returned 42 ByteArray.empty) := by
  with_unfolding_all
    exact ⟨rfl, rfl, ⟨[], rfl, rfl⟩, ⟨_, ⟨_, rfl, .nil _⟩, rfl⟩,
      ⟨_, ⟨_, rfl, .nil _⟩, rfl⟩⟩

private abbrev literalCore := elaboratedReturn 42
private abbrev literalCoreExecution := CoreInput.Execution (language := earlyFixtureLanguage)
private abbrev literalMIRExecution := MIR.Execution (language := earlyFixtureLanguage)
  (lang := returnLowering) (profile := testProfile)
private abbrev literalMemoryExecution := Memory.Execution (language := earlyFixtureLanguage)
  (lang := returnLowering) (profile := testProfile)
private abbrev literalSSAExecution := SSA.Execution (language := earlyFixtureLanguage)
  (lang := returnLowering) (profile := testProfile)
private abbrev literalBIRExecution := BIR.Execution (language := earlyFixtureLanguage)
  (lang := returnLowering) (profile := testProfile)
private abbrev literalJobExecution := LCBInput.Execution (profile := testProfile)
  earlyFixtureLanguage.operands.Event elaborationSource.project

private def literalOrigin : Origin :=
  ⟨⟨0⟩, ⟨⟨0⟩, 18, 28, 1, 19⟩, ⟨.function earlyEntry, ⟨[⟨.return, .ordinal 0⟩]⟩⟩⟩

private def literalSignatures : AList FnId (FunctionSignature earlyFixtureLanguage.operands.Ty) :=
  [(earlyEntry, ⟨[], some returnType⟩)]

private def literalMIR : MIR returnLowering testProfile :=
  ⟨elaborationSource.project, earlyEntry, literalSignatures,
    [(earlyEntry, [], singleBlockGraph (.ret (some ⟨returnType, .literal ⟨[42]⟩⟩)) [literalOrigin])], [], []⟩

private def literalMemory : Memory returnLowering testProfile := literalMIR.logicalView

private def literalSSA : SSA returnLowering testProfile :=
  ⟨elaborationSource.project, earlyEntry, literalSignatures,
    [(earlyEntry, [], singleBlockGraph (.ret (some ⟨returnType, .literal ⟨[42]⟩⟩)) [literalOrigin])], [], [], []⟩

private def literalBIR : BIR returnLowering testProfile :=
  ⟨elaborationSource.project, earlyEntry, literalSignatures,
    [(earlyEntry, [], singleBlockGraph (.ret (some ⟨returnType, .literal ⟨[42]⟩⟩)) [literalOrigin])], [], #[], [], []⟩

private def literalRegisters : MemoryRegisters earlyFixtureLanguage testProfile :=
  {ownerFixtureStart with frame := earlyEntry}

private def literalResult : Concrete.Contracts.Observation := .returned 42 ByteArray.empty

set_option maxRecDepth 4096 in
private theorem literalCoreReaches :
    ∃ s, literalCoreExecution.finite literalCore s ∧
      literalCoreExecution.result literalCore s literalResult ∧ literalCoreExecution.trace literalCore s = [] := by
  let start := literalCore.machineProgram.initial
  let next (q : Exec.Config literalCore.machineProgram.machine) :=
    match Exec.step literalCore.machineProgram.body q with
    | some (.next result) => result
    | _ => q
  let frame (q : Exec.Config literalCore.machineProgram.machine) : literalCoreExecution.State :=
    (⟨(literalCore.machineProgram.ownership, literalCore.machineProgram.declarations), q⟩, [])
  let returned := next start
  let unscoped := next returned
  let finished := next unscoped
  have tick (p : Exec.Config literalCore.machineProgram.machine)
      (dispatch : Exec.step literalCore.machineProgram.body p = some (.next (next p)))
      (active : p.final? = none)
      (trace : literalCoreExecution.trace literalCore (frame p) = [])
      (afterTrace : literalCoreExecution.trace literalCore (frame (next p)) = []) :
      literalCoreExecution.step literalCore (frame p) (frame (next p)) := by
    refine ⟨?_, ⟨p, next p, .silent, rfl, rfl, .silent dispatch, rfl⟩, ?_⟩
    · with_unfolding_all
        change Option.bind p.final? _ = none
        rw [active]
        rfl
    · change Concrete.Contracts.TracePrefix (literalCoreExecution.trace literalCore (frame p))
          (literalCoreExecution.trace literalCore (frame (next p)))
      rw [trace, afterTrace]
      exact ⟨[], rfl⟩
  refine ⟨frame finished, ⟨frame start, rfl,
    .cons (tick start ?_ ?_ ?_ ?_)
      (.cons (tick returned ?_ ?_ ?_ ?_) (.cons (tick unscoped ?_ ?_ ?_ ?_) (.nil _)))⟩, ?_, ?_⟩
  all_goals with_unfolding_all first | rfl | exact nofun

set_option maxRecDepth 4096 in
private theorem literalTables : literalMemory.TablesWF := by
  with_unfolding_all
    refine ⟨.cons nofun .nil, .cons nofun .nil, .cons nofun .nil, .nil, ?_, nofun, ?_⟩
    · intro fn classes member
      cases List.mem_singleton.mp member
      exact .nil
    · intro fn locals graph member
      cases List.mem_singleton.mp member
      refine ⟨.nil, ⟨.cons nofun .nil, ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
        ⟨_, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩⟩
      · intro block present edge follows
        cases List.mem_singleton.mp present
        cases follows
      · intro block present
        cases List.mem_singleton.mp present
        exact ⟨rfl, nofun⟩

set_option maxRecDepth 4096 in
private theorem literalHandoff : literalMemory.HandoffWF := by
  refine ⟨literalTables, List.mem_singleton_self _, ?_, nofun⟩
  intro origin present
  have member : origin ∈ [literalOrigin] := by with_unfolding_all exact present
  obtain rfl := List.mem_singleton.mp member
  let checkedSpan : ConcreteSpec.Core.Span :=
    ⟨⟨0⟩, 18, 28, by decide, 1, 19, by decide, by decide⟩
  exact ⟨checkedSpan, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

set_option maxRecDepth 4096 in
private theorem literalConsumes : ConsumesEvidence literalCore [] literalMIR := by
  refine ⟨.nil, ?_, nofun, ?_, ?_⟩
  · rintro site opcode conditions ⟨op, present, _, _, rfl⟩ nonempty
    cases List.mem_singleton.mp present
    exact (nonempty rfl).elim
  · rintro _ _ ⟨_, found, _⟩
    cases found
  · exact (returnedCoreWF 42).2.1

set_option maxRecDepth 4096 in
private theorem literalUnchecked :
    LowersUnchecked (language := earlyFixtureLanguage) literalCore literalMIR := by
  with_unfolding_all
    obtain ⟨reached, finite, result, trace⟩ := literalCoreReaches
    refine ⟨(ConcreteSpec.Admission.checkStatic_iff (returnedCoreWF 42)).1 rfl, rfl, rfl, ⟨fun _ h => h, fun _ h => h⟩, literalConsumes, ⟨⟨?_, nofun⟩, rfl⟩, ?_⟩
    · intro fn locals graph present
      cases List.mem_singleton.mp present
      exact nofun
    apply refinesToTerminal literalCoreExecution literalMIRExecution reached literalRegisters literalResult
      finite result
    · with_unfolding_all rfl
    · intro _ h
      exact h
    · intro _ h
      exact (Option.some.inj h).symm
    · exact trace

set_option maxRecDepth 4096 in
private theorem literalMemoryContract : LowersMemory (language := earlyFixtureLanguage) literalMIR literalMemory := by
  with_unfolding_all
    have sourceFinal : literalMIRExecution.result literalMIR literalRegisters literalResult := by with_unfolding_all rfl
    have targetFinal : literalMemoryExecution.result literalMemory literalRegisters literalResult := by with_unfolding_all rfl
    have sourceOnly := reachableTerminal literalMIRExecution literalMIR literalRegisters literalResult (fun _ h => h) sourceFinal
    have targetOnly := reachableTerminal literalMemoryExecution literalMemory literalRegisters literalResult (fun _ h => h) targetFinal
    have layout : LayoutsMatch earlyFixtureLanguage.layout literalMemory := by
      refine ⟨?_, nofun⟩
      intro fn locals graph present place ty member
      cases List.mem_singleton.mp present
      cases member
    have promotes : PromotionSound (language := earlyFixtureLanguage) literalMIR literalMemory := by
      constructor
      · intro fn locals graph present place ty member
        cases List.mem_singleton.mp present
        cases member
      · intro fn classes place present member
        cases List.mem_singleton.mp present
        cases member
    refine ⟨rfl, rfl, ⟨fun _ h => h, fun _ h => h⟩, layout, ⟨rfl, rfl⟩, promotes, literalTables, literalTables,
      matchingRefinement (language := earlyFixtureLanguage) (refinesToTerminal literalMIRExecution literalMemoryExecution literalRegisters literalRegisters literalResult
        ⟨literalRegisters, by with_unfolding_all rfl, .nil _⟩ sourceFinal targetFinal (fun _ h => h) (fun _ h => (Option.some.inj h).symm) rfl), ?_⟩
    intro _
    refine ⟨⟨fun p q => p = q, fun _ _ => 0, ?_, ?_, ?_⟩, ?_⟩
    · intro q atEntry
      obtain rfl := atEntry
      exact ⟨literalRegisters, rfl, rfl⟩
    · intro p q q' _ finite _ tick
      obtain rfl := targetOnly q finite
      exact (literalMemoryExecution.terminal _ _ _ targetFinal _ tick).elim
    · intro p q o _ finite same observed
      obtain rfl := targetOnly q finite
      subst p
      obtain rfl := (Option.some.inj (targetFinal.symm.trans observed)).symm
      exact ⟨literalRegisters, .nil _, sourceFinal, rfl⟩
    · intro p q finite reached _
      obtain rfl := sourceOnly p finite
      obtain rfl := targetOnly q reached
      have frame : literalMemory.RepresentsActivation (language := earlyFixtureLanguage)
          literalRegisters.toMemoryFrame literalRegisters.toMemoryFrame literalRegisters.bytes := by
        exact ⟨⟨⟨Mem.empty_wellFormed _, .nil, .nil, nofun⟩, rfl⟩, rfl, nofun, .nil, .nil, .nil, .nil, nofun,
          fun _ => ⟨nofun, nofun⟩, nofun, nofun, nofun, nofun⟩
      exact ⟨⟨frame, rfl, nofun, ⟨rfl, nofun⟩, .nil, rfl, rfl⟩, rfl⟩

set_option maxRecDepth 4096 in
private theorem literalSSAContract : BuildsSSA (language := earlyFixtureLanguage) literalMemory literalSSA := by
  with_unfolding_all
    refine ⟨rfl, rfl, rfl, ⟨rfl, rfl⟩, ⟨fun _ h => h, fun _ h => h⟩, ?_⟩
    exact refinesToTerminal literalMemoryExecution literalSSAExecution literalRegisters literalRegisters literalResult
      ⟨_, rfl, .nil _⟩ (by with_unfolding_all rfl) (by with_unfolding_all rfl)
      (fun _ h => h) (fun _ h => (Option.some.inj h).symm) rfl

set_option maxRecDepth 4096 in
private theorem literalBIRContract : BuildsBIR (language := earlyFixtureLanguage) literalSSA literalBIR := by
  with_unfolding_all
    refine ⟨rfl, ?_, ?_, ⟨fun _ h => h, fun _ h => h⟩, by decide, ⟨rfl, rfl, rfl⟩, BIR.refinesOfViewEq rfl⟩
    · intro fn present block inside slot trap
      cases List.mem_singleton.mp present
      cases List.mem_singleton.mp inside
      cases trap
    · intro fn present block inside kind site trap
      cases List.mem_singleton.mp present
      cases List.mem_singleton.mp inside
      cases trap

set_option maxRecDepth 4096 in
private theorem literalLCBContract : EmitsLCB earlyFixtureLanguage literalBIR (lcbReturnJob 42) := by
  with_unfolding_all
    let job := lcbReturnJob 42
    let jobEntry : literalJobExecution.State := (LeanCompilerBackend.Verified.Source.initial job.body #[]).toOption
    refine ⟨rfl, by with_unfolding_all rfl, nofun, .cons nofun .nil, rfl, ⟨[], rfl, rfl⟩, ?_,
      .cons nofun .nil, .cons nofun .nil, fun _ => Iff.rfl, by with_unfolding_all rfl, literalBIRContract.2.1⟩
    exact refinesToTerminal literalBIRExecution literalJobExecution literalRegisters jobEntry literalResult
      ⟨_, rfl, .nil _⟩ (by with_unfolding_all rfl) (by with_unfolding_all rfl)
      (fun _ h => h) (fun _ h => (Option.some.inj h).symm) rfl

set_option maxRecDepth 4096 in
example (unchecked : Bool) :
    let pipeline := (registry literalReturnPasses).lcbPipeline unchecked
    pipeline.Ready elaborationSource ∧ pipeline.Corresponds elaborationSource (lcbReturnJob 42) := by
  have static := returnAdmissionContract.1.2.1.1
  have checked := returnAdmissionContract.1
  have admitted : (⟨literalCore, []⟩ : Admitted earlyFixtureLanguage).Valid :=
    checked.2
  have fragment : literalCore.InFragment := ⟨_, representsReturn 42⟩
  have elaborates : (registry literalReturnPasses).elaborator.success parsedReturn pendingReturn :=
    ⟨returnProgram, rfl, rfl⟩
  have birWF : BIR.WF (language := earlyFixtureLanguage) literalBIR :=
    ⟨literalBIRContract.2.1, by decide, .cons nofun .nil, List.mem_singleton_self _, nofun, by with_unfolding_all exact literalTables, nofun⟩
  have ready : Source.WF elaborationSource ∧ elaborationSource.slice = slice0 := ⟨parsedReturnDomain.1, rfl⟩
  have front := returnLexerContract
  have parse := returnParserContract
  have lowerCore : LowersCore (language := earlyFixtureLanguage) ⟨literalCore, []⟩ literalMIR := literalUnchecked.2
  have back :
      ∃ bir, Concrete.Contracts.Graph (registry literalReturnPasses).buildBIR.run literalSSA bir ∧
        (registry literalReturnPasses).buildBIR.outputInvariant bir ∧
        (registry literalReturnPasses).buildBIR.success literalSSA bir ∧
        (registry literalReturnPasses).buildBIR.trust literalSSA bir ∧
        (Pipeline.next (registry literalReturnPasses).emitLCB .done).Corresponds bir (lcbReturnJob 42) := by
    exact ⟨literalBIR, by with_unfolding_all rfl, birWF, ⟨literalBIRContract, rfl⟩, trivial, lcbReturnJob 42,
      by with_unfolding_all rfl, ⟨literalBIR, literalLCBContract⟩, literalLCBContract, trivial, rfl⟩
  cases unchecked
  · exact ⟨ready, returnStream, rfl, ⟨front.2.1, rfl⟩, front, trivial,
      parsedReturn, by with_unfolding_all rfl, parsedReturnDomain, parse, trivial,
      pendingReturn, rfl, parsedReturnDomain.2.2.2.2 _ elaborates, elaborates, trivial,
      ⟨literalCore, []⟩, by with_unfolding_all rfl, admitted, ⟨rfl, _, rfl, checked⟩, trivial,
      literalMIR, rfl, literalHandoff, lowerCore, trivial,
      literalMemory, rfl, literalHandoff, ⟨literalMemoryContract, rfl⟩, trivial,
      literalSSA, by with_unfolding_all rfl, by with_unfolding_all exact literalHandoff, ⟨literalSSAContract, rfl⟩, trivial, back⟩
  · exact ⟨ready, returnStream, rfl, ⟨front.2.1, rfl⟩, front, trivial,
      parsedReturn, by with_unfolding_all rfl, parsedReturnDomain, parse, trivial,
      pendingReturn, rfl, parsedReturnDomain.2.2.2.2 _ elaborates, elaborates, trivial,
      literalCore, rfl, ⟨rfl, returnedCoreWF 42, fragment⟩, ⟨rfl, rfl⟩, trivial,
      literalCore, by with_unfolding_all rfl, ⟨rfl, static⟩, rfl, trivial,
      literalMIR, rfl, literalHandoff, literalUnchecked, trivial,
      literalMemory, rfl, literalHandoff, ⟨literalMemoryContract, rfl⟩, trivial,
      literalSSA, by with_unfolding_all rfl, by with_unfolding_all exact literalHandoff, ⟨literalSSAContract, rfl⟩, trivial, back⟩

end StageInterfaceControls
