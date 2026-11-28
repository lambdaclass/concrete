import ConcreteChecks.StageInterfaces.BackendFixtures
import ConcreteChecks.StageInterfaces.Refinement

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar
open LeanCompilerBackend.Verified (validateJob)

namespace StageInterfaceControls

section
variable (verifier : TableVerifier grammar) (size : proposal.size = verifier.words)
  (accept : (verifier.run proposal).2 = some true)

local notation "parser" => testParser verifier size accept

-- Memory and ISA are computed from the registry's resolved platform.
example : testProfile.isa = .x86_64 ∧
    ¬ testProfile.memoryTarget = { linuxX8664 with byteOrder := .big } := by
  constructor
  · rfl
  · intro h
    have order := congrArg Target.byteOrder h
    contradiction

-- The wrong backend's metadata check rejects an LLVM handoff under the LCB profile.
example (project : Project) :
    ¬ LLVMMetadataMatches (testBIR project (profile := testProfile))
      ⟨⟨[llvmBodyDefinition]⟩, ⟨launcherItems []⟩, #[]⟩ := by
  intro success
  have backend := success.1
  contradiction

-- The emitter returns an empty launcher module under the macOS AArch64 LLVM
-- profile. Handoff and refinement premises cannot make that result sound.
example (project : Project) (m : LoweringModels testLanguage testLowering macLLVM)
    (eligible : (r m parser).emit.inputInvariant (testBIR project)) :
    ¬ (r m parser).emit.Checks := by
  intro sound
  have success := (sound.1.1 (testBIR project)
    ⟨⟨[]⟩, ⟨[]⟩, #[]⟩ eligible rfl).2
  obtain ⟨diagnostics, hmap, _, hitems, _⟩ := success.2.2.2
  cases Option.some.inj hmap
  cases hitems

example (project : Project) (m : LoweringModels testLanguage testLowering macLLVM) :
    ¬ (r m parser).emit.success (testBIR project)
      ⟨⟨[]⟩, ⟨[]⟩, #[]⟩ := by
  intro success
  obtain ⟨diagnostics, hmap, _, hitems, _⟩ := success.2.2.2
  cases Option.some.inj hmap
  cases hitems

-- Changing the component body while preserving its launcher and metadata leaves the exact
-- printer invocation under P_print. Neither input gains a semantic judgment
-- from the structural handoff check.
example (project : Project) (m : LoweringModels testLanguage testLowering macLLVM) :
    let ir (status : Nat) : LLVMInput macLLVM :=
      ⟨⟨[.define false (.integer .u32) .body [], .label .entry,
          .terminator (.ret (.integer .u32, .immediate ⟨status⟩)), .endFunction]⟩,
       ⟨launcherItems []⟩, #[]⟩
    (r m parser).emit.kind = .namedPremise "P_print" ∧
    LLVMMetadataMatches (testBIR project) (ir 0) ∧
    LLVMMetadataMatches (testBIR project) (ir 42) ∧
    (ir 0).packageBytes.component.data.toList ≠ (ir 42).packageBytes.component.data.toList := by
  have hOwn : ∀ status : Nat,
      ComponentOwnsSymbols
        [.define false (.integer .u32) .body [], .label .entry,
          .terminator (.ret (.integer .u32, .immediate ⟨status⟩)), .endFunction]
        (launcherItems []) := fun _ =>
    ComponentOwnsSymbols.of_component_globals []
      (fun _ hg => List.mem_singleton.mp hg)
      (fun _ hg => Or.inl (List.mem_singleton.mp hg))
  refine ⟨rfl, ⟨rfl, rfl, [], rfl, rfl, rfl, hOwn 0⟩, ⟨rfl, rfl, [], rfl, rfl, rfl, hOwn 42⟩, ?_⟩
  decide

-- Four BIR definitions exceed the registered profile's three-slot ceiling.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    let block : Block _ _ _ := ⟨⟨0⟩, [⟨(), ⟨0⟩⟩, ⟨(), ⟨1⟩⟩, ⟨(), ⟨2⟩⟩, ⟨(), ⟨3⟩⟩], [], .ret none, []⟩
    let bir : BIR testLowering testProfile :=
      { testBIR project (profile := testProfile) with functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)] }
    ¬ (r m parser).emit.inputInvariant bir := by
  dsimp
  intro eligible
  exact (show ¬ 4 ≤ 3 from by decide) eligible.2.2.1

-- Validate the exact output of the candidate emitter before publishing its job.
example (project : Project) (m : LoweringModels testLanguage testLowering testProfile) :
    ∃ reason, (r m parser).emit.run (testBIR project) = .error (.invalidLCBJob reason) :=
  ⟨_, by with_unfolding_all rfl⟩

end

private theorem birReturnResult (project : Project) (alternate : Bool) :
    birExecution.result (birReturn project alternate) (BIR.entryState (birReturn project alternate))
      (.returned (if alternate then 42 else 0) ByteArray.empty) := by
  cases alternate <;> with_unfolding_all rfl

private theorem birReturnReachable (project : Project) (alternate : Bool) (state : birExecution.State)
    (finite : birExecution.finite (birReturn project alternate) state) :
    state = BIR.entryState (birReturn project alternate) := by
  obtain ⟨start, initial, steps⟩ := finite
  change start = BIR.entryState (birReturn project alternate) at initial
  subst start
  cases steps with
  | nil => rfl
  | cons step _ => exact (birExecution.terminal _ _ _ (birReturnResult project alternate) _ step).elim

private theorem mismatchLCB (project : Project) (alternate : Bool) :
    ¬ EmitsLCB testLanguage (birReturn project alternate)
      (lcbReturnJob (if alternate then 0 else 42)) := by
  intro emits
  let execution := LCBInput.Execution (profile := testProfile) Nat project
  have actual : execution.observes (lcbReturnJob (if alternate then 0 else 42))
      (.returned (if alternate then 0 else 42) ByteArray.empty) := by
    cases alternate <;> with_unfolding_all exact ⟨_, ⟨_, rfl, .nil _⟩, rfl⟩
  obtain ⟨q, finite, result⟩ := actual
  have safe : ProgramSafe birExecution (birReturn project alternate) := by
    intro state stuck
    have equal := birReturnReachable project alternate state stuck.1
    subst state
    exact stuck.2.2 _ (birReturnResult project alternate)
  obtain ⟨p, reachable, wrong, _⟩ := (emits.2.2.2.2.2.2.1 safe).1 q _ finite result
  rw [birReturnReachable project alternate p reachable] at wrong
  have correct := birReturnResult project alternate
  change BIR.hostedResult _ _ = some _ at wrong correct
  have status := congrArg (fun o : Concrete.Contracts.Observation =>
    match o with | .returned code _ => code | _ => 255)
    (Option.some.inj (correct.symm.trans wrong))
  cases alternate
  · exact (show (0 : UInt8) ≠ 42 from by decide) status
  · exact (show (42 : UInt8) ≠ 0 from by decide) status

-- Independently substitute the emitted job and the source graph.
example (project : Project) :
    (¬ EmitsLCB testLanguage (birReturn project false) (lcbReturnJob 42)) ∧
    (¬ EmitsLCB testLanguage (birReturn project true) (lcbReturnJob 0)) :=
  ⟨mismatchLCB project false, mismatchLCB project true⟩

set_option maxHeartbeats 1000000 in
example (project : Project) :
    EmitsLCB testLanguage (birReturn project false) (lcbReturnJob 0) := by
  refine ⟨rfl, by with_unfolding_all rfl, nofun, by simp [lcbReturnJob], rfl,
    ⟨[], rfl, rfl⟩, ?_, by simp [birReturn], by simp [lcbReturnJob], fun _ => Iff.rfl, by with_unfolding_all rfl, ?_⟩
  case refine_2 =>
    intro fn member block included slot trap
    cases List.mem_singleton.mp member
    have eq : block = birReturnBlock false := List.mem_singleton.mp included
    subst block
    cases trap
  let execution := LCBInput.Execution (profile := testProfile) Nat project
  let entry : execution.State :=
    (LeanCompilerBackend.Verified.Source.initial (lcbReturnJob 0).body #[]).toOption
  have result : execution.result (lcbReturnJob 0) entry (.returned 0 ByteArray.empty) := by
    with_unfolding_all rfl
  have only : ∀ p, execution.initial (lcbReturnJob 0) p → p = entry := fun _ h => h
  have reachable : ∀ q, execution.finite (lcbReturnJob 0) q → q = entry := by
    intro q
    rintro ⟨p, initial, steps⟩
    have atEntry := only p initial
    subst p
    cases steps with
    | nil => rfl
    | cons step _ => exact (execution.terminal _ _ _ result _ step).elim
  intro _
  refine ⟨?_, ?_, ?_⟩
  · intro q o finite observed
    obtain rfl := reachable q finite
    have matching : some (Concrete.Contracts.Observation.returned 0 ByteArray.empty) = some o := by
      with_unfolding_all exact observed
    have same := Option.some.inj matching
    subst o
    refine ⟨_, ⟨_, rfl, .nil _⟩, birReturnResult project false, ?_⟩
    with_unfolding_all rfl
  · intro run diverges
    have start := only (run 0) diverges.1
    have step := diverges.2 0
    rw [start] at step
    exact (execution.terminal _ _ _ result _ step).elim
  · intro q stuck
    have same := reachable q stuck.1
    subst q
    exact stuck.2.2 _ result

theorem birReturnTables (project : Project) (profile : Profile) :
    let original := birReturn project false
    let input : BIR testLowering profile :=
      ⟨project, original.entry, [(Exec.Demo.mainF, ⟨[], some ()⟩)],
        original.functions, original.layouts, original.sites, [], []⟩
    (input.memoryView (language := testLanguage)).TablesWF := by
  with_unfolding_all
    refine ⟨(by exact .cons nofun .nil), (by exact .cons nofun .nil), (by exact .cons nofun .nil), .nil, ?_, nofun, ?_⟩
    · intro fn classes member
      cases List.mem_singleton.mp member
      exact List.nodup_nil
    · intro fn locals graph member
      cases List.mem_singleton.mp member
      refine ⟨List.nodup_nil, ⟨(by exact .cons nofun .nil), ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
        ⟨_, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩⟩
      · intro block inside edge successor
        cases List.mem_singleton.mp inside
        cases successor
      · intro block inside
        cases List.mem_singleton.mp inside
        exact ⟨rfl, nofun⟩

private def llvmReturn (status : Nat) : LLVMInput macLLVM :=
  ⟨⟨[.define false (.integer .u32) .body [], .label .entry,
      .terminator (.ret (.integer .u32, .immediate ⟨status⟩)), .endFunction]⟩,
   ⟨launcherItems []⟩, #[]⟩

-- Both registered emitters produce structurally valid LLVM for the selected
-- profile. The fixed behavior interpretation distinguishes their bytes.
set_option maxRecDepth 4096 in
example (parser : Parser grammar) (hsteps : 2 ≤ parser.stepsPerToken) (project : Project) :
    let stages (status : Nat) := fixtureEntries parser
      {testPasses (profile := macLLVM) [] with emit := fun _ => .ok (llvmReturn status)} hsteps
    let original := birReturn project false
    let input : BIR testLowering macLLVM :=
      ⟨project, original.entry, [(Exec.Demo.mainF, ⟨[], some ()⟩)],
        original.functions, original.layouts, original.sites, [], []⟩
    let expected : Behavior Nat := .terminates (.returned 0 ByteArray.empty) []
    (stages 0).emit.run input = .ok (llvmReturn 0) ∧
      (stages 42).emit.run input = .ok (llvmReturn 42) ∧
      (stages 42).emit.success input (llvmReturn 42) ∧
      ((∀ behavior, llvmBehaviors (llvmReturn 0).packageBytes behavior → behavior = expected) →
        (stages 0).emit.trust input (llvmReturn 0)) ∧
      (llvmBehaviors (llvmReturn 42).packageBytes
        (.terminates (.returned 42 ByteArray.empty) ([] : List (Concrete.Contracts.ProcessEvent Nat))) →
        ¬ (stages 42).emit.Checks) := by
  intro stages original input expected
  let start := BIR.entryState (language := testLanguage) input
  have hOwn42 : ComponentOwnsSymbols (llvmReturn 42).component.items (launcherItems []) :=
    ComponentOwnsSymbols.of_component_globals []
      (fun _ hg => List.mem_singleton.mp hg)
      (fun _ hg => Or.inl (List.mem_singleton.mp hg))
  refine ⟨rfl, rfl, ⟨rfl, rfl, rfl, [], rfl, rfl, rfl, hOwn42⟩, ?_, ?_⟩
  · intro exactLLVM _ behavior emitted
    obtain rfl := exactLLVM behavior emitted
    with_unfolding_all exact ⟨start, ⟨start, rfl, .nil _⟩, rfl, rfl⟩
  intro observed sound
  have eligible : (stages 42).emit.inputInvariant input := by
    refine ⟨?_, ?_, (by decide : 0 ≤ 2), by with_unfolding_all exact nofun, nofun,
      ⟨birReturnTables project macLLVM, List.mem_singleton_self _,
        by with_unfolding_all exact nofun, nofun,
        ⟨.nil, ⟨.nil, nofun⟩, nofun⟩, ⟨.nil, nofun⟩⟩⟩
    · exact (BIR.checkRegisterFormation_eq_true_iff (language := testLanguage) input).mp
        (by with_unfolding_all rfl)
    · intro fn member block included slot trap
      cases List.mem_singleton.mp member
      cases List.mem_singleton.mp included
      cases trap
  have trusted := sound.2 input (llvmReturn 42) eligible rfl
  let exec := BIR.Execution (language := testLanguage) (lang := testLowering) (profile := macLLVM)
  let zero : Concrete.Contracts.Observation := .returned 0 ByteArray.empty
  have initial : ∀ p, exec.initial input p → p = start := fun _ h => h
  have result : exec.result input start zero := by with_unfolding_all rfl
  have safe := stationarySafe exec input start zero initial result
  obtain ⟨state, finite, terminal, _⟩ := trusted safe _ observed
  rw [reachableTerminal exec input start zero initial result state finite] at terminal
  have wrong : some (Concrete.Contracts.Observation.returned 0 ByteArray.empty) =
      some (.returned 42 ByteArray.empty) := by with_unfolding_all exact terminal
  cases wrong

def trapEntry : TrapEntry := ⟨.divisionByZero, testOrigin⟩

private def trapBIR : BIR testLowering testProfile :=
  {(birReturn tabProject false) with
    functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[{birReturnBlock false with ending := .trap ⟨0⟩}]⟩)]
    sites := #[trapEntry]}

private def trapJob : LCBInput testProfile :=
  let diagnostic :=
    s!"concrete: trap: {TrapKind.divisionByZero.name} at {mainSourcePath.render.text}:{span.line}:{span.col} (site {testSite.id})\n"
  {lcbReturnJob 256 with
    sites := trapBIR.sites
    runtimeInput := (Backend.Runtime.qbeDiagnostics [diagnostic]).toUTF8}

private def trapResult : Concrete.Contracts.Observation :=
  .trapped 134 .divisionByZero ⟨⟨testSite.id⟩, ⟨mainSourcePath.render.text, span.line, span.col⟩⟩
    (trapDiagnostic tabProject .divisionByZero testSite span)

-- A trapping entry is a valid terminal observation for the exact hosted job.
set_option maxRecDepth 2048 in
private theorem trapRefines :
    BehaviorRefinement birExecution (LCBInput.Execution Nat tabProject)
      birExecution.result (LCBInput.Execution (profile := testProfile) Nat tabProject).result trapBIR trapJob := by
  refine stationaryRefines _ _ _ _ _ _ trapResult trapResult
    (fun _ => Iff.rfl) (fun _ => Iff.rfl) ?_ ?_ ?_ ?_ ?_
  all_goals first
    | (intro o; with_unfolding_all exact ⟨fun h => (Option.some.inj h).symm, fun h => congrArg some h.symm⟩)
    | with_unfolding_all rfl

-- The exact hosted job carries both the trap result and its rendered runtime diagnostics.
example : EmitsLCB testLanguage trapBIR trapJob := by
  refine ⟨rfl, by with_unfolding_all rfl, nofun, by simp [trapJob, lcbReturnJob], rfl,
    ⟨_, by with_unfolding_all rfl, by with_unfolding_all rfl⟩, trapRefines,
    by simp [trapBIR, birReturn], by simp [trapJob, lcbReturnJob], fun _ => Iff.rfl,
    by with_unfolding_all rfl, ?_⟩
  intro fn member block included slot trapped
  cases List.mem_singleton.mp member
  cases List.mem_singleton.mp included
  cases BIRTerm.trap.inj trapped
  exact ⟨trapEntry, rfl⟩

-- Preserve the trap table and runtime, but replace the emitted trap word
-- with an ordinary exit status. Job validation still accepts the body.
set_option maxRecDepth 2048 in
example :
    let output := {trapJob with body := (lcbReturnJob 134).body}
    validateJob (Backend.LCBJob.lcbFunctionJob output.body) = .ok () ∧
      ¬ EmitsLCB testLanguage trapBIR output := by
  refine ⟨by with_unfolding_all rfl, ?_⟩
  intro emitted
  apply stationaryRejects birExecution (LCBInput.Execution Nat tabProject) (LCBInput.Execution Nat tabProject).result
    (BIR.entryState (language := testLanguage) trapBIR) (some ⟨0, 0, #[]⟩)
    trapResult (.returned 134 ByteArray.empty) (fun _ h => h) ?_ ?_ ?_ ?_ emitted.2.2.2.2.2.2.1
  · with_unfolding_all rfl
  · with_unfolding_all rfl
  · with_unfolding_all rfl
  · intro observed
    have wrong : some trapResult = some (.returned 134 ByteArray.empty) := by
      with_unfolding_all exact observed
    cases wrong

private abbrev choiceLanguage : CoreLanguage :=
  {testLanguage with
    operation := fun _ _ _ _ _ _ _ before result effects after =>
      (result = .scalar (.int .u8 0) ∨ result = .scalar (.int .u8 1)) ∧
      effects = [] ∧ after = before
    preservesResidentHandles := by
      intro _ _ _ _ _ _ _ before _ _ after h hwf
      obtain ⟨_, _, rfl⟩ := h
      exact ⟨hwf, Nat.le_refl _, fun _ _ hb hl => ⟨_, hb, hl, rfl, rfl, rfl, rfl⟩⟩}

private def choiceBIR (project : Project) : BIR testLowering testProfile :=
  let block := {birReturnBlock false with
    body := [⟨testOrigin, .op (opcode := .named ⟨"choose"⟩) (ty := ()) ⟨0⟩ () .nil⟩]
    ending := .ret (some ⟨(), .value ⟨0⟩⟩)}
  {(birReturn project false) with functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)]}

private abbrev choiceExecution := BIR.Execution (language := choiceLanguage)
  (lang := testLowering) (profile := testProfile)

set_option maxRecDepth 3072 in
private theorem choiceReturns (project : Project) (alternate : Bool) :
    ∃ state, choiceExecution.finite (choiceBIR project) state ∧
      choiceExecution.result (choiceBIR project) state (.returned (if alternate then 1 else 0) ByteArray.empty) ∧
      choiceExecution.trace (choiceBIR project) state = [] := by
  let bir := choiceBIR project
  let start := BIR.entryState (language := choiceLanguage) bir
  let value : CoreValue Unit := .scalar (.int .u8 (if alternate then 1 else 0))
  let live : MemoryRegisters choiceLanguage testProfile := {start with live := [⟨0⟩]}
  let stored : MemoryRegisters choiceLanguage testProfile :=
    {live with initialized := [⟨0⟩], virtualValues := [(⟨0⟩, value)]}
  have step : choiceExecution.step bir start stored.advance := by
    with_unfolding_all
      refine ⟨rfl, ⟨_, rfl, [], start, [], value, [], Memory.operationStore start false,
        stored, rfl, rfl, rfl, ⟨?_, rfl, rfl⟩, rfl, ?_, rfl⟩, [], ?_⟩
    · cases alternate
      · exact Or.inl rfl
      · exact Or.inr rfl
    · apply Memory.WriteParams.cons (started := live)
      · with_unfolding_all exact ⟨nofun, Or.inl ⟨rfl, rfl⟩⟩
      · with_unfolding_all exact ⟨⟨rfl, nofun⟩, nofun, rfl, rfl, nofun⟩
      · exact .nil _
    · cases alternate <;> with_unfolding_all rfl
  refine ⟨_, ⟨start, rfl, .cons step (.nil _)⟩, ?_, ?_⟩
  all_goals cases alternate <;> with_unfolding_all rfl

-- The source actually reaches both outcomes. The exact emitted job selects one.
set_option maxRecDepth 3072 in
set_option maxHeartbeats 1000000 in
example (project : Project) :
    choiceExecution.observes (choiceBIR project) (.returned 1 ByteArray.empty) ∧
    EmitsLCB choiceLanguage (choiceBIR project) (lcbReturnJob 0) := by
  obtain ⟨another, reachesAnother, observesAnother, _⟩ := choiceReturns project true
  refine ⟨⟨another, reachesAnother, observesAnother⟩, rfl, by with_unfolding_all rfl, nofun,
    by simp [lcbReturnJob], rfl, ⟨[], rfl, rfl⟩, ?_, by simp [choiceBIR], by simp [lcbReturnJob],
    fun _ => Iff.rfl, by with_unfolding_all rfl, ?_⟩
  · let exec := LCBInput.Execution (profile := testProfile) Nat project
    let output := lcbReturnJob 0
    let result : Concrete.Contracts.Observation := .returned 0 ByteArray.empty
    let entry : exec.State := (LeanCompilerBackend.Verified.Source.initial output.body #[]).toOption
    have terminal : exec.result output entry result := by with_unfolding_all rfl
    have reached := reachableTerminal exec output entry result (fun _ h => h) terminal
    obtain ⟨source, sourceReached, observed, sourceTrace⟩ := choiceReturns project false
    intro _
    refine ⟨?_, ?_, ?_⟩
    · intro state _ path observation
      obtain rfl := reached state path
      have same := Option.some.inj (terminal.symm.trans observation)
      exact ⟨source, sourceReached, same ▸ observed, sourceTrace.trans (by with_unfolding_all rfl)⟩
    · intro run infinite
      exact (exec.terminal output entry result terminal _
        ((show run 0 = entry from infinite.1) ▸ infinite.2 0)).elim
    · intro state stuck
      obtain rfl := reached state stuck.1
      exact stuck.2.2 result terminal
  · intro fn member block included slot trap
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp included
    cases trap

-- The selected job still returns zero, but the wrong runtime bytes fail
-- the LCB boundary's exact runtime match.
example (project : Project) :
    ¬ EmitsLCB choiceLanguage (choiceBIR project)
      {lcbReturnJob 0 with runtimeInput := ByteArray.empty} := by
  intro emitted
  obtain ⟨diagnostics, decoded, bytes⟩ := emitted.2.2.2.2.2.1
  have empty : some [] = some diagnostics := by with_unfolding_all exact decoded
  obtain rfl := (Option.some.inj empty).symm
  have wrong := congrArg ByteArray.size bytes
  have different : (ByteArray.empty.size = (Backend.Runtime.qbeDiagnostics []).toUTF8.size) → False := by decide
  exact different wrong

def bundledBIR (project : Project) : BIR testLowering testProfile :=
  let entry := birReturn project false
  {entry with
    functions := List.append entry.functions [(callee, [], ⟨⟨0⟩, #[birReturnBlock true]⟩)]
    signatures := [(callee, ⟨[], some ()⟩)]}

private def leafJob (value : BitVec 32) : LeanCompilerBackend.Verified.FunctionJob :=
  let job := Backend.LCBJob.lcbFunctionJob (lcbReturnJob value).body
  {job with code := {job.code with symbol := {job.code.symbol with name := "concrete_callee"}}}

def bundledJobs (value : BitVec 32) : LCBInput testProfile :=
  {lcbReturnJob 0 with additionalJobs := [(callee, leafJob value)]}

-- An unused helper's code may change from 42 in BIR to 7 in the emitted bundle.
set_option maxHeartbeats 1500000 in
example (project : Project) :
    EmitsLCB testLanguage (bundledBIR project) (bundledJobs 7) := by
  have distinct : [Exec.Demo.mainF, callee].Nodup := by decide
  refine ⟨rfl, by with_unfolding_all rfl, ?_, by decide, rfl, ⟨[], rfl, rfl⟩, ?_,
    distinct, distinct, fun _ => Iff.rfl, by with_unfolding_all rfl, ?_⟩
  · intro _ member
    cases List.mem_singleton.mp member
    with_unfolding_all rfl
  · refine stationaryRefines _ _ _ _ _ _ (.returned 0 ByteArray.empty)
      (.returned 0 ByteArray.empty) (fun _ => Iff.rfl) (fun _ => Iff.rfl) ?_ ?_ ?_ ?_ ?_
    all_goals first
      | (intro o; with_unfolding_all exact ⟨fun h => (Option.some.inj h).symm, fun h => congrArg some h.symm⟩)
      | exact Or.inl (by with_unfolding_all rfl)
      | exact Or.inr (by with_unfolding_all rfl)
      | with_unfolding_all rfl
  · intro fn member block included slot trap
    have member : fn ∈ [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[birReturnBlock false]⟩),
        (callee, [], ⟨⟨0⟩, #[birReturnBlock true]⟩)] := member
    simp only [List.mem_cons, List.not_mem_nil, or_false] at member
    rcases member with rfl | rfl
    all_goals
      cases List.mem_singleton.mp included
      cases trap

-- A hosted invocation that is missing an entry argument cannot return via
-- an unused block parameter.
example (project : Project) :
    let block := {birReturnBlock false with params := [⟨(), ⟨0⟩⟩]}
    let bir : BIR testLowering testProfile := {(birReturn project false) with
      functions := [(Exec.Demo.mainF, [], ⟨⟨0⟩, #[block]⟩)]}
    BIR.hostedResult (language := testLanguage) bir (BIR.entryState bir) = none := by
  with_unfolding_all rfl

end StageInterfaceControls
