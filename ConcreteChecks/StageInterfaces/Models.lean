import Concrete.Interfaces.Contracts

namespace Concrete.Interfaces

open ConcreteSpec
open Concrete.Contracts (ProcessEvent Semantics)
open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface

-- Candidate models are useful for negative tests of the projection rules.
-- The registered edges themselves execute the fixed graph definitions.
def Observation.SameRuntimeResult (observed : Observation)
    (legacy : Concrete.Contracts.Observation) : Prop :=
  match observed, legacy with
  | .returned a bytes, .returned b other => a = b ∧ bytes = other
  | .trapped a k span bytes, .trapped b j site other =>
    a = b ∧ k = j ∧ span.line = site.location.line ∧ span.col = site.location.col ∧ bytes = other
  | .startupRefused a bytes, .startupRefused b other => a = b ∧ bytes = other
  | .resourceExhausted, .resourceExhausted => True
  | _, _ => False

/-- A precise terminal projection over the same transition system used by the composed theorem.
The bridge retains the old runtime diagnostic equality during migration. -/
structure Model (A E : Type) where
  execution : Semantics A E
  result : A → execution.State → Observation → Prop
  projects : A → Observation → Concrete.Contracts.Observation → Prop
  terminalExact : ∀ a state legacy, execution.result a state legacy ↔
    ∃ observed, result a state observed ∧ projects a observed legacy
  completeProjection : ∀ a state observed, result a state observed →
    ∃ legacy, projects a observed legacy
  runtimeExact : ∀ a observed legacy, projects a observed legacy →
    observed.SameRuntimeResult legacy

def Model.Refines {A B E : Type} (source : Model A E) (target : Model B E)
    (a : A) (b : B) : Prop :=
  BehaviorRefinement source.execution target.execution source.result target.result a b

def coreRuntimeEvents {language : CoreLanguage} (core : CoreInput language)
    (state : Exec.Config core.machineProgram.machine) : List (ProcessEvent language.operands.Event) :=
  match state.final? with
  | some (.trapped kind site) => (coreTrapDiagnostic core kind site).data.toList.map .stderr
  | _ => []

def coreObservation {language : CoreLanguage} (core : CoreInput language)
    (outcome : Exec.Final language.operands.Val) : Option Observation := do
  match ← language.hostedResult outcome with
  | .returned status => return .returned status ByteArray.empty
  | .trapped status kind site =>
    let origin ← AList.lookup core.machineProgram.origins site
    return .trapped status kind origin.lexical (coreTrapDiagnostic core kind site)

/-- Relate the shared theorem model to actual spec configurations and spec transition labels. -/
structure CoreModel (language : CoreLanguage) where
  model : Model (CoreInput language) language.operands.Event
  config : (a : CoreInput language) → model.execution.State → Exec.Config a.machineProgram.machine
  specTrace : CoreInput language → model.execution.State → List language.operands.Event
  initialExact : ∀ a state, model.execution.initial a state →
    config a state = a.machineProgram.initial ∧ specTrace a state = [] ∧
      model.execution.trace a state = []
  traceExact : ∀ a state, model.execution.trace a state =
    (specTrace a state).map ProcessEvent.effect ++ coreRuntimeEvents a (config a state)
  authorityExact : ∀ a e, model.execution.permitted a e ↔
    ∃ state events, Exec.Steps a.machineProgram.body a.machineProgram.initial events state ∧
      (e ∈ coreRuntimeEvents a state ∨
        ∃ label next, Exec.Step a.machineProgram.body state label next ∧
          e ∈ label.events.map ProcessEvent.effect)
  stepsExact : ∀ a state next, model.execution.step a state next →
    ∃ label, Exec.Step a.machineProgram.body (config a state) label (config a next) ∧
      specTrace a next = specTrace a state ++ label.events
  stepsComplete : ∀ a state label q, model.execution.finite a state →
    Exec.Step a.machineProgram.body (config a state) label q →
    ∃ next, model.execution.step a state next ∧ config a next = q ∧
      specTrace a next = specTrace a state ++ label.events
  safetyCoversTraps : ∀ (a : CoreInput language) state events site op label next kind,
    Exec.Steps a.machineProgram.body a.machineProgram.initial events state →
    a.machineProgram.activeOperation state = some (site, op) →
    Exec.Step a.machineProgram.body state label next → next.ctrl = .trapped kind site →
    ∃ condition ∈ language.safety op, condition.trap = kind
  terminalExact : ∀ a state observed, model.result a state observed ↔
    ∃ outcome, (config a state).final? = some outcome ∧ coreObservation a outcome = some observed

structure LoweringModels (language : CoreLanguage) (lang : LoweringLanguage language.operands.Ty)
    (profile : Profile) where
  core : CoreModel language
  mir : Model (MIR lang profile) language.operands.Event
  memory : Model (Memory lang profile) language.operands.Event
  ssa : Model (SSA lang profile) language.operands.Event
  mirInvariant : MIR lang profile → Prop
  memoryInvariant : Memory lang profile → Prop
  ssaInvariant : SSA lang profile → Prop
  birInvariant : BIR lang profile → Prop
  memoryState : memory.execution.State → MemoryRegisters language profile
  mirState : mir.execution.State → MemoryRegisters language profile

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

private def executionTrace (state : MemoryRegisters language profile)
    (observation : Option Observation) : List (ProcessEvent language.operands.Event) :=
  state.effects.map ProcessEvent.effect ++
    match observation with
    | some (.trapped _ _ _ diagnostic) => diagnostic.data.toList.map ProcessEvent.stderr
    | _ => []

def Memory.ExecutionExact (memory : Memory lang profile)
    (m : LoweringModels language lang profile) : Prop :=
  ((∀ q, m.memory.execution.initial memory q → memory.Initial (m.memoryState q)) ∧
   (∀ q, m.memory.execution.finite memory q → m.memory.execution.trace memory q =
     executionTrace (m.memoryState q) (memory.terminalObservation (m.memoryState q)))) ∧
  (∀ q next, m.memory.execution.finite memory q → m.memory.execution.step memory q next →
    memory.Step (m.memoryState q) (m.memoryState next)) ∧
  (∀ q state, m.memory.execution.finite memory q → memory.Step (m.memoryState q) state →
    ∃ next, m.memory.execution.step memory q next ∧ m.memoryState next = state) ∧
  ∀ q result, m.memory.execution.finite memory q →
    (m.memory.result memory q result ↔
      memory.terminalObservation (m.memoryState q) = some result)

def MIR.ExecutionExact (mir : MIR lang profile)
    (m : LoweringModels language lang profile) : Prop :=
  ((∀ p, m.mir.execution.initial mir p → mir.logicalView.Initial (m.mirState p)) ∧
   (∀ p, m.mir.execution.finite mir p → m.mir.execution.trace mir p =
     executionTrace (m.mirState p) (mir.terminalObservation (m.mirState p)))) ∧
  (∀ p next, m.mir.execution.finite mir p → m.mir.execution.step mir p next →
    mir.Step (m.mirState p) (m.mirState next)) ∧
  (∀ p state, m.mir.execution.finite mir p → mir.Step (m.mirState p) state →
    ∃ next, m.mir.execution.step mir p next ∧ m.mirState next = state) ∧
  ∀ p result, m.mir.execution.finite mir p →
    (m.mir.result mir p result ↔ mir.terminalObservation (m.mirState p) = some result)

/-- Each target execution is matched against a source path whose checkpoints
retain the required physical representations. -/
def MemorySimulationWitness (m : LoweringModels language lang profile)
    (mir : MIR lang profile) (memory : Memory lang profile) : Prop :=
  memory.project = mir.project ∧ memory.entry = mir.entry ∧
  PreservesOrigins mir.origins memory.origins ∧ LayoutsMatch language.layout memory ∧
  memory.signatures = mir.signatures ∧ PromotionSound (language := language) mir memory ∧
  memory.ExecutionExact m ∧ mir.ExecutionExact m ∧ memory.TablesWF ∧ mir.logicalView.TablesWF ∧
  m.mir.Refines m.memory mir memory ∧
  ∃ sim : CheckpointSimulation m.mir.execution m.memory.execution m.mir.result m.memory.result mir memory,
    ∀ p q, m.mir.execution.finite mir p → m.memory.execution.finite memory q →
      sim.related p q → memory.RepresentsCheckpoint (m.mirState p) (m.memoryState q)

end Concrete.Interfaces
