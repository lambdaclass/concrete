import Concrete.Interfaces.Carriers
import Concrete.Contracts

namespace Concrete.Interfaces

open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface
open Concrete.Contracts (ProcessEvent Semantics EvaluatorAdequacy EvidenceKind Graph StutteringSimulation)

inductive Observation where
  | returned (status : UInt8) (stderr : ByteArray)
  | trapped (status : UInt8) (kind : TrapKind) (site : FileSpan) (stderr : ByteArray)
  | startupRefused (status : UInt8) (stderr : ByteArray)

def Observation.stderr : Observation → ByteArray
  | .returned _ bytes | .trapped _ _ _ bytes | .startupRefused _ bytes => bytes

def Observation.SameRuntimeResult (observed : Observation)
    (legacy : Concrete.Contracts.Observation) : Prop :=
  match observed, legacy with
  | .returned a bytes, .returned b other => a = b ∧ bytes = other
  | .trapped a k span bytes, .trapped b j site other =>
    a = b ∧ k = j ∧ span.line = site.location.line ∧ span.col = site.location.col ∧ bytes = other
  | .startupRefused a bytes, .startupRefused b other => a = b ∧ bytes = other
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

def Model.Simulates {A B E : Type} (source : Model A E) (target : Model B E)
    (a : A) (b : B) : Prop :=
  ∃ sim : Concrete.Contracts.Simulates source.execution target.execution a b,
    ∀ p q observed, source.execution.finite a p → target.execution.finite b q →
      sim.simulation.stateRel p q → target.result b q observed →
      ∃ p', Concrete.Contracts.Steps (source.execution.step a) p p' ∧
        source.result a p' observed ∧ source.execution.trace a p' = target.execution.trace b q

/-- An external edge can state refinement directly, with the same exact terminal projection. -/
def Model.Refines {A B E : Type} (source : Model A E) (target : Model B E)
    (a : A) (b : B) : Prop :=
  Concrete.Contracts.Refines source.execution target.execution a b ∧
  ∀ state observed, target.execution.finite b state → target.result b state observed →
    ∃ before, source.execution.finite a before ∧ source.result a before observed ∧
      source.execution.trace a before = target.execution.trace b state

/-- Evaluator adequacy stays separate from the fuel-free lowering proposition. -/
abbrev EvaluatorContract {A : Type} (model : Model A Empty)
    (evaluate : A → Nat → Concrete.Contracts.Evaluation) :=
  EvaluatorAdequacy model.execution evaluate

/-- The registry owns the function value and uses it for both execution and the proof graph. -/
structure Stage (Input Failure Output : Type) where
  run : Input → Except Failure Output
  kind : EvidenceKind
  inputInvariant : Input → Prop
  outputInvariant : Output → Prop
  success : Input → Output → Prop
  refusal : Input → Failure → Prop

def Stage.Sound {A R B : Type} (edge : Stage A R B) : Prop :=
  (∀ a b, edge.inputInvariant a → Graph edge.run a b →
    edge.outputInvariant b ∧ edge.success a b) ∧
  (∀ a r, edge.inputInvariant a → edge.run a = .error r → edge.refusal a r)

def Stage.Compatible {A R B S C : Type} (left : Stage A R B) (right : Stage B S C) : Prop :=
  ∀ b, left.outputInvariant b → right.inputInvariant b

/-- Composition uses heterogeneous stage entries directly, including their refusal channels. -/
inductive Pipeline : Type → Type → Type 1 where
  | done {A : Type} : Pipeline A A
  | next {A R B C : Type} (edge : Stage A R B)
      (rest : Pipeline B C) : Pipeline A C

def Pipeline.run {A C : Type} (pipeline : Pipeline A C) (input : A) :
    Except (Σ R : Type, R) C :=
  match pipeline with
  | .done => .ok input
  | .next stage rest =>
    match stage.run input with
    | .error failure => .error ⟨_, failure⟩
    | .ok output => rest.run output

structure FrontendSpec (Ast : Type) (language : CoreLanguage) where
  lexes : Source → TokenStream → Prop
  reconstruct : TokenStream → ByteArray
  malformedToken : Source → RuleId → FileSpan → Prop
  lexLimit : Source → ResourceRefusal → Prop
  derives : ParserView → Ast → Prop
  malformed : ParserView → RuleId → FileSpan → Prop
  unsupported : ParserView → RuleId → FileSpan → Slice → Prop
  parseSteps : ParserView → Ast → Nat → Prop
  parseBound : ParserView → Nat
  elaborates : Parsed Ast → Payload language → Prop
  elabRefusal : Parsed Ast → ElabRefusal → Prop

def Lexes {Ast : Type} {language : CoreLanguage} (spec : FrontendSpec Ast language)
    (source : Source) (stream : TokenStream) : Prop :=
  stream.source = source ∧ spec.lexes source stream ∧
  stream.trivia.size = stream.tokens.size + 1 ∧
  ∃ file, source.project.files[source.entryFile.index]? = some file ∧
    (spec.reconstruct stream).data.toList = file.source.toBytes

def Parses {Ast : Type} {language : CoreLanguage} (spec : FrontendSpec Ast language)
    (stream : ParserView) (ast : Parsed Ast) : Prop :=
  ast.source = stream.source ∧ spec.derives stream ast.tree ∧
  ∃ n, spec.parseSteps stream ast.tree n ∧ n ≤ spec.parseBound stream

def Elaborates {Ast : Type} {language : CoreLanguage} (spec : FrontendSpec Ast language)
    (ast : Parsed Ast) (core : CoreInput language) : Prop :=
  core.project = ast.source.project ∧ core.slice = ast.source.slice ∧
  core.LocationsWF ∧ spec.elaborates ast core.payload

/-- The actual admission entry calls exactly the function supplied by ConcreteSpec. -/
def check {language : CoreLanguage} (spec : Admission language) (core : CoreInput language) :
    Except CheckerRefusal (Admitted language) :=
  (spec.check core).map fun discharges => ⟨core, discharges⟩

def Admits {language : CoreLanguage} (spec : Admission language)
    (core : CoreInput language) (out : Admitted language) : Prop :=
  out.core = core ∧ spec.accepted core ∧ spec.statics core ∧
    spec.fragment core.slice core.payload ∧ spec.ValidDischarges core out.discharges

/-- Origin maps enumerate emitted copies as well as discharged and residual operations. -/
def PreservesOrigins (source target : List Origin) : Prop :=
  (∀ o ∈ target, o ∈ source) ∧ (∀ o ∈ source, o ∈ target)

def CFG.origins {P I T : Type} (graph : CFG P (LocatedOp I) T)
    (termOrigins : T → List Origin) : List Origin :=
  graph.blocks.toList.flatMap fun block =>
    block.body.map LocatedOp.origin ++ termOrigins block.ending

def Terminator.origins {A C : Type} : Terminator A C → List Origin
  | .trap _ origin => [origin]
  | _ => []

/-- Each removed check has an admission discharge. Each raw checked operation has either this
justification or a controlling residual guard on every incoming path. -/
structure GuardJudgment {Ty : Type} (lang : LoweringLanguage Ty) (profile : Profile) where
  unchecked : MIR lang profile → Origin → ObligationId → Prop
  residual : MIR lang profile → Origin → ObligationId → Prop
  coverage : MIR lang profile → Prop

def ConsumesEvidence {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (guards : GuardJudgment lang profile)
    (input : Admitted language) (mir : MIR lang profile) : Prop :=
  guards.coverage mir ∧
  (∀ origin obligation, guards.unchecked mir origin obligation ↔
    ∃ d, input.discharges.Contains origin.operation d ∧ d.key = origin.evidenceKey ∧
      d.obligation = obligation) ∧
  (∀ origin obligation, guards.residual mir origin obligation →
    ¬ guards.unchecked mir origin obligation)

def UncheckedGuards {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (guards : GuardJudgment lang profile) (mir : MIR lang profile) : Prop :=
  guards.coverage mir ∧ ∀ origin obligation, ¬ guards.unchecked mir origin obligation

def MIR.origins {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (mir : MIR lang profile) : List Origin :=
  mir.functions.flatMap fun fn => fn.2.2.origins Terminator.origins

def Memory.origins {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (memory : Memory lang profile) : List Origin :=
  memory.functions.flatMap fun fn => fn.2.2.origins Terminator.origins

def SSA.origins {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (ssa : SSA lang profile) : List Origin :=
  ssa.functions.flatMap fun fn => fn.2.2.origins Terminator.origins

def BIR.origins {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) : List Origin :=
  bir.functions.flatMap fun fn => fn.2.2.origins fun
    | .trap slot => (bir.sites[slot.index]?.map TrapEntry.origin).toList
    | _ => []

def BIR.TrapSlotsWF {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) : Prop :=
  ∀ fn ∈ bir.functions, ∀ block ∈ fn.2.2.blocks.toList, ∀ slot,
    block.ending = .trap slot → ∃ entry, bir.sites[slot.index]? = some entry

/-- Every materialized root gets the spec layout. Virtual roots need no byte allocation. -/
def LayoutsMatch {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (layout : LayoutOf Ty) (memory : Memory lang profile) : Prop :=
  (∀ fn locals graph, (fn, locals, graph) ∈ memory.functions →
    ∀ place ty, (place, ty) ∈ locals →
      ∃ classes, AList.lookup memory.storage fn = some classes ∧
        (AList.lookup classes place = some .virtual ∨
          (AList.lookup classes place = some .materialized ∧
            ∃ slots description, AList.lookup memory.layouts fn = some slots ∧
              AList.lookup slots place = some description ∧
              layout profile.memoryTarget ty = some description))) ∧
  ∀ fn slots place description, (fn, slots) ∈ memory.layouts → (place, description) ∈ slots →
    ∃ locals graph ty classes, (fn, locals, graph) ∈ memory.functions ∧ (place, ty) ∈ locals ∧
      AList.lookup memory.storage fn = some classes ∧
      AList.lookup classes place = some .materialized ∧ layout profile.memoryTarget ty = some description

def PromotionSound {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (scalar : Ty → Prop) (escapes : MIR lang profile → FnId → PlaceId → Prop)
    (mir : MIR lang profile) (memory : Memory lang profile) : Prop :=
  ∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
    ∀ place ty, (place, ty) ∈ locals →
      ∃ classes, AList.lookup memory.storage fn = some classes ∧
        ((AList.lookup classes place = some .virtual) ↔ scalar ty ∧ ¬ escapes mir fn place)

/-- Relate the shared theorem model to actual spec configurations and spec transition labels. -/
structure CoreModel (language : CoreLanguage) where
  model : Model (CoreInput language) language.operands.Event
  config : (a : CoreInput language) → model.execution.State → Exec.Config a.payload.machine
  projectFinal : CoreInput language → Exec.Final language.operands.Val → Observation → Prop
  initial : (a : CoreInput language) → Exec.Config a.payload.machine → Prop
  initialExact : ∀ a, (∀ state, model.execution.initial a state → initial a (config a state)) ∧
    (∀ q, initial a q → ∃ state, model.execution.initial a state ∧ config a state = q)
  stepsExact : ∀ a state next, model.execution.step a state next →
    ∃ label, Exec.Step a.payload.body (config a state) label (config a next) ∧
      Concrete.Contracts.TracePrefix
        (model.execution.trace a state ++ label.events.map ProcessEvent.effect)
        (model.execution.trace a next)
  stepsComplete : ∀ a state label q, model.execution.finite a state →
    Exec.Step a.payload.body (config a state) label q →
    ∃ next, model.execution.step a state next ∧ config a next = q
  terminalExact : ∀ a state observed, model.result a state observed ↔
    ∃ outcome, (config a state).final? = some outcome ∧ projectFinal a outcome observed
  trapExact : ∀ a kind operation observed, projectFinal a (.trapped kind operation) observed →
    ∃ origin status bytes, (operation, origin) ∈ a.payload.origins ∧
      observed = .trapped status kind origin.lexical bytes

structure LoweringModels (language : CoreLanguage) (lang : LoweringLanguage language.operands.Ty)
    (profile : Profile) where
  core : CoreModel language
  mir : Model (MIR lang profile) language.operands.Event
  memory : Model (Memory lang profile) language.operands.Event
  ssa : Model (SSA lang profile) language.operands.Event
  bir : Model (BIR lang profile) language.operands.Event
  llvm : Model (LLVMInput profile) language.operands.Event
  lcb : Model (LCBInput profile) language.operands.Event
  layout : LayoutOf language.operands.Ty
  guards : GuardJudgment lang profile
  mirInvariant : MIR lang profile → Prop
  memoryInvariant : Memory lang profile → Prop
  ssaInvariant : SSA lang profile → Prop
  birInvariant : BIR lang profile → Prop
  memoryState : memory.execution.State → MemoryState language.operands.Val profile
  represents : MIR lang profile → Memory lang profile → mir.execution.State →
    MemoryState language.operands.Val profile → Prop
  scalar : language.operands.Ty → Prop
  escapes : MIR lang profile → FnId → PlaceId → Prop
  frameSlots : BIR lang profile → Nat
  llvmHandoff : BIR lang profile → LLVMInput profile → Prop
  lcbHandoff : BIR lang profile → LCBInput profile → Prop

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

def LowersCore (m : LoweringModels language lang profile)
    (input : Admitted language) (mir : MIR lang profile) : Prop :=
  mir.project = input.core.project ∧ mir.entry = input.core.payload.entry ∧
  PreservesOrigins (List.map Prod.snd input.core.payload.origins) mir.origins ∧
  ConsumesEvidence m.guards input mir ∧ m.core.model.Simulates m.mir input.core mir

def LowersUnchecked (m : LoweringModels language lang profile)
    (input : CoreInput language) (mir : MIR lang profile) : Prop :=
  mir.project = input.project ∧ mir.entry = input.payload.entry ∧
  PreservesOrigins (List.map Prod.snd input.payload.origins) mir.origins ∧
  UncheckedGuards m.guards mir ∧ m.core.model.Simulates m.mir input mir

def LowersMemory (m : LoweringModels language lang profile)
    (mir : MIR lang profile) (memory : Memory lang profile) : Prop :=
  memory.project = mir.project ∧ memory.entry = mir.entry ∧
  PreservesOrigins mir.origins memory.origins ∧ LayoutsMatch m.layout memory ∧
  memory.signatures = mir.signatures ∧
  PromotionSound m.scalar m.escapes mir memory ∧ m.mir.Simulates m.memory mir memory ∧
  ∀ q, m.memory.execution.finite memory q →
    (m.memoryState q).BytesWF ∧
    ∃ p, m.mir.execution.finite mir p ∧ m.represents mir memory p (m.memoryState q)

def BuildsSSA (m : LoweringModels language lang profile)
    (memory : Memory lang profile) (ssa : SSA lang profile) : Prop :=
  ssa.project = memory.project ∧ ssa.entry = memory.entry ∧ ssa.layouts = memory.layouts ∧
  ssa.signatures = memory.signatures ∧ PreservesOrigins memory.origins ssa.origins ∧ m.memory.Simulates m.ssa memory ssa

def BIR.TrapsMatch {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (ssa : SSA lang profile) (bir : BIR lang profile) : Prop :=
  ∀ fn ∈ ssa.functions, ∀ block ∈ fn.2.2.blocks.toList, ∀ kind origin,
    block.ending = .trap kind origin →
    ∃ after ∈ bir.functions, after.1 = fn.1 ∧
      ∃ next ∈ after.2.2.blocks.toList, next.id = block.id ∧
        ∃ slot, next.ending = .trap slot ∧
          ∃ entry, bir.sites[slot.index]? = some entry ∧ entry.kind = kind ∧ entry.origin = origin

def BuildsBIR (m : LoweringModels language lang profile)
    (ssa : SSA lang profile) (bir : BIR lang profile) : Prop :=
  bir.project = ssa.project ∧ bir.entry = ssa.entry ∧ bir.layouts = ssa.layouts ∧
  bir.signatures = ssa.signatures ∧ bir.TrapSlotsWF ∧ bir.TrapsMatch ssa ∧ PreservesOrigins ssa.origins bir.origins ∧
  m.frameSlots bir ≤ profile.maxBIRSlots ∧ m.ssa.Simulates m.bir ssa bir

def EmitsLLVM (m : LoweringModels language lang profile)
    (bir : BIR lang profile) (output : LLVMInput profile) : Prop :=
  output.sites = bir.sites ∧ m.llvmHandoff bir output ∧ m.bir.Refines m.llvm bir output

def EmitsLCB (m : LoweringModels language lang profile)
    (bir : BIR lang profile) (output : LCBInput profile) : Prop :=
  output.sites = bir.sites ∧ m.lcbHandoff bir output ∧ m.bir.Simulates m.lcb bir output

end Concrete.Interfaces
