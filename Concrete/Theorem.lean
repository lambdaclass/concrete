import Concrete.Stages
import Concrete.Backend.Observation
import LeanCompilerBackend.Verified.BuildIdentity

namespace Concrete.ImageTheorem

open Contracts Backend
open LeanCompilerBackend.Verified
open ConcreteSpec.Surface0 (Token Derives Lexes Program Expr Span Binding BinderLegal checkBody)

/-- Independent, name-parametric elaboration of operands and operations. The relation
records the origin of each introduced operation and permits any fresh binder IDs. -/
inductive ElaboratesExpr (scope : Elab.Scope) :
    Expr → Core.Atom → List Core.Stmt → List Elab.SourceSite → Prop where
  | integer (n span) : ElaboratesExpr scope (.integer n none span) (.lit n) [] []
  | local {name span x} : scope.lookup name = some x →
      ElaboratesExpr scope (.local name span) (.var x) [] []
  | add {lhs rhs site x a b left right ls rs} :
      ElaboratesExpr scope lhs a left ls → ElaboratesExpr scope rhs b right rs →
      ElaboratesExpr scope (.add lhs rhs site) (.var x)
        (left ++ right ++ [⟨x, .add a b⟩]) (ls ++ rs ++ [⟨x, site⟩])

inductive ElaboratesBindings :
    Elab.Scope → List Binding → Elab.Scope → List Core.Stmt → List Elab.SourceSite → Prop where
  | nil (scope) : ElaboratesBindings scope [] scope [] []
  | atom {scope binding rest final x a body sites} :
      BinderLegal (scope.map Prod.fst) binding.name →
      ElaboratesExpr scope binding.rhs a [] [] →
      ElaboratesBindings ((binding.name, x) :: scope) rest final body sites →
      ElaboratesBindings scope (binding :: rest) final (⟨x, .atom a⟩ :: body) sites
  | operation {scope binding rest final x head tail hs ts} :
      BinderLegal (scope.map Prod.fst) binding.name →
      ElaboratesExpr scope binding.rhs (.var x) head hs → head ≠ [] →
      ElaboratesBindings ((binding.name, x) :: scope) rest final tail ts →
      ElaboratesBindings scope (binding :: rest) final (head ++ tail) (hs ++ ts)

theorem ElaboratesBindings.headLegal {scope : Elab.Scope} {binding : Binding}
    {rest final body sites} (h : ElaboratesBindings scope (binding :: rest) final body sites) :
    BinderLegal (scope.map Prod.fst) binding.name := by
  cases h with
  | atom legal _ _ => exact legal
  | operation legal _ _ _ => exact legal

/-- Exactly one origin per addition, in evaluation order, with globally fresh binders. -/
def additionBinders (body : List Core.Stmt) : List ConcreteSpec.Var :=
  body.filterMap fun stmt => match stmt.e with
    | .atom _ => none
    | .add .. => some stmt.x

private theorem additionBinders_append (left right : List Core.Stmt) :
    additionBinders (left ++ right) = additionBinders left ++ additionBinders right :=
  List.filterMap_append

def OriginWellFormed (core : Elab.Output) : Prop :=
  (core.program.body.map (·.x)).Nodup ∧ core.sites.map (·.binding) = additionBinders core.program.body

private theorem elaboratesExpr_origins {scope : Elab.Scope} {expr : Expr} {ret : Core.Atom}
    {body : List Core.Stmt} {sites : List Elab.SourceSite}
    (h : ElaboratesExpr scope expr ret body sites) :
    sites.map (·.binding) = additionBinders body := by
  induction h with
  | integer | «local» => rfl
  | add _ _ ih₁ ih₂ => simp [List.map_append, additionBinders_append, ih₁, ih₂]; rfl

private theorem elaboratesBindings_origins {scope final : Elab.Scope} {bindings : List Binding}
    {body : List Core.Stmt} {sites : List Elab.SourceSite}
    (h : ElaboratesBindings scope bindings final body sites) :
    sites.map (·.binding) = additionBinders body := by
  induction h with
  | nil => rfl
  | atom _ _ _ ih => exact ih
  | operation _ h _ _ ih =>
    simp only [List.map_append, additionBinders_append]
    rw [elaboratesExpr_origins h, ih]

/-- Fresh IDs are checked across the whole elaboration, including temporary bindings. -/
def Elaborates (ast : Program) (core : Elab.Output) : Prop :=
  ast.name = "main" ∧ (core.program.body.map (·.x)).Nodup ∧
  ∃ bindings expr scope body tail sites origins,
    checkBody ast.body = .ok (bindings, expr) ∧
    ElaboratesBindings [] bindings scope body sites ∧
    ElaboratesExpr scope expr core.program.ret tail origins ∧
    core.program.body = body ++ tail ∧ core.sites = sites ++ origins

theorem Elaborates.originWellFormed {ast : Program} {core : Elab.Output}
    (h : Elaborates ast core) : OriginWellFormed core := by
  obtain ⟨_, fresh, _, _, _, _, _, _, _, _, bindings, expr, body, sites⟩ := h
  refine ⟨fresh, ?_⟩
  rw [body, sites, List.map_append, additionBinders_append]
  rw [elaboratesBindings_origins bindings, elaboratesExpr_origins expr]

private def atomFits (scope : List ConcreteSpec.Var) : Core.Atom → Prop
  | .lit n => n < 256
  | .var x => x ∈ scope

private def bodyFits (scope : List ConcreteSpec.Var) (result : Core.Atom) :
    List Core.Stmt → Prop
  | [] => atomFits scope result
  | stmt :: rest =>
    (match stmt.e with
      | .atom a => atomFits scope a
      | .add a b => atomFits scope a ∧ atomFits scope b) ∧
    bodyFits (stmt.x :: scope) result rest

/-- Slice-0 static typing and scope for the empty-evidence path. Checked addition need
only have typed operands here, so an overflowing operation remains eligible. -/
def ConformanceStatic (core : Elab.Output) : Prop :=
  bodyFits [] core.program.ret core.program.body

/-- A lexical operation is found by binder. The selected residual layout determines its
runtime index. The empty-evidence Core execution uses every origin in the layout. -/
private def coreEvaluationWith (layout : List Elab.SourceSite)
    (input : String × Elab.Output) (fuel : Nat) : Evaluation :=
  let (path, core) := input
  let program := Core.toCore core.program
  match Core.run program fuel with
  | .exit status => .finished (.returned (UInt8.ofNat status) ByteArray.empty)
  | .outOfFuel => .running
  | .stuck _ | .aborted _ => .stuck
  | .trap .arithmeticOverflow => Id.run do
    let some index := (List.range core.program.body.length).find? (fun i =>
      match Core.run program (i + 1) with | .trap _ => true | _ => false) | return .stuck
    let some stmt := core.program.body[index]? | return .stuck
    let some (id, origin) := layout.zipIdx.findSome? (fun (site, id) =>
      if site.binding == stmt.x then some (id, site.operator) else none) | return .stuck
    return .finished (trapObservation ⟨.arithmeticOverflow, ⟨id⟩,
      ⟨origin.start, origin.stop⟩, ⟨path, origin.line, origin.column⟩⟩)

private def coreEvaluation (input : String × Elab.Output) (fuel : Nat) : Evaluation :=
  coreEvaluationWith input.2.sites input fuel

def coreSemantics : Semantics (String × Elab.Output) :=
  evaluatorSemantics coreEvaluation

/-- Admission evidence selects the residual operations. This fixes their runtime indices
on the source side from exact Core alone, independently of the emitted table. -/
def admittedDiagnosticOrigins (core : Elab.Output) : List Elab.SourceSite :=
  let evidence := Admission.evidence core.program
  core.sites.filter fun site => !evidence.any (·.binding == site.binding)

private def admittedEvaluation (input : String × Elab.Output) (fuel : Nat) : Evaluation :=
  coreEvaluationWith (admittedDiagnosticOrigins input.2) input fuel

def admittedCoreSemantics : Semantics (String × Elab.Output) :=
  evaluatorSemantics admittedEvaluation

def SourceTerminates (path : String) (core : Elab.Output)
    (source : Semantics (String × Elab.Output) := coreSemantics) : Prop :=
  (∃ o, source.observes (path, core) o) ∧
  (∀ run, ¬ source.diverges (path, core) run)

/-- Independent, path-indexed semantics are required for the remaining lowering stages.
Memory is also selected by the theorem's target profile. -/
structure SharedModels (_target : LCB.Target) where
  mir : Semantics (String × MIR.Program)
  memory : Semantics (String × Memory.Program)
  evalMIR : (String × MIR.Program) → Nat → Evaluation
  evalMemory : (String × Memory.Program) → Nat → Evaluation
  mirInvariant : MIR.Program → Prop
  memoryInvariant : Memory.Program → Prop

private def slice0Outcome : Observation → Option ConcreteSpec.Outcome
  | .returned status _ => some (.exit status.toNat)
  | .trapped _ .overflow _ _ => some (.trap .arithmeticOverflow)
  | _ => none

/-- Current MIR and Memory evaluators erase diagnostic details. Their independently defined
transition models must agree on the outcome, with full diagnostics checked by the simulations. -/
def Slice0OutcomeAgrees {A : Type} (s : Semantics A) (input : A)
    (expected : ConcreteSpec.Outcome) : Prop :=
  (∀ o, s.observes input o → slice0Outcome o = some expected) ∧
  ((∃ o, s.observes input o) ↔
    (∃ status, expected = .exit status) ∨ (∃ trap, expected = .trap trap)) ∧
  ((∃ q, s.stuck input q) ↔ ∃ reason, expected = .stuck reason) ∧
  ((∃ run, s.diverges input run) ↔ expected = .outOfFuel)

private def residualOrigins (memory : Memory.Program) : List Elab.SourceSite :=
  memory.stores.filterMap fun store =>
    match store.value with
    | .add _ _ origin none => some origin
    | _ => none

/-- The slice-0 table has one entry per residual origin, in emission order. Its index
is the runtime lookup key. The origin retains the operation's binder and exact span. -/
def SiteOrigins (path : String) (core : Elab.Output) (memory : Memory.Program)
    (bir : BIR.Program) : Prop :=
  let origins := residualOrigins memory
  origins.Sublist core.sites ∧
  bir.sites = origins.zipIdx.map fun (origin, id) =>
    let span := origin.operator
    ⟨.arithmeticOverflow, ⟨id⟩, ⟨span.start, span.stop⟩, ⟨path, span.line, span.column⟩⟩

/-- MIR carries every addition origin, including discharged ones, in Core order. -/
def mirOrigins (mir : MIR.Program) : List Elab.SourceSite :=
  mir.body.filterMap fun assignment => match assignment.op with
    | .copy _ => none
    | .add _ _ origin _ => some origin

def residualMIROrigins (mir : MIR.Program) : List Elab.SourceSite :=
  mir.body.filterMap fun assignment => match assignment.op with
    | .add _ _ origin none => some origin
    | _ => none

def LexerContract : EdgeContract (Graph Lex.lex) where
  kind := .directProof
  inputInvariant := fun _ => True
  outputInvariant := fun tokens => ∃ bytes, Lexes bytes tokens
  correspondence := Lexes

def ParserContract : EdgeContract (Graph Parse.parse) where
  kind := .namedPremise "P_parse"
  inputInvariant := LexerContract.outputInvariant
  outputInvariant := fun ast => ∃ tokens, Derives tokens ast
  correspondence := Derives

def ElaborationContract : EdgeContract (Graph Elab.elaborate) where
  kind := .namedPremise "P_elab"
  inputInvariant := ParserContract.outputInvariant
  outputInvariant := OriginWellFormed
  correspondence := Elaborates

def AdmissionContract : EdgeContract (Graph Admission.check) where
  kind := .directProof
  inputInvariant := OriginWellFormed
  outputInvariant := fun admitted =>
    OriginWellFormed admitted.core ∧ Core.Checked (Core.toCore admitted.core.program) ∧
      ConformanceStatic admitted.core ∧
      admitted.tokens = Admission.evidence admitted.core.program ∧
      ∀ path, SourceTerminates path admitted.core admittedCoreSemantics
  correspondence := fun core admitted =>
    admitted.core = core ∧ admitted.tokens = Admission.evidence core.program

/-- Path indices are retained on both sides of each lowering, including SSA's actual path argument. -/
def AdmittedMIRContract {target : LCB.Target} (m : SharedModels target) :
    LoweringContract (fun (a : String × Admission.Output) (b : String × MIR.Program) =>
      a.1 = b.1 ∧ Graph MIR.lower a.2 b.2)
      (admittedCoreSemantics.onInput fun (path, admitted) => (path, admitted.core)) m.mir where
  kind := .directProof
  inputInvariant := fun a => AdmissionContract.outputInvariant a.2
  outputInvariant := fun b => m.mirInvariant b.2

def UncheckedMIRContract {target : LCB.Target} (m : SharedModels target) :
    LoweringContract (fun (a : String × Elab.Output) (b : String × MIR.Program) =>
      a.1 = b.1 ∧ Graph MIR.lowerUnchecked a.2 b.2) coreSemantics m.mir where
  kind := .directProof
  inputInvariant := fun a => OriginWellFormed a.2 ∧ ConformanceStatic a.2
  outputInvariant := fun b => m.mirInvariant b.2

def MemoryContract {target : LCB.Target} (m : SharedModels target) :
    LoweringContract (TotalGraph fun (a : String × MIR.Program) => (a.1, Memory.lower a.2))
      m.mir m.memory where
  kind := .directProof
  inputInvariant := fun a => m.mirInvariant a.2
  outputInvariant := fun b => m.memoryInvariant b.2

def SSAContract {target : LCB.Target} (m : SharedModels target) :
    LoweringContract (fun (a : String × Memory.Program) => Graph (SSA.lower a.1) a.2)
      m.memory birSemantics where
  kind := .provedValidator
  inputInvariant := fun a => m.memoryInvariant a.2
  outputInvariant := BIR.WF

/-- Registry soundness includes the edge invariants, their compatibility and exact origin
preservation. Evaluator adequacy also compares finite prefixes and arbitrary fuel exhaustion. -/
structure SharedSound {target : LCB.Target} (m : SharedModels target) : Prop where
  lexer : LexerContract.sound
  parser : ParserContract.sound
  elaboration : ElaborationContract.sound
  admission : AdmissionContract.sound
  admittedMIR : (AdmittedMIRContract m).sound
  uncheckedMIR : (UncheckedMIRContract m).sound
  memory : (MemoryContract m).sound
  ssa : (SSAContract m).sound
  lexParse : Compatible LexerContract ParserContract
  parseElab : Compatible ParserContract ElaborationContract
  elabAdmit : Compatible ElaborationContract AdmissionContract
  elabUnchecked : ∀ core, ElaborationContract.outputInvariant core → ConformanceStatic core →
    ∀ path, (UncheckedMIRContract m).inputInvariant (path, core)
  admitMIR : ∀ admitted, AdmissionContract.outputInvariant admitted →
    ∀ path, (AdmittedMIRContract m).inputInvariant (path, admitted)
  admittedMemory : Compatible (AdmittedMIRContract m).edge (MemoryContract m).edge
  uncheckedMemory : Compatible (UncheckedMIRContract m).edge (MemoryContract m).edge
  memorySSA : Compatible (MemoryContract m).edge (SSAContract m).edge
  admittedOrigins : ∀ path admitted mir, (AdmittedMIRContract m).inputInvariant (path, admitted) →
    Graph MIR.lower admitted mir → mirOrigins mir = admitted.core.sites ∧
      residualMIROrigins mir = admittedDiagnosticOrigins admitted.core
  uncheckedOrigins : ∀ path core mir, (UncheckedMIRContract m).inputInvariant (path, core) →
    Graph MIR.lowerUnchecked core mir → residualMIROrigins mir = core.sites
  memoryOrigins : ∀ mir, m.mirInvariant mir → residualOrigins (Memory.lower mir) = residualMIROrigins mir
  ssaOrigins : ∀ path core mem bir, (SSAContract m).inputInvariant (path, mem) →
    Graph (SSA.lower path) mem bir → (residualOrigins mem).Sublist core.sites →
    SiteOrigins path core mem bir
  mirAdequacy : ∀ path mir, m.mirInvariant mir →
    EvaluatorAdequacyAt m.mir m.evalMIR (path, mir) ∧
    Slice0OutcomeAgrees m.mir (path, mir) (MIR.run mir)
  memoryAdequacy : ∀ path mem, m.memoryInvariant mem →
    EvaluatorAdequacyAt m.memory m.evalMemory (path, mem) ∧
    Slice0OutcomeAgrees m.memory (path, mem) (Memory.run mem)
  birAdequacy : EvaluatorAdequacy birSemantics birEvaluation

/-- Exact source-to-admitted-Core derivation recorded in the production output. -/
def Frontend (input : Stages.Input) (out : Stages.Output) : Prop :=
  Lexes input.source out.tokens ∧ Derives out.tokens out.ast ∧
  Elaborates out.ast out.core ∧
  ∃ admitted, Admission.check out.core = .ok admitted ∧ admitted.core = out.core ∧
    admitted.tokens = out.evidence ∧ Core.Checked (Core.toCore out.core.program)

/-- Successful production exposes the exact intermediate results of the functions it invokes. -/
def SharedHandoffs (input : Stages.Input) (out : Stages.Output) : Prop :=
  Lex.lex input.source = .ok out.tokens ∧ Parse.parse out.tokens = .ok out.ast ∧
  Elab.elaborate out.ast = .ok out.core ∧
  (∃ admitted, Admission.check out.core = .ok admitted ∧ admitted.tokens = out.evidence ∧
    MIR.lower admitted = .ok out.mir) ∧
  Memory.lower out.mir = out.memory ∧ SSA.lower input.path out.memory = .ok out.bir

/-- Native lowering paths, with the LLVM fast path and both LCB path classes visible. -/
inductive LoweringPath where
  | llvm
  | lcbVerified
  | lcbCompatibility

/-- The residual assumptions of the image claim. Keep their planned dispositions
with the statement until step 4(a) derives the inventory from the composed proof. -/
inductive ExternalPremise where
  | llvmPrinter | llvmObject | lcbNative | lcbCompatibility | runtimeLowering
  | assembler | linkInputs | linker | decoder | loader | runtime | trapRendering
  | imageStack | launch | osLiveness | stderrProgress
  deriving DecidableEq, Repr

def ExternalPremise.name : ExternalPremise → String
  | .llvmPrinter => "LLVM printer"
  | .llvmObject => "LLVM object compilation"
  | .lcbNative => "LCB verified native lowering"
  | .lcbCompatibility => "LCB Linux compatibility lowering"
  | .runtimeLowering => "LCB runtime lowering"
  | .assembler => "assembler"
  | .linkInputs => "driver-resolved linker inputs"
  | .linker => "linker"
  | .decoder => "exact image decoding"
  | .loader => "loader"
  | .runtime => "runtime"
  | .trapRendering => "trap rendering"
  | .imageStack => "image stack budget"
  | .launch => "launch admission"
  | .osLiveness => "OS liveness"
  | .stderrProgress => "stderr and exit progress"

def ExternalPremise.disposition : ExternalPremise → String
  | .llvmPrinter | .llvmObject => "LLVM fast-path premise"
  | .lcbNative => "LP0"
  | .lcbCompatibility => "Linux compatibility premise"
  | .runtimeLowering | .runtime | .trapRendering => "R-0621"
  | .assembler | .linkInputs | .linker => "R-0517 / R-0505"
  | .decoder | .loader => "R-0505"
  | .imageStack => "B0a LLVM / B0b LCB measurement; R-0505 proof"
  | .launch | .osLiveness | .stderrProgress => "continuing environment premise"

def LoweringPath.premises (path : LoweringPath) : List ExternalPremise :=
  (match path with
    | .llvm => [.llvmPrinter, .llvmObject]
    | .lcbVerified => [.lcbNative, .runtimeLowering, .assembler]
    | .lcbCompatibility => [.lcbCompatibility, .runtimeLowering, .assembler]) ++
  [.linkInputs, .linker, .decoder, .loader, .runtime, .trapRendering,
    .imageStack, .launch, .osLiveness, .stderrProgress]

/-- The in-process LCB dispatcher selects its lowering path by target. -/
def lcbPath : LCB.Target → LoweringPath
  | .aarch64Macos => .lcbVerified
  | .x86_64Linux | .aarch64Linux => .lcbCompatibility

/-- Tool slots distinguish invocations even when the same binary serves several roles. -/
inductive ToolRole where
  | llvmObject | runtimeLowering | bodyAssembler | runtimeAssembler | linker
  deriving DecidableEq

structure ToolInvocation where
  role : ToolRole
  tool : String
  revision : String
  arguments : List String
  deriving DecidableEq

structure Profile where
  target : LCB.Target
  maxBIRSlots : Nat
  maxImageStackBytes : Nat
  tools : List ToolInvocation

/-- The build identity check compares this kernel-reducible pin to Lake's declared
and resolved revisions. Handoffs also check the imported backend compiler identity. -/
def pinnedLCBRevision : Option String :=
  some "4bea4b3ffd8a43a73a0016012d865044067b3275"

def Profile.invocation (profile : Profile) (role : ToolRole) (pin : ToolInvocation) : Prop :=
  profile.tools.filter (·.role == role) = [pin]

/-- Actual LLVM artifacts, from the full module input through the linked image. -/
structure LLVMBuild where
  moduleBytes : ByteArray
  objectBytes : ByteArray
  linkInputs : List ByteArray
  image : ByteArray

/-- Actual LCB artifacts, including the exact value passed in-process, the runtime reification
input, both assembly inputs, and the ordered linker inputs. -/
structure LCBBuild where
  revision : String
  compilerIdentity : String
  source : Source.Function
  sites : BIR.SiteTable
  runtimeInput : ByteArray
  bodyAssembly : ByteArray
  runtimeAssembly : ByteArray
  bodyObject : ByteArray
  runtimeObject : ByteArray
  linkInputs : List ByteArray
  image : ByteArray

/-- Each actual invocation is indexed by its effective revision and argument vector. -/
structure NativeTools where
  llvmObject : ToolInvocation → ByteArray → ByteArray → Prop
  runtimeLowering : ToolInvocation → ByteArray → ByteArray → Prop
  assemble : ToolInvocation → ByteArray → ByteArray → Prop
  link : ToolInvocation → List ByteArray → List ByteArray → ByteArray → Prop

def LLVMHandoffs (profile : Profile) (tools : NativeTools) (bir : BIR.Program)
    (build : LLVMBuild) : Prop :=
  build.moduleBytes = (LLVM.module bir).toUTF8 ∧
  (∃ pin, profile.invocation .llvmObject pin ∧
    tools.llvmObject pin build.moduleBytes build.objectBytes) ∧
  (∃ pin, profile.invocation .linker pin ∧
    tools.link pin [build.objectBytes] build.linkInputs build.image)

def LCBHandoffs (profile : Profile) (tools : NativeTools) (bir : BIR.Program)
    (build : LCBBuild) : Prop :=
  pinnedLCBRevision = some build.revision ∧
  build.compilerIdentity = compilerBuildIdentity ∧
  LCB.emit bir = .ok (build.source, build.sites) ∧
  validateJob (LCB.job build.source) = .ok () ∧
  Stages.prepareLCB bir = .ok build.source ∧
  build.sites = bir.sites ∧ build.runtimeInput = (Runtime.qbe build.sites).toUTF8 ∧
  (∃ assembly, LCB.assemble profile.target build.source = .ok assembly ∧
    build.bodyAssembly = assembly.toUTF8) ∧
  (∃ pin, profile.invocation .runtimeLowering pin ∧
    tools.runtimeLowering pin build.runtimeInput build.runtimeAssembly) ∧
  (∃ pin, profile.invocation .bodyAssembler pin ∧
    tools.assemble pin build.bodyAssembly build.bodyObject) ∧
  (∃ pin, profile.invocation .runtimeAssembler pin ∧
    tools.assemble pin build.runtimeAssembly build.runtimeObject) ∧
  (∃ pin, profile.invocation .linker pin ∧
    tools.link pin [build.bodyObject, build.runtimeObject] build.linkInputs build.image)

/-- Resource and environment predicates for a verifier's selected profile. -/
structure ExecutionPolicy (Env : Type) where
  stackAdequate : Profile → ByteArray → Env → Nat → Prop
  launchAdmitted : Profile → ByteArray → Env → Prop
  osLive : Profile → ByteArray → Env → Prop
  stderrAndExitAvailable : Profile → ByteArray → Env → Prop

/-- Independently specified platform image semantics. The decoder is indexed by the
target and consumes the exact final image, including entry and relocation information.
The machine starts from the decoded image and the actual launch environment. -/
structure TargetReference (Env : Type) (_target : LCB.Target) where
  process : Semantics (ByteArray × Env)
  Image : Type
  decode : ByteArray → Option Image
  machine : Semantics (Option Image × Env)

def TargetReference.decodedProcess {Env : Type} {target : LCB.Target} (platform : TargetReference Env target) :
    Semantics (ByteArray × Env) :=
  platform.machine.onInput fun (image, env) => (platform.decode image, env)

abbrev NativeReferenceRegistry :=
  (target : LCB.Target) → Option (Σ env : Type, TargetReference env target)

/-- Verifier-owned native references. R-0505 supplies the decoded final-image
reference for the first production target. Until then, native image claims are
unavailable even on targets supported by the assembly emitter. -/
def nativeReference : NativeReferenceRegistry :=
  fun target => match target with
  | .x86_64Linux => none
  | .aarch64Linux => none
  | .aarch64Macos => none

/-- Native semantic interfaces indexed by the exact image, target and environment. The
intermediate views let linker, loader, runtime and trap rendering discharge distinct claims.
Each view projects to the common observation at its boundary. -/
structure NativeModels (Env : Type) (profile : Profile)
    (platform : TargetReference Env profile.target) where
  llvmModule : Semantics ByteArray
  llvmWellFormed : ByteArray → Prop
  lcbBodyAssembly : Semantics (ByteArray × ByteArray)
  lcbAssembly : Semantics (ByteArray × ByteArray)
  objectOperands : Semantics (List ByteArray)
  objects : Semantics (List ByteArray)
  linked : Semantics ByteArray
  loaded : Semantics (ByteArray × Env)
  runtime : Semantics (ByteArray × Env)
  stackAdequate : ByteArray → Env → Nat → Prop
  launchAdmitted : ByteArray → Env → Prop
  osLive : ByteArray → Env → Prop
  /-- The launch environment permits all required stderr writes and exit, including progress
  through short writes. The rendering edge specifies which bytes the run observes. -/
  stderrAndExitAvailable : ByteArray → Env → Prop

def NativeModels.process {Env : Type} {profile : Profile} {platform : TargetReference Env profile.target} (_n : NativeModels Env profile platform) : Semantics (ByteArray × Env) :=
  platform.process

/-- The native boundary uses the verifier’s resource and environment judgments. -/
def ExecutionPolicy.Anchors {Env : Type} {profile : Profile}
    {platform : TargetReference Env profile.target}
    (policy : ExecutionPolicy Env) (n : NativeModels Env profile platform) : Prop :=
  n.stackAdequate = policy.stackAdequate profile ∧
  n.launchAdmitted = policy.launchAdmitted profile ∧ n.osLive = policy.osLive profile ∧
  n.stderrAndExitAvailable = policy.stderrAndExitAvailable profile

/-- Image-level stack adequacy and launch admission are the finite-prefix safety conditions.
The stack budget is expressed in bytes and includes startup and runtime frames. -/
def LaunchReady {Env : Type} {profile : Profile} {platform : TargetReference Env profile.target} (n : NativeModels Env profile platform)
    (image : ByteArray) (env : Env) : Prop :=
  n.stackAdequate image env profile.maxImageStackBytes ∧ n.launchAdmitted image env

/-- Maximal execution also requires OS liveness and progress through stderr writes and exit. -/
def RunReady {Env : Type} {profile : Profile} {platform : TargetReference Env profile.target} (n : NativeModels Env profile platform)
    (image : ByteArray) (env : Env) : Prop :=
  LaunchReady n image env ∧ n.osLive image env ∧ n.stderrAndExitAvailable image env

/-- Shared shape of the residual image premises. These premises apply separately to each backend
and each artifact, with exact linker inputs and image bytes. Liveness supplies an observation
when finite-prefix safety and absence of target divergence have been established. -/
structure ImagePremises {Env : Type} {profile : Profile} {platform : TargetReference Env profile.target} (n : NativeModels Env profile platform)
    (inputs : List ByteArray) (image : ByteArray) : Prop where
  linker : Refines n.objects n.linked inputs image
  decoder : ∀ env, LaunchReady n image env →
    (∃ decoded, platform.decode image = some decoded) ∧
    PrefixSafety platform.decodedProcess n.process (image, env) (image, env)
  decodedExecution : ∀ env, RunReady n image env →
    MaximalRefinement platform.decodedProcess n.process (image, env) (image, env)
  decodedLiveness : ∀ env, RunReady n image env →
    (∃ o, platform.decodedProcess.observes (image, env) o) →
    ∃ o, n.process.observes (image, env) o
  loader : ∀ env, n.stackAdequate image env profile.maxImageStackBytes →
    n.launchAdmitted image env → Refines n.linked n.loaded image (image, env)
  runtimeSafety : ∀ env, LaunchReady n image env →
    PrefixSafety n.loaded n.runtime (image, env) (image, env)
  trapRenderingSafety : ∀ env, LaunchReady n image env →
    PrefixSafety n.runtime platform.decodedProcess (image, env) (image, env)
  runtime : ∀ env, RunReady n image env →
    MaximalRefinement n.loaded n.runtime (image, env) (image, env)
  trapRendering : ∀ env, RunReady n image env →
    MaximalRefinement n.runtime platform.decodedProcess (image, env) (image, env)
  liveness : ∀ env, RunReady n image env → PrefixSafety n.linked platform.decodedProcess image (image, env) →
    (¬ ∃ run, platform.decodedProcess.diverges (image, env) run) →
    ∃ o, platform.decodedProcess.observes (image, env) o

/-- The LLVM printer edge targets the complete module as written to the tool input. -/
def LLVMEmissionContract {Env : Type} {profile : Profile}
    {platform : TargetReference Env profile.target} (n : NativeModels Env profile platform) :
    EdgeContract (TotalGraph (fun bir => (LLVM.module bir).toUTF8)) where
  kind := .namedPremise "LLVM printer"
  inputInvariant := BIR.WF
  outputInvariant := n.llvmWellFormed
  correspondence := Refines birSemantics n.llvmModule

/-- LLVM's unverified printer and toolchain premises for this exact artifact. -/
structure LLVMPremises {Env : Type} {profile : Profile}
    {platform : TargetReference Env profile.target} (n : NativeModels Env profile platform) (bir : BIR.Program) (build : LLVMBuild) : Prop where
  printer : (LLVMEmissionContract n).outputInvariant build.moduleBytes ∧
    (LLVMEmissionContract n).correspondence bir build.moduleBytes
  toolchain : n.llvmWellFormed build.moduleBytes →
    Refines n.llvmModule n.objectOperands build.moduleBytes [build.objectBytes]
  resolveLinkInputs : Refines n.objectOperands n.objects [build.objectBytes] build.linkInputs
  image : ImagePremises n build.linkInputs build.image

/-- LCB's residual native premises for this exact artifact. The adapter has its own proof
obligation. Native lowering is indexed by the profile-selected path, including Linux reification. -/
structure LCBPremises {Env : Type} {profile : Profile} {platform : TargetReference Env profile.target} (n : NativeModels Env profile platform)
    (build : LCBBuild) : Prop where
  nativeLowering : validateJob (LCB.job build.source) = .ok () →
    lcbPath profile.target = .lcbVerified →
    Refines lcbSemantics n.lcbBodyAssembly (build.source, build.sites, build.runtimeInput)
      (build.bodyAssembly, build.runtimeInput)
  compatibilityLowering : validateJob (LCB.job build.source) = .ok () →
    lcbPath profile.target = .lcbCompatibility →
    Refines lcbSemantics n.lcbBodyAssembly (build.source, build.sites, build.runtimeInput)
      (build.bodyAssembly, build.runtimeInput)
  runtimeLowering : Refines n.lcbBodyAssembly n.lcbAssembly
    (build.bodyAssembly, build.runtimeInput) (build.bodyAssembly, build.runtimeAssembly)
  assembler : Refines n.lcbAssembly n.objectOperands (build.bodyAssembly, build.runtimeAssembly)
    [build.bodyObject, build.runtimeObject]
  resolveLinkInputs : Refines n.objectOperands n.objects [build.bodyObject, build.runtimeObject]
    build.linkInputs
  image : ImagePremises n build.linkInputs build.image

/-- The directly proved LCB emission edge retains the exact site table and refines BIR
against the imported source semantics and the runtime projection. -/
def LCBAdapterContract : EdgeContract (Graph LCB.emit) where
  kind := .directProof
  inputInvariant := BIR.WF
  outputInvariant := fun (_, sites) => ∀ entry ∈ sites, entry.loc.wf = true
  correspondence := fun bir (f, sites) =>
    sites = bir.sites ∧ Refines birSemantics lcbSemantics bir (f, sites, (Runtime.qbe sites).toUTF8)

/-- Profile validation leaves the source function and site table intact on success. -/
def LCBProfileGraph (before after : Source.Function × BIR.SiteTable) : Prop :=
  validateJob (LCB.job before.1) = .ok () ∧ after = before

/-- The job validator establishes profile acceptance for the same value consumed by native lowering. -/
def LCBProfileContract : EdgeContract LCBProfileGraph where
  kind := .provedValidator
  inputInvariant := fun (_, sites) => ∀ entry ∈ sites, entry.loc.wf = true
  outputInvariant := fun (f, sites) =>
    validateJob (LCB.job f) = .ok () ∧ ∀ entry ∈ sites, entry.loc.wf = true
  correspondence := Eq

/-- The adapter proof and job validator discharge the production preparation obligations
independently of the residual LCB native premises. -/
def LCBAdapterSound : Prop :=
  LCBAdapterContract.sound ∧ LCBProfileContract.sound ∧
  Compatible LCBAdapterContract LCBProfileContract

/-- The admission-independent dimensional limit applies to BIR before the driver emits a backend
artifact. It is separate from the stack adequacy obligation for its linked image. -/
def BIRSlotsFit (profile : Profile) (bir : BIR.Program) : Prop :=
  bir.frameSlots ≤ profile.maxBIRSlots

/-- The permitted image behaviors are anchored to the spec evaluator on the exact,
path-indexed Core. Environment premises control progress through short writes and exit. -/
def ImageCorrect {Env : Type} {profile : Profile} {platform : TargetReference Env profile.target} (n : NativeModels Env profile platform)
    (source : Semantics (String × Elab.Output)) (path : String) (core : Elab.Output) (image : ByteArray) : Prop :=
  (∀ env, LaunchReady n image env →
    PrefixSafety source n.process (path, core) (image, env)) ∧
  (∀ env, RunReady n image env →
    MaximalRefinement source n.process (path, core) (image, env)) ∧
  (SourceTerminates path core source → ∀ env, RunReady n image env →
    ∃ o, n.process.observes (image, env) o)

def Compiled (input : Stages.Input) (out : Stages.Output) : Prop :=
  SharedHandoffs input out ∧ Frontend input out ∧ BIR.WF out.bir ∧
  SiteOrigins input.path out.core out.memory out.bir ∧
  Refines admittedCoreSemantics birSemantics (input.path, out.core) out.bir ∧
  SourceTerminates input.path out.core admittedCoreSemantics

def SharedCorrect : Prop :=
  ∀ input out, Stages.compile input = .ok out → Compiled input out

def LLVMCorrect {Env : Type} {profile : Profile}
    {platform : TargetReference Env profile.target} (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  ∀ input out build, Stages.compile input = .ok out → BIRSlotsFit profile out.bir →
    LLVMHandoffs profile tools out.bir build → LLVMPremises n out.bir build →
    Compiled input out ∧ ImageCorrect n admittedCoreSemantics input.path out.core build.image

def LCBCorrect {Env : Type} {profile : Profile}
    {platform : TargetReference Env profile.target} (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  ∀ input out build, Stages.compile input = .ok out → BIRSlotsFit profile out.bir →
    LCBHandoffs profile tools out.bir build → LCBPremises n build →
    Compiled input out ∧ ImageCorrect n admittedCoreSemantics input.path out.core build.image

def UncheckedHandoffs (input : Stages.Input) (core : Elab.Output) : Prop :=
  ∃ tokens ast, Lex.lex input.source = .ok tokens ∧ Parse.parse tokens = .ok ast ∧
    Elab.elaborate ast = .ok core

def UncheckedFrontend (input : Stages.Input) (core : Elab.Output) : Prop :=
  ∃ tokens ast, Lex.lex input.source = .ok tokens ∧ Parse.parse tokens = .ok ast ∧
    Elab.elaborate ast = .ok core ∧ Lexes input.source tokens ∧ Derives tokens ast ∧
    Elaborates ast core

def UncheckedOrigins (input : Stages.Input) (core : Elab.Output) (bir : BIR.Program) : Prop :=
  ∃ mir, MIR.lowerUnchecked core = .ok mir ∧
    SSA.lower input.path (Memory.lower mir) = .ok bir ∧
    SiteOrigins input.path core (Memory.lower mir) bir

def ExactTrap {Env : Type} {profile : Profile} {platform : TargetReference Env profile.target} (n : NativeModels Env profile platform)
    (input : Stages.Input) (core : Elab.Output) (image : ByteArray) : Prop :=
  ∀ status kind site stderr, coreSemantics.observes (input.path, core)
    (.trapped status kind site stderr) → ∀ env, RunReady n image env →
      n.process.observes (image, env) (.trapped status kind site stderr)

def UncheckedCompiled (input : Stages.Input) (core : Elab.Output) (bir : BIR.Program) : Prop :=
  UncheckedFrontend input core ∧ BIR.WF bir ∧ UncheckedOrigins input core bir ∧
  SourceTerminates input.path core ∧ Refines coreSemantics birSemantics (input.path, core) bir

def UncheckedSharedCorrect : Prop :=
  ∀ input core bir, UncheckedHandoffs input core → ConformanceStatic core →
    Stages.unchecked input = .ok bir → UncheckedCompiled input core bir

def UncheckedCorrect {Env : Type} {profile : Profile}
    {platform : TargetReference Env profile.target} (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  ∀ input core bir, UncheckedHandoffs input core → ConformanceStatic core →
    Stages.unchecked input = .ok bir → BIRSlotsFit profile bir →
    UncheckedCompiled input core bir ∧
    (∀ build, LLVMHandoffs profile tools bir build → LLVMPremises n bir build →
      ImageCorrect n coreSemantics input.path core build.image ∧ ExactTrap n input core build.image) ∧
    (∀ build, LCBHandoffs profile tools bir build → LCBPremises n build →
      ImageCorrect n coreSemantics input.path core build.image ∧ ExactTrap n input core build.image)

def LLVMNonvacuous {Env : Type} {profile : Profile}
    {platform : TargetReference Env profile.target} (policy : ExecutionPolicy Env) (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  policy.Anchors n ∧
  ∃ input out build env o,
    Stages.compile input = .ok out ∧ BIRSlotsFit profile out.bir ∧
    LLVMHandoffs profile tools out.bir build ∧ LLVMPremises n out.bir build ∧
    RunReady n build.image env ∧ SourceTerminates input.path out.core admittedCoreSemantics ∧
    admittedCoreSemantics.observes (input.path, out.core) o ∧ n.process.observes (build.image, env) o

def LCBNonvacuous {Env : Type} {profile : Profile}
    {platform : TargetReference Env profile.target} (policy : ExecutionPolicy Env) (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  policy.Anchors n ∧
  ∃ input out build env o,
    Stages.compile input = .ok out ∧ BIRSlotsFit profile out.bir ∧
    LCBHandoffs profile tools out.bir build ∧ LCBPremises n build ∧
    RunReady n build.image env ∧ SourceTerminates input.path out.core admittedCoreSemantics ∧
    admittedCoreSemantics.observes (input.path, out.core) o ∧ n.process.observes (build.image, env) o

def UncheckedNonvacuous {Env : Type} {profile : Profile} {Build : Type}
    {platform : TargetReference Env profile.target} (policy : ExecutionPolicy Env) (n : NativeModels Env profile platform) (image : Build → ByteArray)
    (premises : BIR.Program → Build → Prop) : Prop :=
  policy.Anchors n ∧
  ∃ input core bir build env status kind site stderr,
    UncheckedHandoffs input core ∧ ConformanceStatic core ∧
    Stages.unchecked input = .ok bir ∧ BIRSlotsFit profile bir ∧ premises bir build ∧
    RunReady n (image build) env ∧ SourceTerminates input.path core ∧
    coreSemantics.observes (input.path, core) (.trapped status kind site stderr) ∧
    n.process.observes (image build, env) (.trapped status kind site stderr)

def LLVMUncheckedNonvacuous {Env : Type} {profile : Profile}
    {platform : TargetReference Env profile.target} (policy : ExecutionPolicy Env) (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  UncheckedNonvacuous policy n LLVMBuild.image fun bir build =>
    LLVMHandoffs profile tools bir build ∧ LLVMPremises n bir build

def LCBUncheckedNonvacuous {Env : Type} {profile : Profile}
    {platform : TargetReference Env profile.target} (policy : ExecutionPolicy Env) (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  UncheckedNonvacuous policy n LCBBuild.image fun bir build =>
    LCBHandoffs profile tools bir build ∧ LCBPremises n build

/-- Qualification and correctness use the same reference, profile, tools and native models. -/
def NativeNonvacuous {Env : Type} {profile : Profile}
    {reference : TargetReference Env profile.target} (policy : ExecutionPolicy Env)
    (n : NativeModels Env profile reference) (tools : NativeTools) : Prop :=
  LLVMNonvacuous policy n tools ∧ LCBNonvacuous policy n tools ∧
    LLVMUncheckedNonvacuous policy n tools ∧ LCBUncheckedNonvacuous policy n tools

/-- Conditional composition contract for a specified platform reference. The
same reference indexes the policy and native models throughout the proof. -/
def CompileCorrectUnderReference {Env : Type} {profile : Profile} (m : SharedModels profile.target)
    (reference : TargetReference Env profile.target) (policy : ExecutionPolicy Env)
    (n : NativeModels Env profile reference) (tools : NativeTools) : Prop :=
  SharedSound m ∧ SharedCorrect ∧ UncheckedSharedCorrect ∧ LCBAdapterSound ∧
    NativeNonvacuous policy n tools ∧ LLVMCorrect n tools ∧ LCBCorrect n tools ∧
    UncheckedCorrect n tools

private def CompileCorrectWith {Env : Type} {profile : Profile}
    (registry : NativeReferenceRegistry) (m : SharedModels profile.target)
    (reference : TargetReference Env profile.target) (policy : ExecutionPolicy Env)
    (n : NativeModels Env profile reference) (tools : NativeTools) : Prop :=
  registry profile.target = some ⟨Env, reference⟩ ∧
    CompileCorrectUnderReference m reference policy n tools

/-- A production proof must establish the composition obligations and match the
verifier's registered reference. Registration is part of the claim. -/
def compile_correct {Env : Type} {profile : Profile} (m : SharedModels profile.target)
    (reference : TargetReference Env profile.target) (policy : ExecutionPolicy Env)
    (n : NativeModels Env profile reference) (tools : NativeTools) : Prop :=
  CompileCorrectWith nativeReference m reference policy n tools

theorem compile_correct_requires_registration {Env : Type} {profile : Profile}
    {m : SharedModels profile.target} {reference : TargetReference Env profile.target}
    {policy : ExecutionPolicy Env} {n : NativeModels Env profile reference} {tools : NativeTools}
    (missing : nativeReference profile.target = none) :
    ¬ compile_correct m reference policy n tools := by
  intro correct
  have registered := correct.1
  rw [missing] at registered
  cases registered

namespace Sanity

def answerInput : Stages.Input :=
  ⟨"tests/slice0/answer.con", r#"fn main() -> u8 { return 20 + 22; }"#.toUTF8⟩

private def bindingSpan : Span := ⟨0, 1, 1, 1⟩

private def parseBinderExample (source : String) : Except Refusal Program := do
  Parse.parse (← Lex.lex source.toUTF8)

private theorem refusesFirstBinder {ast : Program} {bindings : List Binding} {binding : Binding}
    {result : Expr} (shape : checkBody ast.body = .ok (binding :: bindings, result))
    (illegal : ¬ BinderLegal [] binding.name) (core : Elab.Output) : ¬ Elaborates ast core := by
  intro h
  obtain ⟨_, _, _, _, _, _, _, _, _, checked, lowered, _⟩ := h
  cases Except.ok.inj (shape.symm.trans checked)
  exact illegal lowered.headLegal

private def wildcardParse :=
  parseBinderExample r#"fn main() -> u8 { let _: u8 = 42; return _; }"#

private def mainBinderParse :=
  parseBinderExample r#"fn main() -> u8 { let main: u8 = 0; return main; }"#

private def intrinsicBinderParse :=
  parseBinderExample r#"fn main() -> u8 { let discard: u8 = 42; return discard; }"#

private def builtinTypeBinderParse :=
  parseBinderExample r#"fn main() -> u8 { let i8: u8 = 42; return i8; }"#

private def duplicateBinderParse :=
  parseBinderExample r#"fn main() -> u8 { let x: u8 = 1; let x: u8 = 20 + 22; return x; }"#

example : ∃ ast, wildcardParse = .ok ast ∧
    Elab.elaborate ast = .error (.unsupported "wildcard binding at byte 22") ∧
    ∀ core, ¬ Elaborates ast core := by
  refine ⟨_, rfl, rfl, ?_⟩
  exact refusesFirstBinder rfl (by decide)

example : ∃ ast, mainBinderParse = .ok ast ∧
    Elab.elaborate ast = .error (.checker "binding shadows 'main' at byte 22") ∧
    ∀ core, ¬ Elaborates ast core := by
  refine ⟨_, rfl, rfl, ?_⟩
  exact refusesFirstBinder rfl (by decide)

example : ∃ ast, intrinsicBinderParse = .ok ast ∧
    Elab.elaborate ast = .error (.checker "binding shadows 'discard' at byte 22") ∧
    ∀ core, ¬ Elaborates ast core := by
  refine ⟨_, rfl, rfl, ?_⟩
  exact refusesFirstBinder rfl (by decide)

example : ∃ ast, builtinTypeBinderParse = .ok ast ∧
    Elab.elaborate ast = .error (.checker "binding shadows 'i8' at byte 22") ∧
    ∀ core, ¬ Elaborates ast core := by
  refine ⟨_, rfl, rfl, ?_⟩
  exact refusesFirstBinder rfl (by decide)

example : ∃ ast, duplicateBinderParse = .ok ast ∧
    Elab.elaborate ast = .error (.checker "binding shadows 'x' at byte 37") ∧
    ∀ core, ¬ Elaborates ast core := by
  refine ⟨_, rfl, rfl, ?_⟩
  intro core h
  obtain ⟨_, _, _, _, _, _, _, _, _, checked, lowered, _⟩ := h
  have shape : ∃ first second result,
      checkBody (duplicateBinderParse.toOption.getD ⟨"", bindingSpan, []⟩).body =
        .ok ([first, second], result) ∧ first.name = "x" ∧ second.name = "x" :=
    ⟨_, _, _, rfl, rfl, rfl⟩
  obtain ⟨first, second, _, shape, firstName, secondName⟩ := shape
  cases Except.ok.inj (shape.symm.trans checked)
  have member : second.name ∈ [first.name] := by rw [firstName, secondName]; exact List.Mem.head _
  cases lowered with
  | atom _ _ rest => exact rest.headLegal.2.2.2.2 member
  | operation _ _ _ rest => exact rest.headLegal.2.2.2.2 member

example :
    ElaboratesBindings [] [⟨"x", bindingSpan, .integer 1 none bindingSpan⟩]
      [("x", ⟨0⟩)] [⟨⟨0⟩, .atom (.lit 1)⟩] [] :=
  .atom (by decide) (.integer 1 _) (.nil _)

def exit42 : Observation := .returned 42 ByteArray.empty

/-- An exact terminal observation supplies a safe prefix and admits its identity extension. -/
theorem returnAtTwo {A : Type} {evaluate : A → Nat → Evaluation}
    {a : A} {o : Observation} (h0 : evaluate a 0 = .running) (h1 : evaluate a 1 = .running)
    (h2 : evaluate a 2 = .finished o) : (evaluatorSemantics evaluate).observes a o :=
  ⟨(2 : Nat), ⟨(0 : Nat), rfl, .cons ⟨h0, rfl⟩ (.cons ⟨h1, rfl⟩ (.nil (2 : Nat)))⟩, h2⟩

private theorem returnAtOne {A : Type} {evaluate : A → Nat → Evaluation}
    {a : A} {o : Observation} (h0 : evaluate a 0 = .running)
    (h1 : evaluate a 1 = .finished o) : (evaluatorSemantics evaluate).observes a o :=
  ⟨(1 : Nat), ⟨(0 : Nat), rfl, .cons ⟨h0, rfl⟩ (.nil (1 : Nat))⟩, h1⟩

example : ∃ out, Stages.compile answerInput = .ok out ∧
    coreSemantics.observes (answerInput.path, out.core) exit42 := by
  with_unfolding_all
    refine ⟨_, rfl, ?_⟩
    exact returnAtTwo rfl rfl rfl

example : ¬ ConformanceStatic ⟨⟨[], .lit 256⟩, []⟩ := by
  with_unfolding_all
    change ¬ (256 < 256)
    decide

private def overflowInput (path : String) (comment : String) : Stages.Input :=
  ⟨path, (comment ++ "\n" ++ r#"fn main() -> u8 { return 200 + 56; }"#).toUTF8⟩

private def overflowObservation (path : String) : Observation :=
  .trapped 134 .overflow ⟨⟨0⟩, ⟨path, 2, 30⟩⟩
    (s!"concrete: trap: overflow at {path}:2:30 (site 0)\n".toUTF8)

example : ∃ core bir,
    UncheckedHandoffs (overflowInput "src/a.con" "// x") core ∧ ConformanceStatic core ∧
    Stages.unchecked (overflowInput "src/a.con" "// x") = .ok bir ∧
    coreSemantics.observes ("src/a.con", core) (overflowObservation "src/a.con") ∧
    coreSemantics.observes ("src/b.con", core) (overflowObservation "src/b.con") := by
  with_unfolding_all
    refine ⟨_, _, ⟨_, _, rfl, rfl, rfl⟩, ?_, rfl, ?_, ?_⟩
    · exact ⟨⟨by change 200 < 256; decide, by change 56 < 256; decide⟩, List.Mem.head _⟩
    · exact returnAtOne rfl rfl
    · exact returnAtOne rfl rfl

/-- Changing the comment changes the origin byte offsets. Both LLVM artifacts, and the
process observations projected from the two BIR runs, still agree. -/
example : ∃ a b,
    Stages.unchecked (overflowInput "src/a.con" "// x") = .ok a ∧
    Stages.unchecked (overflowInput "src/a.con" "// longer") = .ok b ∧
    a.sites ≠ b.sites ∧ LLVM.module a = LLVM.module b ∧
    birObservation a.sites (BIR.run a 2) = some (overflowObservation "src/a.con") ∧
    birObservation b.sites (BIR.run b 2) = some (overflowObservation "src/a.con") := by
  with_unfolding_all
    refine ⟨_, _, rfl, rfl, ?_, rfl, rfl, rfl⟩
    decide

example : (Observation.returned 134 ByteArray.empty) ≠ overflowObservation "src/a.con" := by
  with_unfolding_all
    intro h
    cases h

example : ¬ SameInfiniteTrace (E := Event) (fun _ => []) (fun _ => [ProcessEvent.stderr 65]) := by
  with_unfolding_all
    intro same
    obtain ⟨_, suffix, impossible⟩ := (same [ProcessEvent.stderr 65]).mpr ⟨0, [], rfl⟩
    cases impossible

private def samplePin : ToolInvocation :=
  ⟨.llvmObject, "test-clang", "pinned", ["-c", "llvm.ll", "-o", "llvm.o"]⟩

private def runtimePin : ToolInvocation :=
  ⟨.runtimeLowering, "test-qbe", "pinned", ["-o", "rt.s", "rt.ssa"]⟩

private def bodyAssemblerPin : ToolInvocation :=
  ⟨.bodyAssembler, "test-as", "pinned", ["-c", "body.s", "-o", "body.o"]⟩

private def runtimeAssemblerPin : ToolInvocation :=
  ⟨.runtimeAssembler, "test-as", "pinned", ["-c", "rt.s", "-o", "rt.o"]⟩

private def linkerPin : ToolInvocation :=
  ⟨.linker, "test-ld", "pinned", ["-o", "image"]⟩

def sampleProfile : Profile :=
  ⟨.aarch64Macos, 64, 4096, [samplePin, runtimePin, bodyAssemblerPin, runtimeAssemblerPin, linkerPin]⟩

example : sampleProfile.invocation .llvmObject samplePin := by rfl

example : ¬ sampleProfile.invocation .llvmObject { samplePin with arguments := ["-S"] } := by
  unfold Profile.invocation
  decide

def testTools : NativeTools where
  llvmObject p _ _ := p = samplePin
  runtimeLowering p input output := p = runtimePin ∧ output = input
  assemble p _ output := p = bodyAssemblerPin ∨
    (p = runtimeAssemblerPin ∧ output = ⟨#[4]⟩)
  link p inputs resolved output := p = linkerPin ∧ inputs = resolved ∧ inputs.head? = some output

def instant {A : Type} (observe : A → Observation) : Semantics A where
  State := Unit
  initial _ _ := True
  initialized _ := ⟨(), trivial⟩
  step _ _ _ := False
  result a _ o := observe a = o
  terminal _ _ _ _ _ h := h
  safe _ _ := True
  trace a _ := (observe a).events
  permitted _ := runtimePermitted
  resultTrace _ _ _ h := h ▸ Observation.stderr_events _
  traceGrows _ _ _ h := h.elim

theorem instantObserves {A : Type} (f : A → Observation) (a : A) :
    (instant f).observes a (f a) :=
  ⟨(), ⟨(), trivial, .nil ()⟩, rfl⟩

theorem refinementsToInstant {A B : Type} (source : Semantics A) (a : A)
    (f : B → Observation) (b : B)
    (permit : ∀ e ∈ (f b).events, source.permitted a e) (h : ∃ q, source.finite a q ∧ source.safe a q ∧
      source.result a q (f b) ∧ source.trace a q = (f b).events) :
    Refines source (instant f) a b := by
  with_unfolding_all
    obtain ⟨q, hr, hs, ho, ht⟩ := h
    refine ⟨?_, ?_, ?_, ?_⟩
    · intro _ _
      exact ⟨trivial, fun e he => ⟨permit e he, runtimePermitted_all e⟩, q, hr, hs, [], by simpa [instant] using ht.symm⟩
    · intro _ o _ e
      exact ⟨q, hr, e ▸ ho, ht⟩
    · intro run r
      exact (r.2 0).elim
    · intro _ stuck
      exact stuck.2.2 (f b) rfl

theorem instantRefines {A B : Type} (f : A → Observation) (g : B → Observation)
    (a : A) (b : B) (h : f a = g b) : Refines (instant f) (instant g) a b := by
  with_unfolding_all
    apply refinementsToInstant
    · exact fun e _ => runtimePermitted_all e
    · exact ⟨(), ⟨(), trivial, .nil ()⟩, trivial, h, congrArg Observation.events h⟩

def bytecodeObservation (bytes : ByteArray) : Observation :=
  .returned (bytes.data[0]?.getD 0) ByteArray.empty

def answerCore : Elab.Output :=
  (Stages.compile answerInput).toOption.map (·.core) |>.getD ⟨⟨[], .lit 0⟩, []⟩

theorem answerImageRefines : Refines admittedCoreSemantics (instant bytecodeObservation)
    (answerInput.path, answerCore) ⟨#[42]⟩ := by
  with_unfolding_all
    have obs : admittedCoreSemantics.observes (answerInput.path, answerCore) exit42 :=
      returnAtTwo rfl rfl rfl
    obtain ⟨q, reach, terminal⟩ := obs
    apply refinementsToInstant
    · exact fun e _ => runtimePermitted_all e
    · exact ⟨q, reach, (fun h => by rw [terminal] at h; cases h), terminal,
      show (admittedEvaluation _ q).events = _ by rw [terminal]; rfl⟩

theorem answerDoesNotReturn43 : ¬ admittedCoreSemantics.observes
    (answerInput.path, answerCore) (.returned 43 ByteArray.empty) := by
  with_unfolding_all
    intro bad
    obtain ⟨q, _, terminal⟩ := bad
    change Nat at q
    change admittedEvaluation (answerInput.path, answerCore) q = .finished
      (bytecodeObservation ⟨#[43]⟩) at terminal
    cases q with
    | zero => exact Evaluation.noConfusion (show Evaluation.running = _ from terminal)
    | succ q =>
      cases q with
      | zero => exact Evaluation.noConfusion (show Evaluation.running = _ from terminal)
      | succ q =>
        have conflict : exit42 = bytecodeObservation ⟨#[43]⟩ :=
          Evaluation.finished.inj (show Evaluation.finished exit42 = _ from terminal)
        have h : UInt8.ofNat 42 = UInt8.ofNat 43 := (Observation.returned.inj conflict).1
        exact (by decide : UInt8.ofNat 42 ≠ UInt8.ofNat 43) h

/-- A test platform with identity loading and immediate, byte-indexed exits. -/
def testPlatform (target : LCB.Target) (observe : ByteArray → Observation) :
    TargetReference Unit target where
  process := instant fun (bytes, _) => observe bytes
  Image := ByteArray
  decode := some
  machine := instant fun (bytes, _) => observe (bytes.getD ByteArray.empty)

def answerBIR : BIR.Program :=
  ⟨[⟨[], [.bin .add .u8 0 (.imm .u8 20) (.imm .u8 22)], .ret (.reg 0)⟩], []⟩

def answerImage : ByteArray := ⟨#[42]⟩

private def trapImage : ByteArray := ⟨#[134]⟩

private def trapBytecodeObservation (bytes : ByteArray) : Observation :=
  if bytes.data[0]? == some (134 : UInt8) then overflowObservation "src/a.con"
  else bytecodeObservation bytes

def testReference : TargetReference Unit sampleProfile.target :=
  testPlatform sampleProfile.target trapBytecodeObservation

/-- Regression tests select their reference by target just as production does. -/
def testRegistry : NativeReferenceRegistry :=
  fun target =>
    if h : target = sampleProfile.target then some ⟨Unit, h.symm ▸ testReference⟩ else none

/-- The fixture routes the two emitted payloads by a byte where they differ. The
proofs below check those bytes against the actual printer and runtime inputs. -/
def testNative : NativeModels Unit sampleProfile testReference where
  llvmModule := instant fun bytes =>
    trapBytecodeObservation (if bytes.data[0]? = some 10 then answerImage else trapImage)
  llvmWellFormed _ := True
  lcbBodyAssembly := instant fun (_, runtime) =>
    trapBytecodeObservation (if runtime.data[6]? = some 98 then answerImage else trapImage)
  lcbAssembly := instant fun (_, runtime) =>
    trapBytecodeObservation (if runtime.data[6]? = some 98 then answerImage else trapImage)
  objectOperands := instant fun inputs => trapBytecodeObservation (inputs.headD ByteArray.empty)
  objects := instant fun inputs => trapBytecodeObservation (inputs.headD ByteArray.empty)
  linked := instant trapBytecodeObservation
  loaded := instant fun (bytes, _) => trapBytecodeObservation bytes
  runtime := instant fun (bytes, _) => trapBytecodeObservation bytes
  stackAdequate _ _ _ := True
  launchAdmitted _ _ := True
  osLive _ _ := True
  stderrAndExitAvailable _ _ := True

def testExecutionPolicy : ExecutionPolicy Unit where
  stackAdequate _ _ _ _ := True
  launchAdmitted _ _ _ := True
  osLive _ _ _ := True
  stderrAndExitAvailable _ _ _ := True

theorem testAnchor :
    testExecutionPolicy.Anchors testNative :=
  ⟨rfl, rfl, rfl, rfl⟩

private theorem testImagePremises (image : ByteArray) (rest : List ByteArray) :
    ImagePremises testNative (image :: rest) image where
  linker := instantRefines _ _ _ _ rfl
  decoder env _ := ⟨⟨image, rfl⟩, (instantRefines _ _ (image, env) (image, env) rfl).1⟩
  decodedExecution env _ := (instantRefines _ _ (image, env) (image, env) rfl).2
  decodedLiveness _ _ h := h
  loader env _ _ := instantRefines _ _ image (image, env) rfl
  runtimeSafety env _ := (instantRefines _ _ (image, env) (image, env) rfl).1
  trapRenderingSafety env _ := (instantRefines _ _ (image, env) (image, env) rfl).1
  runtime env _ := (instantRefines _ _ (image, env) (image, env) rfl).2
  trapRendering env _ := (instantRefines _ _ (image, env) (image, env) rfl).2
  liveness env _ _ _ := ⟨_, instantObserves _ (image, env)⟩

def testLLVMBuild (bir : BIR.Program) (image : ByteArray) : LLVMBuild :=
  ⟨(LLVM.module bir).toUTF8, image, [image], image⟩

private theorem testLLVMPremises (bir : BIR.Program) (image : ByteArray)
    (route : trapBytecodeObservation
      (if (LLVM.module bir).toUTF8.data[0]? = some 10 then answerImage else trapImage) =
      trapBytecodeObservation image)
    (h : birSemantics.observes bir (trapBytecodeObservation image)) :
    LLVMPremises testNative bir (testLLVMBuild bir image) where
  printer := by
    obtain ⟨q, reach, terminal⟩ := h
    exact ⟨trivial, refinementsToInstant _ _ _ _ (fun e _ => runtimePermitted_all e)
      ⟨q, reach, (fun e => by rw [terminal] at e; cases e), terminal.trans (congrArg Evaluation.finished route.symm),
        show (birEvaluation bir q).events = _ by rw [terminal, ← route]; rfl⟩⟩
  toolchain _ := instantRefines _ _ _ _ route
  resolveLinkInputs := instantRefines _ _ _ _ rfl
  image := testImagePremises _ _

private theorem lcbSteps {i : LCBInput} {s t : Source.State} {fuel : Nat}
    (h : Source.Steps i.1 fuel s t) : Steps (lcbSemantics.step i) (some s) (some t) := by
  with_unfolding_all
    induction h with
    | zero _ => exact .nil _
    | succ h _ ih => exact .cons ⟨_, _, rfl, rfl, h⟩ ih

private theorem lcbToInstant {i : LCBInput} {value : Source.Value}
    (ret : Source.HasBehavior i.1 #[] (.returned value)) {B : Type}
    {f : B → Observation} {b : B} (project : lcbResult i.2.1 value = some (f b)) :
    Refines lcbSemantics (instant f) i b := by
  with_unfolding_all
    obtain ⟨s, init, _, q, steps, terminal⟩ := ret
    have result : lcbTerminal i (some q) = some (f b) := by simpa [lcbTerminal, terminal]
    apply refinementsToInstant
    · exact fun e _ => runtimePermitted_all e
    · exact ⟨some q, ⟨some s, by simp [lcbSemantics, init]; rfl, lcbSteps steps⟩,
      Or.inr (by rw [result]; rfl), result, by simp [lcbSemantics, result]⟩

def answerOutput : Stages.Output :=
  match Stages.compile answerInput with
  | .ok out => out
  | .error _ => ⟨[], ⟨"", ⟨0, 0, 0, 0⟩, []⟩, answerCore, [], ⟨[], .lit 0⟩,
    Memory.lower ⟨[], .lit 0⟩, answerBIR⟩

theorem answerCompiles : Stages.compile answerInput = .ok answerOutput := by
  with_unfolding_all rfl

theorem answerFrontend : Frontend answerInput answerOutput := by
  with_unfolding_all
    refine ⟨Stages.compile_lexes answerCompiles, ?_, ?_, ?_⟩
    · exact .main _ _ _ _ _ _ _ _
        (.ret _ _ (.expr (.integer 20 none _) (.more (.integer 22 none _) (.nil _))) .nil)
    · refine ⟨rfl, by decide, [], _, [], [], _, [], _, rfl, .nil [], ?_, rfl, rfl⟩
      exact .add (.integer 20 _) (.integer 22 _)
    · have admitted : ∃ a, Admission.check answerOutput.core = .ok a := ⟨_, rfl⟩
      obtain ⟨a, h⟩ := admitted
      obtain ⟨same, tokens⟩ := Admission.check_evidence h
      have checked := Admission.admit_checked h
      rw [same] at checked
      exact ⟨a, h, same, tokens.trans rfl, checked⟩

theorem answerHandoffs : SharedHandoffs answerInput answerOutput := by
  with_unfolding_all
    exact ⟨rfl, rfl, rfl, ⟨_, rfl, rfl, rfl⟩, rfl, rfl⟩

theorem answerOrigins : SiteOrigins answerInput.path answerOutput.core answerOutput.memory
    answerOutput.bir := by
  with_unfolding_all
    exact ⟨List.nil_sublist _, rfl⟩

private def declaredRevision := "4bea4b3ffd8a43a73a0016012d865044067b3275"

theorem pinnedRevision : pinnedLCBRevision = some declaredRevision := by
  with_unfolding_all rfl

private def testLCBBuild (bir : BIR.Program) (image : ByteArray) (assembly : String) : LCBBuild :=
  let (body, sites) := (LCB.emit bir).toOption.getD (⟨.word, #[], #[], #[]⟩, [])
  ⟨declaredRevision, compilerBuildIdentity, body, sites, (Runtime.qbe sites).toUTF8,
    assembly.toUTF8, (Runtime.qbe sites).toUTF8, image, ⟨#[4]⟩, [image, ⟨#[4]⟩], image⟩

private theorem testLCBPremises (bir : BIR.Program) (image : ByteArray) (assembly : String)
    (route : trapBytecodeObservation
      (if (testLCBBuild bir image assembly).runtimeInput.data[6]? = some 98
        then answerImage else trapImage) = trapBytecodeObservation image)
    (h : Refines lcbSemantics testNative.lcbBodyAssembly
      ((testLCBBuild bir image assembly).source, (testLCBBuild bir image assembly).sites,
        (testLCBBuild bir image assembly).runtimeInput)
      ((testLCBBuild bir image assembly).bodyAssembly, (testLCBBuild bir image assembly).runtimeInput)) :
    LCBPremises testNative (testLCBBuild bir image assembly) where
  nativeLowering _ _ := h
  compatibilityLowering _ _ := h
  runtimeLowering := instantRefines _ _ _ _ rfl
  assembler := instantRefines _ _ _ _ route
  resolveLinkInputs := instantRefines _ _ _ _ rfl
  image := testImagePremises _ _

theorem testLLVMHandoffs (bir : BIR.Program) (image : ByteArray) :
    LLVMHandoffs sampleProfile testTools bir (testLLVMBuild bir image) :=
  ⟨rfl, ⟨samplePin, rfl, rfl⟩, ⟨linkerPin, rfl, rfl, rfl, rfl⟩⟩

private theorem assembleMacOS {f : Source.Function} {stack : StackMIR.Function}
    {asm : TypedA64.Function} (jobOK : validateJob (LCB.job f) = .ok ())
    (lower : StackMIR.lower f (planFrame f) = .ok stack)
    (stackOK : StackMIR.validate stack = .ok ())
    (select : TypedA64.select stack = .ok asm)
    (asmOK : TypedA64.validate asm = .ok ()) :
    ∃ text, LCB.assemble .aarch64Macos f = .ok text := by
  have render : ∃ text, TypedA64.render asm LCB.bodySymbol = .ok text := by
    have symbolOK : (!LCB.bodySymbol.name.isEmpty &&
        LCB.bodySymbol.name.toList.all (fun c => c.toNat < 128 && (c.isAlphanum || c == '_')) &&
        (LCB.bodySymbol.name.toList.head!.isAlpha || LCB.bodySymbol.name.toList.head! == '_')) = true := rfl
    have exported : LCB.bodySymbol.exported = true := rfl
    simp only [TypedA64.render, asmOK, bind, Except.bind, symbolOK, Bool.not_true,
      Bool.false_eq_true, ite_false, exported, ite_true, Array.forIn_pure_yield_eq_foldl]
    exact ⟨_, rfl⟩
  obtain ⟨text, render⟩ := render
  refine ⟨".text\n" ++ text ++ ".subsections_via_symbols\n", ?_⟩
  have requested : ((LCB.job f).requested != EvidenceRequest.none) = false := rfl
  have body : (LCB.job f).code.body = f := rfl
  have layout : (LCB.job f).code.layout = planFrame f := rfl
  have symbol : (LCB.job f).code.symbol = LCB.bodySymbol := rfl
  simp only [LCB.assemble, compileFunction, jobOK, bind, Except.bind, Except.mapError,
    requested, Bool.false_eq_true, ite_false, body, layout, lower, stackOK, select, asmOK, symbol, render]
  rfl


private theorem testLCBHandoffs (bir : BIR.Program) (image : ByteArray) (assembly : String)
    (emit : LCB.emit bir = .ok ((testLCBBuild bir image assembly).source, bir.sites))
    (sites : (testLCBBuild bir image assembly).sites = bir.sites)
    (job : validateJob (LCB.job (testLCBBuild bir image assembly).source) = .ok ())
    (assembled : LCB.assemble sampleProfile.target (testLCBBuild bir image assembly).source =
      .ok assembly) :
    LCBHandoffs sampleProfile testTools bir (testLCBBuild bir image assembly) := by
  exact ⟨pinnedRevision, rfl, sites.symm ▸ emit, job, Stages.prepareLCB_of_edges _ _ emit job, sites,
    rfl, ⟨assembly, assembled, rfl⟩, ⟨runtimePin, rfl, rfl, rfl⟩, ⟨bodyAssemblerPin, rfl, Or.inl rfl⟩,
    ⟨runtimeAssemblerPin, rfl, Or.inr ⟨rfl, rfl⟩⟩, ⟨linkerPin, rfl, rfl, rfl, rfl⟩⟩

private theorem coreTerminates (path : String) (core : Elab.Output) (o : Observation) (fuel : Nat)
    (h : coreEvaluation (path, core) fuel = .finished o)
    (obs : coreSemantics.observes (path, core) o) : SourceTerminates path core := by
  refine ⟨⟨o, obs⟩, fun run infinite => ?_⟩
  have running := evaluatorDiverges (evaluate := coreEvaluation) infinite fuel
  rw [h] at running
  cases running

theorem answerTerminates : SourceTerminates answerInput.path answerOutput.core admittedCoreSemantics := by
  with_unfolding_all
    refine ⟨⟨exit42, returnAtTwo rfl rfl rfl⟩, fun _ infinite => ?_⟩
    have running := evaluatorDiverges (evaluate := admittedEvaluation) infinite 2
    have terminal : admittedEvaluation (answerInput.path, answerOutput.core) 2 = .finished exit42 := rfl
    rw [terminal] at running
    cases running

private def trapInput := overflowInput "src/a.con" "// x"

private def trapCore : Elab.Output :=
  (do Elab.elaborate (← Parse.parse (← Lex.lex trapInput.source)) :
    Except Refusal Elab.Output).toOption.getD ⟨⟨[], .lit 0⟩, []⟩

private def trapBIR : BIR.Program :=
  ⟨[⟨[], [.binOvf .add .u8 0 1 (.imm .u8 200) (.imm .u8 56)], .condbr 1 1 [] 2 [.reg 0]⟩,
    ⟨[], [], .trap 0⟩, ⟨[(2, .int .u8)], [], .ret (.reg 2)⟩],
    [⟨.arithmeticOverflow, ⟨0⟩, ⟨34, 35⟩, ⟨"src/a.con", 2, 30⟩⟩]⟩

private theorem trapStatic : ConformanceStatic trapCore := by
  with_unfolding_all
    exact ⟨⟨by change 200 < 256; decide, by change 56 < 256; decide⟩, List.Mem.head _⟩

private theorem trapTerminates : SourceTerminates trapInput.path trapCore :=
  coreTerminates _ _ (overflowObservation trapInput.path) 1 rfl (returnAtOne rfl rfl)

private theorem answerBIRJob : validateJob (LCB.job (testLCBBuild answerBIR answerImage "").source) = .ok () := by
  with_unfolding_all rfl

private theorem answerBIRAssembly : ∃ text, LCB.assemble .aarch64Macos (testLCBBuild answerBIR answerImage "").source = .ok text := by
  let f := (testLCBBuild answerBIR answerImage "").source
  let stack := (StackMIR.lower f (planFrame f)).toOption.getD ⟨#[], .word, #[], planFrame f, #[]⟩
  let asm := (TypedA64.select stack).toOption.getD ⟨#[], .word, 0, #[]⟩
  apply assembleMacOS answerBIRJob (stack := stack) (asm := asm)
  · dsimp only [f, stack]
    with_unfolding_all rfl
  · dsimp only [f, stack]
    with_unfolding_all rfl
  · dsimp only [f, stack, asm]
    with_unfolding_all rfl
  · dsimp only [f, stack, asm]
    with_unfolding_all rfl

private theorem trapBIRJob : validateJob (LCB.job (testLCBBuild trapBIR trapImage "").source) = .ok () := by
  with_unfolding_all rfl

private theorem trapBIRAssembly : ∃ text, LCB.assemble .aarch64Macos (testLCBBuild trapBIR trapImage "").source = .ok text := by
  let f := (testLCBBuild trapBIR trapImage "").source
  let stack := (StackMIR.lower f (planFrame f)).toOption.getD ⟨#[], .word, #[], planFrame f, #[]⟩
  let asm := (TypedA64.select stack).toOption.getD ⟨#[], .word, 0, #[]⟩
  apply assembleMacOS trapBIRJob (stack := stack) (asm := asm)
  · dsimp only [f, stack]
    with_unfolding_all rfl
  · dsimp only [f, stack]
    with_unfolding_all rfl
  · dsimp only [f, stack, asm]
    with_unfolding_all rfl
  · dsimp only [f, stack, asm]
    with_unfolding_all rfl

private theorem firstLLVMByte (bir : BIR.Program) (first : String) (rest : List String)
    (pieces : ((LLVM.moduleItems bir).flatMap LLVM.Item.pieces).map LLVM.Piece.render = first :: rest)
    (nonempty : 0 < first.toUTF8.data.size) :
    (LLVM.module bir).toUTF8.data[0]? = first.toUTF8.data[0]? := by
  unfold LLVM.module LLVM.render LLVM.renderPieces
  rw [pieces, String.join_cons]
  change (first.toUTF8 ++ (String.join rest).toUTF8).data[0]? = _
  rw [ByteArray.data_append, Array.getElem?_append_left nonempty]

private theorem testLLVMAnswerMarker :
    (LLVM.module answerBIR).toUTF8.data[0]? = some 10 :=
  firstLLVMByte answerBIR "\n" _ rfl (by decide)

private theorem testLLVMTrapMarker :
    (LLVM.module trapBIR).toUTF8.data[0]? ≠ some 10 := by
  have byte : (LLVM.module trapBIR).toUTF8.data[0]? = some 100 :=
    firstLLVMByte trapBIR "declare " _ rfl (by decide)
  exact fun h => (by decide : (some 100 : Option UInt8) ≠ some 10) (byte.symm.trans h)

private theorem testLCBAnswerMarker : (Runtime.qbe []).toUTF8.data[6]? = some 98 := rfl

private theorem testLCBTrapMarker :
    (Runtime.qbe trapBIR.sites).toUTF8.data[6]? ≠ some 98 := by
  have byte : (Runtime.qbe trapBIR.sites).toUTF8.data[6]? = some 115 := rfl
  exact fun h => (by decide : (some 115 : Option UInt8) ≠ some 98) (byte.symm.trans h)

private theorem llvmWitness : LLVMNonvacuous testExecutionPolicy testNative
    testTools := by
  with_unfolding_all
    refine ⟨testAnchor, ?_⟩
    exact ⟨answerInput, answerOutput, testLLVMBuild answerBIR answerImage, (), exit42,
      answerCompiles, by unfold BIRSlotsFit; decide, testLLVMHandoffs _ _,
      testLLVMPremises answerBIR answerImage (by
        exact congrArg trapBytecodeObservation (ite_eq_left testLLVMAnswerMarker))
        (returnAtOne rfl rfl),
      ⟨⟨trivial, trivial⟩, trivial, trivial⟩, answerTerminates, returnAtTwo rfl rfl rfl, instantObserves _ _⟩

private theorem lcbWitness : LCBNonvacuous testExecutionPolicy testNative
    testTools := by
  with_unfolding_all
    refine ⟨testAnchor, ?_⟩
    obtain ⟨assembly, assembled⟩ := answerBIRAssembly
    have jobOK := answerBIRJob
    let build := testLCBBuild answerBIR answerImage assembly
    have emit : LCB.emit answerBIR = .ok (build.source, build.sites) := rfl
    have adapter := (LCB.Proof.adapter_correct_run (p := answerBIR) rfl (LCB.Proof.emit_ok emit).1).1
    refine ⟨answerInput, answerOutput, build, (), exit42, answerCompiles, by unfold BIRSlotsFit; decide,
      testLCBHandoffs _ _ assembly rfl rfl jobOK assembled,
      testLCBPremises answerBIR answerImage assembly (by
        exact congrArg trapBytecodeObservation (ite_eq_left testLCBAnswerMarker))
        (lcbToInstant (adapter 1 42 rfl) (by
          change lcbResult build.sites (LCB.word 42) = some (trapBytecodeObservation
            (if build.runtimeInput.data[6]? = some 98 then answerImage else trapImage))
          exact (show lcbResult build.sites (LCB.word 42) = some (trapBytecodeObservation answerImage)
            from rfl).trans (congrArg (some ∘ trapBytecodeObservation) (ite_eq_left testLCBAnswerMarker).symm))),
      ⟨⟨trivial, trivial⟩, trivial, trivial⟩, answerTerminates, returnAtTwo rfl rfl rfl, instantObserves _ _⟩

private theorem llvmUncheckedWitness : LLVMUncheckedNonvacuous testExecutionPolicy testNative
    testTools := by
  with_unfolding_all
    refine ⟨testAnchor, ?_⟩
    exact ⟨trapInput, trapCore, trapBIR, testLLVMBuild trapBIR trapImage, (), 134, .overflow,
      ⟨⟨0⟩, ⟨"src/a.con", 2, 30⟩⟩, (overflowObservation "src/a.con").stderr,
      ⟨_, _, rfl, rfl, rfl⟩, trapStatic, rfl, by unfold BIRSlotsFit; decide,
      ⟨testLLVMHandoffs _ _, testLLVMPremises trapBIR trapImage (by rw [ite_eq_right testLLVMTrapMarker]) (returnAtTwo rfl rfl rfl)⟩,
      ⟨⟨trivial, trivial⟩, trivial, trivial⟩, trapTerminates, returnAtOne rfl rfl, instantObserves _ _⟩

private theorem lcbUncheckedWitness : LCBUncheckedNonvacuous testExecutionPolicy testNative
    testTools := by
  with_unfolding_all
    refine ⟨testAnchor, ?_⟩
    obtain ⟨assembly, assembled⟩ := trapBIRAssembly
    have jobOK := trapBIRJob
    let build := testLCBBuild trapBIR trapImage assembly
    have emit : LCB.emit trapBIR = .ok (build.source, build.sites) := rfl
    have adapter := (LCB.Proof.adapter_correct_run (p := trapBIR) rfl (LCB.Proof.emit_ok emit).1).2.1
    have hRun : BIR.run trapBIR 2 = .trapped 0 := rfl
    have projection : lcbResult build.sites (LCB.word 256) =
        some (trapBytecodeObservation trapImage) := rfl
    exact ⟨trapInput, trapCore, trapBIR, build, (), 134, .overflow,
      ⟨⟨0⟩, ⟨"src/a.con", 2, 30⟩⟩, (overflowObservation "src/a.con").stderr,
      ⟨_, _, rfl, rfl, rfl⟩, trapStatic, rfl, by unfold BIRSlotsFit; decide,
      ⟨testLCBHandoffs _ _ assembly rfl rfl jobOK assembled,
        testLCBPremises trapBIR trapImage assembly (by
          rw [ite_eq_right (show build.runtimeInput.data[6]? ≠ _ from testLCBTrapMarker)])
          (lcbToInstant (i := (build.source, build.sites, build.runtimeInput))
          (adapter 2 0 hRun) (by
            change lcbResult build.sites (LCB.word 256) = some (trapBytecodeObservation
              (if build.runtimeInput.data[6]? = some 98 then answerImage else trapImage))
            rw [ite_eq_right (show build.runtimeInput.data[6]? ≠ _ from testLCBTrapMarker)]
            exact projection))⟩,
      ⟨⟨trivial, trivial⟩, trivial, trivial⟩, trapTerminates, returnAtOne rfl rfl, instantObserves _ _⟩

example : testRegistry sampleProfile.target = some ⟨Unit, testReference⟩ ∧
    NativeNonvacuous testExecutionPolicy testNative testTools :=
  ⟨rfl, llvmWitness, lcbWitness, llvmUncheckedWitness, lcbUncheckedWitness⟩

/-- The conditional composition contract cannot claim correctness by suppressing
all admitted environments, even for a registered test reference. -/
example (m : SharedModels sampleProfile.target) :
    ¬ CompileCorrectWith testRegistry m testReference
      {testExecutionPolicy with launchAdmitted := fun _ _ _ => False} testNative testTools := by
  intro correct
  obtain ⟨anchor, _, _, build, env, _, _, _, _, _, ready, _⟩ := correct.2.2.2.2.2.1.1
  have impossible : testNative.launchAdmitted build.image env = False :=
    congrFun (congrFun anchor.2.1 build.image) env
  exact impossible ▸ ready.1.2

private def trapMIR : MIR.Program :=
  ⟨[⟨⟨0⟩, .add (.lit 200) (.lit 56) ⟨⟨0⟩, ⟨34, 35, 2, 30⟩⟩ none⟩], .var ⟨0⟩⟩

private def evalTrapMIR (input : String × MIR.Program) : Nat → Evaluation
  | 0 => .running
  | _ + 1 =>
    match MIR.run input.2, residualMIROrigins input.2 with
    | .trap .arithmeticOverflow, [site] => .finished (trapObservation
        ⟨.arithmeticOverflow, ⟨0⟩, ⟨site.operator.start, site.operator.stop⟩,
         ⟨input.1, site.operator.line, site.operator.column⟩⟩)
    | _, _ => .stuck

/-- The formerly contradictory unchecked edge has a successful execution with full
diagnostics on both sides, using MIR.run for the target outcome. -/
example : OriginWellFormed trapCore ∧ ConformanceStatic trapCore ∧
    Graph MIR.lowerUnchecked trapCore trapMIR ∧ residualMIROrigins trapMIR = trapCore.sites ∧
    Nonempty (Simulates coreSemantics (evaluatorSemantics evalTrapMIR)
      (trapInput.path, trapCore) (trapInput.path, trapMIR)) := by
  with_unfolding_all
    refine ⟨⟨by decide, rfl⟩, trapStatic, rfl, rfl, ⟨?_⟩⟩
    exact evaluatorSimulation (left := coreEvaluation) (right := evalTrapMIR)
      id (fun _ => 0) rfl (fun n => by cases n <;> rfl) (fun _ => .inl rfl)
      (fun n => ⟨n, Nat.le_refl n⟩) (fun n => by cases n <;> intro h <;> cases h)

example : EvaluatorAdequacy birSemantics birEvaluation := evaluatorAdequacy _

/-- This is the planted collision: successful lowering alone does not validate an origin list. -/
example :
    let planted : Elab.Output := { trapCore with sites := ⟨⟨99⟩, ⟨0, 1, 1, 1⟩⟩ :: trapCore.sites }
    OriginWellFormed trapCore ∧ ConformanceStatic planted ∧
      MIR.lowerUnchecked planted = MIR.lowerUnchecked trapCore ∧ ¬ OriginWellFormed planted := by
  with_unfolding_all
    exact ⟨⟨by decide, rfl⟩, trapStatic, rfl, fun h => by
      have wrong := congrArg (fun xs : List ConcreteSpec.Var => ((xs[0]?).map (fun v : ConcreteSpec.Var => v.id)).getD 0) h.2
      change (99 : Nat) = 0 at wrong
      contradiction⟩

private def firstOrigin : Elab.SourceSite := ⟨⟨0⟩, ⟨3, 4, 1, 4⟩⟩
private def secondOrigin : Elab.SourceSite := ⟨⟨1⟩, ⟨9, 10, 1, 10⟩⟩
private def mixedCore : Elab.Output :=
  ⟨⟨[⟨⟨0⟩, .add (.lit 1) (.lit 2)⟩, ⟨⟨1⟩, .add (.lit 200) (.lit 56)⟩], .var ⟨1⟩⟩,
    [firstOrigin, secondOrigin]⟩

example : ¬ OriginWellFormed { mixedCore with sites := [secondOrigin, firstOrigin] } :=
  fun h => by
    have wrong := congrArg (fun xs : List ConcreteSpec.Var => ((xs[0]?).map (fun v : ConcreteSpec.Var => v.id)).getD 0) h.2
    change (1 : Nat) = 0 at wrong
    contradiction

example : ¬ OriginWellFormed
    ⟨⟨[⟨⟨0⟩, .add (.lit 1) (.lit 2)⟩, ⟨⟨0⟩, .add (.lit 200) (.lit 56)⟩],
      .var ⟨0⟩⟩, [firstOrigin, firstOrigin]⟩ := by
  intro h
  exact (by decide : ¬ ([⟨0⟩, ⟨0⟩] : List ConcreteSpec.Var).Nodup) h.1

private def mixedMemory : Memory.Program := Memory.lower
  ⟨[⟨⟨0⟩, .add (.lit 1) (.lit 2) firstOrigin (some 3)⟩,
    ⟨⟨1⟩, .add (.lit 200) (.lit 56) secondOrigin none⟩], .var ⟨1⟩⟩

/-- The later operation remains lexical column 10 and occupies residual index zero. -/
example : ∃ bir o, OriginWellFormed mixedCore ∧ SSA.lower "mix.con" mixedMemory = .ok bir ∧
    SiteOrigins "mix.con" mixedCore mixedMemory bir ∧
    admittedDiagnosticOrigins mixedCore = residualOrigins mixedMemory ∧
    admittedEvaluation ("mix.con", mixedCore) 2 = .finished o ∧
    birObservation bir.sites (BIR.run bir 2) = some o ∧
    o = trapObservation ⟨.arithmeticOverflow, ⟨0⟩, ⟨9, 10⟩, ⟨"mix.con", 1, 10⟩⟩ := by
  let bir : BIR.Program :=
    ⟨[⟨[], [.bin .add .u8 0 (.imm .u8 1) (.imm .u8 2),
          .binOvf .add .u8 1 2 (.imm .u8 200) (.imm .u8 56)], .condbr 2 1 [] 2 [.reg 1, .reg 0]⟩,
      ⟨[], [], .trap 0⟩, ⟨[(3, .int .u8), (4, .int .u8)], [], .ret (.reg 3)⟩],
      [⟨.arithmeticOverflow, ⟨0⟩, ⟨9, 10⟩, ⟨"mix.con", 1, 10⟩⟩]⟩
  with_unfolding_all
    refine ⟨bir, _, ⟨by decide, rfl⟩, ?_, ⟨.cons _ (List.Sublist.refl _), rfl⟩, rfl, rfl, rfl, rfl⟩
    dsimp only [bir]
    cbv

/-- A process cannot report an empty stderr after recording a visible byte. -/
example {A : Type} (s : Semantics A) (a : A) (q : s.State)
    (h : s.trace a q = [.stderr 65]) :
    ¬ s.result a q (.returned 0 ByteArray.empty) := by
  intro bogus
  have eq := s.resultTrace a q _ bogus
  rw [h] at eq
  cases eq

/-- The extensible event alphabet can distinguish a missing grant from an authorized trace. -/
private def effectModel (grant : Bool) : Semantics Unit Unit where
  State := Unit
  initial _ _ := True
  initialized _ := ⟨(), trivial⟩
  step _ _ _ := False
  result _ _ o := o = .returned 0 ByteArray.empty
  terminal _ _ _ _ _ h := h
  safe _ _ := True
  trace _ _ := [.effect ()]
  permitted _ _ := grant = true
  resultTrace _ _ _ h := h.symm ▸ rfl
  traceGrows _ _ _ h := h.elim

example : PrefixSafety (effectModel true) (effectModel true) () () := by
  intro q _
  exact ⟨trivial, fun _ _ => ⟨rfl, rfl⟩, q, ⟨(), trivial, .nil ()⟩, trivial, [], rfl⟩

example : ¬ PrefixSafety (effectModel false) (effectModel true) () () := by
  intro h
  exact Bool.noConfusion ((h () ⟨(), trivial, .nil ()⟩).2.1 (.effect ()) (List.Mem.head _)).1

end Sanity

end Concrete.ImageTheorem
