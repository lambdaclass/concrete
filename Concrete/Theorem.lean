import Concrete.Stages
import Concrete.Backend.Observation
import LeanCompilerBackend.Verified.BuildIdentity

namespace Concrete.ImageTheorem

open Contracts Backend
open LeanCompilerBackend.Verified
open ConcreteSpec.Surface0 (Token Derives Lexes Program Expr Span Binding checkBody)

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
      ElaboratesExpr scope binding.rhs a [] [] →
      ElaboratesBindings ((binding.name, x) :: scope) rest final body sites →
      ElaboratesBindings scope (binding :: rest) final (⟨x, .atom a⟩ :: body) sites
  | operation {scope binding rest final x head tail hs ts} :
      ElaboratesExpr scope binding.rhs (.var x) head hs → head ≠ [] →
      ElaboratesBindings ((binding.name, x) :: scope) rest final tail ts →
      ElaboratesBindings scope (binding :: rest) final (head ++ tail) (hs ++ ts)

/-- Fresh IDs are checked across the whole elaboration, including temporary bindings. -/
def Elaborates (ast : Program) (core : Elab.Output) : Prop :=
  ast.name = "main" ∧ (core.program.body.map (·.x)).Nodup ∧
  ∃ bindings expr scope body tail sites origins,
    checkBody ast.body = .ok (bindings, expr) ∧
    ElaboratesBindings [] bindings scope body sites ∧
    ElaboratesExpr scope expr core.program.ret tail origins ∧
    core.program.body = body ++ tail ∧ core.sites = sites ++ origins

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

private def coreEvaluation (input : String × Elab.Output) (fuel : Nat) : Evaluation :=
  let (path, core) := input
  let program := Core.toCore core.program
  match Core.run program fuel with
  | .exit status => .finished (.returned (UInt8.ofNat status) ByteArray.empty)
  | .outOfFuel => .running
  | .stuck _ => .stuck
  | .trap .arithmeticOverflow => Id.run do
    let some index := (List.range fuel).find? (fun i =>
      match Core.run program (i + 1) with | .trap _ => true | _ => false) | return .stuck
    let some stmt := core.program.body[index]? | return .stuck
    let some (id, origin) := core.sites.zipIdx.findSome? (fun (site, id) =>
      if site.binding == stmt.x then some (id, site.operator) else none) | return .stuck
    return .finished (trapObservation ⟨.arithmeticOverflow, ⟨id⟩,
      ⟨origin.start, origin.stop⟩, ⟨path, origin.line, origin.column⟩⟩)

def coreSemantics : Semantics (String × Elab.Output) :=
  evaluatorSemantics coreEvaluation

def SourceTerminates (path : String) (core : Elab.Output) : Prop :=
  (∃ o, coreSemantics.observes (path, core) o) ∧
  (∀ run, ¬ coreSemantics.diverges (path, core) run)

/-- Independent, path-indexed semantics are required for the remaining lowering stages.
Memory is also selected by the theorem's target profile. -/
structure SharedModels (_target : LCB.Target) where
  mir : Semantics (String × MIR.Program)
  memory : Semantics (String × Memory.Program)
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

/-- The propositions to discharge against the production registry. The lexer, parser and
elaborator have fixed declarative goals. The disposition of a goal does not change its type. -/
structure SharedSound {target : LCB.Target} (m : SharedModels target) : Prop where
  lexer : ∀ bytes tokens, Graph Lex.lex bytes tokens → Lexes bytes tokens
  parser : ∀ tokens ast, Graph Parse.parse tokens ast → Derives tokens ast
  elaboration : ∀ ast core, Graph Elab.elaborate ast core → Elaborates ast core
  admission : ∀ path core admitted, Graph Admission.check core admitted →
    admitted.core = core ∧ Core.Checked (Core.toCore core.program) ∧
    ConformanceStatic core ∧ SourceTerminates path core
  admittedMIR : ∀ path core admitted mir, Graph Admission.check core admitted →
    Graph MIR.lower admitted mir → m.mirInvariant mir ∧ Nonempty (Simulates coreSemantics m.mir (path, admitted.core) (path, mir))
  uncheckedMIR : ∀ path core mir, Graph MIR.lowerUnchecked core mir →
    ConformanceStatic core → m.mirInvariant mir ∧
    Nonempty (Simulates coreSemantics m.mir (path, core) (path, mir))
  mirAdequacy : ∀ path mir, m.mirInvariant mir →
    Slice0OutcomeAgrees m.mir (path, mir) (MIR.run mir)
  memory : ∀ path mir, m.mirInvariant mir → m.memoryInvariant (Memory.lower mir) ∧
    Nonempty (Simulates m.mir m.memory (path, mir) (path, Memory.lower mir))
  memoryAdequacy : ∀ path mem, m.memoryInvariant mem →
    Slice0OutcomeAgrees m.memory (path, mem) (Memory.run mem)
  ssa : ∀ path mem bir, Graph (SSA.lower path) mem bir → m.memoryInvariant mem →
    BIR.WF bir ∧ Nonempty (Simulates m.memory birSemantics (path, mem) bir)

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

/-- The declared Lake pin is read from the LCB requirement stanza. This deliberately
accepts the project's explicit quoted revision syntax. The build identity gate checks
the resolved checkout, and the handoff also checks the imported compiler identity. -/
private def readLCBRevision (configuration : String) : Option String := do
  let stanzas := configuration.splitOn "[[require]]"
  let stanza ← stanzas.find? fun s =>
    (s.splitOn "\n").contains "name = \"lean-compiler-backend\""
  let line ← (stanza.splitOn "\n").find? (·.startsWith "rev = \"")
  match line.splitOn "\"" with
  | ["rev = ", revision, ""] =>
    if revision.length = 40 && revision.toList.all Char.isHexDigit then some revision else none
  | _ => none

def pinnedLCBRevision : Option String :=
  readLCBRevision (include_str "../lakefile.toml")

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

/-- Independently specified platform image semantics. The decoder is indexed by the
target and consumes the exact final image, including entry and relocation information.
The machine starts from the decoded image and the actual launch environment. -/
structure Platform (Env : Type) (target : LCB.Target) where
  Image : Type
  decode : ByteArray → Option Image
  machine : Semantics (Option Image × Env)
  /-- The independently specified native ISA, ABI and OS execution judgment, on raw bytes. -/
  execution : Semantics (ByteArray × Env)

def Platform.decodedProcess {Env : Type} {target : LCB.Target} (platform : Platform Env target) :
    Semantics (ByteArray × Env) :=
  platform.machine.onInput fun (image, env) => (platform.decode image, env)

/-- Native semantic interfaces indexed by the exact image, target and environment. The
intermediate views let linker, loader, runtime and trap rendering discharge distinct claims.
Each view projects to the common observation at its boundary. -/
structure NativeModels (Env : Type) (profile : Profile)
    (platform : Platform Env profile.target) where
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

def NativeModels.process {Env : Type} {profile : Profile} {platform : Platform Env profile.target} (_n : NativeModels Env profile platform) : Semantics (ByteArray × Env) :=
  platform.execution

/-- Image-level stack adequacy and launch admission are the finite-prefix safety conditions.
The stack budget is expressed in bytes and includes startup and runtime frames. -/
def LaunchReady {Env : Type} {profile : Profile} {platform : Platform Env profile.target} (n : NativeModels Env profile platform)
    (image : ByteArray) (env : Env) : Prop :=
  n.stackAdequate image env profile.maxImageStackBytes ∧ n.launchAdmitted image env

/-- Maximal execution also requires OS liveness and progress through stderr writes and exit. -/
def RunReady {Env : Type} {profile : Profile} {platform : Platform Env profile.target} (n : NativeModels Env profile platform)
    (image : ByteArray) (env : Env) : Prop :=
  LaunchReady n image env ∧ n.osLive image env ∧ n.stderrAndExitAvailable image env

/-- Shared shape of the residual image premises. These premises apply separately to each backend
and each artifact, with exact linker inputs and image bytes. Liveness supplies an observation
when finite-prefix safety and absence of target divergence have been established. -/
structure ImagePremises {Env : Type} {profile : Profile} {platform : Platform Env profile.target} (n : NativeModels Env profile platform)
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
    {platform : Platform Env profile.target} (n : NativeModels Env profile platform) :
    EdgeContract (TotalGraph (fun bir => (LLVM.module bir).toUTF8)) where
  kind := .namedPremise "LLVM printer"
  inputInvariant := BIR.WF
  outputInvariant := n.llvmWellFormed
  correspondence := Refines birSemantics n.llvmModule

/-- LLVM's unverified printer and toolchain premises for this exact artifact. -/
structure LLVMPremises {Env : Type} {profile : Profile}
    {platform : Platform Env profile.target} (n : NativeModels Env profile platform) (bir : BIR.Program) (build : LLVMBuild) : Prop where
  printer : (LLVMEmissionContract n).outputInvariant build.moduleBytes ∧
    (LLVMEmissionContract n).correspondence bir build.moduleBytes
  toolchain : n.llvmWellFormed build.moduleBytes →
    Refines n.llvmModule n.objectOperands build.moduleBytes [build.objectBytes]
  resolveLinkInputs : Refines n.objectOperands n.objects [build.objectBytes] build.linkInputs
  image : ImagePremises n build.linkInputs build.image

/-- LCB's residual native premises for this exact artifact. The adapter has its own proof
obligation. Native lowering is indexed by the profile-selected path, including Linux reification. -/
structure LCBPremises {Env : Type} {profile : Profile} {platform : Platform Env profile.target} (n : NativeModels Env profile platform)
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
def ImageCorrect {Env : Type} {profile : Profile} {platform : Platform Env profile.target} (n : NativeModels Env profile platform)
    (path : String) (core : Elab.Output) (image : ByteArray) : Prop :=
  (∀ env, LaunchReady n image env →
    PrefixSafety coreSemantics n.process (path, core) (image, env)) ∧
  (∀ env, RunReady n image env →
    MaximalRefinement coreSemantics n.process (path, core) (image, env)) ∧
  (SourceTerminates path core → ∀ env, RunReady n image env →
    ∃ o, n.process.observes (image, env) o)

def Compiled (input : Stages.Input) (out : Stages.Output) : Prop :=
  SharedHandoffs input out ∧ Frontend input out ∧ BIR.WF out.bir ∧
  SiteOrigins input.path out.core out.memory out.bir ∧
  Refines coreSemantics birSemantics (input.path, out.core) out.bir ∧
  SourceTerminates input.path out.core

def LLVMCorrect {Env : Type} {profile : Profile}
    {platform : Platform Env profile.target} (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  ∀ input out build, Stages.compile input = .ok out → BIRSlotsFit profile out.bir →
    LLVMHandoffs profile tools out.bir build → LLVMPremises n out.bir build →
    Compiled input out ∧ ImageCorrect n input.path out.core build.image

def LCBCorrect {Env : Type} {profile : Profile}
    {platform : Platform Env profile.target} (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  ∀ input out build, Stages.compile input = .ok out → BIRSlotsFit profile out.bir →
    LCBHandoffs profile tools out.bir build → LCBPremises n build →
    Compiled input out ∧ ImageCorrect n input.path out.core build.image

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

def ExactTrap {Env : Type} {profile : Profile} {platform : Platform Env profile.target} (n : NativeModels Env profile platform)
    (input : Stages.Input) (core : Elab.Output) (image : ByteArray) : Prop :=
  ∀ status kind site stderr, coreSemantics.observes (input.path, core)
    (.trapped status kind site stderr) → ∀ env, RunReady n image env →
      n.process.observes (image, env) (.trapped status kind site stderr)

def UncheckedCorrect {Env : Type} {profile : Profile}
    {platform : Platform Env profile.target} (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  ∀ input core bir, UncheckedHandoffs input core → ConformanceStatic core →
    Stages.unchecked input = .ok bir → BIRSlotsFit profile bir →
    UncheckedFrontend input core ∧ BIR.WF bir ∧ UncheckedOrigins input core bir ∧ SourceTerminates input.path core ∧
    Refines coreSemantics birSemantics (input.path, core) bir ∧
    (∀ build, LLVMHandoffs profile tools bir build → LLVMPremises n bir build →
      ImageCorrect n input.path core build.image ∧ ExactTrap n input core build.image) ∧
    (∀ build, LCBHandoffs profile tools bir build → LCBPremises n build →
      ImageCorrect n input.path core build.image ∧ ExactTrap n input core build.image)

/-- The stage and adapter obligations are part of what this theorem must establish. The
artifact-specific external hypotheses are local to the build claims below them. -/
def compile_correct {Env : Type} {profile : Profile} (m : SharedModels profile.target)
    {platform : Platform Env profile.target} (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  SharedSound m ∧ LCBAdapterSound ∧
    LLVMCorrect n tools ∧ LCBCorrect n tools ∧ UncheckedCorrect n tools

def LLVMNonvacuous {Env : Type} {profile : Profile}
    {platform : Platform Env profile.target} (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  ∃ input out build env o,
    Stages.compile input = .ok out ∧ BIRSlotsFit profile out.bir ∧
    LLVMHandoffs profile tools out.bir build ∧ LLVMPremises n out.bir build ∧
    RunReady n build.image env ∧ SourceTerminates input.path out.core ∧
    coreSemantics.observes (input.path, out.core) o ∧ n.process.observes (build.image, env) o

def LCBNonvacuous {Env : Type} {profile : Profile}
    {platform : Platform Env profile.target} (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  ∃ input out build env o,
    Stages.compile input = .ok out ∧ BIRSlotsFit profile out.bir ∧
    LCBHandoffs profile tools out.bir build ∧ LCBPremises n build ∧
    RunReady n build.image env ∧ SourceTerminates input.path out.core ∧
    coreSemantics.observes (input.path, out.core) o ∧ n.process.observes (build.image, env) o

def UncheckedNonvacuous {Env : Type} {profile : Profile} {Build : Type}
    {platform : Platform Env profile.target} (n : NativeModels Env profile platform) (image : Build → ByteArray)
    (premises : BIR.Program → Build → Prop) : Prop :=
  ∃ input core bir build env status kind site stderr,
    UncheckedHandoffs input core ∧ ConformanceStatic core ∧
    Stages.unchecked input = .ok bir ∧ BIRSlotsFit profile bir ∧ premises bir build ∧
    RunReady n (image build) env ∧ SourceTerminates input.path core ∧
    coreSemantics.observes (input.path, core) (.trapped status kind site stderr) ∧
    n.process.observes (image build, env) (.trapped status kind site stderr)

def LLVMUncheckedNonvacuous {Env : Type} {profile : Profile}
    {platform : Platform Env profile.target} (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  UncheckedNonvacuous n LLVMBuild.image fun bir build =>
    LLVMHandoffs profile tools bir build ∧ LLVMPremises n bir build

def LCBUncheckedNonvacuous {Env : Type} {profile : Profile}
    {platform : Platform Env profile.target} (n : NativeModels Env profile platform) (tools : NativeTools) : Prop :=
  UncheckedNonvacuous n LCBBuild.image fun bir build =>
    LCBHandoffs profile tools bir build ∧ LCBPremises n build


section Sanity

private def answerInput : Stages.Input :=
  ⟨"tests/slice0/answer.con", r#"fn main() -> u8 { return 20 + 22; }"#.toUTF8⟩

private def exit42 : Observation := .returned 42 ByteArray.empty

/-- An exact terminal observation supplies a safe prefix and admits its identity extension. -/
private theorem returnAtTwo {A : Type} {evaluate : A → Nat → Evaluation}
    {a : A} {o : Observation} (h0 : evaluate a 0 = .running) (h1 : evaluate a 1 = .running)
    (h2 : evaluate a 2 = .finished o) : (evaluatorSemantics evaluate).observes a o :=
  ⟨(2 : Nat), ⟨(0 : Nat), rfl, .cons ⟨h0, rfl⟩ (.cons ⟨h1, rfl⟩ (.nil (2 : Nat)))⟩, h2⟩

private theorem returnAtOne {A : Type} {evaluate : A → Nat → Evaluation}
    {a : A} {o : Observation} (h0 : evaluate a 0 = .running)
    (h1 : evaluate a 1 = .finished o) : (evaluatorSemantics evaluate).observes a o :=
  ⟨(1 : Nat), ⟨(0 : Nat), rfl, .cons ⟨h0, rfl⟩ (.nil (1 : Nat))⟩, h1⟩

example : ∃ out, Stages.compile answerInput = .ok out ∧
    coreSemantics.observes (answerInput.path, out.core) exit42 := by
  refine ⟨_, rfl, ?_⟩
  exact returnAtTwo rfl rfl rfl

example : ¬ ConformanceStatic ⟨⟨[], .lit 256⟩, []⟩ := by
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
  refine ⟨_, _, rfl, rfl, ?_, rfl, rfl, rfl⟩
  decide

example : (Observation.returned 134 ByteArray.empty) ≠ overflowObservation "src/a.con" := by
  intro h
  cases h

example : ¬ SameInfiniteTrace (fun _ => []) (fun _ => [Event.stderr 65]) := by
  intro same
  obtain ⟨_, suffix, impossible⟩ := (same [Event.stderr 65]).mpr ⟨0, [], rfl⟩
  cases impossible

private def samplePin : ToolInvocation :=
  ⟨.llvmObject, "clang", "pinned", ["-c", "llvm.ll", "-o", "llvm.o"]⟩

private def sampleProfile : Profile := ⟨.x86_64Linux, 64, 4096, [samplePin]⟩

example : sampleProfile.invocation .llvmObject samplePin := by rfl

example : ¬ sampleProfile.invocation .llvmObject { samplePin with arguments := ["-S"] } := by
  unfold Profile.invocation
  decide

private def instant {A : Type} (observe : A → Observation) : Semantics A where
  State := Unit
  initial _ _ := True
  initialized _ := ⟨(), trivial⟩
  step _ _ _ := False
  result a _ o := observe a = o
  terminal _ _ _ _ _ h := h
  safe _ _ := True
  trace a _ := (observe a).events
  traceGrows _ _ _ h := h.elim

private theorem instantObserves {A : Type} (f : A → Observation) (a : A) :
    (instant f).observes a (f a) :=
  ⟨(), ⟨(), trivial, .nil ()⟩, rfl⟩

private theorem refinementsToInstant {A B : Type} (source : Semantics A) (a : A)
    (f : B → Observation) (b : B) (h : ∃ q, source.finite a q ∧ source.safe a q ∧
      source.result a q (f b) ∧ source.trace a q = (f b).events) :
    Refines source (instant f) a b := by
  obtain ⟨q, hr, hs, ho, ht⟩ := h
  refine ⟨?_, ?_, ?_, ?_⟩
  · intro _ _
    exact ⟨trivial, q, hr, hs, [], by simpa [instant] using ht.symm⟩
  · rintro o ⟨_, _, e⟩
    exact ⟨q, hr, e ▸ ho⟩
  · intro run r
    exact (r.2 0).elim
  · intro _ stuck
    exact stuck.2.2 (f b) rfl

private theorem instantRefines {A B : Type} (f : A → Observation) (g : B → Observation)
    (a : A) (b : B) (h : f a = g b) : Refines (instant f) (instant g) a b := by
  apply refinementsToInstant
  exact ⟨(), ⟨(), trivial, .nil ()⟩, trivial, h, congrArg Observation.events h⟩

private def bytecodeObservation (bytes : ByteArray) : Observation :=
  .returned (bytes.data[0]?.getD 0) ByteArray.empty

private def answerCore : Elab.Output :=
  (Stages.compile answerInput).toOption.map (·.core) |>.getD ⟨⟨[], .lit 0⟩, []⟩

example : Refines coreSemantics (instant bytecodeObservation)
    (answerInput.path, answerCore) ⟨#[42]⟩ := by
  have obs : coreSemantics.observes (answerInput.path, answerCore) exit42 :=
    returnAtTwo rfl rfl rfl
  obtain ⟨q, reach, terminal⟩ := obs
  apply refinementsToInstant
  exact ⟨q, reach, (fun h => by rw [terminal] at h; cases h), terminal,
    show (coreEvaluation _ q).events = _ by rw [terminal]; rfl⟩

example : ¬ Refines coreSemantics (instant bytecodeObservation)
    (answerInput.path, answerCore) ⟨#[43]⟩ := by
  intro bad
  obtain ⟨q, _, terminal⟩ := bad.2.1 _ (instantObserves bytecodeObservation ⟨#[43]⟩)
  change Nat at q
  change coreEvaluation (answerInput.path, answerCore) q = .finished
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
private def testPlatform (target : LCB.Target) (observe : ByteArray → Observation) :
    Platform Unit target where
  Image := ByteArray
  decode := some
  machine := instant fun (bytes, _) => observe (bytes.getD ByteArray.empty)
  execution := instant fun (bytes, _) => observe bytes

private def testNative (profile : Profile) (observe : ByteArray → Observation) (image : ByteArray) :
    NativeModels Unit profile (testPlatform profile.target observe) where
  llvmModule := instant fun _ => observe image
  llvmWellFormed _ := True
  lcbBodyAssembly := instant fun _ => observe image
  lcbAssembly := instant fun _ => observe image
  objectOperands := instant fun _ => observe image
  objects := instant fun _ => observe image
  linked := instant observe
  loaded := instant fun (bytes, _) => observe bytes
  runtime := instant fun (bytes, _) => observe bytes
  stackAdequate _ _ _ := True
  launchAdmitted _ _ := True
  osLive _ _ := True
  stderrAndExitAvailable _ _ := True

private theorem testImagePremises (profile : Profile) (observe : ByteArray → Observation)
    (inputs : List ByteArray) (image : ByteArray) :
    ImagePremises (testNative profile observe image) inputs image where
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

private theorem testLLVMPremises (observe : ByteArray → Observation) (bir : BIR.Program)
    (build : LLVMBuild) (h : birSemantics.observes bir (observe build.image)) :
    LLVMPremises (testNative sampleProfile observe build.image) bir build where
  printer := by
    obtain ⟨q, reach, terminal⟩ := h
    exact ⟨trivial, refinementsToInstant _ _ _ _
      ⟨q, reach, (fun e => by rw [terminal] at e; cases e), terminal,
        show (birEvaluation bir q).events = _ by rw [terminal]; rfl⟩⟩
  toolchain _ := instantRefines _ _ _ _ rfl
  resolveLinkInputs := instantRefines _ _ _ _ rfl
  image := testImagePremises _ _ _ _

private theorem lcbSteps {i : LCBInput} {s t : Source.State} {fuel : Nat}
    (h : Source.Steps i.1 fuel s t) : Steps (lcbSemantics.step i) (some s) (some t) := by
  induction h with
  | zero _ => exact .nil _
  | succ h _ ih => exact .cons ⟨_, _, rfl, rfl, h⟩ ih

private theorem lcbToInstant {i : LCBInput} {value : Source.Value}
    (ret : Source.HasBehavior i.1 #[] (.returned value)) {B : Type}
    {f : B → Observation} {b : B} (project : lcbResult i.2.1 value = some (f b)) :
    Refines lcbSemantics (instant f) i b := by
  obtain ⟨s, init, _, q, steps, terminal⟩ := ret
  have result : lcbTerminal i (some q) = some (f b) := by simpa [lcbTerminal, terminal]
  apply refinementsToInstant
  exact ⟨some q, ⟨some s, by simp [lcbSemantics, init]; rfl, lcbSteps steps⟩,
    Or.inr (by rw [result]; rfl), result, by simp [lcbSemantics, result]⟩

private theorem testLCBPremises (observe : ByteArray → Observation) (build : LCBBuild)
    (h : Refines lcbSemantics (testNative sampleProfile observe build.image).lcbBodyAssembly
      (build.source, build.sites, build.runtimeInput) (build.bodyAssembly, build.runtimeInput)) :
    LCBPremises (testNative sampleProfile observe build.image) build where
  nativeLowering _ _ := h
  compatibilityLowering _ _ := h
  runtimeLowering := instantRefines _ _ _ _ rfl
  assembler := instantRefines _ _ _ _ rfl
  resolveLinkInputs := instantRefines _ _ _ _ rfl
  image := testImagePremises _ _ _ _

private def testLLVMBuild (bir : BIR.Program) (image : ByteArray) : LLVMBuild :=
  ⟨(LLVM.module bir).toUTF8, ⟨#[1]⟩, [⟨#[1]⟩], image⟩

private def answerBIR : BIR.Program :=
  (Stages.compile answerInput).toOption.map (·.bir) |>.getD ⟨[], []⟩

example : ∃ out build, Stages.compile answerInput = .ok out ∧
    LLVMPremises (testNative sampleProfile bytecodeObservation build.image) out.bir build := by
  refine ⟨_, testLLVMBuild answerBIR ⟨#[42]⟩, rfl, ?_⟩
  exact testLLVMPremises _ _ _ (returnAtOne rfl rfl)

private def testLCBBuild (bir : BIR.Program) (image : ByteArray) : LCBBuild :=
  let (body, sites) := (LCB.emit bir).toOption.getD (⟨.word, #[], #[], #[]⟩, [])
  ⟨"", compilerBuildIdentity, body, sites, (Runtime.qbe sites).toUTF8,
    ⟨#[1]⟩, ⟨#[2]⟩, ⟨#[3]⟩, ⟨#[4]⟩, [⟨#[3]⟩, ⟨#[4]⟩], image⟩

example : ∃ out build, Stages.compile answerInput = .ok out ∧
    LCB.emit out.bir = .ok (build.source, build.sites) ∧
    LCBPremises (testNative sampleProfile bytecodeObservation build.image) build := by
  let build := testLCBBuild answerBIR ⟨#[42]⟩
  have hEmit : LCB.emit answerBIR = .ok (build.source, build.sites) := rfl
  refine ⟨_, build, rfl, hEmit, testLCBPremises _ _ ?_⟩
  have h := (LCB.Proof.adapter_correct_run (p := answerBIR) rfl (LCB.Proof.emit_ok hEmit).1).1
  exact lcbToInstant (h 1 42 rfl) rfl

private def trapBytecodeObservation (bytes : ByteArray) : Observation :=
  if bytes.data[0]? == some (134 : UInt8) then overflowObservation "src/a.con"
  else bytecodeObservation bytes

example : ∃ bir llvm lcb,
    Stages.unchecked (overflowInput "src/a.con" "// x") = .ok bir ∧
    LCB.emit bir = .ok (lcb.source, lcb.sites) ∧
    LLVMPremises (testNative sampleProfile trapBytecodeObservation llvm.image) bir llvm ∧
    LCBPremises (testNative sampleProfile trapBytecodeObservation lcb.image) lcb := by
  let bir := (Stages.unchecked (overflowInput "src/a.con" "// x")).toOption.getD ⟨[], []⟩
  let build := testLCBBuild bir ⟨#[134]⟩
  have hEmit : LCB.emit bir = .ok (build.source, build.sites) := rfl
  refine ⟨bir, testLLVMBuild bir ⟨#[134]⟩, build, rfl, hEmit, ?_, testLCBPremises _ _ ?_⟩
  · exact testLLVMPremises _ _ _ (returnAtTwo rfl rfl rfl)
  · have h := (LCB.Proof.adapter_correct_run (p := bir) rfl (LCB.Proof.emit_ok hEmit).1).2.1
    have hRun : BIR.run bir 2 = .trapped 0 := rfl
    have hProjection : lcbResult build.sites (LCB.word 256) =
        some (trapBytecodeObservation build.image) := rfl
    exact lcbToInstant (i := (build.source, build.sites, build.runtimeInput))
      (h 2 0 hRun) hProjection

end Sanity

end Concrete.ImageTheorem
