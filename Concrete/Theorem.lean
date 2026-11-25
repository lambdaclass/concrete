import Concrete.Stages
import Concrete.Backend.Observation

namespace Concrete.ImageTheorem

open Contracts Backend
open LeanCompilerBackend.Verified
open ConcreteSpec.Surface0 (Token Derives Lexes Program)

/-- The declarative frontend judgment and the semantics at the shared compiler boundaries.
Elaboration relates the exact surface derivation to the Core payload and its lexical origins.
Each semantics is shared by both adjacent contracts. -/
structure SharedModels where
  elaborates : Stages.Input → Program → Elab.Output → Prop
  core : Semantics Elab.Output
  mir : Semantics MIR.Program
  memory : Semantics Memory.Program
  bir : Semantics BIR.Program

/-- Admission retains the exact Core payload whose behaviors the lowering must preserve. -/
def SemanticsOnAdmission (s : Semantics Elab.Output) : Semantics Admission.Output :=
  { s with finite := fun a => s.finite a.core,
           terminates := fun a => s.terminates a.core,
           diverges := fun a => s.diverges a.core,
           stuck := fun a => s.stuck a.core }

/-- The source has a terminal observation and excludes divergent executions. The slice-0
termination proof must establish this for admitted Core. -/
def SourceTerminates (m : SharedModels) (core : Elab.Output) : Prop :=
  (∃ o, m.core.observes core o) ∧ (∀ run, ¬ m.core.diverges core run)

/-- Production and conformance contracts, indexed by the source whose path SSA uses. -/
structure SharedContracts (m : SharedModels) (input : Stages.Input) where
  lex : EdgeContract (Graph Lex.lex)
  parse : EdgeContract (Graph Parse.parse)
  elaborate : EdgeContract (Graph Elab.elaborate)
  admission : EdgeContract (Graph Admission.check)
  admittedMIR : LoweringContract (Graph MIR.lower)
    (SemanticsOnAdmission m.core) m.mir
  uncheckedMIR : LoweringContract (Graph MIR.lowerUnchecked) m.core m.mir
  memory : LoweringContract (TotalGraph Memory.lower) m.mir m.memory
  ssa : LoweringContract (Graph (SSA.lower input.path)) m.memory m.bir

/-- Proof obligations of the shared registry. The frontend and admission conclusions constrain
the same judgments and payloads used in the end-to-end claim. -/
def SharedContracts.sound {m : SharedModels} {input : Stages.Input}
    (c : SharedContracts m input) : Prop :=
  c.lex.kind = .directProof ∧ c.parse.kind = .directProof ∧ c.elaborate.kind = .directProof ∧
  c.admission.kind = .provedValidator ∧ c.admittedMIR.kind = .directProof ∧
  c.uncheckedMIR.kind = .directProof ∧ c.memory.kind = .directProof ∧
  c.ssa.kind = .provedValidator ∧
  c.lex.inputInvariant input.source ∧
  c.lex.sound ∧ c.parse.sound ∧ c.elaborate.sound ∧ c.admission.sound ∧
  c.admittedMIR.edge.sound ∧ c.uncheckedMIR.edge.sound ∧ c.memory.edge.sound ∧ c.ssa.edge.sound ∧
  Compatible c.lex c.parse ∧ Compatible c.parse c.elaborate ∧ Compatible c.elaborate c.admission ∧
  Compatible c.admission c.admittedMIR.edge ∧ Compatible c.elaborate c.uncheckedMIR.edge ∧
  Compatible c.admittedMIR.edge c.memory.edge ∧ Compatible c.uncheckedMIR.edge c.memory.edge ∧
  Compatible c.memory.edge c.ssa.edge ∧
  (∀ bytes tokens, c.lex.correspondence bytes tokens → Lexes bytes tokens) ∧
  (∀ tokens ast, c.parse.correspondence tokens ast → Derives tokens ast) ∧
  (∀ ast core, c.elaborate.correspondence ast core → m.elaborates input ast core) ∧
  (∀ core admitted, c.admission.correspondence core admitted →
    admitted.core = core ∧ Core.Checked (Core.toCore core.program) ∧ SourceTerminates m core) ∧
  (∀ bir, c.ssa.outputInvariant bir → BIR.WF bir)

/-- The exact source-to-admitted-Core derivation recorded in the production output. -/
def Frontend (m : SharedModels) (input : Stages.Input) (out : Stages.Output) : Prop :=
  Lexes input.source out.tokens ∧ Derives out.tokens out.ast ∧
  m.elaborates input out.ast out.core ∧
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

/-- The in-process LCB dispatcher selects its lowering path by target. -/
def lcbPath : LCB.Target → LoweringPath
  | .aarch64Macos => .lcbVerified
  | .x86_64Linux | .aarch64Linux => .lcbCompatibility

/-- Exact tool identities and effective arguments for one invocation. -/
structure ToolInvocation where
  tool : String
  revision : String
  arguments : List String

/-- The pinned target and its distinct IR-slot and image-stack budgets. -/
structure Profile where
  target : LCB.Target
  maxBIRSlots : Nat
  maxImageStackBytes : Nat
  tools : List ToolInvocation

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
  source : Source.Function
  sites : BIR.SiteTable
  runtimeInput : ByteArray
  bodyAssembly : ByteArray
  runtimeAssembly : ByteArray
  bodyObject : ByteArray
  runtimeObject : ByteArray
  linkInputs : List ByteArray
  image : ByteArray

/-- External tool handoffs for a single pinned profile. These relations describe the actual
building invocations and their bytes. Semantic correctness is a separate obligation. -/
structure NativeTools (profile : Profile) where
  lcbRevision : String
  llvmObject : ByteArray → ByteArray → Prop
  runtimeLowering : ByteArray → ByteArray → Prop
  assemble : ByteArray → ByteArray → Prop
  /-- Relates the driver's ordered object operands and all resolved linker inputs to the image.
  Resolved inputs include any objects, archives or scripts inserted by the pinned driver. -/
  link : List ByteArray → List ByteArray → ByteArray → Prop

/-- The driver supplied the full LLVM module, including the runtime, and linked its exact object. -/
def LLVMHandoffs {profile : Profile} (tools : NativeTools profile) (bir : BIR.Program)
    (build : LLVMBuild) : Prop :=
  build.moduleBytes = (LLVM.module bir).toUTF8 ∧
  tools.llvmObject build.moduleBytes build.objectBytes ∧
  tools.link [build.objectBytes] build.linkInputs build.image

/-- The driver passed the checked LCB function, runtime and site table through the actual
target dispatcher and tool invocations. Success of the adapter and job validator is required. -/
def LCBHandoffs {profile : Profile} (tools : NativeTools profile) (bir : BIR.Program)
    (build : LCBBuild) : Prop :=
  build.revision = tools.lcbRevision ∧
  LCB.emit bir = .ok (build.source, build.sites) ∧
  validateJob (LCB.job build.source) = .ok () ∧
  Stages.prepareLCB bir = .ok build.source ∧
  build.sites = bir.sites ∧ build.runtimeInput = (Runtime.qbe build.sites).toUTF8 ∧
  (∃ assembly, LCB.assemble profile.target build.source = .ok assembly ∧
    build.bodyAssembly = assembly.toUTF8) ∧
  tools.runtimeLowering build.runtimeInput build.runtimeAssembly ∧
  tools.assemble build.bodyAssembly build.bodyObject ∧
  tools.assemble build.runtimeAssembly build.runtimeObject ∧
  tools.link [build.bodyObject, build.runtimeObject] build.linkInputs build.image

/-- Native semantic interfaces indexed by the exact image, target and environment. The
intermediate views let linker, loader, runtime and trap rendering discharge distinct claims.
Each view projects to the common observation at its boundary. -/
structure NativeModels (Env : Type) (profile : Profile) where
  llvmModule : Semantics ByteArray
  llvmWellFormed : ByteArray → Prop
  lcbSource : Semantics (Source.Function × BIR.SiteTable × ByteArray)
  lcbBodyAssembly : Semantics (ByteArray × ByteArray)
  lcbAssembly : Semantics (ByteArray × ByteArray)
  objects : Semantics (List ByteArray)
  linked : Semantics ByteArray
  loaded : Semantics (ByteArray × Env)
  runtime : Semantics (ByteArray × Env)
  process : Semantics (ByteArray × Env)
  stackAdequate : ByteArray → Env → Nat → Prop
  launchAdmitted : ByteArray → Env → Prop
  osLive : ByteArray → Env → Prop
  /-- The launch environment permits all required stderr writes and exit, including progress
  through short writes. The rendering edge specifies which bytes the run observes. -/
  stderrAndExitAvailable : ByteArray → Env → Prop

/-- Image-level stack adequacy and launch admission are the finite-prefix safety conditions.
The stack budget is expressed in bytes and includes startup and runtime frames. -/
def LaunchReady {Env : Type} {profile : Profile} (n : NativeModels Env profile)
    (image : ByteArray) (env : Env) : Prop :=
  n.stackAdequate image env profile.maxImageStackBytes ∧ n.launchAdmitted image env

/-- Maximal execution also requires OS liveness and progress through stderr writes and exit. -/
def RunReady {Env : Type} {profile : Profile} (n : NativeModels Env profile)
    (image : ByteArray) (env : Env) : Prop :=
  LaunchReady n image env ∧ n.osLive image env ∧ n.stderrAndExitAvailable image env

/-- Shared shape of the residual image premises. These premises apply separately to each backend
and each artifact, with exact linker inputs and image bytes. Liveness supplies an observation
when finite-prefix safety and absence of target divergence have been established. -/
structure ImagePremises {Env : Type} {profile : Profile} (n : NativeModels Env profile)
    (inputs : List ByteArray) (image : ByteArray) : Prop where
  linker : Refines n.objects n.linked inputs image
  loader : ∀ env, n.stackAdequate image env profile.maxImageStackBytes →
    n.launchAdmitted image env → Refines n.linked n.loaded image (image, env)
  runtimeSafety : ∀ env, LaunchReady n image env →
    PrefixSafety n.loaded n.runtime (image, env) (image, env)
  trapRenderingSafety : ∀ env, LaunchReady n image env →
    PrefixSafety n.runtime n.process (image, env) (image, env)
  runtime : ∀ env, RunReady n image env →
    MaximalRefinement n.loaded n.runtime (image, env) (image, env)
  trapRendering : ∀ env, RunReady n image env →
    MaximalRefinement n.runtime n.process (image, env) (image, env)
  liveness : ∀ env, RunReady n image env → PrefixSafety n.linked n.process image (image, env) →
    (¬ ∃ run, n.process.diverges (image, env) run) →
    ∃ o, n.process.observes (image, env) o

/-- The LLVM printer edge targets the complete module as written to the tool input. -/
def LLVMEmissionContract {Env : Type} {profile : Profile} (m : SharedModels)
    (n : NativeModels Env profile) :
    EdgeContract (TotalGraph (fun bir => (LLVM.module bir).toUTF8)) where
  kind := .namedPremise "LLVM printer"
  inputInvariant := BIR.WF
  outputInvariant := n.llvmWellFormed
  correspondence := Refines m.bir n.llvmModule

/-- LLVM's unverified printer and toolchain premises for this exact artifact. -/
structure LLVMPremises {Env : Type} {profile : Profile} (m : SharedModels)
    (n : NativeModels Env profile) (bir : BIR.Program) (build : LLVMBuild) : Prop where
  printer : (LLVMEmissionContract m n).outputInvariant build.moduleBytes ∧
    (LLVMEmissionContract m n).correspondence bir build.moduleBytes
  toolchain : n.llvmWellFormed build.moduleBytes →
    Refines n.llvmModule n.objects build.moduleBytes build.linkInputs
  image : ImagePremises n build.linkInputs build.image

/-- LCB's residual native premises for this exact artifact. The adapter has its own proof
obligation. Native lowering is indexed by the profile-selected path, including Linux reification. -/
structure LCBPremises {Env : Type} {profile : Profile} (n : NativeModels Env profile)
    (build : LCBBuild) : Prop where
  nativeLowering : lcbPath profile.target = .lcbVerified →
    Refines n.lcbSource n.lcbBodyAssembly (build.source, build.sites, build.runtimeInput)
      (build.bodyAssembly, build.runtimeInput)
  compatibilityLowering : lcbPath profile.target = .lcbCompatibility →
    Refines n.lcbSource n.lcbBodyAssembly (build.source, build.sites, build.runtimeInput)
      (build.bodyAssembly, build.runtimeInput)
  runtimeLowering : Refines n.lcbBodyAssembly n.lcbAssembly
    (build.bodyAssembly, build.runtimeInput) (build.bodyAssembly, build.runtimeAssembly)
  assembler : Refines n.lcbAssembly n.objects (build.bodyAssembly, build.runtimeAssembly)
    build.linkInputs
  image : ImagePremises n build.linkInputs build.image

/-- The directly proved LCB emission edge retains the exact site table and refines BIR
against the imported source semantics and the runtime projection. -/
def LCBAdapterContract {Env : Type} {profile : Profile} (m : SharedModels)
    (n : NativeModels Env profile) : EdgeContract (Graph LCB.emit) where
  kind := .directProof
  inputInvariant := BIR.WF
  outputInvariant := fun (_, sites) => ∀ entry ∈ sites, entry.loc.wf = true
  correspondence := fun bir (f, sites) =>
    sites = bir.sites ∧ Refines m.bir n.lcbSource bir (f, sites, (Runtime.qbe sites).toUTF8)

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
def LCBAdapterSound {Env : Type} {profile : Profile} (m : SharedModels)
    (n : NativeModels Env profile) : Prop :=
  LCBSourceAdequacy n.lcbSource ∧ BIREvaluatorAdequacy m.bir ∧
  (LCBAdapterContract m n).sound ∧ LCBProfileContract.sound ∧
  Compatible (LCBAdapterContract m n) LCBProfileContract

/-- The admission-independent dimensional limit applies to BIR before the driver emits a backend
artifact. It is separate from the stack adequacy obligation for its linked image. -/
def BIRSlotsFit (profile : Profile) (bir : BIR.Program) : Prop :=
  bir.frameSlots ≤ profile.maxBIRSlots

/-- The source-to-image conclusion, conditional on the resources and environment of this run.
The final clause applies to observations of terminating source programs. This includes the
admitted slice-0 programs; later slices can have admitted divergence. -/
def ImageCorrect {Env : Type} {profile : Profile} (m : SharedModels)
    (n : NativeModels Env profile) (core : Elab.Output) (image : ByteArray) : Prop :=
  (∀ env, LaunchReady n image env → PrefixSafety m.core n.process core (image, env)) ∧
  (∀ env, RunReady n image env → MaximalRefinement m.core n.process core (image, env)) ∧
  (SourceTerminates m core → ∀ env, RunReady n image env →
    ∃ o, n.process.observes (image, env) o)

/-- The LLVM half of the conditional source-to-image theorem statement. -/
def LLVMCorrect {Env : Type} {profile : Profile} (m : SharedModels)
    (n : NativeModels Env profile) (tools : NativeTools profile) : Prop :=
  ∀ input (c : SharedContracts m input) out build, c.sound →
    Stages.compile input = .ok out → BIRSlotsFit profile out.bir →
    LLVMHandoffs tools out.bir build → LLVMPremises m n out.bir build →
    SharedHandoffs input out ∧ Frontend m input out ∧ BIR.WF out.bir ∧
    Refines m.core m.bir out.core out.bir ∧ SourceTerminates m out.core ∧
    ImageCorrect m n out.core build.image

/-- The LCB half of the conditional source-to-image theorem statement. -/
def LCBCorrect {Env : Type} {profile : Profile} (m : SharedModels)
    (n : NativeModels Env profile) (tools : NativeTools profile) : Prop :=
  ∀ input (c : SharedContracts m input) out build, c.sound → LCBAdapterSound m n →
    Stages.compile input = .ok out → BIRSlotsFit profile out.bir →
    LCBHandoffs tools out.bir build → LCBPremises n build →
    SharedHandoffs input out ∧ Frontend m input out ∧ BIR.WF out.bir ∧
    Refines m.core m.bir out.core out.bir ∧ SourceTerminates m out.core ∧
    ImageCorrect m n out.core build.image

/-- The exact frontend chain for an unchecked conformance case. It intentionally has no
checker acceptance or discharge-token hypothesis. -/
def UncheckedFrontend (m : SharedModels) (input : Stages.Input) (core : Elab.Output) : Prop :=
  ∃ tokens ast, Lex.lex input.source = .ok tokens ∧ Parse.parse tokens = .ok ast ∧
    Elab.elaborate ast = .ok core ∧ Lexes input.source tokens ∧ Derives tokens ast ∧
    m.elaborates input ast core

/-- The conformance conclusion preserves trap kind, lexical site, exit status and stderr.
A terminating source case has a matching target observation when no other source maximal
behavior is allowed. -/
def UncheckedCorrect {Env : Type} {profile : Profile} (m : SharedModels)
    (n : NativeModels Env profile) (tools : NativeTools profile) : Prop :=
  ∀ input (c : SharedContracts m input) bir, c.sound →
    Stages.unchecked input = .ok bir → BIRSlotsFit profile bir →
    ∃ core, UncheckedFrontend m input core ∧ BIR.WF bir ∧
      Refines m.core m.bir core bir ∧
      (∀ build, LLVMHandoffs tools bir build → LLVMPremises m n bir build →
        ImageCorrect m n core build.image) ∧
      (∀ build, LCBAdapterSound m n → LCBHandoffs tools bir build → LCBPremises n build →
        ImageCorrect m n core build.image)

/-- The composed conditional theorem obligation for both backends and the conformance path. -/
def compile_correct {Env : Type} {profile : Profile} (m : SharedModels)
    (n : NativeModels Env profile) (tools : NativeTools profile) : Prop :=
  LLVMCorrect m n tools ∧ LCBCorrect m n tools ∧ UncheckedCorrect m n tools

/-- One joint witness must satisfy the LLVM claim's actual hypotheses and observe a result. -/
def LLVMNonvacuous {Env : Type} {profile : Profile} (m : SharedModels)
    (n : NativeModels Env profile) (tools : NativeTools profile) : Prop :=
  ∃ input, ∃ (c : SharedContracts m input), ∃ out build env o, c.sound ∧
    Stages.compile input = .ok out ∧ BIRSlotsFit profile out.bir ∧
    LLVMHandoffs tools out.bir build ∧ LLVMPremises m n out.bir build ∧
    RunReady n build.image env ∧ SourceTerminates m out.core ∧ m.core.observes out.core o ∧
    n.process.observes (build.image, env) o

/-- One joint witness must satisfy the LCB claim's actual hypotheses and observe a result
on the profile of the claim, including the pilot's launch and OS liveness premises. -/
def LCBNonvacuous {Env : Type} {profile : Profile} (m : SharedModels)
    (n : NativeModels Env profile) (tools : NativeTools profile) : Prop :=
  ∃ input, ∃ (c : SharedContracts m input), ∃ out build env o, c.sound ∧ LCBAdapterSound m n ∧
    Stages.compile input = .ok out ∧ BIRSlotsFit profile out.bir ∧
    LCBHandoffs tools out.bir build ∧ LCBPremises n build ∧
    RunReady n build.image env ∧ SourceTerminates m out.core ∧ m.core.observes out.core o ∧
    n.process.observes (build.image, env) o

end Concrete.ImageTheorem
