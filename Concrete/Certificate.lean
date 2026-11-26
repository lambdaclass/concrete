import Concrete.Theorem

namespace Concrete.Certificate

open ImageTheorem

/-- Domain tags for the canonical, versioned digest encoding. Domains describe payload identity;
semantic correspondence is established by evidence at the appropriate edge. -/
inductive DigestDomain where
  | source | compiler | spec | bir | llvmModule | lcbFunction | lcbJob | siteTable
  | runtimeInput | assembly | objectFile | linkInput | tool | executable | executableNote
  | preLinkCertificate | refusalCertificate | evidence | receipt
  deriving DecidableEq

/-- The digest of exact bytes in a specific domain. The canonical encoding and digest
verification are obligations of the future bounded reader. -/
structure Digest (_domain : DigestDomain) where
  bytes : ByteArray
  deriving DecidableEq

/-- Both backends use the same artifact envelope and canonical encoding. -/
inductive Backend where
  | llvm | lcb
  deriving DecidableEq

/-- Identity common to the pre-link certificate and the receipt. -/
structure BuildIdentity where
  source : Digest .source
  compiler : Digest .compiler
  spec : Digest .spec
  sourcePath : String

/-- A resolved tool pin and its actual effective argument vector. -/
structure PinnedInvocation where
  invocation : ToolInvocation
  binary : Digest .tool

/-- Evidence names fixed obligations instead of supplying its own proposition. -/
inductive Obligation where
  | frontend | bir | llvmPrint | lcbAdapter | lcbProfile
  | toolHandoffs | finalImage | refusal
  | external (premise : ExternalPremise)
  deriving DecidableEq

/-- An evidence reference for one obligation in the artifact's dependency graph. -/
inductive EdgeEvidence where
  | kernelChecked (evidence : Digest .evidence)
  | driverAttested
  | openPremise (name : String)

/-- A dependency node names its obligation, the evidence used at production, and the exact
nodes on which the obligation depends. Canonical readers will validate the graph and its IDs. -/
structure EvidenceNode where
  id : Nat
  obligation : Obligation
  evidence : EdgeEvidence
  dependencies : List Nat

/-- LLVM evidence refers to the full input module, including its runtime, never just the body. -/
structure LLVMInputs where
  llvmModule : Digest .llvmModule
  checkedCorrespondence : Digest .evidence
  objectFile : Digest .objectFile

/-- LCB evidence refers to the actual in-process input and profile job, plus the precise
runtime, site table, assembly and object inputs used by downstream tools. -/
structure LCBInputs where
  revision : String
  compilerIdentity : String
  function : Digest .lcbFunction
  job : Digest .lcbJob
  siteTable : Digest .siteTable
  runtimeInput : Digest .runtimeInput
  checkedAdapter : Digest .evidence
  checkedProfile : Digest .evidence
  bodyAssembly : Digest .assembly
  runtimeAssembly : Digest .assembly
  bodyObject : Digest .objectFile
  runtimeObject : Digest .objectFile

/-- Backend-specific checked and attested inputs in the common pre-link schema. -/
inductive BackendInputs where
  | llvm (inputs : LLVMInputs)
  | lcb (inputs : LCBInputs)

/-- Layer (i), the exact accepted build up to the link operation. The evidence identifies
local pass witnesses and the graph records checked and driver-attested handoffs. -/
structure PreLinkCertificate where
  schemaVersion : Nat
  identity : BuildIdentity
  profile : Profile
  loweringPath : LoweringPath
  bir : Digest .bir
  inputs : BackendInputs
  orderedLinkInputs : List (Digest .linkInput)
  tools : List PinnedInvocation
  evidenceGraph : List EvidenceNode

/-- A refused compilation's evidence binds the exact source and typed refusal. It has no
successful BIR or image attachment. -/
inductive RefusalStage where
  | shared
  | lcbPreparation (profile : Profile)
  | birSlotLimit (profile : Profile) (backend : Backend)

structure RefusalCertificate where
  schemaVersion : Nat
  identity : BuildIdentity
  stage : RefusalStage
  refusal : Refusal
  evidenceGraph : List EvidenceNode

/-- Layer (ii), the optional, non-loaded note. It can be embedded before final-image hashing
because it carries only immutable upstream identity and routing information. -/
structure ExecutableNote where
  schemaVersion : Nat
  certificate : Digest .preLinkCertificate
  source : Digest .source
  compiler : Digest .compiler
  spec : Digest .spec
  bir : Digest .bir
  backend : Backend
  target : Concrete.Backend.LCB.Target
  loweringPath : LoweringPath

/-- The two kernel claims differ in whether the proof includes the final-image stack bound. -/
inductive ImageClaim where
  | conditionalStack
  | provedStack

/-- The strength of the receipt's final-image correspondence. -/
inductive ImageBinding where
  | driverAttested
  | imageValidated (claim : ImageClaim) (evidence : Digest .evidence)

/-- A fact records its production status and the evidence nodes needed to re-evaluate current
acceptance when a checker or assumption is revoked. -/
inductive FactStatus where
  | established | conditional | refuted | notEstablished | notApplicable

inductive FactJudgment where
  | sourceCorrespondence | birCorrespondence | llvmModuleCorrespondence
  | lcbAdapterCorrespondence | lcbProfileValidation | buildHandoffs
  | imageCorrespondence

/-- A fact with its production status and its direct dependencies in the evidence graph. -/
structure Fact where
  judgment : FactJudgment
  productionStatus : FactStatus
  dependencies : List Nat

/-- An offline replay invocation in argument-vector form. -/
structure ReplayCommand where
  invocation : PinnedInvocation
  workingDirectory : String

/-- Layer (iii), the external receipt. The optional note and the final executable are hashed
as separate payloads. Upstream inputs and pins remain accessible through the pre-link certificate.
The receipt's own digest, if needed, belongs to its enclosing transport. -/
structure Receipt where
  schemaVersion : Nat
  executable : Digest .executable
  note : Option (Digest .executableNote)
  certificate : PreLinkCertificate
  certificateDigest : Digest .preLinkCertificate
  binding : ImageBinding
  evidenceGraph : List EvidenceNode
  facts : List Fact
  openPremises : List ExternalPremise
  executionTrust : List String
  replay : List ReplayCommand

/-- The canonical codec and evidence reader implement this interface in step 5.
The replay relation refers to evidence bytes for a particular kernel proposition. -/
structure Codec where
  version : Nat
  hash : (domain : DigestDomain) → ByteArray → Digest domain
  bir : Concrete.BIR.Program → ByteArray
  lcbFunction : LeanCompilerBackend.Verified.Source.Function → ByteArray
  lcbJob : LeanCompilerBackend.Verified.FunctionJob → ByteArray
  sites : Concrete.BIR.SiteTable → ByteArray
  certificate : PreLinkCertificate → ByteArray
  note : ExecutableNote → ByteArray
  refusal : RefusalCertificate → ByteArray
  evidence : Digest .evidence → Prop → Prop

/-- Build identity and binary resolution are supplied by the replay environment, which
must verify the exact compiler, spec and external tool binaries. -/
structure ReplayContext where
  compiler : ByteArray
  spec : ByteArray
  resolve : ToolInvocation → Option ByteArray
  commands : List ReplayCommand
  executionTrust : List String

inductive ExactBuild where
  | llvm (build : LLVMBuild)
  | lcb (build : LCBBuild)

def ExactBuild.backend : ExactBuild → Backend
  | .llvm _ => .llvm
  | .lcb _ => .lcb

def ExactBuild.path (target : Concrete.Backend.LCB.Target) : ExactBuild → LoweringPath
  | .llvm _ => .llvm
  | .lcb _ => lcbPath target

def ExactBuild.linkInputs : ExactBuild → List ByteArray
  | .llvm b => b.linkInputs
  | .lcb b => b.linkInputs

def ExactBuild.linkedImage : ExactBuild → ByteArray
  | .llvm b => b.image
  | .lcb b => b.image

def IdentityMatches (codec : Codec) (ctx : ReplayContext)
    (input : Stages.Input) (identity : BuildIdentity) : Prop :=
  identity.source = codec.hash .source input.source ∧ identity.sourcePath = input.path ∧
  identity.compiler = codec.hash .compiler ctx.compiler ∧ identity.spec = codec.hash .spec ctx.spec

def ToolPinsMatch (codec : Codec) (ctx : ReplayContext) (c : PreLinkCertificate) : Prop :=
  c.tools.map (·.invocation) = c.profile.tools ∧
  ∀ pin ∈ c.tools, ∃ bytes, ctx.resolve pin.invocation = some bytes ∧
    pin.binary = codec.hash .tool bytes

def InputsMatch (codec : Codec) : BackendInputs → ExactBuild → Prop
  | .llvm input, .llvm b =>
    input.llvmModule = codec.hash .llvmModule b.moduleBytes ∧
    input.objectFile = codec.hash .objectFile b.objectBytes
  | .lcb input, .lcb b =>
    input.revision = b.revision ∧ input.compilerIdentity = b.compilerIdentity ∧
    input.function = codec.hash .lcbFunction (codec.lcbFunction b.source) ∧
    input.job = codec.hash .lcbJob (codec.lcbJob (Concrete.Backend.LCB.job b.source)) ∧
    input.siteTable = codec.hash .siteTable (codec.sites b.sites) ∧
    input.runtimeInput = codec.hash .runtimeInput b.runtimeInput ∧
    input.bodyAssembly = codec.hash .assembly b.bodyAssembly ∧
    input.runtimeAssembly = codec.hash .assembly b.runtimeAssembly ∧
    input.bodyObject = codec.hash .objectFile b.bodyObject ∧
    input.runtimeObject = codec.hash .objectFile b.runtimeObject
  | _, _ => False

/-- IDs are global. Strictly backward references within the combined graph prove
acyclicity; upstream evidence can reference only other upstream nodes. -/
def OrderedGraph (graph : List EvidenceNode) : Prop :=
  (graph.map (·.id)).Nodup ∧ (graph.map (·.obligation)).Nodup ∧
  ∀ node ∈ graph, node.dependencies.Nodup ∧
    ∀ id ∈ node.dependencies, id < node.id ∧ ∃ previous ∈ graph, previous.id = id

/-- Resolve dependency names to the unique graph IDs. The exact prerequisite set is
part of the obligation, so a receipt cannot omit a dependency to survive revocation. -/
def HasDependencies (graph : List EvidenceNode) (goal : Obligation)
    (required : List Obligation) : Prop :=
  ∀ node ∈ graph, node.obligation = goal →
    ∀ prior ∈ graph, prior.id ∈ node.dependencies ↔ prior.obligation ∈ required

def Checked (codec : Codec) (d : Digest .evidence) (p : Prop) : Prop :=
  p ∧ codec.evidence d p

def HasEvidence (graph : List EvidenceNode) (goal : Obligation) (evidence : EdgeEvidence) : Prop :=
  ∃ node ∈ graph, node.obligation = goal ∧ node.evidence = evidence

def Replays (codec : Codec) (graph : List EvidenceNode) (goal : Obligation) (p : Prop) : Prop :=
  ∃ d, HasEvidence graph goal (.kernelChecked d) ∧ Checked codec d p

def BackendEvidence (codec : Codec) (bir : Concrete.BIR.Program) (c : PreLinkCertificate) :
    ExactBuild → Prop
  | .llvm b => ∃ i, c.inputs = .llvm i ∧
    HasEvidence c.evidenceGraph .llvmPrint (.kernelChecked i.checkedCorrespondence) ∧
    Checked codec i.checkedCorrespondence (b.moduleBytes = (Concrete.Backend.LLVM.module bir).toUTF8)
  | .lcb b => ∃ i, c.inputs = .lcb i ∧
    HasEvidence c.evidenceGraph .lcbAdapter (.kernelChecked i.checkedAdapter) ∧
    Checked codec i.checkedAdapter (Concrete.Backend.LCB.emit bir = .ok (b.source, b.sites)) ∧
    HasEvidence c.evidenceGraph .lcbProfile (.kernelChecked i.checkedProfile) ∧
    Checked codec i.checkedProfile
      (LeanCompilerBackend.Verified.validateJob (Concrete.Backend.LCB.job b.source) = .ok () ∧
        b.runtimeInput = (Concrete.Backend.Runtime.qbe b.sites).toUTF8)

theorem BackendEvidence.lcbRuntime {codec : Codec} {bir : Concrete.BIR.Program}
    {c : PreLinkCertificate} {b : LCBBuild} (h : BackendEvidence codec bir c (.lcb b)) :
    b.runtimeInput = (Concrete.Backend.Runtime.qbe b.sites).toUTF8 := by
  obtain ⟨_, _, _, _, _, ⟨⟨_, exactRuntime⟩, _⟩⟩ := h
  exact exactRuntime

def PreLinkValid (codec : Codec) (ctx : ReplayContext) (tools : NativeTools)
    (input : Stages.Input) (out : Stages.Output) (build : ExactBuild)
    (c : PreLinkCertificate) : Prop :=
  c.schemaVersion = codec.version ∧ IdentityMatches codec ctx input c.identity ∧
  c.bir = codec.hash .bir (codec.bir out.bir) ∧ InputsMatch codec c.inputs build ∧
  c.loweringPath = build.path c.profile.target ∧ ToolPinsMatch codec ctx c ∧
  c.orderedLinkInputs = build.linkInputs.map (codec.hash .linkInput) ∧
  OrderedGraph c.evidenceGraph ∧
  Replays codec c.evidenceGraph .frontend (Frontend input out) ∧
  Replays codec c.evidenceGraph .bir
    (SharedHandoffs input out ∧ BIRSlotsFit c.profile out.bir ∧ Concrete.BIR.WF out.bir ∧
      SiteOrigins input.path out.core out.memory out.bir) ∧
  BackendEvidence codec out.bir c build ∧
  HasEvidence c.evidenceGraph .toolHandoffs .driverAttested ∧
  HasDependencies c.evidenceGraph .frontend [] ∧
  HasDependencies c.evidenceGraph .bir [.frontend] ∧
  HasDependencies c.evidenceGraph .llvmPrint [.bir] ∧
  HasDependencies c.evidenceGraph .lcbAdapter [.bir] ∧
  HasDependencies c.evidenceGraph .lcbProfile [.lcbAdapter] ∧
  HasDependencies c.evidenceGraph .toolHandoffs
    (match build with | .llvm _ => [.llvmPrint] | .lcb _ => [.lcbAdapter, .lcbProfile]) ∧
  (match build with
    | .llvm b => LLVMHandoffs c.profile tools out.bir b
    | .lcb b => LCBHandoffs c.profile tools out.bir b) ∧
  (∀ node ∈ c.evidenceGraph, node.obligation ∈
    [.frontend, .bir, .toolHandoffs] ++
      (match build with | .llvm _ => [.llvmPrint] | .lcb _ => [.lcbAdapter, .lcbProfile]))

/-- The slot-limit branch states the check required before either backend emits an artifact. -/
def RefusalAt (input : Stages.Input) (stage : RefusalStage) (refusal : Refusal) : Prop :=
  match stage with
  | .shared => Stages.compile input = .error refusal
  | .lcbPreparation profile => ∃ out, Stages.compile input = .ok out ∧
    BIRSlotsFit profile out.bir ∧ Stages.prepareLCB out.bir = .error refusal
  | .birSlotLimit profile backend => ∃ out, Stages.compile input = .ok out ∧
    ¬ BIRSlotsFit profile out.bir ∧
    refusal = .backendResource (match backend with | .llvm => "LLVM" | .lcb => "LCB")
      "BIR slots" out.bir.frameSlots profile.maxBIRSlots

/-- A refusal requires replay of the exact failed invocation or rejected resource check. -/
def RefusalValid (codec : Codec) (ctx : ReplayContext)
    (input : Stages.Input) (c : RefusalCertificate) : Prop :=
  c.schemaVersion = codec.version ∧ IdentityMatches codec ctx input c.identity ∧
  OrderedGraph c.evidenceGraph ∧
  Replays codec c.evidenceGraph .refusal (RefusalAt input c.stage c.refusal) ∧
  ∀ node ∈ c.evidenceGraph, node.obligation = .refusal

def ExpectedNote (codec : Codec) (c : PreLinkCertificate) (build : ExactBuild) : ExecutableNote :=
  ⟨codec.version, codec.hash .preLinkCertificate (codec.certificate c),
   c.identity.source, c.identity.compiler, c.identity.spec, c.bir,
   build.backend, c.profile.target, c.loweringPath⟩

/-- Parse the actual note and validate insertion after link. Insertion must leave the
loaded image unchanged, including entry state and relocations. The link inputs and
pre-link digest precede insertion, so the note cannot depend on itself. -/
structure NoteFormat where
  read : ByteArray → Option ByteArray → Prop
  insertNonLoaded : ByteArray → ByteArray → ByteArray → Prop

def NoteMatches (codec : Codec) (format : NoteFormat) (c : PreLinkCertificate)
    (build : ExactBuild) (exe : ByteArray) (digest : Option (Digest .executableNote)) : Prop :=
  match digest with
  | none => exe = build.linkedImage ∧ format.read exe none
  | some d =>
    let bytes := codec.note (ExpectedNote codec c build)
    d = codec.hash .executableNote bytes ∧ format.read exe (some bytes) ∧
    format.insertNonLoaded build.linkedImage bytes exe

/-- Revocation is recursive in the graph, independently of the production status. -/
inductive StillAccepted (revoked : Obligation → Prop) (graph : List EvidenceNode) :
    Nat → Prop where
  | node {n : EvidenceNode} :
      n ∈ graph → ¬ revoked n.obligation →
      (∀ id ∈ n.dependencies, StillAccepted revoked graph id) → StillAccepted revoked graph n.id

def Fact.currentlyAccepted (fact : Fact) (revoked : Obligation → Prop)
    (graph : List EvidenceNode) : Prop :=
  (fact.productionStatus = .established ∨ fact.productionStatus = .conditional) ∧
  ∀ id ∈ fact.dependencies, StillAccepted revoked graph id

/-- The catalog is the projection of the validated obligations. Each fact depends on
its own node, so revocation also follows that node's transitive dependencies. -/
def factsFromEvidence (graph : List EvidenceNode) : List Fact :=
  graph.filterMap fun node =>
    let fact judgment status := some ⟨judgment, status, [node.id]⟩
    match node.obligation with
    | .frontend => fact .sourceCorrespondence .established
    | .bir => fact .birCorrespondence .established
    | .llvmPrint => fact .llvmModuleCorrespondence .established
    | .lcbAdapter => fact .lcbAdapterCorrespondence .established
    | .lcbProfile => fact .lcbProfileValidation .established
    | .toolHandoffs => fact .buildHandoffs .conditional
    | .finalImage => fact .imageCorrespondence .conditional
    | .external _ | .refusal => none

def environmentPremises : List ExternalPremise := [.launch, .osLiveness, .stderrProgress]

def ImageClaim.openPremises : ImageClaim → List ExternalPremise
  | .conditionalStack => .imageStack :: environmentPremises
  | .provedStack => environmentPremises

def ImageBinding.openPremises (path : LoweringPath) : ImageBinding → List ExternalPremise
  | .driverAttested => path.premises
  | .imageValidated claim _ => claim.openPremises

def ImageDependencies (path : LoweringPath) (binding : ImageBinding) : List Obligation :=
  (match binding with
   | .driverAttested => [.toolHandoffs]
   | .imageValidated .. => [.frontend]) ++ (binding.openPremises path).map Obligation.external

/-- Replay selects one of two fixed propositions. The checked claim supplies the
dependency list, which is never inferred from the receipt's declared premises. -/
private def ImageClaim.proposition {Env : Type} {profile : Profile}
    {reference : TargetReference Env profile.target} (claim : ImageClaim)
    (n : NativeModels Env profile reference) (path : String) (core : Elab.Output)
    (exe : ByteArray) : Prop :=
  (∃ decoded, reference.decode exe = some decoded) ∧
  (∃ env, RunReady n exe env) ∧ ImageCorrect n admittedCoreSemantics path core exe ∧
  match claim with
  | .conditionalStack => True
  | .provedStack => ∀ env, n.launchAdmitted exe env →
      n.stackAdequate exe env profile.maxImageStackBytes

/-- Note insertion preserves decoding, launch admission independently of stack
adequacy, and the verifier's readiness and behavior guarantees. -/
private structure ImagePreserved {Env : Type} {profile : Profile}
    {platform : TargetReference Env profile.target} (n : NativeModels Env profile platform)
    (build : ExactBuild) (exe : ByteArray) : Prop where
  decoding : platform.decode exe = platform.decode build.linkedImage
  admission : ∀ env, n.launchAdmitted build.linkedImage env ↔ n.launchAdmitted exe env
  launchReady : ∀ env, LaunchReady n build.linkedImage env ↔ LaunchReady n exe env
  runReady : ∀ env, RunReady n build.linkedImage env ↔ RunReady n exe env
  prefixes : ∀ env, LaunchReady n exe env →
    Contracts.PrefixSafety n.process n.process (build.linkedImage, env) (exe, env)
  maximal : ∀ env, RunReady n exe env →
    Contracts.MaximalRefinement n.process n.process (build.linkedImage, env) (exe, env) ∧
    ((∃ o, n.process.observes (build.linkedImage, env) o) →
      ∃ o, n.process.observes (exe, env) o)

private def ImageReplay {Env : Type} {profile : Profile}
    {platform : TargetReference Env profile.target} (policy : ExecutionPolicy Env)
    (codec : Codec) (input : Stages.Input) (out : Stages.Output) (build : ExactBuild)
    (exe : ByteArray) (binding : ImageBinding) (n : NativeModels Env profile platform) : Prop :=
  policy.Anchors n ∧ ImagePreserved n build exe ∧
  match binding with
  | .driverAttested => True
  | .imageValidated claim d =>
    Checked codec d (claim.proposition n input.path out.core exe)

private def ReceiptChecks (codec : Codec) (ctx : ReplayContext) (format : NoteFormat)
    (tools : NativeTools) (input : Stages.Input) (out : Stages.Output) (build : ExactBuild)
    (exe : ByteArray) (r : Receipt) : Prop :=
  r.schemaVersion = codec.version ∧ PreLinkValid codec ctx tools input out build r.certificate ∧
  r.executable = codec.hash .executable exe ∧
  r.certificateDigest = codec.hash .preLinkCertificate (codec.certificate r.certificate) ∧
  NoteMatches codec format r.certificate build exe r.note ∧
  OrderedGraph (r.certificate.evidenceGraph ++ r.evidenceGraph) ∧
  r.replay = ctx.commands ∧ r.executionTrust = ctx.executionTrust ∧
  r.facts = factsFromEvidence (r.certificate.evidenceGraph ++ r.evidenceGraph) ∧
  r.openPremises = r.binding.openPremises r.certificate.loweringPath ∧
  (∀ p, p ∈ r.openPremises ↔ HasEvidence r.evidenceGraph (.external p) (.openPremise p.name)) ∧
  (∀ node ∈ r.evidenceGraph, node.obligation = .finalImage ∨
    ∃ p ∈ r.openPremises, node.obligation = .external p) ∧
  (∀ p, HasDependencies (r.certificate.evidenceGraph ++ r.evidenceGraph) (.external p) []) ∧
  HasDependencies (r.certificate.evidenceGraph ++ r.evidenceGraph) .finalImage
    (ImageDependencies r.certificate.loweringPath r.binding) ∧
  match r.binding with
  | .driverAttested => HasEvidence r.evidenceGraph .finalImage .driverAttested
  | .imageValidated _ d =>
    HasEvidence r.evidenceGraph .finalImage (.kernelChecked d)

private def ReceiptValidWith
    (reference : NativeReferenceRegistry)
    (codec : Codec) (ctx : ReplayContext) (format : NoteFormat)
    (tools : NativeTools) (input : Stages.Input) (out : Stages.Output) (build : ExactBuild)
    (exe : ByteArray) (r : Receipt)
    (policy : (registered : (reference r.certificate.profile.target).isSome) →
      ExecutionPolicy ((reference r.certificate.profile.target).get registered).1) : Prop :=
  ReceiptChecks codec ctx format tools input out build exe r ∧
  match r.binding, r.note with
  | .driverAttested, none => True
  | _, _ => ∃ registered : (reference r.certificate.profile.target).isSome,
    ∃ n : NativeModels _ r.certificate.profile
        ((reference r.certificate.profile.target).get registered).2,
      ImageReplay (policy registered) codec input out build exe r.binding n

/-- Replay chooses the native semantics from the certificate's ISA/OS target.
An unmodified driver-attested image requires only the structural checks. A note
or a semantic image claim requires a registered target and semantic evidence. -/
def ReceiptValid (codec : Codec) (ctx : ReplayContext) (format : NoteFormat)
    (tools : NativeTools) (input : Stages.Input) (out : Stages.Output) (build : ExactBuild)
    (exe : ByteArray) (r : Receipt)
    (policy : (registered : (nativeReference r.certificate.profile.target).isSome) →
      ExecutionPolicy ((nativeReference r.certificate.profile.target).get registered).1) : Prop :=
  ReceiptValidWith nativeReference codec ctx format tools input out build exe r policy


example : OrderedGraph [⟨0, .frontend, .driverAttested, []⟩,
    ⟨1, .bir, .driverAttested, [0]⟩] := by
  refine ⟨by decide, by decide, ?_⟩
  intro node membership
  simp only [List.mem_cons, List.not_mem_nil, or_false] at membership
  rcases membership with rfl | rfl
  · exact ⟨List.nodup_nil, fun _ h => by cases h⟩
  · refine ⟨by decide, ?_⟩
    intro id member
    have h : id = 0 := List.mem_singleton.mp member
    subst id
    exact ⟨by decide, _, List.Mem.head _, rfl⟩

example : ¬ OrderedGraph [⟨0, .frontend, .driverAttested, [0]⟩] := by
  intro h
  have cycle := (h.2.2 _ (List.Mem.head _)).2 0 (List.Mem.head _)
  exact Nat.lt_irrefl _ cycle.1

example (codec : Codec) (ctx : ReplayContext) (input : Stages.Input) (c : RefusalCertificate)
    (empty : c.evidenceGraph = []) : ¬ RefusalValid codec ctx input c := by
  rintro ⟨_, _, _, ⟨_, ⟨node, member, _, _⟩, _⟩, _⟩
  rw [empty] at member
  exact List.not_mem_nil member

section Sanity

private def revocationGraph : List EvidenceNode :=
  [⟨0, .frontend, .kernelChecked ⟨ByteArray.empty⟩, []⟩,
   ⟨1, .external .linker, .openPremise "linker", []⟩,
   ⟨2, .finalImage, .driverAttested, [1]⟩]

private def linkerRevoked (goal : Obligation) : Prop := goal = .external .linker

example : factsFromEvidence revocationGraph =
    [⟨.sourceCorrespondence, .established, [0]⟩,
     ⟨.imageCorrespondence, .conditional, [2]⟩] := rfl

example : StillAccepted linkerRevoked revocationGraph 0 := by
  apply StillAccepted.node (n := ⟨0, .frontend, .kernelChecked ⟨ByteArray.empty⟩, []⟩)
  · exact List.Mem.head _
  · intro h
    cases h
  · intro _ h
    cases h

private theorem acceptedNode {revoked graph id} (h : StillAccepted revoked graph id) :
    ∃ node ∈ graph, node.id = id ∧ ¬ revoked node.obligation ∧
      ∀ dep ∈ node.dependencies, StillAccepted revoked graph dep := by
  cases h with
  | node member ok deps => exact ⟨_, member, rfl, ok, deps⟩

example : ¬ StillAccepted linkerRevoked revocationGraph 2 := by
  intro h
  obtain ⟨node, member, id, _, deps⟩ := acceptedNode h
  simp only [revocationGraph, List.mem_cons, List.not_mem_nil, or_false] at member
  rcases member with rfl | rfl | rfl
  · cases id
  · cases id
  · obtain ⟨node, member, id, ok, _⟩ := acceptedNode (deps 1 (List.Mem.head _))
    simp only [revocationGraph, List.mem_cons, List.not_mem_nil, or_false] at member
    rcases member with rfl | rfl | rfl
    · cases id
    · exact ok rfl
    · cases id

example (graph : List EvidenceNode) (fact : Fact) (h : fact ∈ factsFromEvidence graph) :
    fact.dependencies ≠ [] := by
  unfold factsFromEvidence at h
  obtain ⟨node, _, found⟩ := List.mem_filterMap.mp h
  dsimp at found
  split at found <;> cases found <;> simp

example : (.external .imageStack : Obligation) ∈
    ImageDependencies .llvm (.imageValidated .conditionalStack ⟨ByteArray.empty⟩) := by decide

example : (.external .imageStack : Obligation) ∉
    ImageDependencies .llvm (.imageValidated .provedStack ⟨ByteArray.empty⟩) := by decide

example : .toolHandoffs ∉
    ImageDependencies .llvm (.imageValidated .provedStack ⟨ByteArray.empty⟩) := by decide

example : RefusalAt ImageTheorem.Sanity.answerInput
    (.birSlotLimit { ImageTheorem.Sanity.sampleProfile with maxBIRSlots := 0 } .llvm)
    (.backendResource "LLVM" "BIR slots" ImageTheorem.Sanity.answerOutput.bir.frameSlots 0) := by
  refine ⟨_, ImageTheorem.Sanity.answerCompiles, ?_, rfl⟩
  with_unfolding_all
    change ¬ 1 ≤ 0
    decide

open ImageTheorem.Sanity

example {codec : Codec} {bir : Concrete.BIR.Program} {c : PreLinkCertificate}
    {b : LCBBuild} (substitution : b.runtimeInput ≠ (Concrete.Backend.Runtime.qbe b.sites).toUTF8) :
    ¬ BackendEvidence codec bir c (.lcb b) :=
  fun evidence => substitution evidence.lcbRuntime

private def testEvidence : Digest .evidence := ⟨ByteArray.empty⟩

private def testCodec : Codec where
  version := 1
  hash _ bytes := ⟨bytes⟩
  bir p := (reprStr p).toUTF8
  lcbFunction _ := ByteArray.empty
  lcbJob _ := ByteArray.empty
  sites _ := ByteArray.empty
  certificate _ := ByteArray.empty
  note _ := "test note".toUTF8
  refusal _ := ByteArray.empty
  evidence _ _ := True

private def testContext : ReplayContext :=
  ⟨"test compiler".toUTF8, "test spec".toUTF8, fun pin => some pin.tool.toUTF8, [], []⟩

private def testBuild (image : ByteArray) : ExactBuild := .llvm (testLLVMBuild answerOutput.bir image)

private def testPreLink (image : ByteArray) : PreLinkCertificate :=
  let b := testLLVMBuild answerOutput.bir image
  ⟨1, ⟨⟨answerInput.source⟩, ⟨testContext.compiler⟩, ⟨testContext.spec⟩, answerInput.path⟩,
    sampleProfile, .llvm, ⟨testCodec.bir answerOutput.bir⟩,
    .llvm ⟨⟨b.moduleBytes⟩, testEvidence, ⟨b.objectBytes⟩⟩,
    b.linkInputs.map (testCodec.hash .linkInput),
    sampleProfile.tools.map (fun pin => ⟨pin, ⟨pin.tool.toUTF8⟩⟩),
    [⟨0, .frontend, .kernelChecked testEvidence, []⟩,
     ⟨1, .bir, .kernelChecked testEvidence, [0]⟩,
     ⟨2, .llvmPrint, .kernelChecked testEvidence, [1]⟩,
     ⟨3, .toolHandoffs, .driverAttested, [2]⟩]⟩

private theorem testPreLinkValid (image : ByteArray) : PreLinkValid testCodec testContext testTools
    answerInput answerOutput (testBuild image) (testPreLink image) := by
  with_unfolding_all
    refine ⟨rfl, ⟨rfl, rfl, rfl, rfl⟩, rfl, ⟨rfl, rfl⟩, rfl, ⟨rfl, ?_⟩, rfl,
      ?_, ⟨testEvidence, ⟨_, List.Mem.head _, rfl, rfl⟩, answerFrontend, trivial⟩,
      ⟨testEvidence, ⟨_, List.Mem.tail _ (List.Mem.head _), rfl, rfl⟩,
        ⟨answerHandoffs, ?_, SSA.lower_wf answerHandoffs.2.2.2.2.2, answerOrigins⟩, trivial⟩,
      ⟨_, rfl, ⟨_, List.Mem.tail _ (List.Mem.tail _ (List.Mem.head _)), rfl, rfl⟩, rfl, trivial⟩,
      ⟨_, List.Mem.tail _ (List.Mem.tail _ (List.Mem.tail _ (List.Mem.head _))), rfl, rfl⟩,
      ?_, ?_, ?_, ?_, ?_, ?_, testLLVMHandoffs _ _, ?_⟩
    · intro pin member
      obtain ⟨invocation, _, rfl⟩ := List.mem_map.mp member
      exact ⟨invocation.tool.toUTF8, rfl, rfl⟩
    · simp [OrderedGraph, testPreLink]
    · change 1 ≤ 64
      decide
    all_goals simp [HasDependencies, testPreLink, testBuild]

private def testNoteFormat (linked : ByteArray) : NoteFormat where
  read exe note := exe = (note.map (linked ++ ·)).getD linked
  insertNonLoaded linked note exe := exe = linked ++ note

example : NoteMatches testCodec (testNoteFormat answerImage) (testPreLink answerImage) (testBuild answerImage)
    (answerImage ++ "test note".toUTF8) (some ⟨"test note".toUTF8⟩) :=
  ⟨rfl, rfl, rfl⟩

example : ¬ NoteMatches testCodec (testNoteFormat answerImage) (testPreLink answerImage) (testBuild answerImage)
    (answerImage ++ "test note".toUTF8) (some ⟨"substituted note".toUTF8⟩) := by
  intro h
  have bytes := congrArg Digest.bytes h.1
  exact (by decide : "substituted note".toUTF8 ≠ "test note".toUTF8) bytes

private def testImageGraph : List EvidenceNode :=
  [⟨4, .external .launch, .openPremise ExternalPremise.launch.name, []⟩,
   ⟨5, .external .osLiveness, .openPremise ExternalPremise.osLiveness.name, []⟩,
   ⟨6, .external .stderrProgress, .openPremise ExternalPremise.stderrProgress.name, []⟩,
   ⟨7, .finalImage, .kernelChecked testEvidence, [0, 4, 5, 6]⟩]

private def testReceipt (image : ByteArray) : Receipt :=
  let preLink := testPreLink image
  ⟨1, ⟨image⟩, none, preLink, ⟨testCodec.certificate preLink⟩,
    .imageValidated .provedStack testEvidence, testImageGraph, factsFromEvidence (preLink.evidenceGraph ++ testImageGraph),
    environmentPremises, [], []⟩

private theorem testImageReplay (binding : ImageBinding) :
    ImageReplay testExecutionPolicy testCodec answerInput answerOutput (testBuild answerImage) answerImage
      binding testNative := by
  with_unfolding_all
    have ready : ImagePreserved testNative (testBuild answerImage) answerImage :=
      ⟨rfl, fun _ => Iff.rfl, fun _ => Iff.rfl, fun _ => Iff.rfl,
        fun env _ => (instantRefines _ _ (answerImage, env) (answerImage, env) rfl).1,
        fun env _ => ⟨(instantRefines _ _ (answerImage, env) (answerImage, env) rfl).2, id⟩⟩
    refine ⟨testAnchor, ready, ?_⟩
    cases binding with
    | driverAttested => trivial
    | imageValidated claim d =>
      refine ⟨⟨⟨_, rfl⟩, ⟨(), ⟨trivial, trivial⟩, trivial, trivial⟩,
        ⟨fun _ _ => answerImageRefines.1, fun _ _ => answerImageRefines.2,
          fun _ env _ => ⟨exit42, instantObserves _ (answerImage, env)⟩⟩, ?_⟩, trivial⟩
      cases claim with
      | conditionalStack => trivial
      | provedStack => exact fun _ _ => trivial

private theorem testReceiptChecks (image : ByteArray) :
    ReceiptChecks testCodec testContext (testNoteFormat image) testTools
      answerInput answerOutput (testBuild image) image (testReceipt image) := by
  with_unfolding_all
    refine ⟨rfl, testPreLinkValid image, rfl, rfl, ⟨rfl, rfl⟩, ?_, rfl, rfl, rfl, rfl, ?_, ?_, ?_, ?_, ?_⟩
    · simp [OrderedGraph, testReceipt, testImageGraph, testPreLink]
    · intro p
      cases p <;> simp [testReceipt, testImageGraph, environmentPremises, HasEvidence]
    · simp [testReceipt, testImageGraph, environmentPremises]
    · simp [testReceipt, testImageGraph, testPreLink, HasDependencies]
    · simp [testReceipt, testImageGraph, testPreLink, HasDependencies, ImageDependencies,
        ImageBinding.openPremises, ImageClaim.openPremises, environmentPremises]
    · exact ⟨_, List.Mem.tail _ (List.Mem.tail _ (List.Mem.tail _ (List.Mem.head _))), rfl, rfl⟩

private def conditionalGraph (deps : List Nat) : List EvidenceNode :=
  testImageGraph.take 3 ++
    [⟨7, .external .imageStack, .openPremise ExternalPremise.imageStack.name, []⟩,
     ⟨8, .finalImage, .kernelChecked testEvidence, deps⟩]

private def conditionalReceipt (deps : List Nat) : Receipt :=
  { testReceipt answerImage with
    binding := .imageValidated .conditionalStack testEvidence
    evidenceGraph := conditionalGraph deps
    facts := factsFromEvidence ((testPreLink answerImage).evidenceGraph ++ conditionalGraph deps)
    openPremises := .imageStack :: environmentPremises }

private def conditionalReceiptValid (deps : List Nat) : Prop :=
  ReceiptValidWith testRegistry testCodec testContext (testNoteFormat answerImage) testTools
    answerInput answerOutput (testBuild answerImage) answerImage (conditionalReceipt deps)
    (fun _ => testExecutionPolicy)

example : conditionalReceiptValid [0, 4, 5, 6, 7] := by
  refine ⟨?_, ⟨rfl, testNative, testImageReplay _⟩⟩
  with_unfolding_all
    refine ⟨rfl, testPreLinkValid answerImage, rfl, rfl, ⟨rfl, rfl⟩, ?_, rfl, rfl, rfl, rfl, ?_, ?_, ?_, ?_, ?_⟩
    · simp [OrderedGraph, conditionalReceipt, conditionalGraph, testImageGraph, testReceipt, testPreLink]
    · intro p
      cases p <;> simp [conditionalReceipt, conditionalGraph, testImageGraph, environmentPremises, HasEvidence]
    · simp [conditionalReceipt, conditionalGraph, testImageGraph, environmentPremises]
    · simp [conditionalReceipt, conditionalGraph, testImageGraph, testReceipt, testPreLink, HasDependencies]
    · simp [conditionalReceipt, conditionalGraph, testImageGraph, testReceipt, testPreLink, HasDependencies,
        ImageDependencies, ImageBinding.openPremises, ImageClaim.openPremises, environmentPremises]
    · simp [conditionalReceipt, conditionalGraph, testImageGraph, HasEvidence]


/-- The stack premise is present and the only omitted dependency is its edge. -/
example : ¬ conditionalReceiptValid [0, 4, 5, 6] := by
  intro valid
  obtain ⟨_, _, _, _, _, _, _, _, _, _, _, _, _, deps, _⟩ := valid.1
  have missing : 7 ∈ ([0, 4, 5, 6] : List Nat) :=
    (deps ⟨8, .finalImage, .kernelChecked testEvidence, [0, 4, 5, 6]⟩ (by simp [conditionalReceipt, conditionalGraph, testImageGraph]) rfl
      ⟨7, .external .imageStack, .openPremise ExternalPremise.imageStack.name, []⟩ (by
        simp [conditionalReceipt, conditionalGraph, testImageGraph])).mpr
      (by decide)
  exact (by decide : 7 ∉ ([0, 4, 5, 6] : List Nat)) missing

private def noteBytes := answerImage ++ "test note".toUTF8

private def changedByNote : TargetReference Unit sampleProfile.target :=
  {testReference with
    decode := fun bytes => some ⟨#[bytes.data[0]?.getD 0]⟩
    process := instant fun (bytes, _) =>
      .returned (if bytes.size == 1 then 42 else 1) ByteArray.empty }

private def noteModels (platform : TargetReference Unit sampleProfile.target) :
    NativeModels Unit sampleProfile platform where
  llvmModule := testNative.llvmModule
  llvmWellFormed := testNative.llvmWellFormed
  lcbBodyAssembly := testNative.lcbBodyAssembly
  lcbAssembly := testNative.lcbAssembly
  objectOperands := testNative.objectOperands
  objects := testNative.objects
  linked := testNative.linked
  loaded := testNative.loaded
  runtime := testNative.runtime
  stackAdequate := testNative.stackAdequate
  launchAdmitted := testNative.launchAdmitted
  osLive := testNative.osLive
  stderrAndExitAvailable := testNative.stderrAndExitAvailable

/-- This note leaves decoding unchanged but changes the process's exit status. -/
example : changedByNote.decode noteBytes = changedByNote.decode answerImage ∧
    ¬ ImageReplay testExecutionPolicy testCodec answerInput answerOutput (testBuild answerImage) noteBytes
      .driverAttested (noteModels changedByNote) := by
  refine ⟨rfl, ?_⟩
  intro replay
  have ready : RunReady (noteModels changedByNote) noteBytes () := ⟨⟨trivial, trivial⟩, trivial, trivial⟩
  have refines := (replay.2.1.maximal () ready).1
  have observed : changedByNote.process.observes (noteBytes, ()) (.returned 1 ByteArray.empty) :=
    instantObserves _ _
  obtain ⟨state, finite, returned⟩ := observed
  obtain ⟨_, _, linkedResult, _⟩ := refines.1 state _ finite returned
  have conflict : (42 : UInt8) = 1 := (Contracts.Observation.returned.inj linkedResult).1
  exact (by decide : (42 : UInt8) ≠ 1) conflict

/-- Suppressing launch admission in the model cannot make note replay vacuous. -/
example : ¬ ImageReplay testExecutionPolicy testCodec answerInput answerOutput (testBuild answerImage) noteBytes
    .driverAttested (profile := sampleProfile) (platform := changedByNote)
    {noteModels changedByNote with launchAdmitted := fun _ _ => False} := by
  intro replay
  have conflict : False = True := congrFun (congrFun replay.1.2.1 noteBytes) ()
  exact Eq.mpr conflict trivial

private def unchangedByNote : TargetReference Unit sampleProfile.target :=
  { changedByNote with process := testReference.process }

/-- With a process that ignores the note, the same insertion preserves execution. -/
example : ImageReplay testExecutionPolicy testCodec answerInput answerOutput (testBuild answerImage)
    noteBytes .driverAttested (noteModels unchangedByNote) := by
  refine ⟨⟨rfl, rfl, rfl, rfl⟩, ⟨rfl, fun _ => Iff.rfl, fun _ => Iff.rfl, fun _ => Iff.rfl, ?_, ?_⟩, trivial⟩
  · intro env _
    exact (instantRefines _ _ (answerImage, env) (noteBytes, env) rfl).1
  · intro env _
    exact ⟨(instantRefines _ _ (answerImage, env) (noteBytes, env) rfl).2,
      fun _ => ⟨_, instantObserves _ (noteBytes, env)⟩⟩

private def linkedOnlyPolicy : ExecutionPolicy Unit :=
  { testExecutionPolicy with launchAdmitted := fun _ bytes _ => bytes = answerImage }

private def linkedOnlyModels : NativeModels Unit sampleProfile changedByNote :=
  { noteModels changedByNote with launchAdmitted := fun bytes _ => bytes = answerImage }

/-- The same verifier policy admits the original bytes and refuses the noted bytes. -/
example : RunReady linkedOnlyModels answerImage () ∧
    ¬ ImageReplay linkedOnlyPolicy testCodec answerInput answerOutput (testBuild answerImage) noteBytes
      .driverAttested linkedOnlyModels := by
  have linkedReady : RunReady linkedOnlyModels answerImage () :=
    ⟨⟨trivial, rfl⟩, trivial, trivial⟩
  refine ⟨linkedReady, ?_⟩
  intro replay
  have finalReady := (replay.2.1.runReady ()).mp linkedReady
  exact (by decide : noteBytes ≠ answerImage) finalReady.1.2

private def unestablishedStackPolicy : ExecutionPolicy Unit :=
  { linkedOnlyPolicy with stackAdequate := fun _ _ _ _ => False }

private def unestablishedStackModels : NativeModels Unit sampleProfile changedByNote :=
  { linkedOnlyModels with stackAdequate := fun _ _ _ => False }

/-- With stack adequacy open on both images, both readiness implications are
vacuous. Replay still rejects the exit-changing note on admission alone. -/
example :
    unestablishedStackPolicy.launchAdmitted sampleProfile answerImage () ∧
    ¬ unestablishedStackPolicy.launchAdmitted sampleProfile noteBytes () ∧
    (∀ image env, ¬ LaunchReady unestablishedStackModels image env) ∧
    (∀ image env, ¬ RunReady unestablishedStackModels image env) ∧
    ¬ ImageReplay unestablishedStackPolicy testCodec answerInput answerOutput (testBuild answerImage)
      noteBytes .driverAttested unestablishedStackModels := by
  have refuses : noteBytes ≠ answerImage := by decide
  refine ⟨rfl, refuses, fun _ _ ready => ready.1, fun _ _ ready => ready.1.1, ?_⟩
  intro replay
  exact refuses ((replay.2.1.admission ()).mp rfl)

private def wrongImage : ByteArray := ⟨#[43]⟩

private def testReceiptValid (image : ByteArray) : Prop :=
  ReceiptValidWith testRegistry testCodec testContext (testNoteFormat image) testTools
    answerInput answerOutput (testBuild image) image (testReceipt image)
    (fun _ => testExecutionPolicy)

example : testReceiptValid answerImage :=
  ⟨testReceiptChecks _, ⟨rfl, testNative, testImageReplay _⟩⟩

/-- The byte-43 receipt passes structural validation and selects the same
reference as byte 42. Only semantic replay rejects its exit status. -/
example : ReceiptChecks testCodec testContext (testNoteFormat wrongImage) testTools
    answerInput answerOutput (testBuild wrongImage) wrongImage (testReceipt wrongImage) ∧
    ¬ testReceiptValid wrongImage := by
  refine ⟨testReceiptChecks _, ?_⟩
  rintro ⟨_, registered, n, _, _, claim, _⟩
  obtain ⟨_, ⟨env, ready⟩, correct, _⟩ := claim
  have observed : n.process.observes (wrongImage, env) (.returned 43 ByteArray.empty) :=
    instantObserves _ _
  obtain ⟨state, reachable, result⟩ := observed
  obtain ⟨source, finite, returned, _⟩ := (correct.2.1 env ready).1 state _ reachable result
  with_unfolding_all exact answerDoesNotReturn43 ⟨source, finite, returned⟩

end Sanity

end Concrete.Certificate
