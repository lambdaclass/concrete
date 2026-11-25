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
structure RefusalCertificate where
  schemaVersion : Nat
  identity : BuildIdentity
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

/-- The strength of the receipt's final-image correspondence. -/
inductive ImageBinding where
  | driverAttested
  | imageValidated (evidence : Digest .evidence)

/-- A fact records its production status and the evidence nodes needed to re-evaluate current
acceptance when a checker or assumption is revoked. -/
inductive FactStatus where
  | established | conditional | refuted | notEstablished | notApplicable

/-- A fact with its production status and its direct dependencies in the evidence graph. -/
structure Fact where
  judgment : String
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
      (LeanCompilerBackend.Verified.validateJob (Concrete.Backend.LCB.job b.source) = .ok ())

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

/-- A refusal requires replay of the exact failed production invocation. -/
def RefusalValid (codec : Codec) (ctx : ReplayContext)
    (input : Stages.Input) (c : RefusalCertificate) : Prop :=
  c.schemaVersion = codec.version ∧ IdentityMatches codec ctx input c.identity ∧
  OrderedGraph c.evidenceGraph ∧
  Replays codec c.evidenceGraph .refusal (Stages.compile input = .error c.refusal) ∧
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

/-- The caller obtains the fact catalog from the evidence graph. Validation checks the
catalog, replay, all bound payloads and the final-image proposition over the final bytes. -/
def ReceiptValid {Env : Type} (codec : Codec) (ctx : ReplayContext) (format : NoteFormat)
    (tools : NativeTools) (input : Stages.Input) (out : Stages.Output) (build : ExactBuild)
    (exe : ByteArray) (r : Receipt) {platform : Platform Env r.certificate.profile.target}
    (n : NativeModels Env r.certificate.profile platform)
    (facts : List EvidenceNode → List Fact) : Prop :=
  r.schemaVersion = codec.version ∧ PreLinkValid codec ctx tools input out build r.certificate ∧
  r.executable = codec.hash .executable exe ∧
  r.certificateDigest = codec.hash .preLinkCertificate (codec.certificate r.certificate) ∧
  NoteMatches codec format r.certificate build exe r.note ∧
  platform.decode exe = platform.decode build.linkedImage ∧
  (∀ env, LaunchReady n exe env → LaunchReady n build.linkedImage env ∧
    Contracts.PrefixSafety n.process n.process (build.linkedImage, env) (exe, env)) ∧
  (∀ env, RunReady n exe env → RunReady n build.linkedImage env ∧
    Contracts.MaximalRefinement n.process n.process (build.linkedImage, env) (exe, env) ∧
    ((∃ o, n.process.observes (build.linkedImage, env) o) →
      ∃ o, n.process.observes (exe, env) o)) ∧
  OrderedGraph (r.certificate.evidenceGraph ++ r.evidenceGraph) ∧
  r.replay = ctx.commands ∧ r.executionTrust = ctx.executionTrust ∧
  r.facts = facts (r.certificate.evidenceGraph ++ r.evidenceGraph) ∧
  (∀ f ∈ r.facts, f.dependencies.Nodup ∧ ∀ id ∈ f.dependencies,
    ∃ node ∈ r.certificate.evidenceGraph ++ r.evidenceGraph, node.id = id) ∧
  r.openPremises = r.certificate.loweringPath.premises ∧
  (∀ p, p ∈ r.openPremises ↔ HasEvidence r.evidenceGraph (.external p) (.openPremise p.name)) ∧
  (∀ node ∈ r.evidenceGraph, node.obligation = .finalImage ∨
    ∃ p ∈ r.openPremises, node.obligation = .external p) ∧
  HasDependencies (r.certificate.evidenceGraph ++ r.evidenceGraph) .finalImage
    (.toolHandoffs :: r.openPremises.map Obligation.external) ∧
  match r.binding with
  | .driverAttested => HasEvidence r.evidenceGraph .finalImage .driverAttested
  | .imageValidated d =>
    HasEvidence r.evidenceGraph .finalImage (.kernelChecked d) ∧
    Checked codec d ((∃ decoded, platform.decode exe = some decoded) ∧
      (∃ env, RunReady n exe env) ∧ ImageCorrect n input.path out.core exe)


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

end Concrete.Certificate
