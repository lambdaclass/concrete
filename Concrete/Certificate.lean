import Concrete.Theorem

namespace Concrete.Certificate

open ImageTheorem

/-- Domain tags for the canonical, versioned digest encoding. Domains describe payload identity;
semantic correspondence is established by evidence at the appropriate edge. -/
inductive DigestDomain where
  | source | compiler | spec | bir | llvmModule | lcbFunction | lcbJob | siteTable
  | runtimeInput | assembly | objectFile | linkInput | tool | executable | executableNote
  | preLinkCertificate | evidence | receipt
  deriving DecidableEq

/-- The digest of exact bytes in a specific domain. The canonical encoding and digest
verification are obligations of the future bounded reader. -/
structure Digest (_domain : DigestDomain) where
  bytes : ByteArray

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

/-- An evidence reference for one obligation in the artifact's dependency graph. -/
inductive EdgeEvidence where
  | kernelChecked (evidence : Digest .evidence)
  | driverAttested
  | openPremise (name : String)

/-- A dependency node names its obligation, the evidence used at production, and the exact
nodes on which the obligation depends. Canonical readers will validate the graph and its IDs. -/
structure EvidenceNode where
  id : Nat
  obligation : String
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
  openPremises : List String
  executionTrust : List String
  replay : List ReplayCommand

end Concrete.Certificate
