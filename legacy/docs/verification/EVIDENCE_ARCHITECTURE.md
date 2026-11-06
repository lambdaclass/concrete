# Evidence Architecture

Status: normative design direction — not a statement of shipped guarantees

This document defines the target data model for Concrete evidence. The
[verification charter](VERIFICATION_CHARTER.md) states the product promise;
this document states the architectural boundaries needed to keep that promise.
Current support remains documented in [CLAIMS_TODAY.md](CLAIMS_TODAY.md).

## Governing rule

> Concrete represents every claim at least as precisely as it describes that
> claim. Source correspondence, logical validity, dependency closure, replay
> history, and current policy acceptance remain separate, typed, independently
> degradable facts.

No report, renderer, package writer, or policy evaluator may reconstruct an
authoritative fact from display strings. Authority moves only through checked
constructors whose acceptance conditions are executable and negatively
controlled.

## Identity, model, contract, and proposition are different objects

Concrete retains five independently identified objects:

```text
DefinitionIdentity   exact executable implementation
ModelIdentity        mathematical semantics used by a proof
ContractIdentity     exported behavioral interface used by callers
PropositionIdentity  canonical VIR claim proved by a certificate/kernel
PolicyIdentity       rule set under which evidence is accepted
```

Their digest domains and constructors are distinct. A value from one domain
cannot populate another field, and no identity is reconstructed from a display
name, path, raw string, or neighbouring digest.

The architecture contains two independent proof arrows:

```text
model --correspondence evidence--> implementation
model/proposition --logical proof--> claimed property
```

An exact generated implementation reference prevents substitution of the
endpoint. It does **not** prove that the model faithfully represents that
endpoint. A kernel-valid theorem proves its proposition over the model; it does
not establish source/model correspondence. These facts remain independently
degradable through reports, roots, receipts, packages, and policy.

### ImplementationReference

An `ImplementationReference` is an opaque generated selector for an exact
`DefinitionIdentity`: package, module, declaration, and authoritative semantic
implementation digest. Its constructor is private. Proof authors select a
generated typed symbol and never write package components, paths, display names
or hexadecimal digests. The compiler independently recomputes the identity;
missing, stale, forged, duplicate, ambiguous, malformed, or legacy name-only
references refuse or become `needs_recheck`.

Matching recomputation preserves provenance. `generatorAsserted` material does
not become `compilerLinked` merely because producer and consumer invoke the
same canonical digest function.

### ModelIdentity and ModelAttestation

`ModelIdentity` binds the model schema, canonical model digest,
parameterization, logical dependencies, and model codec/toolchain identity. A
proof-script-only change need not move it; a semantic model change must.

`ModelAttestation` records:

```text
implementation : ImplementationReference
model          : ModelIdentity
relation       : implements | refines | equivalent | abstracts |
                 contract_only
claimScope     : ClaimScope
provenance     : AttestationProvenance
correspondence : proved | checked | assumed | missing | stale |
                 ambiguous | unsupported
```

Attaching a model creates a named correspondence obligation whose default is
`missing`; generation does not discharge it and silence is never `assumed`.
Refinement in one direction is not equivalence. Trust is an evidence
mode/disposition rather than a semantic relation; a trusted model remains
assumed even when implementation selection is exact.

A model used as logical contract semantics must also be total over the claimed
domain. An `implements`, `refines`, `equivalent`, or `contract_only`
attestation cannot receive a current logical disposition unless the model is
effect-free, terminating, free of unhandled traps, and has a complete logical
interpretation. Recursive models additionally require accepted
`#[decreases]`/well-foundedness evidence. Missing or invalid totality evidence
leaves correspondence `missing` or `unsupported`, or explicitly `assumed` on
the trusted surface; exact identity, successful replay, and finite behavioral
agreement cannot upgrade it. This is the dependency on the checked total
contract-callable fragment, not an incidental sequencing preference.

The authoritative key is
`(DefinitionIdentity, ModelIdentity, ClaimScope)`. Relation kind and evidence
mode are coherent payload, not key fields: allowing them into the key would
permit trusted/proved or refinement/equivalence rows to coexist as two answers
for one attachment. More than one active row for the key is ambiguous—even
when byte-identical—and refuses.

Relation kinds form an explicit entailment/coherence algebra. At minimum,
equivalence entails the corresponding directional refinements, and a suitable
refinement may satisfy a selected contract. `implements` and `abstracts` retain
explicit direction rather than being forced into an unsound total order. A
stronger row explicitly supersedes or retires the weaker active row; it never
shadows it. Evidence-mode downgrade is trust widening and requires an explicit
transition.

### Correspondence evidence available before full formalization

Concrete should reuse its differential/reference-evaluator machinery to check
model behavior against implementation semantics. Exhaustive agreement earns
`checked` only over a declared finite domain. Sampled agreement over an
infinite domain records domain, strategy, seed and coverage and remains weaker;
it never becomes universal equivalence. A mismatch emits a counterexample and
refuses current correspondence.

This reuses the translation-assurance dimension rather than creating a second
testing ontology. It is correspondence evidence—not a kernel theorem—and its
shared producer/evaluator trust remains explicit.

### ExportedContract

An exported interface has its own `ContractIdentity` and independently
classified components:

```text
functional behavior
effects and capabilities
reads set
modifies/frame set
failure behavior
termination disposition
resource contract
assumptions and trusted boundaries
```

Each component records whether it is authored, inferred-exact,
inferred-conservative, runtime-checked, assumed, or missing. Conservative
inference is safe but weaker; it cannot render or satisfy policy as exact or
proved evidence.

### Dependency strengths

Evidence distinguishes:

- a `contract` edge, invalidated by the exported contract/evidence changing;
- a `model` edge, invalidated by model identity or correspondence changing;
- an `implementation` edge, invalidated by exact implementation semantics
  changing;
- explicit `trusted`, `missing`, and `unclassified` edges.

A contract-preserving body change leaves contract consumers current while
staling implementation consumers. A model change stales model consumers even
when the executable body is unchanged. Edge kind is derived from what the
proof actually uses, never selected as a freshness optimization.

### Partial evidence linking

Package evidence composition is a checked partial operation, not row
concatenation followed by hashing. It rejects duplicate identities,
incompatible contracts/scopes/schemas/rule sets, trust widening, assumption
deletion or conflict, model/implementation substitution, ambiguous
attestations, incomplete closure, and evidence weaker than consumer policy.
Successful linking recomputes closure and roots and records the exact
contract/model/implementation justification for every imported claim.

Receipts bind implementation, model, relation, scope, correspondence evidence,
contract, proposition, policy/rule set, theorem artifact, tables, environment,
dependency root, trust, and assumptions in separately typed fields. A valid
certificate copied to another code object, model, proposition, contract, or
policy refuses. Advisory state remains a validation-time registry input, not a
retroactively mutable receipt fact.

## Five objects, not one status

### Source correspondence

`SourceCorrespondence` answers whether the canonical verification proposition
describes the relevant source and typed-Core behavior. Its assurance is
reported independently:

```text
missing
producer_validated
sampled
differentially_checked
exhaustive_for_finite_fragment
certificate_checked
formally_justified
```

A certificate proving a VIR proposition does not strengthen this relation.
Concrete may later check narrow translation certificates or verified
lowerings, but it must not infer source faithfulness from solver or kernel
success.

### Logical validity

`LogicalValidity` answers whether the exact canonical proposition is
established by the retained evidence. Evidence families remain distinct:

```text
certificate_replayed
same_kernel_replayed
independent_reimplementation_replayed
different_family_replayed
kernel_checked
solver_trusted
runtime_checked
tested
assumed
missing
```

Re-running a derivation is not replay of the original evidence. A solver that
re-solves a query provides corroboration; it does not validate the certificate
or proof object that originally supported the claim.

### Dependency closure

`DependencyClosure` answers whether every compiler edge, theorem witness,
contract hypothesis, table entry, implementation, assumption, and trusted
boundary needed by the claim is present, unique, current, and consumed exactly
once where required. Missing, surplus, duplicate, ambiguous, unclassified, and
mismatched sets are named refusals. They are never discarded by `filterMap`, a
first-match lookup, or an advisory-only warning.

### Replay receipt

`ReplayReceipt` is an immutable historical statement that exact retained
material was checked. It binds at least:

- subject and claim scope;
- canonical proposition and artifact/certificate identities;
- checker implementation, version/commit, rule-set, and exporter identities;
- dependency root and table material;
- toolchain, workspace, imports, schema, and canonical-codec identities;
- trust and assumptions present during replay; and
- the replay result and profile.

A receipt does **not** contain the currently effective vulnerability or
revocation disposition. An advisory discovered after minting does not make the
historical replay fictitious.

### Policy decision

`PolicyDecision` is time-relative:

```text
ReplayReceipt
+ current AdvisoryRegistry
+ accepted expected root or signing key
+ consumer Policy
-> PolicyDecision
```

If policy decisions are themselves retained or signed, they bind the exact
policy and advisory-registry identities used. Updating an advisory registry
can invalidate a decision without changing or reminting its receipt.

## Typed identities and authority states

Interchangeable `String` digests are forbidden at authoritative module
boundaries. Use distinct domain-separated types such as:

```text
SubjectDigest            ImplementationDigest
ContractDigest           ClaimDigest
PropositionDigest        TheoremArtifactDigest
CertificateDigest        DependencyRoot
ReceiptDigest            CheckerIdentity
RuleSetIdentity          ToolchainIdentity
PolicyIdentity           AdvisoryRegistryIdentity
```

Each type defines canonical bytes, domain separation, strict parsing, schema
identity, and a private or validating constructor. A value of one digest family
cannot be supplied where another is required.

Authority stages are also types, not comments:

```text
DeclaredEvidence
StructurallyValidatedEvidence
DependencyClosedEvidence
ReplayBoundEvidence
PolicyAcceptedEvidence
```

Do not replace flat statuses with one giant record of optional fields. Use a
shared envelope plus typed variants for evidence families so invalid
combinations remain unrepresentable.

## Authorship modes

The target proof-attachment surface makes the evidence mode explicit where it
enters the program:

```text
#[certificate_by(...)]
#[proof_object_by(...)]
#[trusted_proof_by(...)]
#[assume(...)]
```

`certificate_by` selects a versioned certificate profile. `proof_object_by`
retains exact replay material. `trusted_proof_by` visibly widens the trusted
base. `assume` remains an assumption. Existing `#[proof_by]` cannot silently
inherit the strongest class during migration; it maps conservatively until its
mode is explicit.

## Certificate profiles

Every certificate profile fixes:

- the supported VIR fragment;
- canonical proposition encoding;
- certificate encoding;
- checker algorithm, implementation, version, and rule-set digest;
- deterministic checking relation and failure taxonomy;
- resource/depth limits;
- exact retained artifacts; and
- resulting evidence class.

Initial candidates are linear-arithmetic witnesses, exact CNF plus retained
DRAT/LRAT, and exhaustive finite-domain witnesses. These remain probes until
stable export, semantic agreement, eligible coverage, and checker cost are
measured. Nonlinear solver success remains `solver_trusted` until a checkable
proof object exists.

## Independence

Different executable names do not establish independence. Record these axes
separately:

- implementation and code lineage;
- algorithm;
- logical/proof-system foundation;
- artifact parser;
- exporter and canonicalizer;
- shared libraries;
- shared specification; and
- shared retained artifact.

Policy can then require two executions, two implementations, two design
families, or a certificate plus a kernel. Those are different claims.

## Advisory and revocation boundary

Checker advisories are validation-time inputs. The machine-readable registry
keys entries by exact implementation/version/rule set and upstream
advisory/reproducer identity, and records `accepted`, `affected`, `revoked`, or
`unknown` plus remediation and affected claim indexes.

An affected checker version is downgraded wholesale by default. Fragment-
scoped narrowing is permitted only when an independently checked
`CheckerFeatureFootprint` establishes that the vulnerable mechanism was absent
from the retained environment. A footprint self-reported only by the affected
checker is not sufficient.

Revocation preserves unaffected dimensions. A claim can remain source-bound
and dependency-closed while its logical-validity evidence is rejected pending
replay.

## Standalone consumer

The independent verifier has three layers:

1. **Envelope/accounting:** strict decoding, digest and root recomputation,
   identity uniqueness, dependency closure, receipts, advisories, trust,
   assumptions, authentication, and policy.
2. **Certificate checking:** check exact retained certificates against exact
   retained canonical propositions without invoking their generators.
3. **Translation assurance:** check narrow correspondence evidence where it
   exists and otherwise report the producer-side assurance without upgrading
   it.

Internal consistency is not authenticity. Verification requires an externally
pinned expected root or a signature rooted in a consumer-accepted key.

The proposition language is not independently reinvented here:
`Concrete.Semantics.TermIR` owns internal syntax and executable semantics, and
VIR is its validated canonical external form. The evidence model binds
`PropositionDigest`; it does not infer proposition identity from renderer text
or duplicate TermIR inside the ledger.

## Authority transitions

Every new path progresses explicitly:

```text
designed
-> prototype
-> negatively controlled
-> mutation-killed
-> shadow-compared
-> independently reproduced
-> authoritative
-> adversarially closed
```

No stage is inferred from code existing or a gate being green. A mutation must
exercise the production consumer, not only a helper. Gate results distinguish
registered, executed, killed, survived, inconclusive, and vacuous.

## Required degradation shape

Concrete never collapses an incident into one badge. A policy report may say:

```text
subject_binding: current
source_correspondence: differentially_checked
dependencies: complete
drat_certificate: valid
lean_replay: revoked_checker
authentication: valid
policy: rejected
required_action: replay Lean artifact with an accepted checker
```

That separability is the purpose of the architecture, not merely a reporting
convenience.
