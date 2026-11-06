# Verification IR

Status: normative design direction — VIR v1 is not yet a shipped compatibility promise

VIR is the public, versioned, validated artifact form of the existing
`Concrete.Semantics.TermIR` proposition language. It is **not** a parallel
semantic IR and must not become a fifth answer beside Core, PExpr, TermIR, and
ObligationCore. TermIR owns internal proposition syntax and executable
semantics; VIR adds validation, canonical hostile bytes, proposition identity,
and certificate attachment.

VIR v1 is TermIR (R-0455) promoted and extended—not rebuilt. Existing TermIR
ownership of sorts, structural evaluation, truncating arithmetic semantics,
uninterpreted applications, goals, and proved transformations is inherited.
The promotion adds validated sorting/context, stable identities, records and
arrays, canonical codec/digest, consumer parsing/evaluation, and certificate
bindings. Any proposal that reimplements an already-owned TermIR meaning is an
architecture violation unless it explicitly replaces and deletes the old owner.

See [EVIDENCE_ARCHITECTURE.md](EVIDENCE_ARCHITECTURE.md) for the surrounding
evidence model and [CLAIMS_TODAY.md](CLAIMS_TODAY.md) for current support.

## Purpose and ownership

```text
Concrete source -> typed Core
      | separately reported source correspondence
      v
TermIR -> Raw VIR -> ValidatedGoal -> canonical VIR proposition
      | certificate or proof-object checking
      v
logical-validity evidence
```

The owners are distinct:

```text
TermIR              internal syntax and executable semantics
Raw VIR             hostile decoded artifact
ValidatedGoal       well-sorted closed proposition and context
VIR codec/digest    canonical public bytes and proposition identity
CertificateProfile  checked subset and certificate relation
ObligationCore      source/claim provenance and disposition
ReplayReceipt       exact proposition, evidence, checker and dependencies
PolicyDecision      current advisory-aware acceptance
```

Proving VIR does not prove that VC generation was faithful. TermIR does not
absorb provenance, policy, receipts, or evidence classification.

## Current TermIR gap

TermIR is the correct seed, not yet the completed boundary. `Srt` exists, but
`Term` is not indexed by a sort; variables and uninterpreted symbols are raw
strings without declared signatures. Promotion requires a total validation
transition:

```text
RawTerm + SymbolContext -> ValidatedTerm sort | named refusal
RawGoal + SymbolContext -> ValidatedGoal | named refusal
```

Only `ValidatedGoal` may reach renderers, certificate profiles, proposition
digests, or receipts. Validation checks operator domains and arity, bit widths,
variable sorts, symbol signatures and definitions, quantifier domains, and
complete context closure. Its constructors are closed or private.

## VIR v1 boundary

The initial intended fragment contains booleans, linear mathematical integers,
selected fixed-width bit-vectors, records, arrays as total functions,
conditionals/lets, and versioned defined or uninterpreted specification
functions.

Uninterpreted calls already exist internally as `TermIR.Term.sym`; promotion
replaces string spelling with stable symbol identities and a canonical
declaration table. Arrays and records are semantic extensions: they must define
type/field identity, sorts, length/bounds, read/update/projection/equality
behavior, and certificate-profile support.

Every operation defines exact typing, evaluation, failure, overflow, and
partiality behavior. Anything outside the fragment produces a named refusal.

## Permanent simplicity constraints

Canonical VIR meaning never depends on source/checkout paths; declaration,
import, allocation, hash-table, or traversal order; process-global counters;
unresolved names; ambient plugins; hidden coercions/dependencies; prover pretty
printing; or unspecified host arithmetic.

VIR v1 excludes arbitrary inductive declarations, general definitional
equality, and prover-kernel projection machinery. This bounds the portable
checker TCB; it does not prove a Lean environment avoided those mechanisms.
Lean replay remains dependent on its exercised kernel surface, and advisory
narrowing requires an independently checked retained-environment footprint.

## Identities and canonical encoding

Parameters use signature position. Locals/binders use function-local lexical
paths. Functions, contracts, fields, records, definitions, and spec symbols use
stable qualified semantic identities. Unresolved identity is a refusal.

Alpha-renaming and unrelated declaration/import reordering leave canonical
bytes unchanged. Semantic changes move the relevant identity.

The normative representation is a versioned canonical binary codec; text and
JSON are diagnostics. Decoding rejects duplicates, non-canonical integers,
alternate ordering, unknown critical fields, trailing bytes, excessive depth,
forbidden cycles, and resource-limit violations. Serializing Lean `repr` is
forbidden. Internal TermIR constructors may evolve without changing public
bytes, and an independently implemented hostile parser must reproduce the
accepted values and bytes.

The proposition digest binds the complete meaning context: VIR schema,
sort/symbol declarations, referenced definitions or identities, arithmetic
profile, quantifier domains, assumptions, transformation profile, relevant
checking limits, and claim scope where it changes meaning.

`ModelIdentity` and VIR `PropositionIdentity` are distinct. A model identifies
the mathematical semantics selected for an implementation; a proposition
identifies one claim over that model. Several propositions may use one model,
and a proof-script change need not change either identity when the model and
claim are semantically unchanged.

Certificates bind and establish logical validity of an exact VIR proposition.
They do not establish that a source implementation corresponds to the selected
model. Correspondence evidence is a separate artifact and receipt field. VIR
must represent relation claims such as refinement and equivalence distinctly;
one-direction refinement cannot be relabeled as equivalence by metadata.

## Semantics

TermIR's structural evaluator is the seed of the executable semantics. VIR
also defines satisfaction/entailment over all declared environments and symbol
interpretations. Evaluating one environment is not universal validity;
exhaustive evaluation establishes validity only for declared finite fragments.
Other certificate checkers owe soundness relative to the satisfaction relation.

The evaluator/common semantics does not prove Core-to-VIR faithfulness. It is
the meaning against which differential, exhaustive, certificate, and future
formal correspondence evidence is stated.

## Certificate profiles

A profile maps a declared validated VIR subset to exact checker input, e.g.:

```text
canonical VIR proposition -> exact CNF -> retained DRAT/LRAT -> checker result
```

The mapping is evidence-bearing. Re-solving CNF to create another certificate
is corroboration, not replay of the retained artifact.

## Extension admission

Every extension requires grammar/typing and executable semantics; codec-version
analysis; source/Core correspondence behavior; backend/profile behavior;
unsupported/malformed controls; semantic-change and invariance controls;
wrong-goal/correct-certificate swaps; resource-exhaustion controls; and an
explicit remaining-trust statement. Backend agreement alone is insufficient.

## Promotion sequence

1. Inventory every current obligation shape and TermIR refusal.
2. Add stable variable, symbol, field, record, and definition identities.
3. Introduce raw versus validated terms/goals and total sorting.
4. Define the canonical context and complete proposition identity.
5. Specify canonical bytes, limits, domain separation, and schema evolution.
6. Implement and cross-check a second independent parser.
7. Bind certificate profiles to exact proposition digests.
8. Migrate one obligation family in shadow using structured comparison.
9. Make every renderer consume only `ValidatedGoal`.
10. Make ObligationCore and receipts reference the proposition digest.
11. Delete direct `Expr -> prover string` paths after corpus agreement.
12. Add arrays/records only through the extension-admission contract.
