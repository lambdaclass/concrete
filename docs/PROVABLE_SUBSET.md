# Provable Subset

Status: standing architecture reference

The release-facing compatibility contract is
[PROVABLE_V1.md](PROVABLE_V1.md). That file is the canonical allowlist for
types, expressions, state forms, failure behavior, and proof-attachment
requirements. This document explains where the subset sits in the compiler and
why it has this shape; it does not duplicate the allowlist.

For user reliance rules, see [PROOF_CONTRACT.md](PROOF_CONTRACT.md). For the
language/proof semantic boundary, see
[PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md).

## Definition

The provable subset is the fragment of validated Concrete programs that the
current ProofCore extractor can represent and the Lean evaluator can execute.
It has three related boundaries:

1. **Eligibility** excludes entry points, capability-bearing functions,
   trusted origins, and other declaration-level boundaries.
2. **Extraction** translates every supported node of an eligible validated
   Core body to `PExpr`; extraction fails closed if any node is unsupported.
3. **Evidence attachment** binds a theorem and coverage class to a stored proof
   subject and replays the theorem in Lean.

ProofCore is a semantic projection of validated Core, not a replacement
whole-language IR. The checker remains responsible for language validity,
ownership, capability discipline, and match/type invariants.

## Pipeline Position

```text
Source
  → Resolve / Check / Elaborate / CoreCanonicalize / CoreCheck
  → Validated Core
  → ProofCore extraction
  → PExpr evaluation and Lean theorems
```

Extraction occurs before whole-program monomorphization. That matters:
per-instantiation generic proof identity is not yet a general facility and is
tracked under R-0271. The runtime compiler continues independently through
Mono, lowering, SSA, and a backend; there is no verified preservation theorem
connecting that chain to `PExpr`.

## Stable Eligibility Boundary

A current proof target is:

- capability-free;
- non-trusted and not from a trusted implementation;
- not an entry point;
- free of FFI, raw-pointer, and allocation behavior;
- non-recursive;
- fully expressible in the named `ProvableV1` surface.

“Capability-free” means authority-free, not total or incapable of checked
failure. A proof target can contain selected bounded loops, functional state,
arrays, structs, enums, matches, and fixed-width operations. Old blanket rules
such as “no loops,” “no mutation,” or “no aggregates” are obsolete.

## Surface Ownership

The documents have deliberately separate jobs:

| Document | Owns |
|---|---|
| `PROVABLE_V1.md` | Exact admitted types, operations, state forms, and exclusions |
| `PROOF_STATE_MODEL.md` | Functional model for admitted local/array mutation |
| `PROOF_STORY_MATRIX.md` | Evidence and open obligation per language family |
| `PROOF_OBLIGATIONS_REGISTER.md` | Formalization and soundness debt |
| `PROOF_CONTRACT.md` | Meaning of attachment states and user reliance |
| `PROOF_SEMANTICS_BOUNDARY.md` | Runtime/proof semantic differences and trust chain |

Adding a new construct updates the V1 contract and its evidence row. It should
not require maintaining competing constructor lists in explanatory documents.

## Failure Model

The proof evaluator returns a value or `none`. `none` represents a stuck proof
evaluation, including an invalid operation, missing proof-table callee,
out-of-bounds functional array access, or insufficient fuel. It is not one
universal theorem that every corresponding runtime failure has the same
mechanism.

Runtime ordinary arithmetic and safe indexing are checked terminal failures.
Proofs over mathematical integers or partial operations must state the
range/no-failure hypotheses they need. Explicit fixed-width operations carry
only the semantics admitted by `ProvableV1`.

## Calls and Composition

Non-recursive direct calls are represented when the proof function table is
complete. This lets the evaluator execute a callee expression; it does not
automatically compose independent correctness theorems.

Opaque application through a function parameter is needed for real std
higher-order proofs. Runtime Core preserves direct-versus-indirect call
identity, but ProofCore's callable identity remains incomplete under bug 061
and R-0442. A source shadowing restriction must never be used as a substitute
for IR identity.

## Attachment Integrity

A supportable proved claim has a stored fingerprint and successful kernel
replay. A source link without a stored fingerprint is `unbound` and fails
closed.

The current fingerprint is body-oriented. It does not yet include every
signature/type fact, contract, toolchain context, or transitive dependency.
R-0004 owns the versioned `ProofSubjectDigest` and dependency-root work. These
limits qualify freshness; they do not erase the already checked theorem.

## Relationship to High-Integrity Profiles

The provable subset and a high-integrity runtime profile answer different
questions:

| | Provable subset | High-integrity profile |
|---|---|---|
| Purpose | Lean-backed claims about selected pure functions | Restrictions and evidence for an entire executable |
| Capabilities | None | A policy-defined restricted set |
| Trusted/FFI | Excluded | May be admitted through approved boundaries |
| Runtime failures | Theorem-specific assumptions/obligations | Profile policy and runtime behavior |
| Evidence | Proof attachment, coverage, replay | Mixed enforced/reported/tested/proved/assumed/trusted evidence |

Programs are not expected to be entirely proof-eligible. A normal design keeps
effectful shells and trusted adapters outside while proving selected pure
algorithms, validators, and state transitions.

## Growth Rule

The subset grows only with:

1. a forcing workload or proof obligation;
2. an explicit ProofCore representation and evaluator semantics;
3. extraction and negative tests;
4. Lean evidence at the claimed coverage;
5. updates to `PROVABLE_V1.md`, the proof-story matrix, and obligation
   register;
6. an honest statement of runtime correspondence and remaining trust.

Removing or weakening an admitted V1 construct is a compatibility change.
Adding a surface form without semantic and evidence ownership is not
completion.
