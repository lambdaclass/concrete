# Proof and Audit Pipeline

Status: **target architecture, not next implementation step.**

This document names the compiler pipeline Concrete is moving toward
for proof, reports, snapshots, release bundles, and external audit.
It complements [ARCHITECTURE.md](ARCHITECTURE.md): the backend path
still compiles through Core, Mono, Lower, SSA, and LLVM, while this
path describes the proof/audit artifacts that reviewers and tools
consume.

## Discipline before reading this

This is **target shape**, not a build plan. The layers named below
do not all land at once; they land when a pull-through candidate
forces them. See ROADMAP Operating Rules "candidate-forced
refactors" and the caveats below:

- **No preemptive layer.** Adding a named IR layer requires
  repeated candidate pressure or visible duplication in ProofCore
  rules. parse_validate forced the Core → ProofCore split.
  fixed_capacity is currently forcing further ProofCore extensions.
  Normalized Core lands when a second candidate makes the gap
  undeniable, not when one example seems to suggest it.
- **Continue direct ProofCore extension** while the per-construct
  rules stay coherent. If those rules start duplicating logic or
  feel ad-hoc, normalization becomes the right next step.
- **Each open caveat below must be resolved before its layer
  lands** — see Open Decisions further down.

## Target Shape

```text
Source
  -> Parse
  -> Resolve
  -> Check
  -> CheckedProgram
  -> Core
  -> Normalized Core
  -> ProofCore
  -> Obligations / proof reports / proof registry
```

Execution still follows the backend path:

```text
Core
  -> Mono
  -> Lower
  -> SSA
  -> LLVM IR
  -> native binary
```

The important split is that the proof/audit path is not the backend
path. Backend lowering can stay practical and target-oriented.
ProofCore can stay small, normalized, and trustworthy.

## Artifacts

### CheckedProgram

The stable artifact after name, type, capability, ownership, and
surface legality checks. It records checked declarations and enough
semantic facts for reports and future tooling before elaboration
details are spread across later passes.

This artifact is still a roadmap item. If it does not simplify
plumbing, caching, and report generation, it should be rejected
explicitly rather than left as an implicit boundary.

### Core

The semantic compiler IR. Core is the authority for the checked
meaning of the program. It may still contain language constructs
that are useful for compilation but too broad for direct proof.

### Normalized Core

The proof/audit normalization layer. Its job is to turn source and
Core conveniences into a smaller, regular shape before ProofCore and
reports consume them.

Examples of normalization:

- early returns become explicit proof-friendly control flow
- field access is represented consistently
- array indexing and assignment have one canonical form
- struct and enum construction are explicit
- casts are explicit
- simple control flow has predictable shape

Normalized Core should remove duplication in the proof path. The
goal is not to add another semantic authority; the goal is to avoid
making ProofCore understand every source-level spelling.

### ProofCore

The small proof IR consumed by Lean-facing proof tooling. ProofCore
is a proof-oriented view, not a rival language semantics. Extraction
either succeeds into a construct with known proof semantics or
reports a precise exclusion reason.

Current flagship pressure says the next ProofCore extensions should
land in this order:

1. Array indexing.
2. Bounded while loops.
3. Struct value construction.
4. Enum value construction.
5. Pattern matching.
6. Casts.
7. Field-heavy struct/enum code.

`fixed_capacity` is the current forcing candidate for this work. The
first three also gate the later real-cryptography flagship slot.

## Shared Fact Layer

Reports should render from one canonical fact layer instead of each
report recomputing its own truth. The same facts should feed:

- `--report caps`
- `--report authority`
- `--report alloc`
- `--report unsafe`
- `--report proof`
- `--report proof-status`
- `--report obligations`
- policy checks
- assumption checks
- snapshots
- release bundles
- showcase manifests

This is what makes drift detectable. If proof status, policy,
assumptions, snapshots, and release bundles all render from the same
facts, contradictions become test failures instead of documentation
rot.

## Pass Contracts

Every named compiler boundary should eventually have a contract:

- input invariants
- output invariants
- identities and fingerprints preserved
- assumptions introduced
- verifier or report that checks it
- examples/regressions that forced the boundary

This is the practical bridge from today's compiler to later Phase 12
compiler proofs. Before proving a pass correct, Concrete needs each
pass to say what correctness would mean.

## Why This Makes Concrete Stronger

The target shape keeps each responsibility narrow:

- Core remains the semantic authority.
- Normalized Core regularizes proof/audit targets.
- ProofCore stays small enough to have Lean semantics.
- Reports and release evidence use stable shared facts.
- Backend lowering remains focused on executable code.

That lets Concrete make stronger claims without pretending the whole
compiler is already verified.

## Current Status

Live today:

- Validated Core exists as the current proof boundary.
- ProofCore extraction exists for a narrow pure fragment that now
  includes integer/bool, function calls, if-then-else (including
  early-return-with-fall-through), struct literal, and field
  access.
- Proof status, assumptions, policy, snapshots, catches, release
  bundles, and showcase manifests are already used by graduated
  examples.

Still roadmap work:

- `CheckedProgram` artifact decision.
- Normalized Core as an explicit proof/audit stage.
- ProofCore verifier and dump as first-class surfaces.
- Shared fact database for reports.
- Stable identities/fingerprints across reports, policies,
  assumptions, snapshots, and bundles.
- ProofCore support for arrays, loops, enums, matches, casts, and
  mod operator.

## Open Decisions

These decisions block specific layers from landing. They are not
ordered by priority; each is a prerequisite for its own surface.

### Fact layer: what is "a fact"?

"Fact layer" is currently vague. Before implementing the
single-source-of-truth reports machinery, pick a concrete shape:

- **Per-declaration structured facts** (one record per
  function/struct/enum, keyed by qualified name, with stable
  field schema). Simplest, file-per-decl JSON or a single blob.
- **Relational facts** (set of typed relations, joinable across
  declarations). More expressive, more machinery.
- **Graph-like facts** (nodes = artifacts, edges = use-def /
  call / capability flow). Most expressive, most machinery.

**Working preference: per-declaration structured facts plus
stable IDs**, not a database. Cheaper to implement, sufficient
for every existing report. Can be promoted to relational if a
later report needs joins.

### Proof fingerprint: source, ProofCore, or both?

When source body or normalization rules change, proofs need to
react. Three honest options:

- **Source fingerprint** (today). Catches all source drift but
  refactors that don't change meaning revoke proofs. Noisy.
- **Normalized/proof fingerprint**. Refactors that normalize to
  the same form preserve proofs. Trusts normalization.
- **Dual key** (source + normalized). Reports can distinguish
  "source changed, proof target unchanged" from "proof target
  changed."

**Working preference: dual key**, with reports surfacing both.
Source fingerprint is the conservative guard; normalized
fingerprint is the upside. Naming both in the same report makes
the difference between "harmless refactor" and "semantic change"
visible to reviewers.

### Policies and assumptions: where do they plug in?

`Concrete.toml [policy]` and `assumptions.toml` are currently
consumed by CI gates external to the compiler. Two places they
could live in the new pipeline:

- **Fed into Check.** Policy violations become type-checker
  errors.
- **Fed into the fact / report layer.** Policy and assumption
  facts join the shared fact set; violations are report claims,
  fail-able by gates.

**Working preference: fact / report layer.** Check should enforce
language legality, not project budgets. Policy and assumption
checks are project-level / release-level gates layered over
compiler facts. A separate `--check policy` mode could promote
specific violations to errors when a deliberate policy-checking
build is requested.

### CheckedProgram: does it pay back?

`CheckedProgram` only justifies its existence if it simplifies
plumbing, caching, and report generation. The decision is
empirical: implement the shared fact layer first, see what it
needs from post-check state. If that surface is large and stable
enough to deserve its own artifact, `CheckedProgram` lands. If
not, it stays an implicit pass boundary, and the proposal is
rejected explicitly.
