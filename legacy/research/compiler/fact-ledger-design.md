# Typed Core + Future CompilerDB Substrate — Objective Spec (Phase 6.5 #9 == #5 == #13b)

Status: **objective design record.** The current type-axis work should not build
a second typed source tree or an eager fact database. Core `CExpr` already
carries `ty : Ty`; the load-bearing first slice is a shared
`Concrete/Semantics/TypeJudgment.lean` used by both Check and Elab.

This document records the long-term substrate decision. The roadmap states the
order; this file states where each kind of fact belongs.

## The Decision

Concrete's compiler facts live in their strongest representation:

- **typed Core (`CExpr.ty`)** owns node-local source type facts;
- **shared `TypeJudgment`** computes those type facts once for both Check and
  Elab;
- **future `CompilerDB`** owns relational, cross-node, cross-stage, provenance,
  dependency, evidence, replay, report, LSP, agent, and query facts when a real
  consumer pulls that layer;
- each fact has one owner and one primary home.

The placement rule is:

> Node-local type facts are computed by one shared `TypeJudgment` and carried by
> typed Core. Relational, cross-node, cross-stage, provenance, dependency,
> evidence, and query facts go in `CompilerDB` when those facts become
> load-bearing.

If both are relevant, both may exist at their own level without duplicating
truth. Example: a literal's type is stamped into `CExpr.ty` by Elab using the
shared judgment; "this literal argument agrees with parameter 0" is a future
`CompilerDB` edge; "Lean replayed the proof of this type-preservation lemma" is
future `CompilerDB` evidence.

## Why Hybrid

The pure typed-source-AST direction fails because Concrete already has the typed
carrier it needs: Core `CExpr`. Creating another typed source tree would add a
second shape to maintain without fixing the real bug, which is Check and Elab
running separate source type judgments.

The pure DB direction also fails as the best verification architecture. A
mandatory typed field makes a missing local fact unconstructable; a DB entry plus
`assertFamilyComplete` only detects absence with an ICE. Phase 6.5's own north
star is "types that forbid drift," and Core already provides the typed field:
`CExpr.ty`.

But a typed IR cannot hold every fact. Some facts are inherently relational:

- pass agreement is a call-site to parameter edge;
- borrow conflicts are edges;
- E0293 container-not-in-context is about pairs of paths;
- capability flow, proof dependencies, invalidation dependencies, lowering
  provenance, and trap origins are edges.

The hybrid gets both properties: structural induction over typed Core for
preservation proofs, and a relational/evidence/query DB for facts that cannot
live on one node.

## Core Invariant

There is exactly one committed fact.

- A source type fact is computed by shared `TypeJudgment` and committed as
  `CExpr.ty`.
- A relational/evidence/provenance/query fact is committed as a future
  `CompilerDB` entry.
- A conflicting DB write fails closed.
- A missing required DB fact fails closed.
- Later stages consume the owning representation; they do not silently re-derive
  another answer.

## Typed Core And Shared TypeJudgment

Core `CExpr` is already typed: each expression constructor carries `ty : Ty`, and
downstream stages read `CExpr.ty`. The type-axis problem is not "there is no
typed carrier"; the problem is that Check and Elab compute source types
independently and can disagree.

The first family is literals:

- shared `TypeJudgment` computes the literal's type once;
- Check uses that judgment for type-dependent checks;
- Elab uses that judgment to stamp `CExpr.intLit ... ty`;
- downstream stages consume `CExpr.ty`;
- no Elab fallback inference exists for migrated literals;
- future `CompilerDB` may record evidence/provenance for the type, but does not
  store a second independent type answer.

This moves the DB-vs-typed-carrier decision into Phase 6.5, at the first slice,
not into Phase 14, while avoiding a redundant typed source AST.

## Future CompilerDB

`CompilerDB` is the future unified interned-ID, relation, evidence, provenance,
and query layer. It should be introduced when pass agreement, borrow conflicts,
E0293, provenance, replay, package evidence, LSP, agent facts, or incremental
checking needs a relational store. It is not a dumping ground for facts that
typed IR can enforce more strongly.

When that layer is pulled, a Datalog-style or stratified-facts model is a useful
design reference for relational facts, provenance edges, dependency/invalidation,
and evidence queries. That does not mean adding a Datalog engine now, and it
does not mean a user-facing logic language. The first version should be ordinary
typed compiler code unless a real relation family proves maps/joins too awkward;
if a rule layer appears later, it must be stratified, replayable, and
fail-closed rather than a hidden second truth source.

Conceptual key shape when pulled:

```lean
inductive SourceKey
  | expr (id : ExprId)
  | stmt (id : StmtId)
  | decl (id : DeclId)
  | param (id : ParamId)
  | type_ (id : TypeId)
  | module_ (id : ModuleId)

inductive FactKey
  | node (key : SourceKey) (role : FactRole)
  | edge (src dst : SourceKey) (kind : EdgeKind)
```

The objective can extend this with Core, SSA, backend, obligation, diagnostic,
and report ids in the same DB. `ValidatedCore`, `ValidatedSSA`, and
`ValidatedBackendIR` remain typed boundary tokens. Their DB-facing certificates
are query views over `CompilerDB`, not independent semantic stores.

## DB API Contract

For DB-owned facts, the fail-closed interface has three distinct operations:

- `insertFact`: writes a fact. Writing a different fact to an already-committed
  key is a hard error.
- `requireFact`: reads a fact required by a migrated relational/evidence family.
  Absence is a hard error, never a fallback to inference.
- `factOf` / optional lookup: permitted only for unmigrated or genuinely
  optional facts.

`assertFamilyComplete` remains useful for DB-owned relational facts and for
transition periods. It is not the primary guarantee for typed node-local facts;
those are complete by construction because typed Core cannot be built without
their fields.

## Query Shape

When `CompilerDB` is introduced, queries should be monadic from day one:

```lean
abbrev QueryM := ReaderM CompilerDB
```

The first implementation may be eager. Later, `QueryM` can become a
state/cache/dependency monad without migrating every call site. This is the
Rust/Salsa lesson adapted to Lean's lack of interior mutability: make the
interface query-shaped when the DB exists, turn on memoization later.

## Hot-Read Rule

Typed IR is the hot path for node-local structural facts. `CompilerDB` is
canonical for relational/evidence/provenance/query facts, but passes must not
repeatedly hash-probe the same DB fact in tight loops.

- Elab/Mono/Lower should read typed fields directly for local facts.
- When a DB fact is needed, bind it locally while processing a node/block.
- Whole-tree passes may materialize dense projections when that is the right
  access pattern.
- Telemetry and complexity gates should watch query counts and scaling.

## Proof-Carrying Evidence

A `CExpr.ty` field can have evidence recorded in `CompilerDB`; a DB-owned
relational fact carries evidence directly. `FactEntry` must be able to grow from
checked evidence to kernel-checked evidence without a model change:

- `evidenceClass = checked_by_stage` means a compiler verifier established it.
- `evidenceClass = proved_by_kernel` means a proof artifact replayed.
- `proofArtifact` and `replayCommand` make that replay explicit.

The DB is not in tension with obligation-style proofs. The tension is with
whole-pass preservation proofs: those are easier over typed Core than over
dynamic lookups. That is why Core owns local type facts.

## First-Class Consumers

The ladder is not only codegen:

- Elab produces typed Core using the shared type judgment.
- The interpreter consumes certified Core and remains the differential oracle.
- Proof extraction consumes certified Core and emits obligations tied to typed
  Core facts and future DB evidence/provenance.
- Lower/SSA/backend stages may later add relations, evidence, and provenance
  edges into `CompilerDB` while keeping local type facts in typed Core.
- Reports, audit, LSP, and agent JSON query typed Core plus future `CompilerDB`;
  they do not restate semantic facts manually.

## Migration Plan

Next load-bearing work:

1. Extract `Concrete/Semantics/TypeJudgment.lean` as the type-axis sibling of
   `IntArith`.
2. Route Check's literal type-dependent checks through it.
3. Route Elab's literal `CExpr.ty` stamping through it.
4. Add a type-agreement gate with the E0228 literal/defaulting red-team.
5. Continue family by family: binops, casts, calls, aggregates, control flow,
   patterns.
6. Continue axis by axis: types, capabilities, ownership/value-flow,
   pass-agreement edges, proof-relevant facts.
7. Introduce `CompilerDB` only when relational/evidence/provenance consumers
   require it, then use the API/query/evidence shape above.

The docs always describe the objective. Status notes and commit history describe
the current slice.
