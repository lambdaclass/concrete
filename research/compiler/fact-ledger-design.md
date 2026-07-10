# CompilerDB certificate substrate — objective spec (Phase 6.5 #9 == #5 == #13b)

Status: **objective locked; implementation in progress.** The code currently
has the first behavior-neutral slice in `Concrete/Semantics/CompilerDB.lean`
plus `Concrete/Elab/Identified.lean`: a minimal source-keyed fact store,
`QueryM`, `IdExpr`/`IdentifiedProgram`, and the dead fat `TExpr` removed. The
objective below is the architecture those slices grow into.

This document is the canonical substrate spec for the Check/Elab typed-fact arc.
The roadmap states the order; this file states the long-term shape.

## The Decision

Concrete's compiler facts live in one unified interned-ID `CompilerDB`, not in a
fat typed AST and not in independent per-IR fact stores.

The decisive reason is that many load-bearing facts are not node-local:

- pass agreement is a call-site to parameter edge;
- borrow conflicts are edges;
- E0293 container-not-in-context is about pairs of paths;
- capability flow, proof dependencies, invalidation dependencies, lowering
  provenance, and trap origins are edges.

A fat `TExpr` cannot hold those facts. It would leave Concrete with both a fat
AST and a separate edge-keyed store, which is exactly the second-truth-source
shape this phase is removing. The AST stays thin-in-facts; facts live in
`CompilerDB`.

## Core Invariant

There is exactly one committed fact.

- A conflicting write fails closed.
- A missing fact in a migrated family fails closed.
- Later stages require committed facts; they do not silently re-derive them.

That is the design. The rest of this document is mechanism.

## CompilerDB, Not A Per-IR Store Chain

The objective is one `CompilerDB` with a universal interned-ID space:

- source ids: expressions, statements, declarations, parameters, types, modules;
- Core ids;
- SSA value/block ids;
- backend operation ids;
- obligations, diagnostics, and report facts;
- fact ids.

Each fact is keyed by the id kind that owns it. Source facts do not force
`ExprId` through Core/SSA/backend. Core/SSA/backend facts get their own ids in
the same DB. Provenance between levels is represented as first-class edges such
as `lowersTo`, `emitsAs`, and `trapSource`.

`ValidatedCore`, `ValidatedSSA`, and `ValidatedBackendIR` remain important:
they are typed boundary tokens that prove a stage ran and verified its output.
They are not separate semantic stores. Their certificates are views/query-groups
over the same `CompilerDB`.

## Current Slice

The current code intentionally starts smaller:

- `NodeId` / `ExprId` and `IdentifiedProgram` provide post-desugar source
  identity.
- `CompilerDB` has source-oriented keys and edge keys.
- `FactEntry` already has owner stage, evidence class, dependencies,
  provenance, and optional proof/replay fields.
- `QueryM` is eager today, but shaped so memoization and dependency tracking can
  be added later without changing call sites.

This is not a retreat from the objective. It is the first load-bearing slice.
Docs describe the objective; status notes describe how much of it has landed.

## Identity

Parser, resolve, and desugar stay id-free. A deterministic mint pass runs after
desugar and before Check, producing an `IdentifiedProgram`. Check and Elab
consume that same identified tree.

This avoids placeholder ids before Check and avoids asking desugar to preserve
identity for nodes it synthesizes. Synthetic-node provenance is carried
separately from identity; source span/origin is not the key.

Long term, downstream IRs mint their own interned ids into `CompilerDB` as they
are constructed. Identity is thin-in-facts, not absent: every stage that wants to
commit facts must have stable ids for the entities it owns.

## Keys And Edges

The source-level key space must be broader than expressions. A parameter,
declaration, type, or module can be an endpoint of a fact.

Conceptual shape:

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

The objective extends this pattern with `CoreKey`, `SSAKey`, `BackendKey`, and
cross-level provenance edges in the same DB.

## API Contract

The fail-closed interface has three distinct operations:

- `insertFact`: writes a fact. Writing a different fact to an already-committed
  key is a hard error.
- `requireFact`: reads a fact required by a migrated family. Absence is a hard
  error, never a fallback to inference.
- `factOf` / optional lookup: permitted only for unmigrated or genuinely
  optional facts.

`assertFamilyComplete` is the mint-time coverage assertion. Before a certificate
is produced, Check walks the relevant tree and asserts that every node in each
migrated family has the required facts. That assertion is what makes a
certificate operational rather than documentary.

## Query Shape

Queries are monadic from day one:

```lean
abbrev QueryM := ReaderM CompilerDB
```

Today queries are eager. Later, `QueryM` can become a state/cache/dependency
monad without migrating every call site. This is the Rust/Salsa lesson adapted
to Lean's lack of interior mutability: make the interface query-shaped now, turn
on memoization later.

## Hot-Read Rule

`CompilerDB` is canonical, but passes must not repeatedly hash-probe the same
fact in tight loops.

- Elab/Mono/Lower should bind required facts locally while processing a node or
  block.
- Whole-tree passes may materialize dense projections from contiguous ids when
  that is the right access pattern.
- Telemetry and complexity gates should watch query counts and scaling.

The goal is to remove semantic drift without recreating bug 027 at the center of
the compiler.

## Proof-Carrying Facts

`FactEntry` must be able to grow from checked evidence to kernel-checked
evidence without a model change:

- `evidenceClass = checked_by_stage` means a compiler verifier established it.
- `evidenceClass = proved_by_kernel` means a proof artifact replayed.
- `proofArtifact` and `replayCommand` make that replay explicit.

LLM-generated proof text is never evidence until the kernel accepts it and the
replay bundle works without the LLM.

## First-Class Consumers

The ladder is not only codegen:

- Elab reads source facts and produces Core.
- The interpreter consumes certified Core and remains the differential oracle.
- Proof extraction consumes certified Core and emits obligations tied to
  committed facts.
- Lower/SSA/backend stages add their own facts and provenance edges into the
  same DB.
- Reports, audit, LSP, and agent JSON must wrap DB/compiler facts; they do not
  restate semantic facts manually.

## Migration Plan

Landed substrate:

1. `CompilerDB.lean` replaces the superseded `FactLedger` name.
2. `IdentifiedProgram` exists as the post-desugar id-carrying source mirror.
3. The dead fat `TExpr`/`TStmt` path was removed.

Next load-bearing work:

1. Convert Check to consume `IdentifiedProgram`.
2. Convert Elab to consume the checked certificate instead of raw source syntax.
3. Migrate the literal family first: Check commits literal type facts, Elab
   requires them, and E0228-style Check/Elab type disagreement becomes
   unrepresentable.
4. Continue family by family: binops, casts, calls, aggregates, control flow,
   patterns.
5. Continue axis by axis: types, capabilities, ownership/value-flow,
   pass-agreement edges, proof-relevant facts.
6. Later, widen the same `CompilerDB` to Core/SSA/backend ids and provenance
   edges as those stages need structured facts, translation validation, and
   replay.

The docs always describe the objective. Status notes and commit history describe
the current slice.
