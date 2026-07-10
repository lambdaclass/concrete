import Concrete.Frontend.AST
import Std.Data.HashMap

/-!
# CompilerDB — the source-fact database (Phase 6.5 #9)

The long-term destination is one unified interned-ID query database with per-IR
certificate *views* and provenance as native edges (ROADMAP #9). This module is
the **minimal, pulled-by-need first landing** of that destination — deliberately
NOT the whole query engine, per the phase's "bug-class-pulled, not aesthetic"
rule:

- expr-keyed source facts only (`FactKey.node id role`); the generalized
  `SourceKey` (param/decl/type/module), relational/provenance `edge` keys,
  cross-IR id spaces, dependency tracking, and the intern counter are **deferred
  until a real family/caching/replay need pulls them** — not built ahead;
- but the **`QueryM` seam is here from day one**: queries run in `QueryM` (eager
  `ReaderM CompilerDB` now), so turning on memoization/incrementality later swaps
  ONLY `QueryM`'s definition, zero call-site churn — the one thing that must exist
  up front because Lean has no interior mutability;
- facts are proof-carrying (`proofArtifact`/`replayCommand`) — a cheap field that
  lets a fact grow `checked → proved` without a model change.

Invariant: exactly one committed fact; readers never re-derive; missing in a
migrated family fails closed; conflicting fails closed. Map-backed, never
assoc-list (bug 027 on a hot path). Design: `research/compiler/fact-ledger-design.md`.
-/

namespace Concrete

open Std

/-- Universal interned node identity: module + deterministic per-module local
    index, minted post-desugar (on `IdentifiedProgram`). NOT a span. -/
structure NodeId where
  moduleId : String
  localId  : Nat
deriving BEq, Repr, Hashable, Inhabited

/-- The identity minted on `IdentifiedProgram` expression nodes. -/
abbrev ExprId := NodeId

/-- The checked value-flow decision — payload of an ownership fact (deferred axis;
    kept as vocabulary). -/
inductive ValueMode where
  | copy | move | borrow | reborrow | consume | place
deriving BEq, Repr, Hashable, Inhabited

/-- Which sub-position of a node a fact is about. -/
inductive FactRole where
  | value | arg (idx : Nat) | field (name : String)
deriving BEq, Repr, Hashable, Inhabited

/-- Key of a committed fact. Expr-keyed for now (`node`); the relational/
    provenance `edge` variant and the generalized `SourceKey` are added when the
    first edge fact (pass agreement) / multi-IR need pulls them. -/
inductive FactKey where
  | node (id : NodeId) (role : FactRole)
deriving BEq, Repr, Hashable, Inhabited

/-- Which stage OWNS (may commit) a fact; readers may not write. -/
inductive FactStage where
  | check | elab | mono | coreCheck | lower
deriving BEq, Repr, Hashable, Inhabited

/-- How a fact is justified. `proved` = a kernel-checked proof (see
    `FactEntry.proofArtifact`). -/
inductive EvidenceClass where
  | checked | inferred | assumed | trusted | proved
deriving BEq, Repr, Hashable, Inhabited

/-- The fact payload — one constructor per semantic axis; migration fills them
    one at a time (types first, for the literals family). -/
inductive Fact where
  | type (ty : Ty)
  | ownership (mode : ValueMode)
  | capability (caps : CapSet)
deriving BEq, Repr, Inhabited

/-- A committed fact + metadata. Proof-carrying: `proofArtifact`/`replayCommand`
    let a fact escalate `checked → proved` without changing the model. (Dependency
    edges for invalidation are deferred to the #6 fact-dependency-graph work.) -/
structure FactEntry where
  fact          : Fact
  owner         : FactStage
  evidence      : EvidenceClass := .checked
  provenance    : Span
  proofArtifact : Option String := none
  replayCommand : Option String := none
deriving Inhabited

/-- Expr/syntax families, migrated one at a time. Once migrated, a missing fact
    for that family is a hard error (no fallback inference). -/
inductive TypedFamily where
  | literals | binops | casts | calls | aggregates | controlFlow | patterns
deriving BEq, Repr, Hashable, Inhabited

/-- DB errors are compiler-internal (an ICE). -/
inductive DBError where
  | conflict (key : FactKey)
  | missingInMigratedFamily (fam : TypedFamily) (key : FactKey)
deriving Repr

/-- The source-fact database: fact table + migrated-family set. Map-backed (hot
    path). The unified cross-IR DB, universal id space, and provenance edges are
    the destination this grows into, pulled by need. -/
structure CompilerDB where
  facts    : HashMap FactKey FactEntry := HashMap.emptyWithCapacity
  migrated : List TypedFamily := []

namespace CompilerDB

def empty : CompilerDB := {}

def isMigrated (db : CompilerDB) (fam : TypedFamily) : Bool := db.migrated.contains fam

def markMigrated (db : CompilerDB) (fam : TypedFamily) : CompilerDB :=
  if db.migrated.contains fam then db else { db with migrated := fam :: db.migrated }

/-- Commit a fact. **Fail-closed on conflict**: a DIFFERENT fact for an
    already-committed key is an error (idempotent re-commit is fine). -/
def insertFact (db : CompilerDB) (key : FactKey) (entry : FactEntry)
    : Except DBError CompilerDB :=
  match db.facts.get? key with
  | some existing =>
    if existing.fact == entry.fact then .ok db
    else .error (.conflict key)
  | none => .ok { db with facts := db.facts.insert key entry }

/-- Per-family completeness: every expected key must have a committed fact. The
    mint-time check that makes the certificate real, not a comment. -/
def assertFamilyComplete (db : CompilerDB) (fam : TypedFamily) (expectedKeys : List FactKey)
    : Except DBError Unit :=
  expectedKeys.forM fun k =>
    match db.facts.get? k with
    | some _ => .ok ()
    | none   => .error (.missingInMigratedFamily fam k)

end CompilerDB

/-- The query monad. Eager today (`ReaderM CompilerDB`); memoization/incremental
    dep tracking later swaps ONLY this definition — call-site transparent because
    queries are monadic from day one (Lean has no interior mutability). -/
abbrev QueryM := ReaderM CompilerDB

namespace Query

/-- Raw fact read — reserved for unmigrated / optional facts. -/
def factOf (key : FactKey) : QueryM (Option FactEntry) := do
  return (← read).facts.get? key

/-- **Fail-closed read** for a migrated family: absence is an ICE, never fallback. -/
def requireFact (fam : TypedFamily) (key : FactKey) : QueryM (Except DBError FactEntry) := do
  match (← read).facts.get? key with
  | some e => return .ok e
  | none   => return .error (.missingInMigratedFamily fam key)

/-- The committed type of a source node (`FactRole.value`), if present. -/
def typeOf (id : NodeId) : QueryM (Option Ty) := do
  match (← factOf (.node id .value)) with
  | some { fact := .type ty, .. } => return some ty
  | _ => return none

end Query

end Concrete
