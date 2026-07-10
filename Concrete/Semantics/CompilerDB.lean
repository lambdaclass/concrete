import Concrete.Frontend.AST
import Std.Data.HashMap

/-!
# CompilerDB — the unified interned-ID fact database (Phase 6.5 #9 ≡ #5)

The certificate substrate is ONE database, not a per-IR certificate chain joined
by hand-maintained provenance maps: if `SourceKey → CoreId → SSAId → BackendId`
links must be maintained, they ARE facts and belong as first-class edges in the
same DB. So there is a single `CompilerDB` with a universal interned-ID space, a
single fact table, and provenance as native edges; per-IR "certificates" are
query-views over it. Interned IDs (#5) and node identity (#9) are one substrate.

Access is **query-shaped from day one, eager for now**: `typeOf`/`ownershipOf`/…
run in `QueryM`, evaluated eagerly (no cache, no incrementality — that full
engine is a later Phase-19 bet). The Lean-specific reason `QueryM` exists now:
Lean has no interior mutability, so a bare `db → id → Except` query cannot
transparently gain a cache later — turning on memoization must swap only
`QueryM`'s definition (`ReaderM CompilerDB` now → `StateT QueryCache …` later)
with zero call-site churn. Writing queries non-monadically now = paying the
call-site migration later.

The invariant: exactly one committed fact; later stages read, never re-derive;
missing (in a migrated family) fails closed; conflicting fails closed. Facts are
also proof-carrying — a fact escalates `checked_by_stage → proved_by_kernel`
without changing the model. Map-backed (never assoc-list — that is bug 027 on a
hot path). Design: `research/compiler/fact-ledger-design.md`, ROADMAP #9.
-/

namespace Concrete

open Std

/-- The universal interned node identity: module + a deterministic per-module
    local index (minted post-desugar). NOT a span. Human output shows source
    names; caches / evidence / replay use this. -/
structure NodeId where
  moduleId : String
  localId  : Nat
deriving BEq, Repr, Hashable, Inhabited

/-- Back-compat alias: the identity minted on `IdentifiedProgram` expression
    nodes. `ExprId` is a `NodeId`; the distinct downstream id spaces
    (`CoreNodeId`, `SSAValueId`, …) join the same DB when those layers land. -/
abbrev ExprId := NodeId

/-- Which kind of source node an id refers to. A source fact attaches to more
    than expressions — a pass-agreement edge endpoint is a PARAMETER, not an
    expr — so the key space is generalized beyond `ExprId`. -/
inductive SourceKey where
  | expr (id : NodeId)
  | stmt (id : NodeId)
  | decl (id : NodeId)
  | param (id : NodeId)
  | type_ (id : NodeId)
  | module_ (id : NodeId)
deriving BEq, Repr, Hashable, Inhabited

/-- The checked value-flow decision — payload of an ownership fact (Concrete is
    linear). Fact vocabulary, not a syntax field. -/
inductive ValueMode where
  | copy | move | borrow | reborrow | consume | place
deriving BEq, Repr, Hashable, Inhabited

/-- Which sub-position of a node a fact is about (a call carries a result type
    and one fact per argument). -/
inductive FactRole where
  | value | arg (idx : Nat) | field (name : String)
deriving BEq, Repr, Hashable, Inhabited

/-- Relational (edge) fact kinds — facts a per-node field could never hold, plus
    the provenance/lowering edges that make this one DB rather than a chain. -/
inductive EdgeKind where
  | passAgreement | borrowConflict | containerExclusion
  | capabilityFlow | proofDep | invalidationDep
  | lowersTo | emitsAs | trapSource
deriving BEq, Repr, Hashable, Inhabited

/-- The key of a committed fact: a node-local position, or an edge between nodes
    (relational facts AND provenance links). -/
inductive FactKey where
  | node (key : SourceKey) (role : FactRole)
  | edge (src dst : SourceKey) (kind : EdgeKind)
deriving BEq, Repr, Hashable, Inhabited

/-- Which stage OWNS (may commit) a fact; reading stages may not write. -/
inductive FactStage where
  | check | elab | mono | coreCheck | lower
deriving BEq, Repr, Hashable, Inhabited

/-- How a committed fact is justified. `checked` = a checker rule established it;
    `proved` = a kernel-checked proof (see `FactEntry.proofArtifact`). -/
inductive EvidenceClass where
  | checked | inferred | assumed | trusted | proved
deriving BEq, Repr, Hashable, Inhabited

/-- The fact payload — one constructor per semantic axis; staging fills them one
    axis at a time (types first). -/
inductive Fact where
  | type (ty : Ty)
  | ownership (mode : ValueMode)
  | capability (caps : CapSet)
  | relational
deriving BEq, Repr, Inhabited

/-- A committed fact plus metadata. Proof-carrying: `proofArtifact`/`replayCommand`
    let a fact escalate `checked_by_stage → proved_by_kernel` without changing the
    model — proofs are a field of the fact system, not a separate universe. -/
structure FactEntry where
  fact         : Fact
  owner        : FactStage
  evidence     : EvidenceClass := .checked
  provenance   : Span
  deps         : List FactKey := []
  proofArtifact : Option String := none   -- path/handle to a kernel-checked proof, if proved
  replayCommand : Option String := none   -- command that re-establishes the fact
deriving Inhabited

/-- Expression/syntax families, migrated one at a time. Once migrated, a missing
    fact for that family is a hard error (no fallback inference). -/
inductive TypedFamily where
  | literals | binops | casts | calls | aggregates | controlFlow | patterns
deriving BEq, Repr, Hashable, Inhabited

/-- DB errors are compiler-internal (an ICE): a certificate that fails to hold is
    never a silently-accepted program. -/
inductive DBError where
  | conflict (key : FactKey)
  | missingInMigratedFamily (fam : TypedFamily) (key : FactKey)
deriving Repr

/-- The one compiler fact database: interned-id fact table + migrated-family set
    + an intern counter for minting fresh ids. Per-IR certificates are views over
    this; provenance lives here as `edge` facts. Map-backed (hot path). -/
structure CompilerDB where
  facts    : HashMap FactKey FactEntry := HashMap.emptyWithCapacity
  migrated : List TypedFamily := []
  nextId   : Nat := 0

namespace CompilerDB

def empty : CompilerDB := {}

def isMigrated (db : CompilerDB) (fam : TypedFamily) : Bool := db.migrated.contains fam

def markMigrated (db : CompilerDB) (fam : TypedFamily) : CompilerDB :=
  if db.migrated.contains fam then db else { db with migrated := fam :: db.migrated }

/-- Mint a fresh interned local id (module-scoped). -/
def freshId (db : CompilerDB) (moduleId : String) : NodeId × CompilerDB :=
  ({ moduleId, localId := db.nextId }, { db with nextId := db.nextId + 1 })

/-- Commit a fact. **Fail-closed on conflict**: a DIFFERENT fact for an
    already-committed key is an error (idempotent re-commit is fine). -/
def insertFact (db : CompilerDB) (key : FactKey) (entry : FactEntry)
    : Except DBError CompilerDB :=
  match db.facts.get? key with
  | some existing =>
    if existing.fact == entry.fact then .ok db
    else .error (.conflict key)
  | none => .ok { db with facts := db.facts.insert key entry }

end CompilerDB

/-- The query monad. Eager today (`ReaderM CompilerDB`); memoization/incremental
    dependency tracking later swaps ONLY this definition (Lean has no interior
    mutability, so queries must be monadic now for that swap to be call-site
    transparent). Query-shaped access is the long-term-DB interface without
    building the full engine yet. -/
abbrev QueryM := ReaderM CompilerDB

namespace Query

/-- Raw fact read — reserved for unmigrated / optional facts. -/
def factOf (key : FactKey) : QueryM (Option FactEntry) := do
  return (← read).facts.get? key

/-- **Fail-closed read** for a migrated family: absence is an ICE, never fallback
    re-inference. -/
def requireFact (fam : TypedFamily) (key : FactKey) : QueryM (Except DBError FactEntry) := do
  match (← read).facts.get? key with
  | some e => return .ok e
  | none   => return .error (.missingInMigratedFamily fam key)

/-- The committed type of a source node (`FactRole.value`), if present. -/
def typeOf (key : SourceKey) : QueryM (Option Ty) := do
  match (← factOf (.node key .value)) with
  | some { fact := .type ty, .. } => return some ty
  | _ => return none

/-- The committed ownership/value-flow mode of a source node, if present. -/
def ownershipOf (key : SourceKey) : QueryM (Option ValueMode) := do
  match (← factOf (.node key .value)) with
  | some { fact := .ownership m, .. } => return some m
  | _ => return none

/-- The committed capability requirement of a source node, if present. -/
def capsOf (key : SourceKey) : QueryM (Option CapSet) := do
  match (← factOf (.node key .value)) with
  | some { fact := .capability c, .. } => return some c
  | _ => return none

/-- Evidence class / proof handle for a fact key. -/
def evidenceOf (key : FactKey) : QueryM (Option (EvidenceClass × Option String)) := do
  match (← factOf key) with
  | some e => return some (e.evidence, e.proofArtifact)
  | none => return none

end Query

/-- Per-family completeness: every expected key must have a committed fact. The
    mint-time check that makes the certificate real, not a comment. -/
def CompilerDB.assertFamilyComplete (db : CompilerDB) (fam : TypedFamily)
    (expectedKeys : List FactKey) : Except DBError Unit :=
  expectedKeys.forM fun k =>
    match db.facts.get? k with
    | some _ => .ok ()
    | none   => .error (.missingInMigratedFamily fam k)

end Concrete
