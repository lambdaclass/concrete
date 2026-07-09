import Concrete.Frontend.AST
import Std.Data.HashMap

/-!
# Fact ledger — the certificate substrate (Phase 6.5 #9 ≡ Phase 14 #13b)

The type-axis (and later capability/ownership/proof) certificate is a **thin AST
carrying stable node identity + a node/edge-keyed, fail-closed fact ledger** —
not a fat typed AST. The forcing argument: some Concrete facts are edges
(pass agreement is a call-site ↔ parameter edge; borrow conflicts are edges;
E0293 container-not-in-context is a path pair; capability/proof/invalidation
deps are edges), so an edge-keyed structure is required regardless — a fat AST
would leave you with *both*, so the ledger is the substrate and the AST stays
thin. Full design: `research/compiler/fact-ledger-design.md`.

The invariant this exists for:

> Exactly one committed fact. Later stages cannot silently re-derive it. Missing
> (in a migrated family) fails closed. Conflicting fails closed.

Map-backed on purpose: an assoc-list ledger read per node would reintroduce the
O(n²) accumulation that was bug 027. This module is the ledger interface; the
identity mint (`IdentifiedProgram`) and per-family population (Check commits,
Elab reads) are the stages that consume it.
-/

namespace Concrete

open Std

/-- Stable identity of a typecheckable node (expression or statement): module +
    a per-module local index assigned in a deterministic traversal of the
    post-desugar tree. The ledger key — NOT a span (a span says "where from" and
    generated nodes can share one; an id says "which node"). -/
structure ExprId where
  moduleId : String
  localId  : Nat
deriving BEq, Repr, Hashable, Inhabited

/-- The checked value-flow decision — the payload of an ownership fact once that
    axis is migrated (Concrete is linear). Fact vocabulary, not a syntax field. -/
inductive ValueMode where
  | copy | move | borrow | reborrow | consume | place
deriving BEq, Repr, Hashable, Inhabited

/-- Which sub-position of a node a fact is about (a call node carries a result
    type and one fact per argument position). -/
inductive FactRole where
  | value | arg (idx : Nat) | field (name : String)
deriving BEq, Repr, Hashable, Inhabited

/-- Relational (edge) fact kinds — the facts a fat AST could never hold. -/
inductive EdgeKind where
  | passAgreement | borrowConflict | containerExclusion
  | capabilityFlow | proofDep | invalidationDep
deriving BEq, Repr, Hashable, Inhabited

/-- The key of a committed fact: a node-local position, or an edge between nodes.
    Edges are admitted from day one (retrofitting when ownership/borrow facts
    land would be a schema break). -/
inductive FactKey where
  | node (id : ExprId) (role : FactRole)
  | edge (src dst : ExprId) (kind : EdgeKind)
deriving BEq, Repr, Hashable, Inhabited

/-- Which stage OWNS (may commit) a fact; reading stages may not write. -/
inductive FactStage where
  | check | elab | mono | coreCheck | lower
deriving BEq, Repr, Hashable, Inhabited

/-- How a committed fact is justified (the compiler's own evidence vocabulary). -/
inductive EvidenceClass where
  | checked | inferred | assumed | trusted
deriving BEq, Repr, Hashable, Inhabited

/-- The fact payload — one constructor per semantic axis; staging fills them one
    axis at a time (types first). -/
inductive Fact where
  | type (ty : Ty)
  | ownership (mode : ValueMode)
  | capability (caps : CapSet)
  | relational
deriving BEq, Repr, Inhabited

/-- A committed fact plus metadata: owner, justification, source provenance, and
    dependencies (for invalidation). -/
structure FactEntry where
  fact       : Fact
  owner      : FactStage
  evidence   : EvidenceClass := .checked
  provenance : Span
  deps       : List FactKey := []
deriving Inhabited

/-- Expression families, migrated one at a time. Once a family is migrated, a
    missing fact for it is a hard error (no fallback inference). -/
inductive TypedFamily where
  | literals | binops | casts | calls | aggregates | controlFlow | patterns
deriving BEq, Repr, Hashable, Inhabited

/-- Ledger errors are compiler-internal (an ICE): a certificate that fails to
    hold is never a silently-accepted program. -/
inductive LedgerError where
  | conflict (key : FactKey)
  | missingInMigratedFamily (fam : TypedFamily) (key : FactKey)
deriving Repr

/-- The fact ledger: `HashMap` from key to entry + the migrated-family set.
    Map-backed (read at every node) — never an assoc list (that is bug 027). -/
structure FactLedger where
  facts    : HashMap FactKey FactEntry
  migrated : List TypedFamily := []

namespace FactLedger

def empty : FactLedger := { facts := HashMap.emptyWithCapacity }

def isMigrated (l : FactLedger) (fam : TypedFamily) : Bool := l.migrated.contains fam

def markMigrated (l : FactLedger) (fam : TypedFamily) : FactLedger :=
  if l.migrated.contains fam then l else { l with migrated := fam :: l.migrated }

/-- Commit a fact. **Fail-closed on conflict**: a DIFFERENT fact for an
    already-committed key is an error (idempotent re-commit is fine). -/
def insertFact (l : FactLedger) (key : FactKey) (entry : FactEntry)
    : Except LedgerError FactLedger :=
  match l.facts.get? key with
  | some existing =>
    if existing.fact == entry.fact then .ok l
    else .error (.conflict key)
  | none => .ok { l with facts := l.facts.insert key entry }

/-- Optional read — ONLY for families not yet migrated. -/
def lookupFact (l : FactLedger) (key : FactKey) : Option FactEntry := l.facts.get? key

/-- **Fail-closed read** for a migrated family: absence is an ICE, never
    fallback re-inference. -/
def requireFact (l : FactLedger) (fam : TypedFamily) (key : FactKey)
    : Except LedgerError FactEntry :=
  match l.facts.get? key with
  | some e => .ok e
  | none   => .error (.missingInMigratedFamily fam key)

/-- Mint-time per-family completeness: every expected key must have a committed
    fact. This is the check that makes `TypedProgram` a certificate. -/
def assertFamilyComplete (l : FactLedger) (fam : TypedFamily) (expectedKeys : List FactKey)
    : Except LedgerError Unit :=
  expectedKeys.forM fun k =>
    match l.facts.get? k with
    | some _ => .ok ()
    | none   => .error (.missingInMigratedFamily fam k)

end FactLedger
end Concrete
