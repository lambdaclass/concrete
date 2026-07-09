# Fact-ledger certificate substrate — increment-0 spec (Phase 6.5 #9 ≡ #13b)

Status: **design locked, not yet implemented.** This is the spec the fresh
identity/ledger run builds against. No codemod has landed; `Concrete/Elab/
Typed.lean` (commit `f22e211a`) is additive and **superseded by this design** —
it is pared only once the new identity home exists (see "Migration of f22e211a").

## The decision, and the argument that forces it

The type-axis (and later capability/ownership/proof) certificate is a **thin
AST carrying stable node identity + a node/edge-keyed, fail-closed fact
ledger** — *not* a fat typed AST (`TExpr` with `ty`/`mode`/… inline).

The argument that ends the fat-vs-ledger debate: **some Concrete facts are
edges, not node fields.**

- pass agreement is a call-site ↔ parameter edge;
- borrow conflicts are edges;
- E0293 container-not-in-context is about *pairs* of paths;
- capability flow, proof dependencies, and invalidation dependencies are edges.

A per-node typed field cannot express any of these, so an edge-keyed structure
is required *regardless*. A fat AST therefore does not remove the ledger — it
leaves you with **both**. Given that, the ledger is the substrate and the AST
stays thin. This is not a preference; once you accept the edge facts, it is
forced.

Secondary (Lean-specific) reason: a fact-per-constructor is permanent walker
churn — every fact axis added to every AST constructor breaks every
`| .ctor …` match across parser/check/elab/mono/lower/interp/report/proof/
format, forever. A thin AST (one identity field) + a ledger pays that structural
cost once, not once per axis.

Withdrawn along the way: the "keep `ty`/`mode` inline (hybrid)" idea. `ty` does
not exist until Check; inline `ty` forces `Option Ty` on every node (`none`
until Check), which reintroduces the exact silent-fallback state we are killing.
The ledger models that lifecycle cleanly: `lookup : Option` while a family is
unmigrated, `require : fails closed` once it is.

## The invariant this exists for

> There is exactly one committed fact. Later stages cannot silently re-derive it.
> Missing (in a migrated family) fails closed. Conflicting fails closed.

That sentence is the whole design. Everything below is mechanism for it.

## Scope: a source-level ledger + per-IR certificates + provenance joins

This is NOT one global ledger keyed by `ExprId` for every stage. It cannot be:
`ExprId` lives on the ephemeral `IdentifiedProgram` and dies at Elab (Decision 1),
so Core/SSA/Backend nodes have no `ExprId` to key by. A single `ExprId`-keyed
store spanning all stages would force identity to thread through every downstream
IR (killing "ephemeral" and re-adding a per-node id to every IR) — the exact cost
the mirror-IR avoids.

So the architecture is a **certificate chain**, not a monolithic ledger:

- `FactLedger` (the source-level certificate; read "source fact ledger") —
  keyed by source identity (see the generalized key space below). Owns type,
  ownership, capability, pass-agreement, and must-use facts. This is "the
  ledger"; it is source-level, not global.
- `ValidatedCore`, `ValidatedSSA`, `ValidatedBackendIR` — each downstream IR is
  its **own** certificate carrying its **own** facts on its **own** node/value
  ids (Core shape + typed ops + proof *obligations*; SSA dominance + traps +
  widths; backend ABI/layout + helper calls).
- **Provenance** joins the layers: `SourceKey → CoreId → SSAId → BackendId →
  emitted location`.

This preserves the thesis without the contradiction: **each fact has exactly one
owning layer; no later layer re-derives another layer's fact; later layers add
their own facts; provenance links them.** That is a certificate chain, not a
second source of truth.

**Single-owner rule (no fact split across layers):** e.g. proof *eligibility*
(pure / capability-free) is a **source** fact; proof *obligations / VCs* are
**Core** facts. A fact belongs to one layer only.

### Provenance is itself a certified, fail-closed relation — not metadata

Because "no re-derivation" now leans on the join, provenance must obey the same
discipline as facts, or misattribution becomes the drift class one level down
(a report blaming the wrong source line, an obligation over the wrong node):

- **Coverage-asserted:** every lowering pass emits a provenance edge for every
  node it creates; a missing link is an ICE or an explicit `provenance-lost →
  backend-trusted` classification, never a silent wrong link.
- **Many-to-many, not 1:1:** one checked `+` → add + overflow-check + trap-block
  (1→many); optimization fuses/deletes nodes (many→1, and deletions). So
  provenance is a *relation* that semantics-preserving passes must **preserve /
  union / invalidate** — wired into the analysis-preservation/invalidation
  contract (ROADMAP 6.5 #8): fold ⇒ union origins; delete ⇒ invalidate dependent
  facts + provenance. A naive 1:1 map breaks on the first inline.

## The ladder

```
Parsed AST
  -> Resolved AST            (id-free)
  -> Desugared AST           (id-free; desugar may synthesize nodes freely)
  -> Identified AST          (mint pass: assign NodeIds, post-desugar / pre-Check)
  -> Check                   (populates the FactLedger; owns type/ownership/cap facts)
  -> TypedProgram { identified : IdentifiedProgram, ledger : FactLedger }
  -> Elab                    (READS facts; zero source-type re-inference)  -> Core
  -> CoreCheck -> ValidatedCore
       ├─> interpreter (oracle)        first-class consumer
       ├─> proof extraction / Lean / replay bundle   first-class consumer
       └─> Mono -> Lower -> SSA -> SSAVerify -> ValidatedSSA -> Emit
```

Boundary objects have hidden constructors so invalid states cannot be built
casually (`ResolvedProgram → Check → TypedProgram → Elab → CoreProgram →
CoreCheck → ValidatedCore → … → SSAVerify → ValidatedSSA`).

## Decision 1 — identity representation (was the recurring un-nailed crux)

**Parser/Resolve/Desugar stay id-free. A post-desugar mint pass produces an
`IdentifiedProgram`; Check consumes that.** No placeholder IDs anywhere.

Why this over "one `Node ExprKind` across the whole compiler":

- It confines the identity representation to the desugar→Check→Elab window —
  exactly where the ledger is used. Parser/resolve/desugar/mono/lower/interp/
  report are **untouched** by identity work.
- It avoids a "meaningless placeholder id" lifecycle state (the alternative
  makes every pre-Check node carry an id that doesn't exist yet, which must
  itself be fail-closed against premature reads — a new bug surface).
- The mint is a single deterministic traversal of the *one* post-desugar tree
  both Check and Elab consume, so ids are stable across the only boundary that
  matters, by construction — not recomputed independently (that would be
  traversal-order keying = span-in-disguise = a silent-desync channel, which is
  out for a fail-closed design).

`IdentifiedProgram` is a thin id-carrying tree (`id` + `span` + `kind`, **no
facts**) — a mechanical mirror of the post-desugar AST shapes. It is ephemeral:
it exists only between mint and Elab; after Elab everything is Core as today.

`NodeId` (currently spelled `ExprId` in `f22e211a`, reused): `{ moduleId :
String, localId : Nat }`, `BEq`/`Hashable`/`Repr`. Synthetic-node provenance
(which source construct a desugared node came from) rides a separate
`origin`/`span`, never conflated with identity.

## Decision 2 — hot-read rule (pure-thin's own 027-shaped risk)

`ty` is the hottest, most universal fact. Pure-thin means type facts live in the
ledger, so a naive design turns every type access in tight loops into
`ledger.requireFact id .type` — an O(1) HashMap probe, but not free, on the
center of the pipeline. That is the shape of bug 027 (this session's O(n²)
EmitSSA fix) relocated into the ledger. Rules:

- **The ledger is canonical, but the ledger is the Check→Elab *handoff*, not a
  global per-access oracle.** Elab reads a node's fact **once** to construct
  typed Core; Mono/Lower/Interp then read types off Core as they do today. The
  ledger is not hash-probed throughout the backend.
- Within a single checking/lowering step, a pass **binds a looked-up fact
  locally** and threads it through the local recursion; it does not re-probe the
  same fact repeatedly.
- Storage is `Std.HashMap`, never an assoc list (an assoc-list ledger read per
  node *is* bug 027 at the hottest path).
- A complexity/telemetry gate watches the Elab/Lower hot path; measure before
  declaring victory.

## FactLedger interface (the three fail-closed entry points)

```lean
structure ExprId where            -- the NodeId minted on IdentifiedProgram
  moduleId : String
  localId  : Nat
deriving BEq, Repr, Hashable, Inhabited

-- The key space is NOT ExprId-only: source facts attach to more than
-- expressions (a pass-agreement edge endpoint is a PARAMETER, not an expr).
inductive SourceKey  | expr (id : ExprId)   | stmt (id : StmtId)
                     | decl (id : DeclId)   | param (id : ParamId)
                     | type (id : TypeId)   | module (id : ModuleId)
deriving BEq, Repr, Hashable

inductive FactRole   | value | arg (idx : Nat) | field (name : String)
inductive EdgeKind   | passAgreement | borrowConflict | containerExclusion
                     | capabilityFlow | proofDep | invalidationDep
inductive FactKey    | node (key : SourceKey) (role : FactRole)
                     | edge (src dst : SourceKey) (kind : EdgeKind)  -- edges from day one
-- Owner is a SOURCE-level sub-stage only. Core/SSA/Backend facts do NOT live
-- here — they live in ValidatedCore/ValidatedSSA/ValidatedBackendIR, joined by
-- provenance (see "Scope"). This is SourceFactLedger, not a global ledger.
inductive FactStage  | check                                        -- owning source stage
inductive EvidenceClass | checked | inferred | assumed | trusted
inductive Fact       | type (ty : Ty) | ownership (mode : ValueMode)
                     | capability (caps : CapSet) | relational
structure FactEntry  where
  fact : Fact; owner : FactStage; evidence : EvidenceClass := .checked
  provenance : Span; deps : List FactKey := []
inductive TypedFamily | literals | binops | casts | calls
                      | aggregates | controlFlow | patterns

structure FactLedger where
  facts    : Std.HashMap FactKey FactEntry
  migrated : List TypedFamily := []

-- fail-closed on WRITE: committing a *different* fact for a committed key is an
-- ICE (idempotent re-commit of the identical fact is fine).
def insertFact  : FactLedger → FactKey → FactEntry → Except LedgerError FactLedger
-- optional read: ONLY for families not yet migrated.
def lookupFact  : FactLedger → FactKey → Option FactEntry
-- fail-closed on READ: absence in a migrated family is an ICE, never fallback.
def requireFact : FactLedger → TypedFamily → FactKey → Except LedgerError FactEntry
-- mint-time per-family completeness — the check that makes TypedProgram a
-- CERTIFICATE, not a comment. Runs before TypedProgram is constructed.
def assertFamilyComplete : FactLedger → TypedFamily → List FactKey → Except LedgerError Unit
```

`ValueMode` (`copy | move | borrow | reborrow | consume | place`) is the
ownership-fact payload — reused from `f22e211a`, no longer a syntax field.

## Fail-closed, precisely

- **conflict (write):** `insertFact` on a key already holding a *different* fact
  → `LedgerError.conflict` (an ICE). Enforces "exactly one committed fact."
- **missing in migrated family (read):** `requireFact` on an absent key whose
  family is migrated → `LedgerError.missingInMigratedFamily` (an ICE). Enforces
  "no silent re-derive."
- **completeness (mint):** `assertFamilyComplete` per migrated family runs in
  `TypedProgram.mk` — a gap is a loud internal error, not a silent pass. This
  per-family assertion is the operational meaning of "fail-closed per family."

## Staging (one axis/family at a time, in dependency order)

The ledger schema lists several `Fact`/`TypedFamily` slots, but increment 1 does
NOT fill them all. Migrate one family at a time, each with its own migration
flag, differential fuzzer + `test-ci-gates` as the net:

literals (the E0228 origin) → binops → casts → calls → aggregates →
controlFlow → patterns. Then widen fact axes: types → capability → ownership →
proof-relevant. Each flip: Check commits the fact, Elab switches from
re-inference to `requireFact`, and a gate proves Check's and Elab's answers
cannot disagree. When every family is migrated, delete Elab's source-type
inference.

## First-class non-codegen consumers (not a codegen-only ladder)

- **Interpreter (the differential oracle).** Must hang off the same certified
  artifact, never a parallel judgment. Partially already true: `IntArith`
  (Phase 6.5 #1) made interp's *arithmetic* single-sourced with opt/backend by
  construction — the ledger generalizes that to the type/ownership axes. Interp
  is the proof the pattern works, extended, not an afterthought.
- **Proof extraction / obligations / Lean replay.** `extractProofCore` consumes
  `ValidatedCore`; obligations point at committed facts (keyed by `NodeId`)
  instead of reconstructing typing side-conditions. The obligation → Lean →
  replay-bundle ladder is first-class output, not one compressed line.

## Migration of f22e211a

`Concrete/Elab/Typed.lean` (fat `TExpr`/`TStmt`) is **additive and not consuming
behavior** — nothing reads it. It is superseded by this design. Do NOT pare it
now: ripping it out before the new identity home (`IdentifiedProgram`) exists is
churn without value. Delete it as the **first coupled step of the fresh run**,
alongside introducing `IdentifiedProgram` + `FactLedger` — keeping `ExprId`
(→ `NodeId`) and `ValueMode` (→ ownership-fact payload).

## Increment-0 → increment-1 boundary

Increment-0 (this spec) is design only. The fresh run's first coupled move:
1. add `IdentifiedProgram` (thin id-carrying post-desugar tree) + the mint pass;
2. add `Concrete/Semantics/FactLedger.lean` (interface above);
3. delete `f22e211a`'s fat `TExpr`, keeping `ExprId`/`ValueMode`;
4. migrate the **literals** family: Check commits `type` facts, Elab
   `requireFact`s them, coverage-asserted at mint, differential-fuzzed.
