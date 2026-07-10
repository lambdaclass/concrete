# Pipeline Architecture: Certificate Ladder + Typed Core + CompilerDB

Date: 2026-07-09
Status: objective design record. The Check/Elab `TypeJudgment` migration is
still ahead; `CompilerDB` remains future infrastructure pulled by relational
facts.
Roadmap: Phase 6.5 #9 ≡ Phase 14 #13b (the two items collapse into this arc).
Research backing: [pipeline-lessons-2026-07.md](pipeline-lessons-2026-07.md).

This is the target architecture for the type/ownership/capability/proof axes,
reconciled after a long design pass. It is a design record, not a second
roadmap; the roadmap stays linear and #9/#13b point here.

> **Canonical substrate spec:** [`fact-ledger-design.md`](fact-ledger-design.md)
> is the authoritative design for typed Core + future `CompilerDB`. This
> document is the broader ladder/lessons synthesis; where they
> overlap, defer to `fact-ledger-design.md`.

## Thesis in one paragraph

Concrete's pipeline commits each fact into its strongest existing
representation, not a per-IR certificate chain and not one all-purpose map.
Source type facts are computed by one shared `TypeJudgment` and carried by
already-typed Core (`CExpr.ty`). Relational, provenance, dependency, evidence,
replay, report, LSP, agent, and query facts live in a future unified interned-ID
`CompilerDB` when those facts are pulled by real consumers. Later stages consume
the owning representation; they never re-derive another answer.

Why hybrid: edge facts force a relational store, but source type facts should not
be weakened from "carried by typed Core" to "detected missing by DB lookup." The
placement rule is: source type -> shared `TypeJudgment` -> `CExpr.ty`;
relational/cross-stage/evidence/provenance/query -> future `CompilerDB`. If both
apply, typed Core owns the structural fact and the DB owns the relation/evidence
about it.

## Part 1 — The certificate ladder

```
 SOURCE TEXT
   │  lex / parse
   ▼
 ParsedProgram            untyped AST, no ids
   │  resolve                        names → stable identities
   ▼
 ResolvedProgram
   │  desugar                        destructuring-let, if/while-let, for, defer, …
   ▼
 DesugaredProgram         Expr Unit — no ids
   │  CHECK                         uses shared TypeJudgment; validates ownership/caps/etc.
   ▼
 Elab                            uses same TypeJudgment; stamps CExpr.ty
   ▼
 Core (CExpr)             thin typed compiler IR
   │  CORECHECK                              ┌───────────────┬────────────────────┐
   ▼                                         ▼               ▼                    │
 ValidatedCore  ◄── CERTIFICATE       INTERP ORACLE     PROOF BRANCH              │
   │  MONO                            (differential)    extract → obligations     │
   ▼                                  arith single-      → Lean kernel replay      │
 MonomorphizedCore                    sourced via        → replay bundle           │
   │  POST-MONO CORECHECK             IntArith                                     │
   ▼                                                                              │
 ValidatedMonoCore  ◄── CERTIFICATE                                               │
   │  LOWER                                                                       │
   ▼                                                                              │
 SSA                                                                              │
   │  SSAVERIFY                                                                   │
   ▼                                                                              │
ValidatedSSA  ◄── CERTIFICATE                                                    │
   │  cleanup / opt   (semantics-preserving; may NOT erase traps / change checked arith)
   ▼                                                                              │
BackendIR                                                                        │
   │  BACKENDIR VERIFY (preserve traps/spans/layout/caps/helper calls)            │
   ▼                                                                              │
ValidatedBackendIR ◄── CERTIFICATE                                               │
   │  emit LLVM first; future native/C/WASM/QBE-like emitters consume same cert   │
   ▼                                                                              │
LLVM / backend artifact intent                                                   │
   │  TRANSLATION VALIDATION  (Alive2 / CompCert-style — Phase 15 #18)            │
   ▼                                                                              ▼
 TranslationValidated | BackendTrusted                                    EvidenceBundle
   │                                                              (per-fact evidence class:
   ▼                                                               proved@source/Core,
 NATIVE ARTIFACT ────────────────────────────────────────────►    validated-through-SSA,
                                                                   backend-trusted,
                                                                   runtime-checked, tested,
                                                                   assumed)
```

Certificate types (`ValidatedCore`, `ValidatedSSA`, …) already exist as
stage-granularity tokens with private constructors in `Concrete/Pipeline/Pipeline.lean`.
This arc extends the pattern to source-type agreement (`TypeJudgment` ->
`CExpr.ty`) plus DB edge/evidence facts.

`BackendIR` is part of the certificate ladder even if LLVM remains the only
emitter. Its value is a structured, validated backend contract between
`ValidatedSSA` and textual LLVM/native artifacts: runtime checks, trap spans,
layout/ABI decisions, helper calls, target constants, and capability/trust facts
remain inspectable and auditable before they disappear into backend text.

## Part 2 — The substrate

### 2.1 Future Source Identity For DB-Owned Facts

Source ids are useful for DB-owned facts such as pass-agreement edges, borrow
conflicts, E0293, diagnostics, and provenance. They are not the type carrier.
Core `CExpr` is the type carrier. Do not introduce source identity solely for
the type-axis fix; introduce it when DB-owned relational/evidence/provenance
facts need it.

- **Mint post-desugar, not at resolve.** Desugar synthesizes new nodes; minting
  after it means every node Check/Elab see has a real id, desugar never has to
  be id-aware, and ids are trivially stable across the Check→Elab boundary
  (both consume the *same* post-mint tree). This dissolves the "preserve ids
  through desugar" problem entirely.
- **Identity ≠ provenance.** A synthetic node gets a fresh `NodeId` plus a
  separate `origin : Span` for diagnostics. The two are never conflated; a
  desugared node is a first-class node with its own identity.

**Preferred representation if pulled: a distinct, ephemeral, id-carrying mirror
IR.** Not a field on the shared `Expr`, and not a phase-indexed `Expr ι`; a
separate shape-preserving mirror of the post-desugar AST that adds `id : ExprId`
+ `span` per node and **no facts**, living only across the consumers that need
source identity:

```lean
structure IdExpr where            -- id-carrying, fact-free, ephemeral
  id   : ExprId
  span : Span
  kind : IdExprKind               -- mirrors Expr's shapes; children are IdExpr

mintIds : Expr → IdExpr           -- deterministic post-desugar traversal
```

Three options were on the table: (a) a `NodeId` field on the shared `Expr`
(rejected — placeholder id upstream + permanent walker churn across every pass);
(b) a phase-indexed `Expr ι` / trees-that-grow (`Expr Unit` upstream, `Expr
NodeId` after mint — type-enforced "no id before mint", but parameterizing the
core AST touches every match); (c) the **distinct mirror**. (c)
wins on confinement if/when needed: parser/resolve/desugar/mono/lower/interp/report are
*untouched* by identity, and it is fail-closed *by exhaustiveness* — `mintIds`
must pattern-match every `Expr` constructor, so a new source form won't compile
until `IdExprKind` + the mint handle it (no silent drift between `Expr` and its
mirror). The cost — a second shape kept in sync — is compile-time-caught, not
silent. This is the identity analogue of "certificates have private
constructors."

### 2.2 TypeJudgment And CompilerDB

`TypeJudgment` is the type-axis sibling of `IntArith`: one shared implementation
for source expression typing/defaulting that Check and Elab both call. Elab
stamps `CExpr.ty` from it. Check uses it for type-dependent checks. A gate proves
the two cannot diverge on literals first, then the rest of the expression
families.

### 2.3 Future CompilerDB keys

`CompilerDB` is the future unified relation, evidence, provenance, dependency,
and query store. It is not the primary home for node-local structural facts that
typed IR can enforce. The key space is **`SourceKey`, not `ExprId`-only** — a
pass-agreement endpoint is a *parameter*, not an expression. The objective
widens the same DB with Core/SSA/backend ids and provenance edges as those
stages start committing structured relations/evidence.

```lean
inductive SourceKey | expr (id : ExprId) | stmt (id : StmtId) | decl (id : DeclId)
                    | param (id : ParamId) | type (id : TypeId) | module (id : ModuleId)
inductive Role      | typ | ownership | capability | passAgreement | proof | …
inductive EdgeKind  | borrowConflict | passAgreement | containerExclusion | …

inductive FactKey                                   -- edges mandatory from day 1
  | node (key : SourceKey) (role : Role)
  | edge (src dst : SourceKey) (kind : EdgeKind)
deriving BEq, Hashable

structure FactEntry where
  payload      : FactPayload      -- PassAgreement | BorrowConflict | Evidence | …
  owningStage  : FactStage        -- Check now; Core/SSA/backend later in the same DB
  evidence     : EvidenceClass    -- proved | enforced | reported | assumed | trusted
  deps         : List FactKey     -- invalidation / the future query graph
  provenance   : Span

structure CompilerDB where
  facts : Std.HashMap FactKey FactEntry             -- HashMap, NEVER an assoc-list.
```

`Std.HashMap`, not `List (FactKey × Fact)`: the DB is read at the hottest
points in the compiler; an assoc-list reintroduces the O(n²) accumulation bug
(bug 027, fixed in EmitSSA this session) at the center of the pipeline.

### 2.4 Three access points (the fail-closed contract)

```lean
insertFact  : CompilerDB → FactKey → FactEntry → Except ConflictError CompilerDB
-- committing a DIFFERENT fact for an already-committed key is a hard error.
-- "No second truth", enforced at write.

requireFact : Family → FactKey → QueryM FactEntry
-- absence of a fact in a MIGRATED family is an internal compiler error.
-- "No silent re-derivation", enforced at read. This is what Elab uses.

factOf : FactKey → QueryM (Option FactEntry)
-- exists ONLY for un-migrated families (the staging fallback). As each family
-- migrates, its call sites move from `lookup`+infer to `require`.
```

For DB-owned facts, the certificate reduces to: **one committed fact;
conflicting fails closed at `insert`; missing (migrated) fails closed at
`require`.** For node-local typed facts, the guarantee is stronger: the typed IR
cannot be constructed without the field.

### 2.5 Fact axes and staging order

Committed **bottom-up in dependency order, one axis at a time**, each with its
own "migrated" flag and one primary home:

```
Typed Core: TypeFact (`CExpr.ty`)
Future CompilerDB facts: capability flow, pass agreement, borrow/container
                         edges, provenance, proof/evidence/dependency facts
```

Types first because every other axis reads them; a capability certificate riding
on multiply-sourced types is a certificate on sand. But types land in the
existing typed Core, not DB-primary facts and not a new typed source tree.

### 2.6 Completeness

Core nodes are already typed by construction. For migrated source families, Elab
must stamp `CExpr.ty` from shared `TypeJudgment`; missing/private fallback
typing is a bug. Coverage assertions remain for DB-owned relational/evidence
families and transition periods.

```lean
inductive CExpr where
  | intLit (val : Int) (ty : Ty)
  | binOp (op : BinOp) (lhs rhs : CExpr) (ty : Ty)
  -- ...
```

### 2.7 Hot-read mitigation (DECISION)

Typed Core is the hot path for `ty` and other local structural facts. DB queries
are for relational/evidence/provenance facts. Mitigation, so this does not
become 027-at-the-center:

- **Read once per node per pass, thread locally.** A consuming pass (Elab,
  Lower) reads typed fields directly and only `require`s DB relations/evidence
  when needed.
- **Dense projection for full-tree passes.** Source `NodeId`s are minted contiguously
  (`0..n`) in one traversal, so a pass that streams the whole tree can project
  DB facts into an `Array` indexed by `NodeId` when useful — O(1), no
  hashing, better than `HashMap` for that access pattern. The `HashMap` is the
  canonical sparse store; the dense array is a per-pass materialization.
- **Measure the Elab/Lower hot path** before declaring the pure-thin read
  pattern acceptable.

### 2.8 Provenance is a certified, fail-closed relation

Because layers are joined by provenance **edges in the one `CompilerDB`**
(`SourceKey → CoreId → SSAId → BackendId → emitted location`), provenance is
load-bearing and must obey the same discipline as facts — otherwise
misattribution becomes the drift class one level down (a report blaming the
wrong source line, an obligation over the wrong node):

- **Coverage-asserted:** each lowering emits a provenance edge for every node it
  creates; a missing link is an ICE or an explicit `provenance-lost →
  backend-trusted` classification, never a silent wrong link.
- **Many-to-many, not 1:1:** one checked `+` → add + overflow-check + trap-block
  (1→many); optimization fuses/deletes (many→1, deletions). So provenance is a
  relation that semantics-preserving passes **preserve / union / invalidate** —
  wired into the analysis-preservation/invalidation contract (Phase 6.5 #8):
  fold unions origins; delete invalidates dependent facts + provenance. A naive
  1:1 map breaks on the first inline.

Single-owner rule: each fact lives in exactly one layer (proof *eligibility* is
a source fact; proof *obligations* are Core facts) — never split across layers.

## Part 3 — The uniform stage contract

Every stage obeys the same contract:

1. **receives** a certified input it cannot fabricate (private constructors);
2. **commits** local structural facts into typed IR and DB-owned facts via
   `insert` (conflict → hard error);
3. **requires** upstream typed fields / DB facts; never re-derives them;
4. **rejects** invalid states with source-linked diagnostics;
5. **emits** the next certificate or diagnostics — never a partial certificate.

Per MLIR: **verifiers run before AND after every pass** (not only at stage
boundaries), cheap-structural checks before expensive-semantic ones, so a pass
that corrupts the IR is caught immediately rather than downstream.

## Part 4 — Three branches, all first-class

The ladder is not codegen-only. Off the certified core:

- **Codegen spine:** Core → SSA → backend → artifact.
- **Interpreter oracle:** consumes the same `ValidatedCore`, differential-tested
  against compiled output. Its arithmetic axis is *already* single-sourced with
  opt/backend via `IntArith` — the existence proof that fact-centralization
  retires the interp-as-second-pipeline risk by construction. Typed IR plus the
  future `CompilerDB` generalize that guarantee to the type/ownership/relational
  axes.
- **Proof branch:** `ValidatedCore` → `extractProofCore` → obligations → Lean
  kernel replay → replay bundle. Tools may *suggest* proofs; the kernel
  *decides*; the replay bundle is the artifact.

## Part 5 — Hard invariants

1. **One committed fact.** Missing (migrated) fails closed; conflicting fails closed.
2. **No hidden second pipeline.** `check / test / run / prove / report / audit /
   LSP / agent-JSON / CI-gates` all consume the same certified artifacts. They
   stop at different rungs; they never recompute their own facts.
3. **Certificates require certified input** (private constructors): no
   un-type-checked AST into Elab, no raw Core into Lower, no raw SSA into Emit.
   This closes the H12-class bypass by construction.
4. **`IntArith` is the one arithmetic meaning** across interp/opt/verifier/
   backend. Optimization may not erase traps or change checked arithmetic.
5. **Evidence stays classed** — no single green badge; every artifact fact
   carries its evidence class.

## Part 6 — Increment plan

- **Next increment (shared type judgment):** extract
  `Concrete/Semantics/TypeJudgment.lean` for source-expression
  typing/defaulting. Check calls it for type-dependent checks. Elab calls it to
  stamp `CExpr.ty`. No second typed source IR is introduced; typed Core is the
  structural carrier that already exists.
- **Increment 1 (types: literals):** the E0228 origin. Literal/defaulting
  decisions move into `TypeJudgment`; Check and Elab both route through it; Elab
  private fallback typing for the migrated literal cases is deleted. `CompilerDB`
  may record evidence/provenance for the decision, but not a second literal type
  truth. An E0228 red-team gate proves Check and Elab cannot disagree.
- **Then, per family:** binops → casts → calls/type arguments → aggregates →
  match/if → patterns/places, each migrating source type decisions into
  `TypeJudgment`; relational facts continue to land in DB edges.
- **Then, per axis:** capability → ownership → pass-agreement (edges) → proof.
- **Later DB pull:** introduce `CompilerDB` only when relation/evidence/
  provenance consumers need it, then grow Core/SSA/backend ids, provenance
  edges, dependency/invalidation tracking, and Rust-style query caching as
  translation validation, replay, LSP, and package facts pull them.
  Destination-passing (Zig RLS × linearity × Hylo projections) remains Phase
  6.5 #10.

## Part 7 — Honest status

- ✅ Landed: `IntArith` (arithmetic single-sourced across interp/opt/backend);
  capabilities as one fact source (#5); `TypedProgram`/`ValidatedCore`/
  `ValidatedSSA` certificate seams (private constructors).
- 🔨 To build: `TypeJudgment`; Check and Elab both use it for literal/defaulting
  decisions; Elab stamps `CExpr.ty` from it; an E0228 agreement gate; then the
  per-family / per-axis migrations. Future `CompilerDB`/identity work is pulled
  by relational/evidence/provenance consumers, not by type drift.

## Part 8 — Where the "cool ideas" attach

Condensed from [pipeline-lessons-2026-07.md](pipeline-lessons-2026-07.md):

- **Rust query/fact graph** → `FactEntry.deps` + invalidation; `CompilerDB`'s long-term
  form.
- **MLIR** → verify before/after every pass; structural-then-semantic ordering;
  per-dialect verifiers ↔ certificate types.
- **Zig** → InternPool (pointer-free, serializable) as the substrate for caching
  facts across incremental runs; Result Location Semantics → destination-passing
  (#10). RLS was bug-prone in Zig — adopt the vocabulary, not the mechanism.
- **Hylo** → second-class references, `let/inout/sink/set`, the exclusivity law;
  the theory under scoped callbacks / projections.
- **Vale** → generational-reference *dynamic fallback* for liveness facts the
  linear checker cannot prove statically (discharged as `checked_dynamically`,
  never `proved`) — Phase 20 research note.
- **Verified-compiler peer group** (Cogent, CompCert, CakeML, F\*/Low\*) +
  **Alive2** → the real comparison set for the proof/translation-validation
  story (Phase 14/15), not Zig/Odin/Gleam.

## Sources

Design synthesized across the 2026-07-09 sessions; external claims validated by
the deep-research pass recorded in `pipeline-lessons-2026-07.md` (26 sources,
117 claims, 25 adversarially verified). Code anchors:
`Concrete/Pipeline/Pipeline.lean` (certificate seams),
`Concrete/Proof/ProofCore.lean` (`extractProofCore`).
