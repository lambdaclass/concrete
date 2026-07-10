# Pipeline Architecture: Certificate Ladder + CompilerDB

Date: 2026-07-09
Status: objective design record. The first substrate slices are landed; the
Check/Elab migrations are still ahead.
Roadmap: Phase 6.5 #9 ≡ Phase 14 #13b (the two items collapse into this arc).
Research backing: [pipeline-lessons-2026-07.md](pipeline-lessons-2026-07.md).

This is the target architecture for the type/ownership/capability/proof axes,
reconciled after a long design pass. It is a design record, not a second
roadmap; the roadmap stays linear and #9/#13b point here.

> **Canonical substrate spec:** [`fact-ledger-design.md`](fact-ledger-design.md)
> is the authoritative design for the identity + `CompilerDB` substrate and is
> what the code (`Concrete/Elab/Identified.lean`,
> `Concrete/Semantics/CompilerDB.lean`) grows toward. This document is the
> broader ladder/lessons synthesis; where they
> overlap, defer to `fact-ledger-design.md`.

## Thesis in one paragraph

Concrete's pipeline commits facts into **one unified interned-ID `CompilerDB`**,
not a per-IR certificate chain. Every semantic fact (type, ownership, capability,
pass-agreement, proof-relevance) is committed exactly once by the stage that owns
it. The **source-level facts** (committed by Check) are keyed by source identity
+ edges; **each downstream IR** (`ValidatedCore`, `ValidatedSSA`,
`ValidatedBackendIR`) remains a boundary token, but its certificate is a **view /
query-group** over the same DB — its facts live in `CompilerDB` under
`CoreId`/`SSAId`/`BackendOpId` keys, and the layers are joined by **provenance
edges IN the DB** (`lowersTo`/`emitsAs`/`trapSource`), not a separate store or a
hand-maintained side map. Later stages *require* facts; they never re-derive
them. Each stage receives a certified input it
cannot fabricate, adds its facts, rejects invalid states, and emits either the
next certificate or a source-linked diagnostic. This is the Phase 6.5
fact-centralization thesis — already realized for arithmetic (one `IntArith`
meaning across interp/opt/backend) and capabilities (#5) — generalized to every
axis and enforced *structurally* (private certificate constructors, fail-closed
reads) rather than by after-the-fact gates.

Why one DB, not a chain: `ExprId` lives on the ephemeral `IdentifiedProgram` and
dies at Elab — but that forces separate id *kinds* inside one store, not separate
stores. The DB holds a universal interned-id space (`ExprId`, `CoreId`, `SSAId`,
`BackendOpId`, …); Core/SSA/backend facts key on `CoreId`/`SSAId`/`BackendOpId`,
so `ExprId` is never re-threaded downstream, yet everything lives in one DB with
provenance as native edges (`lowersTo`/`emitsAs`/`trapSource`). Each fact still
has exactly one owning layer — a single source of truth *per fact*, and now a
single store too. (If those provenance links must be maintained, they ARE facts
and belong in the DB as edges, not a hand-maintained side map — which is the
argument that retired the earlier per-IR-chain framing.)

The debate this record closes: **the AST stays thin; `CompilerDB` is the
certificate.** Some of Concrete's most important facts are not node-local
(`E0293` container-not-in-context is a pair of paths; pass-agreement is a
call-site↔parameter edge; borrow conflicts are edges), so an edge-keyed fact
structure is required *regardless*. A "fat typed AST" therefore does not remove
the DB need — it leaves you maintaining both a fat AST and a separate fact
store, i.e. a second source of truth. So facts go in `CompilerDB`; the AST
carries only identity + syntax where that stage needs identity; and
`TypedProgram` is the wrapper proving the relevant DB facts exist and are
complete for the migrated families.

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
   │  identify (★ mint NodeIds)      deterministic post-desugar traversal
   ▼
 IdentifiedProgram        Expr NodeId — every node carries a stable id
   │  CHECK  ── validates AND commits source facts to CompilerDB, per migrated family
   │           (Check's INPUT type is IdentifiedProgram, so an un-identified
   │            tree is a type error, not a discipline)
   ▼
 TypedProgram { ast, db }            ◄── CERTIFICATE (private mk; mint asserts coverage)
   │  ELAB  ── require()s facts (fail-closed); does NOT re-infer
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
This arc extends the pattern down to node/edge facts through `CompilerDB`.

`BackendIR` is part of the certificate ladder even if LLVM remains the only
emitter. Its value is a structured, validated backend contract between
`ValidatedSSA` and textual LLVM/native artifacts: runtime checks, trap spans,
layout/ABI decisions, helper calls, target constants, and capability/trust facts
remain inspectable and auditable before they disappear into backend text.

## Part 2 — The substrate

### 2.1 Identity representation (DECISION)

`NodeId` is minted **once, in a deterministic traversal of the post-desugar
tree, immediately before Check**. This is the single unavoidable AST cost. Two
consequences fixed here:

- **Mint post-desugar, not at resolve.** Desugar synthesizes new nodes; minting
  after it means every node Check/Elab see has a real id, desugar never has to
  be id-aware, and ids are trivially stable across the Check→Elab boundary
  (both consume the *same* post-mint tree). This dissolves the "preserve ids
  through desugar" problem entirely.
- **Identity ≠ provenance.** A synthetic node gets a fresh `NodeId` plus a
  separate `origin : Span` for diagnostics. The two are never conflated; a
  desugared node is a first-class node with its own identity.

**Representation (as shipped in `Concrete/Elab/Identified.lean`): a distinct,
ephemeral, id-carrying mirror IR — `IdExpr`.** Not a field on the shared `Expr`,
and not a phase-indexed `Expr ι`; a separate shape-preserving mirror of the
post-desugar AST that adds `id : ExprId` + `span` per node and **no facts**,
living only between the mint pass and Elab:

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
core AST touches every match); (c) the **distinct mirror** that shipped. (c)
wins on confinement: parser/resolve/desugar/mono/lower/interp/report are
*untouched* by identity, and it is fail-closed *by exhaustiveness* — `mintIds`
must pattern-match every `Expr` constructor, so a new source form won't compile
until `IdExprKind` + the mint handle it (no silent drift between `Expr` and its
mirror). The cost — a second shape kept in sync — is compile-time-caught, not
silent. This is the identity analogue of "certificates have private
constructors."

### 2.2 CompilerDB keys and current source-fact slice

`CompilerDB` is the unified fact store. The current implementation starts with
the source-fact slice, because Check/Elab disagreement is the first bug class to
remove. The key space is **`SourceKey`, not `ExprId`-only** — a pass-agreement
endpoint is a *parameter*, not an expression. The objective widens the same DB
with Core/SSA/backend ids and provenance edges as those stages start committing
structured facts.

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
  payload      : FactPayload      -- TypeFact | OwnershipFact | CapabilityFact | …
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

### 2.3 Three access points (the fail-closed contract)

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

The whole certificate reduces to: **one committed fact; conflicting fails closed
at `insert`; missing (migrated) fails closed at `require`.**

### 2.4 Fact axes and staging order

Committed **bottom-up in dependency order, one axis at a time**, each with its
own "migrated" flag:

```
TypeFact  →  CapabilityFact  →  OwnershipFact (from ValueMode)
          →  PassAgreementFact (edge)  →  ProofRelevantFact
```

Types first because every other axis reads them; a capability certificate riding
on multiply-sourced types is a certificate on sand. `CompilerDB` has the common
shape from day one, but increment N fills exactly one axis.

### 2.5 Completeness = mint-time coverage assertion

`TypedProgram` is minted only by Check. Its constructor is private, and minting
**walks the tree and asserts, per migrated family, that every node has its
committed fact** — a gap is a loud ICE, not a silent pass. That assertion is the
operational meaning of the certificate: you cannot construct a `TypedProgram`
whose migrated families are incomplete.

```lean
structure TypedProgram where
  private mk ::
  ast   : IdExpr-program       -- thin, id-carrying, post-desugar
  db    : CompilerDB
-- Pipeline.check is the only constructor; mk asserts per-family coverage.
```

### 2.6 Hot-read mitigation (DECISION)

Pure-thin means `ty` — the hottest, most-universal fact — is a DB query.
Mitigation, so this does not become 027-at-the-center:

- **Read once per node per pass, thread locally.** A consuming pass (Elab,
  Lower) `require`s a node's facts once on entry and binds them in local context;
  children and uses reference the local binding, not a fresh `require` per
  access.
- **Dense projection for full-tree passes.** Source `NodeId`s are minted contiguously
  (`0..n`) in one traversal, so a pass that streams the whole tree can project
  `CompilerDB` type facts into an `Array Ty` indexed by `NodeId` — O(1), no
  hashing, better than `HashMap` for that access pattern. The `HashMap` is the
  canonical sparse store; the dense array is a per-pass materialization.
- **Measure the Elab/Lower hot path** before declaring the pure-thin read
  pattern acceptable.

### 2.7 Provenance is a certified, fail-closed relation

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
2. **commits** its own facts via `insert` (conflict → hard error);
3. **requires** upstream facts via `require`; never re-derives them;
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
  retires the interp-as-second-pipeline risk by construction. `CompilerDB`
  generalizes that guarantee to the type/ownership axes.
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

- **Landed substrate:** the ephemeral `IdExpr` mirror IR + `mintIds`
  (post-desugar); `CompilerDB` (HashMap-backed source-fact slice,
  `node|edge` keys, `QueryM`, `insertFact`/`requireFact`/`factOf`);
  the dead fat `TExpr`/`TStmt` path removed. This is the first slice of the
  objective DB, not the final width.
- **Next increment (wire Check/Elab):** Check consumes `IdentifiedProgram`,
  constructs `TypedProgram { ast, db }`, and `TypedProgram.mk` asserts
  per-family coverage.
- **Increment 1 (types: literals):** the E0228 origin. Check `insert`s
  `TypeFact` for the literal family; Elab flips from re-infer to `require` for
  literals; `fuzz_differential.py` + `test-ci-gates` as the net.
- **Then, per family:** binops → calls → aggregates → match, each migrating
  `lookup`→`require`.
- **Then, per axis:** capability → ownership → pass-agreement (edges) → proof.
- **Later widening:** the same `CompilerDB` grows Core/SSA/backend ids,
  provenance edges, dependency/invalidation tracking, and Rust-style query
  caching as translation validation, replay, LSP, and package facts pull them.
  Destination-passing (Zig RLS × linearity × Hylo projections) remains Phase
  6.5 #10.

## Part 7 — Honest status

- ✅ Landed: `IntArith` (arithmetic single-sourced across interp/opt/backend);
  capabilities as one fact source (#5); `TypedProgram`/`ValidatedCore`/
  `ValidatedSSA` certificate seams (private constructors); `IdentifiedProgram`;
  `CompilerDB`; the fat `TExpr`/`TStmt` path removed.
- 🔨 To build: Check consumes `IdentifiedProgram`; Elab consumes the checked
  certificate; then the per-family / per-axis fact migrations.

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
`Concrete/Elab/Identified.lean` (post-desugar identity),
`Concrete/Semantics/CompilerDB.lean` (query-shaped fact substrate),
`Concrete/Proof/ProofCore.lean` (`extractProofCore`).
