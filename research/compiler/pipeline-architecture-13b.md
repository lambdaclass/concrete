# Pipeline Architecture: Certificate Ladder + Typed Fact Ledger

Date: 2026-07-09
Status: design record (the `record` step). Increment-0 not yet built.
Roadmap: Phase 6.5 #9 ≡ Phase 14 #13b (the two items collapse into this arc).
Research backing: [pipeline-lessons-2026-07.md](pipeline-lessons-2026-07.md).

This is the target architecture for the type/ownership/capability/proof axes,
reconciled after a long design pass. It is a design record, not a second
roadmap; the roadmap stays linear and #9/#13b point here.

## Thesis in one paragraph

Concrete's pipeline is a **certificate ladder over a single fact ledger**. Every
semantic fact (type, ownership, capability, pass-agreement, proof-relevance) is
committed exactly once by the stage that owns it, into one node/edge-keyed,
fail-closed `FactLedger`. Later stages *require* facts; they never re-derive
them. Each stage receives a certified input it cannot fabricate, adds its facts,
rejects invalid states, and emits either the next certificate or a source-linked
diagnostic. This is the Phase 6.5 fact-centralization thesis — already realized
for arithmetic (one `IntArith` meaning across interp/opt/backend) and for
capabilities (#5) — generalized to every axis, and enforced *structurally*
(private certificate constructors, fail-closed ledger reads) rather than by
after-the-fact gates.

The debate this record closes: **the AST stays thin; the ledger is the
certificate.** Some of Concrete's most important facts are not node-local
(`E0293` container-not-in-context is a pair of paths; pass-agreement is a
call-site↔parameter edge; borrow conflicts are edges), so an edge-keyed fact
structure is required *regardless*. A "fat typed AST" therefore does not remove
the ledger — it leaves you maintaining both a fat AST and a ledger, i.e. a
second source of truth. So facts go in the ledger, the AST carries only identity
+ syntax, and `TypedProgram` is the wrapper proving the ledger exists and is
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
   │  CHECK  ── validates AND commits facts to the ledger, per migrated family
   │           (Check's INPUT type is IdentifiedProgram, so an un-identified
   │            tree is a type error, not a discipline)
   ▼
 TypedProgram { ast, facts }         ◄── CERTIFICATE (private mk; mint asserts coverage)
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
This arc extends the pattern down to node-level facts via the ledger.

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

**Representation: a phase-indexed AST (trees-that-grow), not a placeholder-id
field.** The core AST is parameterized by an identity annotation:

```lean
inductive Expr (ι : Type)        -- ι = identity annotation for this phase
  | intLit  (id : ι) (span : Span) (v : …)
  | binOp   (id : ι) (span : Span) (op : …) (lhs rhs : Expr ι)
  | …
-- children carry ι too, so ids exist on every node or on none.

abbrev RawExpr    := Expr Unit     -- parser / resolve / desugar: NO id exists
abbrev IdExpr     := Expr NodeId   -- post-mint: every node has an id

mintIds : Expr Unit → Expr NodeId  -- the post-desugar traversal
```

Why phase-indexed over a `id : NodeId` field with a sentinel: a sentinel/`Option`
id is a *runtime* "unminted" state that a pass could read by mistake — a silent
wrong-key bug, exactly the class we are eliminating. With the phase index,
reading an id before mint is a **type error** (`Expr Unit` has no `NodeId`), so
"no id before mint" is compile-time-enforced and fail-closed by construction.
This is the identity analogue of "certificates have private constructors."

Fallback (only if the parameterization proves too invasive in Lean): a single
`Expr` with an `id : NodeId` field defaulted to a distinguished `NodeId.unminted`
sentinel, plus a runtime guard that any `require`/`lookup` on `unminted` is an
ICE. Strictly worse (runtime, not type-level) — prefer the phase index.

### 2.2 The ledger

```lean
inductive Role   | typ | ownership | capability | passAgreement | proof | …
inductive EdgeKind | borrowConflict | passAgreement | aliasOverlap | …

inductive FactKey                                   -- edges are mandatory from day 1
  | node (id : NodeId) (role : Role)
  | edge (from to : NodeId) (kind : EdgeKind)
deriving BEq, Hashable

structure Fact where
  payload      : FactPayload      -- TypeFact | OwnershipFact | CapabilityFact | …
  owningStage  : Stage
  evidence     : EvidenceClass    -- proved | enforced | reported | assumed | trusted
  deps         : List FactKey     -- for invalidation / the future query graph
  provenance   : Span
  -- invalidation rules live with the fact, not in ad-hoc pass logic.

abbrev FactLedger := Std.HashMap FactKey Fact       -- HashMap, NEVER an assoc-list.
```

`Std.HashMap`, not `List (FactKey × Fact)`: the ledger is read at the hottest
points in the compiler; an assoc-list reintroduces the O(n²) accumulation bug
(bug 027, fixed in EmitSSA this session) at the center of the pipeline. `NodeId`
and `FactKey` derive `Hashable` precisely so this is available.

### 2.3 Three access points (the fail-closed contract)

```lean
insert  : FactLedger → FactKey → Fact → Except ConflictError FactLedger
-- committing a DIFFERENT fact for an already-committed key is a hard error.
-- "No second truth", enforced at write.

require : Family → FactLedger → FactKey → Except ICE Fact
-- absence of a fact in a MIGRATED family is an internal compiler error.
-- "No silent re-derivation", enforced at read. This is what Elab uses.

lookup  : FactLedger → FactKey → Option Fact
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
on multiply-sourced types is a certificate on sand. The ledger schema has all
slots from day one, but increment N fills exactly one axis.

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
  facts : FactLedger
-- Pipeline.check is the only constructor; mk asserts per-family coverage.
```

### 2.6 Hot-read mitigation (DECISION)

Pure-thin means `ty` — the hottest, most-universal fact — is a ledger read.
Mitigation, so this does not become 027-at-the-center:

- **Read once per node per pass, thread locally.** A consuming pass (Elab,
  Lower) `require`s a node's facts once on entry and binds them in local context;
  children and uses reference the local binding, not a fresh `require` per
  access.
- **Dense projection for full-tree passes.** `NodeId`s are minted contiguously
  (`0..n`) in one traversal, so a pass that streams the whole tree can project
  the ledger's type facts into an `Array Ty` indexed by `NodeId` — O(1), no
  hashing, better than `HashMap` for that access pattern. The `HashMap` is the
  canonical sparse store; the dense array is a per-pass materialization.
- **Measure the Elab/Lower hot path** before declaring the pure-thin read
  pattern acceptable.

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
  retires the interp-as-second-pipeline risk by construction. The ledger
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

- **Increment 0 (substrate):** phase-indexed `Expr ι` + `mintIds` (post-desugar);
  `FactLedger` (HashMap, `node|edge` keys, `insert`/`require`/`lookup`);
  per-family coverage assertion in `TypedProgram.mk`. Land as its own verified
  green commit — this is a compiler-wide mechanical change and must not leave a
  non-compiling tree. Couple with paring f22e211a (below).
- **Increment 1 (types: literals):** the E0228 origin. Check `insert`s
  `TypeFact` for the literal family; Elab flips from re-infer to `require` for
  literals; `fuzz_differential.py` + `test-ci-gates` as the net.
- **Then, per family:** binops → calls → aggregates → match, each migrating
  `lookup`→`require`.
- **Then, per axis:** capability → ownership → pass-agreement (edges) → proof.
- **Later phases (roadmap homes exist):** translation validation (Phase 15 #18);
  `Fact.deps`/invalidation grow into the Rust-style query/fact graph for
  incremental + LSP + proof-cache; destination-passing (Zig RLS × linearity ×
  Hylo projections) as Phase 6.5 #10.

## Part 7 — Honest status

- ✅ Landed: `IntArith` (arithmetic single-sourced across interp/opt/backend);
  capabilities as one fact source (#5); `TypedProgram`/`ValidatedCore`/
  `ValidatedSSA` certificate seams (private constructors); `ExprId` + `ValueMode`
  from step 1 (f22e211a).
- 🔧 To pare: f22e211a's fat `TExpr`/`TStmt`/typed-syntax `TypedProgram` is a dead
  parallel IR — a second-truth-source — and is removed in increment 0. Only the
  *concepts* survive: `ExprId` → the ledger key (`NodeId`), `ValueMode` → the
  `OwnershipFact` payload.
- 🔨 To build: increment 0 substrate, then the per-family / per-axis migrations.

## Part 8 — Where the "cool ideas" attach

Condensed from [pipeline-lessons-2026-07.md](pipeline-lessons-2026-07.md):

- **Rust query/fact graph** → `Fact.deps` + invalidation; the ledger's long-term
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
117 claims, 25 adversarially verified). Code anchors: `Concrete/Pipeline/Pipeline.lean`
(certificate seams), `Concrete/Elab/Typed.lean` (f22e211a step-1 IR, to be pared),
`Concrete/Proof/ProofCore.lean` (`extractProofCore`).
