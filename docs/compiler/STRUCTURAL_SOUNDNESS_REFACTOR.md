# Structural soundness refactor

Status: planned, 2026-10-18. Owners: R-0491 – R-0500 in [ROADMAP.md](../../ROADMAP.md), with
R-0299, R-0300, R-0353 and R-0460 pulled in where named. Reviewed by Codex before publication
(15 findings, all applied).

## Why

One afternoon of read-only hunting (2026-10-18) after bugs 079 and 080 produced nine more defects,
081 – 089. They are instances of four structural causes:

| cause | bugs | what the structure allows |
|---|---|---|
| **The language's meaning is implemented more than once** | 080, 084, 085, 086, 088 | `Proof.eval` (`Proof.lean`, 3,411 lines) keeps its own integer, cast, match and statement semantics; the interpreter (`Interp.lean`, 23 `partial def`s) and the SSA folder (`SSACleanup.lean`) share checked arithmetic through `IntArith` but not the rest; extraction (`ProofCore.lean`) translates statements by its own rules. Nothing forces agreement; `--report model-diff` (bug 080) samples it on shapes the corpus uses. |
| **Evidence is decided by name** | 079, 081, 083, 089 | Status is computed separately by contracts, vcs, obligation-ledger, audit, check-proofs and receipts (27 source occurrences of `"proved"` and 43 `proved_by_lean` substrings across `Report.lean`, `ReportObligations.lean` and `Main.lean`, including comments and longer status names — these count spellings, not independent authorities). Callees, theorems and specs are joined by `String`. |
| **Names are not unique after resolution** | 039 – 041, 044 – 045, 081, 082, 087 | `CExpr.ident`, `CStmt.letDecl`/`assign`, `CMatchArm.enumArm` bindings, `structLit`/`enumLit` field keys, `fnRef`, `Callee`, SSA registers/blocks/phi predecessors and every `PExpr` binder are `String`. Elab alpha-renames only match payloads. |
| **Prose claims enforcement nothing performs** | 080's comment, `Backend.lean` contract strings, R-18 – R-20 "preservation" | Comments, docs and emitted contract text say "excluded by eligibility", "wrap by default", "discharged"; no gate is tied to them. |

Point fixes close instances. This plan removes the causes, so each class becomes unrepresentable
in the IR types or mechanically caught, and states for each stage which mechanisms it retires, the
check that proves it, and how it migrates without a flag day. Each stage also states what it does
**not** retire.

## Principles

1. **One definition per meaning.** Each semantic fact has exactly one implementation; every
   consumer calls it.
2. **Identities, not spellings.** After resolution, no IR, table or evidence record joins on a
   `String`. Display names live in side tables used only for printing.
3. **Invariants live in types.** A value violating an invariant cannot be constructed (private
   constructors, smart constructors returning `Except`), or is rejected by the verifier that runs
   immediately after the pass producing it.
4. **One authority per verdict, fed by checked evidence.** One function decides each verdict from
   evidence that binds the exact proposition; surfaces render its result.
5. **The compiler states the claim.** A proof's statement is generated from a typed claim
   specification and the code; the author supplies the proof.
6. **Enforcement is demonstrated, not cited.** A claim that something is excluded, checked or
   guaranteed is tied to a gate leg *and* a targeted mutation that leg must kill.
7. **Every comparison counts its comparisons.** A differential or gate leg fails when it compares
   too few cases, so it cannot pass vacuously.

## Stages

S1, S2, S4, S8, S9 and S10 start now in parallel. S3 needs S2's identity types. S5 needs S2, S3
and S4. S6 needs S2 and S3 and may develop alongside S5 against a stable semantics interface. S7
needs S3. S11 needs S3 and S7. S12 consumes S4 and S5.

```
S1 ─────────────────────────────────────────────► (continuous)
S2 ──► S3 ──┬──► S5 ──► S12
S4 ─────────┘    S6 (beside S5)
            S3 ──► S7 ──► S11
S8, S9, S10 ────────────────────────────────────► (continuous; S10 links into S6)
```

Each slice builds, passes the full gate set, is reviewed by Codex and is published before the next
slice builds on it (see *Discipline*).

### S1 — Prevention harness (R-0491)

Catch remaining instances while the structural stages remove their causes. Depends on bug 080
landing (`--report model-diff` and `check_model_diff.sh` are on its branch).

- **Three-way differential fuzzer.** Extend `scripts/tests/fuzz_differential.py` (interpreter vs
  binary) with the extracted model as a third side, over generated programs whose grammar covers
  every shape that broke: sequential assignments in loop bodies, `let` after assignment, `return`
  inside loops, if-without-else fall-through inside loops, discarded `expr;` in branches, block
  `let` shadowing, positional match bindings with renamed binders, unhinted literals at and beyond
  2^63 and 2^64, casts across every width pair.
- **Outcome classes, not skips.** Each comparison is classified as value, trap, rejection,
  unsupported extraction, fuel exhaustion or crash. Today the fuzzer skips rejections and timeouts
  and still exits successfully (`fuzz_differential.py` ~:297, ~:346); that stops.
- **model-diff coverage.** Zero-argument functions, `bool` parameters as booleans, non-scalar
  signatures (structs by declaration order, enums by variant index, arrays, newtypes resolved), a
  compiled-binary leg through an injected test driver, and both-sides-fail reported as an error.
  Its claimed-function inventory is pinned, so a function silently dropping out fails the gate.
- **Scheduled hunts.** The four 2026-10-18 hunts (model vs runtime, name-keyed evidence,
  stated-but-unenforced invariants, adversarial false proof) and S9's checker hunt run before every
  release candidate; each reproducer becomes a fixture.

**Exit:** each of 080 – 089 has a checked-in bug record, reproducer, expected outcome, pre-fix and
fixed SHA, and deterministic seed; the fuzzer fails at each pre-fix SHA and passes at the fixed
SHA; each generated feature has a minimum number of *completed* comparisons per run, and the gate
fails below it or on a missing inventory entry; model-diff's binary leg agrees on every claimed
function with a supported signature.

### S2 — Resolved identities everywhere (R-0492)

Addresses the name-collision mechanisms in 039 – 041, 044 – 045, 081, 082 and 087. It does **not**
retire resolver exhaustiveness (042), FFI representation/lifetime errors (043), or unsupported
proof attribution (083, owned by S4); those need their own controls.

- **Identity vocabulary.**
  - `LocalId`: function-unique, for every binder (block `let`, loop, match arm, parameter, borrow
    binder, pattern binder, callable environment entry). Source checking runs **before** Elab and
    keeps its own name-keyed ownership environment, which is where 093 and 094 live
    (`CheckHelpers.lean` ~:386, ~:556) — so identities are allocated at Resolve and consumed by
    SourceCheck as well as by Elab, not created by Elab after checking has already run.
  - `DefId`: a compilation-scoped declaration handle allocated **before** body elaboration. It
    preserves the callable namespaces (definition vs callable binding, R-0442) and specialization
    arguments, and distinguishes extern and intrinsic declarations. It is *not*
    `DefinitionIdentity`, which carries an implementation digest built from checked facts much
    later (`ProofCore.lean` ~:2954); after checking, the compiler constructs and validates the
    `DefId → DefinitionIdentity` mapping at evidence boundaries.
  - `FieldIdx`/`VariantIdx` with an owner check: every index is verified against its owning type
    (bounds, arity, field type).
  - `RegId`, `BlockId`, `LoopId` for SSA registers, blocks, phi predecessors and branch targets
    (today `String`, `SSA.lean` ~:17, ~:65, ~:75); `TypeId` is retained.
- **Core and SSA changes.** `ident`, `letDecl`, `assign`, `enumArm` bindings, `structLit`/`enumLit`
  fields, field access/update, `fnRef`, both `Callee` forms (direct and indirect), and SSA
  registers/blocks use the identity types.
- **`ElabVerify`** after Elab: each `LocalId` bound once and used in scope; each `DefId` exists in
  its namespace; each field/variant index belongs to its owner, in bounds, with matching arity and
  type.
- **`PExpr`** binders and calls use `LocalId`/`DefId`; `FnTable` is keyed by `DefId`, so a theorem
  table's entry for a call is the callee itself.
- **Language rules**, each removing an ambiguity:
  - a block `let` may not shadow a name visible in an enclosing scope (new diagnostic);
  - an import alias colliding with a name already in scope is an error;
  - a match binding equals its field name (`Full { value }`) or names it (`Full { value: v }`).
- **Migration.** Identities are allocated through explicit scope-aware environments, and consumers
  migrate through checked adapters one at a time. There are **no** `String → identity` coercions —
  a string alone cannot determine a fresh binder, its scope or its declaring owner (the R-0002
  `Coe` worked only because it wrapped an already-canonical name). Hashed identities use
  capture-preserving alpha-canonicalization before hashing; frozen identity bytes
  (`ImplementationIdentity.lean`) are either preserved or explicitly versioned with the affected
  identities invalidated. Display snapshots are normalized separately.

**Exit:** `check_ir_no_string_identity.sh` finds no `String`-typed binder, callee, field key,
register or block reference in `Elab/Core.lean`, `IR/*.lean` or `Proof/Proof.lean` outside display
tables; `ElabVerify` runs in every pipeline; the 081, 082 and 087 reproducers are diagnostics or
bind correctly; mutation families prove `ElabVerify`'s scope, ownership and arity checks and the
theorem-table key check load-bearing.

### S3 — Typed operations and one arithmetic (R-0493)

Retires the width mechanisms of 080 and 088. Depends on S2.

- Integer operations carry their operand `IntTy` (`add i32`, `div u64`, `shl u8`), filled by the
  checker; boolean, floating-point and pointer operations remain distinct node kinds.
- **Literals.** Syntactic signed literals are canonicalized before range checking, preserving
  every signed minimum (`-128 : i8` stays valid, `Check.lean` ~:189,
  `tests/programs/numeric/valid_negative_literals.con`). Contextual literal types are resolved
  before construction; `intLit` is built only by a smart constructor that range checks; generic
  instantiations are re-validated after Mono. An unhinted literal takes its type from context or
  is an error.
- `cast` carries source and target types.
- **One arithmetic.** The interpreter and the SSA folder already delegate checked arithmetic to
  `IntArith` (`Interp.lean` ~:234, `SSACleanup.lean` ~:398); `Proof.eval` keeps separate integer
  semantics, and the folder's cast and comparison paths operate on unnormalized constants (088).
  S3 completes the sharing and enforces it: `check_single_arithmetic.sh` refuses integer
  wrap/range/overflow/division/shift/cast computations outside `IntArith`.
- **Width-aware proof values.** `PVal.int` carries its `IntTy`; overflowable arithmetic yields an
  explicit no-overflow obligation instead of unbounded `Int`, so an unbounded-`Int` theorem cannot
  bind to an `i8` body (red-team `rt_ovf`) and an `iff` cannot be shown over code that traps.
- `Backend.lean`'s contract text is generated from the trap helpers actually emitted.

**Exit:** the `intLit` constructor is private; every integer op in Core, SSA and `PExpr` carries a
width; the arithmetic lint passes; signed-minimum fixtures still pass; 088's C1/C2 reproducers agree
on model, interpreter and binary; the `rt_ovf` binding is refused.

### S4 — One evidence authority (R-0494)

Retires the surface-disagreement and name-attribution mechanisms of 079, 083 and 089.

- `Concrete/Proof/Verdict.lean` defines `Verdict` with a **private** constructor for discharging
  cases. `Verdict.decide` consumes checked evidence that binds the **exact proposition, clause,
  subject, coverage, dependency closure and environment** — a private constructor alone could still
  bless an unrelated replay plus matching metadata, the failure `DependencyEdge.lean` ~:448
  documents.
- One `ClaimJudgment` per claim and per `#[ensures]`/`#[requires]` clause, computed once per
  compilation. Receipt decoding and currency checks *feed* this authority (receipt consumption is
  fresh validation, `Receipt.lean` ~:593, not rendering). Contracts, vcs, obligation-ledger, audit,
  check-proofs (text and JSON), proof-status and receipt issuance are pure renderers.
- Delete `Proof.provedFunctions` and every name-substring replay check.
- `check_single_verdict_authority.sh` refuses status literals outside `Verdict.lean` and the
  renderer table.
- Split `Report.lean` (6,405 lines) and `Main.lean` (4,194 lines) by surface; surface modules
  import `Verdict` and not checker internals.

**Exit:** status spellings appear only in the renderer table; 089's four reproducers read
not-proved on every surface; the cross-surface gate pins the expected judgment — positive and
negative — for every claim and clause in the corpus, not only agreement between surfaces.

### S5 — Compiler-generated claim statements (R-0495)

Retires author-chosen statements as a mechanism for 079, 081 and 089, and author-declared coverage.
Depends on S2, S3 and S4.

Today `#[spec]` names a `PExpr`; the property itself lives only in the theorem
(`proofs/Examples/ProofPatterns/Proofs.lean` ~:28), and the existing statement generator leaves the
result as `sorry` (`Report.lean` ~:2726). Point and invariant theorems cannot imply a universal
equation. So:

- Introduce a typed `ClaimSpec`: input domain, assumptions, conclusion and coverage form,
  including point inputs and loop invariants. Source attributes produce a `ClaimSpec`.
- Generate each claim's Lean statement from its `ClaimSpec` and the extracted code (body inserted
  by value, callees by `DefId`). The author supplies a proof of exactly that statement.
- Coverage is derived from the `ClaimSpec` form; `#[proof_coverage]` becomes a checked assertion.
- Migration: adapt or reprove existing theorems against generated statements, keeping weaker
  coverage where that is what was proved; then switch authority and delete
  `scripts/gen/classifications.lean`'s theorem scan and the subject-binding table generator.

**Exit:** every positive claim in `examples/`, `std/` and `tests/` replays against its generated
statement; intentional negative fixtures are refused; the theorem scan is deleted; coverage labels
are checked.

### S6 — One total Core semantics (R-0496)

Retires the model/runtime divergence mechanisms of 080, 084, 085 and 086 at their root. Depends on
S2 and S3.

- `Concrete/Semantics/CoreEval.lean`: a total, fuel-indexed big-step evaluator over post-S3 Core
  using `IntArith`. Its result distinguishes **returned value, trap and fuel exhaustion**. Fuel
  exhaustion is neither runtime-failure evidence nor termination evidence.
- The interpreter becomes `CoreEval` plus I/O; its `partial def`s go away.
- If `PExpr` is kept for proof ergonomics, prove: fuel monotonicity of both evaluators;
  extraction is defined on every admitted program; and correspondence under related
  environments/values, with explicit fuel transport or existential sufficient fuel — an
  implication only about successful evaluations would be vacuous at zero fuel (`Proof.eval`
  ~:1164 returns `none` there). Otherwise proofs are about Core directly.
- **Scope.** S6 establishes Core/`PExpr` semantic correspondence on the admitted subset. R-0460
  separately proves each Core → VC rule, including hypothesis soundness (`VC_BRIDGE_REGISTER.md`).
  Core → SSA → LLVM preservation remains trusted until S11 validates it.

**Exit:** no `partial def` in the interpreter or `CoreEval`; the three correspondence lemmas are
proved in Lean (or `PExpr` is retired); every claim is about `CoreEval` or transported by them.

### S7 — Typed pass contracts (R-0497)

Depends on S3. The pipeline becomes a typed stage sequence matching the implementation's order:

`Parse → Resolve → SourceCheck → Elab → ElabVerify → CoreCheck → Mono → MonoVerify → Lower →
LowerVerify → SSAVerify → Fold → FoldVerify → SSAVerify → Emit`

- Verified artifacts have **private constructors**; each certificate refers to its exact payload;
  any mutation returns an unverified artifact. Diagnostic artifacts (today's public artifact
  structures and `lowerUnverified`, `Pipeline.lean` ~:48, ~:161) get separate types that Emit and
  the evidence APIs reject.
- Compile-fail controls cover constructor forgery, certificate reuse after mutation and the
  diagnostic-path bypass.
- **Scope.** `FoldVerify` and `LowerVerify` initially check structural and type invariants only
  (constants normalized to their type; Core types preserved); they do **not** retire semantic
  miscompilation — replacing `1 + 2` with a typed `4`, or dropping a trapping operation, passes
  both. The semantic versions are rewrite certificates preserving values, traps and effects for
  Fold (which includes algebraic rewrites, `SSACleanup.lean` ~:447) and a stated Core/SSA
  simulation for Lower; S11 owns them.
- One mutation family per verifier; per-stage timing and IR size reported.

**Exit:** `Pipeline.lean` exposes only the typed sequence; compile-fail controls pass; every
verifier's mutation family is killed in a qualified campaign.

### S8 — Enforcement demonstrated, not cited (R-0498)

- A manifest maps each enforcement claim → gate leg → targeted mutation. Qualification runs the
  positive fixture and requires the targeted mutation to fail *that* leg. Citation existence alone
  establishes traceability, not enforcement.
- Scope: `docs/verification`, `docs/language`, doc comments and enforcement-bearing ordinary
  comments in `Concrete/Proof`, `Concrete/Check`, `Concrete/Backend`, and emitted contract text
  (`Backend.lean` ~:38 string data).
- Register rows R-18 – R-20 are relabelled shape-only until S6 discharges them.

**Exit:** every enforcement phrase in scope has a manifest row; every row's targeted mutation is
killed by its leg in a qualified campaign.

### S9 — The checker's rules, specified and attacked (R-0499)

The first checker hunt (2026-10-18, after the proof-layer hunts) reproduced five accepted
violations and one miscompile:

| bug | violation | mechanism |
|---|---|---|
| 090 | double free: a match guard consumes a linear value, a later arm consumes it again | each guard is checked in an environment reset per arm (`Check.lean` ~:981 – 998) |
| 091 | effect with no capability on the chain: an impl method declares more capabilities than its trait, reached through generic dispatch | impl validation compares return types only (`CoreCheck.lean` ~:985 – 999); no post-Mono capability check |
| 092 | use after free: a reference stored in a struct field does not freeze its source | `borrowedFrom` recorded only for a bare `let x = &y` (`Check.lean` ~:1483) |
| 093 | leak: a shadowing block local or match binder escapes the never-consumed check | after-block comparison by name (`CheckHelpers.lean` ~:556) |
| 094 | leak: consuming a match binder consumes the shadowed outer variable | `consumeVar` marks every entry with the name (`CheckHelpers.lean` ~:386) |
| 095 | stale read: a struct literal taking `&s` after `s` changed in a loop points at an old copy | codegen, the C8 address-of-local class on the struct-literal path |

With 087's shadowing, the hunt also produced a heap use-after-free through a borrow block. Four of
the six are the name-keyed mechanism S2 removes; the other two (090, 092) are rules that were never
stated precisely enough to check exhaustively. The language's guarantees — linear ownership,
references never returned or escaping, capabilities covering every reachable effect, the
`Unsafe`/trusted boundary, bounds checks — need the same treatment the proof layer is getting.

- **Rule specification.** `docs/language/CHECKER_RULES.md` states each rule as a judgment over Core
  (post-S2 identities, so a name cannot dodge a rule): use-once for non-Copy; every path consumes or
  destroys each linear value; no reference in a returned value or escaping its frame; a call's
  required capabilities ⊆ the caller's declared set — directly, through function-pointer target
  sets, generic instantiations, trusted bodies and FFI-declared effects; raw-pointer dereference
  only under `Unsafe`. Each rule has an S8 manifest row.
- **Rule-violation fuzzer.** Takes accepted programs and applies one rule-breaking mutation
  (duplicate a move, drop a linear value on one path, return a reference through a
  struct/enum/array, reach a capability through a pointer, alias or generic, dereference a raw
  pointer outside `Unsafe`). Every mutant must be rejected with that rule's diagnostic; generated
  well-formed programs must be accepted.
- **Runtime witnesses.** An instrumented interpreter mode tracks moves/destroys per allocation,
  reference frames and capability use; a use after move, double destroy, leaked linear value,
  escaped reference or undeclared capability use on any accepted program is a failure.
- **Checker hunt** joins the scheduled hunts (S1).

**Exit:** every rule has an enforcing site, a manifest row and a mutation class; the fuzzer's CI
seed set rejects every mutant and accepts every well-formed program, with minimum completed counts
per rule; the instrumented interpreter runs the corpus and every fuzzer program with zero
violations; each reproduced checker defect is a permanent fixture.

### S10 — Disputed language rules written down (R-0500)

Several defects were two components disagreeing with no document saying which was right (087
shadowing, 084 match binding, 086 statement value, 088 literal typing). Until S6's evaluator is the
executable specification, each such rule is written once in `docs/language/`:

- block scoping and shadowing; match-binding positions and names; statement vs value position
  inside branches and loops; sequential assignment within one loop iteration; literal typing and
  range (including signed minima); cast semantics per width pair; evaluation order of arguments
  and fields; short-circuit evaluation;
- each rule: one normative paragraph, at least one accepted and one rejected example, and the
  model/interpreter/binary fixture that checks it;
- when S6 lands, each paragraph names the `CoreEval` clause implementing it; a disagreement is a
  bug in one of them, decided explicitly.

**Exit:** every rule above has a normative paragraph with cited fixtures; S1 generates each rule's
shapes; S6's clauses link back.

### S11 — Emitted-code validation pulled forward (R-0299, R-0300)

The binary comes from Lower → SSA → Fold → LLVM, tested but trusted; 088's folder defect sits
there. R-0299's per-compile translation validation of a narrow
`ValidatedSSA → BackendIR → ValidatedBackendIR` subset and R-0300's independent replay move from
Phase 15 into this plan, after S3 (typed operations make the operator relation statable) and S7
(typed stages supply the validator's input/output pair):

- first subset: integer arithmetic at every width, casts, branches, bounded loops, direct calls and
  runtime checks — the constructs behind 080, 085, 088;
- S7's semantic `FoldVerify` (rewrite certificates) and `LowerVerify` (Core/SSA simulation) are
  delivered here;
- every proved function's emitted code is validated; a claim whose code fails is reported
  `translation_mismatch`, not proved.

**Exit:** R-0299's V1 exit restricted to every proved function in the corpus, plus R-0300's
independent replay of those certificates.

### S12 — Independent check of evidence (R-0353)

con-leche and nanoda re-check proofs without trusting Lean; the evidence pipeline itself has no
independent reader. R-0353's offline bundle verifier is that reader: it recomputes subject digests,
dependency roots and verdicts without importing the compiler. Here it consumes S4's `ClaimJudgment`
encoding and S5's generated statements, so an evidence defect must fool two independent
implementations.

**Exit:** R-0353's exit, with conformance fixtures including every 079 – 089 reproducer.

## Discipline while the refactor runs

- **Feature freeze on the proof surface.** No new claim kinds, proof attributes, coverage labels or
  evidence statuses until S2 and S4 land. Bug fixes and structural work are exempt.
- **No big-bang rewrite.** Every slice builds, passes the full CI gate set locally (including the
  claim-inventory step that otherwise runs only on Linux CI), is reviewed by Codex, and is published
  to both remotes before the next slice builds on it. No long-lived refactor branches; migration
  uses checked adapters and shadow paths, as R-0004 did.
- **Measure by hunting.** After each stage lands, rerun the hunts for the classes it claims to
  retire. A stage closes when its hunt finds no reproducible defect of its class; a defect found
  reopens it. Defects found per hunt are recorded in the changelog entry.
- **New defects still enter first.** A reproduced false guarantee preempts refactor slices (queue
  row 0), and its fix names the stage that would have prevented it; if none would, the plan gains
  a stage.

## Review

Every slice is reviewed by Codex (`codex exec -s read-only` against the slice's commit, with the
stage's section of this document as specification) before publication. Findings are fixed or
answered in the commit message; a slice is not published with an unanswered finding. Review
complements the gates; it replaces none.

## What this does not change

The kernel, con-leche/nanoda re-checking, the receipt format and the identity freeze are untouched
except where a stage names them (S2's identity-byte versioning). Language rule changes in S2, S3
and S10 follow the gate-corpus sweep discipline: grep fixtures and generators for removed syntax
and run the full CI gate set.
