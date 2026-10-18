# Structural soundness refactor

Status: planned, 2026-10-18. Owners: R-0491 – R-0498 in [ROADMAP.md](../../ROADMAP.md).

## Why

One afternoon of read-only hunting (2026-10-18) after bugs 079 and 080 produced nine more defects,
081 – 089. Every one is an instance of four causes, and each cause is a property of the pipeline's
structure, not of one careless function:

| cause | bugs | what the structure allows |
|---|---|---|
| **The language's meaning is written several times** | 080, 084, 085, 086, 088 (C2) | `Proof.eval` (`Proof.lean`, 3.4k lines), the interpreter (`Interp.lean`, 23 `partial def`s), the SSA constant folder (`SSACleanup.lean`) and codegen each define integer, cast, match and statement semantics separately. Nothing forces them to agree; `--report model-diff` samples them on shapes the corpus already uses. |
| **Evidence is decided by name** | 079, 081, 083, 089 | 35 string literals `"proved"` and 58 sites spelling `proved_by_lean` across `Report.lean`, `ReportObligations.lean`, `Main.lean`; a status is computed separately by contracts, vcs, obligation-ledger, audit, check-proofs and receipts. Callees, theorems and specs are joined by `String`. |
| **Names are not unique after resolution** | 082, 087, 039 – 045 | `CExpr.ident`, `CStmt.letDecl`, `CStmt.assign`, `CMatchArm.enumArm` bindings, `structLit`/`enumLit` fields, `fnRef`, and every `PExpr` binder are `String`. Elab alpha-renames only match payloads. |
| **Prose claims enforcement nothing performs** | 080's comment, `Backend.lean` contract text, R-18 – R-20 "preservation" | Comments and docs say "excluded by eligibility", "wrap by default", "discharged"; no gate cites them. |

Point fixes close instances. This plan removes the four causes, so the defects become
unrepresentable or mechanically caught, and states for each stage the bug classes it retires, the
gate that proves it, and how it is migrated without a flag day.

## Principles

1. **One definition per meaning.** Each semantic fact (integer arithmetic, cast, match binding,
   statement sequencing, scoping) has exactly one implementation; every consumer calls it.
2. **Identities, not spellings.** After Resolve, no IR, table or evidence record joins on a `String`.
   Display names exist only for printing.
3. **Invariants live in types.** An IR value that violates an invariant cannot be constructed
   (private constructors, smart constructors returning `Except`, subtypes carrying proofs), or is
   rejected by the verifier that runs immediately after the pass producing it.
4. **One authority per verdict.** One function decides each verdict; every surface is a view of
   its result. A verdict type has a private constructor owned by its checker.
5. **The compiler states the claim.** A proof's statement is generated from the code and its
   contract; the author supplies only the proof.
6. **Every enforcement claim cites its enforcement.** Prose that says something is excluded,
   checked or guaranteed names the gate leg that fails if it is not.
7. **Every gate leg has a negative control** that the leg must reject, so a leg cannot pass
   vacuously.

## Stages

Stages are ordered by bug classes retired per unit of work. S1 and S2 are cheap and start
immediately; S3 – S5 are independent of each other once S2's identity types exist; S6 is the end
state. Each stage lands in slices that each build, pass the full gate set, are reviewed by Codex
(see *Review*) and are published before the next begins.

### S1 — Prevention harness (R-0491)

Catch the remaining instances before the structural work removes them.

- **Three-way differential fuzzer.** Extend `scripts/tests/fuzz_differential.py` (today
  interpreter vs binary) with the extracted model as a third side, over *generated* programs whose
  grammar includes every statement shape that broke today: sequential assignments in loop bodies,
  `let` after assignment, `return` inside loops, if-without-else fall-through, discarded `expr;` in
  branches, block `let` shadowing, positional match bindings with renamed binders, unhinted
  literals at and beyond 2^63/2^64, casts across every width pair. Disagreement = failure.
  Nightly at 10k programs; CI at a fixed seed set.
- **model-diff coverage.** Zero-argument functions, `bool` parameters as booleans, non-scalar
  signatures (structs, enums, arrays, newtypes) and a compiled-binary leg (plan retained in the
  bug-080 follow-up notes); both-sides-fail on ill-typed input becomes an error, not `agree`.
- **Scheduled hunts.** The four 2026-10-18 hunts (model vs runtime, name-keyed evidence,
  stated-but-unenforced invariants, adversarial false proof) run before every release candidate;
  every reproducer becomes a permanent fixture.
- **Negative-control lint.** `check_gate_negative_controls.sh` requires every gate leg registered
  in the driver to name the fixture or mutation it must reject.

**Exit:** the fuzzer reproduces 084, 085, 086, 087 and 088 from their pre-fix commits within its CI
seed set; model-diff covers every claimed function with a supported signature (≥ 53 today) and
the binary leg agrees on all of them; the lint passes with zero exemptions.

### S2 — Resolved identities everywhere (R-0492)

Retires: 039 – 045, 081, 082, 083, 087 and every future "two things with one name".

- **Identity types.** `LocalId` (function-unique, assigned by Elab), `DefId` (wraps the existing
  `DefinitionIdentity`), `FieldIdx`, `VariantIdx`. Core changes:
  `ident : LocalId → Ty → CExpr`, `letDecl : LocalId → …`, `assign : LocalId → …`,
  `enumArm` binds `List (FieldIdx × LocalId × Ty)`, `structLit`/`enumLit` fields keyed by
  `FieldIdx` in declaration order, `fnRef : DefId → …`, `Callee.direct : DefId`.
  Each carries its source spelling only in a side table for diagnostics and reports.
- **Elab assigns identities for every binder** (block `let`, loop, match arm, parameter), giving
  lexical scoping by construction; Interp, Lower, extraction and the checker consume `LocalId`.
- **`ElabVerify`** runs after Elab: every `LocalId` bound exactly once and used in scope, every
  `DefId` exists, every match binding is a declared field of its variant.
- **`PExpr` binders and calls** use `LocalId`/`DefId`; `FnTable` is keyed by `DefId`. The theorem
  table's entry for a call is therefore *the* callee, not "an entry with that spelling".
- **Language rules** (each small, each removing an ambiguity):
  - a block `let` may not shadow a name visible in an enclosing scope (new diagnostic);
  - an import alias that collides with a name already in scope is an error;
  - a match binding either equals the field name (`Full { value }`) or names it
    (`Full { value: v }`); a binder that is neither is an error.
- **Migration.** Introduce the identity types with `Coe` from the old string fields (the R-0002
  technique: 23 sites, not 288), migrate one consumer at a time, then delete the coercions.
  Core-name churn consumers (goldens, snapshots, proof-link digests) normalize at comparison.

**Exit:** `grep` finds no `String`-typed binder, callee or field key in `Elab/Core.lean`,
`IR/*.lean` or `Proof/Proof.lean` outside display side tables (enforced by
`check_ir_no_string_identity.sh`); `ElabVerify` runs in every pipeline; the 082, 087 and 081
reproducers are compile errors or bind correctly, with mutation families proving `ElabVerify` and
the theorem-table key check load-bearing.

### S3 — Typed operations and one arithmetic (R-0493)

Retires: 080, 088, and every "which width?" defect.

- `BinOp`/`UnaryOp` carry their operand `IntTy` (`add i32`, `div u64`, `shl u8`); the type
  checker fills it; no later pass infers it.
- `intLit` is built only through `CExpr.mkIntLit : Int → IntTy → Except Diag CExpr`, which range
  checks; unhinted literals take their type from context or are an error (no unbounded `Int`
  default for literals).
- `cast` carries source and target type.
- `Semantics/IntArith.lean` is the only implementation of wrap, range, overflow, division, shift
  and cast. The interpreter, `SSACleanup` folder, `Proof.eval` and `EmitBuiltins` constant paths
  call it; `check_single_arithmetic.sh` refuses `% 2^`, `toUInt*`, `Int.emod`… on integer values
  outside `IntArith`.
- **Width-aware proof values.** `PVal.int` carries its `IntTy`; arithmetic that can overflow yields
  an explicit no-overflow obligation instead of unbounded `Int`. A theorem about unbounded `Int`
  can no longer bind to an `i8` body, and an `iff` cannot be shown over code that traps.
- `Backend.lean`'s contract text is generated from the trap helpers actually emitted.

**Exit:** `CExpr.intLit`'s constructor is private; every op in Core, SSA and `PExpr` carries a
width; `check_single_arithmetic.sh` passes; the 088 C1/C2 reproducers agree on all three sides;
the `i8`-to-`Int` binding (red-team `rt_ovf`) is refused.

### S4 — One evidence authority (R-0494)

Retires: 079, 083, 089 and every surface disagreement.

- `Concrete/Proof/Verdict.lean` defines `Verdict` (proved, stale, unbound, missing, trusted, …)
  with a **private** constructor for the discharging cases; only `Verdict.decide` (fed by the
  kernel replay result, the subject binding and the dependency closure) can produce them.
- One `ClaimJudgment` per claim and per `#[ensures]`/`#[requires]` clause, computed once per
  compilation; contracts, vcs, obligation-ledger, audit, check-proofs (text and JSON), proof-status,
  receipts issuance and receipt consumption are pure renderers of it.
- Hardcoded name-keyed evidence (`Proof.provedFunctions`) and name-substring replay checks are
  deleted.
- `check_single_verdict_authority.sh` refuses status string literals (`"proved"`,
  `"proved_by_lean"`, …) outside `Verdict.lean` and its renderers.
- `Report.lean` (6.4k lines) and `Main.lean` (4.2k lines) split by surface; each surface module
  imports `Verdict` and may not import the checker internals.

**Exit:** the 35 `"proved"` and 58 `proved_by_lean` sites reduce to the renderer table; the
089 reproducers read not-proved on every surface; a cross-surface gate asserts every surface
renders the same verdict for every claim in the corpus.

### S5 — Compiler-generated claim statements (R-0495)

Retires: 079, 081 and 089 as a class, and author-declared coverage.

- For each claim the compiler emits the Lean *statement*: the spec equation
  (`∀ args, eval table (body f) args = spec args` with the extracted body inserted by value and
  callees by `DefId`), and for each `#[ensures]` clause its exact proposition. The author writes
  `by …` or names a lemma whose type must be definitionally that statement.
- Coverage (`iff`, `one_direction`, `point`, …) is derived from the statement's shape; the
  `#[proof_coverage]` label becomes a checked assertion about the derived shape.
- `scripts/gen/classifications.lean`'s scanning of theorem types and the subject-binding table
  become redundant and are deleted once every claim is migrated.
- Migration: emit statements alongside existing theorems; prove each existing theorem implies the
  generated statement (`exact old_thm`); then switch the authority; then delete the scan.

**Exit:** every claim in `examples/`, `std/`, `tests/` replays against a compiler-generated
statement; the subject-binding table generator is deleted; coverage labels are checked.

### S6 — One total Core semantics (R-0496)

Retires the model/runtime class (080, 084, 085, 086) at its root; closes H19's lowering half.

- `Concrete/Semantics/CoreEval.lean`: a total, fuel-based big-step evaluator over Core
  (post-S2/S3 Core: identities, typed ops), structural on fuel, with `IntArith` as its arithmetic.
- The interpreter becomes `CoreEval` plus I/O; the 23 `partial def`s go away.
- Proofs are about Core terms under `CoreEval`. `PExpr` is retired; or, if kept for proof
  ergonomics, extraction ships with `extract_sound : CoreEval f args = some v →
  Proof.eval (extract f) args = some v` (and its converse on the eligible subset), proved in Lean.
- R-0460's register rows for the lowering half are discharged by `extract_sound`.

**Exit:** no `partial def` in `Interp`/`CoreEval`; every claim's theorem is about `CoreEval` or
transported by `extract_sound`; `VC_BRIDGE_REGISTER.md` rows discharged by name.

### S7 — Typed pass contracts (R-0497)

The pipeline becomes a typed sequence of stages, each consuming the previous stage's verified output:

`Parse → Resolve → Elab+ElabVerify → Check → Mono+MonoVerify → Lower → SSAVerify → Fold+FoldVerify → Emit`

- Each stage's output type is a structure carrying its invariant (as a proof field where cheap,
  otherwise as the token returned only by that stage's verifier); a later stage's input type
  requires it, so skipping a verifier fails to compile.
- `FoldVerify`: every constant normalized to its type; `LowerVerify`: Core types preserved into SSA.
- Per-pass mutation families (one per verifier) prove each verifier is load-bearing.
- Pass timing and IR size per stage are reported, so the refactor is measured, not assumed.

**Exit:** `Pipeline.lean` exposes only the typed stage sequence; each verifier has a mutation
family killed in the qualified campaign.

### S8 — Claims cite their enforcement (R-0498)

- `check_enforcement_citations.sh` scans `docs/verification/*.md`, `docs/language/*.md` and Lean
  doc comments in `Concrete/Proof`, `Concrete/Check`, `Concrete/Backend` for enforcement phrases
  ("is excluded", "is checked", "guarantees", "ensures that", "fails closed", "cannot",
  "by construction", "discharged") and requires a `[gate: <script>#<leg>]` or
  `[test: <path>]` citation that exists.
- Register rows R-18 – R-20 are relabelled shape-only until S6 discharges them.

**Exit:** zero uncited enforcement phrases; every cited leg exists and has a negative control.

## Order and parallelism

```
S1 ──────────────────────────────► (continuous)
S2 ──► S3 ─┐
      S4 ──┼──► S5 ──► S6
S8 ────────┘          S7 (after S3; parallel to S5/S6)
```

S1, S2, S4 and S8 start now in parallel worktrees. S3 needs S2's identity types. S5 needs S2
(identities in statements) and S4 (one authority to switch). S6 needs S2 and S3.

## Review

Every slice of every stage is reviewed by Codex (`codex review` on the slice's diff, plus
`codex exec` with the stage's section of this document as the specification) before publication.
Findings are fixed or answered in the commit message; a slice is not published with an unanswered
finding. Review complements the gates; it does not replace any.

## What this does not change

The kernel, con-leche/nanoda re-checking, receipts' format and the identity freeze are untouched
except where a stage names them. Language rule changes in S2/S3 follow the gate-corpus sweep
discipline: grep fixtures and generators for removed syntax and run the full CI gate set.
