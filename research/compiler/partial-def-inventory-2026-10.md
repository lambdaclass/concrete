# Partial definitions in the compiler: inventory, what they block, conversion order (2026-10)

Status: research note, 2026-10-28, measured at origin/main `ca49fada`. Nothing in this note converts a
definition. Since the same branch landed the totality ratchet
(`scripts/tests/check_totality_ratchet.sh`, baseline `docs/compiler/totality_baseline.tsv`), the
counts below can only fall.

## Why this matters

A `partial def` is kernel-opaque. Lean compiles it from an `unsafe` twin (`f._unsafe_rec`), and the
kernel sees only an `opaque` constant with no equations. No theorem can unfold it, and no `rfl`
example can pin its behaviour. A pass made of partial definitions therefore cannot be the subject of a
preservation proof, whatever the proof effort. The verified-pipeline plan states the requirement as
principle 8 (`docs/compiler/VERIFIED_PIPELINE.md`): "semantics, checkers, validators and proved
passes are total; untrusted search (heuristics, elaboration search, register-allocation search) may
stay partial". It assigns a "totality lint" to S12a (R-0506).

**Does the plan already track this?** In part. Principle 8 states the rule, S12a's exit criterion
requires that "semantics, checkers, validators and proved passes pass the totality gate", and the
"why the current pipeline cannot get there" table counts "510 `partial def`s" (2026-10-18). Three
things were missing until this branch:

- no gate existed;
- the count was a grep;
- nothing said which partial definitions block which proof.

The ratchet is that gate's first form: it stops growth now, and its baseline is the per-stage list
the S12a exit needs.

## How it was measured

Grep is the wrong instrument, and the difference is large. `scripts/tools/totality_inventory.lean`
imports every `Concrete` module and the `Main` executable root and walks the elaborated environment.
A declaration counts as partial when it is an `opaque` with an `_unsafe_rec` twin. A twin alone proves
nothing: Lean also compiles definitions by well-founded recursion, which are total, through an
`_unsafe_rec` twin (`CheckHelpers.tyToString` is a `WellFounded.fix` definition with one). The
results:

| count | what |
|---|---|
| 693 | source lines matching `partial def` in `Concrete/` and `Main.lean` (the grep the earlier counts used, 690 of them in `Concrete/`; variants of the regex give 690 – 702) |
| 89 | of those are inert. The definition is not recursive, so Lean 4.34 elaborates it as an ordinary total `def` and the keyword means nothing (27 in `Parser.lean`, 8 in `Lower.lean`, 6 in `Reduce.lean`, 5 in `Verify.lean`, …) |
| 60 | partial declarations no grep finds: 12 `deriving BEq`/`Repr` instances on nested inductives (`instBEqTy.beq`, `instBEqPExpr.beq`, …) and 48 `where`/`let rec` helpers (`go`, `goModule`, `rewriteModule`, …) whose source line says `def` or nothing |
| **664** | partial declarations in the environment: 500 call themselves directly and 164 reach themselves only through other partial definitions (mutual) |

Besides the partial definitions, the walk finds 18 non-total declarations of other kinds:

- eight host bridges, each an `implemented_by` definition over an `unsafe` implementation (16 rows):
  seven in the interpreter and `Main`'s stdin reader;
- the two `opaque` extensions that `register_simp_attr` declares.

It also finds one axiom, the native-trust axiom that `bv_decide` mints for
`Concrete.Proof.and255_lo`. That axiom is registered in `scripts/tests/axiom_native_owners.txt` and
`docs/verification/AXIOMS.md`. No compiler declaration mentions `sorryAx`.

## Inventory by stage

Stage is by file (`docs/compiler/totality_stages.tsv`). "Proof" marks the stages the plan proves.
Reasons are the codes in `scripts/tools/totality_ratchet.py`: a source heuristic seeded them, and
every proof-relevant row that was not an AST walker was corrected by reading its body.

| stage | proof | rows | reasons | owner |
|---|---|---|---|---|
| Check (`Check/*`) | yes | 133 | nested-inductive 95, mutual-walker 35, derived-instance 2, index-loop 1 | R-0499 |
| Elab (`Elab/*`) | yes | 54 | nested-inductive 38, mutual-walker 16 | R-0499 |
| Mono | yes | 48 | nested-inductive 28, mutual-walker 19, worklist-fixpoint 1 | R-0510 |
| Lower | yes | 27 | nested-inductive 16, mutual-walker 10, index-loop 1 | R-0501 |
| SSA (`SSA`, `SSACleanup`, `SSAVerify`) | yes | 9 | worklist-fixpoint 9 | R-0504 |
| EmitSSA | yes | 3 | nested-inductive 2, index-loop 1 | R-0504 |
| Semantics (`Semantics/*`, `Interp`) | yes | 67 | nested-inductive 34, object-divergence 15, host-io 14, worklist-fixpoint 2, derived-instance 2 | R-0496 |
| ProofCore | yes | 40 | nested-inductive 22, mutual-walker 17, direct-recursion 1 | R-0495 |
| Frontend | no | 66 | token-consumption 40, nested-inductive 17, mutual-walker 6, derived-instance 2, direct-recursion 1 | R-0506 |
| Resolve | no | 37 | nested-inductive 24, direct-recursion 5, mutual-walker 4, worklist-fixpoint 3, index-loop 1 | R-0506 |
| Report | no | 125 | nested-inductive 106, mutual-walker 8, worklist-fixpoint 5, index-loop 4, token-consumption 2 | R-0506 |
| Proof (other) | no | 52 | nested-inductive 31, mutual-walker 14, derived-instance 4, simp-attr-extension 2, index-loop 1 | R-0506 |
| Pipeline (`Pipeline/*`, `Main`) | no | 16 | nested-inductive 12, mutual-walker 2, host-io 2 | R-0506 |
| Backend (other) | no | 5 | nested-inductive 3, derived-instance 2 | R-0506 |

The proof-relevant stages hold 381 of the 682 rows.

`Browser.lean`, the `concrete-browser-native` root, is not inventoried. It is outside `defaultTargets`
and does not build at `ca49fada` (`Unknown identifier Pipeline.desugar`, line 43), so it cannot be
imported. The gate says so where it lists its modules.

## Why each one is partial

- **nested-inductive (428 rows).** The definition recurses over a type whose constructors hold a
  `List`, `Array` or `Option` of the type itself: `Expr`, `Stmt`, `Ty`, `CExpr`, `CStmt`,
  `CMatchArm`, or `CModule` through its submodules. The recursive call usually sits inside `.map`,
  `.foldl`, `.all` or a `for` loop over a field. Before Lean 4.12 a plain `match` could not be
  structural over such a type, and `partial` was the habitual escape.
- **mutual-walker (131 rows).** The same, across a `mutual` block: `lowerStmt`/`lowerStmts`,
  `monoExpr`/`monoStmt`/`monoArm`, `substExpr`/`substStmt`/… .
- **token-consumption (42 rows).** The parser and lexer advance a position through the input. Each
  step consumes input, but the termination measure (input remaining) is not a structural argument.
- **object-divergence (15 rows).** The interpreter evaluates Concrete programs (`evalExpr`,
  `evalStmt`, `evalWhile`, `runDeferred`, …). A Concrete program may loop forever, so no measure
  exists: totality requires a clock.
- **worklist-fixpoint (20 rows).** Iteration to a fixpoint or over a graph:
  - SSA cleanup (`cleanupFixpoint`, `eliminateDeadInstsFixpoint`, `eliminateTrivialPhis`,
    `chainWalk`, `findReachable`);
  - the dominator algorithms the SSA verifier uses (`dfsPostorder`, `ltDfs`, `ltPath`,
    `domNumber`);
  - Mono's instantiation queue (`drainQueue`);
  - the interpreter's cell-exploding retries (`clearRange`, `copyMem`).
- **index-loop (9 rows).** Recursion on an index, a tick or a shrinking suffix:
  - `VarEnv.entriesFrom.go` and `VarMap.writtenAfter.go` walk a tree map by increasing key;
  - `emitSInsts` recurses on the suffix that a constant-store run leaves.
- **derived-instance (12 rows).** `deriving BEq, Repr` on a nested inductive produces a partial
  instance. The consequential one is `instBEqTy.beq`: every `==` on `Ty` anywhere in the checker,
  elaborator, Mono and Lower goes through an opaque function, so no theorem about any of them can
  decide type equality.
- **direct-recursion (7 rows).** Recursion that already looks structural. `ProofCore.effectOpaqueFix`
  is structurally recursive on its `fuel` argument and needs no `partial` at all.
- **host-io (16 rows), simp-attr-extension (2 rows).** Deliberate trust boundaries, not recursion.
  They are outside what a proof of the pipeline needs.

## Which proofs each blocks

- **R-0496, Core semantics (S7).**
  - The interpreter's 15 object-divergence rows and its 29 walkers are why `Interp` cannot serve as
    the semantics. The plan's Core semantics is a new, clock-indexed definition, so these rows block
    differential *use* of the interpreter in proofs, not the semantics itself.
  - `TermIR` is already total, and the pins keep `evalInt`, `evalBool` and `elimTmod` total (Register
    B depends on it). Only its derived `instReprTerm.repr` is partial, and that is harmless.
- **R-0499, checker rules on Core (S8) and R-0507, type and ownership soundness.**
  - All 133 Check rows and 54 Elab rows: the walkers that compute linearity, divergence
    (`exprEnds`, `stmtNonTerminating`), mentioned names, capability sets and layout.
  - Plus `instBEqTy.beq` (Frontend), which every type comparison in them uses.
  - A declarative judgment can be stated today. Proving that the implementation decides it cannot.
- **R-0510, checked specialization (S8a).** Mono's 48 rows: `substExpr`/`substStmt`, `rewriteTy`,
  `monoExpr`, `collectExprInstances`, `drainQueue`. The plan lets the specialization *search* stay
  partial, so `drainQueue` may stay. The validator must be total and is new code.
- **R-0501, Place MIR (S10).** Lower's 27 rows. The plan replaces `Lower`'s single pass with one
  pass per job, so converting the old `Lower` would be churn; the new passes must be born total.
- **R-0504, validators (S12).**
  - The four dominator definitions in `SSAVerify` block any soundness proof of the SSA verifier,
    which is a validator and must be total.
  - `SSACleanup`'s fixpoints are optimizations: under the plan they stay untrusted behind a
    certificate checker.
  - EmitSSA's three rows (`emitSInsts`, `tyToLLVMTy`, `constStoreRun`) block nothing proved: the
    LLVM printer is trusted and differentially tested.
- **R-0495, claims on Core (S9).** ProofCore's 40 rows extract `PExpr`, and `PExpr` is retired by the
  plan. Its replacement generates claims from checked Core, so these rows block nothing that will be
  proved; converting them is wasted work.
- **Not proof-relevant.** The parser (whose output the checker re-checks), Resolve, Report, Pipeline
  and the evidence layer. The ratchet still stops their growth, and the report layer's pins show
  that totality there pays when a lock or a completeness theorem needs it.

## Conversion strategy, by kind

| reason | conversion |
|---|---|
| inert keyword (89 sites, not in the baseline) | delete the keyword; Lean already treats the definition as total, so behaviour and generated code are unchanged |
| direct-recursion | delete `partial`; if Lean asks, `termination_by` the structural argument |
| nested-inductive, mutual-walker | try plain structural recursion first (Lean ≥ 4.12 derives structural recursion through nested `List`/`Array`/`Option` fields). Where the call goes through `map`/`foldl`/`for`, use `List.attach` and `termination_by sizeOf`, or restate the higher-order call as an explicit mutual helper over the list |
| derived-instance | replace `deriving BEq` with a hand-written structural `beq` and a `LawfulBEq` proof (or `DecidableEq`), starting with `Ty` |
| index-loop | `termination_by` the remaining length or key distance, with the `decreasing_by` fact the loop already checks |
| worklist-fixpoint | fuel bounded by a size the algorithm already knows (node count, the number of names), with fuel exhaustion an explicit result rather than a silent default. Search-like fixpoints may stay partial behind a total checker |
| token-consumption | `termination_by` the input remaining (each step consumes at least one token), or fuel = input length |
| object-divergence | a clock argument (the plan's Core semantics is clock-indexed); the interpreter itself stays a differential reference |
| host-io, simp-attr-extension | stay; they are trust boundaries, outside the pipeline proofs |

## Order

The priority is the next proof that a conversion unblocks, not the size of the bucket.

1. **The 89 inert keywords.** No behaviour change and no proof work. They are removed only so that
   source and environment agree, and the ratchet does not move because they are not in it.
2. **`instBEqTy.beq` and the other derived instances on IR types** (`Ty`, `LLVMTy`,
   `Coherence.Term`). One definition unblocks reasoning about every type comparison. Do it before any
   checker proof.
3. **Check and Elab walkers (R-0499).** Over `Expr`/`Stmt`/`CExpr`/`CStmt`, by nested structural
   recursion. This is the largest proof-relevant bucket (187 rows) and the prerequisite for S8.
4. **SSAVerify's dominator algorithms (R-0504).** Four definitions with fuel = node count; the SSA
   validator's soundness lemma needs them.
5. **Mono's substitution and rewriting walkers (R-0510)**, as the S8a validator is written. Leave
   `drainQueue` partial as search.
6. **Semantics: new code born total.** The Core semantics (R-0496) and the Place MIR passes (R-0501)
   should arrive total under the ratchet; their stages then have no baseline rows to remove. Convert
   `Interp` only where a differential lemma needs a specific definition.
7. **Frontend, Resolve, Report, Proof and Pipeline**, opportunistically, when a lock or theorem asks.
   The report layer's earlier conversions (pinned in `totality_pinned.tsv`) show the pattern.
8. **ProofCore and `Lower`: do not convert.** Both are being replaced, so their rows leave the
   baseline when the code is deleted.

## How the ratchet holds this

- `check_totality_ratchet.sh` fails on any non-total declaration that is not in the baseline. It
  also fails on a baseline row that has become total, until the row is deleted, so the list only
  shrinks.
- In a proof-relevant stage, a row without a reason code fails. So does a `sorry` in any compiler
  declaration, and an axiom other than a registered native-trust axiom.
- It fails if a declaration pinned total in `docs/compiler/totality_pinned.tsv` becomes non-total,
  whatever the baseline says.
- When a stage's rows reach zero the gate needs no change: every non-total declaration in that stage
  is then new.
- It runs in CI's Language-surface gates job (full history, for the base comparison), in the pre-push
  hook for any compiler source or baseline change (docs-only pushes included), and as a
  mutation family (`totality-ratchet-new-partial`: marking Lower's structural `arrayLenOfTy`
  `partial` builds, changes no output, and only this gate kills it).
