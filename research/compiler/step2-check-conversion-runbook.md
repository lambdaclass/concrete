# Step 2 runbook — convert Check (+ Elab) to consume `IdExpr` (Phase 6.5 #9)

Date: 2026-07-10
Status: **not started.** Increment-0 substrate is landed and green
(`FactLedger`, `IdentifiedProgram` + mint, `IdExprKind` mirror with spans-in-ctors).
This is the execution recipe for step 2, to be driven to a single green commit.

## Why this is its own dedicated run (not a mid-session edit)

Check is the core. Converting `checkExpr`/`checkStmt` to take `IdExpr` forces every
helper and the Pipeline entry to convert too, transitively — **there is no
compiling intermediate**. A partial conversion leaves the whole checker
non-compiling, so it is all-or-nothing: it either reaches green in one pass or is
reverted. Two independent scope checks (and one prior burn-and-revert) confirm
this. Do it as one focused pass, ideally scripted; commit only when green.

## The transform is mechanical (arms are byte-identical)

`IdExprKind` mirrors `Expr` constructor-for-constructor **including spans**
(`intLit (span : Span) (val : Int)`, …). So per function the edit is:

- signature: `(x : Expr)` → `(x : IdExpr)`, `(x : Stmt)` → `(x : IdStmt)`,
  `(x : MatchArm)` → `(x : IdMatchArm)`, `List Expr` → `List IdExpr`, etc.;
- scrutinee: `match x with` → `match x.kind with` (x is now the struct;
  `.kind` is the mirrored shape);
- **arms unchanged** (same constructor names, same span-in-ctor binding);
- span access on a node: `x.getSpan` → `x.span` (span now lives on the struct);
- add `import Concrete.Elab.Identified` to each converted file.

The only non-mechanical residues: sites that **construct** an `Expr` (rare in
Check — Check reads, does not build AST), and any interop with passes still on
`Expr`.

## Conversion surface (inventory)

`Concrete/Check/Check.lean` (2248 lines):
- `checkExpr (e : Expr) …`
- `checkStmt (stmt : Stmt) …`
- `checkStmts (stmts : List Stmt) …`

`Concrete/Check/CheckHelpers.lean` (add the import; ~19 fns):
- `isFlexibleLit`, `isLitTrueExpr`, `peekExprType`, `borrowPathOf`, `borrowArgParts`
- divergence family: `exprDiverges`, `armDiverges`, `blockDiverges`, `stmtDiverges`
- non-terminating family: `exprNonTerminating`, `armNonTerminating`,
  `blockNonTerminating`, `stmtNonTerminating`
- exits-function family: `exprExitsFunction`, `armExitsFunction`,
  `blockExitsFunction`, `stmtExitsFunction`
- break family: `stmtHasBreak`, `blockHasBreak`

`Concrete/Elab/*` — the parallel set (Elab must also consume `IdExpr`, because
Elab consumes `TypedProgram.identified`). Same mechanical transform; inventory
this the same way (`grep -nE '(partial )?def .*(Expr|Stmt|MatchArm)'`).

Pipeline wiring (`Concrete/Pipeline/Pipeline.lean`):
- `mint : ResolvedProgram(post-desugar) → IdentifiedProgram` runs before Check;
- `check : IdentifiedProgram → Except Diagnostics TypedProgram` (currently returns
  `Unit`) mints `TypedProgram { identified, ledger }`, asserting per-family
  coverage;
- `elaborate : TypedProgram → …` reads `identified` + `ledger` (no re-inference).

## Suggested execution order (each step compiles nothing until the last)

Because there is no compiling intermediate, order is for the *author's* sanity,
not for intermediate commits:

1. `CheckHelpers.lean` — flip all ~19 fns + import (leaf-most; pure predicates).
2. `Check.lean` — `checkExpr`/`checkStmt`/`checkStmts` + any local helpers.
3. Elab parallel set.
4. Pipeline: `mint`, `check` → `TypedProgram`, `elaborate` consumes it.
5. `lake build`; iterate residual type errors to green.
6. **Increment-1 payload** (the point of all this): in `checkExpr`'s literal
   arms, `insertFact` the committed `TypeFact` keyed by `node(expr e.id, .value)`;
   in Elab's literal arms, switch from re-inference to `requireFact` (family
   `literals` marked migrated); coverage-asserted at `TypedProgram.mk`.
7. Gates: `fuzz_differential.py` + `test-ci-gates`; prove Check and Elab cannot
   disagree on a literal's type (the E0228 origin).

## Scripting note

The signature/scrutinee flip is regular enough to script (a careful `sed`/Python
pass over the two Check files + Elab set), but **not blindly**: `Expr`/`Stmt`
appear in non-convertible contexts, and `match … with` occurs on non-`Expr`
scrutinees. Generate the diff, review it, then build+fix residuals. A scripted
first pass + manual residual fixing is the intended one-shot method.

## Roadmap alignment

Matches ROADMAP #9 exactly. One refinement already folded into the docs:
`FactKey` is keyed by `SourceKey` (expr/stmt/decl/param/type/module + edges), not
`ExprId`-only — but the literals family only needs `expr` keys, so generalize the
key space when param/decl/edge facts land, not in this step.
