# Statement vs Trailing-Expression Model (design)

Status: IMPLEMENTED (core) 2026-06-20 — ROADMAP #36 / LANGUAGE_GAPS #12
Date: 2026-06-20

> **Implemented as designed (Option A).** `AST.Stmt.expr` / `Core.CStmt.expr`
> carry an `isValue` flag; the parser sets it (`parseExprBlock`, the direct
> `=> expr` arm, and — a scope correction found during implementation — the
> while-expression `else` branch, now value-bearing); checker, elaborator,
> lowering, and formatter respect it. Landed in 3 staged commits (flag threading
> → semantics flip → formatter), each full-suite green; the spurious E0225
> statement-match-arm class is fixed; locked by
> `tests/programs/regress_stmt_match_arm_unit.con`. Deferred follow-ups (kept out,
> per the bounded scope): braced block-as-value in arbitrary expression position
> (`let x = { …; v }`), implicit trailing-`return` function bodies, and a
> formatter fix to render single-value-expr arms directly (it currently
> block-wraps `=> if …`, breaking round-trip). The design below is the as-built
> plan.
>
> **Update (2026-07-01)**: trailing `match` and `if/else` inside VALUE blocks
> (if-expression branches, while-else, match-arm blocks) are now value-bearing
> when their arms/branches end with values —
> `let v = if c { match x { .. } } else { 0 };` works. An all-statement
> trailing `if`/`match` stays a statement (its lowering has no value merge, so
> existing programs' SSA is unchanged). Locked by
> `scripts/tests/check_trailing_value_blocks.sh`. The braced-block and
> implicit-return follow-ups above remain out.
>
> **Update (Phase 6D item 2)**: value `while … else` is REMOVED — `while` is
> statement-only, and the `Expr.whileExpr`/`CExpr.whileExpr` constructors are
> gone pipeline-wide. The historical references to a value-bearing while-else
> in this document describe the pre-6D state. The migration is a pre-declared
> mutable result assigned in the body before `break;`
> (tests/programs/error_while_expr_removed.con pins the parse error + hint;
> loop_break_result_*.con are the migrated exemplars). `if`-expressions and
> value `match` are unchanged.

## Problem

Concrete does not distinguish, in the AST, a discarded expression statement
(`expr;`) from a trailing value expression (`expr` with no `;`). Both become the
same `AST.Stmt.expr span e`. So a value-bearing block's type is computed from its
last `.expr` regardless of the `;`, which:

- types a statement-position match arm `=> { side(); }` (where `side() -> i64`) as
  `i64`, so it disagrees with a unit arm `{ }` and is rejected with a spurious
  **E0225** ("match arm type … does not match first arm type …"); and
- is the dual of the now-fixed if-*expression* gap (LANGUAGE_GAPS #5): a block
  cannot serve as a value (`{ …; v }`) because the trailing expression is not
  modeled distinctly.

## Root cause (pinpointed)

`Concrete/Frontend/Parser.lean`:
- `parseExprBlock` (the value-bearing block parser, used for if-expression
  branches and `{ … }` match-arm bodies): a trailing expression with **no** `;`
  (line ~846) and a `;`-terminated statement (line ~862) both emit the identical
  `Stmt.expr sp e`. The `;` is observed and then discarded.
- `parseMatchArmBody` bare arm `=> expr` (line ~1208) emits `[.expr sp e]` too.

Everything downstream that computes a block/arm value or type reads the last
statement and, for `.expr e`, uses `e` — there is no flag to say "this one was
discarded."

## Current model (verified)

- **Function bodies require explicit `return`** (`fn f()->i64 { 5 }` is a parse
  error) — no implicit trailing-expression return. Parsed by `parseBlock`
  (statement-only).
- **if/while/for statement bodies**: `parseBlock` (statement-only).
- **Value-bearing blocks**: ONLY `parseExprBlock` — if-expression branches and
  `{ … }` match-arm bodies — plus the direct `=> expr` arm.
- **Braced block as a free expression** (`let x = { …; v }`): not supported.

So the value/discard distinction only needs to be tracked where `parseExprBlock`
or the direct arm runs. Function/loop bodies are out of scope.

## Chosen representation

**Option A (chosen): a `isValue : Bool` flag on `Stmt.expr`.**
`| expr (span : Span) (e : Expr) (isValue : Bool)` — `isValue = true` iff this is
the block's trailing value expression (no `;`); only the last statement of a
value-bearing block may be `true`. The parser already has the `;` information at
the branch point, so it sets the flag directly; everywhere else constructs
`isValue := false` (a plain discarded statement). Block/arm value+type:

    last stmt is  .expr e true   → type/value = e
    last stmt is  .return_/break  → diverges (never)
    otherwise                     → Unit

Rejected alternatives:
- **Option B — a `Block` record `{ stmts, trailing : Option Expr }`.** Cleaner
  separation, but every `List Stmt` body field (if/while/for/fn/arm) changes
  type — large, invasive churn for no extra correctness over Option A.
- **A distinct `Stmt.tailExpr` constructor.** Avoids touching `.expr` build sites
  but adds a constructor to handle in every `Stmt` match; about the same churn as
  the flag, less uniform. The flag keeps one expression-statement shape.

The flag forces every `.expr` construction site to be updated (Lean won't
compile otherwise), so there is no risk of a missed site.

## Scope

IN (this change):
- Record value-vs-discard and make block/arm type respect it. This fixes E0225
  and makes match-arm typing principled.

OUT (separate, additive follow-ups — not required by #42):
- Braced block as a free expression (`let x = { …; v }`).
- Implicit trailing-expression `return` for function bodies.
These are features, not the bug; keep them out so the fix stays bounded.

## Touch points

- `Concrete/Frontend/AST.lean`: `Stmt.expr` gains `isValue : Bool`.
- `Concrete/Frontend/Parser.lean`: set `isValue` in `parseExprBlock` (trailing-no-`;` →
  true; `;` → false) and `parseMatchArmBody` (bare `=> expr` → true). `parseBlock`
  and all other `.expr` builders → false.
- `Concrete/Check/Check.lean`: the arm/branch type sites (`body.getLast?` at ~1116,
  ~1135, ~1727, ~1800; if-branch at ~1090): `.expr e true` → typeof e, else Unit.
- `Concrete/Elab/Elab.lean`: if-expression result-type inference and arm elaboration
  use the same rule; carry the flag onto `CExpr`/`CStmt` if Core needs it.
- `Concrete/Elab/Core.lean` + `Concrete/Elab/CoreCanonicalize.lean`: thread the flag (or
  the trailing-expr notion) so Lower can see it.
- `Concrete/IR/Lower.lean`: `lastExprVal` returns the trailing value only for
  `.expr … true`; a `;`-terminated last `.expr` contributes no value (Unit) —
  also removes a class of `store void`/wrong-slot hazards.
- Formatter: print a trailing value expression without `;`, a statement with `;`.
- Diagnostics: E0225 should no longer fire for statement-position arms; keep it
  for genuine value-arm type disagreement.

## Staged plan (each stage: full suite green)

1. **AST + parser, behavior-preserving.** Add the flag; set it correctly in the
   parser; make every type/value site treat `isValue=false` last-`.expr` the SAME
   as today *for now* (i.e. keep current behavior) to isolate the mechanical
   change. Build + full suite green.
2. **Flip the semantics.** Make block/arm type = Unit when the last `.expr` is
   discarded (`isValue=false`), and the trailing value only when `true`. This is
   the behavior change that fixes E0225. Update Lower's `lastExprVal` in lockstep.
   Add `tests/programs/regress_stmt_match_arm_unit.con` (the `{ side(); }` arm now
   agrees with a unit arm) and keep the existing value-arm fixtures green.
3. **Formatter + diagnostics.** Round-trip formatting (value expr no `;`,
   statement with `;`); confirm E0225 only fires on real value disagreement.
4. **Docs + gates.** Update LANGUAGE_GAPS #12 → fixed, ROADMAP #36 → done, this
   doc → implemented; CHANGELOG entry; `--full` baseline unchanged.

## Risks

- Any value-bearing block that today relies on a `;`-terminated last expression
  being its value would flip to Unit. Given function bodies require `return` and
  block-as-value isn't supported, the only such sites are if-expression branches
  and match-arm blocks — and those legitimately wanting a value use the no-`;`
  trailing form or a direct `=> expr`. The full suite + examples + `--full` are
  the safety net; Stage 1/2 separation localizes any fallout to the flip.
- `&&`/`||` short-circuit, `defer`, and ghost lets all build `.expr`/statements —
  mechanical flag updates only; covered by the compile-forces-all-sites property.
