# Bug 086 — a discarded statement was modeled as the block's value, and a nested return as a value

<!-- bug-ledger
summary: a discarded `id(x);` was modeled as its block's value (`g(5)` 5 vs 7) and a `return` inside a `let` initializer as a value (11 vs 1); only `isValue` trailing expressions are values and non-tail expressions must not escape; pinned by `check_model_diff.sh` leg T
coverage: a discarded expression statement was modeled as its block's value (if x > 0 { id(x); } return 7; modeled as x) and a return nested in a non-tail expression as a value; only isValue trailing expressions are values and non-tail sites must not escape. Regression vehicle is a gate: check_model_diff.sh leg T pins tests/regressions/bug086_discarded_tail/repro.con; see docs/bugs/086 (2026-10-18)
class: 7
silent: yes
ruling: Outside the spec: tooling — a discarded statement and a nested return were modeled as values in the proof model
-->

**Status:** FIXED (branch `bug085-stmt-extraction`, on the unpublished bug 080 head `00efbaf4`)
**Found:** 2026-10-18 by the model-vs-runtime hunt that followed bug 080 (`--report model-diff`).
**Severity:** a false model: `fn g(x) { if x > 0 { id(x); } return 7; }` extracted to
`if x > 0 then id(x) else 7`, so `g(5)` was 5 in the model and 7 at runtime. A theorem about the model
would have been about a different function. No shipped claim was affected (all extractions of claimed
functions are unchanged).

## Mechanism

`cStmtsToPExprKImpl` and its non-partial wrapper `cStmtsToPExprK` had the case
`[.expr e _], _ => cExprToPExpr e`: a one-statement block's expression was its value, ignoring both the
`isValue` flag (a trailing `e` vs a discarded `e;`) and the continuation `k`. Inside an `if` without
`else`, the then-branch `{ id(x); }` therefore ended the function with `id(x)` instead of falling
through to `return 7`.

The audit of the same extractor found a second instance of the class — an expression's branch was
modeled as its VALUE even where control leaves the function: `let y = if x > 0 { return 1; } else { 2 };
return y + 10;` was `11` in the model and `1` at runtime, and `return id(if c { return 3; } else { 4 }) + 1`
was modeled as `id(3) + 1`.

## Fixed

- Only a value-position trailing expression (`isValue`) is a block's value. A discarded `e;` is not; it
  is a call or a control-flow expression whose effect the model cannot see (a pure discard is E0294), so
  the function is not extracted — except a discarded `match`/`if` whose every arm returns
  (`cExprDiverges`), which is the function's exit and is modeled exactly.
- Non-tail expression sites (let initializers, array stores, assignments, conditions, loop updates)
  require `cExprNoEscape`: no `return`/`break`/`continue` anywhere inside. Tail sites (`return e`, a
  trailing value, a loop `Break`) require `cExprTailOk`: a return may appear only along the branch chain
  of a top-level `if`/`match`, whose branch bodies are themselves extracted under these rules.
- The extraction theorems in ProofSoundness (`let_preservation`, `array_set_preservation`,
  `if_no_else_as_fallthrough_preservation`, `while_flat_preservation`, `while_step_preservation`) take
  the corresponding no-escape hypothesis.

## Prevention

`check_model_diff.sh` leg T pins `tests/regressions/bug086_discarded_tail/repro.con`: `g` and `letret`
must not be extracted at all, and the exact shapes (`tailmatch`, `tailif`) agree on every input and
are pinned to their runtime values. With the old extractor the fixture reports 25 mismatches. (A
third refused shape, `argret`, a `return` inside a call argument, left the fixture on 2026-10-21:
the language itself refuses it since [bug 142](142_exit_in_operand_leaks.md), E0278.)

## Limits

A refused function reports the generic "unmodelled statement or control-flow structure" reason rather
than naming the discarded statement or the nested return.
