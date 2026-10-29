# Bug 085 — a loop step's model read every variable from before the iteration

<!-- bug-ledger
summary: a `while_step` loop body's model built every `Cont` field from the pre-iteration env and ended a fall-through `if` early, so `x = x + 1; x = x + 1;` stepped by 1 and `seq(7)` was 21 vs 28; assignments now bind sequentially and branches continue with the rest of the body; pinned by `check_model_diff.sh` leg S
coverage: a while_step loop body's model read every variable from the pre-iteration env (x = x + 1; x = x + 1 stepped by 1) and dropped the rest of the body after a fall-through if; assignments now bind sequentially. Regression vehicle is a gate: check_model_diff.sh leg S pins tests/regressions/bug085_loop_step/repro.con (ten loop shapes, each pinned to its runtime value); see docs/bugs/085 (2026-10-18)
class: 7
silent: yes
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `bug085-stmt-extraction`, on the unpublished bug 080 head `00efbaf4`)
**Found:** 2026-10-18 by the model-vs-runtime hunt that followed bug 080 (`--report model-diff`).
**Severity:** a false model: a loop whose body has richer control flow than flat assignments was
extracted to a `while_step` that computed a different function from the program. A theorem about
such a model would have been accepted by the kernel and would have been about the wrong function.
No shipped theorem was affected: the one `while_step` proof (`ring_contains`) has a single
assignment that is the last statement of its body, the one shape the old extraction got right.

## Mechanism

`cStmtsToStepExpr` (Concrete/Proof/ProofCore.lean) turned a loop body into a step expression that
evaluates to `LoopStep::Cont { fields }` or `LoopStep::Break { value }`. `evalWhileStep` evaluates the
whole step once, in the env at the top of the iteration, and rebinds the `Cont` fields afterwards. The
extractor collected each `x = e` into the `Cont` payload WITHOUT binding it, so:

- a later statement read the OLD value: `x = x + one; y = y + x;` — `seq(7)` was 21 in the model and
  28 at runtime; `x = x + 10; let t = x; acc = acc + t;` was 30 vs 60; `x = x + 7; if i == 2 { return x; }`
  returned 14 vs 21 (the `Break` read the old `x`);
- a second assignment to the same name REPLACED the first instead of composing:
  `x = x + one; x = x + one;` stepped by 1 (model n, runtime 2n);
- an `if` without `else` whose then-branch fell through ended the iteration at the branch, dropping
  the rest of the body — including the for-loop step `i = i + 1` the elaborator appends — so the
  model looped without progress (`model_stuck`) where the runtime returned 104.

## Fixed

- **Sequential semantics.** An assignment that anything later could observe is emitted as a shadowing
  `letIn x e` and its `Cont` field becomes `.var x`, so later lets, conditions, assignments and
  `return`s read the new value and repeated assignments compose. An assignment that is the last
  statement of the whole body still goes into `Cont` directly — exact, since nothing runs between it
  and the rebind — so `ring_contains`'s extraction (`Cont [i := i + 1]`) is unchanged.
- **Fall-through.** The step extractor (`cStmtsToStepExprK`) takes a continuation: a then-branch (and
  now an else-branch) that falls through continues with the rest of the body. `if`/`else` inside a
  loop body is newly extracted under the same rule.
- **Escapes.** Expressions at non-tail sites (let initializers, assignments, conditions) must not contain
  a nested `return`/`break`/`continue` (`cExprNoEscape`, shared with bug 086).

No specification, digest or proof pin changed: every shipped claim's extraction is byte-identical.

## Prevention

`check_model_diff.sh` leg S pins `tests/regressions/bug085_loop_step/repro.con` — ten loop shapes, each
required to agree on every input and pinned to its runtime value (`twice(7) = 14`, `seq(7) = 28`,
`if_fallthrough = 104`, `ret_after_assign = 21`, ...). With the old extractor the same fixture reports
100 mismatches and 34 `model_stuck` rows.

## Limits

`while_step_preservation` (ProofSoundness R-20) is SHAPE-ONLY: it states which constructor extraction
builds from the step, not that the step computes the body. The bug lived entirely below it and it held
throughout; agreement of step and body is sampled by model-diff, not proved. A `let` inside a branch
that shadows an outer name (scoping, not sequencing) is bug 087, fixed separately in Elab; its proof-model
counterpart, a branch-local `let` scoping over the code after its branch, is [bug 101](101_branch_local_let_captures_continuation.md).
