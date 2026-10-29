# Bug 101 — a branch- or body-local `let` captured the code after its scope in the model

<!-- bug-ledger
summary: a branch-local `let` was nested over the code after its branch (and a loop body's own `let` written back through `Cont`), so `scoped(1)` was 10 in the model vs 1; a block's locals may not be named by its continuation; pinned by `check_model_diff.sh` leg S
coverage: a branch- or body-local let was nested over the code after its scope in the proof model (scoped(1): model 10, runtime 1); branches composing with a continuation refuse when their locals reach it. Regression vehicle: tests/regressions/bug085_loop_step/repro.con scoped/reloop + check_model_diff.sh leg S (2026-10-19)
class: 7
silent: yes
ruling: Violates: SCOPE-3@1
-->

**Status:** FIXED (branch `land-batch1`)
**Found:** 2026-10-18 by the Codex review of the bug 085 change (land1).
**Severity:** a false model. `scoped(1)` below was 10 in the model and 1 at runtime; a theorem about the
model would have been about a different function. No shipped claim was affected (every claimed
function's extraction is unchanged; the classification table's spec bindings are byte-identical).

```concrete
fn scoped(n: i32) -> i32 {
    let x: i32 = 1;
    let mut y: i32 = 0;
    for (let mut i: i32 = 0; i < n; i = i + 1) {
        if i == 0 { let x: i32 = 10; } else {}
        y = x;
    }
    return y;
}
```

## Mechanism

A block is a scope: its `let`s die when control leaves it (the interpreter restores the scope; Elab
alpha-renames only match binders). The extractors compose a fall-through branch with what follows it
by nesting the continuation INSIDE the branch's `letIn`s — `cStmtsToStepExprK` passes the rest of the
loop body into each branch (bug 085 made `if/else` with a continuation extractable), and
`cStmtsToPExprKImpl`/`cStmtsToPExprK` pass the extracted rest `pkRest` (or the outer `k`) into the
branches of an `if`. So `let x = 10` in the branch bound the `x` that `y = x` read after the branch.
The loop body itself is the same kind of scope: `let mut x = 5; x = x + 1;` in the body wrote `x` back
to the OUTER `x` through the step's `Cont` field (`body_local_assigned`, 6 vs 1).

## Fixed

- `cStmtsLetsAtEnd` names the `let`s of a block still in scope where control can fall off its end;
  `pexprMentions` is a conservative occurrence test. A branch composes with its continuation only when
  the continuation mentions none of those names (`cBranchComposes`) — all three non-loop sites.
- `cStmtsToStepExprK` threads `locals`, the names declared by the current block. A branch's end refuses
  when the continuation it returns mentions one; the body's end refuses when a `Cont` field would write
  one back.
- A local that nothing after its scope names (`fresh_branch_let`) stays extractable.
- `if_no_else_as_fallthrough_preservation` (ProofSoundness R-07) takes the side condition as a
  hypothesis.

## Prevention

`check_model_diff.sh` leg S: `scoped`, `scoped_flat` (the non-loop form) and `body_local_assigned` must
not be extracted; `branch_let_returns` (a local consumed before its branch returns) and
`fresh_branch_let` stay extracted and agree on every input.
