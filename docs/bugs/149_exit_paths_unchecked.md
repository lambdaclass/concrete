# Bug 149 — loop conditions, returns in nested blocks and labelled exits were not checked for ownership

<!-- bug-ledger
summary: a `while`/`for` condition was checked outside its loop (moving an outer binding there ran every iteration), a `return` inside a nested block was not checked for live owned values (later unreached code was credited), and a labelled `break`/`continue` checked only the innermost loop's locals (found by Codex). E0207/E0208/E0210/E0211. `error_149_*.con`, `regress_149_exits_accounted.con`. Joins exit 0b. ((`safety-eval2`))
fixtures: error_149_loop_cond_moves_outer.con error_149_return_after_unreached_defer.con error_149_return_in_borrow_block.con error_149_labelled_break_skips.con error_149_labelled_continue_skips.con regress_149_exits_accounted.con regress_149_labelled_exit_runs_defers.con regress_149_return_in_borrow_with_defer.con regress_149_break_in_borrow_with_defer.con error_149_diverging_branches_in_loop.con error_149_loop_cond_in_exiting_branch.con error_149_match_diverge_in_loop.con regress_149_both_branches_return.con regress_149_cond_return_in_borrow.con error_149_continue_in_exit_branch.con regress_149_nested_loop_in_exit_branch.con regress_149_break_value_statement_loop.con error_149_break_value_non_copy.con error_149_break_value_moves_outer.con error_149_loop_revives_consumed.con error_149_rebinding_nested_loop.con regress_149_inner_label_in_exit_branch.con
class: 15 — loop conditions, nested returns and labelled exits
silent: yes
exit: 0b
-->

**Status:** FIXED 2026-10-22 (branch `safety-eval2`)
**Found:** 2026-10-22, by the Codex review of `safety-eval2` (the checks predate the branch; bug
146's fix made the loop-condition case release a heap cell twice instead of leaking it).
**Severity:** skipped or repeated release in safe code, on three exit paths the checker did not
model.

## Reproducers

`tests/regressions/bug149_exit_paths_unchecked/repro.con`:

```con
fn main() with(Alloc) -> Int {
    let h: Heap<Int> = alloc(42);
    let mut n: Int = 0;
    while *h == 42 && n < 2 {
        n = n + 1;
    }
    return 0;
}
```

A `while` condition runs on every iteration, but it was checked outside the loop, so moving the
outer `h` there (`*h` moves the value out and, since bug 146, releases the cell) was accepted. On
main before bug 146 the cell leaked; with bug 146 the second iteration releases it again.

`repro_return.con`: a `return` inside a borrow block left `h` live, and the `free(h)` after the
block, never reached on that path, was credited. The same held for a statement after `return`
(`return 0; defer free(h);`). A labelled `break 'outer` / `continue 'outer` from an inner loop
checked only the inner loop's locals, so the outer body's live values were skipped.

Fixed: all refused at the checker, natively and under `--interp` (E0207, E0208, E0210, E0211).

## Cause

`Concrete/Check/Check.lean`: the `while`/`for` condition was checked at the enclosing loop depth;
a `return` checked nothing about live bindings (only `if`/`match` branches that exit the function
ran `checkReturnPathConsumed`); `break`/`continue` compared locals against the innermost loop depth
whatever their label.

## Fix

- Loop conditions are checked at the loop's depth, so moving an outer binding there is E0207.
- `checkReturnAccountsForAll`: at every `return`, each live owned non-Copy binding (not a reference,
  not a `ghost let`) must be consumed or reserved by a pending `defer` (E0208).
- `TypeEnv.loopLabelDepths` records each label's loop depth; a labelled `break`/`continue` checks
  the locals of every loop it exits (E0210 / E0211).
- `Lower`'s `emitDeferredUntilLoop` unwinds the deferred calls of every block a labelled
  `break`/`continue` leaves (`loopsExitedBy`), not only those inside the innermost loop (Codex
  review: `'outer: while .. { defer free(h); while true { break 'outer; } }` never released `h`;
  the interpreter already ran them). `regress_149_labelled_exit_runs_defers.con` (prints 3 in both
  engines).
- The return check sees through a borrow block's freeze (`VarInfo.frozenFrom`), so a `return`
  inside a borrow block whose owner has a pending `defer` is accepted
  (`regress_149_return_in_borrow_with_defer.con`, 42 in both engines); `break`/`continue` likewise
  (`regress_149_break_in_borrow_with_defer.con`).
- `restoreAfterLoop`: after a loop, a binding declared before it keeps its pre-loop state; a
  binding consumed before the loop and given a new value inside it has no single state after a
  loop that may run zero times, and is E0212 (`error_149_loop_revives_consumed.con`).
- The H14 one-level exemption (a `break value` could move a binding declared outside the loop) is
  withdrawn: loops are statements since 6D, so the value is discarded anyway, and the move was
  credited on paths that never ran it (`if c { break free(h); }` then `free(h)` after the loop
  released `h` twice; `error_149_break_value_moves_outer.con`, E0207). `Lower` now evaluates a `break value` operand in a
  statement loop too (it was skipped when the loop had no result slot, although the checker
  credited its moves: `while true { break free(h); }` never released `h`;
  `regress_149_break_value_statement_loop.con`), and a non-Copy break value, which a statement
  loop discards, is E0287 (`error_149_break_value_non_copy.con`). Any other consumption of an outer binding inside
  the body is on a path that leaves the function, and the loop may run zero times, so crediting it
  after the loop accepted a value never consumed (`while c { if .. { defer free(h); return 1; }
  else { .. return 2; } } return 0;` and the same with a `match`; `error_149_diverging_branches_in_loop.con`,
  `error_149_match_diverge_in_loop.con`, E0208). A loop condition does not inherit an enclosing
  function-exiting branch's exemption (`error_149_loop_cond_in_exiting_branch.con`, E0207), and the
  if/else return-path check sees through a borrow freeze too (`regress_149_cond_return_in_borrow.con`).
  `regress_149_both_branches_return.con` pins that an if/else whose branches both consume and
  return stays accepted.
- The consume-inside-loop exemption for a branch that ends in `return` (`exemptExitBranch`) is
  refused when the branch can `break`/`continue` out of the loop first (an earlier conditional
  `continue` repeated the consumption on the next iteration; `error_149_continue_in_exit_branch.con`,
  E0207), and a nested loop inside such a branch no longer drops the exemption for the statements
  after it (`regress_149_nested_loop_in_exit_branch.con`); a labelled break naming a loop inside the
  branch does not count as leaving it (`regress_149_inner_label_in_exit_branch.con`). The
  reassignment exemption (`h = f(h)` consuming `h` inside a loop) does not reach into a loop nested
  in the initializer (`error_149_rebinding_nested_loop.con`, E0207). (The H14 break exemption, which this
  relied on for `break v`, is withdrawn below.)
- `isCopyType` resolves a type alias at the head of a type and of a newtype payload (Codex noted
  `newtype N = Num` with `type Num = i64` judged non-Copy; such a newtype cannot currently be
  constructed, CoreCheck refuses the cast, so this had no reachable effect).

Migration: 1 test (`pressure_interleaved_linear.con` returned inside a borrow block while `r1` was
live; the sum is now carried out of the block). Fixtures (`run_tests.sh`):
`error_149_loop_cond_moves_outer.con` (E0207),
`error_149_return_after_unreached_defer.con`, `error_149_return_in_borrow_block.con` (E0208),
`error_149_labelled_break_skips.con` (E0210), `error_149_labelled_continue_skips.con` (E0211), and
the positive control `regress_149_exits_accounted.con` (prints 9).

## Structural cause

Ownership state not modeled along control-flow paths ([structural class](STRUCTURAL_CLASSES.md) 15):
each exit form (loop back-edge, return, labelled jump) was checked by its own partial rule.

## Structural owner

Containment: ROADMAP exit 0b. Structurally R-0499 (S8: use-once and conservation at every reachable
exit, over Core's explicit control flow).
