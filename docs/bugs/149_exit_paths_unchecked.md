# Bug 149 — loop conditions, returns in nested blocks and labelled exits were not checked for ownership

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
- `isCopyType` resolves a type alias at the head of a type and of a newtype payload (Codex noted
  `newtype N = Num` with `type Num = i64` judged non-Copy; such a newtype cannot currently be
  constructed, CoreCheck refuses the cast, so this had no reachable effect).

Fixtures (`run_tests.sh`): `error_149_loop_cond_moves_outer.con` (E0207),
`error_149_return_after_unreached_defer.con`, `error_149_return_in_borrow_block.con` (E0208),
`error_149_labelled_break_skips.con` (E0210), `error_149_labelled_continue_skips.con` (E0211), and
the positive control `regress_149_exits_accounted.con` (prints 9).

## Structural cause

Ownership state not modeled along control-flow paths ([structural class](STRUCTURAL_CLASSES.md) 15):
each exit form (loop back-edge, return, labelled jump) was checked by its own partial rule.

## Structural owner

Containment: ROADMAP exit 0b. Structurally R-0499 (S8: use-once and conservation at every reachable
exit, over Core's explicit control flow).
