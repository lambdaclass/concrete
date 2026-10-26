# Bug 271 — after a branch whose last-lowered arm diverged, Lower kept that arm's writes

<!-- bug-ledger
summary: after an if, a value if or a match whose last-lowered arm diverged, Lower continued from that arm's variable table, so a binding only the diverged arm wrote kept its value there (silent wrong code, or E0703)
fixtures: regress_271_diverged_arm_writes.con
class: 8 — the merge's starting table
silent: yes
exit: 0b
-->

**Status:** FIXED (branch `wip/perf-backend`)
**Found:** 2026-10-25, while reading Lower's merge code for bug 266 (the scope cleanup after an `if`
takes the current table's values, and the current table is the else arm's).
**Severity:** silent wrong code. The native binary computes with a value assigned on a path that
never reaches the code using it; `--interp` is right. When the value is defined inside the
diverged arm the program is refused instead (E0703, a use the definition does not dominate).
Confirmed real by Codex review, 2026-10-25: reconciliation leaves bindings the live arm did not change bound to the diverged arm's values, in statement and value ifs and in matches with several live arms (the single-live-arm match path restores that arm's snapshot and avoids it).

## Reproducer

```
fn pick(c: bool, y: i64) -> Int {
    let mut x: i64 = y + 1;
    if c { } else { x = y; return 1; }
    return x as Int;
}
fn main() -> Int { return pick(true, 5); }
```

Observed: the binary exits 5, `--interp` prints 6. The same with a value `if` whose else arm writes
and returns, and with a `match` whose last arm does. With `x = y + 7` in the else arm the program
is refused with E0703.

## Expected

6: the assignment is on the path that returns 1 only (STATEMENT_EXPRESSION_MODEL, evaluation order;
the interpreter is the reference).

## Cause

`IR/Lower.lean`: the statement `if` and the value `if` lower the then arm, restore the pre-branch
table, lower the else arm, and merge in the state the else arm left; the `match` merges in the
state of the arm lowered last. `reconcileBranchVars` rebinds only the bindings the live arms
changed, so when the last-lowered arm diverged, a binding only it wrote kept its value in the table
the code after the branch reads.

## Fix

The merge starts from a live arm's table: the then arm's when the else arm diverged, and for a
`match`, the last live arm's when the last-lowered arm diverged. When the last-lowered arm is live
nothing changes, so the emitted IR of every other program is unchanged (the corpus and 3000
generated fuzz programs compare byte-identical). `regress_271_diverged_arm_writes.con` pins the
three forms and a `break`.

## Structural cause

Class 8: control flow decided ad hoc in Lower. Which table the code after a branch continues from
was an accident of lowering order ("the current state"), not a decision made from the arms that
reach the merge; bugs 078 and 175 are the both-arms-diverged forms of the same question.

## Structural owner

The structured control-flow builder (ROADMAP #5): the merge's input is the set of live arm tables,
never the builder's current state.
