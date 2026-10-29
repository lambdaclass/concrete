# Bug 303 — the value if did not drop the bindings its arms declare, so a later loop's phi took a value defined on one arm's path

<!-- bug-ledger
summary: the value `if` (lowerExpr's `.ifExpr`) kept the bindings its arms declare in Lower's variable table after its merge, while the statement `if` and `match` dropped them; a loop counter or `let` declared in an arm stayed bound to a register defined on that arm's path, and the next loop's header phi took it as a pre-loop value, so a valid program was refused (E0708)
fixtures: regress_303_value_if_arm_scope.con
class: 8 — each branch merge site did its own scope cleanup, and one did not
silent: no
ruling: Violates: PASS-9@1
-->

**Status:** FIXED (branch `wip/bug-303`)
**Found:** 2026-10-26, by the differential fuzzer's new borrow mode (`fuzz_differential.py --mode
borrow`, seed 1, case 6), hunting wrong code in Lower's handling of borrows.
**Severity:** loud. The compiler refuses a valid program with an SSA-verify error; `--interp` runs it.

## Reproducer

```
fn f(c: bool) -> i64 {
    let mut x: i64 = 1;
    let r: i64 = if c { 2 } else { let mut k: i64 = 0; while k < 3 { k = k + 1; } k };
    let mut j: i64 = 0;
    while j < 2 { x = x + j; j = j + 1; }
    return x + r;
}
fn main() with(Console) { println(f(true)); println(f(false)); }
```

## Observed

On main ea21a2e8 `--interp` prints `4` and `5`; the compiler refuses the program:
`error[ssa-verify]: (E0708) f: block 'while.hdr6': phi operand %phi.3 from 'ifexpr.merge2' not
dominated by def block 'while.hdr3'`. A `for` loop in the arm, or a plain `let t: i64 = y + 1;`,
does the same. `regress_303_value_if_arm_scope.con` covers the three; main refuses it.

## Expected

An arm's bindings are out of scope after the `if` (STATEMENT_EXPRESSION_MODEL); the interpreter
drops them (`Interp.lean`, `.ifExpr`), and the binary computes what it computes.

## Cause

`IR/Lower.lean`. After a branch merge (`reconcileBranchVars`) the statement `if` and `match`
restricted the variable table to the bindings in scope before the branch (`restrictTo`, H7 and
WC-0004); the value `if` did not. The table left after it held the else arm's bindings; a loop
lowered after it builds a header phi for every unpromoted scalar in the table, with the value from
the pre-loop block, which on the `then` path is not defined (Codex confirmation, 2026-10-26).

## Fix

The scope cleanup is part of the merge: `reconcileBranchVars` ends by restricting the table to the
pre-branch bindings, and the statement `if` and `match` no longer do it themselves. A branch merge
cannot be written without it.

## Structural cause

Class 8: control flow decided ad hoc in Lower. Each branch construct ran its own merge epilogue;
bug 078/175 (merge reachability) and H7/WC-0004 (this scope cleanup, at the other two sites) are
the same shape: one site missed one step.

## Structural owner

R-0501 (S10 Place MIR) and ROADMAP #5's structured control-flow builder: one merge construction for
every branch form. Until then the scope cleanup lives in the one merge function all three sites call.

Codex confirmation 2026-10-26: CONFIRMED — Value-if lowering omits arm-local scope cleanup, allowing a leaked branch register to become a non-dominating incoming operand of a subsequent loop phi.
