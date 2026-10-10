# Bug 078: an if/else whose arms both diverge left its merge block open

**Status:** FIXED
**Found:** 2026-10-10, while writing fixtures for the `divergence-detection` mutation family.
**Severity:** a valid program is rejected after checking passes (SSA verification, E0703). No
binary is produced, so this fails closed; it is not wrong code.

## Reproducer

```
fn main() -> i32 {
    let a: [i32; 2] = [7, 8];
    let c: bool = false;
    if false { if c { return 1; } else { return 2; } }
    return a[0];
}
```

`concrete repro.con -o out` failed with
`error[ssa-verify]: (E0703) main: block 'merge2': use of %t0 defined in non-dominating block 'entry'`.
The interpreter returned 7. The expression form failed the same way
(`let v: i32 = if c { return 1; } else { return 2; };` inside a branch, `ifexpr.merge`), and so
did a `break`/`return` pair inside a loop.

It needs a value that is live across the enclosing `if` and lowered through memory (an aggregate,
or a linear struct); a scalar that no branch changes produces no phi and hid the bug.

## Cause

`Concrete/IR/Lower.lean` lowered statement `ifElse` and expression `ifExpr` by starting the merge
block unconditionally. When both arms terminated, nothing branched to it, but it was left open, so
the statements after the `if` were lowered into an unreachable block and an enclosing `if` saw a
then-branch that fell through. Its merge then took the unreachable block as a phi source, and the
verifier rejected the use (a block unreachable from `entry` is not dominated by it).

Match lowering already terminated its merge with `unreachable` when every arm diverged; the two
`if` forms did not.

## Fix

Both forms now terminate the merge with `unreachable` when both arms diverged and restore the
pre-`if` variable map, so later statements are skipped and an enclosing construct sees a diverged
arm. The expression form returns `unit` without loading its result slot.

## Gate

`scripts/tests/check_divergence_detection.sh` (CI, language-surface job): four bug 078 rows, plus
the divergence fixture that found it, compile, run and agree with `--interp`. Against the previous
compiler those five rows fail with E0703 and the other seven pass.
