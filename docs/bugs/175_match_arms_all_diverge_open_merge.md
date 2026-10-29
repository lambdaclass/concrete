# Bug 175: a match whose arms all diverge left its merge block open

<!-- bug-ledger
summary: a `match` whose arms all diverge (break / continue / return) left its merge block open whenever the match was unit-typed, because the early return in result handling made the `unreachable` termination dead code. A loop around it failed SSA verification (E0708, a header phi fed from the unreachable block); under an `if` it failed with E0703 (the bug 078 shape). Fails closed. Pinned by `regress_175_match_arms_all_diverge.con` (run_ok plus `--interp == binary`) and four `check_divergence_detection.sh` rows
fixtures: regress_175_match_arms_all_diverge.con
class: 8 — a `match` whose arms all diverge left its merge open: 078's shape, missed in `match`
silent: no
ruling: Violates: PASS-9@1
-->

**Status:** FIXED
**Found:** 2026-10-22, by the safety-eval2 agent, which noted it as a pre-existing limitation.
**Severity:** a valid program is rejected after checking passes (SSA verification, E0708 or
E0703). No binary is produced, so this fails closed; it is not wrong code.

## Observed

```
fn main() -> i32 {
    let mut n: i32 = 0;
    while n < 10 {
        n = n + 1;
        match n {
            1 => { break; },
            _ => { break; },
        }
    }
    return n;
}
```

`concrete repro.con -o out` failed with:

```
error[ssa-verify]: (E0708) main: block 'while.hdr0': phi operand %t2 from 'merge3' not dominated by def block 'while.body1'
```

`--interp` printed 1. The same happened with:

- a labelled `break 'outer` in every arm (`while.hdr3` / `merge6`);
- an enum scrutinee whose arms `continue` and `break`;
- a mix of `continue`, `break` and `return` arms. With an array live across the loop, this also
  failed with E0703 at the loop exit.

Outside a loop, the bug 078 shape written with a `match` failed the same way. That shape is a
`match` whose arms all `return`, inside an `if`, followed by a read of an aggregate:
`error[ssa-verify]: (E0703) ...: block 'merge2': use of %t0 defined in non-dominating block
'entry'`.

The language has no value-carrying `break` (the value `while ... else` was removed in Phase 6D).
An expression-position match whose arms all diverge (`let w: i32 = match k { 1 => { continue; },
_ => { break; } };`) is refused by the checker with E0220 ("expected i32, got ()") before
lowering. That is a separate front-end limitation, and it also fails closed.

## Expected

Each program compiles, and the binary's exit code equals what `--interp` prints.

## Structural cause

`Concrete/IR/Lower.lean`, the `match_` case of `lowerExpr`, decided whether the merge block was
reachable in two separate places:

1. The variable reconciliation counted the arms that reached the merge (`liveSnapshots`). When
   there were none, it restored the pre-match variables, but it did not terminate the block.
2. The result handling terminated the merge with `unreachable` only in its last `else`. The
   first test before it, `ty == .unit || ty == .never || realPhiIncoming.isEmpty`, returned
   early. It is true whenever no arm reached the merge, because diverged arms contribute no phi
   incoming. So that `else` could never run.

The merge was therefore left open whenever every arm diverged. The statements after the `match`
were lowered into that unreachable block. Inside a loop, it branched back to the header, so the
header phi got an incoming edge from it carrying a value defined in the loop body, which does not
dominate it (E0708). Under an `if`, the enclosing merge used it as a phi source (E0703).

This is the class-8 shape of bug 078 ("an if/else whose arms both diverge left its merge block
open"). Bug 078's record and the comment on its fix both said that match lowering "already
terminated its merge with `unreachable` when every arm diverged". The code was there, but it was
unreachable, so the fix for 078 never checked it against a `match`. Each branch construct still
decides on its own whether its merge is reachable. No shared builder primitive answers that
question, and SSA verification is the only thing that notices a mistake.

## Structural owner

Class 8 in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md): storage, places and control flow are
decided ad hoc in `Lower`. The owners are R-0501 (S10 Place MIR, explicit control-flow edges) and
R-0504 (S12 validators). The nearer owner is ROADMAP #5, the structured Lower/SSA builder, which
fixes this kind of defect in slices pulled by bugs. A merge-block primitive that terminates
itself when it has no predecessors would retire the per-construct copies in statement `if`,
`ifExpr` and `match`.

## Fix

When no arm reaches the merge, match lowering now restores the pre-match variables, terminates
the merge with `unreachable`, and returns `unit` before the result handling. This is what the
two `if` forms have done since bug 078. The dead `else` at the end of the result handling was
removed; it is now the single-incoming case.

## Containment

The defect failed closed: SSA verification refused every program it affected, and no binary was
produced.

The following tests now cover it:

- `tests/programs/regress_175_match_arms_all_diverge.con` contains five shapes: every arm breaks;
  every arm breaks to an outer label; an enum with `continue`/`break` arms; `continue`/`break`/
  `return` arms with an aggregate live across the loop; and every arm returns inside a branch. The
  binary exits 42.
- In `run_tests.sh`, `run_ok` runs the fixture, and the `--interp == binary` loop runs it with
  the bugs 102/105 legs.
- `check_divergence_detection.sh` has four bug 175 rows (break, labelled break, mixed arms with
  an aggregate, all-return in a branch). Each row compiles, runs and agrees with `--interp`.

On the previous compiler, every helper in the fixture fails SSA verification (five E0708 and two
E0703 diagnostics).
