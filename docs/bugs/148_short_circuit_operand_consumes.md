# Bug 148 — a move inside the right operand of `&&` / `||` counted although it may not run

<!-- bug-ledger
summary: a move inside the right operand of `&&`/`||` counted as a consumption although the operand may not run (`false && consume(h)` leaked the cell; found by Codex). E0244. `error_148_*.con`, `regress_148_short_circuit_reads.con`. Joins exit 0b. ((`safety-eval2`))
fixtures: error_148_and_operand_consumes.con error_148_or_operand_consumes.con regress_148_short_circuit_reads.con
class: 15 — a short-circuit operand treated as sequential
silent: no
exit: 0b
ruling: Violates: OWN-11@1
-->

**Status:** FIXED 2026-10-22 (branch `safety-eval2`)
**Found:** 2026-10-22, by the Codex review of `safety-eval2` (the operand checking predates the
branch).
**Severity:** skipped release in safe code: the checker treated a value moved inside the right
operand of a short-circuit operator as consumed on every path, but that operand runs only when the
left one does not decide the result.

## Reproducer

`tests/regressions/bug148_short_circuit_operand_consumes/repro.con`:

```con
fn consume(h: Heap<Int>) with(Alloc) -> bool {
    free(h);
    return true;
}

fn main() with(Alloc) -> Int {
    let h: Heap<Int> = alloc(42);
    let b: bool = false && consume(h);
    if b { return 1; }
    return 0;
}
```

On main (`0408f4d4`): accepted; the binary allocates `h`, jumps straight to the short-circuit merge
and returns: `consume` never runs and the cell is never freed. Fixed: refused at the checker,
natively and under `--interp`:

```
error[check]: (E0244) the right operand of '&&' may not move or consume 'h': it runs only when the left operand does not decide the result, so the move may never happen
```

## Cause

`Concrete/Check/Check.lean` checked both operands of every binary operator in sequence against one
environment, so a consumption in the right operand of `&&` / `||` updated the state as if it always
happened.

## Fix

The `&&` / `||` case checks the left operand, snapshots the environment, checks the right operand,
and refuses any change of a non-Copy binding's ownership state made by it
(`refuseConditionalConsumption`, E0244 `conditionalOperandConsumes`). Reads and borrows are
unaffected. Fixtures `error_148_and_operand_consumes.con`, `error_148_or_operand_consumes.con`, and
the positive control `regress_148_short_circuit_reads.con` (native and `--interp`). Migration: 0
sites.

## Structural cause

Ownership state not modeled along control-flow paths ([structural class](STRUCTURAL_CLASSES.md) 15):
short-circuit evaluation is a branch, and the checker treated it as sequence.

## Structural owner

Containment: ROADMAP exit 0b. Structurally R-0499 (S8): the Core checker sees `&&` / `||` as the
branches they lower to, so a move in one arm is a branch-merge disagreement there.
