# Bug 183 — a dereference assignment evaluates its destination before its right-hand side in the binary

<!-- bug-ledger
summary: a dereference assignment `*(&mut a[i()]) = j()` lowered its destination before its right-hand side in the binary (exit 12; `--interp` and rule 7: 21). `Lower` now lowers the right-hand side first, like every other assignment form; pinned by `regress_183_deref_assign_rhs_first.con` (three shapes, `--interp == binary`, 129) and the reproducer's `run_ok`
fixtures: regress_183_deref_assign_rhs_first.con
class: 7 — a dereference assignment's operand order
silent: yes
exit: 0b
-->

**Status:** FIXED (branch `soundness-holes`)
**Found:** 2026-10-21, while writing the evaluation-order rule (the semantics decisions of
2026-10-21; recorded in [STATEMENT_EXPRESSION_MODEL.md](../language/STATEMENT_EXPRESSION_MODEL.md)
and in ROADMAP exit 0b as "not yet filed"); reproduced and filed 2026-10-23 by the
language-tightening probes ([TIGHTENING_DECISIONS.md](../language/TIGHTENING_DECISIONS.md)).
**Severity:** silent wrong code: the binary and `--interp` run the effects of one statement in
different orders, and the binary's order is not the rule's.

## Reproducer

`tests/regressions/bug183_deref_assign_destination_first/repro.con`:

```con
fn tick(c: &mut Int, n: Int) -> Int { *c = *c * 10 + n; return n; }
fn main() -> Int {
    let mut c: Int = 0;
    let mut a: [Int; 3] = [0, 0, 0];
    *(&mut a[tick(&mut c, 1)]) = tick(&mut c, 2);
    return c;
}
```

Expected (rule 7: the right-hand side, then the destination's subexpressions): `c` records 2 then
1, exit 21. Observed on main `185d998c` and `wip/train3` `949d79e9`: the binary exits 12 (the
destination's `tick` ran first); `--interp` prints 21. The same order shows with printing ticks
(`println` in `tick`: the binary prints 1, 2 and `--interp` 2, 1).

## Cause

`Lower.lean`, `lowerStmt` case `.derefAssign target value`, lowers `target` before `value`;
`Interp.lean`'s `.derefAssign` evaluates `value` first. An indexed assignment (`arr[i()] = j()`)
already lowers the value first in both.

## Structural cause

Semantics implemented several times ([structural class](STRUCTURAL_CLASSES.md) 7): evaluation order
is decided separately by each consumer's statement case, and no shared Core semantics fixes it.

## Structural owner

R-0496 (S7 Core semantics) with the evaluation-order rule's gate in exit 2j (R-0500) and the
permanent differential leg in exit 2c (R-0491). Narrow repair under exit 0b: lower `value` before
`target` in `Lower`'s `.derefAssign` case.

## Fix

`Lower`'s `.derefAssign` case lowers the right-hand side before the destination, as the
indexed and field assignment cases and the interpreter already did, so rule 7 holds in both
consumers. The structural owner (one Core semantics fixing the order for every consumer, R-0496)
is unchanged; this is the exit 0b repair the record names.

Regression fixture: `tests/programs/regress_183_deref_assign_rhs_first.con`: the reproducer's
indexed shape, a borrowed field of an array element, and a raw-pointer destination in trusted code,
each recording its order; exit 129 (21 + 43 + 65) natively and under `--interp` (`run_tests.sh`,
`run_ok` and the `--interp == binary` loop). The reproducer now exits 21 (`run_ok`); on the
previous compiler it exits 12.

## Containment while open (historical)

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode `runs-exit`) requires the
binary to exit 12; when it exits 21 that leg fails until this document, the
corpus entry and the leg are updated together.
