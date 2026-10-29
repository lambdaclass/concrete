# Bug 309 — a loop in the right operand of && or || rebound an aggregate that the short-circuit merge did not reconcile (E0703)

<!-- bug-ledger
summary: the short-circuit lowering of `&&` / `||` merged only the scalars its right operand changed and skipped every aggregate, on the assumption that a boolean operand cannot change one; a loop inside the right operand (in a value `match` or `if`) promotes every aggregate in scope and un-promotes it at its exit, rebinding it to a load made on the right operand's path, so after the merge the aggregate named a register that does not dominate its later uses and the program was refused (E0703)
fixtures: regress_309_shortcircuit_aggregate_merge.con
class: 8 — the short-circuit merge was a fourth, hand-written branch merge with its own rule for aggregates
silent: no
ruling: Violates: PASS-9@1
-->

**Status:** FIXED (branch `wip/bug-309`)
**Found:** 2026-10-27, by the differential fuzzer's borrow mode (`fuzz_differential.py --mode
borrow`, seed 33, depth 4) on a build with bugs 301 – 307 fixed; main refuses it too.
**Severity:** loud. A valid program is refused with an SSA-verify error; `--interp` runs it.

## Reproducer

```
struct Copy P { x: i64, y: i64 }
fn idv(v: i64) -> i64 { return v; }
fn f(c: bool, pp: P) -> i64 {
    let b: bool = c || idv(match pp.x { 0 => { let mut k: i64 = 0; while k < 2 { k = k + 1; } 1 }, _ => 2 }) > 1;
    if b { return pp.y; }
    return pp.x;
}
fn main() with(Console) { println(f(false, P { x: 0, y: 4 })); }
```

## Observed

On main ea21a2e8 `--interp` prints `0`; the compiler refuses the program: `error[ssa-verify]:
(E0703) f: block 'then10': use of %match.load.13 defined in non-dominating block 'merge2'`. A value
`if` in the operand, and `&&`, fail the same way. `regress_309_shortcircuit_aggregate_merge.con`
covers `||` with a `match` and `&&` with an `if`; main refuses it.

## Expected

The program is valid and runs as `--interp` does.

## Cause

`IR/Lower.lean`, `lowerExpr`'s `.binOp` case for `.and_` / `.or_`: after the right operand it built
phis for the scalars the operand rebound and skipped aggregates ("a still-unpromoted aggregate
cannot have changed"). A loop lowered inside the operand promotes every aggregate in scope
(`isAggregateForPromotion`) and at its exit loads it back and rebinds the name; the value `match` /
`if` around it then merges it into a `match.load` / `ifexpr.load` register in its own merge block,
inside the operand. The short-circuit merge left that binding in place for the path that skipped
the operand (Codex confirmation, 2026-10-27).

## Fix

The short-circuit merge calls `reconcileBranchVars`, the merge of the statement `if`, the value `if`
and `match`, with the right operand's end and the skipping block (carrying the pre-branch values)
as its two arms (`shortCircuitMergeRules`, which keeps this site's phi prefix). An aggregate changed
in the operand merges through an entry-block slot like any other branch's (bug 033's rule, never a
phi); the scope cleanup of bug 303 applies too.

## Structural cause

Class 8: control flow decided ad hoc in Lower. The short-circuit operator kept its own merge loop,
with its own belief about which bindings can change, beside the shared merge (ROADMAP 1a
consolidated the other three); the belief was invalidated by the loop's promotion scheme.

## Structural owner

R-0501 (S10 Place MIR) and ROADMAP #5's structured control-flow builder: one merge construction for
every branch form. Until then every branch site, the short-circuit operators included, merges
through `reconcileBranchVars`.

Codex confirmation 2026-10-27: CONFIRMED — Loop-induced aggregate rebinding inside a short-circuit RHS survives the scalar-only merge, causing valid subsequent aggregate reads to fail SSA dominance verification with E0703.
