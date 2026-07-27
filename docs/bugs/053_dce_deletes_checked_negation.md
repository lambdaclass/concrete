# Bug 053: DCE deletes checked negations — documented trap silently vanishes

**Status:** Fixed (2026-07-27, R-0005)
**Discovered:** 2026-07-18, middle-end audit (reproduced:
`discard(-x)` with `x: i8 = -128` — compiled prints "survived", exit 0;
interp aborts with `arithmetic overflow (checked negation)`; the emitted
`user_main` contains no `__cc_ssub_i8` call).

## Symptom

Per ARITHMETIC_POLICY, `-x` traps when `x` is the type's MIN (exactly like
checked `0 - x`). The compiled binary silently skips the trap whenever the
negation's result is unused.

## Root cause

`isSideEffecting` (`Concrete/IR/SSACleanup.lean:325-340`) keeps checked
BINOPS live (the fuzz-seed fix) but omits `.unaryOp .neg` — even though
`foldConstants`' own comment documents that neg must stay live to trap, and
EmitSSA lowers integer negation to the checked `ssub/usub` helper. This is
the same class as the fuzz fix, one constructor over: the side-effect
inventory is incomplete for a trapping operation whose result is discarded.

## Candidate fix

Add `.unaryOp .neg` to the side-effecting set (integer widths — float neg
does not trap), mirroring the checked-binop handling. Regression:
`discard(-x)` at `i8` MIN must abort on BOTH paths; a differential fuzzer
case over discarded checked unary ops belongs in the positions gate.

## Fix as shipped

The candidate fix above — add `.unaryOp .neg` to the side-effecting set — is
correct and would close the reproducer. It was not what shipped, because it
leaves the defect's cause in place.

Four consumers each decided independently whether `-x` can trap: the
interpreter, `foldConstants`, EmitSSA, and DCE. Three agreed; DCE never asked.
The folder's comment saying neg must stay live to trap sat one screen above the
`isSideEffecting` arm that deleted it. Adding a fifth local answer would have
restored agreement without removing the thing that let them disagree, and the
next trapping unary constructor would land in exactly the same hole.

`Concrete/Semantics/IntArith.lean` now owns the unary trap inventory, the same
single-source treatment `evalIntBinOp` already gave the binary family:

- `evalIntUnaryOp op n ty : ArithResult` — WHAT the operation yields
  (`.value` / `.trap` / `.notApplicable`).
- `unaryOpCanTrap op ty : Bool` — WHETHER it can trap at all.

DCE reads `unaryOpCanTrap` and refines with `evalIntUnaryOp` for constant
operands, so a provably-safe discarded negation stays deletable; the
interpreter evaluates through `evalIntUnaryOp`. Preserving every discarded
unary op would also have passed a trap regression while quietly costing dead
work at every site — the gate checks both directions for that reason.

## Regression

`scripts/tests/check_trap_inventory.sh` (CI, and the hook's `trap` area for
`Concrete/IR/*`, `Concrete/Semantics/*`, `Concrete/Interp/*`):

- discarded `-x` at MIN aborts on compiled AND interp, at i8/i16/i32/Int;
- the used-result shape still traps (proving the gate observes discard, not
  negation in general);
- non-trapping discards (small const, `~x`, float neg) stay removable;
- `__cc_ssub` survives into the emitted `user_main`;
- structurally, all three consumers read `IntArith` — a value-only gate passes
  for any fix, including four consumers that agree today and drift tomorrow.

Mutations 25-27 in `scripts/tests/test_mutation.sh` are killed by that gate:
DCE ignoring the inventory, the inventory's answer inverted, and checked
negation wrapping at MIN instead of trapping.
