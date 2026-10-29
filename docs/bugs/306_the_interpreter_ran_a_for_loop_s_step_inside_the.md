# Bug 306 — the interpreter ran a for loop's step inside the body's scope, before the body's deferred calls

<!-- bug-ledger
summary: Elab desugars `for (init; cond; step) { body }` to a `.while_` whose body list ends with the step and which also carries the step separately; the interpreter evaluated the whole list as the body's block, so on fall-through the step ran before the body's deferred calls (a deferred call reading the counter saw the stepped value), while on `continue` it ran after them; the binary runs the body's block, then the step, on both (silent divergence: the differential reference was wrong)
fixtures: regress_306_for_step_after_body_defers.con
class: 12 — the step is kept twice in Core's `.while_`, and each consumer cut the body's block out of it on its own
silent: yes
exit: 0b
ruling: Spec gap: STMT — every exit of a scope that does not abort runs one exit sequence, and a `for` loop's step runs after its body's (OD2 part 5, decided 2026-10-29; written as STMT-11@1 on wip/spec-od, cited here once it is on main)
-->

**Status:** FIXED (branch `wip/bug-306-od2`), under the owner's decision OD2 (2026-10-29)
**Found:** 2026-10-27, by the differential fuzzer's new borrow mode (`fuzz_differential.py --mode
borrow`): every remaining mismatch of 1000 programs on a build with bugs 301 – 305 fixed was this
one, and none remained with `for` loops off (`BorrowGen.fors`).
**Severity:** silent. `--interp`, the reference of every differential gate, disagreed with the
binary; here the binary is right.

## Reproducer

```
fn show(p: &i64) with(Console) { println(*p); }
fn f() with(Console) {
    for (let mut k: i64 = 0; k < 3; k = k + 1) {
        defer show(&k);
        if k == 1 { continue; }
    }
}
fn main() with(Console) { f(); }
```

## Observed

On main ea21a2e8 `--interp` prints `1 1 3`, the binary `0 1 2`. Without the `continue` line
`--interp` prints `1 2 3`, the binary `0 1 2`. `regress_306_for_step_after_body_defers.con`:
the binary exits 42; `--interp` on main 0bf8350a stops with `arithmetic overflow (checked *)` (the
fixture now also covers a `break`, a labelled `continue` out of a nested `for` body, a `return` from the body, and a step
that itself returns, reached by `continue` and by falling through; main's interpreter dropped the
step's `return` after a `continue` and ran on, until the recorded digits overflowed).

## Expected

The body's block ends, running its deferred calls, before the step. OD2 part 5 (decided
2026-10-29) gives every non-abort exit of a scope one exit sequence: the exit value, then that
scope's deferred calls (LIFO, innermost scope first), then the end of its loans, then the
destruction of its remaining locals. The body's block is a scope and the step is not part of it,
so on a fall-through end of the body, as on `continue`, the sequence completes before the step
runs; `break` and `return` leave without the step. STMT-6 (docs/spec/statements.md) already had
`continue` leave the loop body's scopes and still run the step. The binary did this on both paths;
the interpreter did on `continue` only.

## Cause

`Interp/Interp.lean`, `evalWhile`: on a true condition it evaluated `body` (which for a `for` loop
ends with the step, appended by Elab's desugaring) with one `evalStmts`, whose block runs its
deferred calls at its end, after the step; on `.cont` it ran `step` after the shortened block.
Lower (`lowerStmt`'s `.while_`) lowers `body.take (body.length - step.length)` as the block and the
step after it (Codex confirmation, 2026-10-26).

## Fix

`CStmt.whileBodyBlock body step` (Elab/Core.lean) names the body's own block once; Lower and the
interpreter both use it. The interpreter runs the step after the block through one local
definition (`stepThenLoop` in `evalWhile`) on every path that reaches it (fall-through, `continue`
of the loop, `continue` out of the condition), and handles the step's own control flow the same
way on each: before, the `continue` paths discarded it, so a step that returns after a `continue`
looped (Codex review, round 1 of this branch).

This is OD2 part 5 as the owner decided it: the option the record recommended (the body's
deferred calls before the step on every path).

## Structural cause

Class 12: one fact kept in two places. Core's `.while_` carries the step inside the body list and
again as its own field, and each consumer decided by itself which statements form the body's block.
CoreCheck checks the whole list as one block and the step again (no runtime effect; noted by the
Codex confirmation); the proof model reads the list with the step and refuses `defer` and
`continue` in loops.

## Structural owner

OD2 part 5 (decided 2026-10-29; the rule ID follows when the specification records it): one exit
sequence per scope, which the interpreter now runs for a `for` body exactly as Lower does; then
R-0496 (S7: the interpreter is Core's semantics) and the Core loop form, a `.while_` whose body does
not contain its step. Until then `CStmt.whileBodyBlock` is the one place that cuts it out.

Codex confirmation 2026-10-26: CONFIRMED — The interpreter incorrectly includes the appended for-loop step in the body's defer scope on fall-through, while native lowering correctly runs body defers before the step on both fall-through and continue.
