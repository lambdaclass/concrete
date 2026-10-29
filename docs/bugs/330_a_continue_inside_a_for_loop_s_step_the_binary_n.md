# Bug 330 — a continue inside a for loop's step: the binary never terminates and the interpreter goes to the condition

**Status:** FIXED (branch `wip/bug-330-fix`): Check refuses the form (E0245), as the owner decided on 2026-10-29
**Found:** 2026-10-29, by a Kimi review of a spec delta (a `for` step's control flow), then hand
probes of `break`, `continue` and `return` inside `for` steps on main 71df9c25.
**Severity:** the compiled program never terminates where `--interp` finishes; which result is
right is not decided by any rule (the ruling below), so this is not counted as silent wrong code.

<!-- bug-ledger
summary: a continue inside a for loop's step: the binary never terminates and the interpreter goes to the condition
fixtures: error_330_continue_in_for_step.con error_330_break_in_for_step.con regress_330_for_step_exits_allowed.con
class: 12
silent: no
ruling: Spec gap: STMT — a `break` or `continue` inside a `for` loop's step that targets that loop is refused (the owner's decision of 2026-10-29, until C-style `for` is removed; no STMT entry states it yet)
-->

## Reproducer

Now `tests/programs/error_330_continue_in_for_step.con` (refused, E0245); first filed as
`tests/regressions/bug330_a_continue_inside_a_for_loop_s_step_the_binary_n/repro.con`:

```
fn f() -> i64 {
    let mut n: i64 = 0;
    for (let mut k: i64 = 0; k < 5; if k == 2 { k = 4; continue; } else { k = k + 1; }) {
        n = n + 1;
        if n > 50 { return 99; }
    }
    return n;
}
fn main() -> Int { return f() as Int; }
```

The step is a statement `if`, which Check accepts with a `continue;` in an arm. The value forms
`k = if k == 2 { continue; } else { k + 1 }` and `k = match k { 2 => { continue; }, _ => k + 1 }` are
refused (E0224, E0225: the arm ending in `continue;` has type `()`); a `return` in a value arm is
accepted (`k = if k == 2 { return 40 + n; } else { k + 1 }`, binary = `--interp` = 43).

## Observed

On main 71df9c25 the binary does not terminate (killed by a 10 s timeout, status 124) and `--interp`
prints 4. On 36d22567, before bug 306's interpreter change, the binary does not terminate either
and `--interp` printed 3: bug 306 did not introduce the disagreement, it changed the interpreter's
side of it. With `break;` or `return 40 + n;` in place of `k = 4; continue;` the two agree (3, 43).

## Expected

No rule says where a `continue` inside a `for` loop's step that targets that same loop goes (a
labelled `continue` of an outer loop, or one belonging to a loop nested in the step, follows its own
loop's rules).
[STMT-6@2](../spec/statements.md#STMT-6) says which loop a `continue` acts on, and its rationale has
a `continue` in the body run the step; neither says what a `continue` in the step does. Three
readings: (a) it re-runs the step (the literal reading, Lower's continue target): P = 3; (b) it ends
the step and goes to the condition (the interpreter since bug 306): P = 4; (c) it is refused. Under
every reading the binary's non-termination is wrong (Codex, 2026-10-29). The owner chooses; until
then the record stays open.

## Cause

Two definitions of a step's `continue`:

- `IR/Lower.lean`, `lowerStmt`'s `.while_`: the loop's `continueTarget` is the step block, and stays
  installed while the step is lowered; the step block's entry phis are built before the step is
  lowered, from the `continue` edges collected so far, so a `continue` edge from inside the step
  never feeds them. The re-entered step reads the header's `k` (2), not the 4 the edge carried, and
  loops (Codex confirmation, 2026-10-29).
- `Interp/Interp.lean`, `evalWhile`'s `stepThenLoop` (bug 306): a `continue` of this loop inside the
  step goes to the next iteration's condition. Before bug 306 the interpreter discarded the step's
  control flow on the `continue` paths.

## Fix

The owner decided on 2026-10-29 to remove C-style `for` (loops become `while` and
`for i from a until b`) and, until then, to refuse the form (option (c)). Check refuses a `break` or
`continue` inside a `for` loop's step whose target is that loop (E0245, "`continue` inside a `for`
loop's step cannot leave or restart that loop"): while it checks the step, `forStepLoopDepth` holds
the loop's depth, and `refuseExitInForStep` resolves the exit's target exactly as `break` and
`continue` do (the innermost loop, or the labelled one) and refuses it when it is that depth. A
`return` in the step, the exits of a loop nested inside the step and a labelled `continue` of an
outer loop from an inner loop's step stay accepted, and the binary and the interpreter agree on them
(`regress_330_for_step_exits_allowed.con`, total 87, binary = `--interp`). E0245 is registered under
STMT-6 (`codes.tsv`, the entry's `Codes:` line, `codeRuleTag`). The interpreter's comment in
`evalWhile` now says the step's own `.brk` / `.cont` arms see only an outer loop's exits.

Regression legs in `run_tests.sh`: `error_330_continue_in_for_step.con` and
`error_330_break_in_for_step.con` (labelled `break`) refused with E0245, and
`regress_330_for_step_exits_allowed.con` (binary 42). The open-bug reproducer row is removed.

## Structural cause

Class 12: one fact kept in two places. What a `for` step's own control flow does is decided
separately by Lower (its continue target and step-entry state) and by the interpreter
(`stepThenLoop`), and by no rule; Core's `.while_` also keeps the step twice (bug 306). Bug 306's
comment in `evalWhile` claimed Check refuses every step that could contain a `continue`; it refuses
only the value forms (corrected with this record).

## Structural owner

The removal of C-style `for` the owner decided on 2026-10-29 (loops become `while` and
`for i from a until b`), which removes step blocks and with them this second definition; until then
the refusal (E0245) keeps any such exit from reaching Lower or the interpreter. Then R-0496 (S7, the
interpreter is Core's semantics) for one definition of a loop's control flow.
