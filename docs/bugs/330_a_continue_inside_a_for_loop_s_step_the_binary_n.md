# Bug 330 — a continue inside a for loop's step: the binary never terminates and the interpreter goes to the condition

**Status:** OPEN
**Found:** 2026-10-29, by a Kimi review of a spec delta (a `for` step's control flow), then hand
probes of `break`, `continue` and `return` inside `for` steps on main 71df9c25.
**Severity:** the compiled program never terminates where `--interp` finishes; which result is
right is not decided by any rule (the ruling below), so this is not counted as silent wrong code.

<!-- bug-ledger
summary: a continue inside a for loop's step: the binary never terminates and the interpreter goes to the condition
coverage: reproducer tests/regressions/bug330_a_continue_inside_a_for_loop_s_step_the_binary_n/repro.con, pinned by `check_example_outputs.sh` (open-bug reproducers leg, `compiles`)
class: 12
silent: no
ruling: Spec gap: STMT — what a `continue` inside a `for` loop's step does (re-run the step, go to the condition, or be refused)
-->

## Reproducer

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

## Structural cause

Class 12: one fact kept in two places. What a `for` step's own control flow does is decided
separately by Lower (its continue target and step-entry state) and by the interpreter
(`stepThenLoop`), and by no rule; Core's `.while_` also keeps the step twice (bug 306). Bug 306's
comment in `evalWhile` claimed Check refuses every step that could contain a `continue`; it refuses
only the value forms (corrected with this record).

## Structural owner

The owner's answer to the ruling (what a `continue` in a step does, or a refusal), then one
definition of a loop step's control flow shared by Lower and the interpreter: R-0496 (S7, the
interpreter is Core's semantics) and a Core loop form whose body does not contain its step. A
counted `for i from a to b` (pending simplification) would remove step blocks altogether.

## Containment while open

`check_example_outputs.sh`'s open-bug reproducers leg pins that main accepts the reproducer
(`compiles`; the binary is not run, since it does not terminate). Meanwhile do not write `continue`
inside a `for` loop's step; write the step's logic in the body.
