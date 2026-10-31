# Bug 350 — the proven-overflow arm of the compile-time safety diagnostics can never fire, so the documented refusal of a proven overflow never happens

**Status:** OPEN
**Found:** \1, by reading the proven-violation diagnostics while splitting E0900 for the spec's code ledger, and three probes under `#[overflow_checked]`; the unreachability argument confirmed by Codex and Grok
**Severity:** no wrong program behaviour: the run-time overflow trap still fires; the compile-time refusal of a proven overflow that the documentation describes can never happen, and the report's constant tier is dead code

<!-- bug-ledger
summary: the proven-overflow arm of the compile-time safety diagnostics can never fire, so the documented refusal of a proven overflow never happens
coverage: reproducer tests/regressions/bug350_the_proven_overflow_arm_of_the_compile_time_safe/repro.con, pinned by check_example_outputs.sh (open-bug reproducers leg, compiles)
class: 13
silent: no
ruling: Outside the spec: tooling — a diagnostic arm that can never fire; the run-time overflow trap, which the spec makes the guarantee, is unaffected
-->

## Reproducer

`tests/regressions/bug350_the_proven_overflow_arm_of_the_compile_time_safe/repro.con`: an `#[overflow_checked]` function returning `(2147483647 as i32) + (1 as i32)`.

## Observed

The program compiles on both consumers with no diagnostic; `return 2147483647 + 1;` in an `i32` function and `let a: i32 = 2147483647 + 1;` under `#[overflow_checked]` compile too, and `--report vcs` shows no obligation for them. No program reaches the proven-overflow arm of `provenViolationDiagnostics`.

## Expected

The proven-overflow arm emits its code for an operation the folder proves always overflows, or is removed and the documentation says no overflow is refused at compile time. docs/spec/arithmetic.md (ARITH-3's rationale) says constant folding may refuse such an operation and calls the code a partial guarantee, and KNOWN_HOLES C7 describes proven safety violations as a hard compile error; neither is true of overflow. No language rule is violated: the spec only permits the refusal, and the run-time trap is the guarantee.

## Cause

`overflowObligations` (Concrete/Report/ReportObligations.lean) sets a constant verdict only when `cEvalInt e` folds the operation, which needs every leaf to be an integer literal, and keeps an operation only when `exprIntTy` finds its type, which needs an identifier or a cast leaf; `cEvalInt` has no case for either. No expression satisfies both, so `closedVerdict` is never `some _`, and `provenViolationDiagnostics`'s overflow arm and `renderOverflow`'s constant statuses are unreachable.

## Structural cause

Class 13 ([STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)): a documented refusal has no reachable enforcing site, and no test exercised the arm, so the gap was invisible: the folder and the type lookup were written for disjoint shapes of expression.

## Structural owner

R-0499 (S8: every rule has an enforcing site), with R-0493 (S6 `IntArith`) supplying one constant evaluator over typed Core.

## Containment while open

`check_example_outputs.sh` (open-bug reproducers leg) pins the acceptance. Every overflow still traps at run time (ARITH-3), so no program computes a wrong value.
