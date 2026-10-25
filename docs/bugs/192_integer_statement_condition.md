# Bug 192 — an integer condition of an `if`, `while` or `for` statement, or a match guard, compiled

<!-- bug-ledger
summary: an integer condition of an `if`, `while` or `for` statement compiled: `checkStmt` discarded the condition's type and CoreCheck admitted integers as truthiness, so the binary ran it (non-zero is true) while `--interp` failed at run time. Silent. Found by the specification's TYPE-5 example (`docs/spec/types.md`, run on both consumers by `check_spec.sh`); refused with E0223; a match guard had the same hole (the `bool` hint was not checked); pinned by `error_192_int_{if,while,for}_condition.con` and `error_192_int_match_guard.con`
fixtures: error_192_int_if_condition.con error_192_int_while_condition.con error_192_int_for_condition.con error_192_int_match_guard.con
class: 13 — an `if` / `while` / `for` statement condition: Check discarded its type, CoreCheck admitted integers
silent: yes
exit: 0b
-->

**Status:** FIXED (branch `wip/spec-single-source`)
**Found:** 2026-10-23, by the language-specification examples (`docs/spec/types.md`, TYPE-5's
reject example `int-condition`, run by `scripts/tests/check_spec.sh`)
**Severity:** a program the language refuses was accepted. The binary ran it with C truthiness (a
non-zero integer is true), while `--interp` stopped at run time ("if condition is not a
boolean"): the two consumers disagreed, silently for the binary.

## Reproducer

```con pseudocode
fn main() -> Int {
    let n: Int = 1;
    if n {
        return 42;
    }
    return 0;
}
```

Expected: refused, E0223 (TYPE-5: the condition of an `if` or `while` has type `bool`). Observed on
`wip/spec-single-source` `2ff6bfbf` (train 4 base): the binary exits 42; `--interp` fails with
"if condition is not a boolean". `while n { .. }`, a `for` header condition and a match guard
(`3 if n => ..`) behaved the same. The
value form `let x = if n { 1 } else { 2 };` was already refused with E0223.

## Cause

- `Concrete/Check/Check.lean`, `checkStmt`: the `.ifElse`, `.while_` and `.forLoop` cases computed
  the condition's type and discarded it (`let _condTy ← checkExpr cond`). Only the expression form
  (`checkExprCore`, `.ifExpr`) compared it with `bool`. `checkGuard` passed `bool` as an inference
  hint to a match guard and discarded the result, so a guard of another type was not refused.
- `Concrete/Check/CoreCheck.lean`, `ccCheckStmt` and `ccCheckExpr`: the re-check admitted any
  integer type (`cond.ty != .bool && !isInteger cond.ty`), C-style truthiness the language never
  had, so the defense in depth let it through too. `ccCheckMatchArm` did not check a guard's type
  at all. The `.ifExpr` case also reported a non-`bool`
  condition with the `while` code (E0540).

## Fix

`checkStmt` refuses a non-`bool` condition of an `if`, `while` or `for` statement, and `checkGuard`
a non-`bool` match guard, with E0223 (`condNotBool`, the statement form of `ifCondNotBool`).
CoreCheck drops the integer allowance (E0541 for `if`, E0540 for `while`) and checks guards (E0541);
its `if` expression now reports E0541, not the `while` code. Regression fixtures:
`error_192_int_if_condition.con`, `error_192_int_while_condition.con`,
`error_192_int_for_condition.con` and `error_192_int_match_guard.con` (E0223, in `run_tests.sh`), and TYPE-5's reject example in
`docs/spec/types.md`, which `check_spec.sh` runs natively and with `--interp`.

## Structural cause

Class 13, documented rules with no enforcing site: E0223's own ledger row said "if condition not
Bool", but only the expression form had an enforcing site, and the second checker's copy of the
rule was a different rule. It is also an instance of class 7 (semantics implemented several
times): the interpreter implemented the documented rule and the backend implemented truthiness. It
was found because the specification's examples run on both consumers: every rule now has a reject
example with its code, stage and rule tag.

## Structural owner

R-0499 (S8: checker rules authoritative on Core, one judgment per rule). Until then, the
specification gate (`check_spec.sh` S4) is the containment: each refusal has a reject example on
both consumers, which is how this bug was found.
