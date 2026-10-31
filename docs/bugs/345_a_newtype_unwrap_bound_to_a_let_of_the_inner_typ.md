# Bug 345 — a newtype unwrap bound to a let of the inner type passes Check and the native build stops at the SSA verifier

**Status:** OPEN
**Found:** \1, by a hand probe of the builtins catalog of the spec's readability pass 2 (the `unwrap` row), native against `--interp`; confirmed by Codex and Grok
**Severity:** loud: a program Check accepts fails to build natively at the SSA verifier (E0715), an internal stage after Check, while `--interp` runs it

<!-- bug-ledger
summary: a newtype unwrap bound to a let of the inner type passes Check and the native build stops at the SSA verifier
coverage: reproducer tests/regressions/bug345_a_newtype_unwrap_bound_to_a_let_of_the_inner_typ/repro.con, pinned by check_example_outputs.sh (open-bug reproducers leg, compile-fails on E0715)
class: 12
silent: no
ruling: Violates: PASS-12@1
-->

## Reproducer

`tests/regressions/bug345_a_newtype_unwrap_bound_to_a_let_of_the_inner_typ/repro.con`: a newtype `Id` over `Int`, `let x: Int = unwrap(m);` and a comparison `x == 42`.

## Observed

The native build stops at the SSA verifier: `error[ssa-verify]: (E0715) main: block 'entry': binop %t1 has mismatched operand types: Concrete.Ty.named "Id" vs Concrete.Ty.int`. `--interp` prints 42. The same program with `m.0` in place of `unwrap(m)` exits 42 on both consumers, and `return unwrap(m);` alone exits 42 on both: the defect shows once the result is used as a value of the inner type.

## Expected

[PASS-12@1](../spec/pipeline.md#PASS-12): a program every rule accepts is compiled, and no stage after Check refuses it. No rule refuses `unwrap(n)` today; whether it stays is an open owner question (TYPE-24 lists the conversions of a newtype and `unwrap` is not among them). Under either answer the program is compiled or refused at Check with a diagnostic, never stopped by the SSA verifier.

## Cause

Check gives `unwrap(n)` the newtype's inner type (`checkExprCore`, Concrete/Check/Check.lean), but Elab (`Concrete/Elab/Elab.lean`, the `intrinsic == some .unwrap` intercept) erases the call to its argument's Core expression unchanged, so the result keeps Core type `Id`; the `let` binds it under the declared `Int`, and the SSA verifier then sees a binary operator over `Id` and `Int`.

## Structural cause

Class 12 ([STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)): the type of `unwrap(n)` is kept in two places, Check's result type and the type of the Core expression Elab produces, and nothing makes them agree; CoreCheck accepts the binding, so the disagreement first surfaces in the SSA verifier.

## Structural owner

R-0499 (one checker over Core, row 4): the type a construct has in Core is the one checked, so an erasure that changes the type is a Core-level conversion the checker sees, and the source checker's result type is not a second fact.

## Containment while open

`check_example_outputs.sh` (open-bug reproducers leg) pins the E0715 failure. The defect is loud; meanwhile `n.0` gives the inner value with its own type.
