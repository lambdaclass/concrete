# Bug 211 — generic arithmetic instantiated at a non-numeric type reached llvm-as

<!-- bug-ledger
summary: a generic `+` / `*` instantiated at a struct or enum reached the backend (`llvm-as` rejected the struct instance; `--interp` stopped at run time): CoreCheck defers a type parameter's operator to the instance, and only comparisons had a post-Mono re-check. `verifyOperatorTypes` checks every monomorphized arithmetic, wrapping, saturating, bitwise and shift operator (E0501, post-mono). Pinned by `error_211_generic_add_struct.con`, `error_211_generic_mul_enum.con`, `error_211_generic_rhs_struct.con`, `error_211_generic_mixed_width.con` and `regress_211_generic_add_numeric.con` (binary and --interp)
fixtures: error_211_generic_add_struct.con error_211_generic_mul_enum.con error_211_generic_rhs_struct.con error_211_generic_mixed_width.con regress_211_generic_add_numeric.con
class: 6 — an operator rule deferred to the instance had no instance check: arithmetic, unlike comparisons
silent: no
ruling: Violates: TYPE-6@1
-->

**Status:** FIXED (branch `wip/diffhunt-1023`)
**Found:** 2026-10-23, in the post-R-0492 differential hunt, while reducing bug 210 (a probe of the
same operator rule through a generic function).
**Severity:** loud. `add<T: Copy>(a: T, b: T) -> T { a + b }` instantiated at a struct failed in
`llvm-as` (`'%a' defined with type 'ptr' but expected 'i64'`), and
`--interp` stopped at run time with "unsupported binop on given value types".

Confirmed real by Codex review, 2026-10-25: docs/spec/types.md TYPE-6 also governs specialized bodies; deferred arithmetic checks had no post-Mono enforcement.

## Reproducer

`tests/programs/error_211_generic_add_struct.con`:

```con
struct Copy P { x: i32 }

fn add<T: Copy>(a: T, b: T) -> T { return a + b; }

fn main() -> Int {
    let p: P = P { x: 1 };
    let q: P = add::<P>(p, p);
    return q.x as Int;
}
```

Observed on main `dcc48dc1`: `llvm-as` rejected the IR. Expected: refused by TYPE-6@1 with a
diagnostic, as the same `+` written on `P` directly is (E0501, bug 210).

## Cause

CoreCheck runs before Mono and defers an operator whose operand type is a type parameter
(`isOpaqueOperandTy`). The comparison rule has a post-Mono re-check on every instance
(`verifyComparisonTypes`, bugs 134, 140); the arithmetic rule had none, so an instance at a
non-numeric type went to the backend unchecked.

## Fix

`verifyOperatorTypes` in `Verify.lean`, part of `verifyPostMono`: in every monomorphized function,
an arithmetic operator takes two operands of one numeric type (or, in trusted or `Unsafe` code, a
raw pointer and an integer), and a wrapping, saturating, bitwise or shift operator two of one integer
type. Both operands are checked and their agreement too: CoreCheck defers the agreement when either
side is a parameter (`a: Int` plus `b: T`, the Codex review's witness). A numeric literal operand
is exempt from the agreement: in a generic body it keeps the type Elab gave it, and std's
`digit_to_int<T>` (`(value as T) - 48`) is `Uint - Int` at `T = Uint`; the literal takes the other
operand's type when emitted, and `verifyLiteralRanges` checks that it fits (found by the full suite:
every project build importing std failed before the exemption). A violation
is E0501 at stage `post-mono` (TYPE-6's code list now says `E0501 (core-check, post-mono)`).
Regression legs in `run_tests.sh`: `error_211_generic_add_struct.con`,
`error_211_generic_mul_enum.con`, `error_211_generic_rhs_struct.con` and
`error_211_generic_mixed_width.con` (`run_err ... (E0501)`), and `regress_211_generic_add_numeric.con`
(42 natively and under `--interp`: numeric instances are admitted).

## Structural cause

Class 6 ("capability facts not carried across ... specialization", here a type rule): a rule that
the source checker defers to the instantiation is only sound when something checks the instance,
and each deferred rule had to add its own post-Mono re-check (comparisons did, arithmetic did not).
Secondary: class 13, TYPE-6@1 had no enforcing site for generic instances.

## Structural owner

R-0510 (S8a checked specialization: every instance is re-checked by the same Core checker, so a
deferred rule cannot lack its instance check) with R-0499 (S8).
