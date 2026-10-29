# Bug 134 — `==` on an enum or struct passes the checker and fails in `llvm-as`

<!-- bug-ledger
summary: `==` on an enum or struct passed the checker and failed in `llvm-as` (`icmp eq i64` on an aggregate) while `--interp` refused it; now E0229 at the checker (comparisons are defined on integers, floats, `bool`, `char`, and raw pointers in trusted/`Unsafe` code). `error_134_enum_eq.con`, `error_134_struct_eq.con`. ((`safety-eval2`, with 140))
fixtures: error_134_enum_eq.con error_134_struct_eq.con
class: 7
class: 8
class: 13 — no type had a defined comparison
silent: no
area: Check — the record's Fix: refused at the checker (E0229)
ruling: Violates: TYPE-6@1
-->

**Status:** FIXED 2026-10-21 (branch `safety-eval2`, with [bug 140](140_comparison_consumes_linear.md))
**Found:** 2026-10-21, by the std ownership-on-failure work (a std test compared an error enum with
`==`); the semantics evaluation (EVALUATION2 D8) had found the same class across struct, enum,
array, fn-pointer, `String` and aggregate-instantiated generic comparisons, without filing it.
**Severity:** loud failure on an accepted program: the checker accepts the comparison, LLVM
validation rejects the emitted IR, and `--interp` refuses it at run time. No silent wrong code was
observed.

## Reproducers

`tests/regressions/bug134_aggregate_eq_invalid_ir/repro.con` (a fieldless `Copy` enum):

```con
enum Copy E { A, B }

fn main() -> Int {
    let e: E = E::B;
    let same: bool = e == E::B;
    if same {
        return 0;
    }
    return 1;
}
```

On main (`e9b0b0db`):

```
llvm-as: .../repro.con.ll:847:21: error: '%t2' defined with type '%enum.E = type { i32, [0 x i8] }' but expected 'i64'
  %t6 = icmp eq i64 %t2, %t5
```

`tests/regressions/bug134_aggregate_eq_invalid_ir/repro_struct.con` (a `Copy` struct, `a == b`):

```
llvm-as: .../repro_struct.con.ll:839:21: error: '%a' defined with type 'ptr' but expected 'i64'
  %t0 = icmp eq i64 %a, %b
```

Both fail under `--interp` with `interp: unsupported binop on given value types`. Expected: a
checker diagnostic, since no rule gives `==` a meaning on these types.

## Cause

The checker admits `==`/`!=` on operands of any equal type, and the backend lowers every equality
to a scalar `icmp` at a default width, so an aggregate operand (an enum held as its union struct, a
struct passed by pointer) produces ill-typed IR. The interpreter has no case for the comparison.

## Fix

The comparison rule (user decision (b), 2026-10-21; [ARITHMETIC_POLICY
§8a](../language/ARITHMETIC_POLICY.md#8a-comparison-operators)): comparison operators are defined on
integers, floats, `bool` and `char`, and on raw pointers in trusted or `Unsafe` code. Both
reproducers are now refused at the checker, natively and under `--interp`:

```
error[check]: (E0229) comparison '==' is not defined on 'E': comparison operators are defined on integers, floats, `bool` and `char` (and raw pointers in trusted or `Unsafe` code)
```

A `Copy`-bounded generic comparison is accepted at its definition and re-checked per instantiation
after monomorphization (`verifyComparisonTypes`, E0229), so `same<T: Copy>` at a struct is also a
diagnostic rather than an `llvm-as` failure. Rejection fixtures `error_134_enum_eq.con` and
`error_134_struct_eq.con` (`run_tests.sh`); the open-bug reproducer rows in
`check_example_outputs.sh` are dropped. Migration: 0 sites. Bug 140 records the silent half
(linear operands consumed, references compared by address).

## Structural cause

A documented-rule gap with no enforcing site ([structural class](STRUCTURAL_CLASSES.md) 13): no rule
says which types have a built-in comparison, so the checker refuses nothing. Semantics implemented
several times (class 7): checker, backend and interpreter each decide what `==` means, and none
agrees. The IR is first checked by `llvm-as`, not by a validator of Concrete's own (class 8).

## Structural owner

The rule (EVALUATION2 D8): comparison operators are defined on integers, floats, `bool`, `char` and,
in trusted or `Unsafe` code, raw pointers; a generic comparison needs a bound that provides it. Its
normative home is VALUE_MODEL under R-0500 (exit 2j/2k), the enforcing check is R-0499 (S8), and
R-0504 (S12) validates the IR. The repair belongs to exit 2c (loud failures that keep valid programs
out of the interpreter/native comparison); it landed with bug 140's containment.

## Workaround (historical)

Compare with `match` on the variants, or a named function over the fields (still the way to
compare aggregates).
