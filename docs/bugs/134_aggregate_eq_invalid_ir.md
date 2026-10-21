# Bug 134 — `==` on an enum or struct passes the checker and fails in `llvm-as`

**Status:** OPEN
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
out of the interpreter/native comparison). Reproduction is pinned by `check_example_outputs.sh`
(open-bug reproducers leg, compile-fails).

## Workaround

Compare with `match` on the variants, or a named function over the fields.
