# Bug 108 — a `bool` cast to a wider integer is emitted as `trunc i1`

<!-- bug-ledger
summary: `bool as <int>` was emitted as `trunc i1` (LLVM rejects it); `bool` is now the 1-bit unsigned width (zext), and `<int> as bool`, which neither backend ever gave a meaning, is refused (E0553); pinned by `regress_108_bool_cast_widen.con` and `check_cast_matrix.sh`
fixtures: regress_108_bool_cast_widen.con error_108_int_as_bool.con
class: 7
class: 8
class: 12
class: 13 — `<int> as bool` admitted with no meaning
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `backend-108-116`, commit 70025ca9)
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** compile failure at LLVM validation (a valid program is rejected after checking passed).

## Reproducer

`tests/regressions/bug108_bool_cast_trunc/repro.con`:

```concrete
// Bug 108: `bool as i32` is emitted as `trunc i1 to i32` (invalid IR).
// Expected: exit 1 (interp prints 1). Actual: LLVM IR validation fails.
fn main() -> i32 {
    let b: bool = true;
    return b as i32;
}
```

## Observed

`concrete repro.con -o out` fails: `invalid cast opcode for cast from 'i1' to 'i32'` (`%t = trunc i1 1 to i32`). The interpreter returns 1.

## Expected

A widening cast from `bool` must be a `zext`; the backend picks `trunc` for it.

## Where it showed up

examples/snippets/generic_enum_impl.con (`x.is_some() as i32`), manifest `known-bug 108`.

## Cause

`Concrete/Backend/EmitSSA.lean`, the integer-cast branch of `.cast` emission: the bit-width table
listed every integer type but `bool`, so `bool` took the 64-bit default. `bool as i32` was then
64 > 32, a `trunc`; `bool as Int` a `bitcast` of equal "widths". The reverse direction was broken
the same way: `x as bool` for an `i32` was a `zext/sext i32 to i1` (llvm-as rejects it), and the
interpreter produced an integer tagged `bool` that the next `if` refused ("if condition is not a
boolean"). `<int> as bool` never had a meaning in either backend.

## Fix

The width table gives `bool` 1 bit (and `bool` is unsigned), so `bool as <int>` is a `zext`
everywhere; CoreCheck no longer admits `<int> as bool` (E0553), since no implementation ever gave it
a value; ARITHMETIC_POLICY §8.1 (2026-10-21) since decided that `as` never accepts it, and bug 130
is the same refusal for an `Int` source. Permanent legs: `tests/programs/regress_108_bool_cast_widen.con`
(run_ok 42: both values, all eight widths, parameter and constant operands) and
`tests/programs/error_108_int_as_bool.con` (run_err E0553) in `run_tests.sh`, and
`run_src bool true false` in `check_cast_matrix.sh` (interpreter and binary agree at every width).
The reproducer exits 1 natively and under `--interp`; examples/snippets/generic_enum_impl.con is
promoted to `compile` with a `@run` rule.

## Structural cause

The cast opcode is chosen in the backend from the source and target widths, with no single cast table shared with the interpreter and the proof model; `bool` (i1) is a width the backend's table does not treat as an unsigned widening source. Casts are implemented several times and disagree, the class of bugs 080 and 102 ([structural class](STRUCTURAL_CLASSES.md) 7), and the emitted IR is first checked by llvm-as rather than by a validator of Concrete's own (class 8). The backend's width table is itself a second copy of `IntArith.intBitWidth` (class 12), and the reverse direction (`<int> as bool`) was admitted by CoreCheck with no meaning in any implementation, a rule with no enforcing site (class 13).

## Structural owner

R-0493 (S6 `IntArith`: one definition of every integer operation and cast, `bool` included) with R-0504 (S12 validators over the lowered program); the interim row `run_src bool true false` in `check_cast_matrix.sh` landed with the fix.

