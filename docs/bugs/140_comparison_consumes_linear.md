# Bug 140 — a comparison operator consumed its linear operands, and `==` on references compared addresses

**Status:** FIXED 2026-10-21 (branch `safety-eval2`)
**Found:** 2026-10-21, by the second semantics evaluation (EVALUATION2 D10, D11; probes `e04`,
`e05`, `e07b`, `e07c`, `e12`, `e18`), not filed then.
**Severity:** silent leak in safe code (D10); an unspecified, engine-dependent meaning for `==` on
references (D11). Fixed together with bug 134 (the loud half of the same missing rule).

## Reproducers

`tests/regressions/bug140_comparison_consumes_linear/repro.con`:

```con
fn b2i(b: bool) -> Int { if b { return 1; } return 0; }

fn main() with(Alloc) -> Int {
    let a: Heap<Int> = alloc(1);
    let b: Heap<Int> = alloc(1);
    return b2i(a == b);
}
```

On main (`5a064e77`): accepted; the binary exits 0. The comparison consumed both shells (a later
`free(a)` is E0205), compared the two cell addresses, and neither cell was ever freed. The same
consumption applied to every linear operand: `a == b` on two `String`s consumed both (and then
failed in `llvm-as`), on a linear struct likewise.

`tests/regressions/bug140_comparison_consumes_linear/repro_ref.con` (`eqr(a: &Int, b: &Int) -> bool
{ return a == b; }`): the binary exits 1, so `eqr(&x, &y)` with `x == y == 5` is false and
`eqr(&x, &x)` true (an address comparison); `--interp` fails with `unsupported binop`.

Fixed: both are refused at the checker, under the binary and `--interp` alike:

```
error[check]: (E0229) comparison '==' is not defined on 'Heap<i64>': comparison operators are defined on integers, floats, `bool` and `char` (and raw pointers in trusted or `Unsafe` code)
error[check]: (E0229) comparison '==' is not defined on '&i64': a reference has no built-in comparison (neither by address nor by content)
```

## Cause

`Concrete/Check/Check.lean` returned `bool` for every comparison ("CoreCheck validates"), and
CoreCheck checked only that the operand types agree. The operands were checked in value position,
where reading a non-Copy binding is its move, so a comparison was a consuming operation on any
linear type; the backend then compared whatever scalar it had (the heap pointer, the reference).

## Fix

The comparison rule (user decision (b), 2026-10-21; normative text in
[ARITHMETIC_POLICY §8a](../language/ARITHMETIC_POLICY.md#8a-comparison-operators)):

- `==`, `!=`, `<`, `<=`, `>`, `>=` are defined on integers, floats, `bool` and `char`, and on raw
  pointers in trusted code or a function declaring `with(Unsafe)`. Every other operand type is
  E0229 (`comparisonNotDefined`) at `Check.lean`'s `.binOp` case: structs, enums (fieldless `Copy`
  ones included), arrays, `String`, `Heap<T>`, fn pointers and references.
- A type parameter compares only when it is `Copy`-bounded, so no instantiation is consumed; each
  instantiation is held to the scalar rule after monomorphization (`verifyComparisonTypes` in
  `Concrete/Check/Verify.lean`, E0229, pass `post-mono`). A raw-pointer comparison
  reached through a `Copy`-bounded generic (`eq<T: Copy>` at `*const Int`) was accepted from safe
  code, because monomorphized instances did not carry their definition's `trusted` flag; that
  known limit is [bug 185](185_generic_rawptr_compare_loses_authority.md), fixed by carrying the
  authority into each instance (Codex review).
- Every comparable type is `Copy`, so a comparison never consumes an operand.

Fixtures (`run_tests.sh`): `error_140_heap_eq.con`, `error_140_linear_eq.con`,
`error_140_ref_eq.con`, `error_140_string_eq.con`, `error_140_generic_unbounded_eq.con`,
`error_140_generic_copy_at_struct.con` (the post-mono leg), `error_140_rawptr_eq_safe.con`,
and
bug 134's `error_134_enum_eq.con` / `error_134_struct_eq.con`; the positive control
`regress_140_scalar_comparisons.con` (integers, `bool`, `char`, `f64`, `pick<T: Copy>` at `Int`, a
raw-pointer comparison in a trusted function: 175). An interpreter leg asserts that `--interp`
refuses both reproducers with E0229 as the binary does.

The rule is decided before the operands are checked when an operand is a bound name, so `x == x`
at an unbounded `T` reports E0229 rather than a fictitious move (E0205)
(`error_140_generic_self_compare.con`; Codex review).

Migration: 2 sites, both in `examples/` (`hexdump` and `png_chunks` discarded a Copy `IoError` with
`discard(error == error)`; now `discard(error)`), 0 in `std/` and `tests/`. The generic comparisons in `std.math`
(`max`, `min`, `clamp`) and `std.test` (`assert_eq`, `assert_ne`, `assert_lt`, ...) are all
`T: Copy` and keep compiling; every instantiation in the corpus is a scalar.

## Structural cause

A documented-rule gap with no enforcing site ([structural class](STRUCTURAL_CLASSES.md) 13): no rule
said which types have a built-in comparison, or that a comparison does not consume, so the checker
refused nothing and value-position checking made it a move. Semantics implemented several times
(class 7): the backend compared addresses, the interpreter refused, nothing specified either.

## Structural owner

R-0499 (S8: every rule has an enforcing site; the Core checker's operand rules), with the
normative text under R-0500 (exit 2j/2k: comparison semantics in VALUE_MODEL and
ARITHMETIC_POLICY). Containment of the silent leak: ROADMAP exit 0b. The reference-identity choice
(refuse rather than specify) keeps Place MIR (R-0501) free of a reference-identity obligation.
