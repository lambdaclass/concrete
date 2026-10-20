# Bug 108 — a `bool` cast to a wider integer is emitted as `trunc i1`

**Status:** OPEN
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

## Structural cause

The cast opcode is chosen in the backend from the source and target widths, with no single cast table shared with the interpreter and the proof model; `bool` (i1) is a width the backend's table does not treat as an unsigned widening source. Casts are implemented several times and disagree, the class of bugs 080 and 102 ([structural class](STRUCTURAL_CLASSES.md) 7), and the emitted IR is first checked by llvm-as rather than by a validator of Concrete's own (class 8).

## Structural owner

R-0493 (S6 `IntArith`: one definition of every integer operation and cast, `bool` included) with R-0504 (S12 validators over the lowered program); proposed until then: a `bool as <int>` row in `check_cast_matrix.sh`.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
