# Bug 108 — an `f32` literal that is not exactly representable is emitted as a decimal LLVM `float` constant

**Status:** OPEN
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** compile failure at LLVM validation.

## Reproducer

`tests/regressions/bug108_f32_literal_invalid_ir/repro.con`:

```concrete
// Bug 108: an f32 literal that is not exactly representable (0.1) is emitted
// as a decimal LLVM `float` constant, which llvm-as rejects.
// Expected: exit 1. Actual: "floating point constant invalid for type".
fn main() -> i32 {
    let x: f32 = 0.1;
    if x > 0.0 { return 1; }
    return 0;
}
```

## Observed

`let x: f32 = 0.1;` emits `fcmp ogt float 0.100000, ...`; llvm-as rejects it: `floating point constant invalid for type`. LLVM accepts a decimal `float` constant only when it is exactly representable; otherwise the constant must be written as the hex form of the double that rounds to the f32 value.

## Expected

Emit `f32` constants in LLVM hex form (the f32 value widened to double). `f64` literals are unaffected.

## Where it showed up

examples/snippets/floats.con, manifest `known-bug 108`.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
