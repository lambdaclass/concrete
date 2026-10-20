# Bug 109 — an `f32` literal that is not exactly representable is emitted as a decimal LLVM `float` constant

**Status:** OPEN
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** compile failure at LLVM validation.

## Reproducer

`tests/regressions/bug109_f32_literal_invalid_ir/repro.con`:

```concrete
// Bug 109: an f32 literal that is not exactly representable (0.1) is emitted
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

examples/snippets/floats.con, manifest `known-bug 109`.

## Structural cause

Floating-point literals are rendered to LLVM text with a decimal formatter that assumes every constant is exactly representable; the literal's value is never converted to the target width before it is printed. The emitter decides a numeric encoding locally, unchecked against the type (classes 7 and 8 in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)).

## Structural owner

R-0502 (S11 one `Layout`/scalar encoding for the backend) with R-0504 (S12 validators); f32 is outside ProvableV1, so the proposed interim owner is the float-cast gate (`check_float_cast.sh`) extended to f32 literals.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
