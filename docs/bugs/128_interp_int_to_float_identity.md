# Bug 128 — the interpreter models an integer-to-float cast as the identity

**Status:** FIXED 2026-10-21 (interpreter-coverage pass, branch `interp-coverage`)
**Found:** 2026-10-21, by the semantics probes (finding N2, `cast_misc.con`).
**Severity:** silent wrong value in the interpreter. The binary rounds an integer to the nearest
`f64` and back; `--interp` keeps the integer exact, so a round trip through `f64` differs between
the engines and the interpreter, the differential reference, is the wrong one.

## Reproducer

`tests/regressions/bug128_interp_int_to_float_identity/repro.con`:

```con
fn to_f(x: u64) -> f64 { return x as f64; }
fn back(x: f64) -> Int { return x as Int; }

fn main() -> Int {
    let f: f64 = to_f(9007199254740993);
    let b: Int = back(f);
    return b % 10;
}
```

2^53 + 1 = 9007199254740993 is not representable in `f64`; round-to-nearest-even gives 2^53.
On main (`5b63cd87`): the binary exits **2** (2^53, correct), and `--interp` prints **3** (2^53 + 1).
Expected: 2 from both.

The interpreter rejects float literals ("interp: float literals not yet supported"), so only
integer-origin float values reach it; this program is one such path. Whether the permanent
differential suites contain any integer-to-float cast was not inventoried.

## Cause

`evalCast` in `Concrete/Interp/Interp.lean` has no float values: its integer case returns
`.int (IntArith.wrapToType targetTy n) targetTy` for every target, and at a float target that is the
unchanged integer tagged `f64`. The later `f64 as Int` takes the same path, so the pair is the
identity (`wrapToType` returns its argument unchanged at a type with no integer width). The binary
emits `uitofp i64 ... to double` (IEEE rounding) and a range-checked `fptosi` (`__cc_f64_to_i64`).

## Fix

The interpreter has float values (`IVal.float`, binary64; an `f32` holds exactly a binary32
value). `evalCast` converts an integer to the nearest representable float, ties to even, computed
exactly on the integer (`intToFloatRounded`, 53 or 24 significant bits), as `sitofp`/`uitofp` do;
a float to an integer is the checked conversion the backend's `__cc_f*_to_*` helpers perform
(`lo <= f < hi` or the program aborts, then truncation toward zero); float to float widens exactly
or rounds. Float literals, `+ - * / %` (`%` is C `fmod`, computed exactly), negation and the
comparisons (ordered, except unordered `!=`) are modelled too, so `examples/snippets/floats.con`
runs. The reproducer prints 2, as the binary exits 2. Permanent legs: `run_tests.sh`'s
interp-vs-native loop over fixed reproducers and `tests/codegen/float_semantics.con` in
`check_codegen_differential.sh`. The first float probe found bug 139 (the binary folded `x != x`
to false for a NaN `x`).

## Structural cause

Semantics implemented several times ([structural class](STRUCTURAL_CLASSES.md) 7), the sibling of
bug 080 (a narrowing cast modelled as the identity in the proof model): the interpreter, the proof
model and the backend each carry their own cast rules, and the interpreter's rule for a float target
was never defined, only defaulted to the integer case. It is also absence read as a fact (class 11):
an unsupported target falls into the integer branch instead of failing.

## Structural owner

R-0493 (S6 `IntArith`, exit 2h: one authority for evaluation, folding and the model, casts included)
with R-0496 (S7 Core semantics); the per-width-pair cast rule is decided under exit 2k (R-0500,
ARITHMETIC_POLICY §8), and R-0491 (exit 2c) adds the permanent differential leg. As silent wrong
code (in the engine the differential gates use as the reference) it joins exit 0b under the defect
disposition rule: the interpreter either computes the IEEE conversion or refuses float targets
fail-closed.

## Containment while open

While open, `scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers", mode
`interp-prints:3`) required the recorded wrong value; the row was removed with the fix.
