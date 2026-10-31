# Bug 348 — float_to_string runs natively and --interp stops with undefined function float_to_string

**Status:** OPEN
**Found:** \1, by a hand probe of the builtins catalog of the spec's readability pass 2 (the `float_to_string` row), native against `--interp`; confirmed by Codex and Grok
**Severity:** loud: the native binary runs and `--interp` stops with `undefined function 'float_to_string'`, so the two consumers disagree on an accepted program

<!-- bug-ledger
summary: float_to_string runs natively and --interp stops with undefined function float_to_string
coverage: reproducer tests/regressions/bug348_float_to_string_runs_natively_and_interp_stops_w/repro.con, pinned by check_example_outputs.sh (open-bug reproducers leg, interp-fails on the undefined function)
class: 7
silent: no
ruling: Violates: PASS-13@1
-->

## Reproducer

`tests/regressions/bug348_float_to_string_runs_natively_and_interp_stops_w/repro.con`: `float_to_string(1.5)`, its length compared with 3.

## Observed

The native binary exits 42 (`float_to_string(1.5)` is `"1.5"`). `--interp` stops with `interp: undefined function 'float_to_string'` and status 1.

## Expected

[PASS-13@1](../spec/pipeline.md#PASS-13): the interpreter and every compiled profile give an accepted program the same result, output and traps.

## Cause

The backend defines `float_to_string` (Concrete/Backend/EmitBuiltins.lean); the interpreter (Concrete/Interp/Interp.lean) has no case for it, so the call falls through to the undefined-function error.

## Structural cause

Class 7 ([STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)): a builtin's semantics are implemented separately in the backend and in the interpreter, and nothing checks that every builtin Check accepts has both.

## Structural owner

R-0496 (S7 Core semantics): builtins get one semantics on Core that the interpreter executes and the backend is checked against, so a builtin missing from either is visible.

## Containment while open

`check_example_outputs.sh` (open-bug reproducers leg) pins the interpreter's failure. The defect is loud; the native result is the correct one.
