# Bug 015: Default `-O0` Distorted Real-Workload Performance

Status: fixed

## Symptom

Real workload benchmarks substantially under-represented Concrete performance because the default clang invocation used `-O0`.

This was especially misleading for string-heavy code where per-character helper calls stayed as real function calls instead of being inlined.

## Root Cause

The compiler driver emitted unoptimized clang builds by default for ordinary compiled output and test-compiled output.

That made serious benchmark interpretation much worse than the underlying language/runtime path deserved.

## Fix

- changed clang invocations to pass `-O2`
- applied the change to both ordinary compilation and test compilation paths

## Effect

The JSON benchmark changed materially:

- old default: ~145ms parse at `-O0`
- optimized: ~40ms parse at `-O2`

That turned the benchmark from “Concrete looks much slower than Python” into “Concrete is competitive on this workload.”

## Why This Counts As A Bug

This is not a semantic miscompile bug, but it was a real product defect:

- it distorted performance conclusions
- it hid the real effect of the current string and parser design
- it pushed Phase H toward the wrong interpretation
