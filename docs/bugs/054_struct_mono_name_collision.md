# Bug 054: struct mono-name collisions — user types shadow generated specializations

**Status:** Open
**Discovered:** 2026-07-18, middle-end audit (two reproduced variants:
compiled crash with Trace/BPT trap 5 vs interp `ok`; and compiled printing
nothing vs interp `ok`).

## Symptom

`monoStructName` (`Concrete/IR/Mono.lean:607-608`) is
`base ++ "_" ++ suffixes` with no bracketing, generated structs append after
user structs, and all layout lookups are first-match-by-name
(`Layout.lean:25-26`). So:

- a hand-written `struct Pair_Int_Bool` shadows the generated
  `Pair<Int, Bool>` — layouts computed for one are used for the other
  (crash / wrong field offsets);
- `Pair<Int, Bool>` collides with a user `Pair_Int<Bool>`.

Amplified by `Layout.fieldOffset` returning a past-end offset for a missing
field instead of erroring. The fn-name analogue (user fn `f_for_Int` vs
specialization `f_for_Int`) fails closed at llvm-as ("invalid
redefinition").

## Root cause

The mangling is not injective and no gate detects the collision: generated
and hand-written names share one namespace and first-match wins.

## Candidate fix

Make struct (and fn) mono names unforgeable/injective (reserved separator or
type-arg bracketing that users cannot write), AND add a collision diagnostic
when a generated name already exists (fail closed, like bug 028's reserved
identifiers). Regression: both repro shapes compile to identical interp ==
compiled output, and a hand-written colliding name gets a clean E-code.
