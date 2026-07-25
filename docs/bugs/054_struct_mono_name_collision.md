# Bug 054: struct mono-name collisions — user types shadow generated specializations

**Status:** Half closed — the collision now FAILS CLOSED (**E0809**), the mangling
is still forgeable.
**Discovered:** 2026-07-18, middle-end audit (two reproduced variants:
compiled crash with Trace/BPT trap 5 vs interp `ok`; and compiled printing
nothing vs interp `ok`).

**Closed 2026-07-24 (via R-0001):** Mono refuses to emit a specialization whose
mangled TYPE name is already declared, and refuses two distinct instantiations
that mangle to one name — both reproduced variants above are now clean E0809
rejections instead of silent layout sharing. Gate:
`scripts/tests/check_mono_name_collision.sh` (`user_shadows_specialization`,
`two_instantiations_one_name`, plus a non-firing case so the check cannot
over-reject ordinary multi-argument generics). This was required by R-0001 rather
than optional: while E0808 refused every user generic enum, a forged ENUM
specialization name was unreachable; per-instantiation enum mono made it
reachable, so it had to be closed in the same change.

**Still open (R-0007):** the mangling itself remains forgeable — `base ++ "_" ++
suffixes` built from source identifiers, with no reserved separator users cannot
write. Failing closed means a legitimate program can be refused because it happens
to spell a generated name; an injective encoding plus semantic
`TypeId`/`FunctionId` separate from link/display symbols is what removes the
ambiguity. The FUNCTION-symbol half is also untouched here: fn specializations are
mangled by a different helper and are not covered by E0809.

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
