# Bug 254 — the interpreter refuses a newtype's construction or unwrap over a struct or enum

**Status:** FIXED (2026-10-25, branch `wip/ident-followups`)
**Found:** 2026-10-25, fixing bug 242: a bug 096 fixture that became a valid program unwrapped an
imported newtype over a struct, which `--interp` refused.
**Severity:** loud. `--interp` refuses a program the binary runs; no wrong value.

## Reproducer

`tests/programs/regress_254_newtype_over_struct.con`: `struct Copy P { pad: i64, x: i64 }`,
`newtype W = P;` and `main` returning `W(P { pad: 99, x: 7 }).0.x` through a local. The binary
returns 7; `--interp` fails: `interp: unsupported cast`. The same over an enum
(`regress_254_newtype_over_enum.con`). Observed on `wip/r0492-steps34` (main `abc6318a` alike).

## Cause

Core writes a newtype's construction `N(v)` and its unwrap `n.0` as a cast between the newtype and
its inner type. The interpreter's cast (`evalCast`) converts scalars and pointers; a struct or enum
value reached its last arm, "unsupported cast". A newtype over a scalar worked because the scalar
arms accept any target.

## Fix

Before the conversions, a cast of a struct or enum value between two types that normalize to one
(`normTy`, which resolves newtypes) is the value itself, as the backend emits it. Regression legs
in `run_tests.sh`: both fixtures, native and `--interp`.

## Structural cause

Class 7 ("semantics implemented several times"): the newtype's representation identity is stated
by Layout for the backend and re-derived per value kind by the interpreter's cast.

## Structural owner

R-0514's shared semantics row: one statement of which casts are representation identities, read by
both consumers.
