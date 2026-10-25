# Bug 245 — the interpreter refuses two modules' private constants of one spelling

**Status:** OPEN
**Found:** 2026-10-25, by the Codex review (round 2) of bug 233's fix (R-0492 step 4): a fixture
whose initializer calls another module's function read that module's constant.
**Severity:** loud. `--interp` refuses a program the native binary runs; no wrong value.

## Reproducer

`tests/regressions/bug245_interp_constants_program_wide/repro.con`: top-level modules `a` and `b`
each declare a private `const K` and a function returning it; `main` adds the two. The binary
returns 42; `--interp` fails: `interp: module constant 'K' is defined by more than one module, and
Core cannot tell which one this use names (bug 132)`. Observed on main `593dc100` and on
`wip/r0492-steps34`.

## Cause

Core references a module's own constant by its bare spelling. Lower resolves it in the constants of
the using function's top-level module tree (`collectAllConstants`, per `lowerModule`); the
interpreter resolves it in every module's constants at once (`Ctx.consts`, `collectConsts
modules`) and refuses a spelling two modules define. Bug 132's refusal was written for the shared
case (two constants in one tree) and its interpreter half states it as program-wide.

## Structural cause

Class 1 ("declaration identity by spelled name"): a constant reference is its spelling, and the two
consumers scope that spelling differently.

## Structural owner

R-0492 / R-0514: a constant reference carries its `ConstId` for every constant, as an imported one
does since bug 233's fix (`ConstId.coreName`), so neither consumer scopes a spelling.
