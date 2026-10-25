# Bug 245 — the interpreter refuses two modules' private constants of one spelling

<!-- bug-ledger
summary: `--interp` refuses two top-level modules' private constants of one spelling ("defined by more than one module") that the binary keeps apart: the interpreter resolves a bare constant spelling program-wide, Lower within the using tree. Owned by R-0492/R-0514 (constant references by `ConstId`); reproducer pinned as an open-bug reproducer (Codex review of bug 233's fix, 2026-10-25). Class 1.
coverage: the interpreter resolves a module constant's bare spelling program-wide and refuses one two top-level modules define, while Lower resolves it in the using tree, so the binary runs what --interp refuses; owner R-0492/R-0514 constant references by identity; reproducer tests/regressions/bug245_interp_constants_program_wide/repro.con, found by the Codex review of bug 233's fix 2026-10-25; pinned as an open-bug reproducer (interp-fails)
class: 1 — a constant reference is its spelling, scoped program-wide by the interpreter and per tree by Lower
silent: no
-->

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
