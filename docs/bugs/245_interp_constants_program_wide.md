# Bug 245 — the interpreter refuses two modules' private constants of one spelling

<!-- bug-ledger
summary: `--interp` refused two top-level modules' private constants of one spelling ("defined by more than one module") that the binary keeps apart: the interpreter resolved a bare constant spelling program-wide, Lower within the using function's top-level tree. Both now take that scope from one definition, `CModule.treeConstants`. Found by the Codex review of bug 233's fix, 2026-10-25. Fixtures `regress_245_*`. Class 1.
fixtures: regress_245_toplevel_private_constants.con regress_245_nested_private_constants.con
class: 1 — a constant reference is its spelling, scoped program-wide by the interpreter and per tree by Lower; fixed 2026-10-25: both scope it by one definition, `CModule.treeConstants`
silent: no
area: Interp/Interp — the record's Fix: the interpreter keeps the scope per tree; it spells only Core.lean
ruling: Violates: MOD-1@1
-->

**Status:** FIXED (2026-10-25, branch `wip/ident-followups`)
**Found:** 2026-10-25, by the Codex review (round 2) of bug 233's fix (R-0492 step 4): a fixture
whose initializer calls another module's function read that module's constant.
**Severity:** loud. `--interp` refuses a program the native binary runs; no wrong value.

Confirmed real by Codex review, 2026-10-25: MOD-1/MOD-3 (docs/spec/modules.md): interpreter lookup conflated separate modules’ private constants, falsely rejecting valid module-local references.

## Reproducer

`tests/programs/regress_245_toplevel_private_constants.con` (the former
`tests/regressions/bug245_interp_constants_program_wide/repro.con`): top-level modules `a` and `b`
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

## Fix

The scope a bare constant spelling is looked up in is one definition, `CModule.treeConstants`
(`Concrete/Elab/Core.lean`): the constants of a top-level module and of every module nested in it.
Lower's `collectAllConstants` is that definition, and the interpreter keeps it per top-level
module (`Ctx.treeConsts`, by the index `Ctx.scope` already carries for call resolution, bug 118):
a use in a function of tree `i` is looked up in `treeConsts[i]` alone. Bug 132's refusal remains
for two constants of one spelling within a tree, in both backends. A context with no scope (a
report harness) keeps the program-wide table, and an imported constant's initializer is still
evaluated in its defining tree's constants (bug 233).

`--report model-diff` had the same program-wide lookup (Codex review): its sampled calls ran in a
context with no definition table, so every call ran with no scope. They now run in the
interpreter's program context (`Interp.mkCtx`, as `--interp` runs them), and the model binds the
constants of the subject's own tree.

A generic function's specialization, lowered in the tree that instantiates it, read that tree's
constants natively, so the scoping here left the interpreter right and the binary wrong for it: bug
251, fixed with this one.

Regression legs in `run_tests.sh`: `regress_245_toplevel_private_constants.con` (the reproducer)
and `regress_245_nested_private_constants.con` (three trees each with a private `K`, one with a
nested module reading its own constant), each `run_ok … 42` and `--interp` = 42, and the
reproducer's `--report model-diff` rows (`a.ga` and `b.gb` agree, 40 and 2).

## Structural cause

Class 1 ("declaration identity by spelled name"): a constant reference is its spelling, and the two
consumers scope that spelling differently.

## Structural owner

The disagreement is removed by giving the scope one owner, `CModule.treeConstants`, which both
backends read. The spelling itself remains: R-0492 / R-0514 give every constant reference its
`ConstId`, as an imported one has since bug 233's fix (`ConstId.coreName`), so that no consumer
scopes a spelling at all and bug 132's within-tree refusal becomes unnecessary.
