# Bug 230 — another top-level module's alias passes Resolve in a cast and crashes the backend

<!-- bug-ledger
summary: a top-level module casting to another top-level module's alias, or to its child's, without importing it passed Resolve (its known types were program-wide, and included its children's aliases) while Check kept the name, so the backend panicked (`EmitSSA.tyToLLVMTy: unknown named type`). Every module's type namespace now comes from one environment, `Resolve.moduleTypeEnvironment` plus its imports' closure (R-0492 slice 6): E0108. Found by the Codex review of bug 217's fix, 2026-10-24. Fixtures `error_230_*`, `regress_230_toplevel_type_environment.con`. Class 12.
fixtures: error_230_toplevel_foreign_alias_cast.con error_230_parent_names_child_alias.con regress_230_toplevel_type_environment.con
class: 12 — which type names a top-level module may write: program-wide in Resolve, per module in Check; fixed 2026-10-25 by one `moduleTypeEnvironment` for every module, R-0492 slice 6
silent: no
-->

**Status:** FIXED (2026-10-25, R-0492 slice 6)
**Found:** 2026-10-24, by the Codex review (round 4) of bug 217's fix: the same shape inside a
nested submodule was closed there; between top-level modules it remains.
**Severity:** a crash. The backend panics (`EmitSSA.tyToLLVMTy: unknown named type 'Nope'`).

Confirmed real by Codex review, 2026-10-25: docs/spec/modules.md MOD-1/MOD-2 exclude an unimported foreign alias; program-wide known types admitted it and caused a backend panic, independently of cast-policy OD11.

## Reproducer

`tests/programs/error_230_toplevel_foreign_alias_cast.con` (the former
`tests/regressions/bug230_toplevel_known_types_admit_foreign_alias/repro.con`): top-level modules `a`
(declaring `pub type Nope = i32`) and `b`, with `main` written last; `b` casts to `Nope` without
importing it. Observed on main `408880cb` and on `wip/bughunt-216-217`: the native build panics in
`EmitSSA.tyToLLVMTy`. Expected: E0108 (`b` neither declares nor imports `Nope`).

## Cause

Resolve checks a top-level module's written types against `ShallowResult.knownTypes`, the type
names of every module of the program (and of their submodules), while Check's environment for the
module is its own declarations and its imports. A name Resolve admits there and Check cannot
expand is kept as written in a cast target (bug 215's `resolvedOr`), CoreCheck reads it as opaque,
and the backend meets an undeclared named type. A `let` annotation with the same type is refused by
Check, so only the cast path crashes.

The parent shape is the same defect: a top-level module's known types also held its children's
structs, enums, aliases and traits (`buildGlobalScopeFromSummary`), which Check's environment for it
does not, so `(x as Al)` with `Al` a child's alias passed Resolve and panicked in the backend too
(`tests/programs/error_230_parent_names_child_alias.con`).

## Fix

R-0492 slice 6 ([DECLARATION_IDENTITY.md](../compiler/DECLARATION_IDENTITY.md)). One function,
`Resolve.moduleTypeEnvironment`, gives a module's own type environment as Check holds it (its own
declarations, its children's newtypes, the builtins), and every module, top-level or nested, gets
its type namespace and known type names from it plus its imports as import resolution transports
them (`importScope`, the import closure Check's `resolveImports` gives) and, for a nested
submodule, its siblings' types. `ShallowResult.moduleTypeEnvs` holds the top-level modules'; the
type namespace of their `Scope` is built from it alone (`Scope.ofSymbolsTyped`), while values still
fall back to the program-wide scope, which holds the other top-level modules' functions that Check
injects. Both reproducers are refused with E0108. The same change fixed the top-level half of bug
227.

Regression legs in `run_tests.sh`: `error_230_toplevel_foreign_alias_cast.con` and
`error_230_parent_names_child_alias.con` (`run_err` on E0108), and the positive control
`regress_230_toplevel_type_environment.con` (an imported alias, a type reached only through an
imported signature, an own struct and a child's newtype: `run_ok … 42` and `--interp` = 42).

## Structural cause

Class 12 ("one fact kept in two places"): which type names a module may write is computed by
Resolve (program-wide) and by Check (per module), differently. Bug 217's fix made the nested-module
case exact; the top-level case keeps the program-wide list because Check also injects other
top-level modules' functions, and narrowing it needs the per-module type environment Check uses.

## Structural owner

R-0492/R-0514 (one resolver: one environment per module, consumed by Check and Elab). Slice 6 makes
Resolve's copy one definition shared by every module; Check still builds its own, and the
remaining step is for Check and Elab to consume Resolve's environment instead of rebuilding it.
