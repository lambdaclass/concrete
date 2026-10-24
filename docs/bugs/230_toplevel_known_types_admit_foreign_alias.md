# Bug 230 — another top-level module's alias passes Resolve in a cast and crashes the backend

**Status:** OPEN
**Found:** 2026-10-24, by the Codex review (round 4) of bug 217's fix: the same shape inside a
nested submodule was closed there; between top-level modules it remains.
**Severity:** a crash. The backend panics (`EmitSSA.tyToLLVMTy: unknown named type 'Nope'`).

## Reproducer

`tests/regressions/bug230_toplevel_known_types_admit_foreign_alias/repro.con`: top-level modules `a`
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

## Structural cause

Class 12 ("one fact kept in two places"): which type names a module may write is computed by
Resolve (program-wide) and by Check (per module), differently. Bug 217's fix made the nested-module
case exact; the top-level case keeps the program-wide list because Check also injects other
top-level modules' functions, and narrowing it needs the per-module type environment Check uses.

## Structural owner

R-0492/R-0514 (one resolver: one environment per module, consumed by Check and Elab).
