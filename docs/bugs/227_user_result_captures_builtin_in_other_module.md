# Bug 227 — a user `enum Result` in one module captures the builtin `Result` in another

**Status:** OPEN (the top-level half fixed 2026-10-25 with bug 230; the nested half open)
**Found:** 2026-10-24, by the Codex review of bug 217's fix (its witness of an enclosing
`enum Result` hiding the builtin inside a nested submodule), widened by probes.
**Severity:** a valid program is refused, at a stage that depends on where the modules sit. No
wrong code: every failure is a refusal or a lowering error.

## Reproducer

`tests/regressions/bug227_user_result_captures_builtin_in_other_module/repro.con`: `outer`
declares `enum Copy Result { A }`; its submodule `inner` takes and builds a builtin
`Result<i32, i32>`. Observed on main `408880cb` and on `wip/bughunt-216-217`: `--interp` returns 42;
the native build fails in Lower, `(E0602) Lower.variantIndex: variant 'Ok' not found in enum
'Result'`. Expected: 42 natively.

`repro_toplevel.con`, now `tests/programs/regress_227_toplevel_builtin_result.con`: the same with
`a` and `b` top-level modules (`main` written last). Observed: Resolve refused `b`'s
`Result<i32, i32>`, `(E0113) 'Result' takes 0 type arguments, but 2 were given`. Expected: 42.
Fixed by bug 230's fix (R-0492 slice 6: a top-level module's type namespace is its own environment
and its imports, not the program-wide scope); `run_tests.sh` runs it natively and `--interp` (42).

The bug 217 witness `tests/programs/regress_217_enclosing_type_not_in_scope.con` is the nested
shape: `run_tests.sh` checks it through `--emit-core` and `--interp` only, and
`check_enum_union_layout.sh` pins it in `OPEN_BUG_REJECTED` (its native emission is refused), so the
fix must remove that entry and the layout probe then covers it.

## Cause

Two lookups of a type by its spelled name in a program-wide table, first match wins:

- `Lower.variantIndex` searches `LowerState.enumDefs`, every module's enums, by name, so `Result::Ok` in `inner` meets `outer`'s `Result`.
- Resolve gives a top-level module its own declarations and imports (`ShallowResult.moduleScopes`,
  bug 096) and then `globalScope`, every module's declarations in one first-wins map, so a name the
  module does not declare is answered by whichever module registered it first.

Check and Elab select per module (`builtinEnums` lets only the module's own or imported enums shadow
the builtin): in the nested case they accept `inner` and the interpreter, which runs Elab's Core,
returns 42, so the passes disagree.

## Structural cause

Class 1 ("declaration identity by spelled name, not a resolved `DefId`"): a type reference is
carried as its spelling and looked up again in each pass, in tables with different scopes.

## Structural owner

R-0492/R-0514 (one resolver: each type reference resolved once to its declaration, consumed by
Check, Elab and Lower).
