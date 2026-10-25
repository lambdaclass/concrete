# Bug 227 — a user `enum Result` in one module captures the builtin `Result` in another

**Status:** FIXED (2026-10-25, R-0492 slice 6: the top-level half with bug 230, the nested half by Core spelling)
**Found:** 2026-10-24, by the Codex review of bug 217's fix (its witness of an enclosing
`enum Result` hiding the builtin inside a nested submodule), widened by probes.
**Severity:** a valid program is refused, at a stage that depends on where the modules sit. No
wrong code: every failure is a refusal or a lowering error.

## Reproducer

`tests/programs/regress_227_nested_builtin_result.con` (the former
`tests/regressions/bug227_user_result_captures_builtin_in_other_module/repro.con`): `outer`
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

## Fix

R-0492 slice 6 ([DECLARATION_IDENTITY.md](../compiler/DECLARATION_IDENTITY.md)), two steps.

- Top level (with bug 230): a top-level module's type namespace is its own environment and its
  imports (`Resolve.moduleTypeEnvironment`), not the program-wide first-wins scope, so `b`'s
  `Result` is the builtin.
- Nested: Core names a type by the declaration it resolved to (`TypeSpelling`, bug 096), qualified
  by its module when the spelling is ambiguous, but the builtin enums were outside the rule
  (`ambiguousTypeNames` skips compiler-known names), so the user's `Result` and the builtin were
  one Core name. The builtin `Result` is a declaration too (`TypeId.builtin`): a user type spelled
  `Result` is ambiguous with it unless every module of the program declares its own
  (`Elab.shadowsVisibleBuiltinEnum`), and is then spelled by its module (`outer_Result`, its
  methods `outer_Result_get`); the builtin keeps its name. Elab's refusal of a method spelling it
  cannot attribute (E0299) no longer fires when every candidate type's module is known and none
  declares the method: a std impl of the builtin (`Result_is_ok`) beside a user `enum Result` in a
  project keeps its name (found by `regress_202_project_let_else`). A user `Option` is not
  respelled: the builtin is never withheld, so the spelling never means the user's.

Regression legs in `run_tests.sh`: `regress_227_nested_builtin_result.con`,
`regress_227_toplevel_builtin_result.con` and `regress_227_user_result_methods_beside_builtin.con`
(a user `Result` with a method used in its module beside the builtin in a nested and another
top-level module), each `run_ok … 42` and `--interp` = 42. Bug 217's witness
`regress_217_enclosing_type_not_in_scope.con` now builds natively: its `run_tests.sh` leg runs the
binary, and `check_enum_union_layout.sh` no longer lists it in `OPEN_BUG_REJECTED`.

## Structural cause

Class 1 ("declaration identity by spelled name, not a resolved `DefId`"): a type reference is
carried as its spelling and looked up again in each pass, in tables with different scopes.

## Structural owner

R-0492/R-0514 (one resolver: each type reference resolved once to its declaration, consumed by
Check, Elab and Lower). Slice 6 closes this bug's two lookups; Core still carries types by spelling
(a module-qualified spelling where it would be ambiguous), and Lower's lookup becomes by identity
when Core types carry `TypeId` (later R-0492 steps).
