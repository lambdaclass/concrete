# Bug 217 — Resolve does not walk the bodies of nested submodules

<!-- bug-ledger
summary: Resolve walked only the modules of the top-level list, so a nested submodule (two modules deep, a module written after `main`, and every `mod x;` file of a project, std included) produced no Resolve diagnostics: a cast to an unknown type there reached the backend (panic; `--interp` printed 7) and undeclared names or unknown annotation types met Check's codes. Resolve now walks the module tree, each submodule resolved against the environment Check gives it (its imports by Check's rule, its own declarations, its siblings' types), nothing of an enclosing module. Fixtures `error_217_*`, `regress_217_nested_submodule_scope.con` and a project leg. Codex review of bug 215, 2026-10-23. Class 11.
fixtures: error_217_nested_cast_unknown_type.con error_217_nested_undeclared_name.con error_217_nested_annotation_unknown_type.con error_217_module_after_main_cast.con error_217_nested_enclosing_alias_cast.con regress_217_nested_submodule_scope.con regress_217_sibling_type_precedence.con regress_217_enclosing_type_not_in_scope.con
class: 11 — a nested submodule Resolve never visits yields no diagnostics
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/bughunt-216-217`)
**Found:** 2026-10-23, by the Codex review of bug 215 (a constant in `mod Outer { mod Inner { .. } }`
escaped the new check), confirmed by probes in the post-R-0492 differential hunt.
**Severity:** a crash, and diagnostics that depend on nesting depth. Two modules deep, a cast to an
unknown type was not refused and the backend panicked (`EmitSSA.tyToLLVMTy: unknown named type
'Nope'`) while `--interp` printed 7. Undeclared names and unknown annotation types there were caught
later by Check with other codes (E0201 instead of E0100, E0220 instead of E0108). The fix found the
gap wider than recorded: a module written after `main` (`fn main ..` then `mod outer { .. }`) is a
submodule of the main module, so one level deep was enough, and every `mod x;` file of a project is
a submodule of its main module, so Resolve checked only `src/main.con` (and std's empty `lib.con`).

Confirmed real by Codex review, 2026-10-25: docs/spec/modules.md MOD-1 requires unknown types to be refused; skipped nested bodies let unknown cast targets reach execution or panic. Diagnostic-code changes alone are not the core defect.

## Reproducer

`tests/programs/error_217_nested_cast_unknown_type.con` (moved from
`tests/regressions/bug217_resolve_skips_nested_submodules/repro.con`):

```con
mod outer {
    mod inner {
        pub fn h() -> Int {
            let a: i32 = 7;
            return (a as Nope) as Int;
        }
    }
    pub fn g() -> Int { return inner::h(); }
}

fn main() -> Int {
    return outer::g();
}
```

Observed on `wip/diffhunt-1023` (with bug 215's fix) and main `408880cb`: the backend panics;
`--interp` prints 7. With `inner` one level up, before `main`, the same cast is E0108. Expected:
E0108 at any depth.

## Cause

`Resolve.resolveBodiesPartial` calls `resolveModule` once per module of the list it is given, and
`resolveModule` walks that module's functions, impl methods, trait-impl methods and constants but
not `m.submodules`. Only the extern-effect and capability-parameter checks descend (bug 113).
Resolve has no scope for a nested submodule (`ShallowResult.moduleScopes` is keyed by the modules
of the list), which is why the walk stops: descending needs each submodule's own symbols, imports
and sibling-module paths.

## Fix

`Resolve.resolveModuleTreeErrors` walks the module tree: each top-level module as before, then every
submodule recursively, its diagnostics stamped with the submodule's source file (Check's
`checkSubmodules` does the same). The per-module rules that already descended on their own
(`externEffectsCheck`, bug 113; `capParamNamesCheck`, bug 188) now cover one module each, so every
rule of `resolveModule` runs once per module of the tree.

A submodule's symbol scope is the environment Check's `checkSubmodules` / `checkModule` give it, in
the same precedence:

1. its imports, resolved by the rule Check and Elab apply to a submodule (`resolveImports`, through
   `submoduleImportScope`), not by Resolve's top-level `summaryImports`, which is stricter about
   privacy (`std.string` imports the non-`pub` externs of `std.libc`, which `summaryImports` refuses
   with E0111 and `resolveImports` admits: bug 226). When they do not resolve, Resolve leaves the
   submodule to Check, which refuses it (E0284/E0285) and does not descend past it either;
2. its own declarations, with its children's functions and foreign bindings by qualified name
   (`leaf_two`) and its children's newtypes, but not their other types or constants;
3. the structs, enums and public methods of the modules declared beside it (Check and Elab inject
   these).

Nothing of an enclosing module is in it, and nothing of the program-wide first-wins `globalScope`.
Each of those was tried and refused accepted programs: rooted at `globalScope` (the first version),
`regress_202_project_let_else`'s non-generic `enum Result` answered `Result<T, E>` inside
`std.option` (E0113, bug 096's shape one level down); with the enclosing module's declarations
searched first (the second), an enclosing `enum E { A }` hid a sibling's `enum E { B }` (E0103,
Codex review r1), and with the children's types included, a child's `E` did the same, and an
enclosing non-generic `enum Result` hid the builtin (E0113, Codex review r2).

Three rules of Resolve's own scope also refused programs Check accepts, at top level on main and,
through this walk, in submodules (Codex review r3): a function hid a same-named enum, a user
`enum Option` hid the builtin, and a child's newtype was unknown. They are bug 229, fixed alongside.

Its known type names are the same environment's: its own (with the builtins), its children's
newtypes, its imports' and its siblings' structs and enums. Inheriting the enclosing module's (the
third version) let `outer`'s `type Nope` pass a cast in `inner`, where Check cannot expand it, and
the backend panicked again (Codex review r4; `error_217_nested_enclosing_alias_cast.con`). The same
shape between top-level modules, whose known types are still program-wide, is bug 230.

Regression legs in `run_tests.sh`: `error_217_nested_cast_unknown_type.con` (E0108),
`error_217_nested_undeclared_name.con` (E0100, was E0201), `error_217_nested_annotation_unknown_type.con`
(E0108, was E0220), `error_217_module_after_main_cast.con` (E0108, was the backend panic),
`error_217_nested_enclosing_alias_cast.con` (E0108, was the backend panic), each
asserting `error[resolve]`; `regress_217_nested_submodule_scope.con` (a nested submodule using its
constant, a sibling's type and an import: 42 natively and under `--interp`),
`regress_217_sibling_type_precedence.con` (the r1 and r2 witnesses: a sibling's `E` over the
enclosing module's and a child's; 42 on both), `regress_217_enclosing_type_not_in_scope.con` (the r2
`Result` witness: the front end accepts it, `--emit-core` succeeds, and `--interp` returns 42; the
native build met bug 227 in Lower and was pinned there until that bug's fix, 2026-10-25, after which `run_tests.sh` also runs the binary); and a project build of
`tests/regressions/bug217_resolve_skips_nested_submodules/project`, whose `src/util.con` must be
refused with resolve E0108 at `src/util.con:5:13`. Every std module now goes through Resolve in every
project build and the whole suite is the control that the walk refuses nothing it accepted.

## Structural cause

Class 11 ("absence read as a fact"): a module Resolve never visits produces no diagnostics, and
"no diagnostics" reads as "resolved". H12 closed the same gap for Check (`checkSubmodules`) and bug
113 for the extern-effects rule; the name and written-type rules of Resolve itself kept the
one-level walk.

## Structural owner

The walk over every module of the tree is in place (`resolveModuleTreeErrors`). The scope it gives a
submodule is assembled from summaries, as Check's and Elab's are, three times over; R-0492/R-0514
(one resolver, one scope per module, consumed by Check and Elab) removes that. The gate that would
catch the class for each rule is a nested-module case per Resolve rule (R-0498, S3).
