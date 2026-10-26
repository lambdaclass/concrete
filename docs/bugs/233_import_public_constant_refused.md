# Bug 233 — a public constant cannot be imported, and the refusal says it is not public

<!-- bug-ledger
summary: an import of a `pub const` was refused as "not public" (E0285; a `pub trait` too): Resolve's public-name list admitted it, the import rule Check and Elab share had no case for it, and Core named a constant by its spelling. Contained first (refused with the reason; traits stay module-local by design, E0126 / E0323 / E0423), then fixed by R-0492 step 4: a constant is imported by its `ConstId`, Core references it as `cfg::BASE`, and Lower and the interpreter inline its initializer in its defining module's scope. Found by the differential hunt, 2026-10-24. Fixtures `regress_233_*`, `error_233_*`. Classes 12 and 1.
fixtures: regress_233_import_constant.con regress_233_nested_import_constant.con regress_233_import_constant_scope.con regress_233_nested_alias_constant.con regress_233_constant_type_alias.con regress_233_constant_type_imported_alias.con error_233_imported_initializer_calls_function.con error_233_imported_initializer_extern.con error_233_imported_initializer_via_constant.con error_233_import_constant_collides_local.con error_233_import_trait.con regress_233_shared_spelling_import.con
class: 12 — which declarations an import may name: the public-name list holds traits and constants, the import rules have no case for them; fixed 2026-10-25, R-0492 step 4: a constant is imported by its `ConstId`, a trait refused with the reason
silent: no
-->

**Status:** FIXED (2026-10-25, R-0492 step 4, branch `wip/r0492-steps34`; contained 2026-10-24 on
`wip/diffhunt-1025`)
**Found:** 2026-10-24, by the differential hunt on main `9622f7e0` (module constants across
modules): `mod cfg { pub const BASE: i32 = 7; } import cfg.{BASE};` is refused by both the binary
and `--interp` with `(E0285) 'BASE' is not public in module 'cfg'`.
**Severity:** a valid program by MOD-2 is refused, with a false diagnostic; no wrong code.

Confirmed real by Codex review, 2026-10-25: docs/spec/modules.md MOD-2 permits importing public constants; missing import handling falsely reported “not public.” Trait-import refusal is intended under docs/spec/traits.md.

## Reproducer

- `tests/programs/regress_233_import_constant.con` (formerly `error_233_import_constant.con`): the
  program above. On main: Check's E0285 "not public". While contained: `(E0126) 'BASE' is a constant of module 'cfg', and a constant cannot be imported
  yet: …`.
- `tests/programs/error_233_import_trait.con`: `import a.{Show}` of a `pub trait`. On main: E0285
  "not public"; traits are module-local and cannot be imported (TRAIT spec), but they are public.
  Now: E0126, "'Show' is a trait of module 'a', and a trait cannot be imported: traits are
  module-local".
- `tests/programs/regress_233_nested_import_constant.con` (formerly
  `error_233_nested_import_constant.con`): the constant import in a nested submodule, whose imports
  Check validates. While contained: E0323 with the same reason.

Expected by MOD-2@1 ("another module uses [a `pub` declaration] only by importing it"): the
constant import is accepted and `main` returns 7. A qualified use, `cfg::BASE`, is refused too
(E0100, "undeclared variable 'cfg_BASE'"); MOD-2 names import as the way in, so that is not this
bug.

## Cause

`FileSummary.publicNames`, the list Resolve's import check reads, includes public traits and
constants, so `Resolve.summaryImports` admitted the import; it then found no struct, enum,
function, newtype or alias of that name and bound nothing. `FileSummary.resolveImports`, the rule
Check and Elab apply, has no constant or trait case at all and fell through to its last arm, "not
public". Nothing below the import can carry an imported constant either: Elab elaborates only a
module's own constants (`ConstSummary` has no initializer), and Lower inlines a constant by its bare
spelling from the module's own tree (`collectAllConstants`; bug 132 refuses two modules' constants
of one name for that reason).

## Containment

`FileSummary.unimportableKind` names a public trait or constant, and both import rules refuse it
with the reason: Resolve (E0126, top-level modules), Check (E0323) and Elab (E0423) through
`resolveImports`. The refusal applies only when the name binds nothing the import rule binds:
callables, constants and types are separate namespaces (MOD-3), so `pub fn F` beside `pub const F`
is still imported as the function (Codex review). Elab's E0423 is defense in depth (Check refuses
first). Traits stay unimportable by design, so that half is fixed; the constant half is open.
Regression legs in `run_tests.sh`: the three `error_233_*.con` fixtures (`run_err` on the code),
`regress_233_shared_spelling_import.con` (a function and a constant, and a function and a trait, of
one spelling: `run_ok … 42` and `--interp` = 42) and the project
`tests/regressions/bug233_import_public_constant_refused/project` (a `mod user;` file importing a
`mod cfg;` file's constant: E0323 attributed to `src/user.con`).

## Fix

R-0492 step 4 ([DECLARATION_IDENTITY.md](../compiler/DECLARATION_IDENTITY.md)). A public constant
is imported by its identity, as MOD-2@1 states; nothing about mutable state changes (a constant is
immutable and inlined at each use):

- `buildFileSummary` stamps each constant's defining module (`ConstSummary.definedIn`, with its
  name the `ConstId`). The import rule Check and Elab share (`FileSummary.resolveImports`) binds a
  public constant under the importer's spelling as an `ImportedConst` (local name, `ConstId`, and
  its type with the aliases its defining module can see, its own and those it imports, expanded
  throughout, along every acyclic chain of alias imports: `writableAliases`),
  only when the name binds no callable or type, so `pub fn F` beside `pub const F` is still the
  function's import (MOD-3; `regress_233_shared_spelling_import.con`). Resolve binds it in both of
  its import paths (`summaryImports`, `importScope`). Only a public trait is still unimportable
  (E0126 / E0323 / E0423).
- Check types the reference from the import; an imported constant beside a local constant of one
  spelling is E0299, as for functions.
- Elab references an imported constant by its identity's Core spelling, `ConstId.coreName`
  (`cfg::BASE`; `::` is no identifier character, so no declaration of the importer can take it),
  with evidence naming the defining module.
- Lower and the interpreter look that spelling up in a program-wide table
  (`importedConstantsOf`, `collectImportedConsts`) and inline the initializer with the DEFINING
  tree's constants in scope, so a private constant it names is the defining module's. Bug 132's
  refusal of two same-spelled constants stays for bare spellings within one tree (and the
  interpreter's program-wide form of it, bug 245); an imported constant is never one.
- Not inlined across modules: an initializer that calls or names a function, itself or through a
  constant it names (`initReachesCallable`). Core names a callee by a spelling, which the using function would
  resolve in its own context (its foreign bindings, its call aliases: an initializer's extern
  `abs` would reach the importer's `fn abs`, Codex review). Lower and the interpreter refuse it
  with that reason until callables carry `DefId`s (a later R-0492 step).

Regression legs in `run_tests.sh`: `regress_233_import_constant.con`,
`regress_233_nested_import_constant.con`, `regress_233_import_constant_scope.con` (an initializer
reading the defining module's private constant; two modules' `BASE`, one aliased; an array
constant), `regress_233_nested_alias_constant.con`, `regress_233_constant_type_alias.con` and
`regress_233_constant_type_imported_alias.con`, each
`run_ok … 42` and `--interp` = 42; `error_233_imported_initializer_calls_function.con` and
`error_233_imported_initializer_extern.con` and `error_233_imported_initializer_via_constant.con`
(the inlining refusal, native and `--interp`);
`error_233_import_constant_collides_local.con` (E0299); `error_233_import_trait.con` (E0126); and
the project `tests/regressions/bug233_import_public_constant_refused/project`, which now builds and
exits 42.

## Structural cause

Class 12 ("one fact kept in two places"): which declarations an import may name is stated twice,
as the module's public-name list (every `pub` declaration) and as the import rules' per-kind cases,
and the two disagree for traits and constants; and class 1 for the open half: a constant's identity
in Core is its spelling, so there is nothing an import could bind.

## Structural owner

R-0492/R-0514 (one resolver with `DefId`s: an import binds a declaration identity, which a constant
reference then carries through Elab and Lower). Step 4 of the design note does this for constants;
the import rule is still stated twice (Resolve's `summaryImports` and `resolveImports`), which
R-0514's one resolver removes.
