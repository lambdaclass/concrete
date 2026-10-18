# Bug 082 — an import alias lost to any same-named definition in the program

**Status:** FIXED (branch `bug082-alias-resolution`)
**Found:** 2026-10-18 by a name-resolution hunt (reproducer `hunt-names/alias/main.con`).
**Severity:** silent wrong code on both backends. A program called a function it never imported.

```text
mod a { pub fn check_class(c: Int) -> Int { ... }      // 1 for c == 2
        pub fn check_version(v: Int) -> Int { ... } }  // 0 for v == 2
mod b { import a.{ check_version as check_class };
        pub fn probe(c: Int) -> Int { return check_class(c); } }
```

`probe(2)` returned 1 compiled and under `--interp`: both ran `a.check_class`, which `b` never
imported. With `as foo` the alias worked. `--emit-core` showed `check_class(c)`: the alias spelling
went into Core and every later stage resolved it against the whole program.

## Mechanism

Core names a direct callee by a string (`Callee.direct name`). Elab wrote the importing module's
local spelling there, so an alias reached Core unresolved, and each consumer decided what it meant:

- **EmitSSA** asked `definedFnNames.contains name` before it consulted any alias table. Any
  definition anywhere with the alias's spelling won.
- **Interp** looked the name up in a flat list of every function in the program (first match). It
  had no alias table, so a non-colliding alias (`as foo`, or `import math.{add as plus}` in a
  project) was "undefined function", and a colliding one called the wrong function.
- **CoreCheck** looked the callee's capabilities up the same way, so an aliased call was checked
  against the other function's `with(...)`.
- **Check** searched imports before local functions and **Elab's callee identity** searched local
  functions first. A local `fn f` beside `import a.{g as f}` compiled: the call was typed against
  `a.g` and ran the local `f`.

The same flat lookup misrouted two unaliased shapes:

- `import c.{f}` while another top-level module also defines `f`: the call ran `a.f` (compiled
  and interp).
- In a project, a submodule doing `import std.math.{abs}` while a sibling submodule defines `abs`:
  Elab's cross-module rename (bare name → `util_abs`) was applied to every body regardless of its
  imports, so the call ran the sibling (measured 99 instead of 2).

## Fixed

- **One binding per name per module (E0299, Check).** An imported function whose local name is
  also a function or extern of the importing module is refused, and so are two imports binding one
  name to different definitions. Importing the same definition twice stays legal. The language
  docs did not say what a collision means, so it is now an error (in `checkModule`, so file and
  inline submodules are covered too).
- **Elab writes the definition's spelling, not the alias (`importCallSpellingsFor`).** For a call,
  a function value, or a `fnRef` whose name an import binds:
  - aliased import from a submodule of this module: `<sub>_<orig>` (the name `prefixModuleFnNames`
    gives the definition);
  - aliased import from a SIBLING submodule (a project's `src/b.con` importing
    `util.{pick as f}`): `<sibling>_<orig>`. This was [bug 055](055_sibling_import_alias_unusable.md),
    still open: the bare alias lowered to an undefined symbol;
  - aliased import by a dotted path: the prefixed path, as the nested linker aliases compute it;
  - import from a sibling top-level module: `<module>_<orig>`, the spelling `module::orig` already
    has. This is used when aliased, and also unaliased when several top-level modules define
    `orig`.
  These are spellings the existing linker-alias tables already resolve, so Mono, Lower and EmitSSA
  see no new shape. Externs keep their spelling (C symbols). Unaliased imports of uniquely-named
  definitions are unchanged, so goldens and evidence digests over them do not move.
- **CoreCheck capabilities** are recorded under the new spelling too
  (`CModule.importedFnCaps`). Without that, an aliased call would have read as "requires nothing".
- **Interp** also reaches every top-level function under `<module>_<fn>`, after the exact names.
  Before this, `module::fn` calls could not be interpreted at all.
- **Elab's cross-module rename** leaves alone a name the body's module imports from somewhere
  other than the submodule the rename points into.

## Regression

- `tests/codegen/import_alias_binding.con` is in the interp-vs-compiled differential, and
  `check_codegen_execution.sh` pins its value at 77324. It covers an alias over another module's
  same-named function, the alias as a function value, a generic alias with explicit and inferred
  type arguments, an unaliased import of a name two modules define, and an alias from an inline
  submodule. The previous compiler returned 12814, and its interpreter could not run the program
  (undefined `plus`). The alias rows agreed across backends in the original report, so agreement
  alone does not catch this bug. That is why the value is pinned.
- `tests/programs/regress_082_project_alias_binding/` (project test, exit 0) covers a sibling
  submodule alias (bug 055) and `std.math.abs` beside a sibling submodule's `abs`. The previous
  compiler does not build it (`use of undefined value '@pick'`). A std-using project cannot run under
  `--interp` (file mode does not resolve `std.*`), so this fixture is compiled-only.
- `tests/programs/error_082_import_alias_shadows_local.con` and
  `error_082_import_duplicate_binding.con` (`run_err E0299`).

## Limits

- Core still names callees by string. This fix makes the string name the definition, but it does
  not carry a `CallableId` on `CExpr.call`, which is the R-0002 end state. Interp, Mono and CoreCheck
  still resolve that string with flat program-wide lookups.
- **Interp only, not fixed:** two top-level modules that each define a function and call their OWN
  copy (`mod a { fn h() ... fa() { h() } } mod c { fn h() ... }`) both get the first `h` under
  `--interp` (11 instead of 12). The compiled path qualifies colliding definitions per module and
  is right. The interpreter has no notion of the calling module.
- Mono's `fnMap` is keyed by bare definition name. A generic function defined by two top-level
  modules specializes whichever was inserted last. This predates this fix and is untouched by it.
- Two imports under one name are refused across all import kinds (types included), not only
  functions.
