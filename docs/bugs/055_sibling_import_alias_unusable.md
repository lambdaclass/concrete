# Bug 055: project-mode sibling file-module import alias is unusable — valid program rejected

**Status:** Fixed 2026-10-18 with [bug 082](082_import_alias_loses_to_same_named_definition.md)
**Discovered:** 2026-07-18, middle-end audit (reproduced: `import a.{pick
as f}` from a sibling file module resolves at Check, then EmitSSA emits a
call to undefined `@pick` — generic AND non-generic callees both fail at
llvm-as; the fully-qualified `import proj.a.{pick as f}` form works).

## Symptom

In a project, `src/main.con` doing `import a.{pick as f}` (sibling
`src/a.con`) compiles the alias to nothing usable: Mono's lookupFn misses
and the emitted call targets an undefined symbol. The program is valid and
should work.

## Root cause

The alias gets only the bare `("f","pick")` orientation: Elab's
local-submodule alias branch requires the importing module's OWN
`submoduleSummaries` to contain `a` (`Elab.lean:1776-1790`), and the
nested-path branch requires ≥2 module components (`Elab.lean:1810-1812`).
The def is `a_pick` (prefixModuleFnNames), which neither orientation names.
The bug-044 fix covered std-style (`std.alloc.{dealloc as dd}`) imports;
this sibling-file topology slips through.

## Candidate fix

Extend the sibling-module alias branch to also register
`("f", "a_pick")` (def-name orientation), matching what
`prefixModuleFnNames` produces. Regression: a two-file project with a
renamed sibling import (generic and plain) builds and runs.

## Fixed (2026-10-18, bug 082)

Elab no longer carries an alias into Core. `importCallSpellingsFor` spells a call
through `import a.{pick as f}` from a sibling submodule of a top-level module as
the sibling's definition, `a_pick`. Regression:
`tests/programs/regress_082_project_alias_binding/` (project test, exit 0). The
previous compiler fails it at llvm-as with `use of undefined value '@pick'`.
