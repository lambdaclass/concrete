# Bug 055: project-mode sibling file-module import alias is unusable — valid program rejected

**Status:** Open
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
