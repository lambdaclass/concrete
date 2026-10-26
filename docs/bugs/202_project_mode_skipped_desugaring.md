# Bug 202 — project builds skipped desugaring, so a `let … else` was refused

<!-- bug-ledger
summary: desugaring was a separate step each driver called between resolve and check, and the project loader, debug bundle, reducer and Lean pipeline tests skipped it, so a project's `let … else` reached Check undesugared and was refused while the same file compiled standalone; desugaring is now part of `Pipeline.resolve`. Pinned by `regress_202_project_let_else` (run_tests.sh project loop) and `check_local_identity.sh` leg R
fixtures: regress_202_project_let_else/src/main.con
class: 12 — the front-end stage sequence restated by each driver; four skipped desugaring
silent: no
-->

**Status:** FIXED (branch `vp-r0492`)
**Found:** 2026-10-23, while moving the checker's binding identities to Resolve (R-0492 slice 1):
the local walk had to run after desugaring on every path into Check, and four of the paths never
desugared.
**Severity:** loud. A valid program was refused, but only in some compilation paths: a function
with a destructuring `let … else` built as a standalone file and was rejected inside a project.

Confirmed real by Codex review, 2026-10-25: docs/spec/patterns.md PAT-5 permits diverging let-else; project builds skipped desugaring and rejected that valid form.

## Reproducer

`tests/programs/let_else.con` compiles and runs (prints 0) standalone. Copied to
`<pkg>/src/main.con` with a `Concrete.toml`, `concrete build` refused it:

```
src/main.con:10:9: error[check]: a destructuring `let` is not supported in a `for` header; destructure before the loop
src/main.con:13:16: error[check]: (E0201) use of undeclared variable 'value'
```

## Cause

`Pipeline.desugar` (destructuring `let` → `match`) was a separate step that each driver called
between `Pipeline.resolve` and `Pipeline.check`. The file-mode driver (`runFrontend`) and two
paths in `Main.lean` called it. The project loader (`Project.loadProject`, behind `build`, `run`,
`test` and project reports), the debug bundle, the reducer and the Lean pipeline tests did not.
Check therefore saw the `letDestructure` statement itself. Check refuses that statement, because
the only place one can survive desugaring is a `for` header.

## Fix

Desugaring is part of `Pipeline.resolve` and `Pipeline.resolvePartial` (`finishResolve`), followed
by the local binder walk. `Pipeline.desugar` is gone, so no caller can forget it, and every path
gives Check and Elab the same body.

## Structural cause

One fact kept in several places: the sequence of front-end stages was restated at each call
site, and four of the nine restatements omitted a stage. Class 12 of
[STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md).

## Structural owner

R-0497, whose target is that `Pipeline.lean` exposes only typed stages. A stage that every later
stage needs is part of the stage before it, not a separate call. R-0492 slice 1, which carries
this fix, depends on it: the checker refuses a function whose local table is missing, which is
exactly what a skipped stage now produces.

## Regression

- `tests/programs/regress_202_project_let_else/`: the project loop in `run_tests.sh` builds and
  runs it (exit 0).
- `check_local_identity.sh` leg R builds `let_else.con` as a project, and leg R checks that
  `Pipeline.resolve` attaches a resolved table to every function.
