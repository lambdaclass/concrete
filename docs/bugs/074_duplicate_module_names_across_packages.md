# Bug 074 — two packages defining the same module name reached LLVM as one module

**Status:** FIXED 2026-10-04 (branch `r0484-assumptions`) — the collision is now refused
with a diagnostic; packages sharing a module path remain unsupported.
**Found:** 2026-10-04, while adding package scope to R-0484 R10 assumption identities: two
dependencies that each define `mod util` with `extern fn putchar` and `fn say`.
**Severity:** wrong behaviour with no diagnostic. A build failed only in LLVM validation;
report modes, which generate no code, succeeded and silently merged the two packages'
declarations, so an assumption report could attribute one package's binding to another.

## Symptom

```text
LLVM IR validation failed for .../app/src/main.con.ll:
llvm-as: ...: error: invalid redefinition of function 'say'
```

`concrete src/main.con --report assumptions` on the same project exited 0 and listed one
`util.putchar` where there were two.

## Mechanism

`loadProject` (`Concrete/Resolve/Project.lean`) appended every dependency's modules to the
project's and merged them by NAME. Nothing compared top-level module names across packages,
so both `util` modules entered resolution, checking and elaboration as one name, and their
functions met only at code generation as duplicate symbols.

## Fixed

`loadProject` records each top-level module's owning package and refuses a name defined by
two packages before resolution:

```text
error: module 'util' is defined by two packages: 'p1' (.../p1/src/lib.con) and 'p2' (.../p2/src/lib.con)
hint: packages cannot yet share a top-level module name; rename one of the modules (bug 074)
```

Package-scoped module paths are not implemented; this is a refusal, not support.
Independently of the refusal, assumption identity carries the package
(`Concrete/Report/AssumptionSummary.lean`), and a build-time `#guard` there shows two modules
named `util` from two packages yield two `util.putchar` identities.

## Regression

`tests/regressions/bug074_duplicate_module_packages/` (p1, p2, app), asserted by
`check_assumption_summary.sh`: both `build` and report mode refuse with the diagnostic naming
both packages. Before the fix the build failed in LLVM and the report succeeded with merged
declarations.
