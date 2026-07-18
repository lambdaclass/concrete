# Bug 039: Imported bare fn name rebinds to another module's colliding import

**Status:** Fixed (2026-07-17)
**Fixed in:** EmitSSA.lean `emitSModule` — the emitting module's OWN
bare→qualified linker aliases (entries whose target names an emitted
definition) are prepended to `localAliases`, which call resolution consults
BEFORE the program-wide `linkerAliases` pool. Which definition a bare
imported name means is a fact of the calling module's imports, not of module
emission order.
**Regression test:** `tests/programs/regress_039_import_alias_collision/`
(project test, exit 0: std.env set/get roundtrip).
**Discovered:** 2026-07-17, workload 5 (envcfg) — first compiled use of
`std.env.get`.

## Repro

```text
import std.env.{get};
...
let home: Option<String> = get(&key);    // SIGSEGV at runtime
```

Compiled output shows `call %enum.Option @args_get(ptr %addr.0)` — the env
import bound to `std.args.get(idx: Int)`, which reads the String pointer as
an argv index. The interpreter resolves imports per-module and was NOT
affected — a compiled-vs-interp differential on any `std.env.get` program
would have caught this, but no env-touching program had ever been compiled
(std.env had interpreter tests only).

## Root cause

Two free functions named `get` exist in std (`std.args.get`,
`std.env.get`), so both definitions are emitted collision-qualified
(`args_get`, `env_get`). Call sites still say `get` and are resolved late,
in EmitSSA, through `s.linkerAliases` — a flat first-match list ACCUMULATED
across all modules in emission order (`s.linkerAliases ++ m.linkerAliases`).
`std/src/cli.con` has `import std.args.{count, get}`, contributing
`("get", "args_get")` before any user module is emitted; a later module's
`("get", "env_get")` entry could never win. The per-module `localAliases`
mechanism existed but only covered the REVERSED orientation
(qualified-call → bare-def sibling aliases), so bare→qualified import
aliases fell through to the global pool.

Same family as bug 036 and the audit-#2 intrinsic identity fix: identity
must be carried by resolution, not re-derived from a name in a global
namespace.

## Fix shape

`emitSModule` filters `m.linkerAliases` for entries whose target is in
`s.definedFnNames` (bare→qualified orientation; sibling qual→bare entries
whose bare target is a real non-colliding definition are harmless to
include) and puts them at the head of `localAliases`. Call emission
(`emitSFnDef` call path and the `@fnref.` function-pointer path in
`svalToOperand`) both consult `localAliases` first, so the calling module's
own imports now shadow the pool.

## Residual risk (documented, not fixed)

Within ONE top-level module, two SUBMODULES importing different `get`s
still share a flat `m.linkerAliases` (collectAllLinkerAliases flattens the
submodule tree), so the submodule-granularity variant of this bug remains
possible. No workload has hit it; pull-gated.
