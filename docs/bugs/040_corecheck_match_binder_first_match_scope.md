# Bug 040: CoreCheck false E0500 on same-named match binders of different scalar types

**Status:** Fixed (2026-07-17)
**Fixed in:** CoreCheck.lean — `addVar` prepends (rebinding shadows, since
`lookupVar` is first-match), and `ccCheckMatchArm` saves/restores the var
table around enum/var arm bodies (payload binders are arm-scoped).
**Regression test:** `tests/programs/regress_040_match_binder_types.con`
(run_ok 42).
**Discovered:** 2026-07-17, switching envcfg onto `Bytes::index_of` — the
idiomatic `Option::Some { value }` payload name collides with itself.

## Repro

```text
match a { Option::Some { value } => { n = value; }, ... }   // value: u64
match c { Option::Some { value } => { t = value; }, ... }   // value: bool
// error[core-check]: (E0500) type mismatch for variable 'value':
//   declared Concrete.Ty.uint, used as Concrete.Ty.bool
```

Check.lean accepts the program (correctly); CoreCheck rejected it — a
front-end/core phase disagreement, fail-closed. Every function matching two
Options/Results with different SCALAR payload types hit this, because the
canonical payload field is named `value` in both. Aggregate payloads
(String, Bytes, structs) escaped via the E0500 leniency for
`.named`/`.generic` types, which is why no prior workload (all
String/TextFile payloads) tripped it.

## Root cause

CoreCheck's `vars` table was append-only for the whole function body with
first-match lookup: the first `value` binder's type won forever, and arm
binders never went out of scope. Lower and the interpreter both handle
same-named binders of different types correctly (verified: the repro runs
right on both sides once CoreCheck admits it), so this was a checker-only
false positive.
