# Bug 045: Nested same-named match binders shared one runtime slot (silent wrong values)

**Status:** Fixed (2026-07-18)
**Fixed in:** Elab — match payload/pattern binders are ALPHA-RENAMED to
unique Core names (`value` → `value.bN`; surface identifiers cannot
contain `.`). A `renames` map (prepend = innermost shadows) is applied at
ident emission and assignment targets; arm bindings carry the fresh name;
the fresh counter is MONOTONE per function (scope restores preserve it) so
sequential arms can never re-mint a name into Lower's function-flat slot
table. Every stage below Elab (Interp, Lower, CoreCheck) inherits correct
scoping from the unique names.
**Regression test:** `tests/programs/regress_045_match_binder_shadow.con`
(run_ok 42) — nested + sequential shadowing, both binders read after the
inner match.
**Discovered:** 2026-07-18, workload 7 (httpget): an inner `value: u64`
binder inside an outer `value: TcpStream` arm hit SSA-verify E0715
(mismatched slot type). The reduced same-type probe revealed the silent
flavor.

## Repro (silent wrong value, both backends)

```text
match outer() {                      // Option<u64>, Some{value}=5
    Option::Some { value } => {
        match inner() {              // Option<u64>, Some{value}=7
            Option::Some { value } => { if value != 7 { return 3; } },
            ...
        }
        if value != 5 { return 4; }  // returned 4: outer read the INNER 7
        ...
```

The interpreter and the compiled binary BOTH returned 4 — the stages share
the flat-name Core representation, so differential testing was blind to
it. The different-type variant fail-closed at SSA-verify (E0715); the
same-type variant was silent wrong-code. Most dangerous member of the
039–044 identity-by-name family, and the root fix for it: with unique
binder names in Core, the whole class (040's CoreCheck table, 041's
ownership merge, this) cannot recur at those layers.

## Why no workload hit it before

The canonical payload name `value` collides constantly, but earlier
workloads never READ the outer binder after an inner same-named match —
they consumed the outer payload before or returned from the inner arm.
wordfreq/envcfg-style code (rebind-immediately idiom) masks it too.
