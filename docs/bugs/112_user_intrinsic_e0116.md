# Bug 112 — the declaration std.mem uses for `sizeof` is rejected (E0116 at 0:0) outside std

**Status:** FIXED 2026-10-21 (branch `symbols`, with bug 113)
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** a valid program is rejected; the diagnostic has no location.

## Reproducer

`tests/regressions/bug112_user_intrinsic_e0116/repro.con`:

```concrete
// Bug 112: the declaration std.mem uses for `sizeof` is rejected outside std
// with E0116 at 0:0 ("foreign binding 'sizeof' does not declare its effects"),
// whether or not it is `trusted` and even when it is written `with()` --
// a body-less fn declaration drops its `with(...)` clause and its span.
// Expected: exit 5 (1 + 4). Actual: E0116.
mod foo {
    #[intrinsic = "sizeof"]
    pub trusted fn sizeof<T>() -> u64;
}
mod bar {
    import foo.{sizeof};
    pub fn main() -> u64 {
        return sizeof::<u8>() + sizeof::<u32>();
    }
}
```

## Observed

`#[intrinsic = "sizeof"] pub trusted fn sizeof<T>() -> u64;` (the exact std.mem form) fails with `(E0116) foreign binding 'sizeof' does not declare its effects` at `0:0`, also when written `fn sizeof<T>() with() -> u64;`. `parseFnDefOrDecl` returns a body-less declaration without its `with(...)` clause and span, so every body-less fn reaches the R-0484 R3 check with `capSet = none`.

## Expected

Carry the parsed `with(...)` (and span) onto the body-less declaration; decide separately whether a compile-time intrinsic needs an effect declaration at all.

## Where it showed up

examples/snippets/sizeof_intrinsic.con and examples/snippets/vec.con, manifest `known-bug 112`.

## Fix

R-0484 R3 no longer applies to an intrinsic the compiler implements: `#[intrinsic = "n"]` on a
declaration named `n` that `resolveIntrinsic` knows (`sizeof`, `alignof`, ...). No C code stands
behind it, so it is not a foreign binding (R10), and Resolve's R3 walk (`externEffectsCheck`, the
bug 113 fix) skips it. Any other annotation (an unknown name, a name that differs from the
declaration, or one combined with `#[link_name]`) leaves the declaration a foreign binding under
every rule: `error_intrinsic_unknown_is_foreign.con` (E0116, from the Codex review). The reproducer compiles and exits 5; it is the
permanent fixture `tests/programs/regress_112_user_intrinsic.con` (`run_ok` exit 5 in
`run_tests.sh`), and `examples/snippets/sizeof_intrinsic.con` (exit 5) and
`examples/snippets/vec.con` (exit 4) are promoted from `known-bug 112` to run rules (the
interpreter still has no `sizeof`). std passed before only because bug 113 hid its submodules from
R3; the 113 fix would otherwise have refused every project that imports std.mem.

The record's other half is fixed too: `parseFnDefOrDecl` keeps a body-less declaration's
`with(...)` clause and span, and `parseExternFn` its span, so a body-less foreign declaration can
write `with()` and E0116 / E0117 point at the declaration instead of `0:0`. The two
`check_cap_sibling_module.sh` fixtures that used a renamed intrinsic (`#[intrinsic = "sizeof"] fn
my_sizeof<T>()`, which calls C `my_sizeof` and is a foreign binding) now declare
`fn my_size() with() -> u64;`.

## Structural cause

A body-less function declaration is parsed into the extern record without its `with(...)` clause or span, so a fact the author wrote is dropped and its absence is then read as "undeclared" ([structural classes](STRUCTURAL_CLASSES.md) 11, absence read as a fact, and 12, one fact kept in two places: the parsed signature and the extern record). std passes only because its declarations are handled on a different path.

## Structural owner

R-0492/R-0514 (one resolver and declaration record for functions and externs); proposed interim: carry `capSet` and span through `parseFnDefOrDecl`.

## Containment while open

While open, `scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") required the recorded failure; the row was removed with the fix.
