# Bug 112 — the declaration std.mem uses for `sizeof` is rejected (E0116 at 0:0) outside std

**Status:** OPEN
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

## Structural cause

A body-less function declaration is parsed into the extern record without its `with(...)` clause or span, so a fact the author wrote is dropped and its absence is then read as "undeclared" ([structural classes](STRUCTURAL_CLASSES.md) 11, absence read as a fact, and 12, one fact kept in two places: the parsed signature and the extern record). std passes only because its declarations are handled on a different path.

## Structural owner

R-0492/R-0514 (one resolver and declaration record for functions and externs); proposed interim: carry `capSet` and span through `parseFnDefOrDecl`.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
