# Bug 111 — `main` may declare parameters, and the wrapper calls it with none

**Status:** OPEN
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** undefined behavior in an accepted program: the parameter reads an uninitialized register.

## Reproducer

`tests/regressions/bug111_main_params_accepted/repro.con`:

```concrete
// Bug 111: `main` may declare parameters. The generated wrapper calls
// `user_main()` with no arguments, so `argc` reads an undefined register
// (it happens to be 0 on arm64). Expected: a diagnostic rejecting the
// signature. Actual: compiles; the interpreter fails "undefined variable 'argc'".
fn main(argc: i64) -> i64 {
    return argc;
}
```

## Observed

`fn main(argc: i64) -> i64 { return argc; }` compiles. The generated C `main` calls `user_main()` with no arguments while `user_main` is `define i64 @user_main(i64 %argc)`, so `argc` is whatever the register holds (0 on arm64 in practice). The interpreter fails `undefined variable 'argc'`.

## Expected

Reject parameters on `main` with a diagnostic (arguments are read through std.args / the argc/argv builtins).

## Where it showed up

examples/snippets/borrow.con used `main(argc: i64)`; it was ported to a local.

## Structural cause

The entry-point contract is documented (EXECUTION_MODEL.md: `fn main() -> Int`) but has no enforcing site: Check accepts any signature for `main` and the backend's wrapper assumes none ([structural class](STRUCTURAL_CLASSES.md) 13, documented rules with no enforcing site).

## Structural owner

R-0498 (S3, exit 2l: every documented rule has an enforcing site); proposed: a Check rule rejecting parameters on `main`.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
