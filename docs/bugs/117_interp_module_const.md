# Bug 117 — the interpreter does not resolve module-level constants

**Status:** FIXED 2026-10-21 (interpreter-coverage pass, branch `interp-coverage`)
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** interpreter/native disagreement: the interpreter rejects a valid program the binary runs.

## Reproducer

`tests/regressions/bug117_interp_module_const/repro.con`:

```concrete
// Bug 117: the interpreter does not resolve module-level `const`s
// ("interp: undefined variable 'K'"); the binary exits 7.
mod m {
    const K: i32 = 7;
    fn main() -> i32 {
        return K;
    }
}
```

## Observed

`const K: i32 = 7; fn main() -> i32 { return K; }`: the binary exits 7; `--interp` fails `interp: undefined variable 'K'`.

## Expected

Bind module constants in the interpreter environment.

## Fix

`evalExpr`'s `.ident` case looks a name up in the current frame first and, when it is not a local,
evaluates the module constant's initializer the way Lower inlines it: in a frame of its own, so a
caller's local of the same name cannot capture a name the initializer reads (Lower's bug 123
hygiene). A constant name defined by two modules is refused, as Lower refuses it (E0602, bug 132).
Name lookup now stops at a frame boundary (`frameMarker`): the environment is one list shared by
every frame so that borrows of a caller's bindings resolve, and without the boundary a callee
reading `K` found a caller's local `K` first. The reproducer and `examples/snippets/constants.con`
agree (7 and 20); permanent legs: `run_tests.sh`'s interp-vs-native loop over fixed reproducers, and
`interp=yes` for `constants.con` in the example manifest.

## Where it showed up

examples/snippets/constants.con, recorded `interp=fails:` in the manifest.

## Structural cause

The interpreter has its own environment construction and binds only functions and locals, not module constants, which Lower inlines for the backend. Semantics implemented several times ([structural class](STRUCTURAL_CLASSES.md) 7, with 066, 084, 085).

## Structural owner

R-0496 (S7 one Core semantics the interpreter executes) with the differential harness R-0491 (exit 2c); proposed: a module-constant case in `check_codegen_differential.sh`.

## Containment while open

While open, `scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") required the
recorded failure; the row was removed with the fix.
