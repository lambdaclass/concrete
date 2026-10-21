# Bug 117 — the interpreter does not resolve module-level constants

**Status:** OPEN
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

## Where it showed up

examples/snippets/constants.con, recorded `interp=fails:` in the manifest.

## Structural cause

The interpreter has its own environment construction and binds only functions and locals, not module constants, which Lower inlines for the backend. Semantics implemented several times ([structural class](STRUCTURAL_CLASSES.md) 7, with 066, 084, 085).

## Structural owner

R-0496 (S7 one Core semantics the interpreter executes) with the differential harness R-0491 (exit 2c); proposed: a module-constant case in `check_codegen_differential.sh`.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
