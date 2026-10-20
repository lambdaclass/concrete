# Bug 118 — the interpreter cannot call a function imported from a nested module

**Status:** OPEN
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** interpreter/native disagreement.

## Reproducer

`tests/regressions/bug118_interp_nested_module_import/repro.con`:

```concrete
// Bug 118: the interpreter cannot call a function imported from a nested
// module (`import a.b.{f}`): "interp: undefined function 'f'"; the binary
// exits 5.
mod m {
    import a.b.{f};
    fn main() -> i32 {
        return f();
    }
}
mod a {
    mod b {
        pub fn f() -> i32 { return 5; }
    }
}
```

## Observed

`import a.b.{f};` then `f()`: the binary exits 5; `--interp` fails `interp: undefined function 'f'`. A function imported from a top-level module works.

## Expected

Resolve imported nested-module functions in the interpreter as Elab does for the backend.

## Where it showed up

examples/snippets/import.con and examples/project/src/main.con, recorded `interp=fails:` in the manifest.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
