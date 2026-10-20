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

## Structural cause

The interpreter resolves a call by the name it sees in Core and keeps its own table of callable names; an import from a nested module is spelled differently there than in the backend's tables. Declaration identity by spelled name ([structural class](STRUCTURAL_CLASSES.md) 1) in a second semantics (class 7).

## Structural owner

R-0492 (S5 `DefId`) and R-0496 (S7 Core semantics).

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
