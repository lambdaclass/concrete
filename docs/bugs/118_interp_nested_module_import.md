# Bug 118 — the interpreter cannot call a function imported from a nested module

**Status:** FIXED 2026-10-21 (interpreter-coverage pass, branch `interp-coverage`)
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

## Fix

The interpreter resolves a direct call's spelling the way the backend does, per calling module,
in `resolveCall`: a function defined under the spelling (several: the one in the calling module's
tree, else refused); then a top-level module's own function under its sibling spelling
`<module>_<fn>` (bug 082; the backend's qualified symbol when two modules define `fn`); then the
calling module's import aliases (`hello2 ↦ Deep_hello2`); then any module's aliases when they
agree. Every ambiguity is refused rather than settled by list
order, which the old flat first-match lookup did silently. Each function runs with its own
top-level module as the scope for the calls it makes, and a function pointer records the
definition it was resolved to. The reproducer, `examples/snippets/import.con` and
`examples/project` agree with the binary (5, 12, 18); permanent legs: `run_tests.sh`'s
interp-vs-native loop over fixed reproducers, and `interp=yes` for the three examples.

## Where it showed up

examples/snippets/import.con and examples/project/src/main.con, recorded `interp=fails:` in the manifest.

## Structural cause

The interpreter resolves a call by the name it sees in Core and keeps its own table of callable names; an import from a nested module is spelled differently there than in the backend's tables. Declaration identity by spelled name ([structural class](STRUCTURAL_CLASSES.md) 1) in a second semantics (class 7).

## Structural owner

R-0492 (S5 `DefId`) and R-0496 (S7 Core semantics).

## Containment while open

While open, `scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") required the
recorded failure; the row was removed with the fix.
