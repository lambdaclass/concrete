# Bug 152 — `string_trim("")` returns a one-byte string

**Status:** FIXED 2026-10-21 (interpreter-coverage pass, branch `interp-coverage`)
**Found:** 2026-10-21, by the interpreter-coverage pass: once the interpreter modelled `String`
over memory as the runtime does, an empty-string probe disagreed with the binary.
**Severity:** silent wrong value, and an out-of-bounds read: the result holds the byte past the end
of the source buffer (for a literal, its NUL terminator; for an empty `String` with no buffer, a
read through a null pointer).

## Reproducer

`tests/regressions/bug152_string_trim_empty/repro.con`:

```con
fn main() with(Console, Alloc) -> Int {
    let e: String = "";
    let t: String = string_trim(&e);
    let n: Int = string_length(&t);
    drop_string(e);
    drop_string(t);
    return n;
}
```

On `725d85ab` the binary exits **1**; expected **0**. `--interp` gives 0.

## Cause

`string_trim` in `Concrete/Backend/EmitBuiltins.lean` scans from the left; when that scan consumes
every byte (an empty or all-whitespace string) it jumps to `trim_result`, where the right end's phi
took `0` from that edge, so `tr_right = 0 + 1 = 1`. For an all-whitespace string the left index is
at least 1 and the result is still empty, but for the empty string `left = 0 < right = 1`, so the
runtime copied one byte starting at the source buffer.

## Fix

The phi's incoming value from the exhausted left scan is `-1`, so `tr_right` is 0 and the result is
the empty string (capacity 1, as for all-whitespace input). Permanent legs:
`tests/programs/regress_152_string_trim_empty.con` (`run_ok` 0) and the reproducer in
`run_tests.sh`'s interp-vs-native loop.

## Structural cause

Semantics implemented several times ([structural class](STRUCTURAL_CLASSES.md) 7): the runtime
builtins are hand-written LLVM with no specification they are checked against, and the interpreter,
the differential reference, modelled `String` as a Lean string rather than the runtime's buffer, so
it could not see edge cases of the runtime code.

## Structural owner

R-0496 (S7 one Core semantics) with the harness R-0491 (exit 2c); proposed: a differential leg over
every string builtin at empty, whitespace and non-ASCII inputs.
