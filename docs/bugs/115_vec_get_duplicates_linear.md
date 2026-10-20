# Bug 115 — the builtin `vec_get` returns a non-Copy element by value, duplicating a linear value

**Status:** OPEN
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** memory safety: the checker accepts a program that double-frees.

## Reproducer

`tests/regressions/bug115_vec_get_duplicates_linear/repro.con`:

```concrete
// Bug 115: the builtin vec_get returns the element BY VALUE with no Copy
// requirement, so a linear String can be taken out of a Vec twice. Both
// copies own the same buffer: the binary double-frees (abort, exit 133/134
// on macOS) while the interpreter prints "AB" and exits 0.
// Expected: a diagnostic (vec_get needs T: Copy). Actual: compiles.
fn main() with(Std) -> Int {
    let mut v: Vec<String> = vec_new::<String>();
    let mut s: String = "";
    string_push_char(&mut s, 65);
    string_push_char(&mut s, 66);
    vec_push::<String>(&mut v, s);
    let a: String = vec_get::<String>(&mut v, 0);
    let b: String = vec_get::<String>(&mut v, 0);
    drop_string(a);
    print_string(&b);
    drop_string(b);
    vec_free::<String>(v);
    return 0;
}
```

## Observed

`vec_get::<String>(&mut v, 0)` twice yields two owning `String`s over one buffer. The binary aborts (double free; exit 133/134 on macOS) while the interpreter prints `AB` and exits 0. The Check intercept for `vec_get` returns `elemTy` with no Copy requirement.

## Expected

Require `T: Copy` for `vec_get` (non-Copy elements go through a borrowing or moving API).

## Where it showed up

examples/json kept a `Vec<String>` string pool read with `vec_get`; the port replaced it with one owned buffer of spans.

## Structural cause

Builtin collection operations are typed by intercepts in Check that return the element type with no ownership requirement, outside the linearity rules every ordinary function signature goes through ([structural classes](STRUCTURAL_CLASSES.md) 13 and 15: a rule, use-once for non-Copy values, with no enforcing site on this path).

## Structural owner

R-0499 (S8 use-once and conservation on Core, row 4); repair owner: queue exit 0b (memory safety in safe code) -- `vec_get` requires `T: Copy`.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
