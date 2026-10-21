# Bug 115 — the builtin `vec_get` returns a non-Copy element by value, duplicating a linear value

**Status:** FIXED (branch `lowering-bugs`)
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

## Fix

The `vec_get` intercept in `Check` requires a Copy element: a non-Copy element read by value is
E0290, the H11 rule a by-value array element read already follows ("cannot move non-Copy value
... out of a Vec element"; the message names `pop`, `swap_remove`, `replace` and `with_at`, the
std `Vec` operations that move a linear element out or read it in place). An unbounded type
parameter is not Copy, so a generic body is refused before instantiation. The sibling `vec_set`
intercept requires Copy too (E0291): it overwrote the element without returning or destroying it,
which loses a linear value, the case the 2026-10-21 ownership-on-failure decision made
`Vec::set` / `Vec::set_unchecked` Copy-only for; `Vec::replace` is the linear form.

Callers migrated: `tests/programs/test_alloca_loop_stress.con` and
`test_string_literal_in_loop.con` took each `String` out with `vec_get` and dropped it (each once,
so no double free, but by the duplicating operation); they now take it with `vec_pop`. Every other
`vec_get` / `vec_set` in `tests/`, `examples/` and `std/` is at a Copy element.

Permanent legs in `run_tests.sh`: `error_115_vec_get_non_copy.con` (the reproducer, E0290),
`error_115_vec_get_generic_unbounded.con` (E0290 at `T`) and `error_115_vec_set_non_copy.con`
(E0291). The pre-fix compiler accepts all three. The open-bug reproducer row is dropped.

## Where it showed up

examples/json kept a `Vec<String>` string pool read with `vec_get`; the port replaced it with one owned buffer of spans.

## Structural cause

Builtin collection operations are typed by intercepts in Check that return the element type with no ownership requirement, outside the linearity rules every ordinary function signature goes through ([structural classes](STRUCTURAL_CLASSES.md) 13 and 15: a rule, use-once for non-Copy values, with no enforcing site on this path).

## Structural owner

R-0499 (S8 use-once and conservation on Core, row 4); repair owner: queue exit 0b (memory safety in safe code) -- `vec_get` requires `T: Copy`.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") ran the reproducer and required the recorded failure until the fix, which dropped the row.
