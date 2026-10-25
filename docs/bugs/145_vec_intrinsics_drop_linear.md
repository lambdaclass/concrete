# Bug 145 — `vec_set` and `vec_free` lost non-Copy elements without destroying them

<!-- bug-ledger
summary: the raw `vec_set` intrinsic overwrote an initialized non-Copy element and `vec_free` released a buffer still holding non-Copy elements, both without destroying anything (found by Codex reviewing rule (c)). Contained: `vec_free` is E0287 unless the element type is Copy (`error_145_vec_free_linear.con`); the `vec_set` half is bug 115's fix (E0291, on main). Joins exit 0b. ((`safety-eval2`))
fixtures: error_145_vec_free_linear.con regress_145_vec_free_copy_param.con regress_145_vec_free_nested_copy_param.con
class: 13 — raw collection intrinsics outside rule (c)
silent: no
exit: 0b
-->

**Status:** FIXED 2026-10-22 (branch `safety-eval2`, containment of `vec_free`; the `vec_set` half is
bug 115's fix, on main since `9757540e`)
**Found:** 2026-10-22, by the Codex review of rule (c) on `safety-eval2` (the branch's claim that no
safe store into, or drop of, an initialized non-Copy place exists without consuming it).
**Severity:** skipped destruction in safe code: a non-Copy element overwritten in place, or left in
a buffer that is released, is never destroyed (a leaked `String` buffer; for a resource, a leaked
handle). A destructor-logging probe returned 0 under `--interp`: no destructor ran.

## Reproducers

`tests/regressions/bug145_vec_intrinsics_drop_linear/repro_set.con`:

```con
fn main() with(Alloc) -> Int {
    let mut v: Vec<String> = vec_new::<String>();
    vec_push::<String>(&mut v, "old");
    vec_set::<String>(&mut v, 0, "new");
    v.drop();
    return 0;
}
```

On main (`0408f4d4`): accepted; `vec_set` copies the new element over the old one
(`EmitBuiltins.lean` implements it with `memcpy`), so `"old"` is never freed.
`repro_free.con` (`vec_free::<String>(v)` with one element pushed): accepted; `vec_free` frees the
buffer only.

Fixed: `repro_free.con` is refused at the checker, natively and under `--interp`:

```
error[check]: (E0287) vec_free would drop the non-Copy elements of type 'String' still in the vector without destroying them
```

`repro_set.con` is refused (E0291) by bug 115's fix (landed with `lowering-bugs`, main `9757540e`),
which makes `vec_set` and `vec_get` Copy-only; this branch does not duplicate that check.

## Cause

`Concrete/Check/Check.lean`'s intercepts for the raw `vec_set` and `vec_free` intrinsics checked
argument shapes only. The `Vec` methods are the linear forms (`Vec::replace` returns the old
element; `Vec::drop` runs the element drop glue, bug 052), and std's `Vec::set` already requires
`T: Copy`; the raw intrinsics, callable from safe code, had no such rule.

## Fix

Containment at the intrinsic: `vec_free` of a `Vec<T>` with non-Copy `T` is E0287
(`vecFreeDropsElements`) outside trusted code. A trusted function may release a buffer it has
emptied (it vouches for that, as for any trusted operation); the builtin `Vec` has no other way to
release an emptied buffer in a single-file program without std. Fixture `error_145_vec_free_linear.con` (`run_tests.sh`); the positive control
`regress_145_vec_free_copy_param.con` and `regress_145_vec_free_nested_copy_param.con` (an
inferred `vec_new::<T>()` / `vec_new::<[T; 1]>()` with `T: Copy`). The `vec_set`
half: bug 115's fix (E0291, on main). Migration: 2 test files (`test_alloca_loop_stress.con`,
`test_string_literal_in_loop.con` empty a `Vec<String>` and then release it through a trusted
`free_emptied` helper).

## Structural cause

A documented rule with no enforcing site ([structural class](STRUCTURAL_CLASSES.md) 13): rule (c)
was enforced for source assignments and heap cells, not for the collection intrinsics that perform
the same store or drop.

## Structural owner

Containment: ROADMAP exit 0b. The structural home is R-0509 (S16, the trusted base: the raw
collection intrinsics should be reachable only through std's checked wrappers) with R-0499's one
Core checker.
