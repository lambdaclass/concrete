# Bug 094: a binder that shadowed a live linear binding consumed it too

**Status:** FIXED
**Found:** 2026-10-18, checker ownership hunt, while fixing bug 093.
**Severity:** high. A program the checker accepted leaked a linear value (no E0208), and the
checker's record of which binding was consumed was wrong, so any rule reading that record could be
fooled. No double free was observed: the masking ran in the leak direction.

## Reproducer

```
fn mk(k: Int) with(Alloc) -> Option<Heap<Int>> { return Option::Some { value: alloc(k) }; }
fn main() with(Std) -> Int {
    let value: Heap<Int> = alloc(5);
    match mk(7) {
        Option::Some { value } => { free(value); },
        Option::None => { free(value); },
    }
    return 0;
}
```

On `origin/main` (and with bug 093's fix) this compiled. On the `Some` path the arm frees its binder
and the outer `value` is never freed. The same held for a destructure binder
(`let value: R = ..; let P { value } = p; eat(value);`) and for a value-pattern binder
(`match b { a => free(a) }` over a live `a`).

## Cause

The checker environment is a list of `(name, VarInfo)`. Bug 093 gave each binding a `uid` and made
the scope-exit and merge checks match by it, but the per-binding state updates still marked every
entry with the name:

- `consumeVar` and `useVar` (`Concrete/Check/CheckHelpers.lean`) set the state of every entry named
  `name`, so consuming an arm binder `value` also consumed the shadowed outer `value`;
- reassignment (`x = ..` on a consumed binding) marked every `x` live again, which would revive a
  consumed outer binding;
- `defer` (reserve), the borrow-block freeze and unfreeze, and a `let r = &x` borrow source
  (`borrowedFrom`) were keyed by name the same way.

`let` refuses to shadow a still-live non-Copy binding (H16, E0292), but match-arm binders (enum
payload and value patterns) and struct destructure binders do not check it, so a live outer binding
could be shadowed and then consumed through the inner name.

## Fix

- `consumeVar`, `useVar`, reassignment, `defer`, and the borrow-block freeze and unfreeze update only
  the binding the name resolves to, by `uid`; a `let` borrow records its source on the binding just
  added. The reproducer is now E0209 (the arms disagree on the outer `value`).
- A match-arm binder is arm-scoped: after the match the outer binding is visible again with its own
  obligation, so an arm binder may shadow a live binding (`match v.byte(0) { Some { value } => .. }`
  over a live `value` is valid; `tests/regressions/owner_bound` relies on it).
- A struct destructure binder stays in scope for the rest of the block, so it calls the same
  `checkNoLiveShadow` as `let` (E0292). Shadowing a consumed binding stays legal.

## Gate

`scripts/tests/run_tests.sh`: `error_094_arm_binder_shadows_live.con`,
`error_094_destructure_shadows_live.con` and `error_094_value_binder_shadows_live.con` must fail
with E0209, E0292 and E0208 respectively (all three compiled on the previous compiler);
`regress_094_shadowing_binders.con` (binders over consumed bindings, and arm binders over a live
binding consumed after the match) and `regress_094_shadow_state_by_identity.con` must compile and
return 42. The first positive control is rejected with a false E0209 if `consumeVar` reverts to
marking every same-named entry; the second with a false E0208 if reassignment or `useVar` does.
