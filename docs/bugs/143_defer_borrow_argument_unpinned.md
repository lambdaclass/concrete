# Bug 143 — a deferred call's borrowed or projected argument reserved nothing (use after free)

<!-- bug-ledger
summary: a deferred call's borrowed, projected or computed argument reserved nothing, so `defer pb(&h); free(h);` read a freed heap cell at the exit and `defer peek(&t); defer kill(t);` read a consumed value (EVALUATION2 D5). Defer rule (a), reserve by place: arguments are locals, borrows of locals or constants (E0279); a borrowed local is pinned (E0206). D6 (by-name re-resolution) was already closed by bug 087. `error_143_*.con`, `regress_143_defer_reserve_by_place.con`. Joins exit 0b. ((`safety-eval2`))
fixtures: error_143_defer_borrow_then_free.con error_143_defer_borrow_then_consume.con error_143_defer_observer_before_consumer.con error_143_defer_field_arg.con error_143_defer_call_arg.con error_143_defer_then_use_after_block.con error_143_defer_borrow_only.con error_143_defer_last_in_match_arm.con regress_143_defer_reserve_by_place.con error_143_defer_shadow_refused.con regress_143_defer_sibling_scopes.con
class: 5 — a deferred call's borrow reserved nothing
class: 15 — a pending deferred call was not part of the state
silent: yes
exit: 0b
-->

**Status:** FIXED 2026-10-21 (branch `safety-eval2`)
**Found:** 2026-10-21, by the second semantics evaluation (EVALUATION2 D5; probes `d04`, `d06`,
`d07`, `d11`, `d16`, `d18`, `d19`, `d23`), not filed then.
**Severity:** use after free in safe code (`d19` printed a freed heap cell's bytes, `d18` a freed
`String`'s), and reads of consumed values at the exit (`d04`, `d06`, `d23`).

## Reproducers

`tests/regressions/bug143_defer_borrow_unpinned/repro.con`:

```con
fn main() with(Console, Alloc) -> Int {
    let h: Heap<B> = alloc(B { v: 77 });
    defer pb(&h);
    free(h);
    let g: Heap<B> = alloc(B { v: 99 });
    free(g);
    return 0;
}
```

On main (`5a064e77`): accepted; at the exit `pb(&h)` reads the freed cell (the evaluation's run
printed `8482603264`; the same program without `free(h)` prints 77). `repro_order.con`
(`defer peek(&t); defer kill(t);`): accepted; `kill` runs first by LIFO, so `peek` reads a consumed
value.

Fixed: both refused at the checker, natively and under `--interp`:

```
error[check]: (E0206) variable 'h' is borrowed by a pending defer: it may not be moved or consumed before the deferred call runs at the end of the block
```

## Cause

`Concrete/Check/Check.lean` `.defer` reserved only an argument that was a bare identifier the
argument check had consumed. Every other argument form (a borrow `&t`, a field `t.id`, a nested
call) reserved nothing, so the local stayed free to be consumed before the deferred call ran, and
`Lower`/the interpreter evaluated the stored argument expressions at the exit.

## Fix

Defer rule (a), "reserve by place" (user decision, 2026-10-21; normative text in
[DEFER.md](../language/DEFER.md) and MEMORY_SEMANTICS §9):

- A deferred call's arguments are local names, borrows of local names (`&x`, `&mut x`) or constants
  (literals, a module constant, a function name). A projection, a nested call or a string literal is
  E0279 (`deferArgNotPlace`): it would be evaluated at the exit.
- A local passed by value is reserved (unchanged); a local passed by borrow is **pinned**
  (`VarInfo.pinnedByDefer`): nothing may move or consume it until the deferred call runs (E0206,
  `variablePinnedByDefer`, in `consumeVar`), a later `defer` included. The borrow discharges
  nothing, so a borrow-only `defer` still leaves the local to be consumed (E0208, `d24`), and an
  observer goes after its consumer in source (`defer kill(t); defer peek(&t);`, `d22`, accepted).
- When the registering block ends (`checkStmts`; for a value block, an `if`-expression branch or a
  match arm, after its trailing value: `deferScopeEntry`/`deferScopeExit`), its reservations become
  consumptions and its pins end, so a reserved local read after the block that ran its deferred
  consumer is E0205. (Before the fix, and in the first version of it, a `defer` as a match arm's
  last statement left its local reserved past the arm, so a later read of the consumed value was
  accepted; Codex review.)
- Ownership is not taken at registration: reserved and pinned locals may still be read, borrowed
  and mutated in place, which the 61 corpus sites that use a resource after deferring its cleanup
  need. A `Copy` argument is read at the exit.

D6 (deferred calls re-resolved by name at the exit: a later shadowing `let` was read, and an inner
block's shadow natively but not in `--interp`) was already closed on main by bug 087's
alpha-renaming: Elab gives a shadowing binder a fresh Core name, so the stored deferred call names
the binding it was registered with. `d08` and `d09` now print `5 1` and `9 1` in both engines; the
positive control below pins it.

Fixtures (`run_tests.sh`): `error_143_defer_borrow_then_free.con`,
`error_143_defer_borrow_then_consume.con`, `error_143_defer_observer_before_consumer.con`,
`error_143_defer_field_arg.con`, `error_143_defer_call_arg.con`,
`error_143_defer_then_use_after_block.con`, `error_143_defer_last_in_match_arm.con`,
`error_143_defer_borrow_only.con` (the `d24` control),
and `regress_143_defer_reserve_by_place.con` (reserved and pinned locals mutated in place, observer
after consumer, exit-time `Copy` read, a shadowing `let`, a pin ending with its block; native and
`--interp` print `7 1 4 2 2 5 5 5`). An interpreter leg asserts both reproducers are refused by both
engines.

Migration: 0 sites (every corpus `defer` passes bare identifiers, none borrows then consumes).

## Structural cause

Borrow tracking by syntax instead of normalized places ([structural class](STRUCTURAL_CLASSES.md) 5):
the reservation recognized one argument spelling. Ownership state not modeled along control-flow
paths (class 15): a pending deferred action was not part of the state, so a consumption before it
ran was invisible.

## Structural owner

Containment: ROADMAP exit 0b. The normative rule: R-0500 (exit 2j, evaluation order and
reservation). Place MIR's cleanup edges (R-0501) are the static expansion of `defer` and need only
reads of places at the exit; binding identity is R-0492 (LocalId).
