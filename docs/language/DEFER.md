# Resource Cleanup: `defer`

Status: CORE-COMPLETE — ROADMAP old Phase 6 #7. The `defer <call>;` form and its
cleanup semantics are implemented and gated (`scripts/tests/check_defer.sh`,
`examples/defer/cleanup_order/`); the open edges (block-form body, failure during
cleanup, and normal-call argument coercions) are documented below.
Date: 2026-06-23

## What `defer` does

`defer <call>;` schedules a function call to run when the enclosing scope exits.

```
fn process() with(Std) {
    let h = open();
    defer close(h);          // runs when process() returns, on any path
    work(h);
    // close(h) runs here automatically
}
```

Implemented semantics (locked by the gate):

- **LIFO order.** Within a scope, deferred calls run in reverse registration
  order — the last `defer` runs first. (`defer a(); defer b();` runs `b` then
  `a`.)
- **Runs on every exit path.** Deferred calls run on normal fall-through, on
  early `return` (including an explicit `Result::Err` return), and on
  `break`/`continue` out of the scope. (`Concrete/IR/Lower.lean`:
  `emitFrameDeferredCalls` at scope pop, `emitAllDeferredCalls` on return,
  `emitDeferredUntilLoop` on break/continue.)
- **Per-scope.** Each block scope has its own deferred list; exiting a block runs
  that block's defers, inner-to-outer.
- **Linear arguments are reserved at registration.** Once `defer drop(x)` is
  accepted, `x` is in the `reserved` state and cannot be moved or consumed by a
  later statement. Move-after-defer is rejected with E0206 (`reserved by
  defer`), pinned by `error_defer_move.con`,
  `error_memory_edge_defer_then_move.con`, and
  `pressure_err_defer_then_move.con`.
- **Arguments are reserved by place (normative, decided 2026-10-21; [bug
  143](../bugs/143_defer_borrow_argument_unpinned.md)).** A deferred call's arguments are local
  names, shared or exclusive borrows of local names (`&x`, `&mut x`), or constants (a literal, a
  module constant, a function name). A projection (`defer f(t.id)`, `defer f(&t.field)`), a nested
  call (`defer f(g(x))`) or a string literal is refused (E0279): it would be evaluated at the exit.
  Write a named cleanup function that takes the local, or a borrow of it, and computes inside.
  - A local passed **by value** is **reserved** (pending consumption): the deferred call
    discharges its obligation when the block ends, and nothing may move or consume it before
    (E0206). Ownership is *not* taken at registration: the reserved local may still be read,
    borrowed and mutated in place (`let f = open(); defer close(f); read(&f);`).
  - A local passed **by borrow** is **pinned**: the borrow discharges nothing, so the local must
    still be consumed, and nothing may move or consume it before the deferred call runs (E0206,
    "borrowed by a pending defer"), a later `defer` included. A deferred observer therefore goes
    *after* the deferred consumer in source (`defer kill(t); defer peek(&t);` runs `peek` first, by
    LIFO); `defer peek(&t); defer kill(t);` is refused, and `defer peek(&t);` alone leaves `t`
    unconsumed (E0208).
  - When the registering block ends, the deferred calls run, the reserved locals are consumed
    (a later use of one is E0205) and the pins end.
  - The call reads its argument places **at the exit**: a `Copy` argument sees the value the local
    holds then (`let mut x = 1; defer show(x); x = 2;` prints 2). A registration-time value is
    spelled `let x0 = x; defer show(x0);`.
  - Arguments are bound by **binding identity**, never re-resolved by name at the exit: a later
    `let` of the same name is a different binding (Elab gives a shadowing binder a fresh Core
    name, [bug 087](../bugs/087_block_let_shadowing_not_lexical.md)), so `let x = 1; defer show(x);
    let x = 5;` prints 1 at the exit, in the binary and `--interp` alike.

## V1 boundaries (documented limitations)

- **Body must be a function call.** `defer f(x);` is accepted. A method call
  `defer obj.method();` is **not** (E0282, `deferBodyNotCall`; an earlier version of
  this document claimed it was, EVALUATION2 D7): write the free-function form the
  method desugars to, or a named cleanup function. A block body `defer { … }` is a
  parse error. Wrap multiple cleanup steps in a named cleanup function and `defer`
  that.
- **No argument coercion in the deferred call.** A deferred call does not apply
  the literal/auto-borrow coercions a normal call site does (e.g. `defer
  println("x")` fails where `println("x")` compiles, because the `&String`
  argument coercion is not applied in defer position). Defer a no-argument
  cleanup function, or pass already-typed values. (Aligning defer-call argument
  coercion with normal calls is a follow-up.)

## Deferred design (routed)

The deeper cleanup-semantics questions from ROADMAP #7 are recorded here rather
than built, as they need their own design pass:

- **Failure during cleanup** — what happens if a deferred call itself fails or
  diverges while handling an ordinary return/error path. Today deferred calls
  are ordinary calls with no special recovery or continuation rule.

These compose with the linear/borrow model (`docs/language/VALUE_MODEL.md`); revisit when
a workload exercises cleanup on error paths or `defer` over linear resources.

There is no automatic scope-end drop to order against `defer`. Concrete has no
implicit destruction: explicit `destroy(x)`, `x.drop()`, or a deferred explicit
call consumes the value. Abort remains terminal and runs none of them.
