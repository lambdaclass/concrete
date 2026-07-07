# Resource Cleanup: `defer`

Status: CORE-COMPLETE — ROADMAP Phase 6 #7. The `defer <call>;` form and its
cleanup semantics are implemented and gated (`scripts/tests/check_defer.sh`,
`examples/defer/cleanup_order/`); the open edges (block-form body, failure during
cleanup, move-after-defer) are documented limitations below.
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
  early `return`, on `break`/`continue` out of the scope, and on `?`/`Err`
  propagation. (`Concrete/IR/Lower.lean`: `emitFrameDeferredCalls` at scope pop,
  `emitAllDeferredCalls` on return/Err, `emitDeferredUntilLoop` on break/continue.)
- **Per-scope.** Each block scope has its own deferred list; exiting a block runs
  that block's defers, inner-to-outer.

## V1 boundaries (documented limitations)

- **Body must be a call.** `defer f();` / `defer obj.method();` are accepted; a
  block body `defer { … }` is rejected (parse error) — the defer body is a single
  call expression (the checker enforces `deferBodyNotCall`). Wrap multiple cleanup
  steps in a named cleanup function and `defer` that.
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
  diverges (especially mid-unwind on an error path). Today deferred calls are
  ordinary calls with no special failure handling.
- **Move-after-defer / linear interaction** — `defer drop(x)` followed by moving
  `x` should be a linearity error; the precise rules (defer "uses" the value at
  the defer point vs. at scope exit) are not yet specified or gated.
- **Automatic drop/cleanup ordering vs. `defer`** — how explicit `defer`
  composes with implicit linear-value drop at scope exit.

These compose with the linear/borrow model (`docs/VALUE_MODEL.md`); revisit when
a workload exercises cleanup on error paths or `defer` over linear resources.
