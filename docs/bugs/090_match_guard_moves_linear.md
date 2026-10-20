# Bug 090: a match guard could move a linear value that a later arm moves again

**Status:** FIXED
**Found:** 2026-10-18, checker ownership hunt (probe `g1.con`).
**Severity:** critical. A program the checker accepted freed a heap value and then read and freed
it again (use after free, double free). Silent wrong code: the binary ran and printed garbage.

## Reproducer

```
struct B { v: Int }
fn take(h: Heap<B>) with(Std) -> Int { let v: Int = h.v; free(h); return v; }
fn main() with(Std) -> Int {
    let h: Heap<B> = alloc(B { v: 5 });
    let k: Int = 1;
    match k {
        1 if take(h) == 99 => { println(1); },
        _ => { println(take(h)); },
    }
    return 0;
}
```

On `64cfea38` this compiled, and the binary printed `8482603264`: the guard called `take(h)`,
which freed `h`; the guard was false, so matching fell through to `_`, which called `take(h)` on
the freed allocation. The same shape works with the arm's own payload binder
(`Option::Some { value } if eat(value) == 99 => …, Option::Some { value } => free(value)`): the
second arm binds the payload the failed guard already consumed.

## Cause

`Concrete/Check/Check.lean` checked each arm from the pre-match environment and checked the guard
inside that arm's environment, so a move in the guard was recorded as the arm's move. Each arm
then looked internally consistent (arm 1 moved `h` once, in its guard; arm 2 moved `h` once, in its
body), and the arms agreed on consumption. But a guard is not part of its arm's path: it runs before
the arm commits, and when it is false control continues to the next arm, which carries none of the
guard's state. The documented rule ("a guard consume is a consume", VALUE_FLOW_SPEC) described the
implementation, not a sound semantics.

## Fix

Guards are read-only. `checkGuard` checks the guard as a `bool` expression and compares the
environment before and after it; a non-Copy binding that was live before and is consumed after is
rejected with the new diagnostic **E0300** (`guardMovesLinear`), whether it is an outer binding or
the arm's payload binder. Reading through a borrow (`peek(&h)`) stays legal. All eight guard sites
(enum and value-pattern matches, every arm shape) go through it. OWNERSHIP_MODEL,
VALUE_FLOW_SPEC and PATTERN_ERGONOMICS now state the rule.

## Gate

`scripts/tests/run_tests.sh`: `error_090_guard_moves_linear.con` and
`error_090_guard_moves_binder.con` must fail with E0300 (both compiled on the previous compiler);
`regress_090_guard_reads.con` (guards that borrow, each arm moving once) must still compile and
return 42.

## Follow-up (Codex review of the landing batch)

The first fix compared the guard's final binding states, so a move hidden by a `defer` inside a guard
block (the binding ends `reserved`) or by reassigning the moved binding (it ends live) passed, and the
fallback arm freed the value again. The guard now freezes every non-Copy binding live at its start
(`TypeEnv.guardUids`), and `consumeVar` rejects moving one with E0300 at the move itself; the
final-state comparison also counts `reserved`. Fixtures `error_090_guard_defer_moves.con` and
`error_090_guard_move_then_reassign.con` (both compiled before).
