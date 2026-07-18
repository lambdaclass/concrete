# Bug 052: T_destroy synthesized as no-op for arrays — Vec<[T; N]>.drop() skips element destruction

**Status:** Open
**Discovered:** 2026-07-18, middle-end audit (reproduced:
`Vec<[Tracked; 2]>.drop()` where `Tracked.destroy` increments a cell —
expected 2 calls, compiled binary prints 0).

## Symptom

Std's contract (H18) is that `Vec<T>.drop/clear` destroy every live element
exactly once. For `T = [Tracked; 2]` (a non-Copy array of a Destroy type)
both destructors are silently skipped — the elements leak.

## Root cause

Chain of three reasonable pieces that compose into a hole:

1. `CheckHelpers.destroyable` accepts ANY unmatched type via catch-all
   (`Concrete/Check/CheckHelpers.lean:710`), so `T: Destroy` bounds accept
   non-Copy arrays.
2. Std's Vec drop glue calls `(ptr as &T).destroy()`, which Elab emits as a
   call to `T_destroy`.
3. Mono's `rewriteCallNames` maps `T` via `tyName` — which returns `""` for
   `.array`/`.ref`/`.ptrMut`/`.heap`/`.fn_` (`Concrete/Resolve/Shared.lean:103`)
   — so the call is never rewritten to the array's destructor, and Mono's
   `_destroy` fallback (`Mono.lean:442-453`) synthesizes an EMPTY no-op on
   the premise "no destroy fn ⇒ element is Copy". The premise is false for
   non-Copy arrays.

Same exposure for `Deque`/`BinaryHeap` Destroy impls of array elements and
other `tyName = ""` element types.

## Candidate fix

Either make `tyName` total for arrays (a mangled array form) so the call
resolves to the real element-destruction loop, or make `destroyable`
reject non-Copy arrays at the `T: Destroy` bound (fail closed with a
diagnostic until arrays get drop glue). Do NOT keep the silent no-op.
Regression: the `Vec<[Tracked; 2]>.drop()` cell-count program — compiled
prints 2 (or the program is rejected with a clear E-code).
