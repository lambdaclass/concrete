# Bug 052: T_destroy synthesized as no-op for arrays — Vec<[T; N]>.drop() skips element destruction

**Status:** FIXED 2026-10-18 (arrays of destroyable elements get real glue; every
other shape that has no destructor is refused at compile time)
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

## Fix (2026-10-18)

The destructor is now decided from the ARGUMENT TYPE of the `_destroy` call,
never from a name lookup that missed:

1. **Mono** (`Concrete/IR/Mono.lean`, the `_destroy` branch of `monoExpr`):
   when no `{Type}_destroy` definition exists,
   - an array `[E; N]` of a non-Copy `E` gets synthesized glue that walks
     `i = 0..N` and calls `E`'s destroy on `&x0[i]` — exactly once per
     element, in index order. Nested arrays recurse through the same branch;
     a generic element (`[Vec<T>; N]`) resolves to that type's own glue. The
     glue is named by the full argument type (`Ref_T_Arr2_T_Tracked_E_E_destroy`),
     so distinct instantiations never share a symbol;
   - a Copy type still gets the empty no-op (trivial destruction);
   - an UNRESOLVED type (`.placeholder` / `.typeVar`) is refused with
     **E0814** — the shared Copy judgment calls a placeholder Copy, so it would
     otherwise have been given the no-op;
   - any other non-Copy type with no destructor is refused with **E0814**.
2. **Check** (`CheckHelpers.checkTraitBounds`, `destroyable`): the catch-all
   that accepted every unmatched type is gone. `[E; N]` is destroyable iff `E`
   is; `String` iff it has a `Destroy` impl; `Heap<T>`, `HeapArray<T>` and
   `&mut T` are not destroyable. The E0241 message now names the type
   (`'[Res; 2]'`, `'Heap<i64>'`) instead of `<type>`.
3. **Interp** (`destroyValue`): the pre-mono interpreter destroyed arrays as a
   no-op too; it now walks the elements (element refs extend the borrow path)
   and runs each element's destructor. `&mut x as *mut T` now evaluates (the
   pointer keeps the reference's path) so a counting destructor can run under
   `--interp`.

## Refused shapes (compile-time, fail closed)

- `T: Destroy` with `T = [E; N]` where `E` has no destructor, `Heap<T>`,
  `HeapArray<T>`, `&mut T`, or `String` without its `Destroy` impl → **E0241**.
- A `_destroy` call whose argument type is unresolved after Mono → **E0814**.
  Reachable today: Elab infers a generic call's type arguments from a shallow
  argument-type peek that does not see through a field access, so
  `kill(&self.items)` instantiates `T = Placeholder`. Write the turbofish
  (`kill::<[Tracked; 2]>(&self.items)`). That inference gap is a separate,
  wider defect (it affects any generic call whose argument is a field access)
  and is not fixed here.
- A `_destroy` call with an argument count other than one → **E0814**
  (compiler containment; no source program produces it).

## Regression

`tests/regressions/bug052_vec_array_destroy/`, asserted by
`scripts/tests/check_collections_copy_only.sh` (COLLECTIONS-DROP-GLUE, six
bug 052 legs):

- `project/` — Vec<[Tracked; 2]> drop and clear, Vec<[[Tracked; 2]; 3]>,
  Vec<[Vec<Tracked>; 2]> and Deque<[Tracked; 2]> print cumulative counts
  `4 8 14 17 21` (the pre-fix compiler prints `0 0 0 0 0`); the emitted array
  glue must call `@Tracked_destroy` and no literal `@T_destroy` may exist;
- `single.con` — a Destroy-bounded generic destroying `[Tracked; 2]` and
  `[[Tracked; 2]; 3]`: compiled and `--interp` both print `2 8 9` (pre-fix:
  compiled `0 0 1`, interp failed);
- `refuse_nondestroy/` — `Vec<[Res; 2]>.drop()` is E0241 naming `[Res; 2]`
  (negative control; pre-fix it compiled);
- `refuse_unresolved.con` — the Placeholder shape above is E0814 (negative
  control; pre-fix it compiled to a no-op).

All six legs fail against the pre-fix compiler.
