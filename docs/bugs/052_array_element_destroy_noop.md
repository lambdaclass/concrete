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

## Follow-up: Codex review of e097c7dd

* **Destructor identity was a string.** Elab spelled `x.destroy()` on a `T: Destroy`
  receiver `T_destroy`, the ordinary mangled-method spelling, so a user function literally
  named `T_destroy(x: &[Tracked; 2])` was found by Mono's (and the interpreter's) ordinary
  lookup before any synthesis ran, and the array's element destructors were silently
  skipped. Trait-method calls on a type parameter are now spelled `T$m`
  (`traitParamCallee`; `$` is not an identifier character). Mono rewrites `T$destroy` per
  instantiation to the reserved typed dispatch `$destroy`, decided from the argument type:
  the type's `Destroy` impl method, element-wise array glue, or a Copy no-op; anything else
  is E0814. Synthesized glue is named `$destroy$<type>`, which no definition can spell. The
  old `T_` prefix rewrite (which also renamed a user's own `T_helper` call inside a generic
  body) is gone. The interpreter dispatches on the reserved spelling, never a `_destroy`
  suffix. Array glue carries `Alloc`, the builtin `Destroy` bound (bug 091).
* **Nested explicit type arguments.** `checkTraitBounds` normalized only a top-level
  `.named "T"`, so `kill::<[T; 2]>(x)` inside `outer<T: Destroy>` was E0241. The supplied
  type is now normalized recursively.
* **Copy newtypes.** Mono's Copy judgment had no newtypes, while function-body types keep
  newtype names, so `kill::<Meters>(&m)` with `newtype Meters = Int` hit E0814. Mono now
  feeds newtypes into the shared judgment (a newtype is Copy iff its inner type is, generic
  newtypes substituted like any aggregate).
* **Reinterpreting pointer casts in the interpreter.** A reference-to-pointer cast kept the
  reference's path regardless of pointee type, so `&mut n as *mut u8` over a `u64` and a
  store of `3` replaced the whole interpreted value (3) where compiled code changed one byte
  (259). The interpreter now refuses a pointer cast whose pointee type changes, with a clear
  runtime error.

Fixtures: `review_forms.con` (compiled and `--interp`: `2 4 4 0 6`; previous compiler
E0241, and 0 destructions with the user `T_destroy`) and `reinterpret_cast.con` (compiled
259; `--interp` refuses; previous interpreter printed 3), both in
`check_collections_copy_only.sh`. `single.con`/`refuse_unresolved.con` now declare
`with(Alloc)` on `kill`, as bug 091 requires.

## Codex round 2

* **Any trait method named `destroy` became builtin Destroy (P1).** Mono rewrote every
  `T$destroy` to typed destructor dispatch by the method NAME; a `Reset { fn destroy(&self); }`
  impl on a Copy type was replaced by the trivial no-op glue. Elab, which knows the resolved
  trait, now emits `destroyDispatchCallee` only for the builtin `Destroy` trait
  (`builtinId == .destroy`); Mono and ProofCore's free-call classification key on that callee,
  and every other `T$m` is an ordinary method call. Fixture
  `tests/regressions/bug052_vec_array_destroy/unrelated_destroy_trait.con` (7 1, compiled and
  --interp; old compiled printed only 1).
* **The interpreter selected destructors by name (P2).** `destroyValue` called
  `findFn (n ++ "_destroy")`: an unrelated `P_destroy` function ran for a Copy `P` with no
  Destroy impl, and a nested module's `sub_Type_destroy` was never found. The interpreter now
  builds a trait-impl table from the impls' own records (`CTraitImpl.implMethodFnName`,
  reserved `$impl$...` keys) and destroys through it; the same table runs non-Destroy `T$m`
  calls on a type parameter (dispatched on the runtime receiver's type), which the
  interpreter previously rejected as undefined functions. Fixture
  `interp_destroy_table.con` (1 2, compiled and --interp; old --interp printed 999, 0, 1).
  Both legs are in `check_collections_copy_only.sh`. Mono's dispatch also names the RESOLVED
  destructor definition now (bug 091 round 2).

## Codex round 3

* **Name collisions still selected another type's impl (P1).** Core names types by their bare
  name. Mono's destructor dispatch explicitly preferred a top-level `Tracked_destroy` over a
  nested type's, and the interpreter keyed impls by bare type name with top-level impls first:
  with a top-level `Tracked` (+100) and a nested `Tracked` (+1), generic destruction of the
  nested value printed 101 instead of 2, compiled and --interp. Ordinary trait dispatch had
  the same shape (`T$score` re-spelled `P_score`, which the alias pool answered with the
  top-level impl: 100 instead of 1). Until the defining module survives into Core and runtime
  values, every ambiguous candidate set is refused, a top-level definition included: a type
  name defined by more than one module (new `CModule.ownTypeNames`), or more than one impl
  definition for (type, method). Mono refuses destructor dispatch with E0814 and trait-method
  dispatch with E0815 (Mono now resolves `T$m` through the trait-impl table, never by
  re-spelling `Type_m`); the interpreter registers the ambiguous key and fails at dispatch.
  std's private vec test helper `Tracked` is now `VecDropTracked`, so a user `Tracked` beside
  std's `Vec` is not ambiguous. Fixtures `collision_destroy.con` and `collision_trait.con`
  (both refused compiled and --interp; before: 101 and 100 100) in
  `check_collections_copy_only.sh` and `run_tests.sh`.

