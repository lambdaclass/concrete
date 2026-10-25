# Bug 239 — a free function named like a method's definition name answers the method call

**Status:** FIXED (branch `wip/name-capture`)
**Found:** 2026-10-25, by the Codex review of bug 227's fix (R-0492 slice 6), reproduced on main
`70bcdeb6` and on the train-17 candidate `593dc100`.
**Severity:** silent wrong code. `p.get()` was checked, compiled and interpreted as a call of the
module's free function `P_get`, by value, without the receiver borrow.

## Reproducer

- `tests/programs/error_239_method_call_answered_by_free_fn.con`: `defs` declares `P`, a third
  module `ext` declares `impl P { fn get }`, and `user` imports `P`, defines `fn P_get(p: P) -> i32
  { return 99; }` and returns `p.get()`. Before the fix the binary and `--interp` returned 99. The
  same program without `user`'s free function is refused E0264 ("no method 'get' on type 'P'": the
  impl in `ext` does not reach `user`), and so is this one now.
- `tests/programs/regress_239_free_fn_beside_imported_method.con`: `P`'s method declared by its own
  module, so it reaches `user`; `user` returns `p.get() + P_get(p) - 99`. Before: 99 (both calls ran
  the free function); now 42, natively and under `--interp`.
- `tests/programs/error_239_free_fn_named_like_own_method.con`: `fn P_get` beside `impl P { fn get
  }` in one module. Before: the native build failed on a redefined LLVM symbol and `--interp`
  refused the call; now E0299 at Check (MOD-3).

## Cause

A method's definition name is `Type_method`, and Check's function table (`fnNames`), Elab's
signature table (`fnSigs`) and the shared argument-type peek kept free functions and impl methods
in one list under those spellings, free functions first. A receiver call `x.m()` and a `T::m()`
call looked the method up by `T_m` in that list, so a free function spelled `T_m` answered them.
Elab then emitted a direct call spelled `P_get`, and the renaming that respells a top-level free
function colliding with another module's method (`qualifyCollidingTopLevelFns`, bug 097:
`user_P_get`) renamed every call of that spelling, the method call included. In one module the
two definitions kept one Core name. Bug 082 had already put a module's method definition names in
its callable namespace (an import aliased to `S_value` beside `impl S { fn value }` is E0299), but
the declaration-collision check compared only free functions, externs and constants.

## Fix

- Check and Elab look a method up in a method table alone (`TypeEnv.methodNames` /
  `lookupMethod`; `ElabEnv.methodSigs` / `lookupMethodSig`; `PeekCtx.methodSig`); the method call's
  callable identity comes from the method resolver (`resolveMethodCallee`). A `T::m()` call tries
  the method table first and keeps the free lookup only for a head that has no such method (an
  upper-case module's function). A free-spelled name keeps today's resolution: free functions
  first, then a method by its definition name (a method used as a value,
  `regress_096_method_value_same_named`).
- A Core direct call carries its namespace (`CallSpace`), and every Elab renaming renames each
  namespace by its own table: submodule prefixing (`FnRenames`), the parent and sibling
  cross-module renames, type respelling (`CoreRename.methodName`) and the top-level collision
  respelling, which now renames free functions only, so a method call keeps its spelling there.
  `CFnDef.isMethod` marks a method's definition.
- `declarationCollisions` counts a module's impl method definition names, so a free function or
  extern spelled like one is E0299 (MOD-3); two methods of one name on one type stay TRAIT-5's.

Regression legs in `run_tests.sh`: the E0264 and E0299 refusals, and the positive control
`run_ok … 42` with `--interp` = 42.

## Structural cause

Class 1 ("declaration identity by spelled name, not a resolved `DefId`"): free functions and impl
methods shared one spelled namespace in Check, Elab and Core, and a method was found by the string
`Type_method`, so whichever definition of that string a lookup met first answered.

## Structural owner

R-0492 (S5 `DefId`) and R-0548 (method tables keyed by the typed `MethodKey`). This fix separates
the method namespace in the tables and in Core's callee; the tables are still keyed by the
`Type_method` spelling until R-0548.
