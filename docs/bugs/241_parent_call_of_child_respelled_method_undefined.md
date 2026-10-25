# Bug 241 — a parent calling its submodule's respelled type's method names an undefined function

**Status:** FIXED (branch `wip/name-capture`)
**Found:** 2026-10-25, while reproducing bug 238 (its enclosing-module shape), on the train-17
candidate `593dc100`.
**Severity:** loud. The native build failed on an undefined LLVM symbol and `--interp` stopped with
"undefined function 'a_P_get'".

## Reproducer

`tests/programs/regress_241_parent_calls_child_respelled_method.con`: `outer.a` and `outer.b` each
declare `struct P`; `outer` imports `outer.a.{P, mk}` and calls `p.get()`. Expected and now
observed: 42, natively and under `--interp`.

## Cause

Bug 096 spells `a`'s `P` as `outer_a_P` in Core and defines its method as `a_outer_a_P_get`; a call
from another module is spelled `outer_a_P_get` (the type respelling) and `a`'s linker alias bridges
it. In the parent the cross-module rename ran first: it rewrites a bare submodule function or
method name to `<sub>_<name>` (`P_get` → `a_P_get`), the spelling of an unrespelled method, so the
type respelling no longer recognised the call and the call named a function nothing defines.

## Fix

The parent's cross-module rename leaves a call of a submodule's respelled type's method alone
(`respelledSubMethods`); the type respelling then spells it `outer_a_P_get`, as for any other
importer. The rename is per namespace now (bug 239), so it touches method calls only.

## Structural cause

Class 1 ("declaration identity by spelled name, not a resolved `DefId`"): a method call is renamed
by two passes that each recompute its definition's spelling from a different table, and the first
pass's answer hid the call from the second.

## Structural owner

R-0492 (S5 `DefId`) and R-0548 (`MethodKey`): a method call resolved once to its definition needs
no renaming chain.
