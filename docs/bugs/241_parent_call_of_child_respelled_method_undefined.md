# Bug 241 — a parent calling its submodule's respelled type's method names an undefined function

<!-- bug-ledger
summary: a parent module calling a method of its submodule's respelled type had the call renamed `<sub>_<Type>_<method>` by the prefix rename before the type respelling could reach it, naming a function nothing defines (undefined LLVM symbol; `--interp`: undefined function). Found reproducing bug 238, 2026-10-25. Fixtures `regress_241_*`. Class 1.
fixtures: regress_241_parent_calls_child_respelled_method.con regress_241_parent_names_child_respelled_method.con
class: 1 — a method call renamed by two passes that each recompute its definition's spelling
silent: no
ruling: Violates: MOD-1@1
-->

**Status:** FIXED (branch `wip/name-capture`)
**Found:** 2026-10-25, while reproducing bug 238 (its enclosing-module shape), on the train-17
candidate `593dc100`.
**Severity:** loud. The native build failed on an undefined LLVM symbol and `--interp` stopped with
"undefined function 'a_P_get'".
**Confirmed real by Codex review, 2026-10-25:** the parent's cross-module rename changes `P_get` to `a_P_get` before type respelling can preserve the imported method's identity, leaving a valid method call targeting an undefined symbol.

## Reproducer

`tests/programs/regress_241_parent_calls_child_respelled_method.con`: `outer.a` and `outer.b` each
declare `struct P`; `outer` imports `outer.a.{P, mk}` and calls `p.get()`. Expected and now
observed: 42, natively and under `--interp`. `regress_241_parent_names_child_respelled_method.con`
names the method by its definition name, as a call `P_get(&p)` and as a value, with the same
failure before the fix (`--interp`: "undefined variable 'a_P_get'").

## Cause

Bug 096 spells `a`'s `P` as `outer_a_P` in Core and defines its method as `a_outer_a_P_get`; a call
from another module is spelled `outer_a_P_get` (the type respelling) and `a`'s linker alias bridges
it. In the parent the cross-module rename ran first: it rewrites a bare submodule function or
method name to `<sub>_<name>` (`P_get` → `a_P_get`), the spelling of an unrespelled method, so the
type respelling no longer recognised the call and the call named a function nothing defines.

## Fix

The parent's cross-module rename leaves a reference to a submodule's respelled type's method
alone (`respelledSubMethods`); the type respelling then spells it `outer_a_P_get`, as for any other
importer. The rename is per namespace now (bug 239), and a free-spelled reference that resolves to
the method is in the method namespace, so a call, a `T::m()` call, a call by definition name and a
method value are all covered.

## Structural cause

Class 1 ("declaration identity by spelled name, not a resolved `DefId`"): a method call is renamed
by two passes that each recompute its definition's spelling from a different table, and the first
pass's answer hid the call from the second.

## Structural owner

R-0492 (S5 `DefId`) and R-0548 (`MethodKey`): a method call resolved once to its definition needs
no renaming chain.
