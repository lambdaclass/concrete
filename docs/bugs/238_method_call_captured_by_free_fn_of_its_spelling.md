# Bug 238 — a free function spelled like a method call's Core spelling captures the call

**Status:** FIXED (branch `wip/name-capture`)
**Found:** 2026-10-25, by the Codex review of bug 227's fix (R-0492 slice 6), reproduced on main
`70bcdeb6` and on the train-17 candidate `593dc100`.
**Severity:** silent wrong code. `p.get()` ran an unrelated free function, natively and under
`--interp` alike, so the differential check could not see it.

## Reproducer

`tests/programs/regress_238_free_fn_spelled_like_method_call.con`: `outer.a` and `outer.b` each
declare `struct P`; module `c` imports `outer.a.{P, mk}`, defines `fn outer_a_P_get(p: &P) -> i32
{ return 99; }` and returns `p.get()`, whose method returns 42. Before the fix the binary and
`--interp` returned 99; now both return 42.

Two more shapes of the same capture:
`regress_238_other_module_free_fn_takes_method_spelling.con` (the free function in another
top-level module `d`; before the fix the type-agreement backstop happened to refuse it, E0299,
because its signature disagreed; one that agreed would have run) and
`regress_238_parent_free_fn_takes_method_spelling.con` (the free function in the enclosing module,
which imports its child's `P`; before the fix it failed to build, bug 241).

## Cause

Core spells a direct call by the CALLER's spelling, and every consumer below Elab (Mono,
Lower/EmitSSA, the interpreter) resolves a callee by that spelling: a definition of that name
first, then the linker aliases. Bug 096 respells a type two modules declare by its module
(`outer_a_P`), so a call of its method from another module is spelled `outer_a_P_get`, and the
defining submodule's linker alias bridges that to the definition `a_outer_a_P_get`. The spelling
`outer_a_P_get` is also a name a user may give a free function; nothing reserved it or compared it
with the program's free functions (`qualifyCollidingTopLevelFns` and `checkRespelledFnNames`
compare definitions, and the call's spelling is no definition). So `c`'s own `outer_a_P_get` was
found first, by name.

## Fix

A Core direct call now carries the namespace it was resolved in (`Callee.direct name space`,
`CallSpace.free | .method`; bug 239), and Elab's last spelling pass, `spellMethodCallsApart`, finds
every method call whose spelling a free function of the program defines and respells it by the
method it names: the targets of its linker aliases that are method definitions
(`CFnDef.isMethod`), which must be exactly one, else E0299 ("Core cannot tell them apart"). Other
method calls keep their spelling, so no proof fingerprint over a method call moves.

Regression legs in `run_tests.sh`: the three fixtures above, `run_ok … 42` and `--interp` = 42.

## Structural cause

Class 1 ("declaration identity by spelled name, not a resolved `DefId`"): a call names its callee by
a spelling, and a free function and a method's caller-side spelling are one string namespace in
Core, so a lookup by spelling meets whichever definition holds the string.

## Structural owner

R-0492 (S5 `DefId`) and R-0548 (method tables keyed by `MethodKey`; the link symbol a derived
spelling of it). This fix makes Core keep the two namespaces apart where Elab can establish them;
the callee still travels by spelling until calls carry the definition's identity.
