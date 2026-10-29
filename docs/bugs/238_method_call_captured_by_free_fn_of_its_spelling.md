# Bug 238 — a free function spelled like a method call's Core spelling captures the call

<!-- bug-ledger
summary: a free function, extern or alias spelled like a method call's Core spelling (`outer_a_P_get` for a respelled type's method) captured the call, natively and under `--interp` (99 for 42): every consumer below Elab resolved a callee by spelling. Core references carry their namespace (`CallSpace`), method references resolve among methods only, and `spellMethodCallsApart` respells a method reference a free definition takes. Found by the Codex review of bug 227, 2026-10-25. Fixtures `regress_238_*`. Class 1.
fixtures: regress_238_free_fn_spelled_like_method_call.con regress_238_other_module_free_fn_takes_method_spelling.con regress_238_parent_free_fn_takes_method_spelling.con regress_238_method_named_by_definition_name.con regress_238_method_call_in_constant.con regress_238_extern_takes_method_spelling.con regress_238_imported_extern_alias_takes_method_spelling.con regress_238_method_definition_name_taken_by_extern.con regress_238_method_value_spelled_like_extern.con regress_238_method_value_beside_unrelated_extern.con regress_238_method_value_beside_extern_of_other_arity.con regress_238_method_value_both_spellings_taken_by_externs.con regress_238_free_alias_takes_method_spelling.con regress_238_extern_alias_takes_nested_method_spelling.con regress_238_unrelated_alias_beside_method.con
class: 1 — a method call's caller-side spelling met a free function (or extern, constant, alias) defined under it; fixed 2026-10-26: Core references carry their namespace (`CallSpace`) and method references resolve among methods only
silent: yes
exit: 0b
ruling: Violates: MOD-1@1
-->

**Status:** FIXED (branch `wip/name-capture`)
**Found:** 2026-10-25, by the Codex review of bug 227's fix (R-0492 slice 6), reproduced on main
`70bcdeb6` and on the train-17 candidate `593dc100`.
**Severity:** silent wrong code. `p.get()` ran an unrelated free function, natively and under
`--interp` alike, so the differential check could not see it.
**Confirmed real by Codex review, 2026-10-25:** the base's spelling-based lookup lets an unrelated free function capture the resolved method's caller-side name, violating MOD-1 and the declaration-identity invariant.

## Reproducer

`tests/programs/regress_238_free_fn_spelled_like_method_call.con`: `outer.a` and `outer.b` each
declare `struct P`; module `c` imports `outer.a.{P, mk}`, defines `fn outer_a_P_get(p: &P) -> i32
{ return 99; }` and returns `p.get()`, whose method returns 42. Before the fix the binary and
`--interp` returned 99; now both return 42.

More shapes of the same capture:
`regress_238_other_module_free_fn_takes_method_spelling.con` (the free function in another
top-level module `d`; before the fix the type-agreement backstop happened to refuse it, E0299,
because its signature disagreed; one that agreed would have run) and
`regress_238_parent_free_fn_takes_method_spelling.con` (the free function in the enclosing module,
which imports its child's `P`; before the fix it failed to build, bug 241), and
`regress_238_method_named_by_definition_name.con` (the method named by its definition name, as a
call `P_get(&p)` and as a value `let f: fn(&P) -> i32 = P_get;`, which Core respells
`outer_a_P_get` too; before the fix both ran the free function, 198 for 42),
`regress_238_method_call_in_constant.con` (the method call in a constant's initializer, which
Lower inlines at each use; before: 99) and `regress_238_extern_takes_method_spelling.con` (a
foreign binding `#[link_name = "abs"] extern fn outer_a_P_get`, which consumers bind before any
program definition; before: the method call ran `abs`, 99). Codex reviews of this fix found these
two and three more foreign-binding shapes:
`regress_238_imported_extern_alias_takes_method_spelling.con` (an extern imported under the
alias `P_get`, the definition name of the imported `P`'s method; before: 99),
`regress_238_method_definition_name_taken_by_extern.con` (foreign bindings under both the call's
spelling and the method's definition name; before: 99) and
`regress_238_method_value_spelled_like_extern.con` (a method value spelled like a foreign binding;
before: refused as a foreign function value, E0588).

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

A Core reference to a function now carries the namespace it was resolved in (bug 239): a direct
call `Callee.direct name space`, a function value `CLocal.space`, with `CallSpace.free | .method`.
A free-spelled name that Check resolved to a method by its definition name is `method` too. Elab's
last spelling pass, `spellMethodCallsApart`, finds every method reference (in function bodies and
constant initializers) whose spelling a free function of the program defines and respells it by
the method it names: the targets of its linker aliases that are method definitions
(`CFnDef.isMethod`), which must be exactly one and a spelling no free function takes, else E0299
("Core cannot tell them apart"). Other method references keep their spelling, so no proof
fingerprint over them moves.

A foreign binding is bound by spelling in the interpreter and in emission (`foreignCalls`, a
module's externs) before any program definition. Those lookups now apply to a free call only
(`SCallee.direct name space` carries the namespace through Lower; `EmitSSA.directSymbol`, the
interpreter's call path), so no foreign binding answers a method call. A method value carries
its namespace too, on `CLocal.space` in Core and `SVal.fnRef` in SSA, and CoreCheck's
foreign-function-value rule, emission and the interpreter skip foreign bindings for it, so no
extern answers a method value either, whether it shares the caller spelling, the definition name
or both (`regress_238_method_value_spelled_like_extern.con`,
`regress_238_method_value_both_spellings_taken_by_externs.con`, Codex review r15) or belongs to an
unrelated module (`regress_238_method_value_beside_unrelated_extern.con`); a method value
devirtualized at its call is still a method call
(`regress_238_method_value_beside_extern_of_other_arity.con`, Codex review r7). Mono, emission (`resolveMethodName`) and the interpreter resolve a method reference among
methods only, so an alias of its spelling to a free function or an extern (an import
`wrong as P_get`) never takes it (`regress_238_free_alias_takes_method_spelling.con`,
`regress_238_extern_alias_takes_nested_method_spelling.con`, Codex reviews r11 – r12: 99 for 42
before), and an unrelated module's alias of a method's own definition name refuses nothing
(`regress_238_unrelated_alias_beside_method.con`).

Regression legs in `run_tests.sh`: the fixtures above, `run_ok … 42` and `--interp` = 42.

## Structural cause

Class 1 ("declaration identity by spelled name, not a resolved `DefId`"): a call names its callee by
a spelling, and a free function and a method's caller-side spelling are one string namespace in
Core, so a lookup by spelling meets whichever definition holds the string.

## Structural owner

R-0492 (S5 `DefId`) and R-0548 (method tables keyed by `MethodKey`; the link symbol a derived
spelling of it). This fix makes Core keep the two namespaces apart where Elab can establish them;
the callee still travels by spelling until calls carry the definition's identity.
