# Bug 247 — the assumption summary misses a function value written as an identifier

<!-- bug-ledger
summary: the assumption summary read function values only in their `fnRef` form, so `--report caps` called a function returning a value that reaches an extern "(no external authority)" with a complete call graph; and free calls and values were attributed to unrelated externs of their spelling. Function values written as identifiers are followed, and a free reference names a foreign binding only when its function binds the spelling to one. Found by the Codex review of bugs 238 – 241, 2026-10-25. Fixtures `regress_247_*`. Class 12.
fixtures: regress_247_function_value_reaches_foreign.con regress_247_free_value_beside_unrelated_extern.con regress_247_free_call_beside_unrelated_extern.con
class: 12 — a function value has two Core forms, `fnRef` and an identifier; the assumption summary's value walk read one
silent: yes
exit: 0b
-->

**Status:** FIXED (branch `wip/name-capture`)
**Found:** 2026-10-25, by the Codex review (round 7) of the fix for bugs 238 – 241, on the train-17
candidate `593dc100`.
**Severity:** silent (a false claim). `--report caps` called a function "(no external authority)"
with a complete call graph although the function value it returns reaches a foreign binding.

## Reproducer

`tests/programs/regress_247_function_value_reaches_foreign.con`: `trusted extern fn abs`, a method
`P::get` whose body calls `abs(-42)`, and `fn factory() -> fn() -> i32 { return P_get; }`. Before
the fix `--report caps` printed `factory : (no external authority)` and "call graph complete";
now `factory : (no declared capability)`, assuming `main.abs` honest. The program returns 42,
natively and under `--interp`.

## Cause

Core writes a function used as a value in two forms: `CExpr.fnRef` and a function-typed `.ident`
with no binding identity, which is the form Elab produces for `let f = g;` and `return g;`. The
assumption summary reaches a foreign binding through a function value as well as through a call
(R-0484 R10: a value may be called later by code no direct edge connects to it), and reads the
values from `collectFnValueRefs*`, which matched `.fnRef` only. Every function value Elab produced
was therefore invisible to it, a free function's as much as a method's.

## Fix

`collectFnValueRefsExpr` also collects a function-typed `.ident` with no binding identity (a
global function used as a value; a local of function type carries its `LocalId`).

A free function value names a foreign binding only when its function binds the spelling to one
(`foreignCalls`) or no program definition answers it, so a value of the program's own `target`
beside an unrelated extern `target` assumes nothing
(`regress_247_free_value_beside_unrelated_extern.con`, Codex review r16). The same holds for a
free CALL, which the summary also resolved against every module's foreign bindings independently
of program definitions (pre-existing; `regress_247_free_call_beside_unrelated_extern.con`, Codex
review r20).

## Structural cause

Class 12 ("one fact kept in two places"): a function value has two Core forms, and the walk that
answers "which functions does this body name as values" knew one of them.

## Structural owner

R-0492 (callable `DefId`): a function value that carries its definition's identity in one Core
form needs no second spelling to be recognised by.
