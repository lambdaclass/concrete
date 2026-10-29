# Bug 251 — a generic's specialization reads the constants of the tree that instantiates it

<!-- bug-ledger
summary: a generic function's specialization, which Mono emits into the module tree that first instantiates it, had its bare constant spellings resolved by Lower in that tree: the binary read the instantiating tree's constant of that spelling (a wrong value, where `--interp` read the generic's own) or, with none there, an undefined register (E0704); likewise a call to the tree's own function spelled like an intrinsic (`sizeof`) was taken for the intrinsic, and a call bound the instantiating module's `extern` of its spelling. A specialization keeps its generic's defining tree (`CFnDef.sourceTree`, `SFnDef.sourceTree`), whose constants and function names Lower uses and whose call context EmitSSA uses. Found by the Codex review of bug 245's fix, 2026-10-25. Fixtures `regress_251_*`. Class 1.
fixtures: regress_251_generic_reads_defining_tree_constant.con regress_251_generic_constant_absent_in_instantiating_tree.con regress_251_nested_generic_constants.con regress_251_generic_calls_shadowed_intrinsic.con regress_251_generic_constant_calls_shadowed_intrinsic.con regress_251_generic_call_beside_destination_extern.con regress_251_generic_calls_source_extern.con regress_251_generic_calls_source_import_alias.con
class: 1 — a generic's specialization moved to the instantiating tree, whose constants and function names then answered its spellings; fixed 2026-10-25: a specialization keeps its defining tree
silent: yes
exit: 0b
ruling: Violates: unrecorded
-->

**Status:** FIXED (2026-10-25, branch `wip/ident-followups`)
**Found:** 2026-10-25, by the Codex review of bug 245's fix: the interpreter, now scoping a
constant by the defining function's tree, disagreed with the binary on a generic function.
**Severity:** silent. The binary returns another module's constant; with no constant of that
spelling in the instantiating tree, the build fails (E0704) instead.

Confirmed real by Codex review, 2026-10-25: PASS-1 (docs/spec/pipeline.md): specialization changed a generic’s constant and callable bindings to those of the instantiating module, changing results.

## Reproducer

`tests/programs/regress_251_generic_reads_defining_tree_constant.con`: `mod a { const K: Int =
40; pub fn get<T: Copy>(x: T) -> Int { return K; } }`, `import a.{get};`, `const K: Int = 2;` and
`main` returning `get::<Int>(0)`. Observed on `wip/r0492-steps34` (and main `abc6318a`): the binary
returns 2; `--interp` returns 40 (and, before bug 245's fix, refused the program as naming a
constant two modules define). Without main's `K`
(`regress_251_generic_constant_absent_in_instantiating_tree.con`) the build fails: `(E0704)
get_for_Int: block 'entry': use of undefined register %K`. Expected: 40. The same holds for the
tree's functions (Codex review): a generic of `a` calling `a`'s own `fn sizeof`, directly or
through a constant's initializer, called the `sizeof` intrinsic in the binary (8, expected 40;
`regress_251_generic_calls_shadowed_intrinsic.con`), and for its module's call context in the
emitted code: beside an `extern fn abs` of the instantiating module, the generic's call to its own
module's `fn abs` called C's `abs` (5, expected 40;
`regress_251_generic_call_beside_destination_extern.con`).

## Cause

Core references a module's own constant by its bare spelling, and Lower resolves it in the
constants of the tree it is lowering (`lowerModule`'s `collectAllConstants m`). Mono emits a
generic function's specialization into the module whose body first instantiates it
(`monoModule` drains the queue into the module being processed), so `get_for_Int` was lowered as
a function of main's tree and its `K` was main's. Likewise Lower decides that a call spelling is an
intrinsic when the tree it lowers defines no function of that name (`definedFnNames`), and main's
tree defines no `sizeof`; and EmitSSA binds a call spelling through the emitting module's import
aliases and own `extern` bindings (`localAliases`, `moduleExterns`), main's. The interpreter evaluates the generic itself, in
its defining tree.

## Fix

A function records the tree its spellings were written against when Mono moves it:
`CFnDef.sourceTree`, the defining top-level module. `monoProgram` stamps it on every generic
template, both specialization constructors copy it, and Lower lowers a function whose `sourceTree`
names another tree in that tree's context: its constants (`CModule.treeConstants`, bug 245's one
definition of the scope) and its defined function names. Lower passes the tree on to SSA
(`SFnDef.sourceTree`, set only when it is not the emitting module), and EmitSSA emits such a
function in that module's call context (`moduleCallContext`, the one definition of a module's
import aliases and own extern bindings, now also used for the module's own functions). A function
Mono does not move keeps `none` and its own tree, so nothing else changes.

Regression legs in `run_tests.sh`: `regress_251_generic_reads_defining_tree_constant.con` (40),
`regress_251_generic_constant_absent_in_instantiating_tree.con` (40) and
`regress_251_nested_generic_constants.con` (a generic of one tree calling a generic of another, each
reading its own constant: 42), `regress_251_generic_calls_shadowed_intrinsic.con` and
`regress_251_generic_constant_calls_shadowed_intrinsic.con`,
`regress_251_generic_call_beside_destination_extern.con`, `regress_251_generic_calls_source_extern.con`
and `regress_251_generic_calls_source_import_alias.con` (40), native and `--interp`.

## Structural cause

Class 1 ("declaration identity by spelled name"): a constant reference, and which function or
foreign symbol a call names, are spellings resolved in whichever tree a pass places the body in;
Mono moves bodies between trees, so their meaning moved with it.

## Structural owner

R-0492 / R-0514: every constant reference carries its `ConstId` (as an imported one does since bug
233's fix), and every call its `CallableId`, so no pass resolves a spelling against the tree a body
happens to sit in. `sourceTree` is the containment until then: it records the scope the spellings
were written in.
