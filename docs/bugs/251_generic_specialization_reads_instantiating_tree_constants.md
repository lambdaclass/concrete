# Bug 251 — a generic's specialization reads the constants of the tree that instantiates it

**Status:** FIXED (2026-10-25, branch `wip/ident-followups`)
**Found:** 2026-10-25, by the Codex review of bug 245's fix: the interpreter, now scoping a
constant by the defining function's tree, disagreed with the binary on a generic function.
**Severity:** silent. The binary returns another module's constant; with no constant of that
spelling in the instantiating tree, the build fails (E0704) instead.

## Reproducer

`tests/programs/regress_251_generic_reads_defining_tree_constant.con`: `mod a { const K: Int =
40; pub fn get<T: Copy>(x: T) -> Int { return K; } }`, `import a.{get};`, `const K: Int = 2;` and
`main` returning `get::<Int>(0)`. Observed on `wip/r0492-steps34` (and main `abc6318a`): the binary
returns 2; `--interp` returns 40 (and, before bug 245's fix, refused the program as naming a
constant two modules define). Without main's `K`
(`regress_251_generic_constant_absent_in_instantiating_tree.con`) the build fails: `(E0704)
get_for_Int: block 'entry': use of undefined register %K`. Expected: 40.

## Cause

Core references a module's own constant by its bare spelling, and Lower resolves it in the
constants of the tree it is lowering (`lowerModule`'s `collectAllConstants m`). Mono emits a
generic function's specialization into the module whose body first instantiates it
(`monoModule` drains the queue into the module being processed), so `get_for_Int` was lowered as
a function of main's tree and its `K` was main's. The interpreter evaluates the generic itself, in
its defining tree.

## Fix

A function records the tree its constant spellings were written against when Mono moves it:
`CFnDef.constTree`, the defining top-level module. `monoProgram` stamps it on every generic
template, both specialization constructors copy it, and Lower lowers a function whose `constTree`
names another tree with that tree's constants (`CModule.treeConstants`, bug 245's one definition
of the scope). A function Mono does not move keeps `none` and its own tree, so nothing else changes.

Regression legs in `run_tests.sh`: `regress_251_generic_reads_defining_tree_constant.con` (40),
`regress_251_generic_constant_absent_in_instantiating_tree.con` (40) and
`regress_251_nested_generic_constants.con` (a generic of one tree calling a generic of another, each
reading its own constant: 42), native and `--interp`.

## Structural cause

Class 1 ("declaration identity by spelled name"): a constant reference is a spelling, resolved in
whichever tree a pass places the body in; Mono moves bodies between trees, so the spelling's
meaning moved with it.

## Structural owner

R-0492 / R-0514: every constant reference carries its `ConstId` (as an imported one does since bug
233's fix), so no pass resolves a constant spelling against the tree a body happens to sit in.
`constTree` is the containment until then: it records the scope the spelling was written in.
