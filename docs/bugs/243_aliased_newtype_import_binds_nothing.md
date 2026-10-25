# Bug 243 — an aliased newtype import binds no name

<!-- bug-ledger
summary: `import m.{N as M}` of a newtype bound no `M` (`resolveImports` appended the newtype under its own name, and a newtype had no definition-name field), so `M::mk` was E0106. A newtype now carries `definitionName` like a struct or enum, the import binds the alias to a copy with the declaration's identity, and Elab spells it in Core by that identity. Found by the probes of bug 223's fix, 2026-10-25. Fixtures `regress_243_*`. Class 1.
fixtures: regress_243_aliased_newtype_import.con regress_243_aliased_newtype_with_original.con regress_243_aliased_generic_newtype.con regress_243_nested_aliased_newtype.con
class: 1 — an aliased newtype import binds no name: an import copies the declaration under the alias, and the newtype kind has no such copy; fixed 2026-10-25: a newtype carries its definition name, and the import binds the alias to a copy with the declaration's identity
silent: no
-->

**Status:** FIXED (2026-10-25, branch `wip/ident-followups`)
**Found:** 2026-10-25, by the probes of bug 223's fix (R-0492 step 3): the alias form of each
importable type kind.
**Severity:** loud. A valid program is refused.

## Reproducer

`tests/programs/regress_243_aliased_newtype_import.con` (the former
`tests/regressions/bug243_aliased_newtype_import_binds_nothing/repro.con`): `mod m { pub newtype N
= i64; impl N { pub fn mk(x: i64) -> N … pub fn get(self) -> i64 … } }` and `import m.{N as M};`
with `let a: M = M::mk(42); return a.get();`. Refused by Resolve: `(E0106) unknown static method
'M::mk'`. Expected: 42, as `import m.{N}` with `N` gives. Observed on `wip/r0492-steps34` and on
main `593dc100`.

## Cause

`FileSummary.resolveImports` appends an imported newtype unchanged (`newtypes := acc.newtypes ++
[nt]`), where a struct or enum is registered under the importer's spelling (bug 069), so `M` names
nothing in Check, Elab or a nested module's Resolve scope. `NewtypeDef` has no definition-name
field (its spelling is its identity, `NewtypeDef.typeId?`), and Layout, Elab and Core find a newtype
by `name`, so renaming the copy as bug 069 does for structs would change its identity downstream.

## Fix

A newtype carries its definition-site name, `NewtypeDef.definitionName`, stamped by
`buildFileSummary` with `definedIn`, as a struct's and an enum's are; `NewtypeDef.typeId?` is
`(definedIn, definitionName)` and `NewtypeDef.declName` the declared name. The import binds the
alias as it binds a struct's (bug 064): a copy whose `name` is the importer's spelling and whose
identity is the declaration's. Check finds its methods by that identity (bug 223's
`methodByIdentity`), and Elab names it in Core by the declaration (`canonTypeName definedIn
declName`; its spelling candidates, method owners, cross-module bindings and the program's type
declarations read `declName`), so Core for `M` is Core for `N`. Coherence's newtype declaration
reads `declName` too.

Regression legs in `run_tests.sh`: `regress_243_aliased_newtype_import.con` (the reproducer),
`regress_243_aliased_newtype_with_original.con` (the alias and the original in one import),
`regress_243_aliased_generic_newtype.con` (a generic newtype under an alias, beside another
module's newtype) and `regress_243_nested_aliased_newtype.con` (inside a nested tree), each
`run_ok … 42` and `--interp` = 42.

## Structural cause

Class 1 ("declaration identity by spelled name"): an import binds a spelling by copying the
declaration under it, so each type kind needs its own renamed copy, and the newtype kind has none.

## Structural owner

R-0492 / R-0514. Every importable type kind now carries its identity apart from its spelling, so
an import of any of them binds its local spelling to that identity, and the passes after it
compare and look up by the identity (bug 223's fix). The binding is still a renamed copy of the
declaration in each pass's own environment; one per-module type environment mapping spellings to
`TypeId`s, consumed by Check and Elab instead of rebuilt, remains R-0514's.
