# Bug 243 — an aliased newtype import binds no name

<!-- bug-ledger
summary: `import m.{N as M}` of a newtype binds no `M` (`resolveImports` appends the newtype under its own name), so `M::mk` is E0106. Owned by R-0492/R-0514; reproducer pinned as an open-bug reproducer (probes of bug 223's fix, 2026-10-25). Class 1.
coverage: an aliased newtype import binds no name (resolveImports appends the newtype under its own spelling), so the alias is unknown (E0106); owner R-0492/R-0514 imports bind identities; reproducer tests/regressions/bug243_aliased_newtype_import_binds_nothing/repro.con, found by the probes of bug 223's fix 2026-10-25; pinned as an open-bug reproducer (compile-fails)
class: 1 — an aliased newtype import binds no name: an import copies the declaration under the alias, and the newtype kind has no such copy
silent: no
-->

**Status:** OPEN
**Found:** 2026-10-25, by the probes of bug 223's fix (R-0492 step 3): the alias form of each
importable type kind.
**Severity:** loud. A valid program is refused.

## Reproducer

`tests/regressions/bug243_aliased_newtype_import_binds_nothing/repro.con`: `mod m { pub newtype N
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

## Structural cause

Class 1 ("declaration identity by spelled name"): an import binds a spelling by copying the
declaration under it, so each type kind needs its own renamed copy, and the newtype kind has none.

## Structural owner

R-0492 / R-0514: an import binds the local spelling to the declaration's identity in a module's
type environment instead of copying the declaration; bug 223's fix (R-0492 step 3) already finds
methods and compares types by that identity.
