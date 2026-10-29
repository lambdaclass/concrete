# Bug 246 — an imported alias whose target is another alias of its module names an unknown type

<!-- bug-ledger
summary: an imported `pub type N = M` whose target `M` is a private alias of its module arrived as `M`, a type the importer cannot name (E0220 on a valid value); the target is now expanded through the exporter's aliases at the import. Found while writing a fixture for the Codex review of bug 233's fix, 2026-10-25. Fixture `regress_246_imported_alias_chain.con`. Class 1.
fixtures: regress_246_imported_alias_chain.con regress_246_imported_alias_chain_across_imports.con regress_246_imported_alias_chain_long.con
class: 1 — an imported alias's target crossed as the exporter's spelling; fixed 2026-10-25 by expanding it at the import
silent: no
ruling: Violates: TYPE-12@1
-->

**Status:** FIXED (2026-10-25, branch `wip/r0492-steps34`)
**Found:** 2026-10-25, while writing a fixture for the Codex review (round 3) of bug 233's fix: an
imported constant's type through an alias its module imports.
**Severity:** loud. A valid program is refused.

Confirmed real by Codex review, 2026-10-25: TYPE-12 (docs/spec/types.md): an imported transparent alias retained its private target’s spelling instead of resolving transitively to the underlying type.

## Reproducer

`tests/programs/regress_246_imported_alias_chain.con`: `mod types { type M = i32; pub type N = M; }`,
`import types.{N};` and `let x: N = 42;`. Refused on main `593dc100`: `(E0220) type mismatch in let
binding 'x': expected N, got i64` (the binary and `--interp`). Expected: 42.

## Cause

`FileSummary.resolveImports` imported a public alias with its target as written, `.named "M"`.
`M` is a private alias of `types`; the importer has no `M`, so Check's alias map (closed over the
importer's own and imported aliases) expanded `N` to the unknown `M`, which matched nothing.

## Fix

The target is expanded at the import through the aliases the exporting module can see
(`writableAliases`: its own, and the public aliases it imports expanded the same way in their
exporters, bounded by the program's module count so every acyclic chain is followed and a cycle
stops), as an imported constant's type is (bug 233). Regression legs in `run_tests.sh`:
`regress_246_imported_alias_chain.con`, `regress_246_imported_alias_chain_across_imports.con` (`b`
exports `M = N` with `N` imported from `a`, Codex review) and
`regress_246_imported_alias_chain_long.con` (ten alias imports), each `run_ok … 42` and `--interp`
= 42.

## Structural cause

Class 1 ("declaration identity by spelled name"): a type crosses a module boundary as a spelling,
which the importer reads in its own environment. Bug 242 is the same for a type in an imported
signature.

## Structural owner

R-0492 / R-0514: types cross imports by declaration identity (the importer never re-reads a
spelling of the exporter). This repair expands the one position where the target is known.
