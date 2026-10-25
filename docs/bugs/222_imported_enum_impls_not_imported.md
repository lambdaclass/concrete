# Bug 222 — an imported enum's impl blocks and trait impls are not imported

<!-- bug-ledger
summary: an imported enum's impl blocks and trait impls were not imported (the struct and newtype import paths each selected them; the enum paths, by name and through the transitive closure, did not), so every bound needing a trait impl on an imported enum was E0241 outside its module (`encode_all<T: Encode>` over `Vec<Instr>`, a `Destroy` bound on a linear enum). One helper (`typeImplsOf`) serves every imported type kind. Pinned by `regress_222_imported_enum_impls.con` (binary and --interp). Found by Phase 7 workload 10 (stackvm)
fixtures: regress_222_imported_enum_impls.con
class: 12 — what travels with an imported type, selected once per type kind; the enum paths did not select it
silent: no
-->

**Status:** FIXED (branch `wip/workload-stackvm-2`)
**Found:** 2026-10-23, by Phase 7 workload 10 (`examples/stackvm`): `codec` encodes the
instruction vector with `isa.encode_all<T: Encode + Copy>(&p.code, ..)`, and `Instr` is an enum.
**Severity:** loud. A valid program was refused: every bound needing a trait impl on an imported enum failed
outside the enum's module (E0241), so a generic helper over the enum, a `Vec` of a linear enum
with a `Destroy` impl (`v.drop()` needs `T: Destroy`) and a call such as `kill<T: Destroy>(x)`
could not be written in another module. Structs and newtypes were not affected.
A `Copy` or `Destroy` bound on a `Copy` enum was satisfied all along: `Copy` is read from the
declaration, which was imported, and a `Copy` type satisfies `Destroy`.

Confirmed real by Codex review, 2026-10-25: docs/spec/traits.md TRAIT-10 recognizes existing impls as satisfying bounds; enum imports dropped those impls and falsely rejected valid calls. Ordinary methods already resolved.

## Reproducer

`tests/programs/regress_222_imported_enum_impls.con` (an enum with an inherent impl and a trait
impl, a linear enum with a `Destroy` impl, both used from sibling modules; a struct whose field is
the enum, imported alone so that the enum arrives through the transitive closure).

Observed on main `408880cb`:

```text
error[check]: (E0241) type 'Shape' does not implement trait 'Area' required by generic function 'total'
error[check]: (E0241) type 'Ticket' does not implement trait 'Destroy' required by generic function 'kill'
```

and, for the transitive case (`import m.shapes.{Tile, mk, total}` with `Tile.shape: Shape`), the
same E0241 on `total(&t.shape)`. `examples/stackvm` stopped at
`type 'Instr' does not implement trait 'Encode' required by generic function 'encode_all'`.
Expected: 42 on both backends.

Method calls on the imported enum (`Shape::square(3)`, `a.area()`, `e.is_empty()`) resolved on
main: Check finds a method's signature by its mangled name (`Shape_area`) in the callee table, which
does not depend on the import path. What the import path alone supplies is the list of trait impls
a bound is checked against, and that list was empty for an enum.

## Cause

`Resolve.resolveImports` (`Concrete/Resolve/FileSummary.lean`) builds what an import brings in.
For a struct and for a newtype, it selected the type's inherent impl blocks, its trait impls and
the signatures of their public methods, and appended them; each of the two paths restated that
selection. The enum path appended the enum declaration only, in both places an enum is imported:
by name (`import m.{E}`) and through the transitive closure (bug 036), which pulls in a type
reachable from an imported declaration.

## Fix

One helper, `typeImplsOf summary name`, selects what travels with an imported type, and every
type import (struct, enum and newtype, by name and through the closure: six sites) takes its impl
blocks, trait impls and method signatures from it. Pinned by
`regress_222_imported_enum_impls.con` (42; `run_tests.sh` also requires `--interp` to print 42).
The workload compiles unchanged.

## Structural cause

Class 12 ("one fact kept in two places"): "an imported type brings its impls" was kept once per
type kind instead of once, so the kind added without it (enum) silently lacked it. The rule did not
live with the type; it lived in each branch of a match on declaration kinds.

## Structural owner

R-0492 / R-0514 (one resolver: an import resolves to a declaration identity, and what a
declaration carries is a property of the declaration, not of the import branch that found it).
Until then, `typeImplsOf` is the single selection site.
