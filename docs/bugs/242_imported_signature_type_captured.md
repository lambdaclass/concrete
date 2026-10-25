# Bug 242 — a type in an imported signature is captured by the importer's own type of that spelling

<!-- bug-ledger
summary: a type in an imported signature (`fn make() -> P`) was read in the importer's environment by its definition spelling, and the import closure's copy of `P` came first, so an importer's own `struct P` was unreachable (E0252 on its literal); likewise where another import binds the spelling. `resolveImports` respells a transported type whose spelling denotes another declaration in the importer to its identity (`m::shapes::P`) and binds that to the declaration; Elab spells a method found by identity through it. Found by the probes of bug 223's fix, 2026-10-25. Fixtures `regress_242_*`, `error_242_*`. Class 1.
fixtures: regress_242_imported_signature_type.con regress_242_toplevel_signature_type.con regress_242_imported_enum_beside_own_enum.con regress_242_imported_field_type.con regress_242_aliased_self_result.con regress_242_imported_constant_type.con regress_242_imported_alias_target.con regress_242_generic_argument.con regress_242_newtype_beside_other_import.con regress_242_struct_beside_other_import.con regress_242_two_modules_signatures.con regress_242_alias_target_closure.con regress_242_child_newtype_from_child_function.con error_242_two_imported_methods_one_core_name.con
class: 1 — a type in an imported signature is a spelling the importer re-reads, so the importer's own type of that spelling is captured; fixed 2026-10-25: a transported type whose spelling denotes another declaration in the importer is respelled to its identity
silent: no
-->

**Status:** FIXED (2026-10-25, branch `wip/ident-followups`)
**Found:** 2026-10-25, by the probes of bug 223's fix (R-0492 step 3): an alias of an imported
type beside a local type of the definition's spelling.
**Severity:** loud. A valid program is refused; the probes found no wrong code.

## Reproducer

`tests/programs/regress_242_imported_signature_type.con` (the former
`tests/regressions/bug242_imported_signature_type_captured/repro.con`): `m.shapes` declares
`pub struct P { pub v: i64 }` and `pub fn make(n: i64) -> P`; `m.user` imports only `make` and
declares its own `struct P { w: i64 }`. `user`'s literal `P { w: 2 }` is refused:
`(E0252) missing field 'v' in struct literal 'P'`. The same with top-level modules. Expected: 42.
Observed on `wip/r0492-steps34` and on main `593dc100` (where an alias `import m.shapes.{P as Q}`
beside the local `P` fails the same way).

## Cause

An imported signature carries its types as the defining module writes them, `.named "P"`. The
import closure (`FileSummary.resolveImports`, bug 036) adds every public type such a signature
names to the importer's imports when no IMPORTED declaration of that spelling is present; it does
not consult the importer's own declarations. Check's and Elab's environments list imports first
(`allStructs := imports.structs ++ summary.structs`), so the imported `P` answers the importer's
spelling `P` everywhere, and the importer's `struct P` is unreachable.

## Fix

A type crosses an import carrying its declaration (`FileSummary.resolveImports`). Every type that
an import transports (a signature's parameters and result, a struct's fields, an enum's payloads,
a newtype's inner type, an alias's target, a constant's type, an impl method's signature) is
written as its defining module spells it; where that spelling denotes another declaration in the
importer, it is respelled to the declaration's identity spelling, `TypeId.importSpelling`
(`m::shapes::P`, `::` being no identifier character), and the import closure binds that spelling
to the declaration. A spelling denotes another declaration when the importer declares a type of
it (`importerTypeSpellings`, the one definition every pass passes in: its structs, enums,
newtypes, aliases and its children's newtypes, each with the declaration it denotes, so a
transported type that is that declaration keeps the spelling), when an explicit import binds it to another
declaration, or when an earlier transported mention claimed it for another declaration (two
imported modules' `P`s). A kept spelling is bound to the declaration that claimed it, never to
another module's of that spelling. When a type's own spelling is respelled, its methods' `Self`
is resolved first. Elab names a method found by identity through the call's spelling of its type
when the definer's spelling denotes another declaration here, and its Core rename maps a method to
its owner's name (`renameMethod`).

Two bug 096 consequences: the programs whose imported types the importer bound to other
declarations are now checked as the definitions mean them. Three are refused by Check (E0220) instead
of Core's backstop (E0299); four are valid and run, renamed `regress_096_*` (native and
`--interp`). One of them needed a newtype over a struct under `--interp` (bug 254).

Regression legs in `run_tests.sh`: `regress_242_imported_signature_type.con` (the reproducer) and
twelve more `regress_242_*` (top-level modules, an enum result, a field type, an aliased type's `->
Self`, a constant's type, an alias target, a generic argument, an aliased newtype and struct beside
another module's of the definition's spelling, two modules' signatures each naming their own `N`,
an alias target that only the alias names, a child's newtype returned by the child's function),
each `run_ok … 42` and `--interp` = 42; `error_242_two_imported_methods_one_core_name.con` (two
imported types of one spelling both with `mk`: Core still calls a method by its mangled spelling,
so Check refuses, E0299).

## Structural cause

Class 1 ("declaration identity by spelled name"): a type in a signature is a spelling, interpreted
in whichever environment reads it, so a signature crossing a module boundary changes meaning.

## Structural owner

R-0492 (S5 `DefId`), applied to the front end by R-0514. The fix carries the identity in the one
representation the passes share, a spelling no source can write, bound by import resolution, the
one place all passes take imports from. `Ty` carrying a `TypeId` (and Core calling methods by
`MethodKey`, which `error_242_two_imported_methods_one_core_name.con` still needs) remains R-0514's.
