# Bug 096 — two modules' same-named types are one type to Core and every consumer below it

**Status:** FIXED 2026-10-21 (`land5`)
**Found:** 2026-10-18, by the probes behind branch `bug096-type-identity` (bug 082's review).
**Severity:** silent wrong code in safe code: a module reads a struct, enum or newtype declared
by another module of the same name, at that type's field offsets.

## Reproducer

`tests/programs/regress_096_imported_type_layout.con`:

```con
mod a { pub struct Copy P { pub x: Int } }
mod c { pub struct Copy P { pub y: Int, pub x: Int } }
mod b {
    import c.{ P };
    pub fn probe() -> Int { let p: P = P { y: 1, x: 7 }; return p.x; }
}
mod main {
    import b.{ probe };
    fn main() with(Std) -> Int { print_int(probe()); print_char(10); return 0; }
}
```

On main (`9d17ccc5`) the binary prints 8482603264 (b's `p.x` read with a's layout of `P`);
`--interp`, which carries fields by name, prints 7. Expected: 7 from both.

The other legs (all in `run_tests.sh`):

- `regress_096_type_identity_same_named.con`: same-named struct, enum, generic struct and newtype
  with an import alias (`P as Q`) and a method; on main Resolve refused `Shape::Line` (E0103)
  because the program-wide scope held a's `Shape`, which has no `Line`. Expected 1234567.
- `regress_096_local_and_imported_same_name.con`: a module that imports c's `P` and declares its own
  `P`; the binary printed 8482603264, `--interp` 7.
- `regress_096_nested_same_named_methods.con`: same-named types with same-named methods in sibling
  submodules; the binary printed 100751300 instead of 751300 (wrong layout and a's `get`).
- `regress_096_same_named_destroy.con`: two modules' private `Tracked`, each with a destructor; main
  refused the program (E0814, bug 052 round 3's containment). Expected 102.

## Cause

Core, Layout, Mono, Lower, EmitSSA and the interpreter identify a struct, enum or newtype by its
spelling: `Ty.named "P"`, `structLit "P"`, a flat program-wide `structDefs.find? (·.name == "P")`.
Elab copied each module's local spelling into Core, so two declarations of `P` became one Core
type, and the first registered layout won program-wide. Resolve's body scope was every module's
declarations in one first-wins map, so a module's own `Shape` lost to another module's.

## Fix

Elab is the last stage that knows which declaration a spelling resolved to
(`StructDef.definedIn` / `definitionName`, stamped by `buildFileSummary`; newtypes now carry
`definedIn` too). `elabProgram` computes the type names two or more modules of the program
declare (`ambiguousTypeNames`), and `elabModule` spells every type in Core by its definition
(`Concrete/Elab/TypeSpelling.lean`): the definition's own name when it is unique (so the common
case, its goldens and its evidence are unchanged), and `<defining module path>_<name>` when it is
not. Definitions are renamed by their own identity; type mentions by the binding this module's
lookups resolve, which also resolves an import alias. Methods (`<Type>_<method>`), trait impls
(`typeName`, `selfTy`, method signatures), trait signatures, externs, constants, `ownTypeNames`,
`callTargets` and the imported capability table follow the new spelling, and a submodule's
respelled methods get linker aliases. Resolve searches each top-level module's own declarations
and imports before the program-wide scope (`ShallowResult.moduleScopes`). Mono's and the
interpreter's ambiguous-name refusals (bug 052 round 3) are no longer reached by these programs;
`check_collections_copy_only.sh` now pins the correct output of its two collision fixtures.

Types written in another module (an imported or sibling declaration's fields, payloads or
signature, a submodule function's signature) are spelled as their module binds them: each such
mention of a respelled name must mean, there, the declaration this module's spelling means, and a
name this module does not bind takes the binding those modules agree on
(`regress_096_imported_struct_field_binding.con`). The declarations checked strictly are the ones
the module imports by name and the whole import closure resolution transports (types reached
through fields and signatures, their public methods), each in its own type-parameter scope; one
that disagrees is refused (E0299: `error_096_import_closure_binds_other_type.con`,
`error_096_generic_method_param_named_like_type.con`), and a private method resolution does not
transport is not consulted (`regress_096_private_method_not_transported.con`). A module's
binding of a spelling follows Check's and Elab's precedence, imports before its own declarations
(`error_096_import_precedence_binding.con`), and a method signature belongs to the one type whose
module declares exactly that method, not to a textual prefix (`A_B_get` is `A_B`'s:
`error_096_method_owner_exact.con`). A submodule's or sibling's declarations, visible without an
import (the parent injects them after import resolution, so Elab keeps the resolved closure apart),
only supply a binding the module lacks (`regress_096_sibling_types_unused.con`).
Example of the refusal: b binding `P`
to a's while calling c's `mk`, which returns c's `P`, was accepted by Check (it compares types by
spelling) and read the wrong layout (`error_096_imported_sig_binds_other_type.con`). A canonical
spelling that is already another type's (`a.b`'s `P` and `a_b`'s `P`) is refused the same way
(`error_096_canonical_type_spelling_clash.con`), and so is a respelled method name that is
already another function's (`error_096_method_spelling_clash.con`, `checkRespelledFnNames`). A function value of a respelled method
(`P_get`) is respelled; a local of function type never is (`CoreRename.locals`, scoped: a `let` binds after
its initializer, `regress_097_let_shadows_colliding_fn.con`). Names the
compiler itself special-cases (`builtinTypeNames`, `HashMap`, `HashSet`, `BinaryHeap`) are never
respelled; a collision on one stays under bug 052's refusals. `CFnDef.declName` keeps a
respelled method's declaration name, so ProofCore's `CallableId`s and the reports' qualified
names are unchanged.

Residual: `--interp` cannot call a sibling submodule's method (`regress_096_nested_same_named_methods`
fails `undefined function` there, as on main); that is the interpreter's alias-resolution gap of
[bug 118](118_interp_nested_module_import.md), so that leg is native only.

## Structural cause

Declaration identity by spelled name, not a resolved `DefId` ([structural class](STRUCTURAL_CLASSES.md)
1, with 039, 044, 050, 051, 054 – 056, 061, 069, 074, 077, 081, 082, 097, 098 and 132): a type's
identity in Core is its spelling, and every pass below Elab re-resolves the spelling program-wide.
The respelling makes spellings unique; it does not give Core an identity.

## Structural owner

R-0492 (S5 `DefId`), applied to the shared front end by R-0514: a type mention resolves to its
definition's identity, after which the canonical spelling is only a symbol name.
