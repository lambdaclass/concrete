# Bug 231 — a bound naming an undeclared trait is accepted

**Status:** FIXED (branch `wip/diffhunt-1025`)
**Found:** 2026-10-24, by the differential hunt on main `9622f7e0` (generics and traits across
module boundaries): a nested module's generic `fn twice<A: Acc>(a: &mut A)` over its enclosing
module's trait was refused only at the method call (E0265, "no method 'add' for type variable
'A'"), while the same bound with no method call compiled and ran.
**Severity:** invalid programs accepted (MOD-1); no wrong code. The bound is decorative where it
is accepted: nothing can be called through it.

## Reproducer

- `tests/programs/error_231_unknown_bound_trait.con`: `struct Holder<T: NoSuchTrait>` and
  `fn id<A: NoSuchTrait>(a: A) -> A`, neither used; on main it compiles and exits 42. A call
  `id(42)` met only E0241, "type 'i64' does not implement trait 'NoSuchTrait'".
- `tests/programs/error_231_enclosing_trait_bound.con`: `mod lib { trait Acc; mod deep { fn
  pass<A: Acc>(a: A) -> A } }` called from `lib` with a `lib` type that implements `Acc`; on main it
  exits 42 natively and under `--interp`, though `deep` cannot name `Acc` (a method call through the
  bound there is E0265).

Expected and now observed: each is refused at Resolve, `(E0125) unknown trait '…' in the bound on
'…' of '…'`.

## Cause

No pass resolved the trait names of a bound. Resolve walks every written type and expression of a
module (`checkTyDeep`, `resolveExpr`) but never read `typeBounds` (of a function, struct, enum,
newtype, impl block or trait impl). Check read bounds in two places only, both by spelling: a call
site (`checkTraitBounds`) asks whether the argument's type has an impl of a trait with that name,
anywhere in the program (`env.traitImpls`), and a method call on a type parameter
(`boundMethodCandidates`) searches the module's `env.traits` (the builtin `Destroy` and the module's
own traits). So an unknown name was never refused, and an enclosing module's trait passed the
call-site test by spelling in a nested module whose own trait table does not contain it.

## Fix

`Resolve.boundTraitsCheck`, run for every module of the tree (bug 217's walk), refuses a bound
trait that is not `Copy`, the builtin `Destroy` or one of the module's own traits: exactly the
traits Check gives that module (`allTraits`, plus `Copy`, which `checkTraitBounds` reads), since
traits are module-local and cannot be imported (TRAIT spec). New code E0125 (MOD-1@1). Regression
legs in `run_tests.sh`: the two `error_231_*.con` fixtures (`run_err … (E0125)`) and the positive
control `regress_231_bounds_name_module_traits.con` (`Copy`, a module trait at top level and in a
nested module, generic struct and impl bounds: `run_ok … 42` and `--interp` = 42).

## Structural cause

Class 13 ("documented rules with no enforcing site"): MOD-1 states that every name a program uses
resolves to one declaration, and Resolve enforces it by visiting name positions one by one; a
bound is a name position the walk did not list, and the later reads of it looked trait names up by
spelling and could only answer "not implemented" or "no method".

## Structural owner

R-0492/R-0514 (one resolver: every name position resolved once to a declaration identity, which a
bound's trait then carries into Check instead of its spelling).
