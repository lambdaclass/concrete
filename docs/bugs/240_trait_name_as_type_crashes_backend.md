# Bug 240 — a trait name in a type position passes Resolve and crashes the backend

**Status:** FIXED (branch `wip/name-capture`)
**Found:** 2026-10-25, during the R-0492 slice 6 identity work, reproduced on main `70bcdeb6` and
on the train-17 candidate `593dc100`.
**Severity:** crash (native build); `--interp` ran the program.

## Reproducer

- `tests/programs/error_240_trait_cast_target.con`: `trait Marker { .. }` and `(x as Marker) as
  Int`. Before the fix `--interp` returned 42 and the native build panicked in EmitSSA
  (`tyToLLVMTy: unknown named type 'Marker'`).
- `tests/programs/error_240_trait_in_type_positions.con`: a field, a parameter, a return type, a
  `let` annotation and a type argument naming the trait. Before the fix a parameter `m: Marker`
  reached Check as an opaque linear type (E0208, "never consumed").

Expected and now observed: E0108 at Resolve, "'Marker' is a trait, not a type: there are no
trait-object types (TYPE-2) …". `regress_240_trait_bounds_and_type_param_shadow.con` keeps a trait
in a bound and an impl, and a type parameter spelled like the trait in type positions (42, also
`--interp`).

## Cause

A trait is in Resolve's type namespace (`SymKind.isType`), since a bound and an impl name it there,
and `checkTyDeep` accepted every name of that namespace as a type (`knownTypes` lists traits too).
TYPE-2 states there are no trait-object types, but its only enforcement was that `Ty` has no such
constructor; a trait's spelling became `.named "Marker"`, which no struct, enum or newtype defines.

## Fix

`checkTyDeep` refuses a written type that denotes a trait (`denotesTrait`: the type namespace binds
the spelling to a trait and no type parameter shadows it), in every position it walks: annotations,
signatures, fields, cast targets and type arguments (bug 215's `checkWrittenTys`). Code E0108
(MOD-1: the name denotes no type), with a TYPE-2 message. Not a cast-semantics change: no cast to a
trait ever had a meaning.

## Structural cause

Class 13 ("documented rules with no enforcing site"): TYPE-2 is a guarantee no site enforced at
the source level; Resolve's type namespace answered "is a declaration" where the position asked "is
a type".

## Structural owner

R-0492/R-0514 (one resolver: a written type resolves to a type declaration's `TypeId`, which a
trait's identity is not).
