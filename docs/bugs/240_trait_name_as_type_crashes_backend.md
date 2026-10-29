# Bug 240 — a trait name in a type position passes Resolve and crashes the backend

<!-- bug-ledger
summary: a trait name in a type position (`x as Marker`, a field, a payload, a parameter, a type argument, a trait or extern signature) passed Resolve and crashed the native backend (`unknown named type`) while `--interp` ran it; refused at Resolve, E0108 (MOD-1, TYPE-2). Found during R-0492 slice 6, 2026-10-25. Fixtures `error_240_*`, `regress_240_*`. Class 13.
fixtures: error_240_trait_cast_target.con error_240_trait_struct_field.con error_240_trait_enum_payload.con error_240_trait_parameter.con error_240_trait_type_argument.con error_240_trait_let_annotation.con error_240_trait_in_trait_signature.con error_240_trait_in_extern_signature.con regress_240_trait_bounds_and_type_param_shadow.con
class: 13 — TYPE-2 at the source level: a trait in a type position was taken for a type
silent: no
ruling: Violates: TYPE-2@1
-->

**Status:** FIXED (branch `wip/name-capture`)
**Found:** 2026-10-25, during the R-0492 slice 6 identity work, reproduced on main `70bcdeb6` and
on the train-17 candidate `593dc100`.
**Severity:** crash (native build); `--interp` ran the program.
**Confirmed real by Codex review, 2026-10-25:** Resolve accepts a trait declaration as a concrete type although TYPE-2 already excludes trait-object types, so the refusal enforces an existing rule independently of cast semantics or pending trait decisions.

## Reproducer

- `tests/programs/error_240_trait_cast_target.con`: `trait Marker { .. }` and `(x as Marker) as
  Int`. Before the fix `--interp` returned 42 and the native build panicked in EmitSSA
  (`tyToLLVMTy: unknown named type 'Marker'`).
- `error_240_trait_struct_field.con` and `error_240_trait_enum_payload.con`: a struct field and an
  enum payload naming the trait. Before the fix the native build printed a Layout panic
  (`tyToLLVM: unknown named type 'Marker'`) and `--interp` returned 42.
- `error_240_trait_parameter.con`, `error_240_trait_type_argument.con` and
  `error_240_trait_let_annotation.con`: a parameter, a type argument and a `let` annotation. Before
  the fix they reached Check as an opaque linear type (E0208, "never consumed") or an unresolved
  type (E0220).
- `error_240_trait_in_trait_signature.con` and `error_240_trait_in_extern_signature.con` (Codex
  review r2): a trait method's parameter and a foreign binding's parameter. Before the fix the
  first compiled (42) and the second reached the FFI-safety check (E0574).

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
the spelling to a trait and no type parameter shadows it), in every position it walks: signatures,
annotations, cast targets, type arguments (bug 215's `checkWrittenTys`), aliases and constants.
Resolve walks no declaration's own types through it (Check validates them), so
`declTraitTypesCheck` applies the same test to struct fields, enum payloads, newtype
representations, trait method signatures and foreign bindings' signatures. Code E0108 (MOD-1: the name denotes no type), with a TYPE-2 message. Not a
cast-semantics change: no cast to a trait ever had a meaning.

Regression legs in `run_tests.sh`: each refusal fixture (one position each) and the positive
control, `run_ok … 42` with `--interp` = 42.

## Structural cause

Class 13 ("documented rules with no enforcing site"): TYPE-2 is a guarantee no site enforced at
the source level; Resolve's type namespace answered "is a declaration" where the position asked "is
a type".

## Structural owner

R-0492/R-0514 (one resolver: a written type resolves to a type declaration's `TypeId`, which a
trait's identity is not).
