# Type Aliases

Status: IMPLEMENTED — ROADMAP Phase 6 #3. Gated by
`scripts/tests/check_type_alias.sh` (fixtures in `tests/programs/type_alias/`).
Date: 2026-06-22

A type alias gives an existing type a second name. It is a **transparent**
alias, not a nominal type: the alias and its target are the same type
everywhere, with identical layout, extraction, and proof identity. For a
*distinct* type that wraps a representation (validated, with its own identity and
methods) use a `newtype` instead (see `docs/VALIDATED_WRAPPERS.md`).

## Syntax

```
type Digest = [u8; 32];
type Id      = i32;
type Point   = P;          // alias of a struct
pub type Meters = i32;     // public alias, importable like any public item
```

An alias is `type Name = <any type>;`. There are no alias type parameters in V1
(`type Pair<T> = …` is not provided); an alias may target a *generic
instantiation* (`type IntBox = Box<i32>;`), which is the common need.

## Transparency

The alias and its target are interchangeable in every position:

- as a `let`/field/parameter/return type;
- an alias-typed value flows where the underlying type is expected, and vice
  versa (`type Id = i32` ⇒ an `Id` is an `i32` and an `i32` is an `Id`);
- aliases compose: a **chain** `type C = B; type B = A; type A = i32` resolves
  `C` all the way to `i32`, and an alias **nested** in another type
  (`type Arr = [E; 3]` with `type E = i32`) expands to `[i32; 3]`.

Aliases are expanded structurally and transitively at type-resolution time
(`closeAliasMap` / `expandAliasDeep`), so downstream phases — layout, the
`Copy`/repr check, monomorphization, lowering — see the underlying type. In
particular a `Copy` struct may have a field typed by an alias to a `Copy` type
(`type Id = i32; struct Copy S { a: Id }`) and still be `Copy`.

## No new identity (transparent to layout and proof)

Because the alias *is* the underlying type, a program written with an alias and
the same program written with the underlying type produce **byte-identical**
`--report layout` and `--report fingerprints`. Aliasing never shifts a struct's
size/offsets and never changes a function's semantic body fingerprint, so it
cannot affect proof/evidence identity. The gate asserts this equivalence
directly.

## Rejected forms

- **Unknown target** — `type Bad = Nope;` (where `Nope` is not a known type) is
  rejected at the alias declaration with **E0108** (`unknown type 'Nope'`),
  even if the alias is never used. Alias targets are validated, not silently
  accepted.
- **Recursive alias** — a self-referential alias, directly (`type A = A;`) or
  through a cycle (`type A = B; type B = A;`), is rejected with **E0112**
  (`recursive type alias 'A'`), a dedicated diagnostic rather than a confusing
  downstream type-mismatch.

## Not provided (V1)

- Alias type parameters (`type Pair<T> = (T, T)`) — target a concrete generic
  instantiation instead.
- Opaque / distinct aliases — use `newtype` for a type with its own identity.
