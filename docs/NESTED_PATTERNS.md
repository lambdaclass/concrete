# Nested patterns — deferred (workload-gated)

Status: DEFERRED — not in V1. ROADMAP Phase 6 #5 (the last open pattern item).
Gated by `scripts/tests/check_nested_patterns.sh`.
Date: 2026-06-22

## What "nested patterns" would be

Destructuring more than one level deep in a single pattern:

```
match e {
    E::Wrap { P { x, y } } => use(x, y),   // NOT in Concrete V1
    Some(Some(n))          => use(n),       // NOT in Concrete V1
}
```

Today a match arm binds each variant field to a single **name**
(`bindings : List String`); the value at each field is bound whole. Going deeper
requires a second step.

## The decision

Nested patterns are **deferred**. A match arm destructures exactly one level; to
reach deeper, bind the field and then field-access or `match` again:

```
// One-level destructure + field access (structs):
match e { E::Wrap { p } => use(p.x, p.y) }

// One-level destructure + nested match (enums):
match outer {
    Some(inner) => match inner { Some(n) => use(n), None => fallback() },
    None        => fallback(),
}
```

This is slightly more verbose but fully expressive — there is no program you can
write with nested patterns that you cannot write with one-level destructure plus
an inner access/`match`.

## Why deferred (not built)

- **Large, cross-cutting refactor for sugar.** Nested patterns require replacing
  the flat `bindings : List String` with a recursive `Pattern` type (var,
  wildcard, struct/enum sub-destructure) threaded through parse → resolve → check
  → elab → lower (recursive field extraction + binding) → the proof path. That is
  the heaviest change in the whole pattern-ergonomics block — for a pure
  convenience that already has a clean, equally-expressive workaround.
- **Workload-driven, not symmetry-driven.** No current example or workload is
  blocked by the one-level limit; decoders/parsers/interpreters are well served
  by ranges, guards, OR, `if let`/`while let`, match-on-`&T`, struct-update, and
  one-level destructure (all of which DID land). Building nested patterns now
  would be adding a big feature because it is conventional, not because a
  workload demands it — the same call made for const generics
  (`docs/CONST_GENERICS_V1.md`) and tuples (`docs/TUPLES.md`).

## If the verdict flips

Build nested patterns when a real workload shows the one-level + inner-`match`
workaround is genuinely untenable (e.g. deep protocol ASTs where the nesting is
pervasive and the inner matches dominate the code). The build is a recursive
`Pattern` type plus recursive destructuring in check/elab/lower. Until then this
is a closed decision, not an open hole: deeper-than-one-level pattern syntax
stays a clean parse error.
