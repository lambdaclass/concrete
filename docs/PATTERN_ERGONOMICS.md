# Pattern Ergonomics

Status: IN PROGRESS — ROADMAP Phase 6 #5. This is a compound usability block
built incrementally; this doc grows one section per landed sub-feature. Gated by
`scripts/tests/check_pattern_ergonomics.sh`.
Date: 2026-06-22

## Baseline (already present)

- `_` wildcard match arms; integer/bool literal patterns; variable-binding arms.
- `let`-destructuring including `let … else`; struct destructuring
  (`let Struct { fields } = expr;`).
- enum-variant patterns with field bindings (`Enum::Variant { f } => …`).

## Landed: integer range patterns

Match arms may match an integer (or `u8`, etc.) against a **range**:

```
match b {
    48..=57  => digit(),      // inclusive:  lo..=hi  matches lo .. hi, both ends
    97..122  => low_letter(), // exclusive:  lo..hi   matches lo .. hi-1
    _        => other(),
}
```

- **Inclusive** `lo..=hi` matches `lo <= x <= hi`. **Exclusive** `lo..hi` matches
  `lo <= x < hi` (the high endpoint is excluded).
- Bounds are integer literals or negated integer literals (`-5..=-1`). They are
  elaborated at the scrutinee's type, and the generated comparisons follow that
  type's signedness — a `u8` scrutinee compares **unsigned**, so `200..=255`
  works as written.
- Range arms compose with literal and `_` arms in the same `match`, and work in
  match-as-expression (value) position.
- **Exhaustiveness:** a range is *not* a catch-all. A `match` whose only arms are
  ranges/literals is non-exhaustive and requires a `_` arm (**E0534**) — the same
  rule literal arms follow.

Range patterns are not yet modelled in the proof path: a `predictable`/proof
function that uses a range pattern is reported as having an unsupported construct
(`range pattern`) rather than being silently mis-modelled. Lifting this is a
later step.

Known small gap: a range arm written against an *enum* scrutinee
(`match someEnum { 0..=9 => … }`) is type-nonsensical but not rejected with a
clean diagnostic in Check today — it is handled defensively in lowering. A
dedicated "range pattern on non-integer scrutinee" diagnostic is a follow-up.

Implementation: lexer tokens `..` / `..=`; `MatchArm.rangeArm` /
`CMatchArm.rangeArm`; lowered to a `lo <= scr && scr (<=|<) hi` comparison-branch
(`Concrete/Lower.lean`), mirroring the literal-arm branch.

## Landed: `if let` / `while let`

Conditional destructuring, desugared to a `match` at parse time (no new AST/Core/
lowering — reuses everything):

```
if let Option::Some { value } = opt { use(value); } else { fallback(); }
//  =>  match opt { Option::Some { value } => { use(value); }, _ => { fallback(); } }

while let Option::Some { value } = next() { consume(value); }
//  =>  while true { match next() { Option::Some { value } => { consume(value); }, _ => { break; } } }
```

- The pattern is an enum variant pattern (`Enum::Variant { binds }`) — the same
  form match arms and `let`-destructuring use.
- `if let` takes an optional `else` block; with no `else`, the non-matching case
  is a no-op (`_ => {}`).
- `while let` re-evaluates the scrutinee each iteration and ends the loop when it
  stops matching (the desugared `_ => break` targets the innermost loop).
- Because both desugar to `match`, exhaustiveness, binding, linear-cleanup, and
  lowering all behave exactly as the equivalent `match` would.

## Landed: match guards

Any arm may carry a guard — an `if <cond>` tested *after* the pattern matches and
its bindings are in scope. If the guard is false, matching falls through to the
next arm:

```
match x {
    n if n > 0 => positive(),
    n if n < 0 => negative(),
    _          => zero(),
}
match opt {
    Option::Some { value } if value > 10 => big(value),
    Option::Some { value }               => small(value),
    Option::None                         => none(),
}
```

- Guards work on every arm shape (enum / literal / range / variable) and in
  value-position matches.
- The guard sees the arm's pattern bindings (e.g. `value` above).
- **Exhaustiveness:** a guarded arm is *not* a catch-all — it can fall through.
  A guarded var arm does not make a match exhaustive, and a guarded enum arm does
  not cover its variant; both still require a fallback (E0534 / missing-variant
  otherwise). A guarded arm also never counts as a duplicate of an unguarded one.
- Guards are not yet modelled in the proof path: a proof/`predictable` function
  using a guard is reported as having an unsupported construct (`match guard`),
  not silently mis-modelled.

Implementation: `guard : Option Expr` on each `MatchArm` (and `Option CExpr` on
each `CMatchArm`); lowered as a test inserted after the pattern's bindings and
before the body, branching to the next arm's check on failure (`Concrete/
Lower.lean`, `finishMatchArmBody`).

## Landed: OR patterns

An arm may list several patterns separated by `|`; the arm matches if any
alternative matches:

```
match c {
    48..=57 | 97..=102 => hex_digit(),   // ranges
    1 | 2 | 3           => small(),       // literals
    E::A | E::B         => ab(),          // enum variants
    _                   => other(),
}
```

- Alternatives may be literals, ranges, enum variants, or bools, and an optional
  guard applies to the whole arm (`1 | 2 if c => …`).
- Implemented as a **parse-time desugar**: `P1 | P2 | … [if g] => body` becomes
  one ordinary arm per alternative, each with the same guard and body. No new
  AST/Core/lowering — every alternative reuses the existing arm machinery.
- Because of the desugar, all alternatives must bind the same variables that the
  body uses (an alternative that fails to bind a name the body references is a
  resolve error in that alternative's copy) — the standard OR-pattern rule.

## Still open (each lands as its own increment + gate section)

- nested patterns; `_` inside destructuring bindings
- match-on-reference ergonomics for `&T` / `&mut T`
- struct update syntax — `Struct { f: x, ..base }`
- tuple types (or a deliberate no-tuples decision)
