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

Implementation: lexer tokens `..` / `..=`; `MatchArm.rangeArm` /
`CMatchArm.rangeArm`; lowered to a `lo <= scr && scr (<=|<) hi` comparison-branch
(`Concrete/Lower.lean`), mirroring the literal-arm branch.

## Still open (each lands as its own increment + gate section)

- match guards — `pattern if cond => …`
- OR patterns — `A | B => …`
- `if let` / `while let`
- nested patterns; `_` inside destructuring bindings
- match-on-reference ergonomics for `&T` / `&mut T`
- struct update syntax — `Struct { f: x, ..base }`
- tuple types (or a deliberate no-tuples decision)
