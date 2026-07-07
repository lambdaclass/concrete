# Pattern Ergonomics

Status: CLOSED — ROADMAP Phase 6 #5. The compound usability block is complete
for V1: built features are gated by `scripts/tests/check_pattern_ergonomics.sh`,
and the two non-built items are explicitly workload-gated decisions rather than
hidden holes.
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
(`Concrete/IR/Lower.lean`), mirroring the literal-arm branch.

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

## Landed: match on a reference scrutinee (`&T` / `&mut T`)

`match` auto-derefs a reference scrutinee, so you can match the pointee directly:

```
fn classify(x: &i32) -> i32 {
    match x {
        0     => zero(),
        1..=9 => small(),
        n     => big(n),   // `n` binds the value (i32), not the reference
    }
}
fn tag(e: &E) -> i32 { match e { E::A { x } => x, E::B => 0 } }  // reads through &E
```

- Enum scrutinees behind `&E` read the tag and payload through the pointer.
- Scalar scrutinees behind `&i32` are dereferenced once before literal/range
  comparisons and variable bindings, so `n` binds the value and `0 => …` /
  `1..=9 => …` compare against the value (not the pointer). This matches Check's
  auto-deref of the scrutinee type (`Concrete/IR/Lower.lean`, value-pattern branch).
- A non-`Copy` value borrowed in a match is still linear: it is read, not
  consumed, so it must be consumed/dropped elsewhere (an unconsumed one is E0208,
  the normal linearity rule).

## Landed: struct functional update (`..base`)

A struct literal may end with `..base` to copy every not-listed field from
another value of the same struct type:

```
let next = State { round: r + 1, ..state };   // round overridden, rest from state
let copy = State { ..state };                  // every field from state
```

- The listed fields take their given values; every omitted field is filled from
  `base`. `base` must be the same struct type (else E0220).
- Desugared in Elab to `base.field` for each omitted field (`Concrete/Elab/Elab.lean`),
  so no new Core/lowering. **Use a variable (or simple place) as the base** — a
  complex base expression is re-read once per copied field.
- Cleanest for `Copy` structs (the typical state-update case); a non-`Copy` base
  follows the usual linearity/borrow rules through the generated field reads.

## Landed: `_` wildcard in destructuring bindings

A field binding named `_` in an enum-variant (or `let`-) destructure is a true
wildcard: it holds its field position so the other fields still bind correctly,
but it is **not** added to scope (reading `_` is E0100), and its field is not
even loaded in codegen.

```
match e {
    E::Tri { _, b, _ } => use(b),   // only `b` is bound; the two `_` fields ignore
}
let E::Pair { a, _ } = p;           // binds `a`, ignores the second field
```

(Previously `_` was a readable binding named `_` — now corrected to a wildcard.)

## Closed / Workload-Gated Decisions

- **Tuples** — no anonymous tuples in V1; use named structs. See
  `docs/TUPLES.md` (gated by `scripts/tests/check_no_tuples.sh`).
- **Nested patterns** — deferred. Use staged destructuring, field access, or
  nested `match`; see `docs/NESTED_PATTERNS.md` (gated by
  `scripts/tests/check_nested_patterns.sh`). The trigger to reconsider is a
  real workload where the workaround obscures correctness or materially bloats
  pattern-heavy code.

No Phase 6 #5 feature remains untracked: ranges, `if let` / `while let`, match
guards, OR patterns, match-on-reference, struct update, and `_` destructuring
wildcards are implemented; tuples and nested patterns are deliberate
workload-gated decisions.
