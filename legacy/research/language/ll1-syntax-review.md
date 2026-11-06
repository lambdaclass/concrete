# LL(1)-Preserving Syntax Review

Status: open

This note records the syntax changes worth considering later, but only if they preserve Concrete's LL(1) parser discipline and are justified by real workload pressure.

## Constraint

Concrete should keep the parser LL(1). Syntax cleanup is allowed only when it:

- keeps parsing local and token-driven
- avoids context-sensitive parse decisions
- pushes inference or disambiguation into elaboration rather than the grammar
- does not add multiple competing syntaxes for the same construct

If a syntax improvement requires parser-level semantic lookup, deep lookahead, or scope-sensitive parsing, it should be rejected.

## Candidate Changes Worth Evaluating

### 1. Unify Qualification Syntax

Current surface:

```con
math::add(...)
Result#Ok { value: 42 }
```

Candidate:

```con
math::add(...)
Result::Ok { value: 42 }
```

Why it is LL(1)-safe:

- this is a token-level qualification change, not a context-sensitive grammar change
- `::` remains a distinct qualification token
- the parse shape stays regular: qualified-name followed by the existing construction or pattern form

Why it may be worth doing:

- removes an obvious inconsistency between module qualification and enum variant qualification
- matches user expectations better
- makes the surface feel more uniform without adding a second naming model

### 2. Clean Up Declaration Modifier Order

Current surface:

```con
pub struct Copy Pair { ... }
```

Candidate directions:

```con
pub Copy struct Pair { ... }
```

or

```con
pub struct Pair : Copy { ... }
```

Why it is LL(1)-safe:

- this is a local declaration-form adjustment
- the parser still sees a fixed keyword/declaration sequence
- there is no new ambiguity in expression parsing

Why it may be worth doing:

- the current order is unusual enough to slow readers down
- declaration syntax should feel unsurprising before the first-release surface freezes

### 3. Improve Generic Construction Ergonomics in Typed Contexts

Current surface:

```con
let p = Pair::<Int, Int> { left: 1, right: 2 };
```

Candidate:

```con
let p: Pair<Int, Int> = Pair { left: 1, right: 2 };
```

Why it can remain LL(1):

- the parser already treats `Ident { ... }` as a local construction form
- elaboration, not parsing, can infer the instantiation from surrounding type context
- no new lookahead or semantic parser tricks are required if omitted generic arguments are accepted only in already-unambiguous constructor forms

Guardrails:

- keep explicit `::<...>` available where needed
- do not allow parser-driven generic omission in ambiguous positions
- do not blur type syntax and value syntax outside clearly typed construction forms

Why it may be worth doing:

- `Pair::<Int, Int> { ... }` is parser-regular but visually heavy
- typed construction should be easy to read if the surrounding type already fixes the instantiation

### 4. Revisit Explicit Field Visibility Only If Examples Force It

Current surface:

```con
struct Pair {
    left: Int,
    right: Int,
}
```

Candidate:

```con
struct Pair {
    pub left: Int,
    pub right: Int,
}
```

Why it is LL(1)-safe:

- `pub` is just a local optional field modifier
- no expression grammar changes are involved

Why this is lower priority:

- the current all-public-by-default model is simple
- explicit field visibility only matters if real stdlib and package boundaries show genuine encapsulation pressure

## Explicit Non-Goals

The following should be rejected unless the parser strategy changes:

- bare enum variants such as `Ok` without qualification
- block `defer`
- parser-driven generic inference
- multiple equivalent syntaxes for enum construction or matching
- tuple-style enum payloads added alongside brace-style payloads if they complicate the grammar

## Timing

This review should happen:

- after the pressure sets
- during stdlib/examples shaping
- before the first-release surface freeze

That is late enough for the changes to be evidence-driven, but early enough to avoid freezing an obviously awkward surface.

## Related Notes

- [Module Qualification](./module-qualification.md)
- [LL(1) Grammar](../compiler/ll1-grammar.md)
- [External LL(1) Checker](../compiler/external-ll1-checker.md)
