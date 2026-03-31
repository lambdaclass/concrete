# LL(1) Grammar

Status: process

This note records the Concrete rule that the surface language should stay strictly LL(1), not merely "easy to parse in practice."

## Why This Matters

Concrete is trying to stay:
- explicit
- auditable
- mechanically understandable
- proof-friendly

Keeping the surface grammar LL(1) supports all four goals.

In practice, this means:
- every parse decision should be possible with one token of lookahead
- syntax should not rely on parser-state rewind or speculative parsing
- new features should be redesigned or rejected if they require parser cleverness

This is not just a parser implementation preference. It is a language-design constraint.

## Working Rule

Concrete should accept:
- clear leading-keyword syntax
- explicit separators
- syntax choices that can be left-factored cleanly

Concrete should reject or redesign:
- syntax that requires save/restore backtracking
- syntax that requires hidden contextual disambiguation beyond normal parser state
- punctuation-heavy forms that collide with existing productions

## Practical Parser Rule

The parser should not rely on:
- speculative parse followed by rewind
- "try one production, then try another" logic
- hidden semantic knowledge to disambiguate syntax

Bounded lookahead inside the lexer is fine for tokenization:
- multi-character operators
- float-vs-dot disambiguation
- char literal vs label tokenization

The main concern is parser-state backtracking.

## Previously Known Non-LL(1) Sites (All Resolved)

All five backtracking sites have been eliminated:

1. **Top-level `mod` handling** — left-factored: `mod name` consumed once, then `{` vs `;` decides.
2. **Method receiver parsing** — `&` commits in method-parameter position; must be `&self` or `&mut self`.
3. **Type turbofish** — `::` in type position commits; `<` is required after `::`.
4. **Expression-level `name::...`** — `::` commits; must be `<` (turbofish) or ident (module path).
5. **Enum-dot fallback** — moved to `parsePostfix`; decided by uppercase check after `.field` consumed.

## Acceptance Criteria

Concrete can honestly claim the parser is LL(1) when all of the following are true:

1. No remaining parser-state rewind is needed for normal parsing decisions.
2. Every parse choice can be explained with one token of lookahead plus ordinary parser context.
3. The syntax reference documents any contextual distinctions that remain.
4. New syntax proposals are evaluated explicitly for LL(1) impact before adoption.

## Design Checklist For New Syntax

Every new syntax idea should answer:

1. What is the first token that introduces this form?
2. What existing production does it overlap with?
3. Can the parser decide with one token of lookahead?
4. Does this require save/restore or speculative parse?
5. If yes, can the syntax be left-factored or keyworded more explicitly?

If the answer still requires parser tricks, the syntax should probably change.

## Preferred Syntax Style

Concrete should continue preferring:
- keywords over overloaded punctuation
- explicit block introducers
- explicit separators
- declaration forms that start with distinct tokens

Examples that fit this well:
- `trusted fn`
- `trusted impl`
- `extern fn`
- `newtype`
- `borrow x as y in R`
- `[T; N]`

## Relationship To Other Docs

- [ROADMAP.md](../ROADMAP.md) records the LL(1) parser cleanup as a completed milestone and keeps the LL(1) commitment active as a standing language rule.
- [docs/LANGUAGE_INVARIANTS.md](../docs/LANGUAGE_INVARIANTS.md) records the LL(1) commitment as a language invariant.
- [docs/ARCHITECTURE.md](../docs/ARCHITECTURE.md) describes the parser as the syntax-only phase; this note sharpens what that means operationally.
