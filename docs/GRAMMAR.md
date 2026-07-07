# Grammar Reference

Status: pointer + reference — ROADMAP Phase 6 #2. The canonical grammar is the
EBNF file; this page indexes it and the surrounding syntax facts.

## Canonical grammar

The single source of truth for Concrete's syntactic shape is
[`grammar/concrete.ebnf`](../grammar/concrete.ebnf). The hand-written
recursive-descent parser (`Concrete/Frontend/Parser.lean`) must accept exactly that
language. The grammar is **LL(1)** — every alternation resolves with one token of
lookahead — and three independent checkers verify this mechanically in CI:

- `scripts/check_ll1.py` (Python)
- `scripts/check_ll1.c` (C)
- `scripts/check_ll1.rs` (Rust)

A change that breaks LL(1) fails CI. The EBNF is kept in sync with the parser as
syntax lands (e.g. it covers range patterns `lo..=hi`/`lo..hi`, OR patterns
`A | B`, match guards `pat if cond`, `if let`/`while let`, struct functional
update `S { ..base }`, field punning, and `defer <call>`).

## Reserved keywords

`mod import pub fn let mut return if else while for match break continue defer
borrow as in struct enum trait impl type newtype const extern trusted ghost
assert assume true false with` — plus the capability/type identifiers that are
contextual (`Int`, `Uint`, `i8`…`u32`, `bool`, `char`, `String`, capability names
like `Std`/`Alloc`/`Console`/`File`/`Unsafe`).

## Attribute syntax

Attributes use `#[name(args)]` form and are a fixed, compiler-recognized set
(e.g. `#[test]`, `#[spec(...)]`, `#[proof_by(...)]`, `#[proof_fingerprint("...")]`,
`#[invariant(...)]`, `#[variant(...)]`, `#[overflow_checked]`, `#[copy]`). There
is no user-defined attribute or macro/`#[derive]` mechanism — see
[MACRO_STANCE.md](MACRO_STANCE.md). Unknown attributes are a parse error.

## Contract / proof syntax

`requires` / `ensures` clauses, `assert`/`assume` statements, and `ghost let`
bindings are the contract surface; see the proof/contract docs
([PROOF_WORKFLOW.md](PROOF_WORKFLOW.md), the contract sections of the obligation
docs). These are erased metadata (no runtime effect beyond `assert`).

## Negative parser fixtures

Syntax that is deliberately rejected is pinned by gates and fixtures rather than
left implicit — e.g. tuples (`scripts/tests/check_no_tuples.sh`), nested patterns
(`check_nested_patterns.sh`), block-form `defer` (`check_defer.sh`), macros/derive
(`check_no_macros.sh`), and the `tests/wrong-code/` corpus. Each documents the
rejection with its diagnostic code.
