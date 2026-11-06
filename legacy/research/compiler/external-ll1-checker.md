# External LL(1) Grammar Checker

**Status:** Open

This note captures a practical way to keep Concrete's surface syntax strictly LL(1) over time without turning the production compiler into a parser-generator project.

## Why add an external checker?

Concrete already has a hand-written lexer and parser, and the parser has now been cleaned up to remove the known save/restore backtracking sites.

That is good, but it is still easy for future syntax changes to drift back toward:

- hidden parser-state tricks
- accidental ambiguity
- “just one more special case” parsing

An external LL(1) checker is a **guardrail**, not a replacement parser.

Its job is to make sure the **intended surface grammar** stays LL(1) even if the production parser evolves.

## What it should consist of

Two artifacts:

1. `grammar/concrete.ebnf`
   - a compact reference grammar for the intended surface syntax
   - human-readable
   - updated alongside syntax changes

2. `scripts/check_ll1.py`
   - parses the grammar file
   - computes FIRST and FOLLOW sets
   - reports LL(1) conflicts clearly
   - exits non-zero in CI if any conflict appears

The point is not to build a second full compiler parser.
The point is to have a machine-checkable statement that the language grammar remains LL(1).

## Why Python?

Use Python for the checker.

Reasons:

- fast to write
- ideal for text processing and set operations
- easy to run in CI
- small maintenance burden
- no need to add a Rust crate or a Lean proof artifact just to get the basic guardrail

Rust would work, but it adds more build/tooling friction for a small quality tool.
Lean would be interesting later if a formally verified grammar checker became important, but that is not the right first move.

## What it should enforce

The checker should fail if:

- two productions for the same nonterminal have overlapping FIRST sets
- epsilon productions create FOLLOW conflicts
- a syntax change makes a previously LL(1) choice require parser-state rewind

It should print:

- the nonterminal
- the conflicting productions
- the overlapping tokens

The output should be simple enough that syntax changes are easy to diagnose.

## Relationship to the hand-written parser

The intended model is:

1. production compiler uses the hand-written lexer/parser
2. reference grammar describes the same intended language
3. external checker proves the grammar remains LL(1)

This means the project can keep:

- a hand-written parser optimized for good diagnostics and direct control
- an independent grammar guardrail to stop syntax drift

## CI guardrail

This should become part of normal quality checks:

1. run the LL(1) grammar checker
2. fail if the grammar is no longer LL(1)
3. optionally also grep the parser for forbidden rewind/backtracking patterns

The strongest setup is:

- grammar says the language is LL(1)
- checker confirms there are no LL(1) conflicts
- parser implementation does not reintroduce rewind logic

## What this does not do

This does **not** prove:

- the production parser exactly matches the grammar
- the parser is bug-free
- the lexer/tokenization is formally verified

It is still worth doing because it protects the most important design commitment:

- future syntax changes must remain LL(1)

## Recommended rollout

1. write the compact grammar file
2. implement the Python checker
3. add it to CI
4. add a small parser audit check forbidding new save/restore-style rewinds

After that, new syntax work should require:

- grammar updated
- LL(1) check still passing
- no parser backtracking added

## Why this matters for Concrete specifically

LL(1) is not just a parser preference here.
It is one of the language’s core design constraints:

- syntax should stay mechanically understandable
- tooling should stay cheap and predictable
- proofs should not be undermined by a slippery surface grammar
- the language should resist feature creep that only works because “the parser can figure it out”

An external checker makes that commitment enforceable instead of aspirational.
