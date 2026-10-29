# Bug 318 — A struct literal in an if, while or match head is accepted without parentheses when its name is uppercase (OD16)

<!-- bug-ledger
summary: A struct literal in an if, while or match head is accepted without parentheses when its name is uppercase, and an uppercase constant before the head's block is refused (E0001), where SYN-4 (OD16, decided 2026-10-29) refuses every unparenthesized struct literal in a head and reads a name before `{` as a value
coverage: reproducer tests/regressions/bug318_a_struct_literal_in_an_if_while_or_match_head_is/repro.con, found checking main against OD16 on 2026-10-29; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 42) and check_spec.sh S4 (the pending SYN-4 examples)
class: 13
silent: no
ruling: Violates: SYN-4@1
-->

**Status:** OPEN
**Found:** 2026-10-29, by checking main against the owner's answer to OD16 (SYN-4): struct literals and uppercase constants in `if` and `while` heads, native against `--interp`, on main 0bf8350a.
**Severity:** loud in one direction and over-strict in the other, never silent: a program SYN-4 refuses is accepted (both consumers agree and run it), and `if n > LIMIT { ... }` or `while n < LIMIT { ... }` with a constant `LIMIT` is refused with E0001.

Codex confirmation 2026-10-29: CONFIRMED — Main's acceptance of an unparenthesized uppercase struct literal in a head conflicts with the decided OD16 rule (the struct-literal branch at Parser.lean:674 checks uppercase spelling with no head restriction); accepting `n < LIMIT {` follows from the uniform rule with no other owner decision; whether a literal inside a call's argument list in a head counts as parenthesized needs the owner (OPEN-34).

## Reproducer

`tests/regressions/bug318_a_struct_literal_in_an_if_while_or_match_head_is/repro.con`: `if P { v: 1 }.v == 1 { return 42; }`.

## Observed

On main 0bf8350a (built on OVH) the program compiles and the binary and `--interp` return 42.
`if (P { v: 1 }).v == 1 { ... }` returns 42 too, `while n < LIMIT { ... }` and `if n > LIMIT { ... }`
are refused at parse (E0001), and `if same(p, P { v: 1 }) { ... }` returns 42.

## Expected

[SYN-4@1](../spec/syntax.md#SYN-4) (OD16): the unparenthesized literal is refused at parse (E0001),
the parenthesized one is accepted, and a constant before the head's block is read as a value.

## Cause

`Concrete/Frontend/Parser.lean`: the struct-literal branch of primary-expression parsing starts a
literal after any uppercase name followed by `{`, with no head restriction; the head wrapper only
adds a hint to the E0001 it causes (bug 212's provisional case rule, SYN-1 and
`grammar/concrete.ebnf`).

## Structural cause

Class 13: a decided rule with no enforcing site. The parser resolves the FIRST/FOLLOW conflict
between a struct literal and a head's block by a spelling convention instead of by the head
context, so the grammar's answer depends on capitalization.

## Structural owner

R-0500 (the specification as the judge of each rule): the parser carries a "no struct literal"
context into heads, as the grammar's head production states, and `scripts/check_ll1.py` checks the
head production without the case rule.

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg (it runs and exits
42), and SYN-4's pending examples record main's outcome. Meanwhile a user parenthesizes a struct
literal in a head and, for a constant before the block, the head (`if (n > LIMIT) { ... }`).
