# Bug 331 — C-style for loops are accepted and the counted loop for i from a until b is not (OD47)

**Status:** OPEN
**Found:** 2026-10-29, by checking main against the owner's decision OD47 (loops are `while` and a counted range loop): the three C-style `for` forms, the counted loop and `from`/`until` as names, native against `--interp`, on main ae896787.
**Severity:** a refusal and an over-acceptance, never silent: the C-style forms OD47 removes are accepted, the counted loop it adds is refused at parse, and `from` and `until` are ordinary identifiers.

Codex confirmation 2026-10-29: CONFIRMED (all five measurements): `parseFor` (Concrete/Frontend/Parser.lean) accepts `for (let ...; cond; step)`, `for (cond)` and `for { ... }` and demands `(` after `for`, so `for i from a until b` is E0001; `lookupKeyword` (Concrete/Frontend/Lexer.lean) has no `from` or `until`; `grammar/concrete.ebnf`'s `for_tail` keeps the removed forms; Check, Elab (`.while_` with the step appended, no variant) and Lower implement the C-style design.

<!-- bug-ledger
summary: C-style for loops are accepted and the counted loop for i from a until b is not (OD47)
coverage: reproducer tests/regressions/bug331_c_style_for_loops_are_accepted_and_the_counted_l/repro.con, found checking main against OD47 on 2026-10-29; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 42) and check_spec.sh S4 (STMT-6's pending examples)
class: 13
silent: no
ruling: Violates: STMT-6@3
-->

## Reproducer

`tests/regressions/bug331_c_style_for_loops_are_accepted_and_the_counted_l/repro.con`: a C-style `for` summing 0..6.

## Observed

On main ae896787 (built on OVH): `for (let mut i: Int = 0; i < 7; i = i + 1)`, `for (i < 42)` and
`for { ... }` compile and return 42 on both consumers; `for i from lo until hi { ... }` is refused
at parse (E0001, "expected (, got ident(i)"); `let from: Int = 40;` is accepted.

## Expected

[STMT-6@3](../spec/statements.md#STMT-6) (OD47): only `while` and the counted loop `for i from a
until b`; the C-style forms refused at parse. Whether `from` and `until` are reserved or contextual is OD50. In Core the counted loop
desugars to `while` with its termination measure; STMT-11@2 orders the index advance after the
body's exit sequence.

## Cause

`Concrete/Frontend/Parser.lean`, `parseFor`, and `grammar/concrete.ebnf`'s `for_tail` implement the
C-style design; `Concrete/Frontend/Lexer.lean`, `lookupKeyword`, does not reserve `from` or `until`;
Check, Elab and Lower carry the step.

## Structural cause

Class 13: a decided rule with no enforcing site. The rule changed on 2026-10-29 and main implements
the earlier loop design.

## Structural owner

Exit 2q (one migration of std, examples and tests to the decided rules), with the grammar change on
its own branch and the Core desugaring by R-0499.

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg, and STMT-6's pending
examples record main's outcomes. Programs keep using `while` and the C-style forms until exit 2q
migrates them.
