# Bug 220 — a `0x`, `0b` or `0o` prefix with no digits lexed as the literal 0

**Status:** FIXED (branch `wip/diffhunt-1023`)
**Found:** 2026-10-23, by the diagnostics probes of the post-R-0492 differential hunt.
**Severity:** silent acceptance of a malformed program. `return 0x;` compiled and returned 0 on both
backends; `0b2` lexed as `0` followed by `b2`.

## Reproducer

`tests/programs/error_220_empty_hex_literal.con`:

```con
fn main() -> Int {
    return 0x;
}
```

Observed on main `dcc48dc1`: both backends return 0. Expected: a lexical error; the grammar's
integer literal is `0x hex | 0b bin | 0o oct` (`grammar/concrete.ebnf`), with at least one digit.

## Cause

`Lexer.lexNumberLoop` consumed the radix letter after a leading `0` and started the digit loop
with accumulator 0; the loop stops at the first non-digit and returns what it accumulated, so no
digit at all yielded the literal 0.

## Fix

The radix prefix is consumed only when a digit of its radix follows; otherwise the lexer emits a
lexical error (`a \`0x\` literal needs at least one hexadecimal digit`, E0001 at `parse`). Pinned by
`error_220_empty_hex_literal.con` and `error_220_empty_binary_literal.con` (`run_err ... (E0001)`)
and `regress_220_radix_literals.con` (203: every prefix and both letter cases still lex).

## Structural cause

Class 13 ("documented rules with no enforcing site"): the grammar's literal shape was stated in the
EBNF and implemented by a digit loop that treats "no digits" as a value; nothing checked the lexer
against the grammar for the empty case.

## Structural owner

R-0498 (S3, exit 2l: every rule has an enforcing site); the grammar-conformance gate
(`check_grammar*.sh`) is where a negative literal corpus would hold it.
