# Bug 220 — a `0x`, `0b` or `0o` prefix with no digits lexed as the literal 0

<!-- bug-ledger
summary: `0x`, `0b` and `0o` with no digit of their radix lexed as the literal 0 (`return 0x;` returned 0 on both backends). The lexer now reports a lexical error (E0001). Pinned by `error_220_empty_hex_literal.con`, `error_220_empty_binary_literal.con`, `error_220_empty_octal_literal.con` and `regress_220_radix_literals.con`. Found by the diagnostics probes of the post-R-0492 differential hunt
fixtures: error_220_empty_hex_literal.con error_220_empty_binary_literal.con error_220_empty_octal_literal.con regress_220_radix_literals.con
class: 13 — the grammar's radix literal needs a digit; the lexer read none as 0
silent: yes
exit: 0b
ruling: Violates: SYN-1@1
-->

**Status:** FIXED (branch `wip/diffhunt-1023`)
**Found:** 2026-10-23, by the diagnostics probes of the post-R-0492 differential hunt.
**Severity:** silent acceptance of a malformed program. `return 0x;` compiled and returned 0 on both
backends; `0b2` lexed as the integer tokens `0` and `2` (the `b` was consumed), a parse error only by accident.

Confirmed real by Codex review, 2026-10-25: SYN-1 and grammar/concrete.ebnf require radix digits; the lexer returned its zero accumulator for empty prefixes. The separate 0b2 case already failed parsing.

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

After a leading `0`, a radix letter starts a radix literal only when a digit of that radix follows;
otherwise the lexer consumes the letter and emits a lexical error (`a \`0x\` literal needs at least
one hexadecimal digit`, E0001 at `parse`). Pinned by `error_220_empty_hex_literal.con`,
`error_220_empty_binary_literal.con` and `error_220_empty_octal_literal.con`, whose `run_err` legs
assert the lexical message (the Codex review: `0b2` was already E0001 before the fix, so the code
alone did not detect a regression), and `regress_220_radix_literals.con` (203: every prefix and both
letter cases still lex).

Not covered: the browser compiler bundled with the site (`site/static/compiler/`) is a Wasm build
of an older compiler and still accepts a bare prefix until the site bundle is rebuilt; the editor
grammar (`editor/`) highlights its own literal shapes and was not changed.

## Structural cause

Class 13 ("documented rules with no enforcing site"): the grammar's literal shape was stated in the
EBNF and implemented by a digit loop that treats "no digits" as a value; nothing checked the lexer
against the grammar for the empty case.

## Structural owner

R-0498 (S3, exit 2l: every rule has an enforcing site). No gate checks the lexer against the
grammar's token descriptions (the EBNF's `INT` is a comment that `scripts/check_ll1.*` skips); a
negative lexical corpus run against the lexer is the proposed holder.
