### A Radix Prefix Needs A Digit (bug 220, 2026-10-23)

`0x`, `0b` and `0o` with no digit of their radix lexed as the literal 0, so `return 0x;` compiled and
returned 0. The lexer now reports a lexical error (E0001). Pinned by two `error_220_*.con` fixtures
and `regress_220_radix_literals.con`.
