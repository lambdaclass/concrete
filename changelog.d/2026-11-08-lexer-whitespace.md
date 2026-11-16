### Slice-0 whitespace and trivia (2026-11-08)

The surface specification and lexer now cover exactly space, tab, LF and CR as whitespace, plus
maximal runs of whitespace and `//` comments. The total scanner is proved sound and complete
against the declarative judgment, with kernel-checked conformance cases for accepted trivia and
non-trivia controls. No tokens, diagnostics, block comments, shebangs or other Unicode whitespace
are introduced.
