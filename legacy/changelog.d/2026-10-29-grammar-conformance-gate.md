### The parser is checked against the grammar (2026-10-29)

`scripts/tests/check_grammar_conformance.sh` checks the half of SYN-1 that nothing checked: that
`Concrete/Frontend/Parser.lean` accepts exactly the programs `grammar/concrete.ebnf` derives.
- From a fixed seed, it generates derivations covering every alternative, optional and repetition
  of the grammar, and requires the parser to accept them.
- It mutates them by one token and requires the parser to refuse every mutant that an exact
  reference recognizer, built from the same EBNF, refuses.
- It counts every place in the parser that reads past one token or can undo a decision.
- It checks that every reserved word of the lexer appears in the grammar.

Today's 41 differences are listed in `grammar/conformance_known.tsv`, each with a category and a
reduced example. The lookahead sites are in `grammar/parser_lookahead_sites.tsv` and the reserved
words in `grammar/conformance_keywords_known.tsv`. All three lists may only shrink. The
differences include:
- syntax the grammar still spells the old way (`E#V` where the language writes `E::V`);
- rules the parser enforces that the grammar does not state;
- conflicts the parser resolves one way;
- forms the parser accepts that the grammar refuses: bodyless functions, missing commas between
  fields and variants, empty enums, raw pointers without `mut`/`const`.

The gate runs in CI and in the pre-push hook for parser, lexer or grammar changes.
