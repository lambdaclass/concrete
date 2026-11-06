### Bug 329: The LL(1) checkers never computed FOLLOW sets: a FIRST/FOLLOW conflict at an optional, repetition or nullable alternative passed, so SYN-1's LL(1) claim was unchecked there (2026-10-29)

`scripts/check_ll1.py`, `.c` and `.rs` now compute FOLLOW sets and report FIRST/FOLLOW conflicts,
not only FIRST/FIRST ones. The grammar has eight. They include the C-style `for` header and the
`as T <` cast, which the parser already resolved its own way. All eight are listed for the owner
in `grammar/ll1_known_conflicts.tsv`, and the grammar itself is unchanged. `check_grammar_ll1.sh`
refuses two planted FIRST/FOLLOW grammars in all three checkers and holds the real grammar's
conflicts to that list, which may only shrink
([bug 329](docs/bugs/329_the_ll_1_checkers_never_computed_follow_sets_a_f.md)).
