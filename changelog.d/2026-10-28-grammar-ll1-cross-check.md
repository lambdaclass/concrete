### The three LL(1) checkers are one cross-checked gate (2026-10-28)

`scripts/check_ll1.py`, `.c` and `.rs` are kept as a deliberate diverse-implementation cross-check.
Each one gains `--canonical`, which prints every rule's nullability and FIRST set and every conflict
in one shared format. `scripts/tests/check_grammar_ll1.sh` requires the three outputs to be
byte-identical on `grammar/concrete.ebnf`, with no conflict. It also requires all three to refuse
the planted conflicting grammars in `tests/fixtures/grammar_ll1/`. CI, `make check-grammar` and the
gate inventory run it. Before, the C and Rust checkers ran only as ad-hoc CI steps and nothing
compared the three. The cross-check found a real defect in the Python checker: a FIRST-set memo that
stayed partial inside recursive rules. Python now computes FIRST sets by fixpoint iteration.
