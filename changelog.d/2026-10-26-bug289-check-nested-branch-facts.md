### Deeply Nested Branches Are Checked Without Re-walking Each Level (bug 289, 2026-10-26)

At every `if`/`else` and `match`, the checker asked whether each branch can fall off its end,
which looks down through the nested branches, and it rebuilt the list of names bound in nested
blocks at every level, so a nest n deep took time n² to check (8000 nested `match`es: 3.3 s, now
0.06 s). The answers are computed once for a whole nest and handed down, and the names are
collected in one pass. Every diagnostic is unchanged
([bug 289](docs/bugs/289_check_nested_branch_end_facts_rewalks.md)).
