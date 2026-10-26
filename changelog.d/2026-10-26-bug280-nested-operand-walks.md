### Deeply Nested Expressions Are Checked and Elaborated Without Re-walking Each Level (bug 280, 2026-10-26)

Check searched a nested expression's operands for an escaping `return`/`break` at every level, and
Check and Elab asked whether each operand is a literal-only tree at every level, each a walk of
the subtree below, so an expression nested n deep took time n² (depth 20000: check 3.8 s, now
8 ms). The literal flexibility of a whole operator region is computed in one walk and handed
down, and an operand of an already-searched operand inherits its answer. Diagnostics and output are
unchanged ([bug 280](docs/bugs/280_nested_expression_subtree_rewalks.md)).
