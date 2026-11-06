### Expressions Nested Through Value `if` and `match` Are Checked Without Re-searching Each Level (bug 298, 2026-10-26)

The checker makes sure no `return`, `break` or `continue` hides inside an operand, and an operand
chain nested through value `if`s or `match`es was searched again at every level, so 8000 levels
took 2.7 s to check. The answer of the outer search now reaches the condition and the branch
values, as it already did through parentheses and operators: 21 ms. Diagnostics are unchanged
([bug 298](docs/bugs/298_operand_escape_search_value_branches.md)).
