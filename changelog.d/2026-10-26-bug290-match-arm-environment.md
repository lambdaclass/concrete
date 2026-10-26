### Matching Many Enum Values Is Checked and Lowered in Linear Time (bug 290, 2026-10-26)

At every arm of every `match`, the checker looked at every non-Copy value in scope, and the checker
and the lowering found each enum by searching the list of all enums, so a function matching n
enum values of n enum types took time n² (8000: checking took 10.1 s, now 0.15 s). The checker
now looks only at the values the arm itself introduced or changed, and enums are found by name in
a table. Diagnostics and generated code are unchanged
([bug 290](docs/bugs/290_check_match_arm_environment_walks.md)).
