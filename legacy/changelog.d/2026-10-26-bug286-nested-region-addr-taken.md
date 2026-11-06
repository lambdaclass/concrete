### Deeply Nested Branches Are Lowered Without Re-walking Each Level (bug 286, 2026-10-26)

Before each `if`, `match`, loop or `&&`/`||` right operand, Lower looked through the code inside
it for locals whose address is taken, and a branch nested n deep was looked through again at every
level above it, so lowering took time n² (8000 nested `if`s: 1.3 s, now 0.12 s). A nested branch
now starts from what its enclosing branch already found and looks again only when that could
change its decision. The generated code is unchanged
([bug 286](docs/bugs/286_lower_nested_region_addr_taken_rewalks.md)).
