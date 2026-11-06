### Deeply Nested Bodies Build Their Identity-Use List in One Pass (bug 281, 2026-10-26)

The body-identity use list (`Proof.stmtFlatUses`) built each branch's uses by appending its nested
blocks' lists, copying the list from below at every level, so statements nested n deep took time
n² to elaborate (10000 nested `if`s: 1.4 s, now 26 ms). The walk appends to one accumulator, in the
same order; output, fingerprints and reports are unchanged
([bug 281](docs/bugs/281_evidence_flat_uses_nested_copy.md)).
