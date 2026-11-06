# partial_contract

**Class:** a postcondition with one direction proved and the converse still
outstanding — the audit is HONEST about partial coverage rather than flipping a
single green badge.

`ct_compare`'s `#[ensures]` is a full iff, but only the same-tag direction is
linked here (`coverage one_direction`, no `ensures_proof`). Since bug 089 a
coverage label grants nothing: `--report contracts` shows `unproven — NOT
discharged`, because nothing yet checks that the linked theorem concludes the
clause (ROADMAP "Exact property acceptance"). The `partial` class returns when
that matcher can confirm one direction.

**Negative/contrast:** the shipped `examples/constant_time_tag` proves BOTH
directions (full iff); this subexample keeps only one on purpose.
