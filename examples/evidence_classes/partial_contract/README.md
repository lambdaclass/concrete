# partial_contract

**Class:** a postcondition with one direction proved and the converse still
outstanding — the audit is HONEST about partial coverage rather than flipping a
single green badge.

`ct_compare`'s `#[ensures]` is a full iff, but only the same-tag direction is
linked here (`coverage one_direction`, no `ensures_proof`). `--report contracts`
shows `partial — one direction proved_by_lean, converse outstanding`, and
`concrete prove ... ` reports the converse as the next obligation.

**Negative/contrast:** the shipped `examples/constant_time_tag` proves BOTH
directions (full iff); this subexample keeps only one on purpose.
