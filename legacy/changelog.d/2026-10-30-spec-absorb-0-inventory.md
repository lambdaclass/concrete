### Language specification: the absorption inventory (2026-10-30)

`docs/spec/absorb_inventory.tsv` lists every statement about the language found outside `docs/spec`,
across `docs/`, `research/`, the grammar, the website guide and the paper, with the chapter or owner
decision it goes to: 402 files, one verdict row each, and 1010 statement rows marked as
duplicating a rule, missing, stale, in conflict with a rule, or an open question. It is the plan for the absorption slices.
`one_spec.py` (S13) keeps it in step with the tree: every file it names exists, each has one verdict
row, and every unabsorbed `docs/language` file is in it.
