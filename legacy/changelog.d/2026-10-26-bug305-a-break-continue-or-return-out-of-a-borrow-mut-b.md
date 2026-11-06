### Bug 305: a break, continue or return out of a borrow mut block lost the writes made through its reference (2026-10-26)

A `break`, `continue` (labelled or not) or `return` that leaves a `borrow mut` block early no
longer loses the writes made through its reference: the reference is now the owner's own storage,
lowered as `&mut owner` is, instead of a copy written back only at the block's fall-through end.
This implements the owner's decision OD2 (2026-10-29): `&mut` is a reference to the place, and
every non-abort exit runs one exit sequence. Regression leg:
`regress_305_borrow_block_early_exit.con` and
`regress_305_borrow_block_owners_and_exits.con` (raw-pointer, `Heap` and reference-parameter
owners; early exits from nested blocks and `match` arms) in `run_tests.sh` (binary and `--interp`). Structural
owner: OD2 parts 3 and 5, then R-0501 ([bug 305](docs/bugs/305_a_break_continue_or_return_out_of_a_borrow_mut_b.md)).
