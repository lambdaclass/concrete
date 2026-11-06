### The specification on the new root (proposed, A2c)

`ConcreteSpec` lands on the new root: the slice-1 rules, the A2b formalization of the Core rows
approved on 2026-11-05, and the A2c additions, split into modules under `ConcreteSpec/Spec/` (owner
decision 2026-11-06) with `ConcreteSpec/Spec.lean` importing them, the proofs in
`ConcreteSpec/Proofs/` and the reading guide in `ConcreteSpec/README.md`. It stays PROPOSED until
the owner freezes Core.

- Slice 1 in Core's binding form (Q9.1) with static refusal of known violations (Q6.1–Q6.6, the
  2026-11-05 field-by-field amendment of Q6.2); `check_iff`, `checked_never_stuck`,
  `checked_exits_or_traps`, `knownViolation_traps`.
- The four design theorems of row Q9.14: `rep_roundtrip`, `defer_cleanup_releases`,
  `open_mut_freezes_root` with `shared_open_grants_no_write`, `reset_fresh`.
- Capability checking (`authority_contained`), observations (exit 125, the returned code, trap
  134), brand erasure (`brand_erasure`, `checked_passes_brand_checks`), the slice-0 fragment
  (`slice0_exits`).
- Range subviews (Q4.7): the refusal of a range proven invalid, complete for the known facts
  (`knownInvalid_iff`); element access inside `[a, b)` or a trap; exact writes and the block frame;
  no escape (`view_not_moved`, `view_not_used_after`, `root_restored`); UTF-8 text views
  (`text_view_utf8`).
- `scripts/spec/lean_site.sh` generates a browsable owner view from the Lean sources.

Lean core only; axioms `propext`, `Quot.sound`, `Classical.choice`; no `sorry`, no `native_decide`.
The README lists the open obligations O1–O10.
