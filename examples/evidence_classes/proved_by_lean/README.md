# proved_by_lean

**Class:** the extracted body is proved to refine an independent Lean spec,
kernel-checked.

SHA-256's `ch` bit-select, linked in source to `Concrete.Proof.ch_selects_high`
(spec `chExpr`). `concrete <src> --report proof-status` shows `proved [point]`;
`--report check-proofs` kernel-verifies it. Edit the body and it goes stale
(see `../stale_proof`).
