### The totality ratchet: the compiler cannot grow new non-total declarations (2026-10-28)

`scripts/tests/check_totality_ratchet.sh` reads the elaborated Lean environment of every compiler
module (`scripts/tools/totality_inventory.lean`), not the source text. It fails on any partial,
unsafe, opaque, `implemented_by` or `extern` declaration missing from
`docs/compiler/totality_baseline.tsv`, and on a baseline that grows relative to the base revision.
It also fails on a `sorry` in any compiler declaration, on an unregistered axiom, and on a
declaration pinned total in `docs/compiler/totality_pinned.tsv`. The baseline holds 682 rows,
664 of them partial definitions; 381 rows are in the stages the verified pipeline proves. Grep
miscounted in both directions: 89 `partial def` keywords are inert, and 60 partial declarations
(derived instances, `where` helpers) have no keyword. The partial-definition point checks of
`check_transform_register.sh` and `check_vc_bridge_register.sh` moved into the pin table. CI and
the pre-push hook run the gate, and a mutation family proves it load-bearing. The inventory, the
proofs each reason blocks and the conversion order are in
`research/compiler/partial-def-inventory-2026-10.md`.
