### The `docs/NOTES/` design notes moved into `research/proof-evidence/` (2026-10-29)

The four notes (`concrete-design.md`, `lean-vs-rocq-tradeoffs.md`,
`response-to-why-rocq-is-better.md`, `why3-architecture-and-positioning.md`) are dated
proof-backend session records, so they now live in `research/proof-evidence/` with entries in
`research/README.md`, and `docs/NOTES/` is gone. The design they sketched binds through
`docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md`, which stays in the drift gate;
`scripts/tests/check_docs_drift.sh` no longer globs `docs/NOTES/*.md`, following the
`research/` convention that dated records are not gated. Links in
`docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md`, `docs/project/INFLUENCES.md`,
`docs/README.md`, `examples/two_kernel_demo/README.md` and the moved notes point at the new
paths.
