# Evidence-class corpus

A curated suite: each subexample demonstrates **one** evidence class cleanly,
with no flagship-scale noise. Concrete's governing frame is that every construct
is `proved`, `enforced`, `reported`, `assumed`, or `trusted` — never a vague
middle — and the proof cell has sub-states (kernel-decision, partial, stale).
This corpus is the worked reference for each, snapshot-backed.

| subexample | evidence class | see it with |
|---|---|---|
| `proved_by_lean/` | proved_by_lean (refinement, kernel-checked) | `--report proof-status` / `check-proofs` |
| `proved_by_kernel_decision_omega/` | proved_by_kernel_decision (omega) | `--report contracts` |
| `proved_by_kernel_decision_bv/` | proved_by_kernel_decision (bv_decide) | `--report contracts` |
| `partial_contract/` | partial — one direction proved, converse outstanding | `--report contracts` |
| `stale_proof/` | stale (the negative case) | `--report proof-status` |
| `assumed_boundary/` | assumed (precondition assumed at entry) | `--report contracts` |
| `trusted_boundary/` | trusted (declared trust boundary) | `--report proof-status` / `effects` |
| `tested_by_oracle/` | differential-oracle evidence (`tested`, not proved) | `oracle/run_oracle.sh` |
| `runtime_checked/` | runtime-error obligation | **planned** (see its README) |

Each real subexample has `src/main.con`, a `README.md`, and a snapshot of the
report that shows its class. See `docs/EVIDENCE_CLASSES.md` for the full catalog
including the flagship references.
