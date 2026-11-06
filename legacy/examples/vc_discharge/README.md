# VC / discharge matrix

One clear worked reference for **every VC status** a user sees in `--report vcs`
(or `--report contracts` for the trust-boundary statuses). The point is not more
examples — it is a small matrix so each status has exactly one canonical worked
output. Pinned by `check_vc_discharge_examples.sh`.

| status | engine | reference | command |
| --- | --- | --- | --- |
| `proved_by_kernel_decision` | omega | `omega.con` | `--report vcs` |
| `proved_by_kernel_decision` | bv_decide | `bv_decide.con` | `--report vcs` |
| `solver_trusted` | smt:z3 | `solver_trusted.con` | `--report vcs --smt` |
| `counterexample` | smt:z3 | `counterexample.con` | `--report vcs --smt` |
| `missing` | — | `missing.con` | `--report vcs` |
| `assumed` (trust boundary) | — | `assumed.con` | `--report contracts` |
| `proved_by_lean` | lean | `../hmac_sha256/` (`ch`) | `--report contracts` |
| `partial` | lean | `../contract_negatives/weakened_postcondition/` | `--report contracts` |
| `stale` | — | `../proof_patterns/stale_missing_partial/` | `--report proof-status` |

The six subexamples here are tiny and self-contained — **no proof registry JSON,
no Lean proof file**. The three proof-backed statuses (`proved_by_lean`,
`partial`, `stale`) each require a *real* Lean proof to be honest; recreating one
per status is neither compact nor adds value over the corpus, and a link to a
nonexistent theorem would be a misleading green — so the matrix cites the existing
**verified** references for those. See `examples/smt/README.md` for the external
solver classes and the kernel boundary in detail.
