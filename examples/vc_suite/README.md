# End-of-Phase-2 VC/SMT examples

Five small programs, each forcing **one** named VC/SMT surface with a report gate
(`check_vc_examples.sh`). Not a flagship phase and not a workload ladder — each
isolates a kind of obligation and shows where the evidence lands. Together they
demonstrate that almost everything is kernel-owned, and the one genuine
external-solver surface is the signed nonlinear product.

| # | example | surface | evidence |
| --- | --- | --- | --- |
| 5 | `packet_window` | mixed bounds (header + payload fit a 64-byte window) | omega; bad length → `unproven` |
| 6 | `fixed_point_filter` | Q-format: linear accumulate vs signed product | accumulate → omega; `scale` → SMT-eligible (`solver_trusted` under `--smt`) |
| 7 | `chunked_hash_padding` | HMAC block-count `(len+72)/64 <= 6` | omega (sound division lowering, #21) — **not** SMT |
| 8 | `rate_limiter` | clamp counter via branch guards | omega (threaded path conditions, #22); weakened guard → `unproven` |
| 9 | `ring_buffer_indices` | wraparound `i % 16 ∈ [0,16)` | omega (sound modulo lowering, #21) — **not** SMT |

Each "bad" variant (`unchecked_end`, `weak_clamp`) stays a non-proof — never a
misleading green. The only SMT-eligible obligation across all five is
`fixed_point_filter::scale` (a signed `var * var` product); run it with `--smt`
to see `solver_trusted` plus the solver provenance, or with `--smt --replay` for
the Lean-replay artifact. See `../smt/README.md` and
[docs/SMT_SOUNDNESS.md](../../docs/SMT_SOUNDNESS.md).
