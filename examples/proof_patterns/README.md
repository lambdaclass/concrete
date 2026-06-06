# proof_patterns — copy-a-small-pattern proof corpus

A bounded teaching + regression set for **source-linked** proof authoring. Each
subdirectory is one small, self-contained shape you can copy when writing a new
proof. The flagships (`parse_validate`, `crypto_verify`, `hmac_sha256`, …) prove
that the system works at scale; this corpus shows the *minimal* version of each
pattern so a new author starts from something tiny.

**The rules every pattern follows** (and the gate enforces, see below):

- proofs are **source-linked only** — `#[spec]` / `#[proof_by]` /
  `#[ensures_proof]` / `#[proof_coverage]` / `#[proof_fingerprint]` in the
  `.con`; no `proof-registry.json`, ever;
- the spec PExpr + the Lean theorem live in `Concrete.Examples.ProofPatterns.Proofs`
  (namespace `Examples.ProofPatterns.Proofs`), **not** in the `Concrete.Proof`
  compiler namespace;
- staleness is caught by the in-source `#[proof_fingerprint]` (these patterns are
  intentionally not registered in `Concrete.Proof.specs`);
- obligation ids are stable across `--json` / `--report contracts` / `--replay`
  (`<qual>@<line>#<Ox>` for loop VCs, `<qual>#refines_spec` / `#ensures`).

## The patterns

| Dir | Function | Shows | Coverage | Expected |
|---|---|---|---|---|
| `straight_line/` | `straight_line.add_three` | tiniest refinement: `x + 3` | iff | proved |
| `array_update/` | `arr.put` | write one cell, frame the rest | point | proved |
| `loop_copy/` | `loopcopy.copy2` | counted copy loop is faithful | point | proved |
| `fold/` | `fold.sum4` | reduction over a fixed array | point | proved |
| `composition/` | `calls.combine` | calls two proved helpers via the FnTable | iff | proved |
| `runtime_safety/` | `rt.*` | bounds/div/overflow obligations discharged | — | omega/bv-proved + 1 `unproven` |
| `stale_missing_partial/` | `states.*` | the three non-green states | — | missing / stale / proved[one_direction] |
| `workspace/` | `workspace.scale_by_two` | the `--workspace` all-in-one bundle | iff | proved |
| `repair/` | `repair.needs_proof` | the agent repair loop | iff | `missing_theorem` under `--check` |

## The exact commands

For a proved refinement (using `straight_line`):

```sh
# structured context (status, obligations with stable ids, next_actions):
concrete prove examples/proof_patterns/straight_line/src/main.con straight_line.add_three --json

# generate a compilable Lean stub (ends in `sorry`; fill it in):
concrete prove examples/proof_patterns/straight_line/src/main.con straight_line.add_three --emit-lean

# kernel-verify the linked proof (the closed loop):
concrete prove examples/proof_patterns/straight_line/src/main.con straight_line.add_three --check --json
#   → {"all_checked": true, "checks": [{"status": "checked", ...}]}

# the in-source link block to paste above the function once proved:
concrete prove examples/proof_patterns/straight_line/src/main.con straight_line.add_three --emit-link

# whole-file kernel check + per-function evidence:
concrete examples/proof_patterns/straight_line/src/main.con --report check-proofs
concrete examples/proof_patterns/straight_line/src/main.con --report proof-status
```

Runtime-safety obligations are compiler-discharged (no Lean proof) — read them with:

```sh
concrete examples/proof_patterns/runtime_safety/src/main.con --report contracts
#   rt.get / rt.sum / rt.ratio  → proved_by_kernel_decision (omega)
#   rt.scale                    → proved_by_kernel_decision (bv_decide)
#   rt.unchecked                → unproven  (the honest negative)
```

The all-in-one workspace (a disposable build output, never committed):

```sh
concrete prove examples/proof_patterns/workspace/src/main.con workspace.scale_by_two --workspace /tmp/ws
#   /tmp/ws/{manifest.json, context.json, obligations/<id>.json,
#            Scale_by_twoProofs.lean, link.con.txt, check.sh, replay.sh, README.md}
#   (and NO proof-registry.json)
```

The agent repair loop — `repair.needs_proof` links a theorem that isn't written yet:

```sh
concrete prove examples/proof_patterns/repair/src/main.con repair.needs_proof --check --json
#   → {"all_checked": false,
#      "checks": [{"obligation_id": "repair.needs_proof#refines_spec",
#                  "status": "missing_theorem", ...}]}
#   the author then writes the theorem in Examples.ProofPatterns.Proofs and re-checks.
```

## Regression gate

`scripts/tests/check_proof_patterns.sh` (CI + `make test-proof-patterns`) pins
the load-bearing facts: `check-proofs` passes for the proved patterns,
`proof-status` reports the expected class per pattern, `--json` carries stable
obligation ids, emitted `--emit-lean` stubs typecheck up to the `sorry`
placeholder, generated workspaces contain no `proof-registry.json`, and the
negative variants (`rt.unchecked`, `repair.needs_proof`) fail for the intended
reason.
