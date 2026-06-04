# Evidence classes — the catalog

Concrete's governing frame: **every construct is `proved`, `enforced`,
`reported`, `assumed`, or `trusted` — never a vague middle.** The `proved` cell
has sub-states (kernel-decision vs Lean refinement) and honest negative states
(partial, stale). This page is the catalog: each class, the canonical audit
line, the command that shows it, and a small worked reference.

The corpus under [`examples/evidence_classes/`](../examples/evidence_classes/)
holds one clean subexample per class (no flagship-scale noise). Flagships are
referenced where they are the better illustration.

## Proved

| class | what it means | command | reference |
|---|---|---|---|
| `proved_by_lean` | extracted body refines an independent Lean spec, kernel-checked | `--report proof-status` → `proved [..]`; `--report check-proofs` | `evidence_classes/proved_by_lean` (ch); flagship `hmac_sha256` |
| `proved_by_kernel_decision (omega)` | linear-integer obligation closed by `omega`, no external SMT | `--report contracts` (loop O1/O4/O5) | `evidence_classes/proved_by_kernel_decision_omega`; `examples/loop_invariant` |
| `proved_by_kernel_decision (bv_decide)` | closed bitvector obligation closed by `bv_decide` (LRAT-checked) | `--report contracts` (call site) | `evidence_classes/proved_by_kernel_decision_bv`; `contract_callsite` |

The trust tiers: `omega`/`bv_decide` are kernel decision procedures with checked
certificates — using them adds **no** external solver to the trusted base. Lean
refinement is a hand-written, kernel-checked theorem. See
[PROOF_LADDER.md](PROOF_LADDER.md).

## Honest non-final states (still under `proved`)

| state | what it means | command | reference |
|---|---|---|---|
| `partial` | one direction of a postcondition proved, converse outstanding | `--report contracts` | `evidence_classes/partial_contract` |
| `stale` | proof link present but the body drifted from the spec | `--report proof-status` → `stale` | `evidence_classes/stale_proof` |
| `missing` | proof-eligible, no proof attached yet | `--report proof-status` → `no proof` | any pure eligible fn |
| `blocked` | eligible but extraction hit an unsupported construct | `--report proof-status` → `blocked` | a `u8 & u8` body |
| `not eligible` | fails the predictable-profile gate (recursion, unbounded loop, caps) | `--report eligibility` | recursive fn |

These are not failures to hide — they are the audit telling the truth. A
`stale` link is the negative case for every `proved_*` entry.

## Enforced / reported / assumed / trusted

| class | what it means | command | reference |
|---|---|---|---|
| `enforced` | capability / bounds discipline enforced by the type system | `--report effects` → `evidence: enforced` | any pure bounded fn |
| `reported` | authority surfaced (not proved away) in the audit | `--report effects` → `evidence: reported` | any `with(..)` fn |
| `assumed` | a precondition assumed at entry, or a tainted `assumptions.toml` entry | `--report contracts` → `assumed_at_entry` | `evidence_classes/assumed_boundary`; `constant_time_tag` (timing) |
| `trusted` | a declared trust boundary — proof bypassed, made visible | `--report proof-status` → `trusted` | `evidence_classes/trusted_boundary` |

`assumed` and `trusted` are **named, audit-loud** — never silent gaps. `assume`
/ `assumptions.toml` taints an obligation to `assumed` and can be forbidden in
release gates.

## Tested (confidence, not proof)

| class | what it means | command | reference |
|---|---|---|---|
| `tested_by_oracle` | compiled program agrees with an independent reference across a vector set | `oracle/run_oracle.sh` | `evidence_classes/tested_by_oracle` (clamp, 200 cases) |

A `tested` class raises confidence but does **not** kernel-verify — it is below
`proved` on the ladder and is labeled as such. Disagreement is real signal.

## Planned

| class | status |
|---|---|
| `runtime_checked` (runtime-error obligation) | planned — `evidence_classes/runtime_checked/README.md` |

Tracked, not faked. The `runtime_error` coverage kind exists, but a clean worked
example needs a discharged runtime-error obligation (bounds / overflow /
div-zero); there is no auto runtime-check mode and no reusable theorem yet, so
building it is a real Lean-proof task rather than wiring.

## See also

- [CONTRACTS_GUIDE.md](CONTRACTS_GUIDE.md) — the authoring workflow
- [PROOFKIT_GUIDE.md](PROOFKIT_GUIDE.md) — how to write the Lean proofs
- [PROOF_LADDER.md](PROOF_LADDER.md) — the discharge tiers and trust costs
