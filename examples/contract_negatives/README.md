# Phase 1 source-contract validation artifact

This is the combined validation surface for Concrete's **source contracts**
(`#[requires]` / `#[ensures]` / `#[invariant]` / `#[variant]`, `ghost let`,
`spec fn`, `assert` / `assume`). Every fixture here exists to pin one honesty
property: a green proof is never misleading, and an honest gap is never hidden.

Run the whole artifact under one gate:

```sh
make test-phase1-contracts          # umbrella: sub-gates + per-class snapshots
# or directly:
bash scripts/tests/check_phase1_contracts.sh
```

The umbrella runs `check_contract_negatives.sh` and `check_contract_stability.sh`,
then asserts one **golden report snapshot per failure class** (under
`scripts/tests/phase1_snapshots/`) is byte-identical — drift is real signal.
Regenerate snapshots deliberately with
`UPDATE_PHASE1_SNAPSHOTS=1 bash scripts/tests/check_phase1_contracts.sh`.

The soundness *why* behind these behaviours is proved in
`Concrete/ProofSoundness.lean` (`Source-contract soundness, R-22..R-28`) and
explained in `docs/CONTRACTS_AND_VCS.md` (Soundness Bridge).

## Failure classes — what each fixture proves, reports, or refuses

| Fixture | Report | What it pins |
| --- | --- | --- |
| `precondition_callsite/` | contracts | A callee `#[requires]` is checked at each **call site**: a constant violation is `failed_at_callsite`; an unestablished one is `unproven_at_callsite`; one that follows from the caller's `#[requires]` or an enclosing guard is omega-`proved`. |
| `missing_postcondition/` | contracts | An `#[ensures]` with no in-source proof link is reported `missing`, never `proved`. |
| `weakened_postcondition/` | contracts | A postcondition with only one direction of an iff proved is `partial`, not `proved`. |
| `invalid_attribute/` | contracts | A malformed proof-link attribute is rejected at parse time. |
| `invalid_invariant/` | contracts | A loop that does not preserve its `#[invariant]` does **not** get its preservation VC omega-proved (no false green). |
| `invalid_contract_expression/` | contracts | An unknown identifier in a contract is `invalid_contract_expression`, naming the offending identifier. |
| `spec_ghost_totality/` | contracts | A contract calling an effectful (capability-requiring) function is rejected — the spec/ghost language must be pure and total. A pure helper + `spec fn` is **not** over-rejected. |
| `vacuous_contract/` | contracts | An unsatisfiable precondition (`#[requires(false)]`, contradictory clauses, `#[invariant(false)]`) is `vacuous` / `VACUOUS`, never `proved` — its postcondition holds only trivially. |
| `assert_obligation/` | contracts | `assert(e)` is an obligation: closed-by-omega → `proved_by_kernel_decision`; unestablished → `unproven`; always-false → `VIOLATION`. Never silently accepted. |
| `assume_taint/` | contracts | `assume(e)` is trust, not proof: evidence class `assumed`, the function marked `TAINTED`, a clean sibling untainted; a `forbid-assume` release profile rejects it (E0614). |
| `duplicate_links/` | contracts | Two of the same proof-link attribute are rejected at parse time. |
| `fabricated_proof/` | proof-status | A nonexistent theorem name passes the fingerprint-trusting proof-status (documented limitation) but is caught by `concrete prove --check` (`missing_theorem`). |
| `../contract_positive/valid_complex_contract_scope/` | contracts | **Positive control**: a contract that legally mentions every name it can (params, `result`, a constant, a pure helper, a `spec fn`, a loop counter, a `ghost let`, a local) produces **zero** false positives. Over-eager scope-checking is as dishonest as a missed obligation. |
| `../contract_stability/{v1,v2}.con` | diff | Contract **API drift**: a `requires` clause added (precondition strengthened) or an `ensures` clause dropped (postcondition weakened) is breaking (`weakened`, exit 1); a `requires` clause removed is compatible (`strengthened`). Pinned by `check_contract_stability.sh`. |
| `../hmac_sha256/` (anchor) | contracts | **Mature-path regression anchor**: `block_to_words_at`'s `#[requires(off+64<=384)]` is discharged symbolically by omega from `sha256_compress_at`'s matching `#[requires]`; the division-bounded `sha256_hash` call site stays honestly `unproven`. |

Each row is exercised by `check_contract_negatives.sh` (33/0),
`check_contract_stability.sh` (6/0), or both, and snapshotted by the umbrella gate.
