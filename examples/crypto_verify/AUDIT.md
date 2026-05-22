# crypto_verify — Pilot Audit

Status: **active pull-through candidate** per ROADMAP Active
Dependency Order rule 2 (parse_validate graduated 2026-05-22,
slot opened for crypto_verify on 2026-05-22).

## Why this example

Second domain from parse_validate's parsing/validation focus.
crypto_verify exercises authentication and integrity verification —
the Phase 7 "real cryptography example" surface (item 6). Honestly
small (85 lines, 5 functions): a toy multiplicative-tag MAC, a
verifier, a nonce range check, a composed verifier, and an entry
point. Not real crypto; the value is in the **proof structure**,
not the algorithm.

The author docstring says it directly:

> *"This is NOT a real MAC. The proof structure (correctness,
> rejection, range validation) would be identical for a real
> HMAC-SHA256 wrapper."*

## 1. Current behavior and oracle

Build + run:

```sh
nix develop --command bash -c '.lake/build/bin/concrete \
    examples/crypto_verify/src/main.con -o /tmp/cv && /tmp/cv'
```

Output: `0`.

**Functions** (5 total):

- `compute_tag(key, message, nonce) -> Int` — pure: `key * message + nonce`
- `verify_tag(key, message, nonce, expected_tag) -> Int` — pure: 1 iff `compute_tag` matches expected
- `check_nonce(nonce, max_nonce) -> Int` — pure: 1 iff `0 < nonce ≤ max_nonce`
- `verify_message(key, message, nonce, expected_tag, max_nonce) -> Int` — pure: 1 iff tag and nonce checks both pass
- `main() -> i32` — entry point, exercises the core, returns 0

**Oracle (today):** independent Python reference in
`oracle/reference.py`, run by `oracle/run_oracle.sh`. `make
test-cv-oracle` runs three seeds (0, 42, 999), 200 cases each:
600 generated valid/invalid messages must agree with the Concrete
binary.

## 2. Authority / capability surface

- **0 capabilities required.** All 5 functions are `(pure)`.
- **0 externs, 0 unsafe, 0 trusted, 0 allocations.**

Strongest possible authority surface. main() is the entry point but
performs no I/O — it computes, returns 0.

## 3. Allocation / stack / failure assumptions

- **Allocation:** none. `--report alloc` is empty.
- **Stack:** max 200 bytes (main). Bounded.
- **Failure:** no explicit error path; `verify_tag` returns 0/1, no
  panics, no aborts.
- **Arithmetic:** `Int` (i64) for all values. The toy tag function
  is `key * message + nonce` — integer overflow is part of the
  "toy" framing (the proof doesn't claim overflow-freedom).

## 4. Report coverage — present vs missing

All 16 reports run cleanly. Substantive content:

- `caps` / `authority` / `effects` — 5 functions, all pure.
- `eligibility` — 4 eligible, 1 excluded (main, entry point).
- `proof-status` — **4 proved**, 0 blocked, 0 unproved. (Better than
  parse_validate's starting state.)
- `stack-depth` — max 200 bytes.
- `layout` / `mono` / `interface` / `consistency` — clean.

## 5. Smallest Lean-backed property candidate

**Already done.** Four theorems already attached in
`Concrete/Proof.lean`, registered in
`examples/crypto_verify/src/proof-registry.json`:

- `compute_tag_correct (key m nonce)`: `compute_tag(k, m, n) = k*m + n`
- `verify_tag_correct (key m nonce expected)`: `verify_tag = 1 ↔ k*m+n = expected`
- `check_nonce_correct (nonce max_nonce)`: `check_nonce = 1 ↔ 0 < n ≤ max`
- `verify_message_composed_correct`: if tag and nonce validators both succeed, `verify_message` returns 1

All four surface as `proved` in `--report proof-status`.

## 6. Missing artifacts / policies / assumptions

**Already met:**

- ✅ Lean-backed proofs (4 functions proved)
- ✅ Proof-registry attachment
- ✅ Predictable profile passes
- ✅ Assumption file and policy file
- ✅ Negative pair and `CATCHES.md`
- ✅ Oracle directory and seeded differential runner
- ✅ Snapshot baseline
- ✅ Honest README
- ✅ Release evidence bundle captures cleanly
- ✅ All reports clean

**Missing (graduation-blocking):**

1. **Not in `tests/showcase/manifest.toml`** (graduation step).
2. **Not yet a real crypto algorithm.** This can still graduate as a
   crypto-adjacent proof-structure showcase, but the manifest must
   frame it honestly as toy crypto rather than cryptographic
   assurance.

## 7. Graduation contract (10 bars, same as parse_validate)

| # | Bar | Status |
|---:|---|---|
| 1 | Lean-backed property surfaced as `proved` | ✅ 4 functions proved |
| 2 | Composition property Lean-backed | ✅ `verify_message_composed_correct` success direction |
| 3 | Assumption file with schema, CI-enforced | ✅ `assumptions.toml`; `make test-assumptions` 2/0 |
| 4 | Policy file with enforceable budgets, CI-enforced | ✅ `Concrete.toml [policy]` 8 fields enforced; `make test-policy` |
| 5 | Oracle beyond hand-written tests | ✅ Python reference + `make test-cv-oracle` |
| 6 | "Concrete catches this" negative pair | ✅ `catches/01_alloc_in_pure_core.con` + `CATCHES.md` |
| 7 | Release evidence bundle capturable | ✅ `capture_release_bundle.sh examples/crypto_verify` captures cleanly |
| 8 | Honest README | ✅ `README.md` |
| 9 | Snapshot/diff baseline | ✅ `examples/crypto_verify/snapshot/` 16 reports baselined; `make test-snapshots` 32/0 across both flagships |
| 10 | Listed in `tests/showcase/manifest.toml` | ❌ absent |

**Today: 9 of 10 bars met.** The remaining decision is whether to
graduate this as a toy crypto-adjacent proof-structure showcase, or
park it and reserve Phase 7 crypto status for a later real
algorithm/wrapper.

## 8. Next step

Make the graduation call:

- **Graduate now** as an explicitly toy crypto-adjacent showcase:
  add `crypto_verify` to `tests/showcase/manifest.toml`, with claims
  scoped to proof composition, allocation/capability discipline, and
  honest assumptions — not real cryptographic security.
- **Park it** as an active candidate: keep the 9-bar evidence, but
  record that Phase 7 crypto graduation waits for a real HMAC /
  Ed25519 / constant-time wrapper.

## See also

- `examples/parse_validate/AUDIT.md` — the template this audit
  follows.
- `examples/parse_validate/README.md` — honest-framing template.
- `tests/showcase/manifest.toml` — graduation target.
- ROADMAP Active Dependency Order rule 2 — pilot ladder.
