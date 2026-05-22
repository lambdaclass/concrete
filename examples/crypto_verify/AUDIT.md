# crypto_verify — Pilot Audit

Status: **active pull-through candidate** per ROADMAP Active
Dependency Order rule 2 (parse_validate graduated 2026-05-22,
slot opened for crypto_verify on 2026-05-22).

## Why this example

Second domain from parse_validate's parsing/validation focus.
crypto_verify exercises authentication and integrity verification —
the Phase 7 "real cryptography example" surface (item 6). Honestly
small (67 lines, 4 functions): a toy multiplicative-tag MAC, a
verifier, a nonce range check, and an entry point. Not real crypto;
the value is in the **proof structure**, not the algorithm.

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

**Functions** (4 total):

- `compute_tag(key, message, nonce) -> Int` — pure: `key * message + nonce`
- `verify_tag(key, message, nonce, expected_tag) -> Int` — pure: 1 iff `compute_tag` matches expected
- `check_nonce(nonce, max_nonce) -> Int` — pure: 1 iff `0 < nonce ≤ max_nonce`
- `main() -> i32` — entry point, exercises the core, returns 0

**Oracle (today):** `--interp` agreement at the Phase A harness scope.

## 2. Authority / capability surface

- **0 capabilities required.** All 4 functions are `(pure)`.
- **0 externs, 0 unsafe, 0 trusted, 0 allocations.**

Strongest possible authority surface. main() is the entry point but
performs no I/O — it computes, returns 0.

## 3. Allocation / stack / failure assumptions

- **Allocation:** none. `--report alloc` is empty.
- **Stack:** max 144 bytes (main). Bounded.
- **Failure:** no explicit error path; `verify_tag` returns 0/1, no
  panics, no aborts.
- **Arithmetic:** `Int` (i64) for all values. The toy tag function
  is `key * message + nonce` — integer overflow is part of the
  "toy" framing (the proof doesn't claim overflow-freedom).

## 4. Report coverage — present vs missing

All 16 reports run cleanly. Substantive content:

- `caps` / `authority` / `effects` — 4 functions, all pure.
- `eligibility` — 3 eligible, 1 excluded (main, entry point).
- `proof-status` — **3 proved**, 0 blocked, 0 unproved. (Better than
  parse_validate's starting state.)
- `stack-depth` — max 144 bytes.
- `layout` / `mono` / `interface` / `consistency` — clean.

## 5. Smallest Lean-backed property candidate

**Already done.** Three theorems already attached in
`Concrete/Proof.lean`, registered in
`examples/crypto_verify/src/proof-registry.json`:

- `compute_tag_correct (key m nonce)`: `compute_tag(k, m, n) = k*m + n`
- `verify_tag_correct (key m nonce expected)`: `verify_tag = 1 ↔ k*m+n = expected`
- `check_nonce_correct (nonce max_nonce)`: `check_nonce = 1 ↔ 0 < n ≤ max`

All three surface as `proved` in `--report proof-status`.

**Composition candidate.** A theorem chaining `verify_tag` and
`check_nonce` ("a message+tag pair is acceptable iff the tag matches
AND the nonce is in range") would be the composition story. The
underlying functions are already proved; composing them is a small
theorem.

## 6. Missing artifacts / policies / assumptions

**Already met (no work needed):**

- ✅ Lean-backed proofs (3 functions proved)
- ✅ Proof-registry attachment
- ✅ Predictable profile passes
- ✅ All reports clean

**Missing (graduation-blocking):**

1. **No `assumptions.toml`.** Phase 2 E.24 surface — copy
   parse_validate's pattern, fill in.
2. **No `[policy]` enforcement.** `Concrete.toml` has
   `predictable = true` and `deny = ["Unsafe"]` but the rest of the
   schema (`no_alloc`, `no_unsafe`, `no_trusted`, `no_externs`,
   `max_stack_bytes`, `forbidden_capabilities`, `allowed_capabilities`)
   is not set. Drift-enforce by adding the full set.
3. **No `catches/` negative pair.** No demonstration of what
   Concrete refuses (authority widening, allocation in a pure
   function, etc.). Phase 1 D.22 surface.
4. **No `snapshot/` baseline.** Phase 2 E.23 surface.
5. **No `oracle/` directory.** Hand-written test only inside
   `main`; no fuzz/differential. Phase 1 D.6/D.9 surface.
6. **No `AUDIT.md`** (this file fills it).
7. **No `README.md`** with honest framing.
8. **No `CATCHES.md`** narrative.
9. **No release evidence bundle on file** (capturable via the
   existing script).
10. **Not in `tests/showcase/manifest.toml`** (graduation step).

## 7. Graduation contract (10 bars, same as parse_validate)

| # | Bar | Status |
|---:|---|---|
| 1 | Lean-backed property surfaced as `proved` | ✅ 3 functions proved |
| 2 | Composition property Lean-backed | ⏳ candidate identified (verify + nonce); to land |
| 3 | Assumption file with schema, CI-enforced | ✅ `assumptions.toml`; `make test-assumptions` 2/0 |
| 4 | Policy file with enforceable budgets, CI-enforced | ✅ `Concrete.toml [policy]` 8 fields enforced; `make test-policy` |
| 5 | Oracle beyond hand-written tests | ❌ absent |
| 6 | "Concrete catches this" negative pair | ❌ absent |
| 7 | Release evidence bundle capturable | ⏳ infrastructure exists (`capture_release_bundle.sh`); just needs the example to carry the artifacts that flesh it out |
| 8 | Honest README | ❌ absent |
| 9 | Snapshot/diff baseline | ✅ `examples/crypto_verify/snapshot/` 16 reports baselined; `make test-snapshots` 32/0 across both flagships |
| 10 | Listed in `tests/showcase/manifest.toml` | ❌ absent |

**Today: 4 of 10 bars met (#1, #3, #4, #9).** The cheap batch
landed in the candidate's first commit — assumption file, full
policy, snapshot baseline — confirming the reusable infrastructure
from parse_validate's pilot pays back. Remaining bars need real
content work (composition theorem, oracle, negative pair, README,
release bundle, graduation).

## 8. Suggested first pilot batch

Cheapest batch — copy the patterns parse_validate established:

1. **Land `assumptions.toml`** matching the actual surface.
2. **Extend `Concrete.toml` [policy]`** with the full 8-field schema.
3. **Land `snapshot/` baseline** by running the snapshot capture.

These three are mechanical given the templates exist. They close
bars #3, #4, #9 in one commit.

Subsequent commits:
- Composition theorem (bar #2) — small, the underlying proofs exist.
- Negative pair + CATCHES.md (bar #6) — one rejected file showing
  what Concrete refuses for this domain.
- README (bar #8) — template from parse_validate.
- Oracle (bar #5) — Python reference + differential, parse_validate
  pattern.
- Graduation (bar #10) — add to `tests/showcase/manifest.toml`.

## See also

- `examples/parse_validate/AUDIT.md` — the template this audit
  follows.
- `examples/parse_validate/README.md` — honest-framing template.
- `tests/showcase/manifest.toml` — graduation target.
- ROADMAP Active Dependency Order rule 2 — pilot ladder.
