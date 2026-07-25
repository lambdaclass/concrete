# hmac_sha256

`hmac_sha256` is the fifth graduated Concrete flagship. It is a bounded
HMAC-SHA256 implementation whose proof is tied to the source body that the
compiler extracts.

The example is intentionally narrow:

- fixed-size arrays;
- `u32` wrapping arithmetic, shifts, and bit operations;
- bounded loops;
- no allocation;
- no FFI;
- no ambient authority in the proof-eligible core.

The entry point `main` uses `Console` to print test results and is excluded from
the proof-eligible core by profile.

## What is proved

The registered proof chain shows:

```text
Concrete source
  -> exact extracted ProofCore body
  -> independent Lean SHA-256/HMAC spec
```

For all inputs within the documented bounds, the extracted Concrete
`hmac_sha256` body evaluates to `Sha256Spec.hmac`.

Current bounds:

- `k_len <= 128`
- `m_len <= 256`
- internal `sha256_hash` messages satisfy `len <= 375`

This is a functional-correctness proof, not a cryptographic-security proof and
not a machine-level constant-time proof.

## Registered proofs

Run:

```sh
.lake/build/bin/concrete examples/hmac_sha256/src/main.con --report check-proofs
```

Expected status:

```text
11 verified, 0 failed
```

Registered proofs:

| Function | Coverage | Theorem |
|---|---:|---|
| `sha256_init` | point | `sha256_init_correct` |
| `ch` | point | `ch_selects_high` |
| `block_to_words` | full_contract | `block_to_words_refines_spec` |
| `block_to_words_at` | full_contract | `block_to_words_at_refines_spec` |
| `sha256_schedule` | full_contract | `sha256_schedule_refines_spec` |
| `sha256_round` | full_contract | `round_refines_list` |
| `sha256_compress` | full_contract | `sha256_compress_refines_spec` |
| `sha256_compress_at` | full_contract | `sha256_compress_at_refines_spec` |
| `state_to_bytes` | full_contract | `state_to_bytes_refines_spec` |
| `sha256_hash` | full_contract | `sha256_hash_refines_spec` |
| `hmac_sha256` | full_contract | `hmac_sha256_refines_spec` |

The `full_contract` entries are refinement theorems against the independent
`Sha256Spec` model. They are not point checks over the RFC vectors.

## Source tie

The proof is registered against the exact extracted source bodies in
`Concrete.Proof.specs`. The proof registry stores source fingerprints for each
function in the chain.

That means a source edit is visible. As a regression check, changing the HMAC
ipad constant made proof status drop from:

```text
11 proved, 0 stale
```

to:

```text
10 proved, 1 stale
```

This is the main evidence claim of the example: the proof is about the extracted
program body, and drift breaks the claim.

### Current status: the links are UNBOUND (2026-07-25)

Read the paragraph above as the intended design, not as today's state. These 11
`#[proof_by]` links carry no `#[proof_fingerprint]`, so there is no stored
proof-subject digest for the freshness check to compare a body against. Since
bug 058 was contained, `--report proof-status` reports them as:

```text
11 proof link unbound
```

Stated precisely:

- **The theorems exist and replay clean.** `--report check-proofs` reports
  `11 verified, 0 failed` against the current bodies. All twelve named theorems
  are present in `proofs/Examples/HmacSha256/Proofs.lean`.
- **Their source linkage is not freshness-validated.** Nothing records which body
  each theorem was verified against, so an edit cannot be detected through the
  fingerprint path. That is the whole of bug 058, and this example is its largest
  instance.
- **Spec-drift coverage is unaffected** — every link still reports
  `spec: drift-checked`, so the `Concrete.Proof.specs` comparison continues to
  catch a body that diverges from its recorded spec. That is the mechanism behind
  the `10 proved, 1 stale` result above; it is a different check from the
  fingerprint, and it still works.

The links are deliberately left unbound. Recording a digest asserts "verified
against this body", and that claim needs a trustworthy receipt — an artifact
tying theorem, body digest, and toolchain together. `--report check-proofs` is
also invocation-sensitive: run from inside this directory it reports
`0 verified, 11 failed` with `theorem_lookup` errors, and from the repository
root it reports `11 verified, 0 failed`. A check whose verdict depends on the
working directory, leaving no artifact behind, is an observation rather than a
receipt. Producing that receipt is part of R-0004's digest phase; until then the
honest report is `unbound`.

Under `[policy] require-proofs = true` these links now fail closed with E0612.

## Runtime oracle

The executable is also checked against published and differential tests:

- SHA-256("abc") from FIPS 180-4;
- HMAC-SHA256 RFC 4231 test cases 1 and 2;
- 600 generated cases against Python `hmac`/`hashlib`, covering short keys,
  long keys, empty messages, block boundaries, and multi-block messages.

Run:

```sh
make test-hmac-oracle
```

The oracle is regression evidence. It is useful, but it is not counted as a
proof.

## Trust boundary

| Layer | Status | Meaning |
|---|---|---|
| Functional correctness | proved | Lean kernel checks the extraction-to-spec theorem chain. |
| Source discipline | enforced | No allocation, no FFI, bounded loops, and capability profile checks are audited. |
| HMAC/SHA-256 security | assumed | PRF/MAC security is a cryptographic assumption about the construction. |
| Machine-level timing | trusted/assumed | LLVM and the target CPU are not proved constant-time. |

Do not use this as a production cryptographic library. Use a maintained crypto
library for production authentication.

## Proof infrastructure produced

The HMAC proof was not left as a one-off. Its reusable parts were extracted into
`Concrete/ProofKit`:

- evaluator stepping and fuel monotonicity;
- loop induction patterns;
- array and frame lemmas;
- `Int`/`Nat`/`BitVec` bridges;
- call and function-table helpers;
- refinement scaffolding.

The guide is in `docs/PROOFKIT_GUIDE.md`.

## ProvableV1 pressure

This example forced the following proof-surface additions:

| Register | Construct | Used by |
|---|---|---|
| R-22 | `u32` bitand | `ch`, `maj` |
| R-23 | `u32` logical right shift | sigma functions |
| R-24/R-25 | `u32` left shift and bitor | rotation and packing |
| R-26 | `u32` wrapping add | compression rounds |
| R-27 | array-element assignment in loop bodies | schedule, packing, serialization, HMAC buffers |
| R-28 | integer division | padded block count |

ProvableV1 conformance is now `Status: full` for the proof-eligible core.

## Run it

```sh
nix develop --command bash -c '
  .lake/build/bin/concrete examples/hmac_sha256/src/main.con -o /tmp/hmac &&
  /tmp/hmac'
```

Evidence reports:

```sh
.lake/build/bin/concrete examples/hmac_sha256/src/main.con --report audit
.lake/build/bin/concrete examples/hmac_sha256/src/main.con --report check-proofs
.lake/build/bin/concrete examples/hmac_sha256/src/main.con --report proof-status
```

## Related files

- `AUDIT.md` — graduation bars and review notes.
- `assumptions.toml` — machine-checked assumptions.
- `Concrete.toml` — policy budgets.
- in-source `#[proof_by]`/`#[spec]`/`#[proof_fingerprint]` links in `src/main.con`
  — attached proof entries and fingerprints (no JSON registry).
- `docs/PROOFKIT_GUIDE.md` — how to reuse the proof infrastructure.
- `docs/PROVABLE_V1.md` — the supported proof subset.
