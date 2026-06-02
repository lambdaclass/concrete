# hmac_sha256 — HMAC-SHA256, evidence-carrying

The fifth flagship and the first **cryptographic-primitive** flagship:
a complete HMAC-SHA256 (RFC 2104 over SHA-256, FIPS 180-4) written in
ordinary bounded systems-style Concrete — fixed arrays, `u32` wrapping
arithmetic, bit operations, bounded loops, no allocation, no FFI, no
ambient effects — that **carries its own machine-checked evidence**.

The point is not "we computed HMAC." It is that the source code, its
authority (capabilities), its assumptions, its tests, and its Lean
proofs move together as one audited artifact, and the compiler reports
exactly which parts are *proved*, *enforced*, *assumed*, and *tested*.

## What it is

- A real RFC primitive: SHA-256 compression + multi-block hashing with
  FIPS padding, and the full HMAC construction (key normalization,
  ipad/opad, inner+outer hash).
- **Runtime-verified** against the published vectors, in BOTH the
  compiled binary and the tree-walking interpreter:
  - `SHA-256("abc")` == FIPS 180-4 Appendix B.1
  - `HMAC-SHA256(0x0b×20, "Hi There")` == RFC 4231 Test Case 1
  - `HMAC-SHA256("Jefe", "what do ya want for nothing?")` == RFC 4231 TC2
- **ProvableV1 `Status: full`**: every proof-eligible function fits the
  provable subset; only the entry point `main` is excluded (it holds the
  `Console` capability, by profile design).

## What it is NOT

- **Not a vetted crypto library. Do not deploy it.** Use libsodium,
  BoringSSL, or a maintained Rust crate for production authentication.
- **Not constant-time at the machine level** (see Layer 4 below).
- **Not a forge-resistance proof**: HMAC's PRF security is a
  cryptographic assumption about SHA-256, not something Concrete proves
  (Layer 3). We prove *functional correctness against the spec*, not
  *security against an adversary*.
- **Proved end-to-end (functional correctness, bounded inputs).** The
  entire SHA-256/HMAC composition chain carries kernel theorems tied to
  the extracted source through the spec-drift gate — 11 registered
  proofs, `check-proofs` = 11 verified, 0 failed. The proofs hold for
  inputs within the documented bounds (`k_len ≤ 128`, `m_len ≤ 256`,
  hashed `len ≤ 375`); they say nothing about machine-level timing or
  PRF security (Layers 3–4). See "Proof status" below.

## The four-layer trust model

This is the load-bearing honesty of the example. See `assumptions.toml`
for the machine-checked versions.

1. **Functional correctness (proved).** Lean theorems, kernel-checked,
   tied to the *extracted* source (not a hand-written parallel model)
   through the spec-drift gate. The whole chain is proved — the
   composition theorem `hmac_sha256_refines_spec` shows the extracted
   body computes exactly `Sha256Spec.hmac` for all inputs in the
   documented bounds; the RFC/FIPS vectors and the oracle differential
   cross-check the BitVec reference spec.
2. **Source-level structural discipline (statically enforced).** No
   allocation, no FFI, no capabilities in the core, compile-time-bounded
   loops, branch-free Boolean round functions, constant-time tag compare
   (no early exit). Visible in `--report` and gated by CI.
3. **HMAC / SHA-256 cryptographic security (assumed from RFC).** Out of
   scope by construction; named, not closed.
4. **Machine-level constant-time + side channels (assumed, NOT proved).**
   LLVM and the target CPU may leak via branches, cache, or speculation.
   The deployer must validate this independently.

## Proof status

`concrete examples/hmac_sha256/src/main.con --report check-proofs`

11 registered proofs, all kernel-verified (`check-proofs` = 11 verified,
0 failed; spec-drift gate = 0 drift, 0 stale):

| Function | Coverage | Theorem |
|---|---|---|
| `sha256_init` | point | `sha256_init_correct` — returns the FIPS H(0) constants |
| `ch` | point | `ch_selects_high` — `Ch(0xFFFFFFFF, y, z) = y`, over the forced u32 `bitand`/`bitxor` |
| `block_to_words` | full_contract | `block_to_words_refines_spec` — big-endian word packing |
| `block_to_words_at` | full_contract | `block_to_words_at_refines_spec` — offset variant |
| `sha256_schedule` | full_contract | `sha256_schedule_refines_spec` — 64-word message schedule |
| `sha256_round` | full_contract | `round_refines_list` — one compression round |
| `sha256_compress` | full_contract | `sha256_compress_refines_spec` — 64-round block compression |
| `sha256_compress_at` | full_contract | `sha256_compress_at_refines_spec` — offset block compression |
| `state_to_bytes` | full_contract | `state_to_bytes_refines_spec` — digest serialization |
| `sha256_hash` | full_contract | `sha256_hash_refines_spec` — multi-block padded hash |
| `hmac_sha256` | full_contract | `hmac_sha256_refines_spec` — full HMAC (ipad/opad + key-prep branch) |
| `main` | ineligible | by design (holds `Console`) |

Each `full_contract` theorem proves the extracted source body equals an
independent BitVec spec (`Sha256Spec.hash`/`compress`/`schedule`/…, and
`Sha256Spec.hmac` at the top) for all inputs in the documented bounds,
by `eval_while_count` loop induction + `bv_decide` (not 64× unfolding).
All 11 are registered with the spec equal to the source-extracted PExpr
(`Concrete.Proof.specs`) and tied through the spec-drift gate: editing a
source body turns the proof `stale`/drifted in CI (regression-verified —
perturbing the ipad constant flips proof-status to "10 proved, 1 stale").

## How ProvableV1 conformance was earned

HMAC forced a whole sequence of provable-subset extensions, each a
separate audited commit (see `docs/PROOF_OBLIGATIONS_REGISTER.md`):

| Register | Construct | Unblocked |
|---|---|---|
| R-22 | u32 `bitand` | `ch`, `maj` |
| R-23 | u32 `shr` (logical) | the σ functions |
| R-24/R-25 | u32 `shl` + `bitor` | `rotr` |
| R-26 | u32 wrapping `add` | the compression rounds |
| R-27 | array-element assignment in loop bodies | `block_to_words`, schedule, `state_to_bytes`, `hmac_sha256` |
| R-28 | i32/u32 `div` | `sha256_hash` (block count) |

The conformance gate walked
`partial → full (stubs) → partial (real loops) → full (R-27) → partial
(real SHA loops) → full (R-28)` — tracking each proof obligation
precisely, the gate behaving as a *progress indicator*, not a green
badge. `u32` arithmetic wraps identically across three semantics — the
proof model (BitVec), the interpreter (`maskWidth`), and the compiled
binary (LLVM) — verified by the oracle differential.

## The headline proof milestone (closed)

Full end-to-end correctness — *for all bounded keys/messages,
`hmac_sha256` returns the RFC digest* — is **done**. It was a serious
proof-engineering project: point proofs by pure kernel evaluation do
**not** scale (a `while_` with `arraySet` updates strains `simp`'s
recursion limit at two iterations), so the chain is proved by *universal*
theorems via loop induction (`eval_while_count`) + `bv_decide`,
bottom-up:

1. universal `ch`/`maj`/`rotr`/σ correctness (BitVec lemmas),
2. universal `sha256_compress` over one block (64-round induction),
3. universal `sha256_hash` over bounded messages (multi-block loop
   induction + data-dependent padding stores),
4. universal `hmac_sha256` (the `k_len > 64` key-prep branch + ipad/opad).

Each step is registered and tied to the exact extracted source through
the spec-drift gate — the proof is about the literal extracted body, not
a hand model — so the thesis holds end to end: *source extracts to this
ProofCore body, this body refines the spec, and drift breaks the claim.*
The bounds (`k_len ≤ 128`, `m_len ≤ 256`, `len ≤ 375`) are limits, not
full generality; the oracle differential (600 cases) covers the tail.

## Run it

```sh
nix develop --command bash -c '
  .lake/build/bin/concrete examples/hmac_sha256/src/main.con -o /tmp/hmac && /tmp/hmac'
# -> SHA-256("abc") matches FIPS 180-4 and
#    HMAC-SHA256 matches RFC 4231 TC1+TC2; all checks passed.

# Evidence reports:
.lake/build/bin/concrete examples/hmac_sha256/src/main.con --report audit
.lake/build/bin/concrete examples/hmac_sha256/src/main.con --report check-proofs
.lake/build/bin/concrete examples/hmac_sha256/src/main.con --report proof-status
```

## See also

- `AUDIT.md` — the pre-implementation contract and the ten graduation bars.
- `assumptions.toml` — the machine-checked four-layer trust boundary.
- `Concrete.toml` `[policy]` — enforceable budgets (no alloc/FFI/trusted,
  bounded stack), CI-gated.
- `docs/PROVABLE_V1.md` — the provable subset this candidate expanded.
- `docs/PROOF_OBLIGATIONS_REGISTER.md` — every extension R-22..R-28.
