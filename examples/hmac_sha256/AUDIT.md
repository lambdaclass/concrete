# hmac_sha256 — Candidate Audit

Status: **8 of 10 bars met** (audit landed 2026-05-30; build-out
2026-05-31). The primitive is implemented and runtime-verified
(FIPS 180-4 + RFC 4231, compiled and interpreted), ProvableV1
conformance is `full`, and two kernel theorems are attached
(`sha256_init_correct`, `ch_selects_high`). Bars met: #1, #3, #4,
#5, #6, #7, #8, #9. **Remaining: bar #2** (a Lean-backed
*composition* / RFC-vector theorem — the loop-induction milestone;
point proofs by evaluation do not scale past a couple of
iterations, so this needs `while_step` induction, bottom-up
ch/maj/σ → compress → hash → hmac) and **bar #10** (showcase
manifest / graduation, gated on bar #2 reaching 10/10). The oracle
(bar #5) covers correctness empirically until the composition
theorem catches up.

## Why this example

Fifth candidate; **first cryptographic-primitive flagship**
per ROADMAP Phase 7 item 5 ("Audit the next stronger
real-crypto candidate: HMAC-SHA256 verification or Ed25519
verification subset").  constant_time_tag was the first
real-crypto flagship; it proved a comparison helper.  This
candidate proves a real RFC primitive.

The thesis-strength bet: **prove HMAC-SHA256 (RFC 2104 over
SHA-256, FIPS 180-4) equivalent to its specification, at
the source level, with the trust boundary explicit**.  That
is the artifact reviewers outside the project take seriously
— "Concrete proved a real cryptographic primitive against
its RFC" — and the artifact that pulls the largest set of
new constructs into `ProvableV1`.

`ProvableV1` conformance status going in: today the function
would surface as `Status: partial` because SHA-256 uses
constructs not yet in the supported surface (u32 shifts,
u32 bitand, u32 rotations).  Each lands as a separate
ProofCore extension; the per-flagship conformance check
moves from `partial` toward `full` as each row in `§ 8` is
discharged.  This is the first flagship that will
demonstrate the conformance gate working as a progress
indicator, not just a green badge.

## What this candidate is NOT

Stated up front so the rest of the audit does not oversell.

- **Not a vetted cryptographic library.**  No constant-time
  guarantees at the machine level (same caveat as
  constant_time_tag, but more load-bearing here because
  HMAC's key-handling is genuinely security-critical).  No
  side-channel hardening.  Do not deploy this for production
  authentication; use a library like libsodium, BoringSSL,
  or a vetted Rust crate.
- **Not key derivation.**  HMAC is a MAC, not HKDF or PBKDF2.
  This candidate computes a 32-byte tag from a key and
  message; it does not derive new keys.
- **Not hardware-accelerated.**  No SHA-NI intrinsics, no
  AES-NI, no vectorization.  The proof and the source target
  the pure software path; a hardware-accelerated variant
  would be a separate candidate.
- **Not a constant-time proof at the machine level.**  The
  source has fixed loop bounds (64 rounds, 16 message-schedule
  expansion steps, etc.) and no content-dependent branches,
  but LLVM and the target CPU may still leak timing.  Same
  honest framing as constant_time_tag: structural
  source-level discipline is a *necessary* condition for
  constant-time; not sufficient.
- **Not a forge-resistance proof.**  HMAC's pseudo-random
  function security property is a cryptographic assumption
  about SHA-256, not something Concrete can prove.  We prove
  functional correctness against the RFC spec, not security
  against an adaptive chosen-message adversary.

## 1. Current behavior and oracle

Not implemented yet.  This audit is the pre-implementation
contract.  Once landed:

```sh
nix develop --command bash -c '.lake/build/bin/concrete \
    examples/hmac_sha256/src/main.con -o /tmp/hmac && /tmp/hmac'
```

Expected behavior: runs a hand-written test set against
**RFC 4231 § 4** test vectors (the standard HMAC-SHA256 test
cases — short key, long key, key-longer-than-block, multi-
block message, etc.).  Each test compares the computed tag
to the published expected tag using `ct_compare`.  Output
ends with `All N tests passed.` and exits 0.

The Python oracle will run `hmac.new(k, m,
hashlib.sha256).digest()` against the Concrete binary for
hundreds of seeded (key, message) pairs and assert
byte-for-byte agreement.  Covers: empty key, empty message,
key shorter than block size, key equal to block size, key
longer than block size (triggers key hashing pre-step),
single-block message, multi-block message, message at exact
block boundary, message one byte under and one byte over the
boundary.

## 2. Authority / capability surface

Designed to be:

- Roughly 8-12 pure functions in the proof-eligible core:
  - `sha256_compress(state: [u32; 8], block: [u8; 64]) -> [u32; 8]`
    — the SHA-256 64-round compression
  - `sha256_block_to_words(block: [u8; 64]) -> [u32; 16]`
    — big-endian byte-to-word packing
  - `sha256_schedule(w: [u32; 16]) -> [u32; 64]` — message
    schedule expansion (rounds 16..63)
  - `sha256_round(state: [u32; 8], k: u32, w: u32) -> [u32; 8]`
    — one compression round
  - `sha256_finalize(state: [u32; 8]) -> [u8; 32]` —
    state-to-bytes big-endian unpack
  - `sha256_init() -> [u32; 8]` — the canonical initial
    hash state (`H0..H7`)
  - `hmac_prepare_key(k: [u8; KEY_MAX], k_len: i32, ipad: u8,
    opad: u8) -> ([u8; 64], [u8; 64])` — key padding /
    pre-hashing
  - `hmac_sha256(k: [u8; KEY_MAX], k_len: i32, m: [u8;
    MSG_MAX], m_len: i32) -> [u8; 32]` — the composed
    primitive
  - `verify_hmac(k, k_len, m, m_len, expected: [u8; 32]) -> i32`
    — entry-point safe wrapper using `ct_compare`-style fold
- 1 entry-point function `main` with `Console` capability
  (for `println`).  Excluded from proof-eligibility by
  profile.
- No `trusted` shells.  All source-level; no raw byte writes
  bypassing types.
- No FFI, no externs, no allocation.

Cross-flagship reuse decision: a `ct_compare`-shape
constant-time fold for the tag-equality check is needed.
For audit purposes the candidate will inline its own
`ct_compare_32` (over `[u8; 32]`) rather than cross-flagship-
import constant_time_tag's `ct_compare` (which is over
`[u8; 16]`).  This keeps the example self-contained for
review and avoids creating a flagship-to-flagship dependency
graph.

## 3. Allocation / stack / failure / arithmetic

- **Allocation:** none.  All buffers are stack-allocated
  fixed-size arrays (`[u32; 8]`, `[u32; 16]`, `[u32; 64]`,
  `[u8; 32]`, `[u8; 64]`, `[u8; KEY_MAX]`, `[u8; MSG_MAX]`
  for some compile-time `KEY_MAX` and `MSG_MAX`).
- **Stack:** estimated 3-5 KB given the message schedule
  (64 × 4 = 256 bytes), the hash state copies, and the
  ipad/opad blocks.  Policy budget will be set after a
  first compile measurement.
- **Failure:** the verification entry point returns 1 iff
  the tag matches, 0 otherwise.  Buffer overflows are
  prevented by type-level sizes; passing `k_len > KEY_MAX`
  or `m_len > MSG_MAX` is a precondition violation (caller
  responsibility — documented in the source).
- **Arithmetic:**
  - `u32` wrapping addition (`+`).  All 32-bit operations
    are modulo `2^32`.
  - `u32` bitand (`&`) — used in `Ch` and `Maj` round
    functions.
  - `u32` bitor (`|`) — already in `ProvableV1` at i32/u32/u8.
  - `u32` bitxor (`^`) — already in `ProvableV1`.
  - `u32` right shift (`>>` `n` for `n ∈ {2, 3, 6, 7, 11, 13,
    17, 18, 19, 22, 25}` — the SHA-256 constants).
  - `u32` left shift (`<<` `n`) — used in byte-to-word
    packing and to implement rotation as `(x << n) | (x >>
    (32 - n))`.
  - `u32` rotate right (`rotr` `n` for `n ∈ {2, 6, 7, 11,
    13, 17, 18, 22, 25}` — the SHA-256 constants).  May be
    implemented as `(x >> n) | (x << (32 - n))` to avoid
    adding a dedicated `rotr` PBinOp.
- **Loops:** several bounded loops, all compile-time bounded:
  - 64 SHA-256 compression rounds
  - 48 message-schedule expansion steps (rounds 16..63)
  - 16 word-load steps in `block_to_words`
  - 8 finalize steps in `state_to_bytes`
  - up to `(MSG_MAX / 64) + 2` block iterations for the
    outer message-loop in `sha256` (depends on `MSG_MAX`)
  - 1 ipad-XOR loop (64 iterations)
  - 1 opad-XOR loop (64 iterations)

## 4. Honest threat model — what the proof does and does NOT claim

Four layers of trust, with explicit boundaries:

**Layer 1: Functional correctness (proved).**  Lean theorems
will state, at minimum: for specific RFC 4231 test vectors,
`hmac_sha256(k, m) = expected`.  These are *point* proofs
(coverage: `point`) for concrete inputs.  The stretch goal
is a parametric correctness theorem stating
`sha256_compress` matches the reference round-by-round, but
that requires inductive reasoning over the loop that may
exhaust heartbeats.

**Layer 2: Source-level structural discipline (statically
checked).**  Concrete reports surface that:
- Every loop bound is a compile-time constant.
- No allocation.
- No content-dependent branches.
- Tag equality check uses a constant-time fold, not `==`
  on byte arrays.
This is reported and enforced; not proved as a theorem.

**Layer 3: Cryptographic security (assumed; out of scope).**
HMAC's MAC security property — "no probabilistic
polynomial-time adversary can produce a valid tag for a
fresh message" — is a cryptographic assumption about
SHA-256's pseudo-random function property.  This is not
something Concrete can prove; it is an assumption about
the construction.  `assumptions.toml` will name it
explicitly: `[claims.hmac_security] = assumed_from_rfc`.

**Layer 4: Machine-level constant-time + side-channel
resistance (NOT proved; assumed).**  Same caveat as
constant_time_tag, more important here because keys are
involved.  LLVM may rewrite branch-free idioms; target CPUs
may leak via cache or branch predictor.
`assumptions.toml` will record this as
`[claims.machine_level_constant_time] = assumed_not_proved`
just like constant_time_tag.

## 5. Forcing surface in ProvableV1

What this candidate forces into the `ProvableV1` supported
surface before it can graduate.  Today (after
constant_time_tag), the supported PBinOps are
`add`/`sub`/`mul` (Int), `mod` (i32/u32), `bitxor`
(i32/u32/u8), `bitor` (i32/u32/u8), plus comparisons.  This
candidate forces:

| Extension | Status today | Forced by | ProvableV1 doc impact |
|---|---|---|---|
| `PBinOp.add` width-tagged at u32 (wrapping `mod 2^32`) | exists as Int add; needs explicit u32 width via BitVec round-trip | every SHA-256 round computing `T1 = h + Σ1(e) + Ch(e,f,g) + K[i] + W[i]` (mod 2^32) | move "multi-word arithmetic" out of Excluded list at u32 |
| `PBinOp.bitand (width, signed)` at u32 | not supported | `Ch(x, y, z) = (x & y) ^ (~x & z)` and `Maj(x, y, z) = (x & y) ^ (x & z) ^ (y & z)` | move "bitand" out of Excluded list at u32 |
| `PBinOp.shr (width, signed)` at u32 (logical right shift) | not supported | message schedule `σ0(x) = ROTR(x, 7) ^ ROTR(x, 18) ^ SHR(x, 3)` and `σ1` | move "shifts" out of Excluded list at u32 |
| `PBinOp.shl (width, signed)` at u32 (left shift) | not supported | byte-to-word packing: `w = (b[0] << 24) | (b[1] << 16) | (b[2] << 8) | b[3]`, and rotation as `(x << n) | (x >> (32 - n))` | move "shifts" out of Excluded list at u32 |
| `~` (bitwise NOT) at u32 | not supported | `~x` in `Ch(x, y, z)` | new PBinOp shape (unary) OR expressed as `x ^ 0xFFFFFFFF`; decision below |
| Rotation as derived op or new PBinOp | derivable from shl + shr + bitor | every round's `Σ0`, `Σ1`, `σ0`, `σ1` functions | decision below; preference is to *derive* from shifts+OR to avoid a new constructor |
| u8-to-u32 cast model | identity cast exists; widening cast needs explicit width | byte-to-word packing | confirm u8 → u32 widening preserves value |
| Multi-iteration loop induction over array elements | exists shape-level (fixed_capacity / constant_time_tag); needs to scale to 64 rounds with composite state | every compression loop | stretch: re-use evalWhileStep technique from fixed_capacity |
| Larger fixed arrays (`[u32; 64]`) in the proof model | currently used at smaller sizes | message schedule | should be "just works" — arraySet + lookup are size-polymorphic |

Decision on `~x`: prefer expressing as `x ^ 0xFFFFFFFF`
(literal XOR with all-ones), reusing the existing u32
bitxor.  This avoids adding a new constructor and keeps the
PBinOp surface append-only.

Decision on `rotr`: prefer expressing as `(x >> n) | (x <<
(32 - n))`, reusing the new shl + shr + existing bitor.
Same reason.  A future `rotr` PBinOp could be added if
proof readability demands it; not for v1.

Net: the forcing surface adds **two real new PBinOp ops**
(u32 shl, u32 shr) plus **two new widths** (u32 add tagged,
u32 bitand), with every other operation expressible in
existing constructors.  This is a smaller forcing set than
constant_time_tag's u8 work given the
constant_time_tag/fixed_capacity PBinOp scaffolding already
in place.

## 6. Smallest Lean-backed property candidate

Bar #1 (first attached theorem) candidate:

  `sha256_init_correct`: `sha256_init() = [0x6a09e667,
  0xbb67ae85, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab,
  0x5be0cd19, 0x6c1f29f]`.

Trivial point proof — the initial hash state is a constant
struct, no operations involved.  Establishes the array-of-u32
shape in the proof model and the body fingerprint mechanism
on a function with no PBinOp dependencies.

Bar #2 (composition / substantive) candidates, in order of
attainability:

  (a) `hmac_sha256_empty_correct`: `hmac_sha256(k = empty,
  m = empty) = 0xb613679a0814d9ec...` (the RFC 4231 §4.7
  "Test Case 7" tag for the empty key + empty message,
  hand-verified against the published RFC).  Point proof
  with a long computation; would exercise the full
  compression pipeline end-to-end on a single block.

  (b) `sha256_compress_identity_under_zero_state`: weaker
  property useful for proving the pipeline shape, not the
  RFC compliance.

  (c) `hmac_sha256_rfc_vector_N_correct` for each of the
  RFC 4231 §4.1..§4.7 test vectors.  Point proofs at the
  primitive's interface.

  (d) Stretch: `hmac_sha256_matches_reference`: a
  parametric theorem stating the source extraction agrees
  with an in-Lean reference SHA-256 implementation, over
  arbitrary key/message inputs (bounded by `KEY_MAX`,
  `MSG_MAX`).  This is the strongest claim but requires the
  reference implementation to be in Lean, not Python, AND
  multi-iteration loop induction at 64 rounds.

For graduation: (a) + (c) covering at least 3 of the 7 RFC
test vectors is the minimum substantive claim.  (d) is the
stretch goal that would put this flagship in genuinely
unprecedented territory.

## 7. The ten graduation bars

| # | Bar | Status |
|---:|---|---|
| 1  | Lean-backed property surfaced as `proved` | **MET** — `sha256_init_correct` (point) and `ch_selects_high` (point, over the forced u32 `bitand`/`bitxor`) are kernel-verified; `--report check-proofs` = 2 verified, 0 failed |
| 2  | Composition property Lean-backed | **OPEN** — the loop-induction milestone. A composition / RFC-vector theorem requires `while_step` induction over compress/hash (point proofs by evaluation hit `simp`'s recursion limit at ~2 array-update iterations, so they do not scale to 64 rounds). Path: universal ch/maj/rotr/σ → `sha256_compress` over one block → `sha256_hash` → `hmac_sha256`. The oracle (bar #5) covers this empirically until the theorem lands |
| 3  | Assumption file with schema, CI-enforced | **MET** — `assumptions.toml` with the four `[claims.*]` blocks; `make test-assumptions` green |
| 4  | Policy file with enforceable budgets, CI-enforced | **MET** — `Concrete.toml [policy]`, 8 fields, `no_trusted=true`, Alloc forbidden; `make test-policy` green |
| 5  | Oracle beyond hand-written tests | **MET** — Python `hmac.new(...)` differential harness, 200 cases/seed × 3 seeds = 600, all length regimes; `make test-hmac-oracle` green |
| 6  | "Concrete catches this" negative pair | **MET** — `catches/01_alloc_in_compression_core.con` (alloc in the round) and `catches/02_ambient_key_read.con` (key from ambient I/O), both rejected; `CATCHES.md` + the honest timing-gap note; `make test-catches` green |
| 7  | Release evidence bundle capturable | **MET** — `scripts/tests/capture_release_bundle.sh examples/hmac_sha256` captures cleanly |
| 8  | Honest README | **MET** — `README.md`: what-is / what-is-NOT / four-layer trust / proof-status table / R-22..R-28 journey / deployment disclaimer |
| 9  | Snapshot/diff baseline | **MET** — 16 reports baselined under `snapshot/`; `make test-snapshots` green |
| 10 | Listed in `tests/showcase/manifest.toml` with full evidence section | **OPEN** — graduation; gated on bar #2 reaching 10/10. Not added until the composition theorem lands (the showcase gate requires "10 of 10 bars met") |

## 8. Strategic value beyond graduation

hmac_sha256 is the **first cryptographic-primitive
flagship** — the artifact that moves Concrete from "we have
demonstrators + a comparison helper + toys" to "we have a
real RFC primitive with kernel-checked correctness."  Its
value is disproportionate to its size:

- **Validates the proof workflow on a real cryptographic
  primitive.**  constant_time_tag graduated as a
  helper-level real-crypto candidate; hmac_sha256 graduates
  as a primitive-level one.  The pattern (full algorithm,
  point proofs against RFC vectors, four-layer threat
  model) is the template for future primitive-level
  candidates (Ed25519 verify, ChaCha20, BLAKE2).
- **Forces the largest single ProvableV1 expansion to date.**
  Two new PBinOps (u32 shl, u32 shr), two new widths (u32
  add tagged, u32 bitand), one width-cast extension (u8 →
  u32), and a multi-iteration loop reasoning scaling
  exercise.  After this candidate, ProvableV1's u32 surface
  is essentially complete for standard symmetric crypto.
- **Demonstrates the conformance gate as a progress
  indicator.**  This is the first flagship that will
  surface `Status: partial` at intake and `Status: full` at
  graduation, with each intermediate construct visible in
  `concrete audit` output.  Reviewers can watch the matrix
  fill in.
- **Names real cryptographic-security assumptions.**  Layer
  3 of § 4 explicitly punts HMAC's PRF security to RFC
  assumption.  That gap should be visible to readers and
  motivate the boundary between Concrete (functional
  correctness, statically enforced discipline) and
  cryptographic analysis (formal security in the random-
  oracle / standard model).

## 9. Suggested first pilot batch

Same shape as fixed_capacity / constant_time_tag first
batches:

1. **Land `src/main.con`** with the skeleton: function
   signatures (no `impl`), constants
   (`H0..H7`, the round constants `K[0..63]`), and a
   minimal `main` running 1-2 RFC test vectors via
   hand-written byte arrays.  At first compile this will
   surface as `Status: partial` in
   `concrete --report audit` because the shl/shr/u32-bitand
   operations are not yet in ProvableV1 — and that is the
   correct status to record at intake.
2. **Land `assumptions.toml`** matching the surface, with
   the four `[claims.*]` blocks above.
3. **Land `Concrete.toml [policy]`** with the 8+ field
   schema: `predictable=true`, `no_alloc=true`,
   `no_externs=true`, `no_unsafe=true`, `no_trusted=true`,
   stack budget (set after first compile measurement),
   `forbidden_capabilities=["File", "Net", "Unsafe",
   "Alloc"]`, `allowed_capabilities=["Console"]`.
4. **Land `snapshot/` baseline** with all 16 reports —
   including the audit report capturing `Status: partial`
   with the specific blocked functions named.  The snapshot
   then becomes the drift signal: as each ProofCore
   extension lands, the snapshot updates and the diff
   visibly closes the gap.

These four close bars #3, #4, #9 in one commit — same
pattern as prior candidates.

Subsequent commits attack the forcing surface (§ 5) one row
at a time, in order:
- Add u32 wrapping `add` with BitVec round-trip.
- Add u32 `bitand` to PBinOp + evalBinOp.
- Add u32 `shr` to PBinOp + evalBinOp.
- Add u32 `shl` to PBinOp + evalBinOp.
- Implement `sha256_init` and attach
  `sha256_init_correct` (bar #1).
- Implement `sha256_compress` and attach a point proof on
  the canonical zero-block.
- Implement the full `hmac_sha256` and attach an RFC test
  vector theorem (bar #2).
- Land the oracle.
- Land the negative pair + CATCHES.md.
- Land README + manifest entry to graduate.

## 10. Open decisions

The audit closes one set of decisions and surfaces another.
These are decided BEFORE bar #1 attempts:

1. **`MSG_MAX` and `KEY_MAX`.**  Bounded array sizes for the
   message and key buffers.  Candidate: `KEY_MAX = 128`
   (covers RFC 4231 test cases up to and including
   key-longer-than-block), `MSG_MAX = 256` (covers the
   multi-block test cases).  Larger sizes are not needed
   for graduation but a future extension could
   parameterize.
2. **u32 vs Int representation in PVal.**  The proof model
   today represents integers as `PVal.int` (mathematical
   Int).  u32 width is enforced at the PBinOp boundary via
   BitVec round-trip.  This stays the design; the
   alternative (a `PVal.u32` constructor) would be a
   constructor refactor we do not take.
3. **Endianness.**  SHA-256 is big-endian on input/output
   bytes.  The compiled binary's target endianness is
   assumed little-endian (x86/ARM); the source code uses
   explicit byte indexing to construct words, so the
   compiled binary is endianness-independent at the API
   level.  `assumptions.toml`
   `[claims.target_endianness] = assumed_little_endian`
   records the host assumption; the algorithmic endianness
   is handled in source.
4. **Cross-flagship reuse of `ct_compare`.**  Decided in
   § 2: inline a `ct_compare_32` helper rather than
   cross-flagship-import.  Keeps the example self-contained.
5. **Stretch theorem decision.**  Whether to attempt the
   parametric `hmac_sha256_matches_reference` theorem (§ 6
   item d).  Decided after bar #1 lands — its feasibility
   depends on heartbeat budgets we cannot yet estimate.

## See also

- `examples/parse_validate/AUDIT.md` — first graduated; the
  10-bar template.
- `examples/crypto_verify/AUDIT.md` — second graduated; toy
  crypto scaffolding.
- `examples/fixed_capacity/AUDIT.md` — third graduated;
  state-model + composition over bounded mutable state.
- `examples/constant_time_tag/AUDIT.md` — fourth graduated;
  the constant-time helper this candidate's `ct_compare_32`
  inlines.
- `docs/PROVABLE_V1.md` — the subset contract this
  candidate's graduation will expand.  Today's "Excluded"
  list names shifts, bitand, and (multi-word) arithmetic as
  out-of-subset; this candidate's graduation moves them in.
- `docs/PROOF_OBLIGATIONS_REGISTER.md` — every ProofCore
  extension this candidate forces appends a row here.
- `docs/PROOF_STATE_MODEL.md` — the state model whose
  64-round loop induction the compression theorem will
  exercise.
- `docs/PROOF_STORY_MATRIX.md` — the per-construct matrix
  this candidate moves several cells of from "open" to
  "proved at u32 width".
- `ROADMAP.md` Phase 7 items 5–7 — the slot this candidate
  fills.
- **RFC 2104** — HMAC: Keyed-Hashing for Message
  Authentication.
- **FIPS 180-4** — Secure Hash Standard (SHA-256
  specification).
- **RFC 4231** — Identifiers and Test Vectors for HMAC-SHA-
  224/256/384/512.
