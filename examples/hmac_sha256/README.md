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
- **Not yet fully proved end-to-end.** Two functions carry kernel
  theorems today; the rest are proof-*eligible* and runtime-verified.
  See "Proof status" and "The honest gap" below.

## The four-layer trust model

This is the load-bearing honesty of the example. See `assumptions.toml`
for the machine-checked versions.

1. **Functional correctness (proved / in progress).** Lean theorems,
   kernel-checked, tied to the *extracted* source (not a hand-written
   parallel model). Two are attached today (see below); the rest are
   established at runtime against RFC/FIPS vectors and awaiting their
   theorems.
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

| Function | Status | Theorem |
|---|---|---|
| `sha256_init` | **proved** (point) | `sha256_init_correct` — returns the FIPS H(0) constants |
| `ch` | **proved** (point) | `ch_selects_high` — `Ch(0xFFFFFFFF, y, z) = y`, over the forced u32 `bitand`/`bitxor` |
| all other eligible fns | `missing` | extractable; runtime-verified; theorem pending |
| `main` | ineligible | by design (holds `Console`) |

`sha256_init_correct` was the first attached theorem (no PBinOp
dependency — it pins the array-of-u32 shape and the body-fingerprint
mechanism). `ch_selects_high` is the first over the *forced u32 bitwise
surface*. Both are tied to the source through extraction + a registered
body fingerprint + the spec-drift gate, so editing the source either
keeps the proof valid or surfaces it as `stale`/drifted in CI.

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

## The honest gap (and why end-to-end is a milestone, not a chore)

Full end-to-end correctness — *for all bounded keys/messages,
`hmac_sha256` returns the RFC digest* — is achievable but is a serious
proof-engineering project, not the next cleanup. Point proofs by pure
kernel evaluation do **not** scale: a `while_` with `arraySet` updates
already strains `simp`'s recursion limit at two iterations, so reducing
a 64-round compression (let alone a multi-block HMAC) by evaluation is
impractical. The real path is *universal* theorems via loop induction
(the `while_step` preservation machinery), bottom-up:

1. universal `ch`/`maj`/`rotr`/σ correctness (BitVec lemmas),
2. universal `sha256_compress` over one block (64-round induction),
3. universal `sha256_hash` over bounded messages (block-loop induction),
4. universal `hmac_sha256`.

That is the flagship's headline proof milestone.

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
