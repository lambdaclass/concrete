# constant_time_tag

A fixed-size byte-array comparison helper with source-level
constant-time discipline.  First **real-crypto candidate**
per the explicit slot reserved by `crypto_verify`'s toy-MAC
graduation framing.

This is the smallest real-crypto artifact that forces the
exact distinction Concrete should be good at: **the code is
simple, but the claim is subtle**.  Six-line OR-accumulate
over `[u8; 16]` arrays; security depends on properties the
code surface alone cannot demonstrate (no early exit, loop
count independent of contents, branch-free fold).  Concrete's
role is to make those properties statically visible and to
prove the equality reasoning that backs them.

## What this example is

Two functions:

- `ct_compare(a: [u8; 16], b: [u8; 16]) -> i32` — returns 1
  iff `a` and `b` are byte-equal at every position, 0
  otherwise.  Always loops 16 times; no early exit; uses
  OR-accumulate over byte-level XORs; branch happens once at
  the end on the accumulator.
- `main` (entry point with `Console` for `println`) — runs 6
  in-source tests and reports pass/fail.

```sh
nix develop --command bash -c '.lake/build/bin/concrete \
    examples/constant_time_tag/src/main.con -o /tmp/ct && /tmp/ct'
```

Runs 6 hand-written cases (zeros, all-ones, single-byte diff
at position 0, single-byte diff at position 15, all-bytes-
differ, 0xFF tags).  Output ends with `All 6 tests passed.`
and exits 0.

## What this example is NOT

Stated up front so the rest of the README does not oversell.

**Not HMAC, not Ed25519, not a substitute for vetted crypto
libraries.**  This is a constant-time comparison helper, not a
MAC or signature.  Do not deploy `ct_compare` as your auth-tag
check; use a vetted constant-time-comparison primitive from
a maintained cryptographic library.  Audience here is
reviewers evaluating Concrete's proof workflow on
crypto-adjacent code.

**Not a constant-time proof at the machine level.**  The
source has no early exit and a fixed loop bound — *necessary*
for constant-time execution.  It is not *sufficient*: LLVM is
allowed to introduce branches during optimization; target
CPUs leak through cache, branch predictor, speculative
execution.  Concrete does not have target-level timing
analysis today.  `assumptions.toml`'s
`[claims.machine_level_constant_time]` entry names this
explicitly as `assumed_not_proved`.

## What is proved

Three Lean theorems, checked by the Lean kernel at `make build`:

```
Concrete.Proof.ct_compare_equal_zeros_correct
  ct_compare on two all-zero tags returns 1

Concrete.Proof.ct_compare_same_tag_correct
  for any 16-byte tag t (parametric in all 16 byte values),
  ct_compare(t, t) = 1

Concrete.Proof.bitxor_u8_self_zero
  helper: any u8 value xor'd with itself is 0
```

`--report proof-status` reports `ct_compare` as `proved`,
backed by the universal same-tag theorem.

The same-tag theorem is the substantive crypto-adjacent
claim: **equal tags always pass, with all 16 loop iterations
executed**, regardless of byte values.  The proof closes by
a single `simp` invocation that chains `BitVec.xor_self`
(per-iteration: `b ^ b = 0`) and `BitVec.zero_or` (per-
iteration: `0 | 0 = 0`) through 16 unrolled iterations of
the bounded while loop.  No induction over array contents is
required because the invariant — `diff` stays at 0 — is
uniform.

## ProvableV1 conformance

`ct_compare` fits the `ProvableV1` profile (see
[`docs/PROVABLE_V1.md`](../../docs/PROVABLE_V1.md)).  The
function uses only constructs in the supported surface: u8
BitVec `bitxor` and `bitor`, array indexing, a bounded
16-iteration `while_` loop, and an `i32` accumulator branch
at the end.  The registered theorem's `coverage` is
`one_direction` — universal positive direction over arbitrary
byte values, with the iff direction named (not hidden) as
open follow-up work.  `main` is outside `ProvableV1` by
design (entry point with `Console`).  The machine-level
constant-time gap is a target-level claim outside the
`ProvableV1` source / PExpr scope; `assumptions.toml`'s
`[claims.machine_level_constant_time]` carries it.

## What is NOT yet proved

Stated up front:

- **The negative direction.**  The theorem proves
  `ct_compare a a = 1`.  It does NOT prove
  `ct_compare a b = 1 ⟹ a = b` (the iff).  Closing the
  reverse direction needs an inductive argument over byte
  positions: "if any byte differs, the OR-accumulator is
  nonzero after the loop, so the final branch returns 0."
  This is the natural stretch theorem; helpers
  (`bitxor_u8_self_zero`, `bitor_u8_zero_zero`,
  `ct_loop_iteration_invariant`) are landed for the work.
  The oracle (see below) closes this gap empirically.
- **Source-level structural constant-time as a Concrete
  *guarantee*.**  No `constant_time` profile exists yet
  to name "loop count must be independent of input
  contents" as a checkable rule.  Today the discipline is
  enforced by manual review of the source's structural
  properties (no `return` inside the loop, fixed bound,
  no content-dependent branch).  Future Phase 3 work
  would lift this to a static profile.  `CATCHES.md`
  names this gap explicitly.
- **Machine-level constant time.** See above; Phase 3 /
  target-model work.

## What is enforced (statically, by the compiler)

- **No allocation.**  `--report alloc` is empty.  Both tag
  arrays are passed by value as `[u8; 16]`; `diff` is a
  `u8` local; the return is `i32`.  No heap.
- **No trusted shells.**  Stricter than `fixed_capacity`;
  `Concrete.toml [policy].no_trusted = true`.  Tag arrays
  are constructed via array literals — no raw byte writes
  required.
- **No FFI, no externs.**  `[policy].no_externs = true`.
- **Predictable profile.**  `--check predictable` passes
  for `ct_compare`; `main` is appropriately excluded
  (entry point with `Console`).
- **Bounded loop.**  The single loop runs exactly 16
  iterations (compile-time constant bound).
- **Stack budget honored.**  `--report stack-depth` reports
  max 209 bytes, within the policy budget of 256.

## What is enforced (by CI gates)

| Gate | Asserts |
|---|---|
| `make test-policy` | `Concrete.toml [policy]` — 8 enforced fields, stricter than other flagships |
| `make test-assumptions` | `assumptions.toml` matches reports, including the three `[claims.*]` entries |
| `make test-catches` | `catches/01_alloc_in_compare_core.con` rejects with `E0520 requires Alloc` |
| `make test-snapshots` | 16 report snapshots byte-identical to baseline |
| `make test-ct-oracle` | Python reference + 600 seeded cases agree (3 seeds × 200) |
| `make test-verify-gates` | Pass-by-pass compiler gates green |

## What is assumed

See `assumptions.toml`.  Summary:

- hosted-libc host;
- u8 byte arithmetic modeled at u8 width via `BitVec`
  round-trip on `bitxor` and `bitor`, with unsigned result
  interpretation (`Int.ofNat ∘ toNat`) so high-bit values
  (0x80..0xFF) surface as positive `Int`;
- no heap, no externs, no trusted, no FFI;
- `Console` capability granted (used by `main` for
  `println`);
- machine-level timing properties are NOT proved (the
  load-bearing assumption named in
  `[claims.machine_level_constant_time]`).

## Where the trust boundary sits

Three layers, same as the prior three flagships:

1. **Lean kernel** checks the three theorems above.  Trust
   anchor: `Lean kernel + Concrete.Proof.*`.
2. **Concrete compiler** extracts properties and reports
   them.  Trusted, not yet Lean-verified (Phase 12).
   The u8 PBinOp extensions (commits `12a4c94`, `28b1f2a`)
   are the load-bearing additions for this candidate; their
   Phase 12 preservation obligations are named in
   `docs/PROOF_OBLIGATIONS_REGISTER.md` R-17 and R-21.
3. **Toolchain + runtime.**  LLVM, clang, linker, libc.
   Not verified.  `assumptions.toml` records the host/
   toolchain exercised AND the machine-level-timing gap.

## The negative pair

`catches/01_alloc_in_compare_core.con` is the same
`ct_compare` shape with an `audit_byte` helper that needs
`with(Alloc)` called inside the loop.  Concrete refuses to
compile it with `E0520 — requires Alloc but caller has
(none)`.

The accepted example proves the language can express the
bounded-allocation discipline; the rejected companion
proves the language refuses the violation.  See
`CATCHES.md`, which ALSO documents the timing/early-exit
gap that Concrete cannot mechanically catch today.

## How the oracle complements the proof

The Lean theorem covers the positive direction universally
(equal tags → return 1, for any byte values).  It does NOT
yet cover the negative direction (differing tags → return
0).  The oracle closes that gap empirically:

- 200 cases per seed × 3 seeds = **600 differential cases**.
- ~152 of 200 cases per seed differ in at least one byte;
  every single one returns 0 against the Python reference.
- Cases cover per-position single-byte diffs (positions 0,
  15, and random middle), multiple-byte diffs, all-byte
  diffs, and high-bit byte values.

This is the right shape for an honest proof + evidence
story: the theorem is universal where it can be cheap; the
oracle catches what the theorem does not yet cover.

## Graduation status

10 of 10 bars met as of HEAD; graduated 2026-05-30.  See
`AUDIT.md` for the per-bar state.  The substantive
proof-side claim is `ct_compare_same_tag_correct`; the
substantive evidence claim is the oracle's 600 cases
covering both directions; the substantive
discipline-violation claim is the alloc-in-compare-core
catch.

## See also

- `AUDIT.md` — graduation bars and progress.
- `CATCHES.md` — negative-pair narrative + named gap.
- `assumptions.toml` — declared trust surface.
- `Concrete.toml` `[policy]` — enforced budgets.
- `oracle/reference.py` + `oracle/run_oracle.sh` — the
  differential harness.
- in-source `#[proof_by]`/`#[spec]`/`#[proof_fingerprint]` links in
  `src/main.con` + `Concrete/Proof.lean` — attached theorems.
- `docs/PROOF_OBLIGATIONS_REGISTER.md` R-17, R-21 — the
  Phase 12 preservation obligations this candidate's u8
  PBinOp extensions created.
- `examples/parse_validate/`, `examples/crypto_verify/`,
  `examples/fixed_capacity/` — the three sibling
  graduated flagships; this README's structural template.
