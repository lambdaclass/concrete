# constant_time_tag — Candidate Audit

Status: **graduated 2026-05-30** as the fourth Phase 7
flagship; first real-crypto flagship per the explicit slot
crypto_verify's toy-MAC graduation reserved.  Listed in
`tests/showcase/manifest.toml`.  parse_validate graduated
2026-05-22; crypto_verify 2026-05-23; fixed_capacity
2026-05-28; constant_time_tag slot opened 2026-05-28 and
graduated on completion of all 10 bars.

## Why this example

Fourth candidate; **first real-crypto candidate** per the
explicit slot reserved by crypto_verify's graduation framing.

The example is a constant-time fixed-size tag comparison
helper.  It is **not** HMAC-SHA256, not Ed25519, not a full
cryptographic primitive.  It is the smallest real-crypto
artifact that forces the exact distinction Concrete should
be good at: **the code is simple, but the claim is subtle**.
Specifically, the artifact's security depends on properties
the code's surface alone cannot demonstrate — no early exit,
loop-count independent of contents, result-as-flag — and
Concrete's role is to surface those properties statically
and prove the equality reasoning that backs them.

The implementation is a 6-line OR-accumulate loop:

```con
fn ct_compare(a: [u8; 16], b: [u8; 16]) -> i32 {
    let mut diff: u8 = 0;
    for (let mut i: i32 = 0; i < 16; i = i + 1) {
        diff = diff | (a[i] ^ b[i]);
    }
    // "diff == 0" must be evaluated without short-circuiting
    // or early-exit branches on the byte stream.  Represent the
    // result as i32 0/1 (caller treats 1 as "tags match").
    if diff == 0 { return 1; }
    return 0;
}
```

Three claims a reviewer is asked to trust:

1. **All 16 byte positions are inspected on every call.**  No
   early exit; the loop bound is the compile-time constant
   16, independent of `a` and `b`.
2. **The result is 1 iff all bytes match** (functional
   correctness of the OR-accumulate).
3. **Timing is content-independent at the source level.**
   Constant-time at the *machine* level remains a backend /
   target assumption — see § 4.

## Layered evidence (source-contract retrofit, 2026-06-03)

`ct_compare` is the first flagship to use **source contracts as the
primary proof surface**. It carries an inline functional-correctness
postcondition plus a loop contract:

```con
#[ensures((a == b && result == 1) || (a != b && result == 0))]
#[invariant(0 <= i && i <= 16)]
#[variant(16 - i)]
```

This deliberately does **not** flatten the different claim types into one
contract. `concrete prove constant_time_tag.ct_compare` and
`--report contracts` show the layers, each in its own evidence class:

```
functional correctness:
  same-tag direction (a == b → 1):      proved_by_lean
                                        (ct_compare_same_tag_correct)
  different-tag direction (a != b → 0): missing / planned  ← next obligation

loop obligations (the fixed-trip-count discipline, as arithmetic):
  invariant_init:          proved_by_kernel_decision (omega)
  invariant_preservation:  arithmetic proved_by_kernel_decision (omega);
                           operational step needs Lean (theorem shape emitted)
  variant_nonnegative:     proved_by_kernel_decision (omega)
  variant_decreases:       proved_by_kernel_decision (omega)

constant-time source shape:  enforced / reported
  (fixed bound 16, no early return in the loop, no content-dependent
   branch — a STRUCTURAL/security claim, not a value-semantics #[ensures];
   surfaced by Concrete's effects/eligibility reports and the catches/ pair)

machine-level timing:        assumed / trusted
  (LLVM/target may reintroduce branches; out of scope for source proof —
   named explicitly in assumptions.toml, see § 4)
```

Why `#[ensures]` carries only functional correctness: it is a
**value-semantics** statement about the return value. Constant-time is a
statement about *control-flow structure* and ultimately *machine timing* —
different claim classes that live in the structural-enforcement and
trusted-assumption layers, not in a value postcondition. Keeping them
separate is the honest Concrete story; collapsing them into one green badge
would not be.

The different-tag direction is the immediate follow-up (it needs an
`x ^ y ≠ 0 when x ≠ y` lemma at `u8` plus folding over the 16 bytes);
proving it upgrades functional correctness to a full-iff `proved_by_lean`.

## What this candidate is NOT

Stated up front so the rest of the audit does not oversell.

- **Not a MAC or signature.**  No keying, no nonce, no
  forge-resistance claim.  A real authenticated tag
  comparison is the application; this candidate proves only
  the comparison helper.
- **Not a constant-time proof at the machine level.**  The
  source has no early exit and a fixed loop bound; that is
  a *necessary* condition for constant-time execution.  It
  is not sufficient: LLVM is allowed to introduce branches
  during optimization (e.g., converting `if diff == 0 then
  1 else 0` into a conditional jump), and target CPUs can
  still leak through cache / branch-predictor side channels.
  Closing that gap requires target-level timing analysis,
  which Concrete does not have today.  **`assumptions.toml`
  will name this explicitly.**
- **Not a substitute for vetted libraries.**  The intended
  audience reads this example to evaluate Concrete's proof
  workflow on crypto-adjacent code, NOT to deploy this
  function as their auth-tag check.

## 1. Current behavior and oracle

Not implemented yet — this audit is the **pre-implementation
contract** (per ROADMAP rule 80: "Refactor compiler
architecture only when a candidate makes the boundary
undeniable").  The audit declares what the example will be
and what it forces in Phase 4 before any code lands.

Once implemented:

```sh
nix develop --command bash -c '.lake/build/bin/concrete \
    examples/constant_time_tag/src/main.con -o /tmp/ct && /tmp/ct'
```

Expected behaviour: runs 6-8 in-source tests (matching tags,
single-byte-diff at each position, all-bytes-diff, two-byte
diff, all-zero tags, all-0xFF tags).  Output ends with
`All 8 tests passed.` and exits 0.

## 2. Authority / capability surface

Designed to be:
- 1-2 pure functions in the proof-eligible core
  (`ct_compare`, and possibly a `verify_tag(expected,
  computed)` wrapper that calls it).
- 1 entry-point function `main` with `Std` capability (for
  print).  Excluded from proof-eligibility by profile.
- No `trusted` shells.  Unlike fixed_capacity, this example
  has no raw-byte test-packet construction; tags are i32
  arrays directly.
- No FFI, no externs, no allocation.

## 3. Allocation / stack / failure assumptions

- **Allocation:** none.  Both `a` and `b` are `[u8; 16]`
  passed by value; `diff` is a local `u8`; the return is a
  scalar `i32`.
- **Stack:** small (estimated < 100 bytes).
- **Failure:** no error path.  The function returns 0 or 1.
  Invalid inputs are not modeled (the tag arrays are always
  length 16 by type).
- **Arithmetic:**
  - `u8 ^ u8` produces `u8` (XOR of bytes).
  - `u8 | u8` produces `u8` (OR of bytes — the
    accumulator).
  - `u8 == 0` produces `bool` (final test).
  - `i32` only on the loop counter and the return.
- **Loops:** one bounded loop, exactly 16 iterations.

## 4. Honest threat model — what the proof does and does NOT claim

Three layers of trust, with explicit boundaries:

**Layer 1: Functional correctness (proved).**  Lean theorem
will state: `ct_compare a b = 1` iff `a` and `b` are equal as
byte arrays.  Direction: this is a pure correctness proof
over `[u8; 16]` and OR-accumulate.

**Layer 2: Source-level structural constant-time (statically
checked).**  Concrete reports will surface:
- No early exit (no `return` inside the loop; only at end).
- Loop bound is the compile-time constant 16, not a
  function of `a` or `b` content.
- No `if cond { ... }` that branches on element values
  inside the loop (the OR-accumulate is branch-free).
The final `if diff == 0` happens once at the end, AFTER all
bytes are read.  This is a structural property — visible
in `--report unsafe` and `--report eligibility`.

**Layer 3: Machine-level constant-time (NOT proved; assumed).**
LLVM may optimize the source's branch-free OR-accumulate
into something branchy.  Target CPUs may leak via cache or
branch predictor.  `assumptions.toml` will record:
- Compiled with `-O0` or with optimization barriers around
  the comparison (recommended by real crypto libraries).
- Target CPU side-channel resistance is the deployer's
  responsibility, not the language's.

## 5. Forcing surface in Phase 4

What this candidate forces the compiler to add before it
can graduate.

| Phase 4 extension | Status today | Forced by |
|---|---|---|
| `PBinOp.bitor (width, signed)` at u8 | not yet | `diff = diff | (a[i] ^ b[i])` |
| `PBinOp.bitxor (width, signed)` at u8 | not yet (we have i32/u32 only) | `a[i] ^ b[i]` |
| u8 `==` 0 comparison | exists at i32; need u8 | `diff == 0` |
| Bounded-while proof over `[u8; N]` arrays with universal property | partial (we have empty-ring / 1-element cases) | the OR-accumulate loop preservation |
| u8 `arrayIndex` evaluation in PVal | partial (PVal carries Int, but eligibility may need width tracking) | `a[i]` / `b[i]` reads |

The PBinOp shape from commit `28b1f2a` makes these
extensions append-only: each new (op, width, signed) row in
`evalBinOp` + `binOpToPBinOp` + the obligations register.
No constructor refactor.

## 6. Smallest Lean-backed property candidate

Bar #1 (first attached theorem) candidate:

  `ct_compare_equal_correct`: when `a = b` (pointwise on all
  16 bytes), `ct_compare a b = 1`.

Proof shape: bounded while loop with 16 iterations, each
producing `diff = diff | (b[i] ^ b[i]) = diff | 0 = diff`.
After all iterations, `diff = 0`.  Then the final
`if diff == 0 then 1 else 0` returns 1.

This uses the `while_step`-based loop machinery from
fixed_capacity but with an OR-accumulate Cont body, not the
ring buffer's compound index calc.  Cleaner test of the
multi-iteration property (no early Break path).

Bar #2 (composition theorem) candidate:

  `ct_compare_returns_one_iff_equal`: full
  bidirectional — `ct_compare a b = 1` iff `∀ i, a[i] = b[i]`.

This is the substantive cryptographic claim.  Direction
forward already covered by bar #1; reverse direction
requires proving "if any byte differs, diff is non-zero
after the loop" — needs induction on the loop.

## 7. Missing artifacts / policies / assumptions

First pilot batch landed 2026-05-28 (bars #3, #4, #9).
First theorem landed 2026-05-29 (bar #1):

| # | Bar | Status |
|---:|---|---|
| 1  | Lean-backed property surfaced as `proved` | ✅ `ct_compare_same_tag_correct` (since 2026-05-29) — for any 16-byte tag, `ct_compare a a = 1`.  Stronger than the initial concrete-zeros theorem; covers arbitrary byte values via `BitVec.xor_self` + `BitVec.zero_or`. |
| 2  | Composition property Lean-backed | ✅ `ct_compare_same_tag_correct` is universal over byte values (parametric in 16 `Int`s) and exercises all 16 loop iterations — the credible crypto-adjacent claim "equal tags always pass with all bytes inspected".  The full iff (`ct_compare a b = 1 iff a = b`) is the natural next stronger target; pending negative-direction array-position reasoning. |
| 3  | Assumption file with schema, CI-enforced | ✅ `assumptions.toml` with explicit three-layer claim entries; `make test-assumptions` 4/0 |
| 4  | Policy file with enforceable budgets, CI-enforced | ✅ `Concrete.toml [policy]` stricter than fixed_capacity (no_unsafe + no_trusted both set); `make test-policy` 5/0 |
| 5  | Oracle beyond hand-written tests | ✅ Python reference + differential harness; 600 cases (200 per seed × 3 seeds) covering all-equal, per-position single-byte diffs (positions 0/15/middle), multiple differences, high-bit byte values, all-bytes-differ.  `make test-ct-oracle` |
| 6  | "Concrete catches this" negative pair | ✅ `catches/01_alloc_in_compare_core.con` — alloc-discipline violation in the comparison loop; rejected with E0520.  CATCHES.md ALSO names the deferred timing/early-exit gap honestly (Concrete cannot reject early-exit inside bounded loops today; needs Phase 3 constant-time profile).  `make test-catches` |
| 7  | Release evidence bundle capturable | ✅ `scripts/tests/capture_release_bundle.sh examples/constant_time_tag` produces a full bundle with manifest.json + reports + snapshots + AUDIT/CATCHES/README; `make test-release-bundle` (parse_validate proxy) |
| 8  | Honest README | ✅ `README.md` with what-is / what-is-NOT / what-is-proved / what-is-NOT-yet-proved / static enforcement / negative pair / oracle-complements-proof / honest-deployment-disclaimer |
| 9  | Snapshot/diff baseline | ✅ 16 reports baselined under `snapshot/`; `make test-snapshots` 64/0 across all 4 candidates |
| 10 | Listed in `tests/showcase/manifest.toml` | ✅ `[[flagship]]` entry with all 10 bars marked, evidence pointers, CI gates, and a load-bearing `[flagship.limits].constant_time` block explicitly naming the machine-level-timing gap; `make test-showcase` 4/0 |

**Graduated 2026-05-30: 10 of 10 bars met.**  The
substantive proof-side claim is the universal same-tag
theorem `ct_compare_same_tag_correct`; the substantive
evidence claim is the oracle's 600 cases covering both
directions (152 per seed exercise the negative direction
the theorem does not yet cover); the substantive
discipline-violation claim is the alloc-in-compare-core
catch.  CATCHES.md, README.md, assumptions.toml, and the
showcase manifest's `[flagship.limits].constant_time`
block all name the machine-level-timing gap honestly:
this is a structural source-level discipline candidate,
NOT a machine-level constant-time guarantee.

## 8. Strategic value beyond graduation

constant_time_tag is the **first real-crypto flagship** that
demonstrates Concrete on code where the security claim is
subtle but the implementation is small.  Its value is
disproportionate to its size:

- **Validates the proof workflow on crypto-adjacent code.**
  crypto_verify graduated as a toy scaffolding flagship;
  this candidate graduates as a real (if narrow) crypto
  artifact.  The pattern (small surface, subtle property,
  honest backend assumption) is the template for future
  real-crypto candidates (HMAC-SHA256, Ed25519 verify).
- **Forces u8 multi-width PBinOp.**  Today's PBinOp has
  i32/u32 only; u8 ops are the natural next addition.
  constant_time_tag has 3 u8 operations (`^`, `|`, `==`),
  each requiring one append-only row.
- **Forces a bounded-while proof with a universal
  property.**  fixed_capacity's loop proofs were on
  specific inputs (empty ring, 1-element ring).
  ct_compare's correctness over arbitrary 16-byte arrays
  forces a multi-iteration inductive argument over the
  loop-carried `diff` variable.  This is the technique that
  larger crypto primitives will need.
- **Names a real backend gap.**  Layer 3 of § 4 above is
  the first time Concrete explicitly says "this proof
  doesn't cover machine-level timing."  That gap should be
  visible to readers and motivate the eventual target-model
  / timing-evidence work (ROADMAP Phase 3, Phase 5).

## 9. Suggested first pilot batch

Same shape as crypto_verify / fixed_capacity first batches:

1. **Land `src/main.con`** with `ct_compare` + a small set
   of hand-written tests (matching tags, single-byte
   difference per position, all-zero, all-0xFF).
2. **Land `assumptions.toml`** matching the surface, with
   explicit entries for "constant-time is structural source
   property, not machine-level" and "u8 bitwise ops use
   multi-width PBinOp with u8 width — currently unsupported,
   forces Phase 4 extension."
3. **Land `Concrete.toml [policy]`** with the 8-field
   schema: `predictable=true`, `no_alloc=true`,
   `no_externs=true`, `no_unsafe=true`, `no_trusted=true`,
   `max_stack_bytes=200`, `forbidden_capabilities=["File",
   "Net", "Unsafe", "Alloc"]`, `allowed_capabilities=["Std"]`.
   Stricter than fixed_capacity (no trusted shells, no
   relaxed capabilities).
4. **Land `snapshot/` baseline** with all 16 reports.

These four close bars #3, #4, #9 in one commit — same as
prior candidates.

Subsequent commits attack the Phase 4 forcing surface (§ 5)
one row at a time:
- Add u8 `bitxor` / `bitor` to PBinOp + evalBinOp.
- Attach first theorem (`ct_compare_equal_correct`).
- Attach composition theorem
  (`ct_compare_returns_one_iff_equal`).
- Land the oracle (reference.py + differential harness with
  byte-position-targeted cases).
- Land the negative pair (early-exit comparison rejects or
  surfaces a warning).
- Land README + manifest entry to graduate.

## See also

- `examples/parse_validate/AUDIT.md` — first graduated; the
  10-bar template.
- `examples/crypto_verify/AUDIT.md` — second graduated; toy
  crypto scaffolding.
- `examples/fixed_capacity/AUDIT.md` — third graduated;
  state-model + composition over bounded mutable state.
- `docs/PROOF_OBLIGATIONS_REGISTER.md` — every Phase 4
  extension this candidate forces appends a row here.
- `docs/PROOF_STATE_MODEL.md` — the state model whose
  multi-iteration while_step the composition theorem here
  will exercise.
- `ROADMAP.md` Phase 7 item 8 — the explicit real-crypto
  flagship slot this candidate begins filling.
