# constant_time_tag — Candidate Audit

Status: **active pull-through candidate** per ROADMAP Active
Dependency Order rule 2.  fixed_capacity graduated
2026-05-28; constant_time_tag slot opened 2026-05-28.

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

First pilot batch landed 2026-05-28 (bars #3, #4, #9):

| # | Bar | Status |
|---:|---|---|
| 1  | Lean-backed property surfaced as `proved` | ❌ pending Phase 4 forcing (u8 bitor + u8 bitxor + universal-property while_step proof) |
| 2  | Composition property Lean-backed | ❌ pending bar #1 |
| 3  | Assumption file with schema, CI-enforced | ✅ `assumptions.toml` with explicit three-layer claim entries; `make test-assumptions` 4/0 |
| 4  | Policy file with enforceable budgets, CI-enforced | ✅ `Concrete.toml [policy]` stricter than fixed_capacity (no_unsafe + no_trusted both set); `make test-policy` 5/0 |
| 5  | Oracle beyond hand-written tests | ❌ |
| 6  | "Concrete catches this" negative pair | ❌ |
| 7  | Release evidence bundle capturable | ❌ |
| 8  | Honest README | ❌ |
| 9  | Snapshot/diff baseline | ✅ 16 reports baselined under `snapshot/`; `make test-snapshots` 64/0 across all 4 candidates |
| 10 | Listed in `tests/showcase/manifest.toml` | ❌ |

**Today: 3 of 10 bars met (#3, #4, #9), and the Phase 4
forcing surface from § 5 is closed.**  Source landed
2026-05-28 with extraction blocked exactly as the audit
predicted.  On 2026-05-29 the u8 PBinOp extensions landed
(R-17 bitxor extended to u8; new R-21 bitor at u8); the
candidate now extracts (`ct_compare` is `no proof` on
`--report proof-status`, not `blocked`).  Next batch is
the first attached theorem (bar #1: `ct_compare_equal_correct`).

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
