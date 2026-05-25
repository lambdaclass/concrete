# fixed_capacity — Pilot Audit

Status: **active pull-through candidate** per ROADMAP Active
Dependency Order rule 2 (parse_validate graduated 2026-05-22,
crypto_verify graduated 2026-05-23, fixed_capacity slot opened
2026-05-23).

## Why this example

Third candidate. parse_validate did parsing; crypto_verify did
authentication (toy). fixed_capacity demonstrates the **bounded /
no-allocation predictable subset** on a realistic message
processor — Phase 7 item 6 names this category explicitly:
"ring buffer, bounded queue, bounded-state controller, fixed
parser state machine."

The structural value of this candidate is that **it forces the
Phase 4 ProofCore extractor extensions** the next real-crypto
flagship is gated on. ROADMAP Phase 4 item 2 enumerates the named
subgoals; this example surfaces every one in concrete blocked
functions:

| ProofCore gap | Blocked function(s) in fixed_capacity |
|---|---|
| struct literal | `err_result`, `ok_result`, `ring_new`, `validate_payload` |
| field access | `process_packet`, `ring_contains`, `ring_record` |
| cast | `validate_payload`, `process_packet` |
| while loop | `compute_tag`, `ring_record` |
| array literal | `validate_payload` |
| array index assignment | `ring_record` |
| `mod` operator | `ring_record` |

Closing these gaps in Phase 4 unlocks both `fixed_capacity`'s
graduation AND the real-cryptography flagship slot the
`crypto_verify` toy explicitly reserved.

## 1. Current behavior and oracle

```sh
nix develop --command bash -c '.lake/build/bin/concrete \
    examples/fixed_capacity/src/main.con -o /tmp/fc && /tmp/fc'
```

Runs 8 in-source tests covering valid messages, replay detection,
and every error variant. Output ends with `All 8 tests passed.`
then `0`. Final exit 0.

20 functions: 18 pure + 4 marked `trusted` (raw byte writes for
test-packet construction). 2 entry-points (a test entry plus
`main`).

Phase A oracle vector has this example as **PENDING** today
because it interleaves `print` / `println` with the return value
— a known limit of the oracle harness contract (only the int
return is compared). Differential testing needs the per-example
oracle (the `oracle/` pattern parse_validate established).

## 2. Authority / capability surface

- 18 pure functions; 4 trusted shells; 2 entry points use `Std`
  capability set for `print`/`println` output.
- 0 allocations.
- The 4 `trusted` functions are deliberate: they construct
  adversarial test packets via raw byte writes through `&mut`
  array references. Each is small, narrow, and confined to the
  test-driver section of the source — not the validation core.

## 3. Allocation / stack / failure assumptions

- **Allocation:** none. Everything is stack-allocated, including
  the 256-byte `MsgBuf` and 16-slot `RingBuf`.
- **Stack:** max 2960 bytes (main). Within budget for hosted
  systems; would need careful examination for freestanding /
  embedded targets.
- **Failure:** every validator returns an explicit error code
  (1–7); the closed `ValidateResult` enum carries the failure
  reason. No abort, no panic.
- **Arithmetic:** `i32` and `u8` for header bytes; the XOR fold
  uses i32 throughout.
- **Loops:** the only loops are `compute_tag` (bounded by count)
  and `ring_record` (bounded by 16). Both extract-blocked on
  while-loop support — Phase 4 item 2(b).

## 4. Report coverage — present vs missing

All 16 reports run cleanly. Substantive content:

- `caps` / `authority` / `effects` — 20 functions, 18 pure +
  2 capability'd (main, run_test).
- `eligibility` — 14 eligible, 2 excluded (entry points),
  4 trusted.
- `proof-status` — **0 proved, 5 unproved (extract clean),
  9 blocked, 2 ineligible, 4 trusted.** The 5 already extracting
  are the trivial integer validators (`validate_version`,
  `validate_msg_type`, etc.) — same shape as parse_validate.
- `stack-depth` — max 2960 bytes.

## 5. Smallest Lean-backed property candidate

**Easy wins** (already extracting, same proofs would copy from
parse_validate verbatim):

- `validate_version_v2` / `validate_msg_type_v2` / etc. — the
  integer validator shapes. Trivially provable today.

**Real composition theorem candidate** — the same
`validate_header_fields_success` shape that worked for
parse_validate, applied to fixed_capacity's int-input validator
chain. Provable today using existing ProofCore.

**Bar #2 candidate that requires Phase 4 work**:
`compute_tag(buf, count)` — the XOR-fold checksum. Blocked on
while-loop extraction. Closing Phase 4 item 2(b) (bounded while
loops) makes this provable.

## 6. Missing artifacts / policies / assumptions

**Already met:**

- ✅ Predictable profile passes
- ✅ All reports clean
- ✅ 5 of 14 eligible functions already extract (no theorems
  attached yet)

**Missing (graduation-blocking):**

1. **No `assumptions.toml`.** Copy parse_validate's pattern.
2. **`[policy]` is `predictable = true` only** — extend to the
   8-field schema, with caveats: this example uses `Std` in the
   entry points, so `allowed_capabilities` cannot be `[]`. Either
   allow `Std` explicitly or split the example's policy by
   function role (entry vs validation core).
3. **No attached Lean theorems.** Need at minimum bar #1 (one
   theorem on a single-integer validator) and bar #2 (composition).
4. **No `catches/` negative pair.**
5. **No `snapshot/` baseline.**
6. **No `oracle/` directory** — and the oracle pattern from
   parse_validate compares int main returns; this example's
   stdout is multi-line. Either widen the harness contract or
   write a `verify_message`-like wrapper that returns int.
7. **No `README.md`** with honest framing.
8. **No `CATCHES.md`** narrative.
9. **Not yet auto-bundleable** — release-bundle script works,
   but the example's artifacts (AUDIT, CATCHES, README) don't
   exist yet to flesh it out.
10. **Not in `tests/showcase/manifest.toml`.**

## 7. Graduation contract (10 bars)

| # | Bar | Status |
|---:|---|---|
| 1 | Lean-backed property surfaced as `proved` | ✅ `ring_new_correct` attached (2026-05-24); first proof in the project that composes arrayLit + structLit + letIn under the kernel |
| 2 | Composition property Lean-backed | ⏳ scalar-validator composition provable today; whole-flow composition blocked on Phase 4 while-loop / struct-literal / cast |
| 3 | Assumption file with schema, CI-enforced | ✅ `assumptions.toml` with trusted list; `make test-assumptions` 3/0 |
| 4 | Policy file with enforceable budgets, CI-enforced | ✅ `Concrete.toml [policy]` 6 fields enforced (omits no_unsafe/no_trusted since 4 trusted shells are by design); `make test-policy` |
| 5 | Oracle beyond hand-written tests | ❌ absent; harness needs widening or wrapper |
| 6 | "Concrete catches this" negative pair | ❌ absent |
| 7 | Release evidence bundle capturable | ⏳ infrastructure exists; artifacts to come |
| 8 | Honest README | ❌ absent |
| 9 | Snapshot/diff baseline | ✅ `examples/fixed_capacity/snapshot/` 16 reports baselined; `make test-snapshots` 48/0 across all 3 candidates |
| 10 | Listed in `tests/showcase/manifest.toml` | ❌ absent |

**Today: 4 of 10 bars met (#1, #3, #4, #9), with bar #1 now
backed by 4 attached theorems and zero remaining extraction
blockers.** Bar #1 closed 2026-05-24 with `ring_new_correct`;
extended 2026-05-25 with `compute_tag_zero_correct` (first
while-loop theorem), `ring_push_zero_correct` (first arraySet
theorem), and `ring_contains_empty_correct` (first while_step
theorem — empty-ring case). Every extraction-eligible
function in fixed_capacity is now either proved or has a
clean `no proof` slot — 0 blocked. Remaining graduation bars
are content work (#5 oracle, #6 catches, #7 bundle, #8 README,
#10 manifest) + the composition theorem (#2).

## 8. Strategic value beyond graduation

fixed_capacity is **the most strategic candidate so far** because
its 9 extraction-blocked functions are exactly the Phase 4
extractor extensions Phase 7 item 7 (real cryptography flagship)
is gated on. Each Phase 4 extension landed against fixed_capacity
unlocks downstream work:

- **struct literal** → unblocks `parse_header`'s `Result::Ok` /
  `Result::Err` construction in parse_validate too.
- **field access** → unblocks `error_code` match-arm field reads.
- **while loop** → unblocks `compute_checksum` proofs and any
  SHA-256-style round loop.
- **cast** → broadly useful, especially `as` between integer
  widths.
- **array indexing** (read) — already extracts; **array index
  assignment** (write) is the Phase 4 subgoal here.

The graduation timeline for fixed_capacity is gated on Phase 4
progress. Unlike parse_validate (which graduated under existing
ProofCore) and crypto_verify (which graduated on the same
ProofCore + composition pattern), fixed_capacity's full
graduation requires real compiler work in the Phase 4 extractor.

## 9. Suggested first pilot batch

Same shape as crypto_verify's first batch:

1. **Land `assumptions.toml`** matching the actual surface.
2. **Extend `Concrete.toml` [policy]`** with the 8-field schema,
   relaxing `allowed_capabilities` to include `Std` (or splitting
   by function role).
3. **Land `snapshot/` baseline.**

These three close bars #3, #4, #9 in one commit, same as
crypto_verify's first batch did.

Subsequent commits would then attack the Phase 4 ProofCore
extensions one at a time, with each extension forcing closure of
both fixed_capacity's blocked functions and unblocking the
real-crypto flagship slot. That work is the bridge between this
audit and graduation.

## See also

- `examples/parse_validate/AUDIT.md` — template this audit follows.
- `examples/crypto_verify/AUDIT.md` — second candidate to graduate.
- `tests/showcase/manifest.toml` — graduation target.
- ROADMAP Phase 4 item 2 — the extractor extensions this example
  surfaces by name.
- ROADMAP Phase 7 item 7 — real-cryptography flagship slot
  gated on those extensions.
