# fixed_capacity

A bounded message validator with no allocation, in pure
Concrete.  Third flagship candidate, following the same
honest-framing template as `parse_validate` and `crypto_verify`.

This is the **bounded / no-allocation systems** flagship that
the Phase 7 corpus needs alongside the parsing (parse_validate)
and crypto-adjacent (crypto_verify) flagships.  Ring buffer,
message validator, XOR-fold tag, and a composition theorem
over bounded mutable state, all kernel-checked.

## What this example is

Twenty functions total; the proof-relevant core is:

- `validate_version`, `validate_msg_type`, `validate_payload_len`,
  `validate_total_len`, `validate_tag` — pure scalar validators
- `read_u8`, `read_u16_be` — safe byte readers via `arr[i] as i32`
- `compute_tag(buf) -> i32` — bounded XOR fold of header bytes 0..5
- `ring_new() -> RingBuf` — canonical empty ring (16 zeros)
- `ring_push(rb, val) -> RingBuf` — functional update at `head % 16`
- `ring_contains(rb, val) -> i32` — bounded scan (up to 16 iters)
- `validate_message(buf, ring) -> ValidateResult` — composition
- 4 trusted shells for adversarial test-packet construction
- 2 `with(Console)` entry points (`main`, `run_test`)

```sh
nix develop --command bash -c '.lake/build/bin/concrete \
    examples/fixed_capacity/src/main.con -o /tmp/fc && /tmp/fc'
```

Runs 8 in-source tests (every error variant + replay).  Output
ends with `All 8 tests passed.` and exits 0.

## What this example is NOT

**This is not a real network parser.**  The message format is a
purposely-tiny illustrative shape: 8-byte header, XOR-fold tag,
optional payload up to 240 bytes.  A real protocol parser (HTTP,
TLS framing, ELF, etc.) would have more structure and more
edge cases.  The value of this example is the **bounded-state
discipline + proof-side composition**, not the protocol.

**The XOR-fold tag is not real cryptography.**  Same caveat as
`crypto_verify`: this is a structural integrity check at the
fixed-size header level, not a MAC.  An adversary can trivially
forge tags.

## What is proved

Four Lean theorems, checked by the Lean kernel at `make build`:

```
Concrete.Proof.ring_new_correct
  ring_new() = RingBuf{ data: [0×16], head: 0, count: 0 }

Concrete.Proof.compute_tag_zero_correct
  compute_tag on a 6-zero-prefix buffer returns 0

Concrete.Proof.ring_push_zero_correct
  pushing v into the empty ring yields
  RingBuf{ data: [v, 0×15], head: 1, count: 1 }

Concrete.Proof.ring_push_then_contains_correct
  the 1-element ring containing v at index 0 has
  ring_contains(v) = 1   (the composition theorem)
```

`--report proof-status` reports all 4 as `proved`.

The composition theorem is the central proof-side claim:
**push then contains finds the value, end-to-end, kernel-
checked**.  It exercises functional array update (`arraySet`),
the bounded while loop with rich body (`while_step` + LoopStep
enum), and BitVec mod at i32 width — the full state-model
surface from `docs/PROOF_STATE_MODEL.md`.

A non-composition empty-ring corollary
(`ring_contains_empty_correct`) is also kernel-checked.

## What is NOT yet proved

Stated up front:

- Theorems hold for **specific inputs** (empty ring, 1-element
  ring with the pushed value).  General properties parameterized
  over arbitrary ring state are follow-up work — they need
  multi-iteration while_step reasoning.
- The 5 scalar validators (`validate_version`, `validate_msg_type`,
  etc.) extract cleanly but have no attached theorems.  Their
  proofs would copy verbatim from parse_validate's shape.
- `validate_message`, the composition, extracts to PExpr but is
  not yet proved end-to-end against arbitrary input.
- The Lean spec is at PExpr level, not LLVM machine code.
  Phase 12 will eventually prove the extraction preserves
  semantics; today the gap is documented in `assumptions.toml`.

## What is enforced (statically, by the compiler)

- **Bounded allocation.** `--report alloc` is empty.  Every
  buffer is stack-allocated (`MsgBuf` is `[u8; 256]`, `RingBuf`
  is `[i32; 16]`).  No heap.
- **Predictable profile.** `--check predictable` passes for
  the 18 pure functions; the 2 `with(Console)` entry points
  are appropriately excluded.
- **Bounded loops.** Every loop has a compile-time iteration
  bound (6, 16, payload_len, count).  `--report eligibility`
  classifies all eligible loops as `bounded`.
- **Trust surface enumerated.** The 4 trusted shells (raw
  byte writes for test-packet construction) are listed in
  `assumptions.toml` so a new trusted shell surfaces as drift.
- **Stack budget honored.** `--report stack-depth` reports max
  2960 bytes, within the policy budget of 3000.

## What is enforced (by CI gates)

| Gate | Asserts |
|---|---|
| `make test-policy` | `Concrete.toml [policy]` — 6 enforced fields |
| `make test-assumptions` | `assumptions.toml` matches reports |
| `make test-catches` | The negative case rejects with the documented diagnostic |
| `make test-snapshots` | 16 report snapshots byte-identical to baseline |
| `make test-fc-oracle` | Python reference + 600 seeded generated cases agree |
| `make test-verify-gates` | Pass-by-pass compiler gates green |

## What is assumed

See `assumptions.toml`.  Summary: hosted-libc host; i32 / u8
wrapping semantics modeled at i32 width via `BitVec` round-trip
on `mod` and `bitxor`; no heap; the 4 trusted shells trusted
for raw byte writes confined to test-packet construction; no
externs; no FFI; only `Std` capability granted (used by entry
points for `println`).

## Where the trust boundary sits

Three layers, same as parse_validate and crypto_verify:

1. **Lean kernel** checks the four theorems above.  Trust
   anchor: `Lean kernel + Concrete.Proof.*`.
2. **Concrete compiler** extracts the properties and reports
   them.  Trusted, not yet Lean-verified (Phase 12).  The
   `evalWhileStep` factoring (commit `7743579`) is the lemma
   surface Phase 12 will state preservation against.
3. **Toolchain + runtime.** LLVM, clang, linker, libc.  Not
   verified; `assumptions.toml` records the host/toolchain
   exercised.

## The negative pair

`catches/01_alloc_in_bounded_core.con` is the same `ring_push`
shape with an `audit_log` helper that needs `with(Alloc)`.
Concrete refuses to compile it with `E0520 — requires Alloc
but caller has (none)`.

The accepted example proves the language can express the
bounded-allocation discipline; the rejected companion proves
the language refuses the violation.  See `CATCHES.md`.

## Graduation status

5 of 10 bars met as of HEAD; remaining bars are #5 (oracle —
just landed), #6 (catches — just landed), #7 (release bundle),
#8 (this README), #10 (showcase manifest entry).  Bar #2
(composition theorem) was closed by
`ring_push_then_contains_correct`, which validates the
substance: a 2-function chain over bounded mutable state,
kernel-checked.  Bars #1, #3, #4, #9 were closed earlier.

See `AUDIT.md` for the full graduation contract and per-bar
state.

## See also

- `AUDIT.md` — graduation bars and progress.
- `CATCHES.md` — negative-pair narrative.
- `assumptions.toml` — declared trust surface.
- `Concrete.toml` `[policy]` — enforced budgets.
- `src/proof-registry.json` + `Concrete/Proof.lean` — attached
  theorems.
- `docs/PROOF_STATE_MODEL.md` — the state-model design that
  the composition theorem validates.
- `examples/parse_validate/` and `examples/crypto_verify/` —
  the graduated siblings; this README's structural template.
