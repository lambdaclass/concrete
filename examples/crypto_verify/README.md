# crypto_verify

A toy authenticated-tag model in pure Concrete. **Active
pull-through candidate** per ROADMAP Active Dependency Order rule 2
(slot taken 2026-05-22 after parse_validate graduated).

Second-domain flagship from parse_validate's parsing focus. The
domain is authentication / integrity — Phase 7 item 6 names this
explicitly.

This README follows the honest-framing template established by
`examples/parse_validate/README.md`.

## What this example is

Four functions:

- `compute_tag(key, message, nonce) -> Int` — pure: `key * message + nonce`
- `verify_tag(key, message, nonce, expected) -> Int` — pure: 1 iff tag matches
- `check_nonce(nonce, max) -> Int` — pure: 1 iff `0 < nonce ≤ max`
- `verify_message(key, message, nonce, expected, max) -> Int` — composed: 1 iff both validators succeed

Plus a pure `main()` that exercises the core and returns 0.

```sh
nix develop --command bash -c '.lake/build/bin/concrete \
    examples/crypto_verify/src/main.con -o /tmp/cv && /tmp/cv'
```

Output: `0`.

## What this example is NOT

**This is not real cryptography.** The tag function `key * message
+ nonce` is invertible; the "key" offers no real secrecy. The
author docstring in `src/main.con` says this directly: the value is
in the **proof structure**, not the algorithm.

The proof shape (correctness, rejection, range validation,
composition) would be identical for a real HMAC-SHA256 wrapper. The
toy is the smallest demonstrator; a real flagship would replace
the algorithm and keep the proof scaffolding.

## What is NOT yet done

Stated up front so the rest of this document does not oversell.

- **Not yet graduated.** Currently 9 of 10 graduation bars met
  (`AUDIT.md`). The remaining decision is whether to graduate this
  as an explicitly toy crypto-adjacent proof-structure showcase or
  park it until a real algorithm/wrapper exists.
- **No real cryptographic algorithm.** Toy multiplicative MAC; see
  above. A real HMAC / Ed25519 / etc. wrapper would replace the
  core and keep the proof scaffolding.
- **Composition theorem proves the success direction only.** Same
  caveat as parse_validate's composition: failure-direction cases
  are follow-up small theorems.

## What is proved

Four Lean theorems, checked by the Lean kernel at `make build`:

```
Concrete.Proof.compute_tag_correct   : compute_tag(k, m, n) = k*m+n
Concrete.Proof.verify_tag_correct    : verify_tag = 1 ↔ tag matches
Concrete.Proof.check_nonce_correct   : check_nonce = 1 ↔ 0 < n ≤ max
Concrete.Proof.verify_message_composed_correct
                                     : both validators pass → returns 1
```

`--report proof-status` reports all 4 as `proved`. The composition
theorem is the central thesis claim: when both sub-validators
succeed, the composed verifier returns success.

## ProvableV1 conformance

Every proof-eligible function in `crypto_verify` fits the
`ProvableV1` profile (see [`docs/PROVABLE_V1.md`](../../docs/PROVABLE_V1.md)).
The four pure scalar functions use only constructs in the supported
surface: integer arithmetic, comparisons, let bindings,
if/then/else, direct calls. Each registered theorem has a `coverage`
field (`iff`, `one_direction`). The "toy crypto" caveat is at the
algorithm level, not the `ProvableV1` level — a real cryptographic
primitive flagship would inherit the same conformance shape.

## What is enforced (statically, by the compiler)

- **Pure surface.** All 4 user functions and `main` are `(pure)`.
- **No allocation.** `--report alloc` is empty.
- **No unsafe / no trusted.** No FFI, no trusted shells.
- **Bounded stack.** Max 200 bytes.
- **Predictable profile.** `--check predictable` passes.

## What is enforced (by CI gates)

| Gate | Asserts |
|---|---|
| `make test-policy` | `Concrete.toml [policy]` — 8 enforced fields |
| `make test-assumptions` | `assumptions.toml` matches reports |
| `make test-catches` | The negative case rejects with the documented diagnostic |
| `make test-snapshots` | 16 report snapshots byte-identical to baseline |
| `make test-cv-oracle` | Python reference + 600 seeded generated cases agree with Concrete |
| `make test-verify-gates` | Pass-by-pass compiler gates green |

## What is assumed

See `assumptions.toml`. Summary: any llvm/hosted-libc host;
wrapping Int (i64) arithmetic — note the toy tag function uses i64
multiplication, overflow is part of the "toy" framing; no heap; no
externs; no trusted boundaries; no capabilities granted.

## Where the trust boundary sits

Three layers, same as parse_validate:

1. **Lean kernel** checks the four theorems. Trust anchor:
   `Lean kernel + Concrete.Proof.*`.
2. **Concrete compiler** extracts the properties and reports them.
   Trusted, not yet Lean-verified (Phase 12).
3. **Toolchain + runtime.** LLVM, clang, linker, libc. Not
   verified; `assumptions.toml` records the host/toolchain/runtime
   exercised.

## The negative pair

`catches/01_alloc_in_pure_core.con` is the same example with
allocation introduced into `verify_tag`. Concrete refuses to
compile it with `E0520 — requires Alloc but caller has (none)`.

The accepted example proves the language can express the
zero-allocation verification property; the rejected companion
proves the language refuses the violation. See `CATCHES.md`.

## Graduation status

9 of 10 bars met (`AUDIT.md`). The last bar is the Phase 7
manifest decision. If it graduates now, it must be framed as a
toy crypto-adjacent proof-structure showcase, not as real
cryptographic assurance.

## See also

- `AUDIT.md` — graduation bars and progress.
- `CATCHES.md` — negative-pair narrative.
- `assumptions.toml` — declared trust surface.
- `Concrete.toml` `[policy]` — enforced budgets.
- in-source `#[proof_by]`/`#[spec]`/`#[proof_fingerprint]` links in
  `src/main.con` + `Concrete/Proof.lean` — attached theorems.
- `examples/parse_validate/` — the graduated sibling; this
  README's structural template.
