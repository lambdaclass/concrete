# AXIOMS — what the Lean proof layer itself trusts

`TRUSTED_COMPUTING_BASE.md` names the language-level trust boundary. This
file names the **proof-layer** trust boundary: what a theorem backing a
`proved_by_lean` / `proved_by_kernel_decision` fact actually depends on,
checked mechanically by `scripts/tests/check_axiom_inventory.sh`
(ROADMAP Phase 10 #16). The gate runs `#print axioms` over every theorem
named by a `#[proof_by(...)]` attribute and fails on anything not
documented here.

Replay: `bash scripts/tests/check_axiom_inventory.sh`

## Tier 0 — kernel axioms (allowlisted)

Standard classical Lean. Every theorem may depend on these and nothing else:

- `propext` — propositional extensionality
- `Classical.choice` — choice
- `Quot.sound` — quotient soundness

These are part of trusting the Lean kernel at all; they are not
Concrete-specific.

## Tier 1 — native-code trust (named, theorem-by-theorem)

`Lean.ofReduceBool` and `Lean.trustCompiler` extend trust from the Lean
kernel to the **Lean compiler and native code generator**. They enter via
`native_decide`, and — less obviously — via `bv_decide`, whose LRAT
certificate checker runs as compiled Lean. A theorem proved "by
kernel-checked bitblasting" is therefore *not* kernel-only: the SAT
certificate validation step is native code.

Wherever a report or doc says `proved_by_kernel_decision (bv_decide)`,
read it as "kernel-checked reflection over a natively-executed certificate
check" — honest, but a larger TCB than `omega`.

Theorems currently granted this trust (must match
`scripts/tests/axiom_native_trust.txt`; the gate fails on any unlisted
addition):

- `Examples.HmacSha256.Proofs.hmac_sha256_refines_spec`
- `Examples.HmacSha256.Proofs.round_refines_list`
- `Examples.HmacSha256.Proofs.sha256_compress_at_refines_spec`
- `Examples.HmacSha256.Proofs.sha256_compress_refines_spec`
- `Examples.HmacSha256.Proofs.sha256_hash_refines_spec`
- `Examples.HmacSha256.Proofs.state_to_bytes_refines_spec`

Reason: the SHA-256 refinement stack discharges word-level identities with
`bv_decide`; kernel-only LRAT checking at this goal size is currently
impractical. Revisit if `bv_decide` gains a kernel-reduction mode or the
goals shrink.

`Concrete.Sha256Spec`'s RFC test vectors and `Concrete.Diagnostic`'s render
self-test also use `native_decide`, but they are anonymous
`example`s/internal checks, not proof evidence, so they are outside the
gate's scope.

## Tier 2 — forbidden

- `sorryAx` — an incomplete proof can never back evidence. Gate fails hard.
- Any user-declared `axiom` — Concrete's proof layer declares none, and the
  gate keeps it that way. (The planned float profile will introduce a named
  trusted axiom layer, `float_semantics_trusted` — see ROADMAP Phase 11;
  when it lands it becomes a Tier 1-style named entry here, not a silent
  allowlist widening.)

## Fixture-only theorem names

Some `#[proof_by(...)]` attributes in test fixtures name theorems that do
not exist as Lean constants (`Concrete.Proof.pure_add_correct`,
`pure_sub_correct`, `pure_mul_correct`, `left_add_correct`,
`right_add_correct`, `compute_checksum_correct`, `PureAdd.add_comm`,
`Nonexistent.Module.totally_fake_theorem`, `Examples.DoesNotExist.*`).
These exercise the documented limitation that
`proof-status` validates fingerprints, not names — `concrete prove --check`
is the net that catches fabricated names. The gate keeps this list closed:
a *new* unresolvable name is a broken proof link and fails.

## What no axiom check can see (unproven trust, tracked elsewhere)

The axiom inventory bounds what *Lean* assumes. The following links in the
evidence chain are trusted without a theorem at all; they are the deeper
items, tracked in the ROADMAP, not silently assumed here:

- **Core→PExpr extraction preservation** — per-rule theorems in progress
  (ROADMAP Phase 12, R-01…R-28). Until complete, "the extracted body" is
  trusted to represent the Core IR.
- **PExpr evaluator** — `partial def eval`; its agreement with the
  interpreter and compiled code is tested, not proved (Phase 12).
- **BitVec ↔ LLVM semantics** — Lean's `BitVec` ops stand in for LLVM
  instruction semantics with no correspondence proof (Phase 12).
- **Unbounded-`Int` proof model vs fixed-width runtime** — see
  `PROOF_CONTRACT.md`; proofs hold within representable ranges.
- **The Lean toolchain itself** — pinned in `lean-toolchain`; version drift
  is a Phase 10 #15 recheck trigger.
