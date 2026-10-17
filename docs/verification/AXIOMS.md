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
`native_decide`, and — less obviously — via `bv_decide`, whose bit-blasting
and LRAT certificate check run as compiled Lean. A theorem proved "by
kernel-checked bitblasting" is therefore *not* kernel-only: the kernel checks
the reflection, but the bit-blasting and certificate validation are native
code. (Lean 4.34 spells this trust as a fresh axiom per call rather than
`Lean.ofReduceBool`; see below.)

Wherever a report or doc says `proved_by_kernel_decision (bv_decide)`,
read it as "kernel-checked reflection over natively executed bit-blasting
and certificate check" — honest, but a larger TCB than `omega`.

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

### What the native axioms assert, and what is corroborated

In Lean 4.34 each SAT-backed `bv_decide` call mints one axiom, `<lemma>._native.bv_decide.ax_<n>`,
stating a closed computation: `verifyBVExpr expr cert = true` — bit-blast the reflected goal to a
CNF and check the solver's LRAT certificate against it, evaluated by compiled Lean rather than the
kernel. Four lemmas own all six such axioms (pinned in `scripts/tests/axiom_native_owners.txt`):

- `Concrete.Proof.and255_lo`
- `Examples.HmacSha256.Proofs.be64_low_byte` (three axioms)
- `Examples.HmacSha256.Proofs.ch_refines`
- `Examples.HmacSha256.Proofs.round_refines`

Every native-trust theorem — the six evidence theorems above and 29 lemmas between them, 35 in all —
rests on these six and nothing else. (Kernel evaluation of the axiom's statement does not go through:
`decide +kernel` gets stuck in `verifyBVExpr`, which is why `bv_decide` evaluates it natively.)

`scripts/tests/check_bv_certificates.sh` (`make test-bv-certificates`, CI under
`nix develop .#provers`) corroborates the SAT side with **drat-trim**, a separate implementation in
C. It captures the CNF Lean hands its solver through `set_option sat.solver`, pointed at
`scripts/bv_capture_solver.sh` (Lean deletes the files and has no option to keep them; the shim
copies the CNF aside and delegates to the real `cadical` unchanged), re-solves it independently for a
textual DRAT proof (Lean's proof is binary LRAT, which drat-trim does not read), and checks that.
It does so for sample goals and, since 2026-10-15, for **the project's own goals**: it re-elaborates
the files defining the owner lemmas through the shim and requires one captured SAT problem per
native axiom (six for six).

**Measured, and the reason this section was rewritten:** all six of the project's SAT problems are
trivial — one variable, two clauses. `bv_decide`'s normalization and AIG construction already reduced
each goal to a contradiction, so there is no non-trivial certificate to check, and drat-trim confirms
only that a contradiction is a contradiction. The earlier wording here ("a bug in Lean's native LRAT
checker alone cannot make an unsound bit-blasting claim pass unnoticed") was true of the samples and
says nothing about the project's goals. The trust these axioms carry is the **compiled bit-blasting**
— that `verifyBVExpr`, run natively, computed the CNF the goal denotes — which no certificate check
reaches, drat-trim's included. The gate reports the trivial/non-trivial split rather than asserting
it, so a future goal that needs a real certificate is checked there.

Boundaries the gate enforces on itself:

- **drat-trim is itself unverified C.** Two independent checkers agreeing is strictly better than one
  and strictly weaker than a verified checker (`cake_lpr` is not packaged in nixpkgs, nor SMTCoq).
- **A trivially contradictory CNF is reported `VERIFIED` without the proof being read** (drat-trim
  warns `stop reading proof`). Those are counted separately; the sample goals must include a
  non-trivial certificate, and drat-trim must reject an empty certificate, so a rubber-stamping
  checker cannot pass.

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

## Re-checked outside the Lean kernel (con-leche, nanoda)

Every theorem in Concrete's Lean sources is re-checked by checkers that share no code with the Lean
kernel. Replay: `bash scripts/tests/check_external_checker.sh` (CI job `External proof re-check`).

- **[con-leche](https://github.com/leanprover/con-leche)** — an external Lean checker with a
  machine-checked consistency proof. If it accepts a stream of declarations, that environment has a
  model in ZF set theory with an ω-chain of Grothendieck universes in which every constant is a
  member of its type (`model_exists`); in particular no proof of `False` is accepted
  (`no_False_declaration`, both in `ConLeche/MainTheorem.lean`). Run with `--verified`, the mode the
  theorem is about.
- **[nanoda](https://github.com/ammkrn/nanoda_lib)** — an independent checker in Rust, with no code
  shared with the Lean kernel or con-leche. No consistency proof: it adds implementation diversity.

**Population.** Every theorem the `Concrete` and `Examples` libraries declare, read from the
elaborated environment (`scripts/tools/proof_layer_theorems.lean`), not grepped from source; the
`#[proof_by]` evidence theorems are a subset. Measured at `ab01f856`: 3869 theorems, 48 of them
evidence; 3834 on the three standard axioms, 35 on native trust; no `sorryAx`.

**What is established, per population:**

- **The 3834 standard-axiom theorems** are exported with lean4export (format 3.1.0, built with this
  repository's Lean 4.34.1 so it reads the same `.olean` files) and **accepted by con-leche --verified**
  (14385 declarations including Lean core dependencies, about six seconds) **and by nanoda** (16377).
- **The 35 native-trust theorems**, HMAC/SHA-256 among them, are declined by con-leche, which accepts
  only the three standard axioms. nanoda **accepts them with exactly the six native axioms above
  permitted**: every step of those proofs is re-checked except the six natively evaluated
  `verifyBVExpr` facts, which the section above describes.
- **Statement fidelity.** lean4export is unverified, and con-leche's theorem is about what it was
  handed. The gate renders every exported statement and the statement in Lean's own environment in
  one canonical form and requires all 3869 to be equal byte for byte. The form is the kernel's view:
  binder names, binder annotations and the `let` `nondep` flag are erased. That is required, not a
  convenience: lean4export shares one index among expressions equal under Lean's `Expr` BEq, which
  ignores exactly those, so the export carries whichever spelling it met first (measured: 185
  statements differ in binder names and 32 in `nondep` before erasure, none after).

**The gate checks the checkers.** One theorem is handed another theorem's proof: con-leche must reject
it as a type mismatch at that theorem, nanoda must fail its definitional-equality check. The reason
is checked, not the exit code — a first draft of this control was green on a malformed stream that
never reached a type comparison. A native export must be declined by con-leche and refused by nanoda
when only the standard axioms are permitted.

**What the claim does not cover:** that a Lean statement says what a Concrete function does (the
evidence pipeline's job: extraction, fingerprints, the identity and dependency gates); con-leche's
I/O driver around its verified core; the Lean runtime's `Nat` arithmetic, which con-leche uses for
literal reduction; the set-theoretic assumption itself; and, for the 35 native-trust theorems, the
six native facts.

**Updating the pins.** `scripts/tests/external_checker.pins` holds the three revisions; the CI cache
for the built tools is keyed on it. On a Lean toolchain bump: move `LEAN4EXPORT_REV` to the
lean4export tag for the new release (it is built with this repository's `lean-toolchain`, so its tag
must match the release that wrote the `.olean` files), and check the Lean Kernel Arena for the
con-leche and nanoda revisions it evaluated against that release — prefer those over newer commits,
since the Arena's soundness suite is the external evidence that a revision rejects what it should.
Then run the gate; the export metadata leg fails if the exporter and toolchain disagree. A new
`bv_decide` that mints a native axiom fails the owner leg until it is added to
`axiom_native_owners.txt` and named here.

## Fixture-only theorem names

Some `#[proof_by(...)]` attributes in test fixtures name theorems that do
not exist as Lean constants (`Concrete.Proof.compute_checksum_correct`,
`Nonexistent.Module.totally_fake_theorem`, `Examples.DoesNotExist.*`, and the
`not_yet_proved` / `drift_test_theorem` adversarial names). Such a claim is
not proved: a theorem that does not exist evaluates no specification, so the
subject binding refuses it (bug 079), and `concrete prove --check` names the
missing theorem. The gate keeps this list closed: a *new* unresolvable name is
a broken proof link and fails.

Until bug 079 the plumbing fixtures' theorems (`pure_add_correct` and siblings,
`PureAdd.add_comm`) were on this list too, and those claims read `proved`
because `proof-status` validated fingerprints, not names. They are now real
theorems about real specifications, in `proofs/Examples/TestFixtures/Proofs.lean`.

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
