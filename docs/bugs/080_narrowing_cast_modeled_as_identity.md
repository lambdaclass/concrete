# Bug 080 — a narrowing cast was the identity in the proof model, so a proof could be about a different function

<!-- bug-ledger
summary: `PExpr.cast` was the identity, so `x as u8` was 300 in the proof model and 44 in the binary; value-changing casts now extract as `wrapCast` (the runtime's wrap formula), affected proofs show their values fit, and `check_model_diff.sh` compares model and runtime on boundary inputs across the corpus
coverage: a narrowing cast was the identity in the proof model (x as u8 = 300 in the model, 44 in the binary); value-changing casts extract as wrapCast. Regression vehicle is a gate: check_model_diff.sh compares the model with the interpreter on boundary inputs across the corpus and pins tests/regressions/bug080_narrow_cast (300 -> 44 on both sides); see docs/bugs/080 (2026-10-18)
class: 7
silent: yes
-->

**Status:** FIXED (branch `bug080-cast-model`)
**Found:** 2026-10-18 by the R-0460 scoping review ("Bridge B"), confirmed by running it.
**Severity:** a false proof: the model a theorem is about computed a different value from the
compiled program. `fn narrow(x: u64) -> u8 { return x as u8; }` was proof-eligible ("pure, bounded,
no FFI"), extracted to `.cast (.var "x")`, and `.cast` evaluated as the identity — so `narrow(300)` was
300 in the model while the binary (and the interpreter) returned 44. A theorem stating the model's
value would have been accepted by the kernel and would have carried the claim.

## Mechanism

`PExpr.cast`'s doc comment said narrowing semantics were not modeled and that "the eligibility profile
is responsible for keeping narrowing casts out of proof-eligible code". Nothing did: eligibility
rejected only float casts. `cast_identity_preservation` stated the identity for every target type, and
PROOF_OBLIGATIONS_REGISTER's R-14 row recorded "narrowing is excluded by eligibility" as discharged.
Proved claims relied on it silently: the HMAC byte and length stores, base64 `char_of` and the numeric
`try_from` specifications all modeled a `u32 as u8` (or similar) as the identity — true on every
reached path only because the values happen to fit, which no theorem showed.

No interp-vs-model check existed: the differential fuzzer compares the interpreter with the compiled
binary, never with the model the proofs are about.

## Fixed

- **Model:** `PExpr.wrapCast width signed inner`, evaluated by `wrapInt` — the same two's-complement
  formula as `IntArith.wrapToType`, which the interpreter and codegen use. `.cast` is now reserved for
  value-preserving casts.
- **Extraction (`castToPExpr`):** a cast whose target range contains the source's is `.cast`; any other
  integer cast is `.wrapCast` at the target's width; a newtype wrap/unwrap (CoreCheck's one-step rule)
  is `.cast`; any other non-integer cast refuses extraction (six unclaimed std functions with `char`
  casts are now not extractable rather than modeled).
- **Specifications and proofs:** the affected specs model the wrap, and their theorems now PROVE the
  narrowed values fit (`wrapInt_unsigned_of_lt`, `wrapInt_bv_toNat`). Two theorems now state the
  unsigned-parameter fact they relied on (`0 ≤ v`). Every previously proved claim is proved again.
- **Kernel controls (ProofSoundness):** `narrowing_cast_wraps`, `widening_cast_is_identity`,
  `wrapCast_300_u8` (300 as u8 is 44 in the model). `cast_identity_preservation` is restated as
  `cast_preservation` (extraction is `castToPExpr`).

## Prevention

`--report model-diff` evaluates, in process, the extracted model (`Proof.eval`) and the interpreter on
boundary inputs of every scalar-signature extracted function. `check_model_diff.sh` requires zero
value mismatches across the corpus (140 functions, 27 claimed, 5666 inputs), re-derives every verdict
from its columns, and pins the fixture `tests/regressions/bug080_narrow_cast` (300 → 44 on both sides).
With the old identity model restored, it reports `narrow(300): model=300 runtime=44`. Family
`model-diff-compares-values` kills a comparator that always agrees.

## Limits

Sampling boundaries is evidence, not proof; the extraction-preservation theorems are R-0460's.
`partial` rows (346) are inputs where the model's unbounded integers give a value and the runtime
traps on checked overflow — a claim about such a function is partial correctness, still tracked by
R-0460. Non-scalar signatures (arrays, structs) are not yet compared.
