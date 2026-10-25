# Bug 067 — a capped obligation escapes `forbid-assume` when more kernels attest

<!-- bug-ledger
summary: a capped obligation escapes `forbid-assume` when more kernels attest
coverage: a capped obligation escaped forbid-assume when Rocq/Isabelle are present: the cap is display-only, so an obligation resting on an UNSOUND HYPOTHESIS passes the release gate. Asserted by check_known_wrong_corpus.sh, which fails in CI (and only there — no external provers locally). Authority leak. FIX: the capped list keys on hypDebt (kernel-independent) instead of status, so promotion to proved_by_multi_kernel can no longer erase the cap. Gated by check_known_wrong_corpus.sh, which only fires in CI where Rocq/Isabelle exist; see docs/bugs/067 (2026-08-05)
class: 9
class: 11
-->

**Status:** FIXED 2026-08-05
**Found:** 2026-08-05, by CI on the multi-prover merge (`04fa2426`). Not reproducible
locally without Rocq/Isabelle installed, which is why local runs were green.
**Severity:** authority leak. An obligation resting on an UNSOUND HYPOTHESIS passes the
release gate.

## Witness

`examples/unsound_hypothesis/src/main.con`, with `forbid-assume = true`.

| environment | multi-kernel verdict | `concrete check` |
|---|---|---|
| no external provers (local) | `=> assumed` (rocq: off, isabelle: off) | **E0617 — rejected** |
| Rocq + Isabelle present (CI) | graduates out of `assumed` | **passes** |

`scripts/tests/check_known_wrong_corpus.sh` asserts the rejection and fails in CI:

    FAIL forbid-assume does NOT reject a capped obligation — the cap is display-only

## Why it matters

The cap exists because the obligation rests on a hypothesis known to be unsound. Adding
independent kernels does not make the hypothesis sound — each kernel closes ITS LOWERING of
the same unsound premise. So more attestation removes the cap that was protecting against
exactly this.

This contradicts the spike's own stated invariant in `ObligationCore.lean`: these classes
"never launder past a `trusted` boundary". Here the cap is lost, and the release gate that
should stop it does not fire.

It is also the concrete instance of the gap the spike documents: *"nothing verifying they
spell the SAME proposition — there is no subject-digest cross-check"*. Without that check,
N kernels accepting N lowerings is being treated as stronger evidence than one, when the
premise is unsound in all N.

## Fixed — the cap now dominates attestation

`Main.lean` computed the capped list as

    if v.status == "assumed" && !v.hypDebt.isEmpty then ...

so promotion to `proved_by_two_kernels` / `proved_by_multi_kernel` removed the VC from the
list and `forbid-assume` never saw it. Keyed on the DEBT instead:

    if !v.hypDebt.isEmpty then ...

`hypDebt` derives from the obligation's hypotheses and the function's loop contracts alone —
it does not move with kernel count — so the cap survives any number of attestations. That is
what `multiKernelVerdict`'s own comment already required: *"Kernel count never overrides
that — which is the H23 lesson expressed in the type."* The lesson was in the type; the gate
keyed on the status instead.

Verified: the H23 fixture is still rejected (E0617) locally, a clean project still passes
(no false positive), known-wrong-corpus 13/0, contract-negatives 33/0, examples 141/0, trust
gate 1501/0. The three-kernel path itself can only be exercised in CI, where the provers
exist.

## Resolution options (as diagnosed)

1. **Cap dominates attestation.** A capped obligation stays capped regardless of how many
   kernels attest; `forbid-assume` fires on the cap, not on the badge. Preferred — the cap
   is a statement about the PREMISE, which kernel count cannot affect.
2. **Revert the merge** until 1 is implemented, keeping R-0004 on main.

## Gate

`check_known_wrong_corpus.sh` already asserts the property and already fails. It must NOT be
weakened; it is doing exactly its job, and it caught this only because CI has the provers.
