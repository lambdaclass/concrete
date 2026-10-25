# Bug 060: `#[requires]`/`#[ensures]` are outside the proof fingerprint — a false postcondition stays `proved`

<!-- bug-ledger
summary: `#[ensures]` is outside the hash, so a FALSE postcondition still reports `proved` (R-0004, 2026-07-25)
coverage: contracts are inside the v2 subject, so attaching or changing an #[ensures] moves the digest and the claim no longer reports proved; LOAD-BEARING regression = check_proof_freshness.sh '060 CLOSED' leg (a false postcondition must not report proved) ((R-0004 V2 activation 2026-08-17))
class: 10
-->

**Status:** FIXED 2026-08-17 by the R-0004 V2 activation — third of R-0004's evidence-integrity defect class.

Contracts are inside the v2 subject (`CheckedDeclFacts` is captured in Elab BEFORE contract
erasure, precisely so they survive into the digest). Attaching or changing an `#[ensures]` moves the
subject, so the stored value no longer describes the claim and the function does not report
`proved`.

**What is and is not claimed.** The digest binds WHAT WAS CLAIMED, not whether the claim is true —
only the kernel decides that. So a true and a false postcondition both invalidate the stored
subject; the fix is that neither can ride an old proof, not that the digest can tell them apart.

**Regression that holds it closed:** `scripts/tests/check_proof_freshness.sh`, the leg labelled
`060 CLOSED`, which requires that a false `#[ensures]` does not report `proved`.
**Discovered:** 2026-07-25, filing R-0004's reproducers before implementation.
**Severity:** the highest of this class. The other two let a stale proof survive;
this one lets a function advertise a postcondition that is FALSE while the report
says the function is proved.

## Symptom

With a stored `#[proof_fingerprint]` and an untouched body, only the contract
changes — from a true postcondition to a false one:

```
#[ensures(result == 28)]   (true  — the loop sums 0..7)   -- proved
#[ensures(result == 999)]  (FALSE)                        -- proved
```

Nothing stales, nothing warns. A reader of `--report proof-status`, or any
downstream consumer of the claim, is told a function satisfying
`result == 999` has been proved.

## Root cause

The fingerprint is computed from the function BODY alone
(`bodyFingerprint body`, see bug 059). `#[requires]`, `#[ensures]` and
`#[invariant]` are attributes on the declaration, not statements in the body, so
they never reach the hash. Editing one therefore cannot change the fingerprint,
and the freshness check — the only thing standing between a linked theorem and a
`proved` verdict — sees no difference.

The selected spec and theorem identity are outside the hash for the same reason,
so swapping which theorem `#[proof_by]` names is equally invisible.

## Candidate fix

Include requires/ensures/invariants plus the normalized selected specification
and claim scope/coverage in R-0004's `ProofSubjectDigest`. A proof is a proof OF
a statement; changing the statement must invalidate the subject exactly as
changing the code does. Bind theorem identity and theorem-artifact digest in the
separate `ProofEvidenceReceipt`: changing the evidence requires replay against
the same subject, but does not redefine the subject itself.

Regression: the `28 -> 999` edit above stales; swapping the `#[proof_by]` theorem
invalidates the receipt and requires replay; swapping the `#[spec]` stales the
subject; reformatting a contract without changing its meaning does not. A
mutation that omits contracts from the digest must be killed.

## Executable witness (R-0004 slice 1)

`scripts/tests/check_proof_freshness.sh` adds `#[ensures(result == 999)]`
(FALSE — the loop sums 0..7 = 28) and, separately, `#[ensures(result == 28)]`
(TRUE), leaving body and types untouched. Measured: both report
`proved [invariant]`, and the gate asserts they are INDISTINGUISHABLE rather
than asserting a particular string, which is the actual defect.

**Tripwire**: it fails once the two differ, which is the signal to close this
document.
