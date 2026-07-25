# Bug 060: `#[requires]`/`#[ensures]` are outside the proof fingerprint — a false postcondition stays `proved`

**Status:** Open — third of R-0004's evidence-integrity defect class.
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

Include requires/ensures/invariants and the selected spec/theorem identity in
R-0004's `ProofSubjectDigest`. A proof is a proof OF a statement; changing the
statement must invalidate it exactly as changing the code does.

Regression: the `28 -> 999` edit above stales; swapping the `#[proof_by]` theorem
stales; swapping the `#[spec]` stales; reformatting a contract without changing
its meaning does not. A mutation that omits contracts from the digest must be
killed.
