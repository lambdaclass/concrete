# Bug 284 — the evidence producer dropped every arm of a non-enum match, so subject and implementation identity ignored arm bodies

<!-- bug-ledger
summary: Elab built each match arm's Core and its evidence in two accumulators and pushed evidence only for the enum branch's variant, literal and variable arms; a match on an Int, Bool, Char or String, a match whose enum lookup failed, and every range arm became `matchExpr scrutinee []` with no gap, so the V2 subject digest, the implementation digest and `DefinitionIdentity` ignored every arm body and a v2 pin read current after an arm edit
coverage: reproducer tests/regressions/bug284_match_arm_evidence_dropped/repro.con, pinned by check_binder_refs.sh legs "a literal arm's body references reach evidence", "a body edit inside a literal arm changes the body bytes", "bug 284: an edit inside a literal arm moves the subject digest" and "bug 284: each match arm's Core and evidence are pushed as one entry"
class: 10
silent: yes
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/bug284-match-arm-evidence`)
**Found:** 2026-10-26, checking an earlier unconfirmed observation that binder references inside
literal match arms were missing from the R-0004 evidence producer, on main `cc7a4084`.
**Severity:** unsound freshness and identity. A proof link's stored `#[proof_fingerprint("v2:...")]`
read `current` after any edit inside the arms of a non-enum match, and the function's
`DefinitionIdentity` did not move, so a table attestation keyed by it (`scopedEvidenceContains`)
still matched a callee whose arm bodies had changed. The function's own claim was still contained
by the V1 subject binding (`subjectBindingOf` compares the extracted body, which keeps literal
arms, with the theorem's specification), so no shipped claim was reported `proved` wrongly; no
shipped proof covers a literal-pattern match. A caller whose body edge to such a callee is
justified by a table attestation was not contained when the callee stayed independently proved.

Confirmed real by Codex (read-only review of cc7a4084 with the reproducer outputs, 2026-10-26):
"VERDICT: CONFIRMED — omitted match-arm evidence falsely validates as complete and feeds live
freshness and identity authorities, although independent V1 binding and dependency checks can
contain particular proof claims." Codex also found that the fix decides no owner decision
(OD18 arm order and OD19 V1 admission of guards and ranges are untouched).

## Reproducer

`tests/regressions/bug284_match_arm_evidence_dropped/repro.con`: `pick` matches an `Int` on literal
arms that read the parameter `p` and call `helper`.

## Observed

On cc7a4084, `--report subject-facts` for `main.pick` printed subject digest
`5b6b5b8255e1e55a1a2a9ac3d7413ea7` and shadow bodyV2 `651a82af09b720f9b1620bd34c8693df` both for
the fixture and for a copy with `0 => p * 9` and `1 => other(k)`, and `shadow edges: none (body
reaches no callable)` although the body calls `helper`. The bodyV2 digest was also the same for
`fn rng` (a range arm) and `fn flag` (a Bool match): only `let r = match <param 0> {}; return r`
reached evidence. With a spec, a proof link and the fixture's v2 pin, `--report
migration-manifest` read `current` for the edited copy.

## Expected

Every arm of every match reaches evidence (pattern, guard, body), or the body carries a gap and
is refused: a body digest that describes less than the program must not validate as complete
(the R-0004 rule in `Concrete/Proof/IdentityUseBytes.lean`, "REFUSES an uncovered input").

## Cause

`Concrete/Elab/Elab.lean`, the `.match_` case of `elabExprEv`: `cArms` (Core) and `armEvsRev`
(evidence) were separate lists. Of the twelve arm cases across the three branches (enum found,
enum lookup failed, scrutinee not an enum), three pushed evidence; the other nine pushed Core only.
`EvidenceExprV2.matchExpr scrutinee []` holds no gap, so `Proof.validate` accepted the body, and
`proofSubjectDigestV2` and `implementationDigest` digested it.

## Fix

Each arm pushes one `(CMatchArm × EvidenceArmV2)` entry, so no arm can reach Core without
evidence. Literal arms emit their literal pattern, variable arms a binder or wildcard, range arms
`.range lo hi inclusive`, and a variant arm over a scrutinee that is not a resolved enum a gap.
`evLitPattern` also describes a negative integer arm (`-1 =>`, which elaborates as negation of a
literal) as the integer pattern of its value instead of a gap. Subject digests of functions with
such matches move once; that is the change in what they describe, not drift.

## Structural cause

Class 10, evidence identity computed from a partial subject: the evidence was assembled beside
Core by hand in every branch, and nothing compared the two, so an omitted node made the subject
smaller without making it incomplete.

## Structural owner

R-0521 (one canonical `VerificationTask` bound to the exact checked artifact) and R-0497
(payload-bound artifacts) for the class. Here, the per-arm pairing makes the omission
unrepresentable in the `match` case; the remaining hand-paired constructs are R-0004's producer
seam (`ElaboratedExprV2`) until evidence is derived from Core by one constructor.

## Containment while open

None needed: fixed on the branch above.
