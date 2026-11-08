# Fix round (A2i integration round 4, C8 over C6; reviewers Codex and agy: CHANGES). Fix on this branch.

HEAD is fcb18d674 (axioms golden regenerated on concrete-ci-2 and committed). Do not edit scripts/proofs/axioms.golden. All rules of _review/BRIEF.md and _review/COMMON.md (if present) apply: commit each piece after `nix develop -c timeout 1200 lake build ConcreteSpec` passes; do not push; never --no-verify. Kimi's weekly quota is exhausted, so this round is written by a different model than the previous one; read the earlier ROUND files for context.

Read the review(s) in full: `_review/rev-intg3-codex.final.md` `_review/rev-intg3-agy.out.md`. Fix every finding marked REAL DEFECT (blocker, major, minor) with the structural fix the reviewer describes or a better long-term one; for a PREFERENCE, adopt it or say why not. Where reviewers disagree, decide from the statement itself and say why. Do not change any decided rule's meaning; if a fix needs a rule the decided rows do not answer, stop that part and list it under QUESTIONS with options and a recommendation.

Also apply the FINAL vacuity convention (_review/VACUITY_CONVENTION_FINAL.md) to every theorem of the files you touch: exactly one class tag per theorem; Instance = full closed application; Planted = the law's hypotheses with the subject replaced by a mutant at the instance's arguments, plus the negated conclusion; projections and rfl-laws are reclassified as Helper/Representation lemma (approved: PENDING) or turned into real laws; obligations are `def ... : Prop`.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch: no law that holds by construction or restates its definition, no weakened statement, no premise that switches off an obligation. If staging is truly unavoidable, record the long-term design and its roadmap row in your final message.

## Final message
Commits (SHA + one line), per finding what changed and its Instance/Planted witnesses, every exemption left PENDING, the build/rules/trust_ban results you ran, and QUESTIONS ("None" if none).
