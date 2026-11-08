# Fix round (C4 round 2; reviewer agy: CHANGES (alloc_failure omits four failure branches; decodePtr_valid_inv dead projection)). Fix on this branch.

HEAD is c8c76adef (axioms golden regenerated on concrete-ci-2 and committed). Do not edit scripts/proofs/axioms.golden. All rules of _review/BRIEF.md and _review/COMMON.md (if present) apply: commit each piece after `nix develop -c timeout 1200 lake build ConcreteSpec` passes; do not push; never --no-verify. Kimi's weekly quota is exhausted, so this round is written by a different model than the previous one; read the earlier ROUND files for context.

Read the review(s) in full: `_review/rev-c4a-agy.out.md`. Fix every finding marked REAL DEFECT (blocker, major, minor) with the structural fix the reviewer describes or a better long-term one; for a PREFERENCE, adopt it or say why not. Where reviewers disagree, decide from the statement itself and say why. Do not change any decided rule's meaning; if a fix needs a rule the decided rows do not answer, stop that part and list it under QUESTIONS with options and a recommendation.

Also apply the FINAL vacuity convention (_review/VACUITY_CONVENTION_FINAL.md) to every theorem of the files you touch: exactly one class tag per theorem; Instance = full closed application; Planted = the law's hypotheses with the subject replaced by a mutant at the instance's arguments, plus the negated conclusion; projections and rfl-laws are reclassified as Helper/Representation lemma (approved: PENDING) or turned into real laws; obligations are `def ... : Prop`.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch: no law that holds by construction or restates its definition, no weakened statement, no premise that switches off an obligation. If staging is truly unavoidable, record the long-term design and its roadmap row in your final message.

## Final message
Commits (SHA + one line), per finding what changed and its Instance/Planted witnesses, every exemption left PENDING, the build/rules/trust_ban results you ran, and QUESTIONS ("None" if none).

## Coordinator rules (binding)
Best long-term design, never cut corners. Do not run reviewers (no codex/kimi/agy review invocations of your own; review is done separately). Do not write git notes.
