# C5 round 9: act on the contract-valid approval review (agy writes; the review is by Codex)

State: the branch is at 902ce88a; seven gates pass. _review/APPR-C5-codex-902ce88a.md is an independent Codex review of this exact head. It passes the official reviewer citation contract (checker 60934a193, status CHANGES; sha256 bf5b244a8b6d444b2e1373d5ba7c757f805e10fc5690d359297af3e7cc5acb27). Read it completely, with _review/VACUITY_CONVENTION_FINAL.md, _review/BRIEF.md and _review/COMMON.md.

The 196 approvals now in source, `(approved: REVIEW7-agy)`, rest on a review that fails that contract, so none of them may stand.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Shortcuts are findings.

## Tasks
1. For the 132 declarations the review APPROVES: replace their approval reference with `(approved: APPR-C5-codex-902ce88a)`. Match by the fully qualified name in the review row, and check that the row's cited tag line is that declaration's tag.
2. For the 64 declarations the review REJECTS (finding F1: preservation, lifetime safety, progress and simulation contracts labelled Helper): promote each to a Law under the final convention.
   - Name its semantic subject definitions.
   - Give a closed full-application Instance and, for every subject and case, a same-typed premise-preserving Planted. Its type is the hypotheses with the subject replaced by the mutant (also inside definitions reaching it) at the Instance's exact arguments, ∧ ¬ the mutated conclusion.
   - Where a rejected declaration is only the internal step of a public law that states the same contract, choose the best long-term structure (one Law, not two copies of the same statement) and say why.
3. Every approval reference other than the new one (for example any remaining REVIEW7-agy) becomes `(approved: PENDING)`. No declaration may keep an approval that the new review did not give.
4. Check every Law of C5 (ConcreteSpec/Proofs/Grant.lean, ConcreteSpec/Proofs/Own.lean) against the convention, not only the changed ones. Then run the N2 gate:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Grant,ConcreteSpec.Proofs.Own'
  It must report no FAIL except: PENDING approvals; the instance-false-discharge false positive on `by decide`/simp premises (a confirmed gate defect; do not reshape around it) with its cascade; and subject-kind for inductive Step/Steps/AnswerStep subjects (an owner question; leave those as they are). Do not edit anything under that gate directory.

## Rules
Do not change any owner-decided rule, the ratified vacuity convention, or the meaning of any normative definition in ConcreteSpec/Spec/. If a fix seems to need one, stop that item and explain it in QUESTIONS. Commit each piece after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/ or scratch files. Lean core only, no new axioms, no sorry. Keep tool output short. Final message: per item, done or left (with why), counts, commit SHAs, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
