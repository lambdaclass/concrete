# C7 round 7: act on the contract-valid approval review (Codex writes; the review is by agy)

State: the branch is at 5d94c4b4d; seven gates pass. _review/APPR-C7-agy-5d94c4b4d.md is an independent agy review of this exact head. It passes the official reviewer citation contract (checker 60934a193, status CHANGES; sha256 2db50f7b94ac303b96b523bba2fcc3d33fd842313cd8ff74d63d6e7b86ba916c). Read it completely, with _review/VACUITY_CONVENTION_FINAL.md, _review/BRIEF.md and _review/COMMON.md.

The 63 approvals in source, `(approved: REVIEW5-codex.md)`, rest on a review that fails that contract, so none of them may stand.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Shortcuts are findings.

## Tasks
1. Fix the blocker. The evidence for ConcreteSpec.Core.Leak.Demo.secure_ni, secure_ct and timing_ni (Proofs/Leak.lean:401-465, 661-676, 1644-1690) must restate the conclusions as the non-interference statements the review names, with closed full-application Instances and premise-preserving Planted mutants. The review proposes DelimitedReleaseNI for secure_ni.
2. The major finding concerns Bound.Sound and Bound.SoundOptional (ConcreteSpec/Spec/Leak.lean:373-379).
   - If it can be fixed inside the proofs (laws, evidence, classification) without changing what these normative definitions mean, fix it.
   - If the fix requires changing the meaning of Bound.Sound or Bound.SoundOptional, or any other normative definition, STOP that item. Explain the needed change, with file:line, in QUESTIONS; it goes to the owner.
3. For every declaration the review APPROVES: set its reference to `(approved: APPR-C7-agy-5d94c4b4d)`. Match by fully qualified name, and check that the cited tag line is that declaration's tag. Any declaration whose classification changes in this round, or that has no approving row, becomes `(approved: PENDING)`. No old REVIEW5-codex.md reference may remain.
4. Check every Law of C7 (ConcreteSpec/Proofs/Leak.lean) against the convention, not only the changed ones. Then run the N2 gate:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Leak'
  It must report no FAIL except: PENDING approvals; the instance-false-discharge false positive on `by decide`/simp premises (a confirmed gate defect; do not reshape around it) with its cascade; and subject-kind for inductive Step/Steps/AnswerStep subjects (an owner question). Do not edit anything under that gate directory.

## Rules
Do not change any owner-decided rule, the ratified vacuity convention, or the meaning of any normative definition in ConcreteSpec/Spec/. If a fix seems to need one, stop that item and explain it in QUESTIONS. Commit each piece after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/ or scratch files. Lean core only, no new axioms, no sorry. Keep tool output short. Final message: per item, done or left (with why), counts, commit SHAs, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
