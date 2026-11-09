# C2 round 6: review findings and approvals (Codex writes; the review is by agy)

State: the branch is at 5372ad2a0; seven gates pass. _review/APPR-C2-agy-5372ad2a0.md is an independent agy review of round 5 (9cb0ef7fe..5372ad2a0). It passes the official reviewer citation contract (checker 60934a193, status CHANGES; sha256 7f1c43ec786f8f696d10e42c34be09a0b2d73e6e1bd96a61f81c6e9f4d356830). Read it completely, with _review/VACUITY_CONVENTION_FINAL.md, _review/BRIEF.md and _review/COMMON.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Shortcuts are findings.

## Tasks
1. Fix finding 1 (major): ConstOrderIndependent needs a per-subject Planted for each of its two Instance cases, for both declared subjects. Each must be premise-preserving, with every definition that reaches the subject (ConstsEvaluate, ConstsAdmitted) mutated in hypotheses and conclusion. The instance-false-discharge gate false positive no longer hides this; the gate fix is in progress. Do not reshape the Instances around that gate defect.
2. Finding 2 (nit, preference) concerns redundant duplicate pairs. Apply it only if it is the better long-term structure (each Instance case must still have a Planted for every subject), and say why either way.
3. For every declaration the review APPROVES (48 rows), replace `(approved: PENDING)` with `(approved: APPR-C2-agy-5372ad2a0)`. Match by fully qualified name, and check that the cited tag line is that declaration's tag. Anything without an APPROVE row stays PENDING and is reported.
4. Run the N2 gate on Const and Scalar:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Const,ConcreteSpec.Proofs.Scalar'

## Rules
Do not change any owner-decided rule, the ratified vacuity convention, or the meaning of any normative definition in ConcreteSpec/Spec/. If a fix seems to need one, stop that item and explain it in QUESTIONS. Commit each piece after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/ or scratch files. Lean core only, no new axioms, no sorry. Keep tool output short. Final message: per item, done or left (with why), counts, commit SHAs, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
