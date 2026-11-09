# C4 round 5: two partial-application Instances (Codex writes; agy wrote round 4)

State: the branch is at 314a4f068 (55 contract-valid approvals recorded). _review/REV-C4-codex-314a4f068.md is a contract-valid Codex review (checker 60934a193, status CHANGES; sha256 267410a8874e805deb3e0e84d628a68069c50d0b207fd5eb602aaab3fc861f0c). Read it completely, with _review/VACUITY_CONVENTION_FINAL.md, _review/BRIEF.md and _review/COMMON.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Shortcuts are findings.

## Tasks
1. ConcreteSpec.Core.Mem.access_valid (Instance and Planted at Proofs/Memory.lean:1514-1531): make the Instance a full closed application at an exact closed pointer, and the Planted the mutated hypotheses ∧ ¬ the mutated conclusion at those arguments. Keep the successful-access coverage.
2. ConcreteSpec.Core.Mem.fromExposed_sound (Proofs/Memory.lean:1771-1785): apply the review's long-term fix or a better one. Reformulate the Law so that a full closed application exists, for example as a match on the reconstruction result with the block properties in its successful branch. Give it a full closed Instance and an exact mutated Planted. If the old inversion form is still needed, it becomes a Helper with `(approved: PENDING)`.
3. Sweep every C4 Instance for the same shape: a universally quantified statement left after the application.
4. Any declaration whose classification changes, and any new Helper, carries `(approved: PENDING)`. Never write an approval.
5. Run the N2 gate:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Memory,ConcreteSpec.Proofs.Hybrid'

## Rules
Do not change any owner-decided rule, the ratified vacuity convention, or the meaning of any normative definition in ConcreteSpec/Spec/. If a fix seems to need one, stop that item and explain it in QUESTIONS. Commit each piece after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/ or scratch files. Lean core only, no new axioms, no sorry. Keep tool output short. Final message: per item, done or left (with why), counts, commit SHAs, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
