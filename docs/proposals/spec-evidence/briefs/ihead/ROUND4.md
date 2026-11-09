# Integration head, round 4: record the round-3 approvals (Codex writes; agy reviews)

State: the branch is at f68840b0e; use HEAD. _review/REV-integ-agy-f68840b0e.md is an independent agy review of round 3. It passes the official reviewer citation contract (checker 60934a193, status ACCEPT; sha256 541a855c4ef58f24f33547551071422d04ac58bd2ca2dc98cc782ef73d1f2a7b), every citation resolves, and it has no findings.

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
The review APPROVES four declarations that are marked `(approved: PENDING)`:
- ConcreteSpec.Core.Exec.Demo.observationEnv
- ConcreteSpec.Core.Exec.Demo.erasing_internal_infinite
- ConcreteSpec.Core.Exec.Demo.bumpedMove
- ConcreteSpec.Core.Leak.no_step_of_none
For each, replace the tag with `(approved: REV-integ-agy-f68840b0e)`. Match by fully qualified name, and check that the tag line is that declaration's tag. Change nothing else. Report any `(approved: PENDING)` left anywhere in ConcreteSpec/, with its owning contract.

## Rules
Commit after the build, rules.sh and trust_ban.sh pass. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: the approvals recorded, the PENDING left, the commit SHA, and QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1800 lake build ConcreteSpec`. Run `nix develop -c timeout 600 bash scripts/spec/rules.sh` and `nix develop -c timeout 900 bash scripts/proofs/trust_ban.sh`. Gate the commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
