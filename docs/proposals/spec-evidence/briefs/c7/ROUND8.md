# C7 round 8: record contract-valid approvals (Codex writes; the approval review is by agy)

State: the branch is at f2e93a7e0 plus its regenerated golden (round 7; gates running). _review/APPR-C7-agy-f2e93a7e0.md is an independent contract-valid agy review of round 7 (5d94c4b4d..f2e93a7e0). It passes the official reviewer citation contract (checker 60934a193, status ACCEPT; sha256 536790f35172c596d9e8559a002825f515c13af80ae795d13321d1e84dd937d4). It approves the 12 declarations that round 7 left `(approved: PENDING)` in ConcreteSpec/Proofs/Leak.lean; the 65 earlier approvals already cite APPR-C7-agy-5d94c4b4d and stay.

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
For every declaration the review APPROVES, replace `(approved: PENDING)` with `(approved: APPR-C7-agy-f2e93a7e0)`. Change nothing else.
- Match by the fully qualified name in the review row, and check that the row's cited tag line is that declaration's tag.
- A declaration whose row does not say APPROVE, or that has no row, becomes `(approved: PENDING)` and is reported. Leave the existing APPR-C7-agy-5d94c4b4d references unchanged; no reference other than these two APPR-C7 reviews may remain.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: approvals recorded, PENDING left (with why), commit SHA, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself with `set -o pipefail`. Do not run reviewers; do not write git notes.
