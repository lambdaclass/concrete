# C1 round 8: record contract-valid approvals (Codex writes; the approval review is by agy)

State: the branch is at 7031e101c (round 7 plus its regenerated axioms golden), and all seven gates pass. _review/REV-C1-agy-f6f3683ff.md is an independent agy review of eb046cec7..f6f3683ff. It passes the official reviewer citation contract (checker 60934a193, status ACCEPT; sha256 286d20a715a59f9ad1e69cb66f0da4dafb5af80e645d25f52b280ee2d2022e45).

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
For every declaration the review APPROVES (20 rows), replace `(approved: PENDING)` with `(approved: REV-C1-agy-f6f3683ff)`. Change nothing else.
- Match by fully qualified name, and check that the cited tag line is that declaration's tag.
- Report any `(approved: PENDING)` left in the C1 files.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: the approvals recorded, the PENDING left (with why), the commit SHA, and QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
