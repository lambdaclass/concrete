# C4 round 6: record one contract-valid approval (Codex writes; the approval review is by agy)

State: the branch is at 9d78b23cc, and all seven gates pass. _review/APPR-C4-agy-951060caa.md is an independent agy review of 314a4f068..951060caa. It passes the official reviewer citation contract (checker 60934a193, status ACCEPT; sha256 803261c9f41c3bda682fdf53a49a0e9c9ae5a46f68581b6de94baf95f3b3cba4).

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
The review APPROVES `ConcreteSpec.Core.Mem.fromExposed_sound_inv`. On that declaration's tag, replace `(approved: PENDING)` with `(approved: APPR-C4-agy-951060caa)`. Check first that the tag sits on that declaration. Change nothing else. Report any other `(approved: PENDING)` left in the C4 files.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: the approval recorded, the PENDING left, the commit SHA, and QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
