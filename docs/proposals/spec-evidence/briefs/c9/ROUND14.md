# C9 round 14: record contract-valid approvals (Codex writes; the approval review is by agy)

State: the branch is at 115a73b2d, or at the axioms.golden commit the gates lane adds on top; use HEAD. _review/REV-C9-agy-115a73b2d.md is an independent agy review of rounds 12 and 13 (0551add93..115a73b2d). It passes the official reviewer citation contract (checker 60934a193, status ACCEPT; sha256 102908fc88c5a9235310045a0bb410f21cd02652ef5827a299b9a2fb0d93182d).

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
For every declaration the review APPROVES (31 rows), replace `(approved: PENDING)` with `(approved: REV-C9-agy-115a73b2d)`. Change nothing else.
- Match by fully qualified name, and check that the cited tag line is that declaration's tag.
- Report any `(approved: PENDING)` left in the C9 files.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: the approvals recorded, the PENDING left (with why), the commit SHA, and QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
