# C6 round 6: record contract-valid approvals (Codex writes; the approval review is by agy)

State: the branch is at 6477c65a7; seven gates pass. _review/APPR-C6-agy-6477c65a7.md is an independent agy review of this exact head. It passes the official reviewer citation contract (checker 60934a193, status ACCEPT; sha256 fc785d93d8e5ddf2b131faeb497884df90689f1314e9a53717a6e00f1f35ed13). It gives verdicts on the 69 Helper and Representation lemma declarations of C6.

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
For every declaration the review APPROVES, replace `(approved: PENDING)` with `(approved: APPR-C6-agy-6477c65a7)`. Change nothing else.
- Match by the fully qualified name in the review row, and check that the row's cited tag line is that declaration's tag.
- A declaration whose row does not say APPROVE, or that has no row, stays PENDING and is reported.
- This includes the Observe2 helpers stripPrefix_append and decodeTrapLine_render.
- REVIEW4 finding 2 (source-path disposition, duplicate site keys) is still an owner question: do not touch it.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: approvals recorded, PENDING left (with why), commit SHA, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself with `set -o pipefail`. Do not run reviewers; do not write git notes.
