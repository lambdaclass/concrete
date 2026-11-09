# C10 round 10: record contract-valid approvals (Codex writes; the approval review is by agy)

State: the branch is at 17aa93314; seven gates pass. _review/APPR-C10-agy-17aa93314.md is an independent agy review of this exact head. It passes the official reviewer citation contract (checker 60934a193, status ACCEPT; sha256 c84eba0120803de799e88d18e6a6b075a9a94f6d9008e69cd1cd647190ebdcfe). It gives verdicts on the 53 Helper and Representation lemma declarations of C10 (Frame.lean). All 53 currently carry approval references (_review/REVIEW5-agy.md, _review/REVIEW6-codex.md, _review/REVIEW7-agy.md, alone or combined) that rest on reviews failing the contract; every one of those references must go.

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
For every declaration the review APPROVES, replace the existing approval reference (or PENDING) with `(approved: APPR-C10-agy-17aa93314)`. Change nothing else.
- Match by the fully qualified name in the review row, and check that the row's cited tag line is that declaration's tag.
- A declaration whose row does not say APPROVE, or that has no row, becomes `(approved: PENDING)` and is reported. No old reference may remain anywhere in the C10 files.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: approvals recorded, PENDING left (with why), commit SHA, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself with `set -o pipefail`. Do not run reviewers; do not write git notes.
