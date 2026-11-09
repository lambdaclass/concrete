# C9 round 11: record contract-valid approvals (Codex writes; the approval review is by agy)

State: the branch is at 3e22e6417; seven gates pass. _review/APPR-C9-agy-3e22e6417.md is an independent agy review of this exact head. It passes the official reviewer citation contract (checker 60934a193, status ACCEPT; sha256 0a045077bc4c59cb06cda8fd2a89083bd747d0e25f641af6adb24cef76bbf91e). It gives verdicts on the 42 Helper and Representation lemma declarations of C9 (Ident.lean, Prim.lean, the C9 part of Observe2.lean). All 42 currently carry approval references (REVIEW9-codex, REVIEW7-codex, REVIEW5-codex, REVIEW8-agy) that rest on reviews failing the contract; every one of those references must go.

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
For every declaration the review APPROVES, replace the existing approval reference (or PENDING) with `(approved: APPR-C9-agy-3e22e6417)`. Change nothing else.
- Match by the fully qualified name in the review row, and check that the row's cited tag line is that declaration's tag.
- A declaration whose row does not say APPROVE, or that has no row, becomes `(approved: PENDING)` and is reported. No old reference may remain anywhere in the C9 files.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: approvals recorded, PENDING left (with why), commit SHA, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself with `set -o pipefail`. Do not run reviewers; do not write git notes.
