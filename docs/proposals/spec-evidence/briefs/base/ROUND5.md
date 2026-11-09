# Base migration round 5: record contract-valid approvals (Codex writes; the approval review is by agy)

State: the branch is at 942d74b39, or at the axioms.golden commit the gates lane adds on top; use HEAD. _review/REV-base4-agy-942d74b39.md is an independent agy review of round 4 (980e80550..942d74b39). It passes the official reviewer citation contract (checker 60934a193, status ACCEPT; sha256 aead6bb35ee1a1609e3a0eb0938475b540a7bd9af03da57fa197a2d713088db4).

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
For every declaration the review APPROVES (7 rows), replace `(approved: PENDING)` with `(approved: REV-base4-agy-942d74b39)`. Change nothing else.
- Match by fully qualified name, and check that the cited tag line is that declaration's tag.
- Report any `(approved: PENDING)` left in the eleven base modules.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: the approvals recorded, the PENDING left (with why), the commit SHA, and QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
