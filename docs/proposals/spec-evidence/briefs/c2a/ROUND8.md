# C2 round 8: record contract-valid approvals (Codex writes; the approval review is by agy)

State: the branch is at 59d4c501d. _review/REV-C2-agy-59d4c501d.md is an independent agy review of round 7 (e19a4248c..59d4c501d). It passes the official reviewer citation contract (checker 60934a193, status ACCEPT; sha256 6a7ffb5cb22cdbe26aa29dd85afe02f387b1c7f1cce18c1e9e09d3ab6a6e30ec).

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
For every declaration the review APPROVES (6 rows), replace `(approved: PENDING)` with `(approved: REV-C2-agy-59d4c501d)`. Change nothing else.
- Match by fully qualified name, and check that the cited tag line is that declaration's tag.
- Report any `(approved: PENDING)` left in the C2 files.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: the approvals recorded, the PENDING left (with why), the commit SHA, and QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
