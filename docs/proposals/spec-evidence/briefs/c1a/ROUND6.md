# C1 round 6: two stale Law doc comments (Codex writes; agy wrote round 5)

State: the branch is at f0c7491db (84 contract-valid approvals recorded). _review/REV-C1-codex-f0c7491db.md is a contract-valid Codex review of round 5 (checker 60934a193, status CHANGES; sha256 28668ad64805da03d62b7e90871aa8f047f77ecf4cf8b820d6e5556924754012). Its two minor REAL DEFECTs:
- the Law doc comments at ConcreteSpec/Proofs/Scalar.lean:108 (bitNot_inRange) and :216 (saturating_of_checked) carry stale copied text and an exemption approval field.

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
Rewrite both Law doc comments so they accurately describe what each Law states, keep its `Law (subject …)` tag, and remove the approval field (Laws carry no exemption approval). Every backticked name must resolve. Change nothing else. Do not touch the held Step/AnswerStep determinism Laws.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: what changed, commit SHA, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself with `set -o pipefail`. Do not run reviewers; do not write git notes.
