# C1 round 5: record contract-valid approvals (agy writes; the approval review is by Codex)

State: the branch is at 5531d9e34 (round 4). _review/APPR-C1-codex-5531d9e34.md is an independent Codex review of round 4 (ccca4abaa..5531d9e34). It passes the official reviewer citation contract (checker 60934a193, status CHANGES; sha256 d6121cfde998536fd30c403e0094cb5d5590a5f67fb4e525c29e92df4a9a8218). It APPROVES all 84 `(approved: PENDING)` Helper and Representation lemma declarations: 63 in ConcreteSpec/Proofs/Exec.lean and 21 in ConcreteSpec/Proofs/Scalar.lean.

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
For every declaration the review APPROVES, replace `(approved: PENDING)` with `(approved: APPR-C1-codex-5531d9e34)`. Change nothing else.
- Match by the fully qualified name in the review row, and check that the row's cited tag line is that declaration's tag.
- A declaration without an APPROVE row stays PENDING and is reported.

The review's finding 1, on the determinism Laws at Exec.lean:1492-1521, concerns the Step/AnswerStep Laws, which are held for an owner decision. Do NOT change them, their tags or any Step representation in this round.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: approvals recorded, PENDING left (with why), commit SHA, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself with `set -o pipefail`. Do not run reviewers; do not write git notes.
