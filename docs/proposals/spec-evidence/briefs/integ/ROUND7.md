# Integration (wip/spec-core) round 7: record contract-valid approvals (Codex writes; the approval review is by agy)

State: the branch is at 0dbeba29e; seven gates pass. _review/APPR-integ-agy-0dbeba29e.md is an independent agy review of this exact head. It passes the official reviewer citation contract (checker 60934a193, status ACCEPT; sha256 090240e7a54abd7f5b8b584240a340fe55d1d14d8c0950fbed82956cb092889e). It approves all 96 Helper and Representation lemma declarations in ConcreteSpec/Proofs/Adequacy.lean (93 Helper, 3 Representation lemma).

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
For every declaration the review APPROVES, replace `(approved: PENDING)` with `(approved: APPR-integ-agy-0dbeba29e)`. Change nothing else.
- Match by the fully qualified name in the review row, and check that the row's cited tag line is that declaration's tag.
- A declaration whose row does not say APPROVE, or that has no row, stays PENDING and is reported.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: approvals recorded, PENDING left (with why), commit SHA, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself with `set -o pipefail`. Do not run reviewers; do not write git notes.
