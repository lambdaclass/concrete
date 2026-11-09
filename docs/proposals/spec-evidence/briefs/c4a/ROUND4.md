# C4 round 4: record contract-valid approvals (agy writes; Codex wrote round 3)

State: the branch is at 78ee5a2db; seven gates pass. _review/APPR-C4-agy-78ee5a2db.md is an independent agy review of this exact head. It passes the official reviewer citation contract (checker 60934a193, status ACCEPT; sha256 b73e52c99d49693d699960967a40c1f24d4a9a93bbb489cb016dc6f0e0a74095). Its table gives APPROVE on all 55 `(approved: PENDING)` Helper and Representation lemma declarations in ConcreteSpec/Proofs/Memory.lean, ConcreteSpec/Proofs/Hybrid.lean and ConcreteSpec/Spec/Hybrid.lean.

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
For every declaration that the review approves, replace `(approved: PENDING)` in its doc comment with `(approved: APPR-C4-agy-78ee5a2db)`. Change nothing else in that comment, and nothing else anywhere.
- Match by the fully qualified name in the review row, not by line number alone. Check that the row's cited tag line is that declaration's tag.
- If a row does not match a declaration, or a PENDING declaration in those files has no row, leave it PENDING and report it.
- Never approve anything the review did not approve.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: the number of approvals recorded, any unmatched rows or PENDING left (with why), commit SHA, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself with `set -o pipefail`. Do not run reviewers; do not write git notes.
