# C10 round 12: record contract-valid approvals (Codex writes; the approval review is by agy)

State: the branch is at 367764f3b; use HEAD. _review/REV-C10-agy-367764f3b.md is an independent agy review of round 11 (de0e8bfd2..367764f3b, including the merge of C9 115a73b2d). It passes the official reviewer citation contract (checker 60934a193, status ACCEPT; sha256 a51e7e8323da866bfaf9daea81755a38346cf88e7d5f602f070d74587b1dd8d4). Every location and identifier it cites resolves mechanically in the tree.

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
For each of the nine ConcreteSpec/Proofs/Frame.lean declarations the review APPROVES (section "ConcreteSpec/Proofs/Frame.lean (9 of 9 approved)"), replace `(approved: PENDING)` with `(approved: REV-C10-agy-367764f3b)`. Change nothing else.
- Match by fully qualified name, and check that the cited tag line is that declaration's tag.
- Do NOT touch the Ident.lean or Observe2.lean rows. Those files belong to C9, which records the same approvals on its own branch; C10 takes them by a later merge of C9.
- Report any `(approved: PENDING)` left in the C10-owned files (Spec/Frame.lean, Proofs/Frame.lean and other C10 files), with why.

## Rules
Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: the approvals recorded, the PENDING left (with why), the commit SHA, and QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
