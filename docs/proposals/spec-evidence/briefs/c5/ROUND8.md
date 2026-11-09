# c5: record reviewer approvals (ROUND8; agy writes; Codex wrote the previous round)

State: head 02074d4f; all 7 gates pass on concrete-ci-2. The previous round was reviewed by agy in _review/REVIEW7-agy.md (VERDICT: ACCEPT, mechanically verified: every citation resolves in the tree). That review gives an explicit APPROVE verdict on every Helper / Representation lemma still marked `(approved: PENDING)` (196 declarations).

## Best long-term design (owner rule, binding)
Do the best long-term thing and never cut corners; shortcuts are findings. Do not run reviewers, do not write git notes, do not push.

## Task (mechanical, exact)
1. For EACH declaration whose tag says `(approved: PENDING)`, find its verdict in _review/REVIEW7-agy.md by its exact declaration name. If the verdict is APPROVE, replace `(approved: PENDING)` with `(approved: REVIEW7-agy)`, following the existing approval format exactly (for example `(approved: REVIEW7-codex)`). Change nothing else in the tag or the declaration.
2. If a declaration has no verdict in that file, or the verdict is not an unconditional APPROVE, leave it PENDING and list it in your final message. Never write an approval the file does not give.
3. Change no statement, proof, definition or other tag. This round is provenance only.
4. Build through the semaphore: `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Commit gated on that command's own exit status with set -o pipefail, with message "spec c5: record REVIEW7-agy approvals", and only after confirming with `git diff --stat` that only tag comment lines changed. Never commit _review/.
5. Final message: the commit SHA, the count of approvals recorded, any left PENDING with reasons, and QUESTIONS (none expected).
