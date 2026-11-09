# Integration head, merge round 1 (Codex writes; agy reviews)

Goal: ONE integration head that merges every accepted spec head. The coordinator will send it for an unscoped full audit. This branch, wip/spec-integration-head, starts at db4eda38f, the accepted integration branch: Adequacy on top of the spec base 0dbeba29e. Its remotes are fetched.

## Best long-term design (owner rule, binding)
Resolve every conflict by the best long-term design, never by the quickest edit. A merge must not weaken, drop or silently rewrite any law, definition, tag or approval of either side. Shortcuts are findings.

## Merge, in this order, one merge commit each (`git merge --no-ff <sha>`; never rebase, squash or rewrite history)
1. origin/wip/spec-c1-audit at f18c2d546 (C1: Exec, Scalar)
2. origin/wip/spec-c2-audit at 67cb19abd (C2: Const, Scalar)
3. origin/wip/spec-c4-audit at f6f112462 (C4: Memory)
4. origin/wip/spec-c5 at 56e58d592 (C5: Grant, Own)
5. origin/wip/spec-c7 at fb8f565c2 (C7)
6. origin/wip/spec-c10 at de0e8bfd2 (C10: Frame)
7. origin/wip/spec-c9 at 0551add93 (C9: Ident, Prim, Observe2). Use this exact SHA, not the branch tip; newer C9 rounds are still under review.
8. origin/wip/spec-c6-audit at 5120f5210 (C6: Observe*)

Do not merge wip/spec-base-migration in this round; it is still under review and comes in a later round.

## Conflict rules
- **Ownership.** Each contract owns its own modules: C1 owns Exec and the Scalar tagging; C2 owns Const and its Scalar laws (checkedNeg); C9 owns Ident, Prim and the C9 part of Observe2; C6 owns Observe and its proof modules.
- **Scalar.** In Proofs/Scalar.lean, C1 and C2 both changed tags and laws. Keep both sides' substance: C2's checkedNeg laws and its removal of checkedNeg_inRange, and C1's tagging and approvals. Where both changed the same declaration, keep the stronger statement with its evidence, and mark its approval `(approved: PENDING)` if the merged text differs from what either approval covered. List every such declaration.
- **Spec/Exec.lean.** C1, C6 and C9 all changed it. C1's Rules relation is the newest normative addition. Keep every side's additions, and report any semantic overlap.
- **Spec/Observe.lean and Proofs/Observe2.lean.** C6 and C9 changed both. Keep both, and report any overlap where the two disagree (for example SiteTable.Valid's signature). Pick the long-term structure only when one side clearly subsumes the other; otherwise leave the C6 form, document the overlap in your final message, and do not invent a third design.
- **Spec.lean, ConcreteSpec.lean and README.md.** Union of the imports and sections, de-duplicated, in a sensible order.
- **scripts/proofs/axioms.golden.** Do not hand-edit it. In any conflict, keep this branch's side. The coordinator regenerates it on the CI server after the merge.
- **ISSUES.tsv.** Take both sides' rows; there must be no duplicate IDs.

## Checks
After each merge commit, build (`bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1800 lake build ConcreteSpec`). If the build fails, fix it in a follow-up commit before the next merge; never leave a broken merge for later. Gate commits on the exit status of the check itself, with `set -o pipefail`. After the last merge, run `nix develop -c timeout 600 bash scripts/spec/rules.sh` and `nix develop -c timeout 900 bash scripts/proofs/trust_ban.sh`.

## Rules
- Do not change any owner-decided rule, the ratified vacuity convention, or the meaning of any normative definition. If a conflict seems to need that, stop and put it in QUESTIONS with the options.
- Do not push.
- Do not edit docs/decisions.md or ROADMAP.md.
- Never commit _review/.
- Do not run reviewers, and do not write git notes.
- Final message: each merge commit SHA with its conflicts and how you resolved each; every declaration whose approval you reset; the overlaps reported; the build, rules and trust results; QUESTIONS.
