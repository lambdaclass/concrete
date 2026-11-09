# Integration head, round 3: fix the round-2 review findings and record its approvals (Codex writes; agy reviews)

State: the branch is at 3b54f736d (round 2); use HEAD. _review/REV-integ-agy-3b54f736d.md is an independent agy review of round 2's manual merge resolutions (the remerge diffs of 50dd56ca2 and 51ea5c7e2). It passes the official reviewer citation contract (checker 60934a193, status CHANGES; sha256 cc5292766d98f1418e2b7173032523a085250b22854d94542b76ddb2217838cb), and every location and identifier it cites resolves. It finds every resolution substance-preserving, and every migrated statement at least as strong as before. It has two findings.

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
1. **Findings 1 and 2.** ConcreteSpec.Core.Exec.Demo.erasing_internal_infinite and ConcreteSpec.Core.Exec.Demo.bumpedMove changed text in the C6 merge (env became observationEnv), but kept their old approval APPR-integ-agy-0dbeba29e. Set both to `(approved: PENDING)`. Then search the whole of ConcreteSpec/Proofs/Adequacy.lean for any other declaration whose text changed in 51ea5c7e2 (compare with both parents) and still carries an old approval, and reset those too. List every one.
2. **Record approvals.** For each declaration the review APPROVES that is currently `(approved: PENDING)`, replace the tag with `(approved: REV-integ-agy-3b54f736d)`. Match by fully qualified name, and check that the tag line is that declaration's tag. Do not record an approval on a declaration you reset in step 1: the review did not approve its new text.
3. Run the build, `nix develop -c timeout 600 bash scripts/spec/rules.sh` and `nix develop -c timeout 900 bash scripts/proofs/trust_ban.sh`.

## Rules
Change only approval tags. Commit after the checks pass, with the reset and the recording as two separate commits. Do not push. Do not edit scripts/proofs/axioms.golden (its regeneration is blocked on an owner decision), docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: the resets (with why), the approvals recorded, any PENDING left (with why), the commit SHAs, and QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1800 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
