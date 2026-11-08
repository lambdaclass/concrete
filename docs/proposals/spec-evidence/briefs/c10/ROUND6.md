# C10 round 6 (agy writes; Codex wrote round 5)

State: wip/spec-c10 at 487085db1 (round 5 4159b7c56 by Codex plus the regenerated golden). All seven gates pass at 487085db1. agy's review of 2dfce5860..4159b7c56 says CHANGES; it is in _review/REVIEW5-agy.md (its first section answers whether the fixes are real, whether the toy machines are independent, whether helpers hide laws and whether the named obligations are legitimate deferrals; then 9 findings). Read it in full, together with _review/ROUND5.md, _review/ROUND5b.md, the earlier briefs and reviews in _review/, _review/COMMON.md if present and _review/VACUITY_CONVENTION_FINAL.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch. Shortcuts are findings. A named obligation is legitimate only when a later slice really supplies the model it needs; otherwise prove it now.

## Tasks
1. Fix every finding of REVIEW5-agy.md marked REAL DEFECT, following its long-term fix or a better one (say why if you deviate).
2. Act on the review's answers on deferrals and helpers: any obligation it judges a shortcut is proved now; any helper it judges a contract law becomes a Law with Instance and premise-preserving Planted cases.
3. Act on every exemption verdict; keep `(approved: PENDING)` only on helpers that stay helpers, each with its own reason.
4. Check EVERY law of C10 against the final convention, not only the changed ones.

## Rules
Do not change any owner-decided rule; if a task seems to need one, stop it and ask in QUESTIONS. Commit each piece after the build passes (do not push; do not edit scripts/proofs/axioms.golden, docs/decisions.md or ROADMAP.md; never commit _review/ or scratch files). Lean core only, no new axioms, no sorry. Final message: per finding, done or left (with why), commit SHAs, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build through the machine-wide build semaphore: `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec` (at most two heavy builds on this machine). Gate every commit on the exit status of the check itself with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
