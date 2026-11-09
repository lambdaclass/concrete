# C6 audit round 3 (Codex writes; agy wrote the previous round)

State: wip/spec-c6-audit; round 2 is ad3f1ba85 by agy; the gates lane adds a golden commit on top (run git log -1 and build on HEAD). The review of the previous round says CHANGES; it is in _review/REVIEW2-codex.md. Read it in full, together with the previous round brief, the earlier briefs and reviews in _review/, _review/COMMON.md if present and _review/VACUITY_CONVENTION_FINAL.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch. Shortcuts are findings: a law that restates a definition, a runtime check or an admission premise, a conclusion of the form `match ... | none => True`, or a "specification" copied from the function it checks.

## Tasks
1. Fix every finding of _review/REVIEW2-codex.md marked REAL DEFECT, following its long-term fix or a better one (say why if you deviate).
2. Act on every exemption verdict; keep `(approved: PENDING)` only on helpers that stay helpers, each with its own reason. Do not write approvals yourself.
3. Check EVERY law of the contract against the final convention (closed full-application Instance, same-typed premise-preserving Planted mutant), not only the changed ones.
4. Also do the two carried items in _review/CARRY.md (runFrom_validTrace evidence; byte-column theorems at Proofs/Observe2.lean ~483, ~492); this branch owns Observe2.lean. 5. Decided C6 rules bind exactly (trap line, forward-slash project-relative path, 1-based line and byte column, exit 134, 125, fuel exhaustion is an evaluator report); which concrete state is externally visible stays adapter-supplied.

## Rules
Do not change any owner-decided rule; if a task seems to need one, stop it and ask in QUESTIONS. Commit each piece after the build passes (do not push; do not edit scripts/proofs/axioms.golden, docs/decisions.md or ROADMAP.md; never commit _review/ or scratch files). Lean core only, no new axioms, no sorry. Keep tool output short (pipe builds through tail). Final message: per finding, done or left (with why), commit SHAs, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build through the machine-wide build semaphore: `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec` (at most two heavy builds on this machine). Gate every commit on the exit status of the check itself with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
