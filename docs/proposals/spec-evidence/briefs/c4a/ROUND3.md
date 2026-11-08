# C4 audit round 3 (Codex writes; agy wrote round 2)

State: branch at 61145d7cf (round 2 by agy plus the regenerated golden). All seven gates pass at 61145d7cf. Codex's full-access review of c8c76adef..(round 2) says CHANGES; it is in _review/REVIEW2-codex.md. Read it in full, together with _review/BRIEF.md, _review/ROUND2.md, the earlier reviews in _review/, _review/COMMON.md if present and _review/VACUITY_CONVENTION_FINAL.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch. Shortcuts are findings. A law that restates a validator, a guard or a constructor list is a shortcut, whichever direction it points.

## Tasks
1. Fix every finding of REVIEW2-codex.md marked REAL DEFECT, following its long-term fix or a better one (say why if you deviate).
   Owner item 20 binds: bytes that are not a whole stored pointer carry no pointer meaning.
2. Act on every exemption verdict in the review; keep `(approved: PENDING)` only on helpers that stay helpers, each with its own stated reason. A substantive invariant is a Law, not an inversion helper.
3. Check EVERY law of the contract against the final convention (closed full-application Instance, same-typed premise-preserving Planted mutant), not only the changed ones.

## Rules
Do not change any owner-decided rule; if a task seems to need one, stop it and ask in QUESTIONS. Commit each piece after the build passes (do not push; do not edit scripts/proofs/axioms.golden, docs/decisions.md or ROADMAP.md; never commit _review/ or scratch files). Lean core only, no new axioms, no sorry. Keep tool output short (pipe builds through tail) so the session does not hit its output limit. Final message: per finding, done or left (with why), commit SHAs, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build through the machine-wide build semaphore: `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec` (at most two heavy builds on this machine). Gate every commit on the exit status of the check itself with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
