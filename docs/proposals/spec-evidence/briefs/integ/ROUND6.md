# A2i integration round 6 (Codex writes; agy wrote round 5)

State: wip/spec-integrate at 18747a8ef (round 5 db5cacda1 by agy plus the regenerated golden). All seven gates pass at 18747a8ef. Codex's full-access review of fd589aad0..db5cacda1 says CHANGES; it is in _review/REVIEW5-codex.md. Read it in full, together with _review/ROUND5.md, the earlier briefs and reviews in _review/, _review/COMMON.md if present and _review/VACUITY_CONVENTION_FINAL.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch. Shortcuts are findings; a law that projects a premise or a conclusion of another theorem is a shortcut.

## Tasks
1. Blocker 1: map each machine transition to a finite block of C6 steps and flatten with an explicit boundary-index embedding; prove each block holds exactly that transition's projected events (a silent block for silent or erased transitions). Add a multi-event adapter Instance and a premise-preserving mutant that moves events across transition boundaries. Do not restrict adapters to singleton emissions.
2. Major 2: give `InfiniteExecution.projected_completeTrace` independent behavioral evidence over the execution-to-trace projection with fixed `CompleteTrace` semantics and a mutant producing a malformed trace; or classify the derived theorem as a Helper and state the independent Law.
3. Major 3: separate representation consistency from semantic uniqueness; state uniqueness over the execution relation and give a mutant admitting two executions with different state sequences.
4. Major 4 (`runFrom_validTrace` in Proofs/Observe2.lean): that theorem belongs to C6, whose audit branch is reworking Observe2.lean now; do not edit Observe2.lean here. Say in your final message what C8 needs from it.
5. Act on every exemption verdict; keep `(approved: PENDING)` only on helpers that stay helpers, each with its own reason.
6. Check EVERY law of C8 and of the C6 bridge it uses against the final convention, not only the changed ones.

## Rules
Do not change any owner-decided rule; if a task seems to need one, stop it and ask in QUESTIONS. Commit each piece after the build passes (do not push; do not edit scripts/proofs/axioms.golden, docs/decisions.md or ROADMAP.md; never commit _review/ or scratch files). Lean core only, no new axioms, no sorry. Keep tool output short (pipe builds through tail). Final message: per finding, done or left (with why), commit SHAs, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build through the machine-wide build semaphore: `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec` (at most two heavy builds on this machine). Gate every commit on the exit status of the check itself with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
