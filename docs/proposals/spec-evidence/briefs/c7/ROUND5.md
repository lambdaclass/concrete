# C7 round 5 (agy writes; Codex wrote round 4)

State: wip/spec-c7 at f81d146b7 (round 4 224ccfb3f by Codex plus the regenerated golden). All seven gates pass at f81d146b7. agy's review of e13f0f27a..224ccfb3f says CHANGES; it is in _review/REVIEW4-agy.md. Read it in full, together with _review/ROUND4.md, the earlier briefs and reviews in _review/, _review/COMMON.md if present and _review/VACUITY_CONVENTION_FINAL.md. The review confirms the peak-trajectory fix and the reset/release and lineage arithmetic; the remaining defects are evidence and classification.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch. Shortcuts are findings. If staging is truly unavoidable, say so in your final message with the long-term design.

## Tasks (each finding of REVIEW4-agy.md; fix every real one)
1. Blocker 1: the `timing_not_ct` Planted mutant must keep every `ConstantTime` hypothesis inhabited (low-equivalent starts, equal choices, equal releases) so it fails for the right reason.
2. Blocker 2: the closed `secure_ni` / `secure_ct` instances must use distinct secrets, so the release-equality hypothesis is exercised and not discharged by `rfl`. You may restate the laws over a configuration family or over all low-equivalent pairs, but do not change what delimited-release noninterference or constant time MEANS (owner decisions: Secret<T>, declassify, delimited release). If a fix needs a change of meaning, stop that task and ask in QUESTIONS.
3. Blocker 3: every Instance is one full closed application of its law with every premise discharged on the concrete witness and the instantiated conclusion as its type; no implication wrappers, no duplicate theorem layer.
4. Major 4: classify `Account.enter_wf`, `free_conservation`, `reset_conservation`, `authorized_release_not_ni`, `authorized_release_not_ct` and `run_report_transport` honestly (as Laws with Instance and premise-preserving Planted pairs where they are contract facts), and generalize `CertifiesReport` / `report_certified` to arbitrary lineage reports (`.notEstablished` bounds, absent footprint) if that is the long-term shape, as the review proposes.
5. Major 5: delete duplicate examples and subject-free tautologies; convert orphaned instances and mutants into proper Instance/Planted declarations; either use `step_unique` where it belongs or classify it as a Law with `permissive_step_not_unique` as its Planted mutant. Do not mass-tag the remaining examples `approved: PENDING`; each remaining exemption needs its own stated reason.
6. Act on the review's per-item helper notes; keep `(approved: PENDING)` only on helpers that stay helpers.
7. Check EVERY law of C7 against the final convention, not only the changed ones.

## Rules
Do not change any owner-decided rule; if a task seems to need one, stop it and ask in QUESTIONS. Commit each piece after `lake build ConcreteSpec` passes (do not push; do not edit scripts/proofs/axioms.golden, docs/decisions.md or ROADMAP.md; never commit _review/ or scratch files). Lean core only, no new axioms, no sorry. Final message: per task, done or left (with why), commit SHAs, QUESTIONS.

## Coordinator rules (binding)
Best long-term design, never cut corners. Do not run reviewers (no codex/kimi/agy review invocations of your own; review is done separately). Do not write git notes.

## Build and commit gating (binding)
Run every Lean build through the machine-wide build semaphore: `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec` (at most two heavy builds on this machine). Gate every commit on the check's own exit status with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
