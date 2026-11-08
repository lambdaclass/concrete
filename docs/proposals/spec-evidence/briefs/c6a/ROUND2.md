# C6 audit round 2 (agy writes; Codex wrote the audit round)

State: wip/spec-c6-audit at 425ee14b5 (audit round 8dd47d36e by Codex plus the regenerated golden). All seven gates pass at 425ee14b5. agy's review of 07c16bb5f..8dd47d36e says CHANGES; it is in _review/REVIEW1-agy.md. Read it in full, together with _review/BRIEF.md, _review/AUDIT_FINDINGS.md, _review/COMMON.md if present and _review/VACUITY_CONVENTION_FINAL.md. C6 stays "reopened" until its audit passes.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch. Shortcuts are findings; a README deferral of an owner-required property is a shortcut.

## Tasks (each finding of REVIEW1-agy.md; fix every real one)
1. Blocker 1, final external state: the decided rule says observations preserve externally visible final state. Implement it: keep the final store in C1 terminal endings, project the externally visible part through the adapter (an abstract projection supplied by the adapter; do NOT decide which concrete state is externally visible, that is not yours to decide), include it in finite executions, terminal observations and observation equivalence, and re-prove the stuttering and delay laws with it. Remove the README deferral. If C1's changes needed for this conflict with a C1 rule, stop and ask in QUESTIONS.
2. Blocker 2: `ReportTraceValid` must not define the `none` case as `True`; state exactly when no report is produced and make the law fail for a mutant that always returns `none`.
3. Major 3: tie the per-event output correspondence of `project_physical_outputs` to the execution instead of an ad-hoc premise discharged by `fun _ => rfl`.
4. Majors 4 and 5: state the environment/oracle reply laws and output-before-trap as general C6 laws over the real operations (`Exec.runFrom` / `reportObservation`), with the demos as their Instances, not as demo-only laws while the general theorems are demoted to helpers.
5. Major 6: `trapLine_injective` is a Law with Instance and Planted cases.
6. Major 7: enforce `Span.PathOk` by construction or validation in `sourceSpan` / `trapTerminal`, backed by a Law, instead of inverting an assumed `WellFormed` field.
7. Major 8: give exit codes 134 (trap) and 125 (missing launch grants refuse startup) Law coverage with Instance and Planted cases.
8. Minor 9 and every exemption the review rejects: act on them; keep `(approved: PENDING)` only on helpers that stay helpers, each with its own stated reason.
9. Check EVERY law of C6 against the final convention, not only the changed ones.

## Rules
Do not change any owner-decided rule (trap line `concrete: trap: <kind> at <path>:<line>:<col> (site <id>)`, project-relative forward-slash path, 1-based line and byte column, exit 134 vs normal exit, 125 for missing launch grants, fuel exhaustion is an evaluator report and never an external event); if a task seems to need one, stop it and ask in QUESTIONS. Commit each piece after `lake build ConcreteSpec` passes (do not push; do not edit scripts/proofs/axioms.golden, docs/decisions.md or ROADMAP.md; never commit _review/ or scratch files). Lean core only, no new axioms, no sorry. Final message: per task, done or left (with why), commit SHAs, QUESTIONS.

## Coordinator rules (binding)
Best long-term design, never cut corners. Do not run reviewers (no codex/kimi/agy review invocations of your own; review is done separately). Do not write git notes.

## Build and commit gating (binding)
Run every Lean build through the machine-wide build semaphore: `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec` (at most two heavy builds on this machine). Gate every commit on the check's own exit status with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
