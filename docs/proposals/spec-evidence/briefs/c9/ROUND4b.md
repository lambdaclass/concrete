# C9 round 4, continued (Codex takes over from Kimi, whose weekly quota ran out mid-round)

Kimi committed 65aafb86f ("C9 round 4: make identity and evidence contracts operational") and left an uncommitted edit to ConcreteSpec/Spec/Observe.lean (`git diff`). Read _review/ROUND4.md (the round's task, items 1–10 including the owner decisions 8–10), Kimi's log tail in the coordinator's note below, and the current diff `git diff 4313e251a..HEAD` plus the working-tree change. Decide whether the uncommitted Observe.lean edit is correct and needed (keep and complete it, or discard it with `git checkout -- ConcreteSpec/Spec/Observe.lean`), then complete every item of ROUND4.md that is not yet done, committing each piece after the build passes. All rules of _review/BRIEF.md apply (do not push; do not edit scripts/proofs/axioms.golden). In your final message say per item 1–10 whether it was done by Kimi's commit, by you, or is left (with why).
Kimi's last log line before stopping: "The operational slice is committed as 65aafb86f. I'm adding direct accepted/refused examples showing that site-table validation now checks stored coordinates against the actual source bytes."

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch. If staging is truly unavoidable, record the long-term design and its roadmap row in your final message.

## Vacuity rule and evidence tags
Every law you add or change needs a non-trivial instance and a planted counter-example (an `example` with a negation that falsifies its conclusion), tagged `/-- Instance (law `Fully.Qualified.name`). -/` and `/-- Planted (law `Fully.Qualified.name`). -/`.
