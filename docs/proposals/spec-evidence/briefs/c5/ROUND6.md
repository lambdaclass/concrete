# C5 round 6 (agy writes; Codex wrote round 5)

State: wip/spec-c5 at a8fd5384 (round 5 8e11b786 by Codex plus the regenerated golden). All seven gates pass at a8fd5384. agy's review of fb205bc4e..8e11b786 says CHANGES; it is in _review/REVIEW5-agy.md. Read it in full, together with _review/REVIEW4-codex.md, _review/ROUND5.md, the earlier briefs in _review/, _review/COMMON.md if present and _review/VACUITY_CONVENTION_FINAL.md. The review confirms that Q1-Q3 are unchanged in meaning and that tagging is complete.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch. Shortcuts are findings.

## How the two reviews fit together
REVIEW4-codex said these theorems were definitional (projections, guard restatements, constructor facts) and must not count as soundness evidence. REVIEW5-agy says 29 of them are real C5 contract obligations (including Q1's downward-only restriction, Q2's deferred-call law, the erasure laws the README advertises, loan birth/supply, access preservation and progress) and must not simply be demoted. Both hold: keep any definitional form as an internal helper if it helps proofs, and state each of those 29 obligations as a `Law` over the actual operation results, with a closed Instance and a premise-preserving Planted pair, so that it says something a wrong operation could violate.

## Tasks (each finding of REVIEW5-agy.md)
1. Major 1: the 29 obligations listed in section 4A of the review, as above.
2. Major 2: make activation-local ownership complete in `WellFormed` (every slot of an active frame's activation is one of its locals; every slot's activation is an active frame), and prove cleanup from that, as the review describes.
3. Major 3: replace `passOwnerRefuse` with a mutant that succeeds through pass, enter and leave but leaks the root loan, so `directOwner_roundtrip` fails for the right reason.
4. Minor 4: replace the generic `corruptState` wrapper with operation-specific mutants for the `*_preserves` laws (the existing `Examples.*` mutants where they fit).
5. Finding 5: make the instrumentation or the oracle lifting the subject of the simulation and converse laws and mutate it, keeping `eraseConfig` fixed.
6. Act on every exemption the review rejects; keep `(approved: PENDING)` only on helpers that stay helpers, each with its own reason.
7. Check EVERY law of C5 against the final convention, not only the changed ones.

## Rules
Do not change any owner-decided rule (Q1, Q2, Q3 as in ROUND5.md); if a task seems to need one, stop it and ask in QUESTIONS. Commit each piece after `lake build ConcreteSpec` passes (do not push; do not edit scripts/proofs/axioms.golden, docs/decisions.md or ROADMAP.md; never commit _review/ or scratch files). Lean core only, no new axioms, no sorry. Final message: per task, done or left (with why), commit SHAs, QUESTIONS.

## Coordinator rules (binding)
Best long-term design, never cut corners. Do not run reviewers (no codex/kimi/agy review invocations of your own; review is done separately). Do not write git notes.
