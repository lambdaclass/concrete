# A2i integration round 5 (agy writes; Codex wrote round 4)

State: wip/spec-integrate at ed98a8476 (round 4 by Codex) plus the golden commit the gates lane adds (run `git log -1` first; build on HEAD). agy's review of fcb18d67..ed98a8476 says CHANGES; it is in _review/REVIEW4-agy.md, including a verdict on each of the 86 PENDING exemptions. Read it in full, together with _review/ROUND4.md, _review/ROUND4_RESULT.md, the earlier briefs in _review/, _review/COMMON.md if present and _review/VACUITY_CONVENTION_FINAL.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch. Shortcuts are findings. If staging is truly unavoidable, say so in your final message with the long-term design.

## Tasks (each finding of REVIEW4-agy.md)
1. Finding 1 (blocker): `RequestResolution` must be an independent specification of one request transition, not a transcription of `runFromQ`'s match arms, and must not mention `runFromQ` in its own constructors. Relate the continued case through `Move` to an intermediate position. Re-prove `runFromQ_request_correspondence` against it.
2. Finding 2: give each of its five Instance cases (terminal, invalid answer, continued, occurrence-sensitive, refused bind) a matching premise-preserving Planted case.
3. Finding 3 (blocker): define the actual C6 step stream induced by an `InfiniteExecution` under an `Observe.ExecAdapter` (silent or erased transitions become `Observe.Step.silent`, emitted visible events `Observe.Step.emit`), use `Observe.ExecAdapter.WellFormed` where it is owed, prove the execution matches its own induced stream, and state the finite and infinite observation laws about that stream, with the observed events or trace tied in the conclusion to the projected prefixes of the execution. The subject is the induced stream or the projection, not the hypothesis `MatchesProjectedHistory`. Remove the synthetic canonical-stream shortcut; no premise is discharged by reflexivity.
4. Finding 4: reclassify as `Law` with Instance and Planted pairs the contract theorems the review lists (`runFrom_mono`, `runFrom_trace_prefix_le`, `move_of_fuel_succ`, `InfiniteExecution.unique`, `silentDivergence_not_infiniteInteraction`, `not_infinite_of_final`, `not_infinite_of_noAnswer`, `endlesslyBlocked_of_noAnswer`, `TerminationEvidence.excludes_infinite`). State negation laws so a closed Instance inhabits the hypotheses.
5. Finding 5: `ConditionalTermination.dependencies` either goes or indexes `Premises`; pick the long-term design and say why.
6. Finding 6: delete the duplicate `interleaved_execution`, fix the stale examples, and make the orphan reconstruction example the Planted case for `move_of_fuel_succ`.
7. For the exemptions the review APPROVES, keep the tags and leave `(approved: PENDING)`; the coordinator records approvals. For every REJECT, act on it.
8. Check EVERY law of C8 and of the C6 bridge it uses against the final convention, not only the changed ones.

## Rules
Do not change any owner-decided rule; if a task seems to need one, stop it and ask in QUESTIONS. Commit each piece after `lake build ConcreteSpec` passes (do not push; do not edit scripts/proofs/axioms.golden, docs/decisions.md or ROADMAP.md; never commit _review/ or scratch files). Lean core only, no new axioms, no sorry. Final message: per task, done or left (with why), commit SHAs, QUESTIONS.

## Coordinator rules (binding)
Best long-term design, never cut corners. Do not run reviewers (no codex/kimi/agy review invocations of your own; review is done separately). Do not write git notes.
