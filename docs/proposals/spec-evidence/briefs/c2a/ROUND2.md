# C2 round 2 (agy writes; Codex wrote round 1b)

State: branch at cf203a6df (round 1b ce447ebbd plus the regenerated golden). All seven gates pass at cf203a6df. agy's review of 07c16bb5f..ce447ebbd says CHANGES; it is in _review/REVIEW1b-agy.md. Read it in full, together with _review/BRIEF.md (owner items 18 and 19 plus the vacuity audit), _review/COMMON.md and _review/VACUITY_CONVENTION_FINAL.md. Codex's round-1b summary: item 18 done; item 19 partial (the acyclic-order existence and order-independence proofs were left as `def ... : Prop` obligations).

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch. Shortcuts are findings. If staging is truly unavoidable, say so in your final message with the long-term design.

## Tasks (each finding of REVIEW1b-agy.md; fix every real one)
1. Item 19, finding 1: prove `DependencySortingComplete` (every acyclic declaration list has a dependency order, constructively, e.g. by a fuel-free topological sort whose output is the certificate) and `ConstOrderIndependent` as theorems in ConcreteSpec/Proofs/Const.lean, each a tagged `Law` with closed `Instance`s and `Planted` counter-examples. Remove the deferred-obligation note from ConcreteSpec/README.md once they are proved.
2. Finding 3: order independence must cover admission itself: one permutation admitted iff the other is, with the same values per id. Do not assume both are admitted.
3. Finding 4: prove the law that a `CycleChain` precludes every `DependencyOrder` (and so the list is refused with that chain), and classify `cycle_chain_real` honestly (reflection helper or law) under the convention.
4. Finding 2: a law for `CExpr.eval` / `evalConsts` on the fallible fragment: with sufficient fuel the evaluator returns either a value or a located refusal (never fuel exhaustion), and a failing operation refuses the constant (owner item 18: every initializer evaluated; failure or fuel exhaustion refuses). Do not switch the law off with `fallibleFree = true`.
5. Finding 5: restore `ofC_copy : ∀ c, isCopy (ofC c) = true` to `ConstTypes` (every scalar constant type is Copy), remove `NonCopyU` and the `_proj` lemmas it needed, drop the runtime `U.isCopy d.ty = true` guard from `typeConsts` if it becomes redundant, and prove `admitted_copy` from `ofC_copy`. Pick the planted mutant as the review suggests or better; keep the non-Copy declaration refused example.
6. Finding 6: do not tag C1's laws in ConcreteSpec/Proofs/Scalar.lean as `Helper`. A separate C1 audit branch owns migrating that file's pre-existing theorems. Scope this branch's Scalar.lean change to the checked-negation theorems you add or change (and anything the tag checker requires for them); revert the blanket `Helper (...) (approved: PENDING)` tags on the pre-existing C1 theorems. If the tag gate then fails on untouched C1 theorems, say so in your final message instead of mis-tagging them.
7. Finding 7: remove the redundant conjunct and dead lemmas.
8. Check EVERY law of the contracts this branch touches against the final convention (a non-trivial instance and a planted counter-example each), not only the changed ones.

## Open owner question (do NOT decide it)
Checked forms in initializers need a Result constant type; `CTy` stays unchanged until the owner decides. Do not add a Result type. If a task above cannot be done without deciding it, stop that task and say so.

## Rules
Commit each piece after `lake build ConcreteSpec` passes (do not push; do not edit scripts/proofs/axioms.golden, docs/decisions.md or ROADMAP.md; never commit _review/ or scratch files). Lean core only, no new axioms, no sorry. Your final message: per task, done or left (with why), the commit SHAs, and any QUESTIONS.

## Coordinator rules (binding)
Best long-term design, never cut corners. Do not run reviewers (no codex/kimi/agy review invocations of your own; review is done separately). Do not write git notes.
