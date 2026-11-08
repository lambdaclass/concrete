# Task: vacuity fix round for an accepted contract (owner rule: no law may hold by construction)

Contract: C1, the core machine (Exec) (files: ConcreteSpec/Spec/Exec.lean, ConcreteSpec/Proofs/Exec.lean, the C1 lines of ConcreteSpec/README.md). Base: the integrated wip/spec-core (fetch it: `git fetch origin wip/spec-core && git checkout -B wip/spec-c1-audit origin/wip/spec-core`).

The vacuity audit of the accepted contracts (Codex, Kimi, agy; read-only) is in _review/AUDIT_FINDINGS.md. Fix every VACUOUS or WEAK verdict that concerns your contract's files. For each, choose one of two remedies:
- (a) strengthen it into a real law: a statement about the operation's behaviour, the step relation or runs, which a mutation of the rule would falsify. Examples: replace a bare continuation lemma with a theorem over `Step`/`runFrom`; add the predecessor-success premises and prove an iff; derive Copy from static typing rather than from the evaluation guard.
- (b) demote it to a clearly named helper (`private`, or a name ending in `_inv` / `_proj`, with a doc comment saying it is an inversion or projection helper and not a contract law), provided a real law covering the same property exists or is added.

Where reviewers disagree on a verdict, decide from the statement itself and say why in your final message. Do not change any rule's meaning. If a fix would need a rule the decided rows do not answer, stop that part and list it under QUESTIONS.

Also fix the WEAK examples (for instance a Refused example refused for a different reason than the one it claims to test).

This brief overrides the common rule "do not edit Exec.lean": you are to edit Spec/Exec.lean and Proofs/Exec.lean, without changing any rule's meaning.

C1 rows to fix (see the tables in _review/AUDIT_FINDINGS.md, and the C7 audit's agy table in _review/agy-vacuity-audit.md, which lists the same C1 laws): `continuation_refused` (a bare Option lemma), the `*_refused` laws (`bind_refused`, `assign_refused`, `enter_refused`, `call_refused`, `scope_drop_refused`, `exit_drop_refused`, `test_drop_refused`, `return_refused`, `prim_bind_refused`: each restates a `step` unfolding; several also hold for a reason other than the refused lifecycle event, e.g. a failed lookup or arity mismatch, so add the predecessor-success premises and state them over `Step`/`runFrom`, each with an instance where only the lifecycle event refuses and a planted rule that ignores the refusal), `step_deterministic` / `step_silent_deterministic` (determinism of a function is free: either relate `Step` to an independently stated relation or demote them to helpers), `destroy_not_copy` (projection of `D.notCopy`), `cleans_iff_runs_plan` (weak: add a law over actual runs that cleanup runs exactly the plan and nothing else, with a planted extra/missing cleanup).

Owner decision 2026-11-08 (15:08 UTC), item 21, now confirms what this module already encodes: a cleanup (lifecycle) step can be refused, and a refused step ends stuck. Cite it in the doc comment where the partial lifecycle is described (replacing any wording that presents it as pending), and keep the behaviour.

## Vacuity rule (owner, binding)
No law may hold because a guard or a definition makes it trivially true (for example a run-time re-check of the invariant inside the operation, a theorem that restates its definition, or a hypothesis that can never hold). For every law you add or change, give (a) a non-trivial instance: an example where its hypotheses hold, and (b) a planted counter-example: a mutated rule or a bad state that fails it, written as an `example` with a negation. Say in your final message, per law, which examples witness (a) and (b).

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch: no law that holds by construction or restates its definition, no weakened statement, no premise that switches off an obligation. If staging is truly unavoidable, say so in your final message and record the long-term design and the roadmap row it belongs to (text for the coordinator; do not edit ROADMAP.md).

## Evidence tags (for the coming vacuity gate)
Tag every instance and planted counter-example in its doc comment: `/-- Instance (law `Fully.Qualified.name`). ... -/` and `/-- Planted (law `Fully.Qualified.name`). ... -/` (always the fully-qualified Lean name, never a short name; one example may list several laws). Mark helpers `/-- Helper (inversion|projection|congruence|arithmetic). ... -/` (and make them `private` or suffix `_inv` / `_proj`), representation lemmas `/-- Representation lemma. ... -/`, and named obligations on later components `/-- Obligation (discharged at slice N, R-xxxx). ... -/`. Keep the existing `Accepted (rule ...)` / `Refused (rule ...)` tags.
