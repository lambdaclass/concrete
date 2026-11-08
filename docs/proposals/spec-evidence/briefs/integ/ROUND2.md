# A2i integration round 2: merge accepted C8 into the integrated spec core

This clone is on branch wip/spec-integrate at 07c16bb5, which is origin/wip/spec-core (C1/C2, C4, the C1 partial lifecycle, C6; all seven gates pass). C8 (adequacy) is accepted at origin/wip/spec-c8 6c24007e, and its Exec.lean already carries the partial continuation (`k : Answer → Option (Exec.Config O)`), so the merge is expected to conflict only in module/import lists and the golden; but it was not built together with C4 and C6 or the integration fixes. Check that C8's Exec.lean equals spec-core's after the merge (`git diff origin/wip/spec-core -- ConcreteSpec/Spec/Exec.lean ConcreteSpec/Proofs/Exec.lean` must be empty or explained).

Do, in order, committing each step after `nix develop -c timeout 1200 lake build ConcreteSpec` passes (do not push):
1. `git fetch origin wip/spec-c8 && git merge --no-edit origin/wip/spec-c8`. Expected textual conflicts: ConcreteSpec.lean and ConcreteSpec/Spec.lean (import / module lists: keep every import and list entry from both sides, in the existing order style) and scripts/proofs/axioms.golden (take the spec-core side verbatim: `git checkout --ours scripts/proofs/axioms.golden`; the reviewer regenerates it). Commit the merge even if it does not build yet, with the conflict resolutions only, and say in the message which files conflicted.
2. Make the merged tree build. C8's Spec/Adequacy.lean and Proofs/Adequacy.lean must work over the partial continuation without weakening any C8 law's meaning: the interpreter/report soundness (`runFromQ_sound` and the rest) keeps its statement, a refused continuation maps to the continuation-refused stuck conclusion with its exact query, and only the natural premise that the continuation succeeded is added where a law needs it. Keep or adapt C8's existing examples so each changed law still has a non-trivial instance and a planted counter-example.

Do not edit Exec.lean, Memory.lean, Hybrid.lean, Scalar.lean, Const.lean, Outcome.lean or the C6 files (Observe*.lean). If C8's meaning cannot be kept over the integrated C1, stop and say why under QUESTIONS. Do not touch docs/decisions.md or ROADMAP.md.

## Vacuity rule (owner, binding)
No law may hold because a guard or a definition makes it trivially true (a run-time re-check of the invariant inside the operation, a theorem that restates its definition, a hypothesis that can never hold). For every law you add or change, give (a) a non-trivial instance and (b) a planted counter-example written as an `example` with a negation, and list them per law in your final message.

## Final message
The commits (SHA + one line), the conflicted files and how each was resolved, every C8 statement you changed (before/after) and why, the (a)/(b) witnesses per changed law, the build/rules/trust_ban results you ran, and QUESTIONS ("None" if none).
