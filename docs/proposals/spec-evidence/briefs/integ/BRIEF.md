# Task: integration merge fix (A2i) — make C6's proofs build over C1's partial lifecycle

This clone is at 6157005eb on branch wip/spec-integrate. That commit merges three accepted contracts into the spec core:
- the partial lifecycle change to C1 (d8f89ea75: `Operands.life : Life → Store → Option Store`; a refused lifecycle event is stuck; the request continuation of `Exec.Out.request` now returns `Option (Exec.Config O)`, and `none` means the continuation was refused);
- C4 (b9c3aeaaf);
- C6 (738500fb, observations; written over the old C1, whose continuation returned `Exec.Config O`).

The merge builds except ConcreteSpec/Proofs/Observe2.lean (around lines 495–532): C6's adapter proofs still assume the total continuation. Read ConcreteSpec/Spec/Exec.lean and Proofs/Exec.lean (including the new laws `*_refused`, `continuation_refused`, `no_step_of_none`), Spec/Observe.lean and Proofs/Observe2.lean.

Make Spec/Observe.lean and Proofs/Observe2.lean build over the partial continuation without weakening any C6 law's meaning:
- a refused continuation is stuck (C1's stuck ending), not an observation of a response or an output;
- the adapter's projection and `ValidTrace`/`BlockingPermitted` laws keep their statements, with only the natural premise that the continuation succeeded where needed;
- add one example where a refused continuation gives the stuck ending and no response event.

Do not edit Exec.lean, Memory.lean, Hybrid.lean, Scalar.lean, Const.lean or Outcome.lean. If C6's meaning cannot be kept, stop and say why under QUESTIONS.

## Vacuity rule (owner, binding)
No law may hold because a guard or definition makes it trivially true. For every law you add or change:
- give a non-trivial instance (an example where its hypotheses hold);
- give a planted counter-example (a mutated rule or a bad state) that fails to satisfy it, as an `example` with a negation.
## Hard rules (all apply)
- Lean 4.34.1, core only (no Mathlib); warningAsError and autoImplicit off; namespace ConcreteSpec.Core (sub-namespace allowed). Lean IS the spec: readable Lean, doc comments, examples ("Accepted"/"Refused" examples where a rule is registered), no generated prose.
- No `sorry`, no new axioms, no `decide +native`, no `native_decide`; every theorem must use only propext / Quot.sound / Classical.choice (scripts/proofs/axioms.sh checks this).
- Do NOT edit docs/decisions.md or ROADMAP.md. Do NOT decide any language question the decided rows do not answer: if you hit one, stop that part, leave it out (or leave the existing draft behaviour, documented as pending), and list it under "QUESTIONS" in your final message with the options you see.
- Do not edit ConcreteSpec/Spec/Exec.lean, Scalar.lean, Const.lean, Outcome.lean except where the brief says; they are reviewed and accepted.
- Only work in this clone (the current directory), on branch shown by `git branch --show-current`. Commit each piece as soon as `nix develop -c lake build ConcreteSpec` passes (run it with `timeout 1200`). Do NOT push; do NOT run git commands that rewrite history; never --no-verify.
- Light local checks you may run: `nix develop -c timeout 1200 lake build ConcreteSpec`, `nix develop -c timeout 600 bash scripts/spec/rules.sh`, `nix develop -c timeout 900 bash scripts/proofs/trust_ban.sh`. Do NOT run axioms.sh, kernel_replay.sh, external.sh or checks/run.sh (heavy; the reviewer runs them on a CI server). Every new module must be imported from ConcreteSpec/Spec.lean (spec) or ConcreteSpec.lean (proofs), or trust_ban fails.
- Final message: list of commits (SHA + one line), what each theorem states, what is left out and why, QUESTIONS.

## Vacuity rule (owner, binding)
No law may hold because a guard or a definition makes it trivially true (for example a run-time re-check of the invariant inside the operation, a theorem that restates its definition, or a hypothesis that can never hold). For every law you add or change, give (a) a non-trivial instance: an example where its hypotheses hold, and (b) a planted counter-example: a mutated rule or a bad state that fails it, written as an `example` with a negation. Say in your final message, per law, which examples witness (a) and (b).
