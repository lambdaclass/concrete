# Task: vacuity fix round for an accepted contract (owner rule: no law may hold by construction)

Contract: CONTRACT (files: FILES). Base: the integrated wip/spec-core (fetch it: `git fetch origin wip/spec-core && git checkout -B BRANCH origin/wip/spec-core`).

The vacuity audit of the accepted contracts (Codex, Kimi, agy; read-only) is in _review/AUDIT_FINDINGS.md. Fix every VACUOUS or WEAK verdict that concerns your contract's files. For each, choose one of two remedies:
- (a) strengthen it into a real law: a statement about the operation's behaviour, the step relation or runs, which a mutation of the rule would falsify. Examples: replace a bare continuation lemma with a theorem over `Step`/`runFrom`; add the predecessor-success premises and prove an iff; derive Copy from static typing rather than from the evaluation guard.
- (b) demote it to a clearly named helper (`private`, or a name ending in `_inv` / `_proj`, with a doc comment saying it is an inversion or projection helper and not a contract law), provided a real law covering the same property exists or is added.

Where reviewers disagree on a verdict, decide from the statement itself and say why in your final message. Do not change any rule's meaning. If a fix would need a rule the decided rows do not answer, stop that part and list it under QUESTIONS.

Also fix the WEAK examples (for instance a Refused example refused for a different reason than the one it claims to test).
