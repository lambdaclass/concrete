# Project Documentation

Status: stable reference index

Project identity, design policy, principles, decisions, workflow, examples, release bundles, and
repository-wide conventions.

Start with [PROJECT.md](PROJECT.md): why Concrete exists, its identity, principles, design policy,
language shape and recorded language decisions, in one document.

Policy and process:

- [PROJECT.md](PROJECT.md): why Concrete, identity, principles, design policy, language shape and
  language decisions
- [AGENTS.md](AGENTS.md): the agent and contributor workflow (playbooks, shared rules, concurrent
  work)
- [STYLE.md](STYLE.md): naming, structure and idioms for Concrete code, including the iteration
  protocol
- [EXAMPLES.md](EXAMPLES.md): the example inventory, promotion levels and the no-duplicate rule
- [ABSENCE_IS_NOT_A_FACT.md](ABSENCE_IS_NOT_A_FACT.md): the gated rule that an unknown value never
  defaults to a reassuring one
- [INFLUENCES.md](INFLUENCES.md): what Concrete copies, adapts or rejects from other languages

Tool manuals and contracts: [LANDING.md](LANDING.md), [REVIEW.md](REVIEW.md),
[WORK_QUEUE.md](WORK_QUEUE.md), [STATUS_PAGE.md](STATUS_PAGE.md),
[SELF_HOSTED_RUNNERS.md](SELF_HOSTED_RUNNERS.md), [PERF_TRACKING.md](PERF_TRACKING.md),
[BUG_BUNDLE.md](BUG_BUNDLE.md), [RELEASE_BUNDLE.md](RELEASE_BUNDLE.md),
[POLICY_FILES.md](POLICY_FILES.md), [GOLDEN_ORACLES.md](GOLDEN_ORACLES.md),
[CONSTRUCTOR_COVERAGE.md](CONSTRUCTOR_COVERAGE.md), [PROJECT_BOOTSTRAP.md](PROJECT_BOOTSTRAP.md),
[STANDALONE_VS_PROJECT.md](STANDALONE_VS_PROJECT.md),
[AUTHORING_WALKTHROUGH.md](AUTHORING_WALKTHROUGH.md).

Roadmap companions and records: [ROADMAP_TASKS.md](ROADMAP_TASKS.md),
[ROADMAP_BACKLOG.md](ROADMAP_BACKLOG.md), [ROADMAP_DIRECTION.md](ROADMAP_DIRECTION.md),
[ROADMAP_DECISIONS.md](ROADMAP_DECISIONS.md), [ROADMAP_RECONCILIATION.md](ROADMAP_RECONCILIATION.md),
[PROMOTION_PIPELINE.md](PROMOTION_PIPELINE.md), [GATE_PROFILE.md](GATE_PROFILE.md),
[DETERMINISTIC_SIMULATION.md](DETERMINISTIC_SIMULATION.md),
[REGISTRY_FIXTURE_INVENTORY.md](REGISTRY_FIXTURE_INVENTORY.md). Tables read by tools:
`candidates.tsv` (the candidate records ROADMAP's generated span is built from) and
`flaky_gates.tsv` (the CI triage list).

`IDENTITY.md`, `PRINCIPLES.md`, `DESIGN_POLICY.md`, `LANGUAGE_SHAPE.md` and `DECISIONS.md` are
one-line pointers into PROJECT.md, kept because documents outside this directory still link them.
