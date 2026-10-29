### docs/project policy and process documents consolidated

Eleven overlapping policy and process documents in `docs/project/` were absorbed into four:

- [docs/project/PROJECT.md](docs/project/PROJECT.md) absorbs `IDENTITY.md`, `WHY_CONCRETE.md`,
  `PRINCIPLES.md` (principle numbers unchanged), `DESIGN_POLICY.md`, `LANGUAGE_SHAPE.md` and
  `DECISIONS.md`: why Concrete, identity, principles, design policy, language shape and the
  recorded language decisions in one document. The website's reference section shows it as one
  page (`scripts/gen/site_reference.py`) in place of the four it replaces.
- [docs/project/AGENTS.md](docs/project/AGENTS.md) absorbs `CONCURRENT_WORK.md` and is now the
  single agent and contributor workflow document. Its server-run section drops the deleted
  concrete-ci-1 host selector (`CONCRETE_REMOTE=scaleway`, width 32), and its throughput rules now
  name the landing script and the heavy-run semaphore that exist.
- [docs/project/EXAMPLES.md](docs/project/EXAMPLES.md) absorbs `EXAMPLE_INVENTORY.md`,
  `EXAMPLE_LIFECYCLE.md` and `EXAMPLE_NO_DUPLICATES.md`.
- [docs/project/STYLE.md](docs/project/STYLE.md) absorbs `ITERATION_PROTOCOL.md` as its
  "Iteration protocol" section.

`IDENTITY.md`, `PRINCIPLES.md`, `DESIGN_POLICY.md`, `LANGUAGE_SHAPE.md` and `DECISIONS.md` remain as
one-line pointers because the language documents link them. Tool manuals, contracts, roadmap
companions and tool-read documents (`LANDING.md`, `REVIEW.md`, `PROMOTION_PIPELINE.md`,
`ABSENCE_IS_NOT_A_FACT.md` and the rest) are unchanged.
