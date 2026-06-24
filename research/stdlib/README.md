# Stdlib Research

Status: exploratory index

This directory collects standard-library, runtime-surface, hosted/freestanding,
resource-budget, collection, text/output, and concurrency/runtime research.

Use this directory for comparative and exploratory work. Stable stdlib contracts
belong in `docs/`; active sequencing belongs in `ROADMAP.md`; landed behavior
belongs in `CHANGELOG.md`.

Start here:

1. [stdlib-comparative-inventory.md](stdlib-comparative-inventory.md) — what
   other language stdlibs expose and how Concrete should classify each family.
2. [stdlib-design.md](stdlib-design.md) — current stdlib direction and style.
3. [stdlib-api-cleanup.md](stdlib-api-cleanup.md) — public API cleanup pressure.
4. [builtin-vs-stdlib.md](builtin-vs-stdlib.md) — compiler builtin versus
   stdlib ownership boundary.
5. [text-and-output-design.md](text-and-output-design.md) — text/output surface.
6. [runtime-collections.md](runtime-collections.md) — collection maturity.
7. [iterators.md](iterators.md) — iterator/traversal direction.
8. [allocation-budgets.md](allocation-budgets.md) — allocation classification.
9. [no-std-freestanding.md](no-std-freestanding.md) — hosted/freestanding split.
10. [concurrency.md](concurrency.md) and
    [async-concurrency-evidence.md](async-concurrency-evidence.md) — runtime and
    concurrency research.
