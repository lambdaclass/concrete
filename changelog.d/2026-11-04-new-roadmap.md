### A new, single-file roadmap; the old one archived

[ROADMAP.md](ROADMAP.md) is now one file that can be read in about 15 minutes. It holds the
vision, the binding rules, phases A–G and T, the queue (T1, A1–A3, B1–B3) and one row per live task
(objective, done when, depends on). The old roadmap, task register, backlog, direction,
reconciliation record, restructure plan, work-queue manual and decision log moved unchanged to
[docs/archive/roadmap-2026-11/](docs/archive/roadmap-2026-11/README.md). The still-binding decisions
are restated as the roadmap's binding rules, each linking its dated entry there.

Every one of the 555 old task IDs has a row in
[task_disposition.tsv](docs/archive/roadmap-2026-11/task_disposition.tsv):

| disposition | rows |
|---|---|
| retained | 79 |
| transferred-with-obligations | 342 |
| deferred-with-trigger | 109 |
| retired | 10 |
| done | 15 |

Each transferred row names the obligations it carries to its destination. Archived IDs are never
reused.

New tasks:
- R-0558: move the old tree to `legacy/` and set up the minimal root. Legacy gates run only when
  `legacy/` changes, and are otherwise reported as skipped.
- R-0559: an outside-reader dry run on the Core proposal.
- R-0560: the one-main-file Lean spec layout.
- R-0561: the bug ledger by compiler.
- R-0562: a retention manifest, checked before B3.
- R-0563: an operational migration acceptance test.
- R-0564: a pinned, reproducible legacy oracle.
- R-0565: the website transition.

Folded into existing tasks:
- the conformance format, into R-0425;
- the fuzzer retarget, into R-0272.

Gates:
- `check_roadmap.sh` replaces `check_roadmap_linear.sh` and `check_roadmap_coverage.sh`. It
  checks task IDs, archived-ID reuse, dispositions and their destinations, the queue, the owners of
  the bug classes, links and candidate bugs, with negative controls.
- The work-queue builder reads the new task rows.
- The spec's tracking-ID check (S8), the convergence inventory, the V2-freeze marker checks and the
  absorption inventory point at the new files.
