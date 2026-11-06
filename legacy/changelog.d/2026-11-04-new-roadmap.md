### A new, single-file roadmap; the old one archived

[ROADMAP.md](ROADMAP.md) is now one file that can be read in about 15 minutes. It holds the
vision, a summary of the rules in force, phases A–G and T, the queue (T1–T2, A1–A3, B1–B3) and one row per live task
(objective, done when, depends on). The old roadmap, task register, backlog, direction,
reconciliation record, restructure plan, work-queue manual and decision log moved unchanged to
[docs/archive/roadmap-2026-11/](docs/archive/roadmap-2026-11/README.md). The still-binding decisions
are summarized as the roadmap's rules in force, each linking its dated entry, which governs; the
roadmap decides nothing itself.

Every one of the 555 old task IDs has a row in
[task_disposition.tsv](docs/archive/roadmap-2026-11/task_disposition.tsv):

| disposition | rows |
|---|---|
| retained | 79 |
| transferred-with-obligations | 345 |
| deferred-with-trigger | 109 |
| retired | 7 |
| done | 15 |

Each transferred row names the obligations it carries to its destination. Archived IDs are never
reused.

The owner chose, on 2026-11-04, to move the old tree into a `legacy/` folder on main and start the
new layout from scratch beside it. The roadmap's details of that move (byte-frozen snapshot,
rehearsal first, retention manifest) are the restart proposal P5–P7/P21, pending the owner.

New tasks:
- R-0558: move the old tree to `legacy/` in one train and set up the minimal root. `legacy/` is a
  byte-frozen snapshot, never patched or run; a root check refuses changes to it other than
  deletions its retention row allows.
- R-0559: an outside-reader dry run on the Core proposal.
- R-0560: the one-main-file Lean spec layout.
- R-0561: the bug ledger by compiler.
- R-0562: the root retention manifest `RETENTION.tsv`, written before the move; B3 deletes only
  the old pipeline, and retained assets leave `legacy/` only when their rows resolve.
- R-0563: an operational migration acceptance test, rehearsed before the move.
- R-0564: a pinned, reproducible legacy oracle built from the tag, independent of `legacy/`.
- R-0565: the website transition.

Phase E keeps frame reasoning and termination proofs within OD115–OD117: no source `old`, no
snapshot construct, total spec functions, and no total-function category (R-0524, R-0526 and their
disposition rows reworded).

Folded into existing tasks:
- the conformance format, into R-0425;
- the fuzzer retarget, into R-0272.

Gates:
- `check_roadmap.sh` replaces `check_roadmap_linear.sh` and `check_roadmap_coverage.sh`. It
  checks that the disposition table has exactly one row per archived ID, task rows and their IDs,
  archived-ID reuse, dispositions with their destinations and obligations, the queue and its phases,
  the "depends on" items, the owners of the bug classes, links and candidate bugs, with negative
  controls.
- The work-queue builder reads the new task rows. It resolves a phase or queue-row dependency to
  its tasks, and refuses a "depends on" item it cannot read. It holds back a task that waits on an
  owner decision or on a live task that is not queued.
- The spec's tracking-ID check (S8), the convergence inventory, the V2-freeze marker checks and the
  absorption inventory point at the new files.

Integrated review (Codex, 2026-11-04):
- The restart runs in three queue rows: T0 prepares the root infrastructure beside the old tree
  (new task R-0568: new-root CI, `required-checks.txt` and an explicit old-to-new job mapping,
  `ISSUES.tsv` and its validator, archive records), T1 rehearses the move on the exact candidate
  root (R-0563) and T2 moves (R-0558). The oracle lock carries a release artifact and its digest,
  and a missing artifact leaves the check incomplete.
- Phase A follows decision row Q9.19 (pending): A2a provisional approval, A2b a proposed `Spec.lean`
  with the four named design theorems, A2c the owner's freeze (new task R-0567). R-0560 depends on
  the freeze.
- New task R-0566 consolidates the still-binding decisions in one live home (decision row P20,
  pending). Prose chapters retire only after a browsable rendering of the Lean rules and a
  rule-by-rule coverage report.
- Fuzzing restarts from the repository's surviving regressions and new programs: the ci-3 corpus,
  findings and perf history were not copied (decision row P14).
- Queue row IDs may carry `0` and a stage letter (T0, A2b); `check_roadmap.sh` and the work-queue
  builder accept them.
