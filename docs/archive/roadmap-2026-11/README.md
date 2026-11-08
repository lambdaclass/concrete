# Roadmap archive, November 2026

Status: historical. It schedules nothing.

This directory holds what is left at the root of the roadmap as it stood on 2026-11-04, before the
[new roadmap](../../../ROADMAP.md) replaced it. The files were moved here with `git mv`. Their text is
unchanged except for relative links, which were re-based to this directory.

These files stay here, outside `legacy/`, so they survive its deletion
([P21](../../decisions.md#decided-2026-11-05-project)); they are decision sources or disposition
records:

| file | what it is |
| [CANDIDATES.md](CANDIDATES.md) | the historical 2026-11-02 candidate table, retired from the live roadmap; not a current readiness record |
|---|---|
| [ROADMAP_DECISIONS.md](ROADMAP_DECISIONS.md) | the dated decision log, which docs/decisions.md and the roadmap cite |
| [task_disposition.tsv](task_disposition.tsv) | the disposition of every archived task ID (below) |
| [ROADMAP_TASKS.md](ROADMAP_TASKS.md) | the task register (bodies of the A, B and T tasks; it records the 2026-10-17 package-management decision) |
| [ROADMAP_RECONCILIATION.md](ROADMAP_RECONCILIATION.md) | the 2026-10-19 reconciliation record |
| [ROADMAP_RESTRUCTURE.md](ROADMAP_RESTRUCTURE.md), [roadmap_restructure_2026-11-02.tsv](roadmap_restructure_2026-11-02.tsv) | the 2026-11-02 restructure plan the owner adopted, and its classification |

The rest of the old roadmap is history only. On 2026-11-06 the owner ruled "do the whole movement",
so its root copies were removed. The frozen copies in `legacy/` stay until the legacy deletion point:

| file | what it was |
|---|---|
| [ROADMAP.md](../../../legacy/docs/archive/roadmap-2026-11/ROADMAP.md) | the control plane: phases, the A1 – B3 queue, the throughput track, releases |
| [ROADMAP_BACKLOG.md](../../../legacy/docs/archive/roadmap-2026-11/ROADMAP_BACKLOG.md) | the backlog (bodies of every later task) and the pull rules |
| [ROADMAP_DIRECTION.md](../../../legacy/docs/archive/roadmap-2026-11/ROADMAP_DIRECTION.md) | standing direction and product goals |
| [WORK_QUEUE.md](../../../legacy/docs/archive/roadmap-2026-11/WORK_QUEUE.md) | the manual of the agent work-queue dispatcher, retired with this archive |

## The disposition table

[task_disposition.tsv](task_disposition.tsv) has one row for every task ID the old roadmap declared.
That is the 546 task headings, plus the 9 tasks completed in `CHANGELOG.md`. R-0457 and R-0489 were
never allocated. Each row has one disposition:

| disposition | meaning | required columns |
|---|---|---|
| `retained` | a live task of the new roadmap, with the same ID | `new_phase`, `obligations` |
| `transferred-with-obligations` | its acceptance obligations move to the retained task in `destination` | `destination`, `obligations` |
| `deferred-with-trigger` | no committed plan; it comes back when `trigger` fires | `trigger` |
| `retired` | ends with the legacy compiler | `reason`; `owner_question` when the retirement drops a substantive obligation |
| `done` | already closed | `reason` |

The `obligations` column holds the requirement that survives. It is the surviving requirement of
the 2026-11-02 restructure where one was recorded, and otherwise the task's objective. The full text
stays in the archived body (ROADMAP_TASKS.md here, or ROADMAP_BACKLOG.md, now only in `legacy/`). The
classification follows the substance of each body:

- a task whose work the new plan needs as a unit is retained;
- a task that adds requirements to such a unit is transferred to it;
- a pull-gated idea is deferred.

When it was unclear whether work was committed, it was deferred.

## Rules

- **Archived IDs are never reused.** New tasks take numbers after the highest one ever allocated
  (R-0557). `scripts/tests/check_roadmap.sh` refuses a roadmap task whose ID has a disposition here
  other than `retained`, or a new ID at or below R-0557.
- **The decision log is history; the roadmap only summarizes.** The still-binding decisions are
  summarized, short, under the new roadmap's
  [rules in force](../../../ROADMAP.md#rules-in-force), each linking its dated entry here, which
  governs. A new plan decision is recorded with its date, not in the roadmap. Language decisions stay in
  [decisions.md](../../decisions.md).
