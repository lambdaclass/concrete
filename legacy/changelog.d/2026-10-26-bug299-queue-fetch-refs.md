### The work-queue dispatcher fetches into its own refs (bug 299)

The dispatcher's `git fetch --prune origin` wrote `refs/remotes/origin/*`, which agent worktrees of
the same clone also write when they push or fetch. An agent push landing mid-fetch failed the fetch
("incorrect old value provided"); the tick then reported `source_error` and started nothing, and
`check_work_queue.sh`'s budgets leg intermittently missed its `budget_exhausted` event (CI run
40377660985). The dispatcher now fetches only into `refs/queue/remote/<remote>/*` and reads its base
and resume points from there; `QUEUE_BASE` must name a branch as `<remote>/<branch>`. A new gate leg
checks that a tick neither moves nor prunes `refs/remotes/`
([docs/bugs/299_work_queue_dispatcher_fetch_races_agent_pushes.md](docs/bugs/299_work_queue_dispatcher_fetch_races_agent_pushes.md)).
