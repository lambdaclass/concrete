# Bug 299 — work-queue dispatcher fetch races agent pushes on shared remote-tracking refs

<!-- bug-ledger
summary: the work-queue dispatcher fetched with `git fetch --prune origin` into refs/remotes/origin/*, the namespace every agent worktree of the same clone also writes (a push updates its remote-tracking ref); an agent push landing mid-fetch failed the fetch ("incorrect old value provided"), the tick reported source_error and skipped dispatch, so check_work_queue.sh's budgets leg intermittently saw no budget_exhausted event. The dispatcher now fetches only into its own namespace refs/queue/remote/<remote>/* and reads its base and resume points from there.
coverage: tooling-only defect pinned by scripts/tests/check_work_queue.sh leg "the dispatcher's fetch writes only its own refs" (an agent's diverged and pruned-on-remote refs/remotes/ entries survive a tick; the fetch lands in refs/queue/remote/ and prunes there; a QUEUE_BASE outside <remote>/<branch> is refused) and its budgets leg; no compiler fixture applies.
class: 16
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (2026-10-26, branch `wip/bug299-queue-fetch-refs`)
**Found:** 2026-10-26, CI run 40377660985 (branch `wip/bug-298`, which changes only `run_tests.sh`
and compiler sources), job "LL(1) grammar check" (121765036323) on `concrete-ci-2`:
`FAIL hours: 'R-0547' events=0`, followed by `ok status.json reports the budget used`. The same gate
passed in main's run 40374824336 on `1bcb7c36`. Confirmed by Codex (read-only, on `1bcb7c36`):
"VERDICT: REAL" (below).
**Severity:** loud. In the gate, an intermittent false FAIL of the budgets leg. In the production
queue, a tick that overlaps an agent's push or fetch reports `source_error` (and notifies) and starts
nothing that tick; monitoring and the kill switch still run, and the next tick retries.

## Reproducer

On OVH at `1bcb7c36`, in `nix develop .#gates`:

- The budgets leg alone (`scenario hours`, hang stub, `tick $T0`, `tick $((T0+5400))`), repeated:
  1 failure in 200 runs (run 194), then 0 in 1200 more (200 with 16 CPU hogs, 4×250 in parallel):
  about 1 in 1400. The failing run's `events.log`:
  `{"error": "refresh: git fetch -q --prune origin: error: fetching ref refs/remotes/origin/wip/queue/R-0547 failed: incorrect old value provided", "event": "source_error"}`.
- The mechanism alone: a bare remote, a clone, and a worktree of the clone that loops `git commit
  --allow-empty; git push origin HEAD:refs/heads/wip/queue/R-0547` while the clone loops `git fetch
  -q --prune origin`: 31 of 300 fetches failed with that error.

## Cause

`Q.refresh_sources` (`scripts/queue/dispatch.py`) ran `git fetch -q --prune origin` in the queue
clone. Agents run in worktrees of that clone and share its ref store. A push from a worktree updates
the matching remote-tracking ref `refs/remotes/origin/wip/queue/<task>`, and an agent's own fetch
writes `refs/remotes/origin/*`. When such an update lands between the dispatcher fetch reading a
ref's old value and committing its ref transaction, the transaction's compare fails and the fetch
exits nonzero. `tick` then flags `source_error`, `queue` is `None` and `dispatch` never runs, so
`budget_block` is never consulted. The gate's hang stub pushes right after the first tick's `go`
file, so its push can overlap the second tick's fetch. Agent-hours themselves come only from the
injected clock (`runs[].start = t_now`, `usage_today(t_now)`), which is why `status.json` still
reported 1.5 hours: this was never a clock or process-start-time effect.

## Fix

The dispatcher owns a ref namespace no agent writes, `refs/queue/remote/<remote>/*` (`REMOTE_NS`).
Its fetch is `git fetch -q --prune --no-tags --refmap= <remote> +refs/heads/*:refs/queue/remote/<remote>/*`:
the empty `--refmap=` drops the configured refspec, so not even the opportunistic remote-tracking
update touches `refs/remotes/`. The base (`QUEUE_BASE`, default `origin/main`, now required to be
`<remote>/<branch>`), the landed check and the resume point of a restarted task are read from that
namespace. No sleep, retry or wait was added: the race is removed by ownership, so the budgets leg
is deterministic as written.

Regression coverage: `check_work_queue.sh`, leg "the dispatcher's fetch writes only its own refs":
an agent's view of a remote branch that differs from the remote, and one the remote no longer has,
survive a tick (the old fetch force-updated the first and pruned the second, so the leg fails on
`1bcb7c36` every time); an agent's stale `refs/remotes/origin/main` is neither moved nor used: the
source checkout is detached at the remote's main read from `refs/queue/remote/`; the fetch lands in
and prunes `refs/queue/remote/`; a `QUEUE_BASE` that is not `<remote>/<branch>` (`main`, `origin/`,
`origin/main~1`, `origin/main^`, `origin/@{1}`) is refused.

Measured on OVH (`nix develop .#gates`; the first two rows run the budgets leg alone, the dispatcher at `1bcb7c36` and at `8789b10d`):

| run | before (`1bcb7c36`) | after |
|---|---|---|
| as written, repeated | 1 / 1400 failed | 0 / 1200 failed (4 × 300 in parallel, 16 CPU hogs) |
| amplified: the stub pushes in a loop and the second tick waits for its first push | 9 / 200 failed, all `source_error` with "incorrect old value provided" | 0 / 200 failed |
| mechanism: worktree push loop against the clone's fetch | 31 / 300 fetches failed | 0 / 300 (the new fetch command) |

## Structural cause

Class 16 ([STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md), artifacts at paths no run owns), in a ref
store instead of a file system: the dispatcher wrote a namespace (`refs/remotes/origin/*`) that the
agents it starts also write, and assumed it was its only writer. The same shape as 179 and 191: a
location derived from a shared default instead of from the process that writes and reads it.

## Structural owner

The work-queue dispatcher (`scripts/queue/dispatch.py`, `REMOTE_NS`), under R-0543 (validation under
load); `check_work_queue.sh` enforces that the dispatcher's branch fetch writes only its namespace
(after a tick, no `refs/remotes/` entry has moved or been pruned, and the source checkout follows
`refs/queue/remote/`, not the agents' `refs/remotes/origin/main`). Remaining
exposure of the same shape, not observed: readiness runs `scripts/land/notes.sh fetch` in the queue
clone, which writes `refs/notes/remote/concrete-*`, a namespace the landing scripts an agent runs
also write. A failed notes fetch there is ignored rather than skipping the tick, and the read then
sees the newer local value, so it cannot skip dispatch; moving it into the dispatcher's namespace
belongs to the landing bot's notes contract (`notes.sh`).

## Containment while open

None needed: fixed on the branch above.
