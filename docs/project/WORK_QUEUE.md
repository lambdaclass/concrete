# Agent work queue

The work queue lets agents run without a human orchestrator. A systemd timer on `concrete-ci-1`
runs one dispatcher tick every 5 minutes as user `agent`. Each tick rebuilds the queue from its
sources, watches the running agents, and starts headless Claude Code agents on the next ready tasks.
A finished agent leaves a branch with clean review and gate notes, and the landing bot
([LANDING.md](LANDING.md)) picks it up. The dispatcher never pushes to `main` or `ci/*`.

| file | role |
|---|---|
| `scripts/queue/build_queue.py` | sources → `queue.json` (generated, never edited) |
| `scripts/queue/dispatch.sh`, `dispatch.py` | the tick, `status` and `release <task>` |
| `scripts/queue/run_agent.sh` | runs one agent detached; writes its log and exit status |
| `scripts/queue/push_guard.sh` | the queue clone's pre-push hook |
| `scripts/queue/install-queue.sh` | installs the copy, the clone and the units; enables nothing |
| `tasks/*.md` | explicit tasks and dispositions of sourced tasks |
| `scripts/tests/check_work_queue.sh` | the gate (stub `claude` and `cswap`, throwaway repository) |

## Sources

`queue.json` is regenerated on every tick from the source checkout, which is the queue's own
worktree, detached at `origin/main`. Each fact has one owner:

- **Open bugs.** These are the `docs/bugs/NNN_*.md` records whose first Status line is OPEN,
  CONTAINED, Half closed or Partially fixed. This is the vocabulary that `scripts/gen/bug_ledger.py`
  reads. The prompt is `/fix-bug N`.
- **ROADMAP.** First come the rows of the development-throughput table (R-0539 – R-0547 and R-0513),
  in table order. The table's "depends on" column becomes dependencies. Then come the R-ids named in
  the execution queue's rows, in row order. A task body's `**Depends on:**` line adds dependencies,
  and text after `**Blocks:**` is ignored. The prompt is `/roadmap-item R-NNNN`.
- **Fuzz-farm findings.** These are the files in `$QUEUE_FUZZ_DIR/*.json`. The interface the fuzz
  farm writes is `{"id", "class", "summary", "areas"?, "owner"?, "module"?, "bug"?}`. Only the classes
  `silent`, `unsoundness` and `crash` are tasks. Refusals are not, and neither is a finding with
  `bug` set, because the promoted bug owns it. The prompt asks the agent to promote the finding with
  the farm's `promote.sh` and then follow `/fix-bug`.
- **Explicit tasks.** These are `tasks/*.md` files, in the format below.

### `tasks/*.md`

```
---
id: task-docs-sweep        # default: task-<file stem>; [A-Za-z0-9_.-]+
title: Sweep the glossary
tier: explicit             # throughput | silent | crash | explicit | roadmap | loud
areas: docs/GLOSSARY.md, R-0500, Lower
depends: R-0547, bug-136
disposition: in-flight     # in-flight | done | skip; omit to schedule
branch: wip/some-branch    # informational
---
The prompt for the agent (or `prompt: /some-command` above).
```

A file whose `id` names a sourced task (`R-0539`, `bug-136`, `fuzz-x`) does not create a task. It
amends the sourced one. `disposition` takes the task out of dispatch, which is how in-flight branches
are recorded. `areas`, `depends`, `tier` or a body override the derived values. An area written by
hand is a rule if it reads `R-NNNN`, a path if it contains `/` or `.`, and a compiler module
otherwise. A file with an unknown key, tier or disposition exits 2, and the tick writes nothing.

## Priority, dependencies and areas

Tiers are taken in this order. Inside a tier, tasks keep their source order: table order, bug number,
or file name.

1. **throughput**: the R-0539 – R-0547 track, which comes first by user priority.
2. **silent**: silent wrong code, unsound or false-proof claims and memory-safety bugs. The record's
   ledger `silent:` line decides when it has one. Otherwise its `**Severity:**` line decides.
   Fuzz findings of class silent or unsoundness also go here.
3. **crash**: crashes, panics and aborts, and fuzz findings of class crash.
4. **explicit**: `tasks/*.md` with no tier.
5. **roadmap**: the execution-queue rows.
6. **loud**: every other open bug. Under the ROADMAP's defect disposition rule, a loud defect waits
   for its structural owner, so these come after the roadmap.

A task is ready when none of its dependencies is still in the queue undone. The dispatcher's state or
a `done` disposition marks a dependency done. A dependency cycle is marked `cycle` in `queue.json`,
is never started, and is reported once as `dependency_cycle`.

**Touched areas.** Two tasks run together only when their area sets are disjoint. Areas are rules
(`rule:R-NNNN`), compiler modules (`mod:Lower`, named in the text), and paths (`path:...`). Two paths
conflict when one is a prefix of the other. The areas are derived as follows:

- **Bugs:** the "Structural cause" and "Structural owner" sections. The owner's R-ids are the rules,
  so two bugs owned by R-0500 never run at once. A record with neither section uses the whole record.
- **Roadmap tasks:** the task's own R-id, plus the modules and the non-`docs/` paths that its body
  names. Cited R-ids and documents are references, not areas.

A module whose name is also an ordinary word (`Core`, `Check`, `Report`, ...) counts only when it
appears as `` `Name` ``, as `Name.lean`, or as a path. A task with no derivable area gets `unknown`, so
unknown-area tasks run one at a time. A task that is `exited` holds its areas until it is done or
released, because its branch is not landed yet.

## The tick

Each tick runs these steps:

1. It takes an exclusive lock, fetches, re-detaches the source checkout at `origin/main`, and writes
   `queue.json`.
2. **Monitor.** For every running or exited task:
   - If the process has ended, it reads the exit status and the run's `result` event (cost, tokens).
     A usage limit releases the task. Any other nonzero exit is a failure.
   - A new SHA on `refs/heads/wip/queue/<task>` is a **heartbeat**.
   - If that SHA carries `verdict: clean` in `refs/notes/concrete-review` and `gates: pass` in
     `refs/notes/concrete-gates`, the task is **done**. This is the landing bot's readiness contract;
     `scripts/land/notes.sh ready` is used when the source checkout has it. The dispatcher notifies.
     It removes the worktree once the agent has exited and the worktree's HEAD is the published SHA.
   - If there has been no new push for `QUEUE_STUCK_MINUTES` (60) since the last push or the start,
     the task is **stuck**. The dispatcher kills the agent's process group, notifies, and releases
     the task.
3. **Kill switch.** While `~/.concrete-queue-paused` exists, nothing starts and monitoring continues.
   If the file's first line is `kill`, running agents are also stopped and their tasks are released
   without charge.
4. **Dispatch.** The tick walks the queue in order and skips:
   - disposed, cycled, active, done and abandoned tasks;
   - tasks with unmet dependencies;
   - tasks whose areas conflict with an active task.

   It starts each remaining task until the concurrency cap, a budget or the slot supply stops it. It
   refuses to start anything when the clone's push guard is missing.
5. It writes `state.json` and `status.json`.

A released task returns to the queue. A failure or a stuck release charges one attempt, and at
`QUEUE_MAX_ATTEMPTS` (2) the task becomes `abandoned` (notified; it needs a human). Clear the
abandoned state with `dispatch.sh release <task>`. A usage limit and the kill switch charge nothing.

### What an agent gets

Each task gets its own worktree, `~/.local/state/concrete-queue/worktrees/<task>`, and its own branch,
`queue/<task>`. The branch starts at `origin/wip/queue/<task>` when an earlier run published there,
and at `origin/main` otherwise. The prompt is the playbook command (`/fix-bug N`,
`/roadmap-item R-NNNN`). The playbooks own the working rules:

- a wip push every 5 minutes;
- `review.sh` and its notes;
- impact gates;
- the review cap;
- the structural cause and owner.

An appended system prompt states the task, the worktree, the wip ref, the stuck window, the done
condition and the push limits. The environment carries `QUEUE_TASK_ID`, `QUEUE_BRANCH`,
`QUEUE_WIP_REF`, `QUEUE_RUN_DIR` and `QUEUE_SLOT`. The command is:

```
cswap run <slot> -- claude -p "<playbook command>" --permission-mode dontAsk \
  --settings <run>/settings.json --output-format stream-json --verbose \
  --session-id <uuid> --append-system-prompt "<queue context>" [--max-budget-usd N] [--model M]
```

Its output goes to `runs/<task>.<n>/agent.log`, its exit status to `runs/<task>.<n>/exit`, and the
exact argv to `command.json`.

## Trust model

The agents run unattended with broad tools, so the boundary is placed where a misbehaving agent
cannot cross it. The permission list is not that boundary.

- **The security boundary is the Unix user `agent`.** It has no sudo, its home is mode 700, it
  belongs to its own group only, and it is separate from `dev` (the gate host) and `gha1..6` (the
  runners). An agent can damage only what `agent` owns: its own clones, worktrees and the queue
  state.
- **The queue clone (`~/queue/concrete2`) has `origin` only.** It has no `lambdaclass` remote and
  no `fork` remote. Its `core.hooksPath` is the **push guard**, which refuses every target except
  `refs/heads/wip/*` and the two readiness-note refs, and then runs the repository's own pre-push
  wip tier. The dispatcher starts nothing while the guard is missing. The interactive clone
  `~/concrete2` is never touched.
- **Permissions: `dontAsk` with an explicit allow list.** The allow list is Read, Edit, Write, Glob,
  Grep, NotebookEdit, TodoWrite, Task/Agent, Skill and Bash. Anything else is denied without a
  prompt, because no human is there to answer. The deny list wins over every allow, including
  allows in user settings. It covers:
  - WebFetch and WebSearch, because fetched text is a prompt-injection surface and the tasks need
    no web;
  - pushes to `main` and `ci/*`, force pushes, and the `fork` and `lambdaclass` remotes;
  - `--no-verify`, `CONCRETE_SKIP_GATES` and `core.hooksPath`;
  - `git remote add/set-url`, `git config --global`, `gh pr merge`, `gh api`, `sudo`, and
    `train.sh land`;
  - touching the pause file;
  - edits to `~/.claude`, `~/.ssh`, `~/.config`, `~/.local` and the shell profiles.

  These denies are guard rails against mistakes. They are not a sandbox: Bash can reach around a
  pattern.
- **Bypasses that remain.** A determined agent could rewrite its own hooks or push with a crafted
  command. Two things stop that from reaching `main`. First, the deploy key is the server's only
  GitHub credential. Second, **branch protection on `main`**, with the landing tier required, is
  the control that must hold, and enabling it is recommended before the timer is enabled. Merges are
  the landing bot's alone.
- **Credentials.** Claude logins live in cswap's slot store, and Codex uses `~/.codex/auth.json`.
  Agents run as the same user, so they can read them. That is the accepted cost of an agent that
  runs `codex exec` reviews.

## Budgets and rotation

All knobs are set in `~/.config/concrete-queue/env`.

| knob | default | meaning |
|---|---|---|
| `QUEUE_MAX_AGENTS` | 3 | concurrent running agents |
| `QUEUE_MAX_AGENT_HOURS` | 36 | agent wall-clock hours per UTC day (running runs count to now); 0 = off |
| `QUEUE_MAX_DAILY_USD` | 0 (off) | sum of `total_cost_usd` from the runs' `result` events that ended today |
| `QUEUE_MAX_DAILY_TOKENS` | 0 (off) | sum of the `*_tokens` usage fields of those runs |
| `QUEUE_TASK_MAX_USD` | unset | per-run cap, passed as `--max-budget-usd` |
| `QUEUE_SLOTS` | `1 2 3 4` | cswap account slots |
| `QUEUE_STUCK_MINUTES` | 60 | no wip push for this long releases the task |
| `QUEUE_MAX_ATTEMPTS` | 2 | charged failures before a task is abandoned |
| `QUEUE_FUZZ_DIR`, `QUEUE_STATUS_JSON`, `QUEUE_NOTIFY`, `QUEUE_MODEL` | unset | as named |

Cost and tokens become known only when a run ends, so a running agent counts toward agent-hours but
not toward cost. On subscription logins, `total_cost_usd` is Claude Code's API-price estimate. It is
not a bill.

**Account rotation.** A new agent goes to the slot with the fewest running agents, then the slot
least recently started, so work spreads across the four accounts. A run counts as a usage limit only
when the CLI's own error says so: a non-JSON stderr line, or the `result` event of an `is_error` run
matching `QUEUE_USAGE_LIMIT_RE`. Text inside tool output never counts. A usage limit has three
effects:

- the slot rests for `QUEUE_SLOT_COOLDOWN` (3600 s), so it is retried at most once per hour;
- the task is released uncharged;
- the task restarts on another free slot in the same tick, or waits when every slot is resting.

## State and visibility

These files live under `~/.local/state/concrete-queue/`:

- `queue.json` is the generated queue.
- `state.json` holds tasks, slots, runs and flags. Each task records its status, agent (session id),
  slot, pid, worktree, branch, wip ref, start time, last push and attempts.
- `events.log` is append-only JSON lines: `claimed`, `heartbeat`, `exited`, `done`, `stuck`,
  `usage_limit`, `released`, `worktree_removed`. Conditions are logged when they change: `paused`,
  `budget_exhausted`, `no_slot_available`, `push_guard_missing` and `dependency_cycle`, each with
  its `_cleared`.
- `status.json` feeds the status page. It holds the running tasks, the first 10 of the queue with
  their blocking reason, the budget used against its caps, the resting slots, and counts. It is
  copied to `QUEUE_STATUS_JSON` when that is set.
- `runs/<task>.<n>/` holds `agent.log`, `exit`, `command.json` and `settings.json`.

**Notifications** go to `QUEUE_NOTIFY`, or to `scripts/status/notify.sh` or `scripts/notify.sh` in
the source checkout. The call is `notify.sh "<title>" "<body>"`. The dispatcher notifies on:

- done;
- failure;
- stuck;
- usage limit;
- an abandoned task;
- each condition flag.

## Install and enable

Run as `agent` on the server:

```
bash ~/concrete2/scripts/queue/install-queue.sh      # copies the code, makes ~/queue/concrete2, writes units
```

The script enables nothing. Once Claude is logged in on every cswap slot (`cswap list` shows
credentials for 1 – 4), enable the timer:

```
sudo loginctl enable-linger agent                    # once, as ubuntu (agent has no sudo)
systemctl --user enable --now concrete-queue.timer   # as agent
```

To check on it, use `systemctl --user list-timers` and `~/.local/share/concrete-queue/bin/dispatch.sh status`.

## Interfaces with the other in-flight branches

- **Landing bot** (`wip/landing-bot`): readiness is the bot's `notes.sh ready` contract. The
  dispatcher only reads it.
- **Review tool** (`wip/review-tool`): `review.sh` writes the review note that the agents' playbooks
  run.
- **Playbooks** (`wip/playbooks`): the playbooks provide `/fix-bug`, `/roadmap-item` and
  `new_bug.sh`. Until they land on main, an agent receives the command text as its prompt.
- **Status page** (`wip/status-page`): the page reads `status.json` and is notified through
  `notify.sh`.
- **Fuzz farm** (`wip/fuzz-farm`): the farm writes the findings JSON and `promote.sh`.

`tasks/*.md` records in-flight branches as `disposition: in-flight`, so the queue does not start a
duplicate. When a branch lands, set its file to `done`.
