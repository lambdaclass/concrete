# Landing trains

Status: operational procedure (ROADMAP R-0539, measurement R-0547)

Ready branches land together as one **train**: one integrated candidate, one CI run, and
bisection by member only when that run is red. `scripts/land/train.sh` does what an orchestrator
used to do by hand, and refuses the mistakes that hand-run trains made on 2026-10-22.

## The cycle

```sh
scripts/land/train.sh build  <name> <ref>...   # train/<name> = origin/main + each ref, --no-ff, in order
scripts/land/train.sh check  <name>            # fast surface + impact-selected gates (+ remote full set)
scripts/land/train.sh ci     <name>            # push ci/<name>, dispatch CI, record the run id
scripts/land/train.sh watch  <name>            # poll to a verdict; failing jobs' log tails
scripts/land/train.sh land   <name>            # verify, then push the exact SHA to origin and lambdaclass
scripts/land/report.sh                         # median time to land, per-stage medians
```

`--dry-run` before the subcommand prints pushes, dispatches and the remote run instead of doing
them. `land --dry-run` still performs every verification, so it is the way to ask "would this land?".
`train.sh status <name>` prints the train's state and log.

### build

- Creates `train/<name>` from `origin/main` (after a fetch) and merges each ref with `--no-ff`.
  The merge message names the member and its SHA. A ref that is already in main is skipped.
- `--force` rebuilds an existing train. That is the normal response to a moved main.
- Conflicts are resolved mechanically only in three kinds of file
  (`scripts/land/resolve_conflicts.py`):

  | kind | files | resolution |
  |---|---|---|
  | append-only ledgers | `CHANGELOG.md`, `scripts/tests/audit_bug_corpus.sh`, `docs/bugs/discovery.tsv`, `docs/project/candidates.tsv` | union of both sides' lines, only when both sides inserted lines (the diff3 base of every hunk is empty); in the corpus, only map entries inside a `declare -A` map |
  | whole generated files | `Concrete/BuildIdentity.lean` | regenerated on the train |
  | generated blocks | any file whose every conflict hunk is inside a `BEGIN GENERATED ... (scripts/gen/<generator>; ...)` block whose generator the train will run | regenerated on the train |

  A generated view is resolved only when its generator is in the regeneration plan that runs after
  the last merge. A generator that merely exists is not enough. Any other conflict, including prose
  next to a generated block and two edits of one ledger line, aborts that merge. The script
  names the files and exits 3. The train keeps the members merged before it.
- After every merge, `resolve_conflicts.py corpus-keys` refuses a duplicate key in the corpus maps,
  and a bug listed in both `BUG_TEST_MAP` and `SKIP_BUGS`. bash keeps the last of two equal keys
  without a word, and a union merge left stale OPEN entries for fixed bugs on 2026-10-22.
- After the last merge the generated views are regenerated once:
  `scripts/gen/refresh_generated.sh` when it exists (branch `ledgers`, R-0542), otherwise
  `bug_dashboard.py --write`, `candidate_table.py --write` and `build_identity.sh`, each when present.
  Any change becomes one `regenerate generated views` commit. A generator's refusal stops the build.
- After every git operation the script checks for `MERGE_HEAD`, `CHERRY_PICK_HEAD`, `REVERT_HEAD`
  and a rebase in progress, and stops if one exists. It never continues past a stopped operation.
  A dirty tree is refused before anything starts.

### check

`check` runs up to three independent **legs at the same time** and collects every one before the
verdict; any failing leg fails the check (see [Pipelining](#pipelining)).

- **impact** (`scripts/land/impact_gates.sh`): runs the pre-push hook's own selection (the R-0022 fast surface gates plus the touched areas)
  over exactly the range the `ci` push will present: `train/<name>` at `base..head` on `ci/<name>`.
  The hook therefore writes its validation record for that range, and the `ci` push reuses it
  instead of running the gates again.
- If `scripts/tests/select_gates.sh` (R-0513) exists and the hook does not already call it, its
  output is run as well. The interface is `select_gates.sh --base <sha> --head <sha>`, printing one
  gate command per line, which runs through `run_ci_gates_local.sh --first`.
- **remote**: the full gate set (R-0541) through `LAND_REMOTE_RUNNER <ref> <command>`. The
  default runner is `~/bin/concrete-remote`; on the server it is `scripts/land/run-at-ref.sh`, which
  runs in a fresh `clone --local`, not a worktree, because some gates refuse linked worktrees. A
  runner that fetches from origin first gets the head published under a ref unique to that run,
  `wip/land-<name>-<sha>-<pid>-<n>` (the hook's backup tier). It is created only if absent. The
  push carries an empty lease, and ownership is taken only from `--porcelain`'s `*` (new ref)
  line, because git reports an existing ref at the same SHA as up to date before it checks the
  lease. On a collision the run tries a fresh name. The ref is deleted afterwards with a lease on
  that SHA, in one attempt that is never retried. A failed push may still have deleted it, so an
  unconfirmed deletion is reported and left alone. An EXIT backstop covers interruption before
  that attempt. As a result,
  concurrent checks never share or delete each other's refs. The run counts only if the runner reports
  `sha=<head>`. `--no-remote` skips this leg.
- **review**: `LAND_REVIEW_CMD`, else `scripts/review/review.sh`, called with `--base --head`.
  Exit 0 means clean, and a published `findings` note at the head fails the leg. `--no-review`
  skips it. The landing bot skips it on trains, because every member was reviewed at its own head.
- The per-leg durations, the wall time and their sum go to the log as a `check-legs` row.
- Runs inside `nix develop` unless already in the dev shell. `LAND_NO_NIX=1` disables this.

### ci

- Refuses unless `check` passed at this exact HEAD. Each `check` run first withdraws the previous
  pass, so a failed re-check leaves no pass behind. `--skip-check` overrides that, and the log
  records the check as `skipped`.
- Pushes in two steps:
  1. `ci/<name>` is reset to the train's base. The base is main's own commit and has already been
     published, so this push skips the hook.
  2. The train is pushed on top with the hook running. Its range is `base..head`, so the record
     written by `check` is reused.

  Without the first step, a new branch has no remote base and the hook would select gates by
  diffing the whole tree.
- Dispatches `gh workflow run CI -R unbalancedparentheses/concrete2 --ref ci/<name>`. It finds the
  run whose head SHA is the train's, records the run id and prints the run URL.

### watch

Polls until the run completes. A timeout is reported as `incomplete`, never as a result. The
command then lists every job that did not succeed, by tier, and prints the log tail of each failed
job (`gh api repos/<repo>/actions/jobs/<id>/logs`).

A run at another SHA, or a run with no landing-tier jobs, is no verdict: `watch` exits 1 and logs
`refused`.

**Campaign-only failure.** The campaign tier is the mutation campaign, the fuzz campaign and the
proof replay (`LAND_CAMPAIGN_JOBS`). When it is the only part that failed, `watch` says so and
exits 0: the campaign gates stage (c) qualification, not merges. The failure is still printed and
logged.

### land

`land` takes the same repository-wide lock as `push-both.sh` first, then checks every condition below
before anything is pushed:

- the CI run's `headSha` is the local HEAD, and the run is completed;
- every landing-tier job concluded `success`, and a run with no landing-tier jobs is refused;
- `origin/main`, read with `ls-remote`, is still the train's base;
- the mirror's main is an ancestor of the train, so no half-landing happens.

If main moved, the command exits 4 and prints the rebuild command. The result described a
candidate that no longer integrates main, so the train is rebuilt, checked and run again.

The push sequence:

1. Pushes the exact SHA to `origin main` through the hook, which reuses the `check` record. main
   is **never force-pushed**, not even with a lease. A plain push is a fast-forward or a refusal,
   so a publisher that moved main without taking the lock makes the push fail. Nothing it pushed
   is undone.
2. Pushes the same SHA to `lambdaclass main`. The mirror's main was checked to be an ancestor of
   the train, so the push is a fast-forward. Both remotes are required (the owner's decision of
   2026-10-22, which supersedes the 2026-07-31 "stop pushing to lambdaclass" note in
   `push-both.sh`): a missing `lambdaclass` remote is refused before anything is pushed. Following
   `push-both.sh` policy, this push runs with the hook
   skipped, because the identical commit was just accepted by the primary's hook. If it fails after
   the primary landed, the command says so and exits non-zero.
3. Checks both remotes with `ls-remote`.

A remote named `fork` is refused as primary or mirror. `--no-mirror` (or `LAND_MIRROR=`) lands on
origin only. That is a deliberate exception, and the landing is not complete until the same SHA
reaches `lambdaclass`. `push-both.sh` follows the same rule: it mirrors to `lambdaclass` by default.

### bisect

```sh
scripts/land/train.sh bisect <name> --test '<command that fails on the red train>'
scripts/land/train.sh bisect <name> --ci        # each subset through check, ci and watch
```

Halves the member list until one member remains. Each subset is its own train (`<name>-b<k>`),
built from the members' recorded SHAs. Its result is logged against that train's head. The suspect
is then built alone. If it is green alone, the failure is an interaction between members and the
report says so. The command prints the rebuild without the culprit. A member removed by bisection
keeps the failure attributed to it.

## Where the record lives (R-0547)

Every subcommand appends one row to the train's log:

- the time in UTC;
- the train, stage, base and head;
- the members, as `ref@sha`;
- the run id, result and duration;
- a note.

Results use the promotion pipeline's vocabulary:

- `passed`, `failed` and `skipped`;
- `incomplete` for no verdict;
- `refused` and `conflict` for a refusal by the script.

**Decision: the log is kept local, one file per train, in the git common directory**
(`<git-common-dir>/concrete-landing/log/<train>.tsv`). Train state lives next to it, in
`<train>/train.env`. The reasons:

- **Committing it to the train changes the candidate.** A row appended to the train after CI ran
  would land a SHA that CI never validated. The `land` row cannot exist before the land happens.
- **One shared TSV conflicts.** Every train would append to the same file, which recreates the
  ledger conflicts R-0542 removes. One file per train cannot conflict.
- **The common directory is shared by every worktree.** Any worktree can watch or land a train
  built in another, and `report.sh` sees all of them.

To publish the record, `report.sh --export docs/project/landing_log/` copies the per-train files
into the tree for a documentation-only commit. Per-train files never conflict on rebase. That
directory is what R-0547's generated median, next to the candidate table, will read.

## Readiness

A branch is **ready** when all of the following hold:

- it is on origin, named `wip/<name>`;
- its head commit is not in main;
- its head carries both notes below, published on origin.

| notes ref | lines | writer |
|---|---|---|
| `refs/notes/concrete-review` | `verdict: clean\|findings\|blocked`, `rounds: N`, `reviewer: codex`, `utc: ...`, `base: <sha>`, `pieces: N` (and `carried-from: <sha>` on a verdict the bot carried across a rebase) | `scripts/review/review.sh` (`scripts/review/verify.sh` reads it) |
| `refs/notes/concrete-gates` | `gates: pass\|fail`, `selected: N`, `utc: ...` | `scripts/land/ready.sh`, the bot |

One `key: value` per line; when a key repeats, the last line wins. Either note may carry
`order: N` to place the branch explicitly (lower first). `scripts/land/notes.sh` reads and
writes the notes (`get`, `put`, `ready`). `put` builds each write on its own temporary ref on top
of the remote's notes tip, pushes that commit without force, and retries on a race. Concurrent
writers therefore never drop each other's notes, including writers that share one clone or its
worktrees.

**Decision: candidates are `wip/*` branches whose head carries both notes. There is no `ready/*`
prefix.** The reasons:

- **A note is bound to a commit.** A new push starts with no notes, so "ready at the exact head"
  needs no extra rule, and no ready branch can go stale.
- **Agents already push `wip/*`.** The pre-push hook's backup tier keeps those pushes cheap, and
  marking a branch ready is a metadata write, not another branch to keep in step.
- **The ready list is a query**, so nothing has to be cleaned up after a landing.

`wip/land-*` is reserved for the remote leg's temporary refs and is never a candidate.

To mark a branch ready, push it, then run `scripts/land/ready.sh wip/<name>` from a clean
checkout at its head. It runs the review, the impact gates and the remote full set in parallel,
then publishes the gates note: `gates: pass` only when every leg that ran passed. A failed review
leg also publishes `gates: fail`, so an older clean verdict cannot keep the head ready. It exits 0
when the head is ready.

## The landing bot

`scripts/land/bot.sh tick` does one pass of the whole loop. On the server, a systemd user timer runs
it every 10 minutes after the previous tick ends (`scripts/land/systemd/`).

1. **Resume.** If the state file says a train is in CI, the bot watches that run instead of
   dispatching again. The phase `dispatching` is written, with its time, before the dispatch. A
   restart in that phase, or a dispatch whose run could not be found, makes the bot look up the
   run at the train's head SHA:
   - a failed lookup (network, auth, bad JSON) is no answer, and the bot asks again next tick;
   - an empty answer within `BOT_DISPATCH_GRACE` (900 s) of the dispatch waits;
   - only an empty answer after that discards the train and dispatches again. A train interrupted while it was being built or checked is discarded and
   rebuilt. A `blocked` state waits for a human (`bot.sh status`, then `bot.sh reset`).
2. **Auto-rebase** (below).
3. **Collect and order.** It lists the ready heads (`bot.sh list` shows them with their keys):
   - every explicit `order: N` comes first, whatever its value, in decimal order (`08` is 8);
   - then **language-rule branches**. A rule branch touches `Concrete/Check/` or
     `Concrete/Elab/` (CoreCheck lives in Check) and adds an `E` code that main's `Concrete/`
     does not contain;
   - then everything else;
   - ties go to the smaller diff, then the name.

   Rule changes land first and small, so they break other branches' fixtures before those
   branches land rather than after.
4. **Train.** The bot pins each member's SHA (`refs/bot/<branch>`) and runs
   `train.sh build` → `check` → `ci` → `watch` → `land`. Landing pushes to origin and lambdaclass
   and never to a remote named `fork`.
5. **Red train.** It bisects and ejects the culprit:
   - a build conflict ejects the member that conflicted;
   - another build failure, or a red check, bisects with that same step as the test;
   - a red landing tier bisects with `--ci`.

   The culprit is recorded at its SHA (`gates: fail`, with the reason), the event is notified, and
   the bot retries with the rest. A culprit that is green on its own is still ejected, and the
   event says it is an interaction. A new push of that branch makes it eligible again. A watch with
   no verdict keeps the train in CI for the next tick. A moved main rebuilds the train. A failed
   land, or a bisection that names no culprit, blocks the bot, and the block survives later ticks
   until `bot.sh reset`.
6. At most `BOT_MAX_TRAINS` (4) trains run per tick.

**Safety.**

- **One instance.** A kernel lock on every platform. The bot opens `lock.flock` on a file
  descriptor and takes `flock(2)` on it, through `flock(1)` or, where that is missing (macOS),
  Python's `fcntl.flock`. The lock belongs to the open file, so it lasts as long as the bot and is
  released by the kernel on any exit, and nothing stale is ever left. A second instance exits 0.
- **State in a file.** `<git-common-dir>/concrete-landing/bot/bot.env` holds the phase, the train
  and the ejected heads. `events.tsv` beside it is the event log.
- **Kill switch.** `touch ~/.concrete-bot-paused` stops the bot before its next stage:
  - no tick starts;
  - no train is checked and no CI is dispatched;
  - a green train waits in CI without landing;
  - bisection reads `LAND_PAUSE_FILE` before and after every operation of every probe (build,
    test or check, dispatch, watch), whatever the operation returned, and once more before
    attribution. A probe that sees the switch gives no verdict and nothing is attributed. A red
    train in CI waits there;
  - the rebase pass stops before the next branch, and a gated rebase is not pushed.

  The switch is the file's presence, read between operations. Leave it until the bot has logged a
  `paused` event (`bot.sh status`, or the notification): the instance holding the lock writes it
  once per pause file, at the next stage it reaches. A file created and removed within one
  operation is not seen. Remove it to
  resume; the train is picked up where it stopped.
- **Dry run.** `--dry-run` prints the ready list, the order and the train, and changes nothing on
  the remotes or in the bot's state.
- **main.** main is pushed only by `train.sh land`, as a fast-forward.
- **Notification.** Every event calls `scripts/land/notify.sh <level> "landing bot: <event>" <message>` when that file
  exists (`BOT_NOTIFY` overrides). A notifier that fails never stops the bot.

**Running it.** The bot needs a **dedicated clone**, because it checks out trains there. The
launcher that `scripts/land/install-bot.sh` writes runs the bot from an immutable snapshot of
`scripts/land` at `BOT_SOURCE_REF` (default `origin/main`), so a checkout never rewrites the
script while it runs. On the server, the remote leg runs locally (`run-at-ref.sh`), fetching the
train SHA from the bot's clone. The bot also needs two credentials:

- `gh` authentication, to dispatch CI;
- write access to both repositories, to push main, `ci/*` and the notes.

```sh
scripts/land/install-bot.sh --ssh dev@<server> [--source-ref <ref>]
# then, when the maintainers decide (install-bot.sh never enables it):
sudo loginctl enable-linger dev                                   # once, as ubuntu
systemctl --user daemon-reload && systemctl --user enable --now concrete-landing-bot.timer   # as dev
```

## Auto-rebase

When main moves, every ready head that does not contain main is rebased onto it.

- **Skip rules.** A head with activity in the last `BOT_ACTIVE_SECONDS` (600) is never rebased;
  an agent may still be pushing to it. Activity is the latest of:
  - the head's commit time;
  - the time this clone first saw it (the remote-tracking reflog);
  - the notes' `utc`.

  A (head, main) pair is tried once. Branches outside `wip/` are never rewritten.
- **Procedure.** The rebase runs in a scratch worktree and is never piped. After it, the bot
  checks that no rebase or merge is still in progress. Then:
  1. `scripts/gen/refresh_generated.sh` runs when the tree has it (branch `ledgers`), and its
     result is committed.
  2. The impact gates run over `main..new-head`.
  3. The result is pushed to the same `wip/` branch with `--force-with-lease=<old head>`. If the
     agent pushed meanwhile, the push is refused and the branch is left alone. An agent whose
     branch was rebased resets its local branch to the new head before continuing.
- **Notes.** The new head gets a gates note and the **carried** review verdict. The carried note
  names the SHA that was reviewed (`carried-from:`). A carry happens only for a rebase without
  conflicts; generated-view commits are the only new content.
- **Conflict.** A real conflict aborts the rebase and is notified with the branch and the files.
  The branch is not touched.

## Pipelining

The review, the impact gates and the remote full set do not depend on each other. `ready.sh` and
`train.sh check` therefore start all three together and wait for all of them. The wall time
becomes the slowest leg instead of the sum. Both scripts record each leg's duration, the wall time
and the sum: `ready.sh` in `<git-common-dir>/concrete-landing/ready.tsv`, and `check` as the
`check-legs` row of the train log.

**Measured on 2026-10-22, on branch `wip/landing-bot`.** Two runs of `ready.sh`, each with its legs
started together:

| run | review (Codex) | impact | remote full set | wall | one after another |
|---|---|---|---|---|---|
| laptop, `f55935ac` | 117 s | 51 s | lost when the laptop lost its network | n/a | n/a |
| concrete-ci-1, `93aa7ac5` | not run (no Codex login for `dev`) | 69 s | 5239 s, red | 5239 s | 5308 s |

The server was shared with other full runs at a load average of 15 to 32.

- **Saving.** The impact gates (69 s) and the review (117 s, from the laptop run) finish inside the
  remote leg, so about 3 minutes come off every readiness check. Measured: 69 s with the impact
  leg alone, and 186 s with the review included.
- **What dominates.** The remote full set is about 98% of the cost: 87 minutes on a loaded host.
  Pipelining removes the queue in front of it, not the leg itself. The next lever is the full
  set's own time (R-0546 caches, R-0541 dedicated runners) and keeping concurrent full runs off
  the same host.
- **Why the remote leg was red.** Five gates failed:
  - `check_phase6c_observability.sh` #5 and `check_campaign_summary_golden.sh` refuse to run in a
    linked worktree ("Run it in a clone"), which is how that run was made. `run-at-ref.sh` now
    uses `clone --local`.
  - `check_example_outputs.sh` under valgrind, `run_tests.sh` and `check_external_checker.sh`
    were not examined.

  This branch changes only `scripts/land`, one build-free gate with its registration, and this
  document. CI on a train is where those three would be settled.
- **Not counted.** Before pipelining, a human orchestrator usually started the review, the impact
  gates and the remote set by hand in sequence, and each hand-off waited for someone to notice
  the previous result. The scripts remove that wait, and this table does not measure it.

## Watching trains

`scripts/status/status.py` renders the landing log, the `ci/*` runs in flight, both mains and the
ready branches as one phone-friendly page. `scripts/land/notify.sh <level> <title> <body>` pushes
a message to ntfy or Slack, falling back to stdout, and drops duplicates. Setup and the
publishing decision are in docs/project/STATUS_PAGE.md.

## Configuration
| variable | default | meaning |
|---|---|---|
| `LAND_REPO` | `unbalancedparentheses/concrete2` | repository for `gh` |
| `LAND_REMOTE` | `origin` | primary remote |
| `LAND_MIRROR` | `lambdaclass` | required mirror, pushed after the primary; a missing remote is refused; empty (or `--no-mirror`) skips it deliberately |
| `LAND_CAMPAIGN_JOBS` | `^(Gate mutation\|Weekly clean-checkout proof replay\|Nightly differential fuzz)` | job names in the campaign tier |
| `LAND_STATE_DIR`, `LAND_LOG_DIR` | under the git common directory | state and log location |
| `LAND_NO_NIX` | unset | `1` never re-enters `nix develop` |
| `LAND_GH`, `LAND_HOOK` | `gh`, `.githooks/pre-push` | test seams |
| `LAND_REMOTE_RUNNER` | `~/bin/concrete-remote` | `<runner> <ref> <command>` for the remote leg; empty disables it |
| `LAND_REMOTE_PUSH` | `1` | `0`: the runner fetches the SHA from this clone, nothing is published |
| `LAND_FULL_GATES_CMD` | `lake build && bash scripts/tests/run_ci_gates_local.sh` | the remote leg's command |
| `LAND_REVIEW_CMD` | `scripts/review/review.sh` when present | the review leg's command (`--base --head` appended) |
| `BOT_*` | see the header of `scripts/land/bot.sh` | the landing bot's state, pause file, prefix, intervals and limits |
## Tests

`scripts/tests/check_landing_script.sh` runs in CI's build-free grammar job. It drives the real
script against a throwaway repository with bare-repo remotes and a fake `gh`. It covers:

- every resolution route;
- every refusal: a real conflict, prose next to a generated block, a duplicate corpus key, a merge
  or rebase already in progress, a dirty tree, an existing train, a head-SHA mismatch, a red
  landing job, a moved main and a remote named `fork`;
- the check → ci → watch → land pipeline, with both dry-runs;
- bisection;
- the report.

`scripts/tests/check_landing_bot.sh` runs in the same job. It drives the real bot, and `train.sh`
under it, against a throwaway repository with bare-repo origin, lambdaclass and fork remotes. Its
fake `gh` reports CI red exactly when the candidate contains `CIRED`, and its fake hook fails
exactly when the checkout contains `BAD`. It covers:

- ready detection: a stale note, a `findings` verdict, a failed gate, `wip/land-*` and a branch
  outside `wip/` are not ready;
- ordering: explicit first (including `order: 0800`, above the old fixed keys), then a new E-code
  under Check/Elab; reusing an existing code is not a rule change;
- the dry run and the kill switch;
- the lock: a live holder of the kernel lock stops a second instance, and its release lets the
  next tick run;
- the remote leg on a per-run `wip/land-*` ref that is gone afterwards (with a control that the
  probe sees a planted ref), and an existing ref at the same SHA that is refused, not adopted;
- a four-member train landing in order on both remotes, with fork untouched;
- red-check and red-CI bisection that ejects exactly the culprit and lands the rest;
- the kill switch stopping a bisection before it starts, during a halving probe, during the red
  culprit probe, and between a `--ci` probe's check and its dispatch; a culprit-less bisection blocking the bot until `reset`;
- resuming a train held in CI without a second dispatch, and reconciling a `dispatching` restart
  with the run at the train's head; a failed lookup and an empty one within the grace period keep
  the train dispatching;
- `ready.sh`: a failed review leg publishing `gates: fail` over an older clean verdict, and
  passing legs making a head ready;
- the auto-rebase skip rules, a clean rebase with carried notes, and a conflict notified with its
  files that leaves the branch and the clone untouched;
- a static check that nothing under `scripts/land` force-pushes main.
