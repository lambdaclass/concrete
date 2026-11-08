# Landing trains

Status: operational procedure (ROADMAP R-0539, measurement R-0547)

Ready branches land together as one **train**: one integrated candidate, one CI run, and
bisection by member only when that run is red. `scripts/land/train.sh` does what an orchestrator
used to do by hand, and refuses the mistakes that hand-run trains made on 2026-10-22.

## The cycle

```sh
scripts/land/train.sh build  <name> <ref>...   # train/<name> = origin/main + each ref, --no-ff, in order
scripts/land/train.sh check  <name>            # fast surface + impact-selected gates on the gate host (+ remote full set)
scripts/land/train.sh ci     <name>            # push ci/<name>, dispatch CI, record the run id
scripts/land/train.sh watch  <name>            # poll to a verdict; failing jobs' log tails
scripts/land/train.sh land   <name>            # verify, then push the exact SHA to origin and lambdaclass
scripts/land/report.sh                         # median time to land, per-stage medians
```

`--dry-run` before the subcommand prints pushes, dispatches and the remote run instead of doing
them. `land --dry-run` still performs every verification, so it is the way to ask "would this land?".
`train.sh status <name>` prints the train's state and log.

**Where a train runs.** Only in a full clone (`git clone`) whose pre-push hook is the repository's
(`git config core.hooksPath .githooks`, or a wrapper hook that runs `.githooks/pre-push`, like
`acceptance.sh`'s guard). Every subcommand except `status` and `record` (which push nothing) refuses a linked worktree and a clone
with the hook off, and the hook itself refuses a gated push (anything beyond the `wip/*` and review-
notes tiers) from a linked worktree. The reason is ISS-0015: git hands a worktree's hooks
`GIT_DIR=<repo>/.git/worktrees/<name>`, and a check run under it once wrote its sandbox's branches
and test notes into a real clone and onto GitHub. Scripts that make throwaway repositories clear
git's repository variables through `scripts/checks/lib/git_sandbox.sh` (checked by
`scripts/checks/sandbox_isolation.sh`), so the refusals are a second wall, not the only one.

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
  The root hook writes no validation record, so the `ci` push runs its own checks again (the
  full root set); this leg is the early verdict, not a substitute for the push. With a gate host configured, this leg runs **there**
  (see [Gate host](#gate-host)).
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
- **review** (fails closed): the leg passes at once when the train already has **review
  provenance**: a clean review note at its head or, when the head has no note, a clean note on every
  member at its exact recorded SHA (each member was reviewed at its own head). Each note is judged by
  `scripts/review/verify.sh` on origin's notes; an absent, malformed, blocked or findings note
  qualifies nothing, and a head note that is not clean is not overridden by the members. Otherwise
  the leg runs `LAND_REVIEW_CMD`, else `scripts/review/review.sh`, with `--base --head`, and then
  requires provenance: a non-zero exit fails, and so does a zero exit that leaves no clean note at the
  head. With no review command at all, `check` fails; it never disables the leg silently.
  `--no-review` skips the leg explicitly, and `land` still requires review provenance. A review that refuses a patch recording an owner
  decision is answered in the review ([REVIEW.md](REVIEW.md#owner-decisions-choosing-versus-recording)),
  never by `--no-review`.
- The per-leg durations, the wall time and their sum go to the log as a `check-legs` row.
- Runs inside `nix develop` unless already in the dev shell. `LAND_NO_NIX=1` disables this.

### ci

- Refuses unless `check` passed at this exact HEAD. Each `check` run first withdraws the previous
  pass, so a failed re-check leaves no pass behind. `--skip-check` overrides that, and the log
  records the check as `skipped`.
- Pushes in two steps:
  1. `ci/<name>` is reset to the train's base. The base is main's own commit and has already been
     published, so this push skips the hook.
  2. The train is pushed on top with the hook running. The root hook builds the spec and runs
     the full root set; it reuses no record written by `check`. The push is verified (see [Verified pushes](#verified-pushes)).

  Without the first step, a new branch has no remote base and the hook would select gates by
  diffing the whole tree.
- Dispatches `gh workflow run CI -R unbalancedparentheses/concrete2 --ref ci/<name> -f
  base=<base>`. The base-sensitive checks compare with the train's base, and the run's title records
  it (`ci/<name> at base <sha>`), which main's Landing record requires (see `land`). It finds the
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
- the mirror's main is an ancestor of the train, so no half-landing happens;
- the train has review provenance (as in `check`'s review leg), read again from origin's notes
  rather than trusted from `check`: a check run with `--no-review` lands only on a clean note at the
  train head or, when the head has none, clean notes on every member;
- a legacy/ move candidate (a change under `legacy/`, a `legacy` that is not a directory, or a change
  to the set of top-level entries `scripts/checks/root_inventory.txt` lists; comments, order and
  nested `docs/...` or `scripts/...` entries do not count) has an accepted rehearsal of its exact
  tree on its base (`scripts/land/acceptance.sh`; the move gate in `train.sh`).

If main moved, the command exits 4 and prints the rebuild command. The result described a
candidate that no longer integrates main, so the train is rebuilt, checked and run again.

The push sequence:

1. Pushes the exact SHA to `origin main` through the hook, which runs the full root set again. main
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
3. Checks both remotes with `ls-remote` once more; the landing is complete only when both are at
   the head.
4. Cancels nothing. The push to main starts CI on main at the landed SHA, and that run's first job,
   **Landing record**, runs `train.sh record <head> --base <before>` (R-0570). It finds the newest
   completed `workflow_dispatch` run of CI at exactly that SHA whose title says it ran at base
   `<before>`: `ci` dispatches with `-f base=<train base>`, the workflow's `run-name` writes
   `<ref> at base <sha>`, and the base-sensitive checks (`LL1_BASE`, `TOTALITY_BASE`,
   `CONFORMANCE_BASE`, `SPEC_BASE`) compare with that base as a push to main compares with the
   previous main. It checks that run under `land`'s own rule: every landing-tier job and every
   required job succeeded, with the required set computed from the SHA main had before the push,
   and the head strictly ahead of it. When the record holds, main's run skips every other job and concludes success, and
   its summary links the qualifying run. When it does not (a hand push, a run at another SHA, a
   skipped, cancelled or failed job, an unreadable API), nothing is skipped and main's run runs
   every job itself. So main shows green only on a result at its exact SHA, and a landing still
   costs one full run. The mirror's CI reads the record from this repository, which both remotes
   share commit for commit. Confirm with `gh run list --workflow CI --branch main`.

#### Verified pushes

A push's exit status is not its result. GitHub drops the connection during a long pre-push hook, so
a push can fail after the remote moved, or before. The train's push to `ci/<name>` and both pushes
of `land` are followed by an `ls-remote` of the ref: at the head, it is done, whatever git reported; still at its old value, it is
retried (`LAND_PUSH_ATTEMPTS`, default 3, `LAND_PUSH_RETRY_SLEEP` seconds apart); moved to anything
else, it stops at once. A retry to main is still a plain push, a fast-forward or a refusal, and the
hook runs the full root set again. The reset of `ci/<name>` to the base skips the hook and is
not retried; a failure there stops `ci` before anything is dispatched.

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

### Gate host

Heavy legs run on the CI server, not on the machine that pushes. The pushing machine keeps the
credentials, and the gate host (concrete-ci-2) does the compute. The gate host is `LAND_GATES_HOST`
when set (empty means none), else `git config land.gatesHost`. Set it once per clone, for example
`git config land.gatesHost concrete-ci-2`, where `concrete-ci-2` is an ssh destination from
`~/.ssh/config`.

With a gate host, `check`'s impact leg is `scripts/land/gates_remote.sh`:

1. It bundles `base..head` under a temporary ref.
2. The transport (`scripts/land/gates_transport_ssh.sh`) copies one directory to the host: the
   bundle, this checkout's `run-at-ref.sh`, and a script that quotes the command for bash. The
   host's login shell sees only `bash land-remote-<id>/run.sh`. That script runs `git fetch origin`
   in the host's gate clone (`~/concrete2`), runs `run-at-ref.sh` on the bundle, and removes the
   directory. The host therefore needs no GitHub credentials and no copy of the landing scripts.
3. In a fresh clone of exactly the head, the host runs `impact_gates.sh`: the same hook, with the
   same input line the `ci` push will present. The command first unsets `CONCRETE_SKIP_GATES`
   (whatever the host's environment holds) and the hook's re-entry marker. Then it exports this
   checkout's `CONCRETE_*` variables, NUL-delimited, with validated names and `%q`-quoted values.

The verdict comes from the log, bound to the SHA. The log must hold exactly one start line and one
exit line from `run-at-ref.sh`, and both must name the head.

| result | exit | effect |
|---|---|---|
| passed at the head | 0 | the leg passes |
| gates failed at the head | 1 | the check fails |
| run at another SHA, unreachable host, lost connection, log missing either line | 3 | **INCOMPLETE**: the check fails and the log records `check incomplete` |

INCOMPLETE is never a pass. It does not fall back to running the gates here. With a gate host
configured, running them on this machine takes the explicit `train.sh check <name> --local-gates`.
A clone with no gate host (CI, the landing gate's sandbox, a clone on the gate host itself)
runs them here, says so, and logs `impact here`.

**Reuse at the `ci` push.** The root hook (`.githooks/pre-push`) writes no validation record and
never consults an attestation, so at the root no push skips its checks. What follows describes the
mechanism for a hook that writes one (the archived legacy hook): when the host's hook wrote a validation record, the log carries it
between `=== concrete-remote-record-begin` / `-end` markers. On a pass, the train worktree keeps two
files in its git directory:

- the log, at `concrete-prepush-remote-record.log`;
- an attestation, at `concrete-prepush-remote-record`.

The attestation records the head, the host, the log's digest and `complete=1`.

The pre-push hook stays the enforcement point. It skips its own run **only** when all of these hold:

- the attestation is complete and names this HEAD;
- the log's digest matches the one attested;
- the log shows exit 0 at this SHA;
- the embedded record is complete and a pass;
- every host-independent component equals this push's. These are the commit and every pushed
  ref, `lean-toolchain`, the committed gate definitions, the resolved selection, the changed-file
  list, the fuzz seed and the `CONCRETE_*` environment. The tracked tree must also be clean.

Host-specific components (compiler binary, lean, cc) describe the host and are not compared. If any
check fails, the hook names the reason and runs the gates here. If the host passed but wrote no
record, the leg passes and says that the push will run the gates locally.

Each run of `gates_remote.sh`, and each `check`, first removes the previous attestation. The attestation and its log are files in the git
directory, like the local record. They catch mismatches, truncation and digest-breaking edits. They
are not a signature against someone who can write the git directory.

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
- **The common directory is shared by every worktree.** `report.sh` and `train.sh status` see every
  train of the clone from any of its worktrees; building, checking and landing run only in the full
  clone itself ("Where a train runs").

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
| `refs/notes/concrete-review` | `verdict: clean\|findings\|blocked`, `rounds: N`, `reviewer: codex`, `utc: ...`, `base: <sha>`, `pieces: N` | `scripts/review/review.sh` (`scripts/review/verify.sh` reads it) |
| `refs/notes/concrete-gates` | `gates: pass\|fail`, `selected: N`, `utc: ...` | `scripts/land/ready.sh` |

One `key: value` per line; when a key repeats, the last line wins. `scripts/land/notes.sh` reads and
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

## Configuration
| variable | default | meaning |
|---|---|---|
| `LAND_REPO` | `unbalancedparentheses/concrete2` | repository for `gh` |
| `LAND_REMOTE` | `origin` | primary remote |
| `LAND_MIRROR` | `lambdaclass` | required mirror, pushed after the primary; a missing remote is refused; empty (or `--no-mirror`) skips it deliberately |
| `LAND_CAMPAIGN_JOBS` | `^(Gate mutation\|Weekly clean-checkout proof replay\|Nightly differential fuzz\|Checker pin freshness \(campaign finding\))` | job names in the campaign tier; the default must match exactly the workflow's campaign jobs (`check_landing_script.sh`) |
| `LAND_STATE_DIR`, `LAND_LOG_DIR` | under the git common directory | state and log location |
| `LAND_NO_NIX` | unset | `1` never re-enters `nix develop` |
| `LAND_GH`, `LAND_HOOK` | `gh`, `.githooks/pre-push` | test seams |
| `LAND_GATES_HOST` | `git config land.gatesHost` | gate host (ssh destination) for `check`'s impact leg; empty = none; `check --local-gates` runs here instead |
| `LAND_GATES_TRANSPORT` | `scripts/land/gates_transport_ssh.sh` | `<transport> <host> <bundle> <ref> <command>`, printing `run-at-ref.sh`'s output; `LAND_GATES_SSH`/`LAND_GATES_SCP` override its ssh and scp |
| `LAND_REMOTE_RUNNER` | `~/bin/concrete-remote` | `<runner> <ref> <command>` for the remote leg; empty disables it |
| `LAND_REMOTE_PUSH` | `1` | `0`: the runner fetches the SHA from this clone, nothing is published |
| `LAND_FULL_GATES_CMD` | `lake build && bash scripts/checks/run.sh` | the remote leg's command |
| `LAND_REVIEW_CMD` | `scripts/review/review.sh` when present | the review leg's command (`--base --head` appended); with neither, `check` fails |
| `LAND_PUSH_ATTEMPTS`, `LAND_PUSH_RETRY_SLEEP` | `3`, `15` | attempts and pause for a verified push |
## Tests

`scripts/checks/landing_script.sh` runs in CI's build-free grammar job. It drives the real
script against a throwaway repository with bare-repo remotes and a fake `gh`. It covers:

- every resolution route;
- every refusal: a real conflict, prose next to a generated block, a duplicate corpus key, a merge
  or rebase already in progress, a dirty tree, an existing train, a head-SHA mismatch, a red
  landing job, a moved main and a remote named `fork`;
- the check → ci → watch → land pipeline, with both dry-runs;
- `ci` dispatches with the train's base; `land` cancels no run; `record` names the qualifying run
  only for the newest completed dispatch of CI at exactly the head and the base with every
  landing-tier and required job green, and refuses each of: no run, a run at another SHA or base, a
  push, pull-request or base-less dispatch, another workflow, an unfinished run or the asking run
  itself, a skipped, cancelled or failed job, an older green run behind a newer red one, a head not
  strictly ahead of the base, an absent or all-zero base and unreadable APIs; and the workflow gates
  every landing-tier job on the record and every base-sensitive check on the dispatch's base (with
  controls for an ungated job, a dependent forced by `always()`, a gate without `!cancelled()` and
  a check that ignores the base);
- verified pushes: a push dropped before the remote moved is retried, one dropped after it moved is
  accepted from `ls-remote`, and exhausted retries fail without moving main;
- the gate host: a result bound to the train head's SHA. A run at another SHA, an unreachable
  host and a lost log are each INCOMPLETE and block. A failure at the head fails. `--local-gates`
  and `land.gatesHost` work as described. `run-at-ref.sh` runs a bundled head. The hook's library
  reuses an attestation only for this exact push and refuses each of the following: another SHA,
  changed files, selection or environment, a dirty tree, a tampered or re-digested log, a tampered
  or truncated attestation, and a log that is not a pass;
- bisection;
- the report.

`scripts/checks/landing_ready.sh` runs in the same job. It drives the real `ready.sh` and
`train.sh` against a throwaway repository with bare-repo remotes, a fake `gh`, a fake remote runner
and a fake hook that fails exactly when the checkout contains `BAD`. It covers:

- the remote leg: an existing ref at the candidate's SHA is refused, not adopted; the runner runs
  once, on a fresh `wip/land-*` ref, and only the run's own ref is deleted;
- `bisect --ci`: a member whose CI is red is named, each probe dispatching its own run, and nothing
  lands;
- `ready.sh`: a failed review leg publishing `gates: fail` over an older clean verdict, and
  passing legs making a head ready;
- a static check that nothing under `scripts/land` force-pushes main.
