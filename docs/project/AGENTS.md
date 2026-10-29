# Agent and contributor workflow

Status: operational procedure. How work is done in this repository: the agent playbooks, the
rules they share, and [concurrent work](#concurrent-work-one-worktree-per-writer) (one worktree per
writer, the server gate runs, publishing). It absorbed `CONCURRENT_WORK.md` on 2026-10-29. The
tool manuals it points to stay separate: [LANDING.md](LANDING.md) (landing trains and the bot),
[REVIEW.md](REVIEW.md) (`scripts/review/review.sh`), [WORK_QUEUE.md](WORK_QUEUE.md) (the agent work
queue), [STATUS_PAGE.md](STATUS_PAGE.md) and [PROMOTION_PIPELINE.md](PROMOTION_PIPELINE.md) (the
validation stages, CI tiers and result vocabulary).

## Playbooks

Project playbooks live in `.claude/commands/`. Every agent session, on the laptop or the server,
runs the same procedure by name instead of receiving a long prompt:

| command | what it does |
|---|---|
| `/fix-bug <N or description>` | reproduce, classify, repair or contain one bug on its own branch |
| `/roadmap-item <R-NNNN>` | carry one roadmap task to a landable candidate |
| `/file-bug <title>` | file a new bug with `scripts/gen/new_bug.sh` |
| `/creview` | Codex review of the branch through `scripts/review/review.sh` |
| `/rebase` | the safe rebase onto `origin/main` |
| `/land` | hand a validated branch to the landing train (`scripts/land/train.sh`) |
| `/cstatus` | where this worktree, its branch, its CI and this machine stand |

`/creview` and `/cstatus` carry a `c` prefix so they do not shadow Claude Code's built-in `/review`
and `/status`; a new playbook takes a name no built-in uses.

This file owns the rules the commands share. A command links here rather than restating them, so a
rule changes in one place.

## Rules

**Owner decisions: never decide one.** The language decisions OD1 – OD22
([ROADMAP.md](../../ROADMAP.md#owner-decisions), exit 2n) are decided only by the project owner. No
agent, orchestrator or reviewer chooses, encodes, implements or provisionally ships any side of any
of them, inside a bug fix or anywhere else. Agents present options and a recommendation to the owner
and wait for an explicit answer. Behaviour already on main that took a side stays as it is, recorded
as a conflict, until the owner answers; nobody extends or reverts it meanwhile.
- The Codex review of every fix (`scripts/review/review.sh` asks it) also answers: "does this fix
  pick a side on any OD in ROADMAP exit 2n?" If yes, or if the fix cannot be made without picking
  one, stop and report to the owner; do not land it.

**Bugs.**
- Every bug gets a structural cause and a structural owner: the design fact that let it exist, and
  the roadmap task or pipeline stage that removes the whole class
  ([STRUCTURAL_CLASSES.md](../bugs/STRUCTURAL_CLASSES.md)). A patch without them is not a fix.
- To file a bug, run `scripts/gen/new_bug.sh`, which writes its record and runs the ledger
  generator. To log a change, add a `changelog.d/` fragment
  ([changelog.d/README.md](../../changelog.d/README.md)). Never hand-edit a generated view (the bug
  lists, the class table's bugs column, ROADMAP's generated spans, `CHANGELOG.md`,
  `Concrete/BuildIdentity.lean`); see [scripts/tests/README.md](../../scripts/tests/README.md),
  "Shared ledgers".
- Each bug's compiler area is derived from its fix commits ([AREAS.md](../bugs/AREAS.md);
  `python3 scripts/gen/bug_ledger.py report` for the stage, class, month, silent, how-found and
  time-to-fix tables, `--json` or `--sqlite PATH` for queries). Name the bug in the subject of each
  commit that fixes it (`bug NNN: ...`), and after committing a fix rerun
  `scripts/gen/refresh_generated.sh` and commit the regenerated AREAS.md. Add an `area:` line to a
  record only when the record shows the derived area is wrong
  ([README.md](../bugs/README.md#bug-areas)); never fill areas in by hand otherwise.
- A silent defect (wrong code, unsound claim) is stopped at once by repair or fail-closed
  containment; a loud one is filed, pinned and owned (the defect disposition rule in
  [ROADMAP.md](../../ROADMAP.md#the-current-execution-queue)).

**Branches.**
- One bug or one rule per branch, in its own worktree (`scripts/worktree-new.sh`,
  [concurrent work](#concurrent-work-one-worktree-per-writer)). This applies to all agent work now, not only once
  R-0545 closes. A branch that changes a language rule lands first and stays small; work that
  depends on it waits or rebases onto it.
- Rebase onto `origin/main` after every landing (`/rebase`), not only before your own landing, so a
  conflict or a changed rule shows up while the branch is small.
- Commit each validated slice. Push the branch to `wip/<branch>` at least every 5 minutes of work
  (`git push origin HEAD:refs/heads/wip/<branch>`); the hook's wip tier only scans for conflict
  markers.
- Never push to `main`, `ci/*` or the remote `fork`. Landings go to both `origin` and
  `lambdaclass`, only through the landing script (`scripts/land/train.sh`) or the landing bot.
  `train.sh check` runs the hook's gates before the push, so the push reuses that validation record;
  the train's push to `ci/<name>` and both pushes of `land` are read back with `ls-remote` and
  retried when they did not move the ref (GitHub drops the connection during long hooks); `land` checks that both remotes are at
  the head ([LANDING.md](LANDING.md#land)).
- After any push of your own whose hook ran gates, check that the remote ref moved
  (`git ls-remote origin refs/heads/<branch>`) and push again if it did not.

**Validation.**
- Locally, run only the impact-selected gates (pre-push runs the fast surface gates and the gates
  selected by what the push touches). For the full set, dispatch CI on the branch
  (`gh workflow run CI --ref <branch>`) or use the server (`concrete-remote wip/<branch> 'lake build
  && bash scripts/tests/run_ci_gates_local.sh'`); never a full local run on the laptop.
- At most 2 heavy runs (full suites, mutation campaigns, `lake build` of the whole tree) per machine.
  Queue behind a running one; continue light work meanwhile. Honour a user's explicit CPU limits.
- Run gates inside `nix develop`. A gate that passes by hand but fails under the hook: suspect an
  inherited `GIT_*` variable first.
- Record each result against the exact commit SHA it ran on.
- Read the obligation, not the prose: a header says what was true when it was written. Make gates
  assert properties, not counts, so an improvement does not read as a regression.

**Before asking for review.** Each of these caused an integration failure in the week to
2026-10-25; run them on the slice, in `nix develop`, before `/creview`:
- `scripts/tests/test_golden.sh` after any change to compiler output or a report.
- `scripts/tests/test_mutation.sh --check-patterns` after editing any file a mutation targets: it
  finds an anchor the edit broke in seconds, before a mutation campaign fails on it.
- `scripts/tests/check_type_identity.sh` and the other gates that elaborate Lean probes against the
  compiler (`grep -l 'lake env lean' scripts/tests/check_*.sh`) after changing a signature or a
  structure they construct.
- A fixture's `run_ok` / `run_err` leg in `run_tests.sh` spells its file name literally: the
  bug-corpus audit looks for each fixture's basename in `run_tests.sh`, so a name assembled from
  parts is reported as uncovered.
- Bound a command with `bounded_run` (`scripts/tests/lib/bounded.sh`), never GNU `timeout`: the
  macOS job has no `timeout`, and `bounded_run` reports a second timeout as INCOMPLETE, not a pass.
- A ledger `coverage:` line names only the gates that cover the bug: the filing check accepts a
  `.sh` name there as the coverage claim without running or opening it.
- File in the migrated ledger format with `scripts/gen/new_bug.sh`, which writes the `bug-ledger`
  block, a `changelog.d/` fragment and the `discovery.tsv` row (`--batch` or `--new-batch`, with
  `--silent`) and regenerates the views; after editing a record, regenerate with
  `scripts/gen/bug_ledger.py --write` and `scripts/gen/bug_dashboard.py --write`. If the script fails
  after reserving a number, rerun the `--number NNN` command it prints rather than filing by hand.
- Have Codex confirm each bug is real (a reproducer and a verdict line in the record) before
  filing it.

**Review.** Every slice gets a Codex review before it lands, through `scripts/review/review.sh`:
at most 5 rounds, the first over the whole diff and each later round over the delta since the
previous one. Answer each finding with a fix or a stated reason. Run Codex by hand only with
`GIT_OPTIONAL_LOCKS=0` (and `</dev/null`): its sandboxed `git status` otherwise takes the
worktree's `index.lock` and can leave it behind (bug 209).

**Git safety.**
- Never pipe `git rebase` (or `git merge`, `git cherry-pick`) into anything: a pipe hides the stop.
  After every git operation, check that no merge, rebase, cherry-pick or revert is in progress
  (`git status`; `MERGE_HEAD`, `rebase-merge`, `rebase-apply` under `git rev-parse --git-dir`).
- No bare `git stash`; no interactive git (`-i`).
- Check a process's working directory (`lsof -a -p <pid> -d cwd`) before killing it; many processes
  on this machine belong to other repositories and other sessions.
- Remove a stale `index.lock` only when no process owns it (`lsof <path>/index.lock` is empty);
  otherwise wait and retry.
- Preserve other sessions' edits and untracked files. Never reset or clean a shared checkout.

**Language invariants.** Link to the normative documents instead of restating them:
[OWNERSHIP_MODEL.md](../language/OWNERSHIP_MODEL.md) (linear, not affine: non-Copy values are used
exactly once), [MEMORY_SEMANTICS.md](../language/MEMORY_SEMANTICS.md) (strictly second-class
references, §15), [LANGUAGE_INVARIANTS.md](../language/LANGUAGE_INVARIANTS.md),
[STATEMENT_EXPRESSION_MODEL.md](../language/STATEMENT_EXPRESSION_MODEL.md) (evaluation order, value
vs statement position), [KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) (the only authority on
hole status) and [VERIFIED_PIPELINE.md](../compiler/VERIFIED_PIPELINE.md) (the structural owners).

## Concurrent work: one worktree per writer

**The default: every concurrent line of work gets its own git worktree.**

```sh
scripts/worktree-new.sh <name> [base-committish]
```

This is not an optimization. One working tree cannot serve two writers, because
the file system is the shared mutable state and neither writer can see the
other's intent.

### What went wrong when this was not the default

On 2026-07-31 two agents were found working in the same worktree
(`.claude/worktrees/r0442-proofcore-callee`). Every symptom was initially
misread as something else:

| symptom | what it was blamed on | what it actually was |
| --- | --- | --- |
| an edit to `ProofCore.lean` vanished, and the file dropped out of `git status` | the mutation harness | the mutation harness restoring a backup **over a concurrent edit**, then verifying against the pre-edit hash and reporting success |
| `origin/main` moved three times mid-session | stale fetches | the other agent pushing |
| two `push-both.sh` runs waiting on CI for the same SHA | a duplicated invocation | two agents each publishing |

The first row is the expensive one: work was destroyed *and* the tool reported
that nothing was wrong. A postcondition that compares against a stale baseline
confirms the corruption it was meant to catch.

### What a worktree does and does not isolate

| | |
| --- | --- |
| **Isolated** | working tree, index, `HEAD`, current branch |
| **Shared** | object store, remotes, **the stash stack**, branch namespace |

So worktrees fix file-level races, not coordination:

- Pushes must still be sequenced — hence the lock in `push-both.sh`.
- `git stash` is still unsafe across worktrees; bare `git stash` / `git stash pop`
  is banned repo-wide. Use a temporary WIP commit, or
  `git stash push -u -m "<unique-tag>"` and recover by SHA with `apply`, never
  `pop`.
- A branch name is a shared resource. `worktree-new.sh` refuses to reuse one
  rather than let `git` fail halfway, because the existing branch may belong to
  another writer.

### Enforcement is at the point of danger, not by head-count

Two heuristics for "is another agent here?" were measured and both cried wolf:

- matching process names (`pgrep -lf "codex|claude"`) reported **115** — every
  shell wrapper's command line contains the `.claude` snapshot path;
- counting processes whose CWD is the tree reported **7** in a worktree freshly
  created by a single agent — its own shells and background waiters all sit there.

Neither distinguishes "two writers" from "one agent working normally", and a
warning that fires on non-events trains the reader to ignore the one that
matters. So concurrency is caught where the signal is a fact:

- **`scripts/tests/test_mutation.sh`** records a hash of the content it itself
  wrote. At restore time, content that is neither its mutation nor the original
  means a third party wrote the file: it **refuses to restore**, preserves their
  version under `CONCURRENT-EDIT/`, keeps its own backup for reconciliation, and
  fails the run. It cannot silently overwrite an edit it did not make.
- **`scripts/push-both.sh`** takes an exclusive lock in the **common** git dir
  (`git rev-parse --git-common-dir`), not the worktree — remotes are shared, so a
  per-worktree lock would miss two worktrees publishing at once.

### Working while something long is running

Long-running work does not always require an idle tree. Distinguish the phases:

| phase | tree must be stable? |
| --- | --- |
| pre-push hook running gates | **yes** — gates read the working tree |
| `run_ci_gates_local.sh` | **yes** — same reason |
| `test_mutation.sh` | **yes** — it rewrites source in place |
| `push-both.sh` waiting for remote CI | **no** — it only polls `gh` |

The CI wait touches no files, so work can continue during it — in another
worktree. Treating the whole publish as tree-freezing is over-caution; treating
the hook phase as safe is the error that costs a run.

### Landing, review and load (development throughput)

On 2026-10-22 a finished branch took 6 – 10 h to reach main for about 1 h of work.
The [development throughput](../../ROADMAP.md#development-throughput) track in the
roadmap owns the repair. These rules apply to every writer now:

- **One bug or one rule per branch** (R-0545). A branch that changes a language or
  checker rule lands first and small, with the migrations of main's fixtures and
  nothing else. A branch that needs two rules is two branches.
- **Review per slice, delta after the first pass** (R-0544). After the first full
  review, each round reviews only the diff since the previous round. Past 5 rounds,
  split the slice. Review ownership and memory-safety work with neutral prompts and
  piecewise diffs.
- **At most 2 heavy local runs per machine** (R-0543). Full gate sets, closure-mode
  runs and mutation campaigns count as heavy. Agents run the fast and
  impact-selected gates; full sets go to the servers (R-0541) or to CI. The
  machine-wide semaphore `scripts/tests/lib/heavy_slots.sh` holds the full runners,
  `run_tests.sh --full` and the mutation drivers to two slots; for any other heavy
  command, check the load and queue behind a running heavy run instead of starting a third.
- **Ready branches land together** (R-0539). Ready branches are integrated into one
  train candidate with one CI run, rather than each rebasing onto the last landing,
  through `scripts/land/train.sh` ([LANDING.md](LANDING.md)). A train built by hand
  follows the same rules: one integrated candidate, results recorded against its
  exact SHA, and bisection by member only on red.

### Running the full gate set on the server

The full set runs on a server (or in CI), never on the laptop:

```
~/bin/concrete-remote <pushed-ref> \
  'lake build && bash scripts/tests/run_ci_gates_local.sh'
```

`concrete-remote` checks the ref out as a `git clone --shared` (not a worktree:
two mutation-driver gates refuse worktrees) and sets the runner's default width
to the measured best for that host (`CONCRETE_GATE_JOBS_DEFAULT`; override with
`CONCRETE_REMOTE_JOBS=N`, or pass `JOBS=N` in the command). The measurements and
the critical path were taken on concrete-ci-1, since deleted (2026-10-23), and are
in [GATE_PROFILE.md](GATE_PROFILE.md#parallel-scaling-on-concrete-ci-1-r-0546-step-4).

**A gate must be safe to run beside every other gate.** In a pass the gates run
JOBS-wide in one checkout, and several passes run on one host. So a gate:

- writes scratch only under `mktemp -d` (or a `.$$` name), never a fixed
  `/tmp/<name>` — `check_gate_hygiene.sh` refuses fixed `/tmp` paths;
- never edits a tracked file in place, not even to restore it a moment later.
  A probe that perturbs a source runs in a private copy of the checkout
  (`run_fast_surface_gates.sh --mutate`, `check_attestation_manifest.sh`). An
  in-place edit of a `.lean` file makes every concurrent `require_fresh_binary`
  rebuild the shared compiler from it;
- runs sub-steps concurrently only under one lock it already holds (fresh.sh
  refuses a second concurrent acquisition) and records each sub-step's exit
  status, so a lost result is a failure, not a pass.

A pool failure that passes on the runner's sequential re-check is listed under
"Recovered under sequential re-run" and in `.ci-gate-timings.tsv` (stage
`pool`, verdict `FAIL`, then stage `recheck`). Treat it as a race in that gate
to fix, not as noise.

### Killing a publish does not un-publish it

`push-both.sh` runs `git push` as a CHILD process. Killing the script does not
kill that child, and a `git push` already in flight will complete.

Measured 2026-07-31: a publish was killed during its pre-push hook, `git ls-remote`
showed the primary unchanged, and that was read as "nothing was published". The
`git push` child was still running; it finished moments later and advanced the
primary. The next publish was then rejected as non-fast-forward — harmless here,
but the reasoning behind the kill was a race read as a guarantee.

So: a ref check taken *at* the moment of the kill proves nothing about a push
that has already started. If a publish must be abandoned, re-check the remote
AFTER the process tree is gone (`pgrep -f "git push"` clear), and treat the
remote's actual state as the answer rather than the state you saw before killing.

### Orchestrating a publish: never infer state from process names

`pgrep -f "push-both"` matches the WAITER'S OWN command line, which contains that
string. Every `until ! pgrep -f "push-both"; do sleep 30; done` loop therefore
waits on itself forever, and a status check written the same way reports "publish
running" when nothing is. Measured 2026-08-01: six background tasks were stuck on
this at once, and the reported state was wrong for as long as they ran. It is the
same failure as counting `pgrep -lf "codex|claude"` and getting 115 — a
process-name match catches the matcher.

Do this instead:

* capture the launched PID and `wait "$pid"`;
* verify completion by checking the EXACT REMOTE SHA (`git ls-remote`), which is
  the fact you actually care about;
* query CI for that SHA (`gh run list --commit <full-sha>` — the full SHA; the
  short form returns nothing);
* never infer state from a process-name match.

For a one-off inspection, `ps -eo command | grep -c "[p]ush-both.sh"` excludes
itself via the bracket, but a ref check is still the better answer.

### Reviewing a branch

Every agent reviews the same way: `scripts/review/review.sh`, one round per
invocation, with the verdict recorded as a git note on the reviewed commit. The
note is shared through `origin`, so rounds run in another worktree or by another
agent count toward the same cap, and the next round reviews only the delta. See
[REVIEW.md](REVIEW.md).
