# Concurrent work: one worktree per writer

**The default: every concurrent line of work gets its own git worktree.**

```sh
scripts/worktree-new.sh <name> [base-committish]
```

This is not an optimization. One working tree cannot serve two writers, because
the file system is the shared mutable state and neither writer can see the
other's intent.

## What went wrong when this was not the default

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

## What a worktree does and does not isolate

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

## Enforcement is at the point of danger, not by head-count

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

## Working while something long is running

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

## Landing, review and load (development throughput)

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
  impact-selected gates; full sets go to the servers (R-0541) or to CI. Until the
  machine-wide semaphore exists, check the load and queue behind a running heavy
  run instead of starting a third.
- **Ready branches land together** (R-0539). Ready branches are integrated into one
  train candidate with one CI run, rather than each rebasing onto the last landing.
  Until the landing script exists, a hand-built train follows the same rules: one
  integrated candidate, results recorded against its exact SHA, and bisection by
  member only on red.

## Killing a publish does not un-publish it

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

## Orchestrating a publish: never infer state from process names

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

## Reviewing a branch

Every agent reviews the same way: `scripts/review/review.sh`, one round per
invocation, with the verdict recorded as a git note on the reviewed commit. The
note is shared through `origin`, so rounds run in another worktree or by another
agent count toward the same cap, and the next round reviews only the delta. See
[REVIEW.md](REVIEW.md).
