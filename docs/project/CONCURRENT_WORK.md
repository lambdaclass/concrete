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

## Running the full gate set on the server

The full set runs on concrete-ci-1 (64 threads), never on the laptop:

```
CONCRETE_REMOTE=scaleway ~/bin/concrete-remote <pushed-ref> \
  'lake build && bash scripts/tests/run_ci_gates_local.sh'
```

`concrete-remote` checks the ref out as a `git clone --shared` (not a worktree:
two mutation-driver gates refuse worktrees) and sets the runner's default width
to the measured best for that host (`CONCRETE_GATE_JOBS_DEFAULT`, 32 on
scaleway; override with `CONCRETE_REMOTE_JOBS=N`, or pass `JOBS=N` in the
command). The measurements and the critical path are in
[GATE_PROFILE.md](GATE_PROFILE.md#parallel-scaling-on-concrete-ci-1-r-0547).

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
