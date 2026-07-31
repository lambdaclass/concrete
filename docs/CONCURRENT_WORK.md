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
