# Bug 209: a Codex review left a stale `index.lock` in the agent's worktree

<!-- bug-ledger
summary: a `codex exec --sandbox read-only` review ran `git status` in the caller's worktree, which took `index.lock` for an index refresh; stopped mid-refresh it left the lock behind, 0 bytes, and the next commit there failed. `scripts/review/review.sh` and the `/creview` commands run Codex with `GIT_OPTIONAL_LOCKS=0`; pinned by two `check_review_tool.sh` legs. Found during the Hetzner gate-host work
coverage: a read-only Codex review left a stale index.lock in the caller's worktree. Regression vehicle is check_review_tool.sh (every reviewer call runs with GIT_OPTIONAL_LOCKS=0), not a tests/programs fixture (2026-10-23)
class: 16 — a read-only review's git left the caller's `index.lock` behind
silent: no
ruling: Outside the spec: tooling — a read-only Codex review left a stale index.lock in the caller's worktree
-->

**Status:** FIXED (branch `checker-unidentified-cause`, for landing train 11)
**Found:** 2026-10-23, during the Hetzner gate-host and checker-cause work: three times an empty
`.git/worktrees/<name>/index.lock` appeared in the agent's own worktree after a
`codex exec --sandbox read-only` review, and the next `git commit` or `git merge` there failed with
"Unable to create ... index.lock: File exists" while no git process was running.
**Severity:** loud. No wrong result: the next git write in that worktree refused until someone
removed the lock by hand, which `/creview` step 5 and AGENTS.md already told agents to do.

Confirmed real by Codex review, 2026-10-25: Optional index writes could strand index.lock, violating docs/project/REVIEW.md’s read-only reviewer contract; the invocation lacked lock suppression.

## Reproducer

In a scratch worktree with every tracked file's stat data dirty (`git ls-files -z | xargs -0 touch`),
poll `.git/worktrees/<name>/index.lock` every 5 ms while running

```
codex exec --sandbox read-only "Run git status, then git show --stat HEAD, ..." </dev/null
```

Observed: the lock appears about 2 s into the run, for about 50 ms, while Codex's sandboxed
`git status` refreshes the index; the read-only sandbox does not prevent it. Killing a
`git status` with SIGKILL inside that window leaves the lock behind, 0 bytes (2 of 60 kills at
0.25 s). The same Codex run with `GIT_OPTIONAL_LOCKS=0` in its environment shows the variable inside
its commands and takes no lock.

Expected: a read-only review never writes into the caller's repository.

## Cause

`git status` (and other porcelain reads) opportunistically refresh the index: they create
`index.lock`, write the refreshed index and rename it over `index`. Codex runs git inside the
caller's worktree, and its `--sandbox read-only` policy did not stop that write. A reviewer command
stopped mid-refresh (a command timeout, or the session ending) leaves the empty lock. The review
entry points ran Codex with the caller's environment unchanged: `scripts/review/review.sh`
(`run_one`) and the by-hand commands in `.claude/commands/creview.md`.

## Fix

The review run tells git it may not take optional locks.

- `scripts/review/review.sh` runs the reviewer as `GIT_OPTIONAL_LOCKS=0 "$CODEX_BIN" exec ...`.
  With it, `git status` reads the index and writes nothing.
- `.claude/commands/creview.md`: the by-hand round commands carry `GIT_OPTIONAL_LOCKS=0`, and
  step 5 says why; removing a lock nobody owns stays the fallback.
- `docs/project/REVIEW.md` records the environment of the reviewer call.

## Containment

`check_review_tool.sh`: the fake reviewer records `GIT_OPTIONAL_LOCKS` for every call, and two legs
require that every call saw `0` (the second fails on any call that did not). Before the fix every
call records `optional_locks=unset`.

## Structural cause

A process that only reads a repository wrote into it: the review run's git commands share the
caller's `.git`, and git's default is to refresh the index under a lock whenever it can. The run did
not own that path, and an interrupted write left an artifact there for the next process to trip
over. This is class 16, artifacts at paths no run owns: 179 and 191 were intermediates and test
files at shared names; here it is a lock file in a repository the run was only meant to read.

## Structural owner

The review entry point, `scripts/review/review.sh`, which owns how the reviewer is invoked; the
`/creview` playbook for by-hand rounds. `check_review_tool.sh` enforces it. Class 16 in
[STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md).

## Residual

A Codex run started by hand without the variable can still take the lock; `/creview` and AGENTS.md
name the rule and the removal fallback. Other tools that run git in a worktree from outside (an
editor's git integration, a desktop app) are not covered.
