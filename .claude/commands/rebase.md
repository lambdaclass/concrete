---
description: Safely rebase this branch onto origin/main (no pipes, stop on every stopped operation)
argument-hint: "[onto ref, default origin/main]"
---

Rebase this branch onto: $ARGUMENTS (`origin/main` when empty; written `<onto>` below).

Rules: [AGENTS.md](../../AGENTS.md) ("Git safety").

1. **Clean start.** `git status`: commit any work (a WIP commit, never `git stash`). Refuse to start
   if a merge, rebase, cherry-pick or revert is already in progress. Do not rebase a worktree whose
   checks or hook are running.
2. `git fetch origin`. Note the current head SHA (`git rev-parse HEAD`) so it can be recovered.
3. `git rebase <onto>` on its own line. Never pipe it, never `-i`. Read its exit status.
4. **On a stop**, `git status` and `git diff --name-only --diff-filter=U`. Resolve by hand, keeping
   both sides' intent. `changelog.d/` fragments do not conflict; `CHANGELOG.md` is not edited by
   hand. Nothing under `legacy/` may change: a conflict there means a side edited the frozen tree,
   which is itself the defect. Then `git grep -nE '^(<{7}|>{7}|\|{7})( |$)|^={7}$' -- . ':(exclude)legacy/'`
   must be empty, and `GIT_EDITOR=true git rebase --continue`. Repeat. If it goes wrong,
   `git rebase --abort` and report; the old head is still at the noted SHA.
5. **After it finishes**, confirm no operation is in progress (`git status` shows none; no
   `rebase-merge`/`rebase-apply` under `git rev-parse --git-dir`).
6. Re-validate in `nix develop`: `lake build` and `bash scripts/checks/run.sh`. Results from before
   the rebase do not carry over unless you can say why the rebase cannot affect them.
7. `git push --force-with-lease origin HEAD:refs/heads/wip/<branch>` (only your own `wip/*` branch).

Report: old and new head SHAs, the conflicts and how each was resolved, the checks re-run.
