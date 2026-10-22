---
description: Report where this worktree, its branch, its CI and this machine stand (read-only)
---

Report status. Read-only: change nothing, kill nothing.

1. **Worktree**: `pwd`, `git branch --show-current`, `git rev-parse HEAD`, `git status --short`,
   and whether a merge, rebase, cherry-pick or revert is in progress.
2. **Branch vs main**: `git fetch origin`, then `git rev-list --left-right --count origin/main...HEAD`
   (behind / ahead), and whether `origin/wip/<branch>` equals HEAD (when it was last pushed:
   `git log -1 --format=%cr origin/wip/<branch>`; over 5 minutes of unpushed work is overdue).
3. **CI** for HEAD: `gh run list --commit $(git rev-parse HEAD) --limit 5`; for main:
   `gh run list --workflow CI --branch main --limit 3`. Distinguish passed, failed, cancelled,
   skipped and in progress.
4. **Machine load**: heavy runs here (`pgrep -fl 'run_ci_gates_local|test_mutation|lake build'`),
   each with its working directory (`lsof -a -p <pid> -d cwd`), so you can tell which worktree
   and repository owns it. More than 2 heavy runs is over the limit; say so, do not kill anything.
5. **Bugs and trains**: reservations held (`git ls-remote origin 'refs/bug-reservations/*'`), the
   bug records this branch adds or changes (`git diff --name-only origin/main...HEAD -- docs/bugs`),
   and any train in progress (`scripts/land/train.sh status <name>` when one is named).

Report in this order: the candidate being validated (branch, SHA, gates and CI state), independent
work advancing alongside it, and what still blocks merge or closure.
