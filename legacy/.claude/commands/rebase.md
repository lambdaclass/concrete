---
description: Safely rebase this branch onto origin/main (no pipes, stop on every stopped operation, regenerate views)
argument-hint: "[onto ref, default origin/main]"
---

Rebase this branch onto: $ARGUMENTS (`origin/main` when empty; written `<onto>` below).

Rules: [docs/project/AGENTS.md](../../docs/project/AGENTS.md) ("Git safety").

1. **Clean start.** `git status`: commit any work (a WIP commit, never `git stash`). Refuse to start
   if a merge, rebase, cherry-pick or revert is already in progress. Do not rebase a worktree whose
   gates, hook or mutation harness is running.
2. `git fetch origin`. Note the current head SHA (`git rev-parse HEAD`) so it can be recovered.
3. `git rebase <onto>` on its own line. Never pipe it, never `-i`. Read its exit status.
4. **On a stop**, `git status` and `git diff --name-only --diff-filter=U`:
   - conflict only inside generated views (bug lists, class table, ROADMAP generated spans,
     `Concrete/BuildIdentity.lean`): `bash scripts/gen/refresh_generated.sh`, then `git add` them;
   - a bug record, prose or code: resolve by hand, keeping both sides' intent; a renumbered bug
     means re-running `new_bug.sh --number-only` for the newcomer, never two records for one number;
   - `changelog.d/` fragments do not conflict; `CHANGELOG.md` is never edited.
   Then `git grep -nE '^(<{7}|>{7}|\|{7})( |$)|^={7}$' -- .` must be empty, and
   `GIT_EDITOR=true git rebase --continue`. Repeat. If it goes wrong, `git rebase --abort` and
   report; the old head is still at the noted SHA.
5. **After it finishes**, confirm no operation is in progress (`git status` shows none; no
   `rebase-merge`/`rebase-apply` under `git rev-parse --git-dir`). Then
   `bash scripts/gen/refresh_generated.sh`; commit any change as "regenerate generated views".
6. Re-validate in `nix develop`: `run_fast_surface_gates.sh`, the area gates, and
   `bash scripts/tests/audit_bug_corpus.sh` if bug records moved. Results from before the rebase do
   not carry over unless you can say why the rebase cannot affect them.
7. `git push --force-with-lease origin HEAD:refs/heads/wip/<branch>` (only your own `wip/*` branch).

Report: old and new head SHAs, the conflicts and how each was resolved, the gates re-run.
