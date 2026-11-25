---
description: Open a pull request and enable auto-merge through GitHub's merge queue
argument-hint: <branch>
---

Land `wip/$ARGUMENTS` using [LANDING.md](../../docs/LANDING.md).

Confirm that the coordinator cleared the published head SHA. Open or reuse its
PR to `main` and enable auto-merge with `gh pr merge <PR-number> --auto --match-head-commit <reviewed-SHA>`.
Report the PR, head SHA and any PR or merge-group check that blocks landing.
