# Land a change

Push the validated branch to `origin/wip/<branch>` and send its head SHA to the
coordinator for review. Once the coordinator clears that revision, open a pull
request and enable auto-merge.

```sh
gh pr create --repo lambdaclass/concrete --base main --head wip/<branch>
gh pr merge --repo lambdaclass/concrete <PR-number> --auto --match-head-commit <reviewed-SHA>
```

GitHub queues the PR once it meets the branch requirements. The `merge_group`
run checks the candidate built on current `main` and any changes ahead of it.
Missing, failed, cancelled or timed-out checks block the merge. Resolve
conflicts or failures on the WIP branch and requeue once it clears checks and
review. Use `gh pr checks <PR-number>` for PR checks and
`gh run list --workflow CI --event merge_group` for queue runs.

## GitHub settings for the coordinator

- Enable **Allow auto-merge**. Protect exactly `main` with **Require a pull
  request before merging**, **Require merge queue**, and **Require status checks
  to pass**. Select every exact name in [required-checks.txt](../required-checks.txt)
  with GitHub Actions as the expected source. Remove the retired **Landing
  record** check if configured.
- Use the **Merge** queue method and enable **Only merge non-failing pull
  requests** (called **Require all queue entries to pass required checks** in
  rulesets). Allow merge commits and leave **Require linear history** off.
  Set the check timeout to cover the full CI run, including runner wait.
- Enforce the rules for admins and apps with no bypass actors, force pushes,
  or branch deletion. The queue is the only path into `main`.
- For the cutover, protect `main` with the checks above and require the PR to
  be up to date. Merge this PR with a merge commit after CI passes on its
  current merge result. Once the workflow is on `main`, require the queue
  before admitting further PRs.

See [GitHub's merge queue settings](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/configuring-pull-request-merges/managing-a-merge-queue).
