# Working while validation runs

Remote CI gates publication and merge of the candidate it checks. It does not
block independent development on other branches.

- While remote CI or a push hook runs, continue the next authorized task in a
  separate branch and worktree. Prefer useful implementation, focused checks,
  documentation or read-only audits over waiting. Do not repeatedly ask whether
  to continue work the user already authorized.
- Keep the candidate under validation fixed. Never edit compiler sources, test
  scripts, fixtures or generated files in a worktree under active local
  validation or a push hook. Read-only inspection can continue there.
- Start independent work from validated main when possible. If it depends on an
  unvalidated candidate, record that dependency and keep its publication or
  merge subject to the applicable checks. A branch under development is not a
  validated release.
- Avoid competing heavy local runs. Run at most two full suites, mutation
  campaigns or other CPU-heavy checks per machine at once; queue further ones
  (ROADMAP R-0543). Prefer running full gate sets on the CI servers or in CI. Separate worktrees do not
  provide separate CPU budgets. Continue light work while waiting, and honor
  explicit user limits on CPU use.
- If CI fails, inspect the logs before diagnosing it. Repair the candidate on its
  branch, propagate relevant fixes to dependent branches, and validate the final
  integrated revision before merging. Do not infer contention from a successful
  rerun alone.
- Record results against exact commit SHAs. Reuse earlier results only with an
  explicit account of why the change cannot affect those checks; run required
  integration checks on the final candidate. Preserve distinctions between
  passed, failed, skipped, cancelled and incomplete qualification.
- Merge only after the required checks pass under the agreed policy. Do not
  bypass hooks or treat a timeout exception as a passing result. When main's CI
  is required for closure, continue independent work while it runs too.
- Preserve other sessions' edits and untracked files. Do not reset or clean a
  shared checkout to make progress.

Progress updates should identify the candidate being validated, the independent
work advancing alongside it, and the checks that still block merge or closure.
