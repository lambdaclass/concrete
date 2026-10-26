# Agent playbooks

Status: operational procedure

Project playbooks live in `.claude/commands/`. Every agent session, on the laptop or the server,
runs the same procedure by name instead of receiving a long prompt:

| command | what it does |
|---|---|
| `/fix-bug <N or description>` | reproduce, classify, repair or contain one bug on its own branch |
| `/roadmap-item <R-NNNN>` | carry one roadmap task to a landable candidate |
| `/file-bug <title>` | file a new bug with `scripts/gen/new_bug.sh` |
| `/creview` | Codex review of the branch through `scripts/review/review.sh` |
| `/rebase` | the safe rebase onto `origin/main` |
| `/land` | hand a validated branch to the landing train (`scripts/land/train.sh`) |
| `/cstatus` | where this worktree, its branch, its CI and this machine stand |

`/creview` and `/cstatus` carry a `c` prefix so they do not shadow Claude Code's built-in `/review`
and `/status`; a new playbook takes a name no built-in uses.

This file owns the rules the commands share. A command links here rather than restating them, so a
rule changes in one place.

## Rules

**Bugs.**
- Every bug gets a structural cause and a structural owner: the design fact that let it exist, and
  the roadmap task or pipeline stage that removes the whole class
  ([STRUCTURAL_CLASSES.md](../bugs/STRUCTURAL_CLASSES.md)). A patch without them is not a fix.
- To file a bug, run `scripts/gen/new_bug.sh`, which writes its record and runs the ledger
  generator. To log a change, add a `changelog.d/` fragment
  ([changelog.d/README.md](../../changelog.d/README.md)). Never hand-edit a generated view (the bug
  lists, the class table's bugs column, ROADMAP's generated spans, `CHANGELOG.md`,
  `Concrete/BuildIdentity.lean`); see [scripts/tests/README.md](../../scripts/tests/README.md),
  "Shared ledgers".
- A silent defect (wrong code, unsound claim) is stopped at once by repair or fail-closed
  containment; a loud one is filed, pinned and owned (the defect disposition rule in
  [ROADMAP.md](../../ROADMAP.md#the-current-execution-queue)).

**Branches.**
- One bug or one rule per branch, in its own worktree (`scripts/worktree-new.sh`,
  [CONCURRENT_WORK.md](CONCURRENT_WORK.md)). A branch that changes a language rule lands first and
  stays small; work that depends on it waits or rebases onto it.
- Commit each validated slice. Push the branch to `wip/<branch>` at least every 5 minutes of work
  (`git push origin HEAD:refs/heads/wip/<branch>`); the hook's wip tier only scans for conflict
  markers.
- Never push to `main`, `ci/*` or the remote `fork`. Landings go to both `origin` and
  `lambdaclass`, only through the landing script (`scripts/land/train.sh`) or the landing bot.

**Validation.**
- Pre-push runs the fast surface gates and the gates selected by what the push touches. Full gate
  sets run on the server (`concrete-remote wip/<branch> 'lake build && bash
  scripts/tests/run_ci_gates_local.sh'`) or in CI (`gh workflow run CI --ref <branch>`), never as a
  full local run on the laptop.
- At most 2 heavy runs (full suites, mutation campaigns, `lake build` of the whole tree) per machine.
  Queue behind a running one; continue light work meanwhile. Honour a user's explicit CPU limits.
- Run gates inside `nix develop`. A gate that passes by hand but fails under the hook: suspect an
  inherited `GIT_*` variable first.
- Record each result against the exact commit SHA it ran on.
- Read the obligation, not the prose: a header says what was true when it was written. Make gates
  assert properties, not counts, so an improvement does not read as a regression.

**Before asking for review.** Each of these caused an integration failure in the week to
2026-10-25; run them on the slice, in `nix develop`, before `/creview`:
- `scripts/tests/test_golden.sh` after any change to compiler output or a report.
- `scripts/tests/test_mutation.sh --check-patterns` after editing any file a mutation targets: it
  finds an anchor the edit broke in seconds, before a mutation campaign fails on it.
- `scripts/tests/check_type_identity.sh` and the other gates that elaborate Lean probes against the
  compiler (`grep -l 'lake env lean' scripts/tests/check_*.sh`) after changing a signature or a
  structure they construct.
- A fixture's `run_ok` / `run_err` leg in `run_tests.sh` spells its file name literally: the
  bug-corpus audit looks for each fixture's basename in `run_tests.sh`, so a name assembled from
  parts is reported as uncovered.
- Bound a command with `bounded_run` (`scripts/tests/lib/bounded.sh`), never GNU `timeout`: the
  macOS job has no `timeout`, and `bounded_run` reports a second timeout as INCOMPLETE, not a pass.
- A ledger `coverage:` line names only the gates that cover the bug: the filing check accepts a
  `.sh` name there as the coverage claim without running or opening it.
- File in the migrated ledger format (`scripts/gen/new_bug.sh`: the `bug-ledger` block, a
  `changelog.d/` fragment, a `discovery.tsv` row), then regenerate with
  `scripts/gen/bug_ledger.py --write` and `scripts/gen/bug_dashboard.py --write`.
- Have Codex confirm each bug is real (a reproducer and a verdict line in the record) before
  filing it.

**Review.** Every slice gets a Codex review before it lands, through `scripts/review/review.sh`:
at most 5 rounds, the first over the whole diff and each later round over the delta since the
previous one. Answer each finding with a fix or a stated reason. Run Codex by hand only with
`GIT_OPTIONAL_LOCKS=0` (and `</dev/null`): its sandboxed `git status` otherwise takes the
worktree's `index.lock` and can leave it behind (bug 209).

**Git safety.**
- Never pipe `git rebase` (or `git merge`, `git cherry-pick`) into anything: a pipe hides the stop.
  After every git operation, check that no merge, rebase, cherry-pick or revert is in progress
  (`git status`; `MERGE_HEAD`, `rebase-merge`, `rebase-apply` under `git rev-parse --git-dir`).
- No bare `git stash`; no interactive git (`-i`).
- Check a process's working directory (`lsof -a -p <pid> -d cwd`) before killing it; many processes
  on this machine belong to other repositories and other sessions.
- Remove a stale `index.lock` only when no process owns it (`lsof <path>/index.lock` is empty);
  otherwise wait and retry.
- Preserve other sessions' edits and untracked files. Never reset or clean a shared checkout.

**Language invariants.** Link to the normative documents instead of restating them:
[OWNERSHIP_MODEL.md](../language/OWNERSHIP_MODEL.md) (linear, not affine: non-Copy values are used
exactly once), [MEMORY_SEMANTICS.md](../language/MEMORY_SEMANTICS.md) (strictly second-class
references, §15), [LANGUAGE_INVARIANTS.md](../language/LANGUAGE_INVARIANTS.md),
[STATEMENT_EXPRESSION_MODEL.md](../language/STATEMENT_EXPRESSION_MODEL.md) (evaluation order, value
vs statement position), [KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) (the only authority on
hole status) and [VERIFIED_PIPELINE.md](../compiler/VERIFIED_PIPELINE.md) (the structural owners).
