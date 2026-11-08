# Agent and contributor workflow

Status: operational procedure for contributors and agents. The manuals for
[review](docs/REVIEW.md) and [landing](docs/LANDING.md) own their detailed procedures;
[this file's rules](#rules) govern the shared workflow.

The old compiler, bug ledger and gates are frozen in `legacy/` and are never edited or run here.
Current checks enter through `scripts/checks/run.sh`; current defects live in [ISSUES.tsv](ISSUES.tsv).
The [documentation index](docs/README.md) distinguishes current guides, proposals and history.

## Playbooks

Project playbooks live in `.claude/commands/`. Every agent session, on the laptop or the server,
runs the same procedure by name instead of receiving a long prompt:

| command | what it does |
|---|---|
| `/roadmap-item <R-NNNN>` | carry one roadmap task to a landable candidate |
| `/creview` | Codex review of the branch through `scripts/review/review.sh` |
| `/rebase` | the safe rebase onto `origin/main` |
| `/land` | hand a validated branch to the landing train (`scripts/land/train.sh`) |
| `/cstatus` | where this clone, its branch, its CI and this machine stand |

`/creview` and `/cstatus` carry a `c` prefix so they do not shadow Claude Code's built-in `/review`
and `/status`; a new playbook takes a name no built-in uses.

This file owns the rules the commands share. A command links here rather than restating them, so a
rule changes in one place.

## Rules

**Design questions** are decided by [PHILOSOPHY.md](docs/PHILOSOPHY.md). Any important design decision is
put to the owner by Claude or Codex, with a recommendation reasoned from docs/PHILOSOPHY.md.

**Owner decisions: never decide one.** The language decisions OD1 – OD157
([decisions.md](docs/decisions.md#owner-decisions)) are decided only by the project owner. No
agent, orchestrator or reviewer chooses, encodes, implements or provisionally ships any side of any
of them, inside a bug fix or anywhere else. Agents present options and a recommendation to the owner
and wait for an explicit answer. Behaviour already on main that took a side stays as it is, recorded
as a conflict, until the owner answers; nobody extends or reverts it meanwhile.
- The Codex review of every fix (`scripts/review/review.sh` asks it) also answers: "does this fix
  pick a side on any OD in docs/decisions.md?" If yes, or if the fix cannot be made without picking
  one, stop and report to the owner; do not land it. Recording an answer the owner explicitly gave
  is not picking a side; what the review checks then is in
  [REVIEW.md](docs/REVIEW.md#owner-decisions-choosing-versus-recording).

**Defects and change records.**
- Record current spec, proof, compiler, launcher, CI and migration defects in [ISSUES.tsv](ISSUES.tsv),
  with evidence, an owner and a closure condition. Its schema is checked by
  `scripts/checks/issues.sh`.
- Add a `changelog.d/` fragment for each change. Never edit generated `CHANGELOG.md` by hand.
- The old bug ledger, generators and bug-filing playbooks are historical, under `legacy/`;
  never run them or patch that tree. The [retention manifest](RETENTION.tsv) governs migration.
- New-compiler silent defects are repaired or contained fail-closed; loud defects are recorded,
  pinned and owned. See the [roadmap's defect disposition rule](ROADMAP.md#defect-disposition-rule).

**Who works on what** (owner, 2026-11-08, [decision](docs/decisions.md#decided-2026-11-08-eleven)).
- Claude's quota goes to the critical path: the spec, slice 0 and the front end. Mechanical work
  goes to Codex and Kimi as writers.
- One writer edits `docs/decisions.md` and `ROADMAP.md` at a time; other workers send that writer
  their records instead of editing those files.

**Branches.**
- Every branch's full clone is `~/projects/clones/<branch>`, removed after the branch lands (owner,
  2026-11-08; R-0619).
- One bug or one rule per branch, in its own full clone
  ([concurrent work](#concurrent-work-one-worktree-per-writer)). A branch that changes a language rule lands first and stays small; work that
  depends on it waits or rebases onto it.
- Rebase onto `origin/main` after every landing (`/rebase`), not only before your own landing, so a
  conflict or a changed rule shows up while the branch is small.
- Commit each validated slice. Push the branch to `wip/<branch>` at least every 5 minutes of work
  (`git push origin HEAD:refs/heads/wip/<branch>`); the hook's wip tier only scans for conflict
  markers.
- Never push to `main`, `ci/*` or the remote `fork`. Landings go to both `origin` and
  `lambdaclass`, only through the landing script (`scripts/land/train.sh`).
  `train.sh check` runs the checks before the push, and the push runs the root hook's full set again;
  the train's push to `ci/<name>` and both pushes of `land` are read back with `ls-remote` and
  retried when they did not move the ref (GitHub drops the connection during long hooks); `land` checks that both remotes are at
  the head ([LANDING.md](docs/LANDING.md#land)).
- After any push of your own whose hook ran gates, check that the remote ref moved
  (`git ls-remote origin refs/heads/<branch>`) and push again if it did not.

**Validation.**
- Locally, run only the impact-selected gates (pre-push runs the fast surface gates and the gates
  selected by what the push touches). For the full set, dispatch CI on the branch
  (`gh workflow run CI --ref <branch>`) or use the server (`concrete-remote wip/<branch> 'lake build
  && bash scripts/checks/run.sh'`); never a full local run on the laptop.
- Heavy legs default to the CI server (concrete-ci-2). Set `git config land.gatesHost concrete-ci-2`
  in the clone you land from, and `train.sh check` then runs its impact gates there, bound to the
  train head's SHA. The `ci` push still runs the root hook's full set; it reuses no remote result. An unreachable host is INCOMPLETE and blocks;
  run locally only with `check --local-gates` (docs/LANDING.md, "Gate host").
- At most 2 heavy runs (full suites, mutation campaigns, `lake build` of the whole tree) per machine.
  Queue behind a running one; continue light work meanwhile. Honour a user's explicit CPU limits.
- Run gates inside `nix develop`. A gate that passes by hand but fails under the hook: suspect an
  inherited `GIT_*` variable first.
- Record each result against the exact commit SHA it ran on.
- Read the obligation, not the prose: a header says what was true when it was written. Make gates
  assert properties, not counts, so an improvement does not read as a regression.

**Before asking for review.** Commit the candidate and run the applicable checks in
`nix develop`; the root entry point is `bash scripts/checks/run.sh`. Compiler and proof changes
also run their owning gates under `scripts/checks/compiler/` and `scripts/proofs/` and the required
CI jobs. Bind results to the exact SHA. Legacy test and ledger commands are not current checks.
Use `bounded_run` from `scripts/checks/lib/bounded.sh` for portable bounded commands.

**Review.** Codex reviews everything before it is published or brought to the owner, and Kimi and
agy (Gemini) review every branch too; a reviewer's absence is recorded, never assumed
([decision](docs/decisions.md#decided-project-review)). agy cannot read files: run it as
`bounded_run 1500 -- agy -p "<prompt>"` with the diff and the context it needs in the prompt, ending with a
required verdict line. Every slice gets a Codex review before it lands, through `scripts/review/review.sh`:
at most 2 rounds for docs and the roadmap, 3 for scripts and 5 for the compiler and the spec
(owner, 2026-11-08; `review.sh` enforces 5 until R-0618 lands), the first over the whole diff and each later round over the delta since the
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

**Language authority.** Read the [owner decisions](docs/decisions.md) and the
[specification guide](ConcreteSpec/README.md) for the proposed Lean rules, freeze status and
open obligations. Archived design records and hole inventories describe the legacy compiler;
they do not establish the new compiler's guarantees.

**Gate hygiene.** Checks run concurrently. Keep scratch data in a private `mktemp -d` directory;
never rewrite tracked source files in place for a probe or mutation. Use a private copy, report
all sub-step failures, and retain the distinction between failed and incomplete results. A pass
on a sequential retry does not explain a concurrent failure. Honour the heavy-run limit and the
shared gate locks. Never count a skipped or timed-out check as passing.

<a id="concurrent-work-one-worktree-per-writer"></a>
## Concurrent work: one full clone per branch

Use `~/projects/clones/<branch>` for each branch, as required by R-0619. The old
`scripts/worktree-new.sh` creates linked worktrees and does not implement this requirement;
until the clone-creation script lands, create the full clone explicitly:

```sh
git clone https://github.com/unbalancedparentheses/concrete2.git ~/projects/clones/<branch>
cd ~/projects/clones/<branch>
git config core.hooksPath .githooks
git remote add lambdaclass https://github.com/lambdaclass/concrete.git
git switch -c <branch> origin/main
```

Choose a fresh branch name and destination; do not reuse another writer's clone. Remove the clone
after its branch lands. Preserve validation and landing records when needed for handover.

Only the landing script publishes `main` and `ci/*`. Run the whole train in the same full clone,
with its hook enabled; linked worktrees are refused. Follow [LANDING.md](docs/LANDING.md),
including the gate-host configuration and local-gate fallback. The train's state and logs belong
to that clone's git directory.

Do not edit a candidate while its checks or push hook read it. Continue independent work in another
clone. Separate clones provide no separate CPU budget: the machine-wide limit of two heavy runs
still applies. Never reset or clean another session's checkout, and never infer ownership or
publication from process names; inspect working directories and exact remote SHAs.

Review commands, round accounting, verdict records and owner-decision checks have one home in
[REVIEW.md](docs/REVIEW.md). Landing, verified pushes and readiness notes have one home in
[LANDING.md](docs/LANDING.md). Historical concurrency incidents and superseded procedures remain
in the frozen [legacy workflow](legacy/docs/project/AGENTS.md).
