# Agent and contributor workflow

This file covers repository workflow. See [LANDING.md](docs/LANDING.md)
for the GitHub merge queue and [TOOLING.md](docs/project/TOOLING.md) for the
active script inventory. The old compiler and its gates are frozen in
`legacy/`. File defects in the active spec, proofs, tooling or CI in
[ISSUES.tsv](ISSUES.tsv).

## Playbooks

Project playbooks live in `.claude/commands/`. Every agent session, on the laptop or the server,
runs the same procedure by name instead of receiving a long prompt:

| command | what it does |
|---|---|
| `/roadmap-item <R-NNNN>` | carry one roadmap task to a landable candidate |
| `/rebase` | the safe rebase onto `origin/main` |
| `/land` | open a PR and enable auto-merge through the merge queue |
| `/cstatus` | where this worktree, its branch, its CI and this machine stand |

`/cstatus` uses a distinct name from Claude Code’s built-in `/status`.

This file owns the rules the commands share. A command links here rather than restating them, so a
rule changes in one place.

## Rules

**Design questions** are decided by [PHILOSOPHY.md](docs/PHILOSOPHY.md). Any important design decision is
put to the owner by Claude or Codex, with a recommendation reasoned from docs/PHILOSOPHY.md. Agents may settle mechanical and representation defaults and
record them in the [owner veto list](docs/decisions.md#mechanical-defaults-and-owner-veto-list).
Core semantics remain owner decisions.

**Owner decisions: never decide one.** The language decisions OD1 – OD157
([decisions.md](docs/decisions.md#owner-decisions)) are decided only by the project owner. No
agent, orchestrator or reviewer chooses, encodes, implements or provisionally ships any side of any
of them, inside a bug fix or anywhere else. Agents present options and a recommendation to the owner
and wait for an explicit answer. Behaviour already on main that took a side stays as it is, recorded
as a conflict, until the owner answers; nobody extends or reverts it meanwhile.
- The coordinator review checks whether a fix picks a side on any owner decision.
  If a fix would make that choice, stop and ask the owner. An explicit owner
  answer can be recorded with attribution.

**Bugs.** Record each defect in [ISSUES.tsv](ISSUES.tsv) with evidence, a
structural cause, an owner and a closure condition. Repair or contain silent
defects in the new compiler immediately. File loud defects with a reproducer
and owner. Record legacy defects against `legacy-compiler-final` under the
[defect disposition rule](ROADMAP.md#defect-disposition-rule).

**Who works on what** (owner, 2026-11-08, [decision](docs/decisions.md#decided-2026-11-08-eleven)).
- Claude's quota goes to the critical path: the spec, slice 0 and the front end. Mechanical work
  goes to Codex and Kimi as writers.
- One writer edits `docs/decisions.md` and `ROADMAP.md` at a time; other workers send that writer
  their records instead of editing those files.

**Branches.**
- Every branch's full clone is `~/projects/clones/<branch>`, removed after the branch lands (owner,
  2026-11-08; R-0619).
- One bug or one rule per branch, in its own worktree (`scripts/worktree-new.sh`,
  [concurrent work](#concurrent-work-one-worktree-per-writer)). This applies to all agent work now, not only once
  R-0545 closes. A branch that changes a language rule lands first and stays small; work that
  depends on it waits or rebases onto it.
- Rebase onto `origin/main` after every landing (`/rebase`), not only before your own landing, so a
  conflict or a changed rule shows up while the branch is small.
- Commit each validated slice and back it up to `origin/wip/<branch>` at least
  every five minutes. The pre-push hook scans WIP tips for conflict markers.
- Use only `origin` pointing to `lambdaclass/concrete`. Land through a PR and
  the GitHub merge queue. Direct pushes to `main` and `ci/*` are prohibited.

**Validation.**
- Run impact-selected checks in `nix develop` and run the full tier in CI.
  Use a bounded Linux host for the root checks and record validation against
  the exact revision tested.
- At most two heavy runs per machine. Honor explicit CPU limits.
- Use [`bounded_run`](scripts/checks/lib/bounded.sh) for portable command time
  limits. An incomplete result blocks validation.
- Read the obligation and make checks assert properties that distinguish a
  regression from a valid change.

<a id="review-proportion"></a>
**Review proportion.** A soundness bug, wrong language semantics, false theorem or broken
build blocks. Disputes over mutant, fixture or test construction, helper factoring or
out-of-slice edge cases are non-blocking notes, including in `Proofs/`.
The [November 22 direction](docs/decisions.md#decided-2026-11-22-direction) governs wherever
the earlier risk rule would block additional findings. See the [roadmap reordering](docs/decisions.md#decided-2026-11-22-roadmap-reordering) for the
Phase T pause.

**Review.** The coordinator arranges Codex, Kimi and agy review outside the
repository and records absences. Clear review before landing. Review the full
slice first and the delta in subsequent rounds. The caps are two rounds for
docs and roadmap, three for scripts and five for compiler and spec. Answer
each finding with a fix or a stated reason.

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
[OWNERSHIP_MODEL.md](legacy/docs/archive/design-records/OWNERSHIP_MODEL.md) (linear, not affine: non-Copy values are used
exactly once), [MEMORY_SEMANTICS.md](legacy/docs/archive/design-records/MEMORY_SEMANTICS.md) (strictly second-class
references, §15), [the spec's overview](legacy/docs/spec/overview.md),
[STATEMENT_EXPRESSION_MODEL.md](legacy/docs/archive/design-records/STATEMENT_EXPRESSION_MODEL.md) (evaluation order, value
vs statement position), [KNOWN_HOLES.md](legacy/docs/verification/KNOWN_HOLES.md) (the only authority on
hole status) and [VERIFIED_PIPELINE.md](legacy/docs/compiler/VERIFIED_PIPELINE.md) (the structural owners).

## Turning recurring mistakes into checks

The owner approved mechanical enforcement of recurring workflow mistakes on 2026-11-17.
[R-0710](ROADMAP.md#r-0710) owns the follow-through. Use the root gate or the tool that
can observe the failure, with passing and failing controls. The rows classify the
active repository concern in each lesson. For compound lessons, the scope and any
remaining work are explicit. Historical commands and machine procedures are omitted.

### A. Already enforced

| Lesson | Check and scope |
|---|---|
| Scan advertised WIP tips | The [pre-push hook](.githooks/pre-push) checks the commits named in Git's ref list for conflict markers. |
| Qualify the exact merged tree | GitHub's merge queue runs the full tier on its candidate. Required checks block advancement of `main`. |
| Split declarations with expansions | The [shell assignment check](scripts/checks/lib/shell_assignments.py#L91-L143) parses tracked scripts and hooks with shfmt and fails on parse errors. A `local`, `declare`, `typeset`, `readonly`, or `export` statement with more than one name must have no parameter, arithmetic, or command expansions in its initializers. Split a declaration that uses one of those expansions into one declaration per line. |

### B. Mechanically checkable in this repository

| Lesson | Check or remaining work under R-0710 |
|---|---|
| Detect dependencies that require Bash runtime context | Under R-0710, qualify computed variable names and context-dependent subscript evaluation beyond the explicit expansions covered by the shell assignment check if these patterns enter maintained tooling. Qualify with a same-declaration reference resolved through a nameref and an indexed-array subscript, plus passing controls with split assignments and a literal associative-array key. |
| Bound a reproducer and prevent recursive hooks | Extend the task, memory and wall-time bounds already used by [`sandbox_isolation.sh:154`](scripts/checks/sandbox_isolation.sh) to a shared entry point for live repository probes. Prove that it refuses to start without containment and that nested test Git processes cannot re-enter the hook. |
| Confirm a defect independently before filing it | Verify that a new defect record links an independent, matching reproduction verdict when that evidence is machine-readable. Whether the observation violates the specification remains a review judgment. |
| Sweep the full fixture corpus after behavior or API changes | Extend the active [`test_manifest.sh:24`](scripts/checks/test_manifest.sh) inventory approach to gate-only Lean probes and exercised job obligations. Test that an unregistered or stale fixture is surfaced by the relevant check. Frozen fixtures stay archived. |
| Stop a commit or push when a check piped into a formatter fails | Give tracked validation-to-publication paths a status-preserving entry point, then plant a failed check whose trailing formatter succeeds. Interactive shell lines require use of that entry point. |
| Reserve issue IDs with the coordinator | Bind new IDs to a reservation before publication and test a collision across branches. [`issues.sh:25`](scripts/checks/issues.sh) checks ordering and uniqueness in a single ledger. |
| Prevent repeat integration failures | Assert structured edits took effect, guard repository writers against stopped Git operations and validate deletion targets and fixture inventories. |
| Keep status tags out of roadmap tasks | Lint the roadmap task rows for retired status tags and qualify the check with a tagged row and an ordinary bracketed link. |
| Record structural cause and owner for defects | Check the structure and owner reference of new defect records in the active issue workflow. The relevance of the cause and suitability of the owner still need review. Frozen bug records stay archived. |

### C. Requires owner process, judgment or external operations

| Lesson | Reason |
|---|---|
| Default build-host OS and core-dump policy | Host provisioning and kernel settings are external infrastructure. |
| Require merge queue publication to main | The coordinator maintains the GitHub branch rules in [LANDING.md](docs/LANDING.md). |
| Limit autonomous research to stable, agreed objectives | Choosing worthwhile objectives and granting research authority are owner decisions. |
| Batch validated slices when publication costs dominate | This was based on retired publication tooling and was superseded by frequent WIP backups. Batching independent landings depends on readiness. |
| Prefer a structural long-term fix and treat shortcuts as findings | Assessing the tradeoff and whether a fix removes the whole class requires judgment. Generated prompts are covered above. |
| Assign writers and reviewers by role and available capacity | The coordinator assigns and provisions agents outside the repository. |
| Commit validated slices and back them up frequently | A repository check cannot observe uncommitted work on another machine or know when a slice becomes ready. |
| Commit coherent slices promptly | A gate cannot decide whether a partial change is coherent or schedule the writer's commits. |
| Reason from project philosophy and put important decisions to the owner | Importance and tradeoffs require judgment, and the decision comes from the owner. |
| Defer features until a real workload needs them | Workload demand and its design implications need project judgment. |
| Explain the background, tradeoffs and recommendation for each owner option | Understanding and faithfully explaining the options requires judgment. |
| Keep machine provisioning outside the public repository | Fleet layout and provisioning belong to external operations. Identifying sensitive information in arbitrary prose requires context. |
| Recheck missed worker exit notifications | Monitoring external jobs and notifications is a coordinator responsibility. |
| Distinguish mutation kills from toolchain failure and restore pristine binaries | The lesson's harness is frozen. Its checks would matter when an active mutation driver returns. |
| Negotiate language decisions before implementation | Only the owner can give an explicit disposition. Determining whether an implementation picks a side requires review. |
| Prefer one transparent form and keep semantic authority in the Lean spec | Deciding if two forms or descriptions express the same rule requires semantic judgment. |
| Discuss decisions in text without a popup | This is an interaction preference for tools outside the repository. |
| Let the owner review every change to language design, syntax or grammar | Identifying a semantic change and its approval scope requires owner process and review. |
| Speak in plain language about concrete changes | Clarity for the owner depends on the conversation and audience. |
| Read the proof obligation itself and assert the property under test | Interpreting the theorem and choosing the property that distinguishes a regression from a valid improvement require judgment. |
| Resume only workers stopped by a usage limit | Usage limits and session notifications belong to external agent orchestration. |
| Keep owner updates short and give each question context | Whether a message gives enough context requires conversational judgment. |
| Stay silent on no-op watchdog ticks | The watchdog and its chat response policy run outside the repository. |
| Prioritize specification work on the critical path | Allocation of quota and judgment about blockers are coordinator work. |
| Place heavy work on the designated CI capacity | Host availability and fleet assignment are external operations. |
| Delegate mechanical work when orchestration capacity is scarce | Quota and writer availability are external to the repository. |
| Accept verbosity where it improves clarity or safety | Evaluating clarity and safety requires design judgment. |

## Concurrent work: one worktree per writer

**The default: every concurrent line of work gets its own git worktree.**

```sh
scripts/worktree-new.sh <name> [base-committish]
```

This is not an optimization. One working tree cannot serve two writers, because
the file system is the shared mutable state and neither writer can see the
other's intent.

A worktree isolates its files, index and branch checkout. The object store,
remotes, branch namespace and stash are shared. Use distinct branch names and
avoid sharing a checkout with a running check that reads or writes its files.
CI watches the published revision, so work can continue in a separate worktree
while the queue runs.
