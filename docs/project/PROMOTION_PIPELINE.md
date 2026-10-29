# Validation and Promotion

Status: process — the stages a change passes during development, before merge and before
qualification or release, the CI tiers, nightly qualification, triage, flakes and the result
vocabulary. [ROADMAP.md](../../ROADMAP.md#releases) links here from its release section; Phase 1 exits
2f and 0c and the candidate table name these stages.

Moved verbatim from ROADMAP.md on 2026-10-28 (the roadmap compaction); only heading levels and relative links were adjusted.

## Promotion pipeline

A change is promoted through three stages. Each result is recorded against the exact commit SHA it
ran on, with passed, failed, skipped, cancelled and incomplete kept distinct: a timeout, skipped job or
cancelled run is never a pass. A result is reused for a later commit only with an explicit account of
why the difference cannot affect it.

| stage | when | what runs | recorded as | invalidated by |
|---|---|---|---|---|
| **(a) fast affected checks** | every push, during development | the [pre-push hook](../../.githooks/pre-push) (`make pre-push`): gate hygiene always; for a docs-only push (every changed path under `docs/` or ending `.md`) the docs-drift gate (which runs `check_roadmap_coverage.sh`) and `check_roadmap_linear.sh`; otherwise the fast surface gates, the bug-filing check when bug files change, and the gates and CI jobs of the touched areas, resolved by `run_ci_gates_local.sh`. Branch pushes also run the fast surface gates in the branch-health workflow | the hook's validation record (`scripts/tests/lib/prepush_record.sh`), bound to the pushed commit, a clean tree, untracked files, the toolchain and compiler binary, the gate definitions, the selection and the changed-file list | any change to one of those inputs; the hook reruns rather than reuse a mismatched record. A stage (a) pass never stands for (b) or (c) |
| **(b) integrated full gate set** | on the integrated candidate before it merges to main, and on main after each push | the CI workflow's ([`lean_action_ci.yml`](../../.github/workflows/lean_action_ci.yml)) push/pull-request jobs: CI on pushes to main and pull requests, or `gh workflow run CI --ref <branch>` for a branch (a dispatch runs the landing tier only; `-f campaign=true` adds the campaign tier, see [CI tiers](#ci-tiers)). This includes the Phase 6C observability capstone (`check_phase6c_observability.sh`) and, on pushes to main, the multi-kernel evidence gate. Locally, `scripts/tests/run_ci_gates_local.sh` extracts the same gate commands but excludes the mutation campaign, its aggregator and `checker_acceptance_evidence.sh`, and does not reproduce step-level environments; `make pre-push-full` runs it in parallel for feedback only. A local completion record needs closure mode (`CONCRETE_CLOSURE_RUN=1`, which forces `JOBS=1`) on a clean candidate and covers only that locally runnable population | the CI run ID and its head SHA; or a local closure-mode summary with `completed=1` and that head, which never stands in for the CI-only steps | any change outside documentation (compiler, `std`, examples, proofs, gate scripts, workflow, toolchain, lake or nix inputs) reruns the whole set. A docs-only change reruns only the gates that read the changed documents (docs drift and roadmap gates, links, and the hole-status, claims and axiom-inventory gates when those documents change) |
| **(c) full qualification** | on an immutable candidate: before a release candidate or nightly, and before closing an exit that names a qualified result | stage (b) on the same SHA, plus the jobs that run only on schedule or dispatch: the gate-mutation campaign (`check_gate_mutation_coverage.sh`, sharded by CI's mutation plan, shard and aggregate jobs on the nightly schedule on `main` of `lambdaclass/concrete` or a `workflow_dispatch` with `campaign=true`; it refuses a dirty tree and a git worktree, so a local campaign runs in a fresh clone of the candidate), the weekly clean-checkout proof replay, the nightly differential fuzz campaigns and the scheduled multi-kernel mutation coverage (`MULTI_KERNEL_MUTATE=1`); the con-leche external re-check already runs in (b) | one qualification verdict over **every** required job at the same candidate SHA: `scripts/ci/qualification_record.py` reads the record of exactly one run: the first attempt of a `workflow_dispatch` run of the candidate dispatched with `campaign=true` (the only event that starts every job; a dispatch without it skips the campaign tier and is refused by those skipped jobs; scheduled runs skip each other's jobs, partial runs are never combined, and a rerun could hide a failure) and emits the candidate SHA, each job of the candidate commit's own `lean_action_ci.yml` (read from that commit and parsed structurally; a workflow shape it does not understand is refused) with its run, head SHA, status and conclusion (each mutation shard 1..N individually), and `verdict=qualified` only when the run and every required job succeeded at that SHA and every unconditional step of each job, plus the conditional multi-kernel mutation coverage, ran exactly once and succeeded. The mutation aggregate's `qualified=1` (every planned shard completed) is one component of it, never the verdict: the aggregate succeeds while proof replay, fuzzing, the multi-kernel and checker checks or the regression suites fail. A missing, cancelled, skipped, failed or in-progress job or required step, a missing, duplicate or unexpected shard, a run at another SHA or with a non-success conclusion, or a failure hidden behind a successful rerun at the same SHA prevents qualification, and a workflow the parser does not understand is refused rather than read as fewer jobs (`check_qualification_record.sh` attacks the verdict with these records). A local closure-mode summary cannot qualify. The record, with its run IDs, is kept in R-0333's candidate record for a release and in the [candidate record](../../ROADMAP.md#implemented-not-yet-accepted) for an exit | any change outside documentation, including a rebase onto a new base; a docs-only change invalidates neither (b) nor (c) except their document-reading gates |

### CI tiers

The CI workflow's jobs fall in two tiers. Every job runs somewhere; none was dropped.

- The **landing tier** is what a merge candidate needs (stage (b)). It runs on every push to `main`,
  every pull request, every `workflow_dispatch` and both crons. Its jobs: `LL(1) grammar check`,
  `Build`, `Main test suite`, `Build + main tests (macOS)`, `SSA test suite`, `Trust gate`, `Proof
  evidence gate`, `External proof re-check (con-leche)`, the four language-surface jobs
  (`Language-surface gates (Phase 5-6)`, `Receipt-path gates`, `Checker-identity gates`,
  `Evidence-identity gates`; one ~80-minute job until 2026-10-22, split by measured step time into
  four of ~20 minutes), `Compiler-infra gates`, the two `Campaign gates` jobs (they test the campaign
  tooling with fixtures, not the campaign), `Thesis evidence smoke test`, `Examples`, `Extra gates`,
  and `Multi-kernel evidence` (on push, dispatch and schedule, and on a pull request labelled
  `multi-kernel`). The critical path is now `Build` then `Proof evidence gate` (~60 minutes), down
  from `Build` then the unsplit language job (~85 minutes).
- The **campaign tier** is stage (c) evidence and gates qualification, not merges. It runs only in a
  dispatch with `campaign=true`: `gh workflow run CI --ref <ref> -f campaign=true` for a candidate,
  and every night on `main` (below). Its jobs: `Gate mutation plan`, the 24 `Gate mutation shard N`
  jobs, `Gate mutation coverage (aggregate verdict)`, `Nightly differential fuzz`, `Weekly
  clean-checkout proof replay` (now nightly; the job keeps its name) and the `Multi-kernel mutation
  coverage` step of the multi-kernel job. The CI workflow has no cron of its own.

### Nightly qualification, triage and flakes

- **Nightly.** [`nightly.yml`](../../.github/workflows/nightly.yml) dispatches CI on `main` with
  `campaign=true` at 03:23 UTC, on `lambdaclass/concrete`, and on `unbalancedparentheses/concrete2`
  when the repository variable `CONCRETE_NIGHTLY_QUALIFICATION` is `true` (R-0468's opt-in). It
  reuses CI's jobs, so nothing runs twice, and a campaign dispatch is the one event
  `scripts/ci/qualification_record.py` accepts: every night yields a stage (c) verdict for `main`'s
  tip. When the run completes, the report job (`scripts/ci/nightly_report.py`) writes to its job
  summary the verdict, the gates that failed tonight but not last night, the triage of the run and
  the mutation-coverage diff against the previous night (`scripts/ci/coverage_diff.py`: a killed
  family that survives, or any family dropped, is a regression). The report is red unless the run
  qualified with no new failure and no lost coverage. Its jobs route to self-hosted runners under
  `CONCRETE_SELF_HOSTED_CAMPAIGN`, like the campaign jobs.
- **Triage.** `scripts/ci/triage.sh <run-id>` classifies each failed job as infra (runner lost, a
  cancellation, a network, apt or cache failure, or a bounded run's `INCOMPLETE` marker with no
  failed assertion), infra-defect (our tooling cannot build a pinned tool, e.g. lean4export under a
  leaked `GIT_DIR`: reported, never rerun, never a flake), known-flaky (a ledger row whose signature
  matches) or real. It reruns the failed jobs once (`gh run rerun --failed`) only when every failure
  is infra or known-flaky, never on attempt 2 and never for a run cancelled as a whole; otherwise it
  writes `.triage/<run>.md` with the gate, its FAIL lines, a local reproduction, the impact-selected
  command, the suspected commits and, when `scripts/gen/new_bug.sh` exists, the command to file a bug
  skeleton (printed, not run). A rerun is diagnostic: qualification reads only a first attempt.
- **Flakes.** `scripts/ci/flake_history.py` derives per-gate outcomes (passed, failed, skipped,
  cancelled, incomplete) from GitHub's run records on demand, so nothing is committed per run; the
  nightly keeps each reading as the `flake-history-<run>` artifact. A gate that failed and passed on
  one SHA is a flake candidate (infra and infra-defect failures excluded). The ledger
  [`docs/project/flaky_gates.tsv`](flaky_gates.tsv) is committed and human-curated:
  `known-flaky` and `quarantined` need an owner, a quarantine needs an expiry, and a quarantined
  gate still runs and still fails its job; triage reports it separately as failed and reruns it
  once, and after the expiry its failures are real again. `check_ci_triage.sh` pins all of this.

Runners: every Ubuntu job runs on GitHub-hosted `ubuntu-latest` unless the repository variable
`CONCRETE_SELF_HOSTED` (landing tier) or `CONCRETE_SELF_HOSTED_CAMPAIGN` (campaign tier) is `true`;
a `pull_request` event never runs on a self-hosted runner. The macOS job stays on `macos-latest`.
Setup and security rules: [SELF_HOSTED_RUNNERS.md](SELF_HOSTED_RUNNERS.md).

**Result vocabulary.** Every check reports one of five results, and they are never merged:

- **passed**: the check ran to completion and every assertion held;
- **failed**: it ran to completion and observed a wrong result;
- **incomplete**: it did not finish, so nothing was observed. A timeout is incomplete, not failed and
  not passed. A test that exceeds its budget is re-run once, alone, with a larger budget: a
  completed re-run is judged normally and recorded *recovered*; a second timeout is reported
  `INCOMPLETE`, and the timed-out run's partial output is discarded, so no output check can pass on
  it. `scripts/tests/lib/bounded.sh` implements this (a command's own exit 124 is told apart from a
  timeout; its behaviour is pinned by controls in `check_example_outputs.sh`) for the per-test
  budgets of `check_example_outputs.sh`, `check_callable_values.sh`,
  `check_hashmap_probe_invariants.sh`, `check_model_diff.sh` and `run_tests.sh`'s project fixtures,
  which count incomplete results apart and list recovered ones, and as the watchdog wrapper of the
  gates that bound single commands; `test_ssa.sh` applies the same rule to its parallel 5-second
  alarms (re-run alone with 30 s). In the seventeen watchdog-form gates a second timeout gives the
  wrapped command no output and status 124, and the gate's last line (`bounded_gate_status`) lists
  recovered and incomplete runs and fails the gate on any incomplete one, whatever its own checks
  concluded; their per-check tallies still count such a command among their failures, and R-0513
  owns moving them to per-check accounting. A timed-out command is killed with its whole process
  group (TERM, then KILL) before the retry starts. Fuzzers are the exception by
  design: a hang on a generated input is their finding, triaged like a crash;
- **skipped**: deliberately not run, with a recorded reason (a platform without the tool, an opt-in
  leg). A judgment a host cannot make soundly is skipped there, not approximated: the gate reports it
  NOT-JUDGED and exits 77, and `run_ci_gates_local.sh` records that as *deferred to CI* only for a
  gate `scripts/tests/lib/host_tools.sh` declares judged on CI (its `judged_on`) and CI runs
  unconditionally; CI itself must be able to judge it, or the gate fails there (bug 315: the scaling
  gate's wall verdicts on a host without instruction counters);
- **cancelled**: the run was stopped from outside (a superseded CI run).

A gate with an incomplete result exits nonzero, so incomplete never reads as green; its log names
the result for what it is, so it is not investigated as a wrong answer. A **qualifying run**
(stage (b) or (c), or a local closure-mode record) contains no failed, incomplete or cancelled
result, and a skipped result only where the stage's definition allows it. Machine load is a reason
to re-run a check, never to accept an incomplete one.

**How the stages run today.** CI is the validator; local runs give feedback.

1. **Development.** Work happens on a candidate branch in its own worktree
   ([CONCURRENT_WORK.md](CONCURRENT_WORK.md)). Work in progress is backed up to
   `wip/<name>` every few minutes; the pre-push hook's wip tier checks only for conflict markers, so
   a usage limit or crash never loses work, and a `wip/*` ref is never a candidate.
2. **Stage (a), the fast local tier.** The pre-push hook (`.githooks/pre-push`) runs the selected
   fast gates on every push of a non-`wip` ref and records its result against the pushed commit.
3. **Stage (b) and (c), CI.** The candidate is pushed to `ci/<name>` (full hook) and the CI workflow
   is dispatched on it (`gh workflow run CI --ref ci/<name>`). A dispatched run starts every job,
   the qualification-only jobs included, so one run yields both the regular verdict and the
   qualification record. Failures are diagnosed from the run's logs and repaired on the candidate
   branch; the corrected SHA is pushed and dispatched again.
4. **Landing.** When every regular job of the run is green at the candidate SHA, main is
   fast-forwarded to exactly that SHA on both remotes (`origin` and `lambdaclass`). A candidate that
   is not a fast-forward of main is rebased, which is a new SHA and needs its own run. A
   qualification job that fails or does not finish blocks the closures that need a qualified
   result, not the landing of a change whose regular jobs are green.
5. **Heavy campaigns.** Fuzzing hours, mutation campaigns and other long runs go to remote servers
   when they are available, and otherwise queue behind each other locally; their results are
   recorded against the exact SHA they ran on, like any other.

R-0513 owns making stage (a) a dependency-closure-selected fast tier within its budget and keeping
stage (c) bound to exact commits, toolchains and artifacts (Phase 1 exit 2f); R-0516 automates (c)'s
record for releases (today `qualification_record.py` is run by hand against the run IDs).

**Trains and tiers (2026-10-22, [development throughput](../../ROADMAP.md#development-throughput)).** Ready branches
are meant to land as one **train**: one integrated candidate, one CI run, and bisection by member only
when that run is red (R-0539). CI is split into two tiers that follow the stages above:

- the **landing tier** is stage (b)'s required job set for merge, with a target median of ≤ 40 min
  (R-0540, branch `ci-tiering`);
- the **campaign tier** is the set of jobs that run only on schedule or dispatch, the mutation
  campaign among them. It is part of stage (c), so it gates qualification but not merges.

Every result is recorded against the SHA of the candidate that actually ran, which is the train or a
bisection candidate. Attribution of a failure to a member is a separate field of that record. A
member's own SHA receives a result only from a run at that SHA, and a landed member's record names
the train candidate whose result it relies on. A member removed by bisection keeps the failure that
was attributed to it. The landing tier never stands in for stage
(c), and a stage (c) verdict still needs every job of one dispatch run. Passed, failed, skipped,
cancelled and incomplete stay distinct throughout: a cancelled or incomplete train run is not a pass
for any member.
