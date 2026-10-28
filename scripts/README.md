# Scripts

Repository scripts live here instead of the project root.

- [`tests/`](./tests/README.md) contains test runners and test-maintenance helpers.
- `check_ll1.{py,c,rs}` are three deliberately independent LL(1) checkers for
  `grammar/concrete.ebnf` (a diverse-implementation cross-check, added together in ced8b114).
  They share no code: each parses the EBNF and computes nullability and FIRST sets on its own.
  Run them through `scripts/tests/check_grammar_ll1.sh` (`make check-grammar`, CI's grammar job),
  which requires their `--canonical` dumps to be byte-identical on the real grammar and all three to
  refuse the planted conflicts in `tests/fixtures/grammar_ll1/`. A checker that only prints "passed"
  is an opinion; the gate turns three opinions into one cross-checked claim. Change all three
  together: a semantic change in one of them is a disagreement the gate reports.
- [`perf/`](./perf/runner.sh) tracks compiler performance (retired instructions, peak RSS) per origin/main commit; results go on the `perf-data` branch, never merged. See docs/project/PERF_TRACKING.md, which also lists the server provisioning that lives outside this repository.

Rule of thumb:

- put reusable developer scripts in `scripts/`
- put test entrypoints in `scripts/tests/`
- keep the repo root for project entrypoints and major top-level directories only

## Concurrent work

Every concurrent line of work gets its OWN git worktree:

```sh
scripts/worktree-new.sh <name> [base-committish]
```

One working tree cannot serve two writers. See `docs/project/CONCURRENT_WORK.md` for what
a worktree does and does not isolate (remotes, the stash stack and the branch
namespace stay shared), and for why the concurrency guards live in
`test_mutation.sh` and `push-both.sh` rather than in a head-count heuristic.
