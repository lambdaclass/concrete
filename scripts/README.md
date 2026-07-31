# Scripts

Repository scripts live here instead of the project root.

- [`tests/`](./tests/README.md) contains test runners and test-maintenance helpers.
- `check_ll1.*` contains grammar-check helpers for the parser.

Rule of thumb:

- put reusable developer scripts in `scripts/`
- put test entrypoints in `scripts/tests/`
- keep the repo root for project entrypoints and major top-level directories only

## Concurrent work

Every concurrent line of work gets its OWN git worktree:

```sh
scripts/worktree-new.sh <name> [base-committish]
```

One working tree cannot serve two writers. See `docs/CONCURRENT_WORK.md` for what
a worktree does and does not isolate (remotes, the stash stack and the branch
namespace stay shared), and for why the concurrency guards live in
`test_mutation.sh` and `push-both.sh` rather than in a head-count heuristic.
