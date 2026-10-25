### Working Rules While Validation Runs, And A Lean-Backend Research Proposal (2026-10-22)

The project's [CLAUDE.md](CLAUDE.md) states how work continues while remote CI or a push hook
runs: continue the next authorized task on a separate branch and worktree; keep the candidate under
validation fixed; run at most two heavy runs per machine (R-0543); inspect the logs before
diagnosing a CI failure; and record results against exact SHAs, keeping passed, failed, skipped,
cancelled and incomplete apart.

[lean-backend-architecture.md](research/compiler/lean-backend-architecture.md) is a research
proposal for the standalone `lean-compiler-backend` project: keep full QBE 1.3 compatibility as its
first complete milestone while establishing function isolation, explicit semantics, typed lowering
boundaries and reproducible artifacts, with pure compile and verify operations per function and a
checked artifact that records exactly which properties were established. It changes neither that
project's roadmap nor Concrete's implementation.
