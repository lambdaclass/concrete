### CI Tiers, Self-Hosted Runners And A Shared Lean Build Seed (R-0540, R-0541, R-0546, 2026-10-22)

The CI workflow's jobs are in two tiers. The landing tier is what a merge candidate needs and runs
on every push to main, pull request and dispatch. The campaign tier (the mutation plan, its 24
shards and aggregate, the nightly differential fuzz, the clean-checkout proof replay and the
multi-kernel mutation step) runs only in a dispatch with `campaign=true`; it gates qualification,
not merges. The language-surface job, about 80 minutes in one job, is split by measured step time
into four jobs of about 20 minutes, so the critical path is `Build` then `Proof evidence gate`
(about 60 minutes, from about 85). The tiers are listed in the roadmap's promotion pipeline.

The Ubuntu jobs can run on the maintainers' servers. The repository variable
`CONCRETE_SELF_HOSTED` routes the landing tier, and `CONCRETE_SELF_HOSTED_CAMPAIGN` routes the
campaign tier separately. A `pull_request` event never runs on a self-hosted runner, whatever the
variables say, because the repository is public. Setup, security rules and how to switch the
routing off are in [SELF_HOSTED_RUNNERS.md](docs/project/SELF_HOSTED_RUNNERS.md).

On self-hosted runners CI seeds `.lake/build` from the server's build cache
(`scripts/dev/seed-lake.sh`). The seed key binds the toolchain, the Lake manifest, the platform
and the `lean` executable; a seed from an interrupted build or a checkout still being written is
refused, and `lake build` still verifies the result. `check_build_cache.sh` (CI) holds the
controls.
