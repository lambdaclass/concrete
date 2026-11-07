- Sandboxes no longer inherit the caller's git environment (ISS-0015). One helper,
  `scripts/checks/lib/git_sandbox.sh`, unsets every variable `git rev-parse --local-env-vars` names;
  every script that makes a repository (`review_tool.sh`, `landing_script.sh`, `landing_bot.sh`,
  `legacy_frozen.sh`, `acceptance.sh`, `run-at-ref.sh`, `bot.sh`, `scripts/oracle/build.sh`,
  `scripts/proofs/external.sh`) calls it, and the ssh transport
  stages it beside `run-at-ref.sh`. New check `scripts/checks/sandbox_isolation.sh`: the structural
  rule, plus `review_tool.sh` and `legacy_frozen.sh` run from a linked worktree with `GIT_DIR`
  exported, bounded (120 s, and 512 tasks / 8G in a systemd user scope, or the finite memory.max of
  the cgroup it runs in plus a process limit; with neither the probes are INCOMPLETE), against a
  leaking control.
- Trains run only from a full clone with the repository hook enabled (ISS-0015): `train.sh` refuses
  a linked worktree and a clone whose pre-push hook is off or is not (a wrapper of)
  `.githooks/pre-push`; the pre-push hook refuses a gated push from a linked worktree (the `wip/*`
  and review-notes tiers still pass). The landing bot's rebase and `acceptance.sh`'s negative
  controls use scratch clones instead of worktrees.
