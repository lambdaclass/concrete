- Sandboxes no longer inherit the caller's git environment (ISS-0015). One helper,
  `scripts/checks/lib/git_sandbox.sh`, unsets every variable `git rev-parse --local-env-vars` names;
  every script that makes a repository (`review_tool.sh`, `landing_script.sh`, `landing_bot.sh`,
  `legacy_frozen.sh`, `acceptance.sh`, `run-at-ref.sh`, `bot.sh`) calls it, and the ssh transport
  stages it beside `run-at-ref.sh`. New check `scripts/checks/sandbox_isolation.sh`: the structural
  rule, plus `review_tool.sh` and `legacy_frozen.sh` run from a linked worktree with `GIT_DIR`
  exported, bounded (timeout 120, 512 tasks, 8G), against a leaking control.
