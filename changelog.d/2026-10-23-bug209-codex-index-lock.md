### Bug 209: A Codex Review No Longer Leaves A Stale `index.lock` (2026-10-23)

A `codex exec --sandbox read-only` review runs git in the caller's worktree, and its `git status`
took the worktree's `index.lock` to refresh the index; the sandbox did not prevent it. A reviewer
command stopped mid-refresh left the lock behind, 0 bytes, and the next commit there failed.
`scripts/review/review.sh` and the by-hand `/creview` rounds now run Codex with
`GIT_OPTIONAL_LOCKS=0`, so git reads the index without writing it. `check_review_tool.sh` requires
every reviewer call to see the variable. Structural class 16 (artifacts at paths no run owns).
