#!/usr/bin/env bash
# Create an ISOLATED worktree for one line of work. This is the default way to
# start work in this repo, not an optimization.
#
# Why it is the default: on 2026-07-31 two agents were found working in the SAME
# worktree (`.claude/worktrees/r0442-proofcore-callee`). The symptoms were all
# read as something else at the time —
#
#   * a source edit to ProofCore.lean vanished and the file dropped out of
#     `git status` (the mutation harness restored a backup over a concurrent
#     edit, then verified against the PRE-edit hash and reported success);
#   * `origin/main` appeared to move on its own three times mid-session;
#   * two `push-both.sh` runs waited on CI for the same SHA.
#
# One working tree cannot serve two writers: the file system is the shared
# mutable state, and neither writer can see the other's intent.
#
# What a worktree does and does NOT isolate:
#   ISOLATED  — the working tree, the index, HEAD, the current branch.
#   SHARED    — the object store, remotes, the STASH STACK, branch namespace.
# So pushes still have to be sequenced and `git stash` is still unsafe (bare
# stash is banned repo-wide for exactly this reason). Worktrees fix file-level
# races, not coordination.
#
# Usage:
#   scripts/worktree-new.sh <name> [base-committish]
#
# Creates ../<name> alongside the current worktree, on a new branch <name>,
# based on the current HEAD unless a base is given.
set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

NAME="${1:-}"
BASE="${2:-HEAD}"
if [ -z "$NAME" ]; then
  echo "usage: scripts/worktree-new.sh <name> [base-committish]" >&2
  exit 2
fi
case "$NAME" in
  */*|.*) echo "error: <name> must be a plain directory name" >&2; exit 2 ;;
esac

# Worktrees live beside each other under .claude/worktrees/ when we are already
# in one; otherwise under .claude/worktrees/ from the repo root.
PARENT="$(dirname "$ROOT_DIR")"
if [ "$(basename "$PARENT")" = "worktrees" ]; then
  DEST="$PARENT/$NAME"
else
  DEST="$ROOT_DIR/.claude/worktrees/$NAME"
fi

if [ -e "$DEST" ]; then
  echo "error: $DEST already exists" >&2
  exit 2
fi

# A branch name is a SHARED resource across worktrees — the namespace is not
# isolated. Refuse rather than let git fail halfway, and say why.
if git show-ref --verify --quiet "refs/heads/$NAME"; then
  echo "error: branch '$NAME' already exists (branch names are shared across worktrees)." >&2
  echo "       another line of work — possibly another agent's — may own it." >&2
  echo "       Existing worktrees:" >&2
  git worktree list >&2
  exit 2
fi

BASE_SHA="$(git rev-parse --verify "$BASE" 2>/dev/null)" || {
  echo "error: cannot resolve base '$BASE'" >&2; exit 2; }

echo "worktree-new: $NAME"
echo "  path   : $DEST"
echo "  branch : $NAME (new)"
echo "  base   : ${BASE_SHA:0:8} ($BASE)"
git worktree add "$DEST" -b "$NAME" "$BASE_SHA" || exit 1

echo ""
echo "worktree-new: ready. cd $DEST"
echo "worktree-new: run all commands from there — do NOT cd back to $(basename "$ROOT_DIR")."

# DELIBERATELY NOT WARNING about other processes in the tree. Two attempts were
# measured and both cried wolf:
#
#   * `pgrep -lf "codex|claude"` reported 115 — every shell wrapper's command
#     line contains the `.claude` snapshot path.
#   * counting processes whose CWD is the tree reported 7 in a worktree freshly
#     created by a single agent — an agent's own shells, background waiters and
#     tool invocations all sit there.
#
# Neither distinguishes "two writers" from "one agent working normally", and a
# warning that fires on non-events trains the reader to ignore the one that
# matters. So concurrency is caught at the POINT OF DANGER instead, where the
# signal is exact and needs no heuristic:
#
#   * test_mutation.sh compares on-disk content against what it itself wrote, so
#     a foreign edit is a fact rather than an inference — and it refuses to
#     restore over it.
#   * push-both.sh takes an exclusive lock in the COMMON git dir, so two
#     publishes cannot interleave even from different worktrees.
#
# This script's job is to make isolation the cheap default, not to police it.
