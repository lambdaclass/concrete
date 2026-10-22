#!/usr/bin/env bash
# run-at-ref.sh <ref-or-sha> <command...>
# The server-local twin of the laptop's ~/bin/concrete-remote, for the landing bot running ON the
# gate host: fetch <ref> into the gate clone, run <command> in a fresh detached worktree inside
# `nix develop`, print `[concrete-remote] ... sha=<sha>` lines, exit with the command's status.
#
# Env: RUN_AT_REF_REPO (gate clone, default ~/concrete2), RUN_AT_REF_RUNS (default ~/runs),
#      RUN_AT_REF_SOURCE (what to fetch from: a remote name or a path to a clone, default origin;
#      the bot passes its own clone so an unpublished train SHA can run), RUN_AT_REF_KEEP=1.
set -euo pipefail
if [ $# -lt 2 ]; then sed -n '2,10p' "$0"; exit 2; fi
REF="$1"; shift
CMD="$*"
REPO="${RUN_AT_REF_REPO:-$HOME/concrete2}"
RUNS="${RUN_AT_REF_RUNS:-$HOME/runs}"
SRC="${RUN_AT_REF_SOURCE:-origin}"
mkdir -p "$RUNS"
ID="$(date -u +%Y%m%dT%H%M%S)-$$"; WT="$RUNS/$ID"
cleanup() {
  if [ "${RUN_AT_REF_KEEP:-0}" = 1 ]; then echo "[concrete-remote] kept worktree: $WT"; return 0; fi
  git -C "$REPO" worktree remove --force "$WT" >/dev/null 2>&1 || rm -rf "$WT"
  git -C "$REPO" worktree prune
}
{
  flock 9
  git -C "$REPO" fetch --quiet "$SRC" "$REF"
  SHA="$(git -C "$REPO" rev-parse FETCH_HEAD)"
  git -C "$REPO" worktree add --quiet --detach "$WT" "$SHA"
} 9>"$RUNS/.fetch.lock"
trap cleanup EXIT
trap 'exit 130' INT TERM HUP
echo "[concrete-remote] ref=$REF sha=$SHA worktree=$WT host=$(hostname)"
if [ -d "$REPO/.lake" ]; then cp -a "$REPO/.lake" "$WT/.lake"; fi
cd "$WT"
START=$(date +%s)
RC=0
nix develop -c bash -c "$CMD" || RC=$?
echo "[concrete-remote] exit=$RC elapsed=$(( $(date +%s) - START ))s sha=$SHA"
exit "$RC"
