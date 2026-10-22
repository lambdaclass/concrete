#!/usr/bin/env bash
# run-at-ref.sh <ref-or-sha> <command...>
# The server-local twin of the laptop's ~/bin/concrete-remote, for the landing bot running ON the
# gate host: fetch <ref> into the gate clone, run <command> in a fresh CLONE of it at that commit,
# inside `nix develop`, print `[concrete-remote] ... sha=<sha>` lines, exit with the command's status.
#
# A clone, not a worktree: some gates refuse to run in a linked worktree (the mutation campaign and
# its summary producer, check_phase6c_observability #5: "Run it in a clone"), so a full set run in
# a worktree is red for a reason that has nothing to do with the candidate. `clone --local`
# hardlinks the objects, so it costs about what a worktree costs.
#
# Env: RUN_AT_REF_REPO (gate clone, default ~/concrete2), RUN_AT_REF_RUNS (default ~/runs),
#      RUN_AT_REF_SOURCE (what to fetch from: a remote name or a path to a clone, default origin;
#      the bot passes its own clone so an unpublished train SHA can run), RUN_AT_REF_KEEP=1.
set -euo pipefail
if [ $# -lt 2 ]; then sed -n '2,14p' "$0"; exit 2; fi
REF="$1"; shift
CMD="$*"
REPO="${RUN_AT_REF_REPO:-$HOME/concrete2}"
RUNS="${RUN_AT_REF_RUNS:-$HOME/runs}"
SRC="${RUN_AT_REF_SOURCE:-origin}"
mkdir -p "$RUNS"
ID="$(date -u +%Y%m%dT%H%M%S)-$$"; WT="$RUNS/$ID"
cleanup() {
  if [ "${RUN_AT_REF_KEEP:-0}" = 1 ]; then echo "[concrete-remote] kept clone: $WT"; return 0; fi
  rm -rf "$WT"
}
{
  flock 9
  git -C "$REPO" fetch --quiet "$SRC" "$REF"
  SHA="$(git -C "$REPO" rev-parse FETCH_HEAD)"
  git -C "$REPO" update-ref "refs/run-at-ref/$ID" "$SHA"   # keeps the commit reachable for the clone
} 9>"$RUNS/.fetch.lock"
trap 'cleanup; git -C "$REPO" update-ref -d "refs/run-at-ref/$ID" 2>/dev/null || true' EXIT
trap 'exit 130' INT TERM HUP
git clone --quiet --local --no-checkout "$REPO" "$WT"
git -C "$WT" fetch --quiet origin "+refs/remotes/origin/*:refs/remotes/origin/*" "+refs/run-at-ref/$ID:refs/run-at-ref/$ID"
git -C "$WT" checkout --quiet --detach "$SHA"
echo "[concrete-remote] ref=$REF sha=$SHA clone=$WT host=$(hostname)"
if [ -d "$REPO/.lake" ]; then cp -a "$REPO/.lake" "$WT/.lake"; fi
cd "$WT"
START=$(date +%s)
RC=0
nix develop -c bash -c "$CMD" || RC=$?
echo "[concrete-remote] exit=$RC elapsed=$(( $(date +%s) - START ))s sha=$SHA"
exit "$RC"
