#!/usr/bin/env bash
# run-at-ref.sh <ref-or-sha> <command...>
# The server-local twin of the laptop's ~/bin/concrete-remote, run ON the gate host (by
# scripts/land/gates_remote.sh): fetch <ref> into the gate clone, run <command> in a fresh CLONE of it at that commit,
# inside `nix develop`, print `[concrete-remote] ... sha=<sha>` lines, exit with the command's status.
#
# A clone, not a worktree: some gates refuse to run in a linked worktree (the mutation campaign and
# its summary producer, check_phase6c_observability #5: "Run it in a clone"), so a full set run in
# a worktree is red for a reason that has nothing to do with the candidate. `clone --local`
# hardlinks the objects, so it costs about what a worktree costs. The gate clone ($REPO) is the
# local object source only (`--origin source`, removed after fetching); the execution checkout's
# `origin` is configured from the gate clone's canonical `origin` and validated with
# `land_check_destination` before <command> runs.
#
# Env: RUN_AT_REF_REPO (gate clone, default ~/concrete2), RUN_AT_REF_RUNS (default ~/runs),
#      RUN_AT_REF_SOURCE (what to fetch from: a remote name, a path to a clone or a git bundle,
#      default origin; scripts/land/gates_remote.sh passes a bundle so an unpublished train SHA can
#      run), RUN_AT_REF_KEEP=1, RUN_AT_REF_NO_NIX=1 (run the command
#      without `nix develop`: the landing gate's sandbox only).
set -euo pipefail
# The clone below is a sandbox: no repository variable of the caller may reach it (ISS-0015). The
# transport stages the helper beside this script at the same relative path.
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../checks/lib/git_sandbox.sh"
git_sandbox
# shellcheck source=scripts/land/lib.sh
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib.sh"
if [ $# -lt 2 ]; then sed -n '2,19p' "$0"; exit 2; fi
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
git clone --quiet --local --no-checkout --origin source "$REPO" "$WT"
git -C "$WT" fetch --quiet source "+refs/heads/*:refs/remotes/origin/*"
git -C "$WT" fetch --quiet source "+refs/remotes/origin/*:refs/remotes/origin/*" "+refs/run-at-ref/$ID:refs/run-at-ref/$ID"
git -C "$WT" remote remove source
while IFS= read -r u; do
  [ -n "$u" ] || continue
  if git -C "$WT" config --get "remote.$LAND_CANONICAL_REMOTE.url" >/dev/null 2>&1; then
    git -C "$WT" config --add "remote.$LAND_CANONICAL_REMOTE.url" "$u"
  else
    git -C "$WT" remote add "$LAND_CANONICAL_REMOTE" "$u"
  fi
done < <(git -C "$REPO" config --get-all "remote.$LAND_CANONICAL_REMOTE.url" 2>/dev/null || true)
while IFS= read -r u; do
  [ -n "$u" ] || continue
  git -C "$WT" config --add "remote.$LAND_CANONICAL_REMOTE.pushurl" "$u"
done < <(git -C "$REPO" config --get-all "remote.$LAND_CANONICAL_REMOTE.pushurl" 2>/dev/null || true)
git -C "$WT" checkout --quiet --detach "$SHA"
if [ -d "$REPO/.lake" ]; then cp -a "$REPO/.lake" "$WT/.lake"; fi
cd "$WT"
land_check_destination "run-at-ref" || exit 1
echo "[concrete-remote] ref=$REF sha=$SHA clone=$WT host=$(hostname)"
START=$(date +%s)
RC=0
if [ "${RUN_AT_REF_NO_NIX:-0}" = 1 ]; then bash -c "$CMD" || RC=$?
else nix develop -c bash -c "$CMD" || RC=$?; fi
echo "[concrete-remote] exit=$RC elapsed=$(( $(date +%s) - START ))s sha=$SHA"
exit "$RC"
