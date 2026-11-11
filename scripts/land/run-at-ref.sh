#!/usr/bin/env bash
# run-at-ref.sh <ref-or-sha> <command...>
# The server-local twin of the laptop's ~/bin/concrete-remote, run ON the gate host (by
# scripts/land/gates_remote.sh): fetch <ref> into the gate clone, run <command> in a fresh CLONE of it at that commit,
# inside `nix develop`, print `[concrete-remote] ... sha=<sha>` lines, exit with the command's status.
#
# A clone, not a worktree: some gates refuse to run in a linked worktree (the mutation campaign and
# its summary producer, check_phase6c_observability #5: "Run it in a clone"), so a full set run in
# a worktree is red for a reason that has nothing to do with the candidate. `clone --local`
# hardlinks the objects, so it costs about what a worktree costs.
#
# Env: RUN_AT_REF_REPO (gate clone, default ~/concrete2), RUN_AT_REF_RUNS (default ~/runs),
#      RUN_AT_REF_SOURCE (canonical repository URL or a verified git bundle file,
#      default https://github.com/lambdaclass/concrete.git; relative bundles resolve against the caller.
#      scripts/land/gates_remote.sh passes a bundle so an unpublished train SHA can
#      run), RUN_AT_REF_KEEP=1, RUN_AT_REF_NO_NIX=1 (run the command
#      without `nix develop`: the landing gate's sandbox only).
set -euo pipefail
# The clone below is a sandbox: no repository variable of the caller may reach it (ISS-0015). The
# transport stages the helper beside this script at the same relative path.
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../checks/lib/git_sandbox.sh"
git_sandbox
if [ $# -lt 2 ]; then sed -n '2,16p' "$0"; exit 2; fi
REF="$1"; shift
CMD="$*"
REPO="${RUN_AT_REF_REPO:-$HOME/concrete2}"
RUNS="${RUN_AT_REF_RUNS:-$HOME/runs}"
SRC="${RUN_AT_REF_SOURCE:-https://github.com/lambdaclass/concrete.git}"
# Freeze caller-relative directories before entering the run clone (including cleanup).
REPO="$(cd "$REPO" && pwd -P)"
RUNS="$(python3 -I -c 'import os,sys; print(os.path.abspath(sys.argv[1]))' "$RUNS")"
# Validate before creating run state; bundle paths are caller-relative.
SOURCE_HELPER="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/bundle_source.py"
SRC="$(python3 -I "$SOURCE_HELPER" "$SRC" "$REPO" "$REF")" || exit 1
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
if [ "${RUN_AT_REF_NO_NIX:-0}" = 1 ]; then bash -c "$CMD" || RC=$?
else nix develop -c bash -c "$CMD" || RC=$?; fi
echo "[concrete-remote] exit=$RC elapsed=$(( $(date +%s) - START ))s sha=$SHA"
exit "$RC"
