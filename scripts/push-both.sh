#!/usr/bin/env bash
# Push to the gated primary FIRST, then fast-forward the mirror — never both at
# once, and never the mirror alone.
#
# On 2026-07-30 `lambdaclass/main` sat at 2b8c831b while `origin/main` was still
# at 4dd6232f: the mirror push skips gates (CONCRETE_SKIP_GATES=1) and completed
# while origin's hook was still running. The mirror was therefore carrying a
# commit the primary had not accepted. If origin's gates had FAILED, the mirror
# would be advertising code the primary rejected — and remote parity is process
# state, so nothing in the compiler would have said so.
#
# Order is the whole point: primary gates -> primary lands -> primary CI is
# GREEN -> mirror fast-forwards to exactly the primary's tip.
#
# Why the mirror waits for remote CI and not just the local hook: the hook runs a
# SUBSET of CI, and this tree has been bitten by exactly that gap — CI sat
# silently dead for 40+ pushes, and when it was resurrected it immediately
# exposed two real regressions the local suite had passed. The mirror is what
# other people consume, so publishing a tip that CI then rejects advertises
# broken code, and a fast-forward cannot retract it. Waiting is cheap and rare;
# unpublishing is neither.
#
# `--no-ci-wait` exists for when that judgement does not apply, but it has to be
# passed deliberately and it says what it skipped.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

# EXCLUSIVE, repo-wide. Two publishes at once is not hypothetical: on 2026-07-31
# two agents sharing a worktree both ran this script and both sat in the CI-wait
# loop for the same SHA. That instance was harmless (same commit, fast-forward,
# idempotent), but the general case is not — two runs can push different tips and
# then race to mirror, and the mirror would end up at whichever finished last
# rather than at what the primary accepted.
#
# The lock lives in the COMMON git dir, not the worktree, because remotes are
# shared across worktrees: two different worktrees publishing at once is exactly
# the case a per-worktree lock would miss.
COMMON_GIT_DIR="$(git rev-parse --git-common-dir 2>/dev/null || echo .git)"
PUSH_LOCK="$COMMON_GIT_DIR/concrete-push-both.lock"
if ! mkdir "$PUSH_LOCK" 2>/dev/null; then
  echo "error: another push-both holds $PUSH_LOCK" >&2
  echo "       remotes are shared across worktrees, so two publishes can race to" >&2
  echo "       mirror different tips. Wait for it, or if no run is active:" >&2
  echo "         rmdir $PUSH_LOCK" >&2
  if [ -f "$PUSH_LOCK/owner" ]; then
    echo "       holder: $(cat "$PUSH_LOCK/owner" 2>/dev/null)" >&2
  fi
  exit 2
fi
printf 'pid=%s worktree=%s\n' "$$" "$(pwd)" > "$PUSH_LOCK/owner" 2>/dev/null || true
trap 'rm -f "$PUSH_LOCK/owner" 2>/dev/null; rmdir "$PUSH_LOCK" 2>/dev/null || true' EXIT

PRIMARY="${PRIMARY:-origin}"
MIRROR="${MIRROR:-lambdaclass}"
BRANCH="${BRANCH:-main}"
LOCAL="$(git rev-parse HEAD)"
CI_WAIT=1
CI_WORKFLOW="${CI_WORKFLOW:-CI}"
# Bounded so this cannot hang a session forever; on timeout the mirror is left
# alone, which is the fail-closed direction.
#
# CALIBRATED against observed runs, not guessed. A first version used 2400s
# (40min) and timed out on a run that was still in progress — this workflow's
# recent full runs took 41m42s and 42m39s, so the timeout was SHORTER than the
# job it was waiting for. A timeout below the expected duration does not bound a
# hang, it manufactures a failure on every healthy push. 4200s leaves real
# headroom above ~43min while still being finite.
CI_TIMEOUT="${CI_TIMEOUT:-4200}"
CI_INTERVAL="${CI_INTERVAL:-30}"

while [ $# -gt 0 ]; do
  case "$1" in
    --no-ci-wait) CI_WAIT=0; shift ;;
    *) echo "push-both: unknown option $1" >&2; exit 2 ;;
  esac
done

echo "push-both: $PRIMARY (gated) then $MIRROR (fast-forward)"
# Push the RECORDED SHA, not `HEAD`. `HEAD` is a moving ref: everything below —
# the primary verification, the CI wait, the parity check — is about $LOCAL, so
# pushing HEAD means publishing something other than what was checked.
if ! git push "$PRIMARY" "$LOCAL:$BRANCH"; then
  echo "push-both: PRIMARY push failed — mirror deliberately NOT touched." >&2
  exit 1
fi

# Confirm the primary actually has our tip before mirroring. A successful exit is
# not the same as the ref being where we think.
got="$(git ls-remote "$PRIMARY" "refs/heads/$BRANCH" | cut -f1)"
if [ "$got" != "$LOCAL" ]; then
  echo "push-both: $PRIMARY/$BRANCH is $got, expected $LOCAL — mirror NOT touched." >&2
  exit 1
fi

# ------------------------------------------------------------------
# Wait for the PRIMARY's CI on exactly this commit.
# ------------------------------------------------------------------
if [ "$CI_WAIT" -eq 1 ]; then
  if ! command -v gh >/dev/null 2>&1; then
    echo "push-both: gh not available, cannot confirm CI for ${LOCAL:0:8} — mirror NOT touched." >&2
    echo "push-both: re-run with --no-ci-wait to publish without that confirmation." >&2
    exit 1
  fi
  echo "push-both: waiting for $CI_WORKFLOW on ${LOCAL:0:8} (timeout ${CI_TIMEOUT}s)"
  deadline=$(( $(date +%s) + CI_TIMEOUT ))
  conclusion=""
  while :; do
    # Query by COMMIT, not by branch: a branch query races with anyone else's
    # push and could report a green run for a different tip.
    row="$(gh run list --workflow "$CI_WORKFLOW" --commit "$LOCAL" \
             --limit 1 --json status,conclusion,url 2>/dev/null || true)"
    status="$(sed -n 's/.*"status":"\([^"]*\)".*/\1/p' <<<"$row")"
    conclusion="$(sed -n 's/.*"conclusion":"\([^"]*\)".*/\1/p' <<<"$row")"
    if [ "$status" = "completed" ]; then break; fi
    if [ "$(date +%s)" -ge "$deadline" ]; then
      echo "push-both: CI for ${LOCAL:0:8} did not conclude within ${CI_TIMEOUT}s (status='${status:-none}') — mirror NOT touched." >&2
      exit 1
    fi
    sleep "$CI_INTERVAL"
  done
  if [ "$conclusion" != "success" ]; then
    echo "push-both: CI for ${LOCAL:0:8} concluded '$conclusion' — mirror NOT touched." >&2
    echo "push-both: $PRIMARY is red. Stop the line and fix it before publishing." >&2
    exit 1
  fi
  echo "push-both: CI green on ${LOCAL:0:8}"
else
  echo "push-both: --no-ci-wait — publishing to $MIRROR WITHOUT remote CI confirmation." >&2
fi

# The mirror gets exactly the primary's tip. Gates are skipped because they
# already ran for this exact commit on the primary; running them twice proves
# nothing and doubles the wait.
# Fast-forward ONLY, and say so before attempting it. If the mirror holds a
# commit we do not contain, that is the exact anomaly this script was written
# after — a mirror carrying work the primary never accepted. Report it as such
# instead of letting a bare push failure stand in for the diagnosis.
mnow="$(git ls-remote "$MIRROR" "refs/heads/$BRANCH" | cut -f1)"
if [ -n "$mnow" ] && ! git merge-base --is-ancestor "$mnow" "$LOCAL" 2>/dev/null; then
  echo "push-both: $MIRROR/$BRANCH is at ${mnow:0:8}, which is NOT an ancestor of ${LOCAL:0:8}." >&2
  echo "push-both: the mirror holds commits the primary does not. Reconcile before publishing;" >&2
  echo "push-both: do NOT force-push the mirror." >&2
  exit 1
fi

# $LOCAL, emphatically not HEAD. MEASURED FAILURE: this line pushed `HEAD` while
# the CI wait above had been polling $LOCAL. A commit made in the same worktree
# during the ~45min wait moved HEAD, so the mirror received a30a18f9 — a commit
# the primary did not have and CI never validated — leaving the MIRROR AHEAD OF
# THE PRIMARY, the exact anomaly this script was written to prevent. The parity
# check below caught it, but only after publication.
#
# Re-verify first: if HEAD has moved, say so, because it means the caller kept
# working and the SHA being mirrored is deliberately not their latest.
head_now="$(git rev-parse HEAD)"
if [ "$head_now" != "$LOCAL" ]; then
  echo "push-both: NOTE HEAD has moved to ${head_now:0:8} since this run began." >&2
  echo "push-both: mirroring ${LOCAL:0:8} — the commit the primary accepted and CI validated." >&2
fi
if ! CONCRETE_SKIP_GATES=1 git push "$MIRROR" "$LOCAL:$BRANCH"; then
  echo "push-both: mirror push failed; $PRIMARY is correct and ahead." >&2
  exit 1
fi

p="$(git ls-remote "$PRIMARY" "refs/heads/$BRANCH" | cut -f1)"
m="$(git ls-remote "$MIRROR" "refs/heads/$BRANCH" | cut -f1)"
if [ "$p" = "$m" ] && [ "$p" = "$LOCAL" ]; then
  echo "push-both: parity at ${LOCAL:0:8}"
else
  echo "push-both: NO PARITY — $PRIMARY=$p $MIRROR=$m local=$LOCAL" >&2
  exit 1
fi
