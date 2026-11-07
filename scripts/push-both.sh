#!/usr/bin/env bash
# Publish main: push the exact SHA of HEAD to the one repository's main and confirm it landed.
#
# ONE REPOSITORY (2026-11-06, the owner's decision: "migrating everything to lambdaclass/concrete").
# Until then this script pushed the gated primary (unbalancedparentheses/concrete2) and then
# fast-forwarded a mirror (lambdaclass/concrete) to exactly the primary's tip. There is no mirror
# now: `origin` is lambdaclass/concrete, and a clone whose origin is a GitHub URL naming another
# repository (above all the archived concrete2) is refused before anything is pushed. The name is
# kept so the documented command and the shared lock stay the same. Landings go through
# scripts/land/train.sh; this script is the by-hand publisher, and shares the train's lock.
#
# Kept from the mirror era, each written after a measured mistake:
#   * push the RECORDED SHA, never `HEAD` (HEAD moved during a CI wait and the wrong commit was
#     published);
#   * read the ref back with ls-remote: a push's exit status is not its result;
#   * the CI wait is OPT-IN (--ci-wait; owner, 2026-08-07: "stop waiting on CI in push-both"). Most
#     waits ended `cancelled` because a later push superseded the run. A cancelled run is NO
#     VERDICT, not a red one. The compensating control is the CI-health ritual: check
#     `gh run list --workflow CI` after pushing and treat red as stop-the-line;
#   * the CI query is by full 40-character COMMIT and `--event push`: a branch query races other
#     pushes, an abbreviated SHA returns [] instead of an error, and a scheduled run on the same SHA
#     masked the push run's verdict;
#   * the wait is bounded (CI_TIMEOUT, calibrated above the observed run time), and absence of a
#     run is reported as UNVERIFIED, never as success.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

# EXCLUSIVE, repo-wide. Two publishes at once is not hypothetical: on 2026-07-31
# two agents sharing a worktree both ran this script and both sat in the CI-wait
# loop for the same SHA. That instance was harmless (same commit, fast-forward,
# idempotent), but the general case is not — two runs can push different tips, and the
# train's land holds the same lock while it checks main and pushes.
#
# The lock lives in the COMMON git dir, not the worktree, because remotes are
# shared across worktrees: two different worktrees publishing at once is exactly
# the case a per-worktree lock would miss.
COMMON_GIT_DIR="$(git rev-parse --git-common-dir 2>/dev/null || echo .git)"
PUSH_LOCK="$COMMON_GIT_DIR/concrete-push-both.lock"
if ! mkdir "$PUSH_LOCK" 2>/dev/null; then
  echo "error: another push-both holds $PUSH_LOCK" >&2
  echo "       remotes are shared across worktrees, so two publishes can race." >&2
  echo "       Wait for it, or if no run is active:" >&2
  echo "         rmdir $PUSH_LOCK" >&2
  if [ -f "$PUSH_LOCK/owner" ]; then
    echo "       holder: $(cat "$PUSH_LOCK/owner" 2>/dev/null)" >&2
  fi
  exit 2
fi
printf 'pid=%s worktree=%s\n' "$$" "$(pwd)" > "$PUSH_LOCK/owner" 2>/dev/null || true
trap 'rm -f "$PUSH_LOCK/owner" 2>/dev/null; rmdir "$PUSH_LOCK" 2>/dev/null || true' EXIT

REMOTE="${PUSH_REMOTE:-origin}"
REPO="${LAND_REPO:-lambdaclass/concrete}"
if [ "$REMOTE" = "fork" ]; then
  echo "push-both: refusing the remote named fork (a divergent repository)" >&2
  exit 2
fi
if ! URL="$(git remote get-url "$REMOTE" 2>/dev/null)"; then
  echo "push-both: the remote '$REMOTE' is not configured. Nothing was pushed." >&2
  exit 1
fi
case "$URL" in
  *github.com[:/]*)
    path="${URL#*github.com[:/]}"; path="${path%/}"; path="${path%.git}"
    if [ "$(printf '%s' "$path" | tr 'A-Z' 'a-z')" != "$(printf '%s' "$REPO" | tr 'A-Z' 'a-z')" ]; then
      echo "push-both: $REMOTE is $URL, not github.com/$REPO. This clone points at another repository;" >&2
      echo "push-both: repoint it (git remote set-url $REMOTE https://github.com/$REPO.git). Nothing was pushed." >&2
      exit 1
    fi ;;
esac
BRANCH="${BRANCH:-main}"
LOCAL="$(git rev-parse HEAD)"
CI_WAIT=0
CI_WORKFLOW="${CI_WORKFLOW:-CI}"
# Bounded so this cannot hang a session forever; a timeout is reported as no verdict.
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
    --no-ci-wait) CI_WAIT=0; shift ;;          # retained: scripts may still pass it
    --ci-wait)    CI_WAIT=1; shift ;;          # opt in to blocking on the verdict
    *) echo "push-both: unknown option $1" >&2; exit 2 ;;
  esac
done

echo "push-both: ${LOCAL:0:8} to $REMOTE/$BRANCH ($URL)"
# Push the RECORDED SHA, not `HEAD`. `HEAD` is a moving ref: everything below —
# the primary verification, the CI wait, the parity check — is about $LOCAL, so
# pushing HEAD means publishing something other than what was checked.
if ! git push "$REMOTE" "$LOCAL:$BRANCH"; then
  echo "push-both: the push to $REMOTE failed." >&2
  exit 1
fi

# A successful exit is not the same as the ref being where we think.
got="$(git ls-remote "$REMOTE" "refs/heads/$BRANCH" | cut -f1)"
if [ "$got" != "$LOCAL" ]; then
  echo "push-both: $REMOTE/$BRANCH is $got, expected $LOCAL." >&2
  exit 1
fi

# ------------------------------------------------------------------
# Wait for CI on exactly this commit (opt-in).
# ------------------------------------------------------------------
if [ "$CI_WAIT" -eq 1 ]; then
  if ! command -v gh >/dev/null 2>&1; then
    echo "push-both: gh not available, cannot confirm CI for ${LOCAL:0:8}." >&2
      exit 1
  fi
  echo "push-both: waiting for $CI_WORKFLOW on ${LOCAL:0:8} (timeout ${CI_TIMEOUT}s)"
  deadline=$(( $(date +%s) + CI_TIMEOUT ))
  conclusion=""
  while :; do
    # Query by COMMIT, not by branch: a branch query races with anyone else's
    # push and could report a green run for a different tip.
    # FILTER TO THE PUSH EVENT. `--commit` alone matches ANY run for that SHA,
    # including `schedule`-triggered ones — and a nightly run sitting `pending`
    # on the same tip made this wait time out at 4200s while the push run had
    # already finished SUCCESS. Measured on efafe5e2: two schedule runs
    # (pending, in_progress) ahead of a completed push run in the same list.
    row="$(gh run list -R "$REPO" --workflow "$CI_WORKFLOW" --commit "$LOCAL" --event push \
             --limit 1 --json status,conclusion,url 2>/dev/null || true)"
    status="$(sed -n 's/.*"status":"\([^"]*\)".*/\1/p' <<<"$row")"
    conclusion="$(sed -n 's/.*"conclusion":"\([^"]*\)".*/\1/p' <<<"$row")"
    if [ "$status" = "completed" ]; then break; fi
    # An empty result is NOT a status. `gh run list --commit` requires the full
    # 40-char SHA: given an abbreviation it returns [] instead of erroring, so an
    # empty list is indistinguishable from "the run has not been created yet" and
    # from "the workflow name is wrong". Distinguish them explicitly rather than
    # letting all three fall through to a 'pending'-looking timeout.
    if [ -z "$status" ]; then
      if [ "${#LOCAL}" -ne 40 ]; then
        echo "push-both: internal error — CI query needs a full 40-char SHA, got '${LOCAL}'." >&2
        exit 1
      fi
      saw_no_run=1
    else
      saw_no_run=0
    fi
    if [ "$(date +%s)" -ge "$deadline" ]; then
      if [ "${saw_no_run:-0}" -eq 1 ]; then
        echo "push-both: no CI run found for ${LOCAL:0:8} on workflow '$CI_WORKFLOW' (event=push) within ${CI_TIMEOUT}s." >&2
        echo "push-both: treating absence as UNVERIFIED, not as success." >&2
        exit 1
      fi
      echo "push-both: CI for ${LOCAL:0:8} did not conclude within ${CI_TIMEOUT}s (status='${status:-none}')." >&2
      exit 1
    fi
    sleep "$CI_INTERVAL"
  done
  # A CANCELLED run is NO VERDICT, not a red one, and saying "red" sends the reader to
  # investigate a failure that did not happen. Both cancellations seen on 2026-08-06 were
  # environmental: one run superseded by a later push (the workflow sets
  # `cancel-in-progress` for pushes on a ref), one killed by a GitHub outage resolving
  # action downloads. Every job showed `cancelled`; nothing was compiled.
  #
  # Still exits non-zero — no verdict is not a pass — but it says
  # what happened and what to do, which is RE-RUN rather than debug.
  case "$conclusion" in
    success) : ;;
    cancelled|skipped|"")
      echo "push-both: CI for ${LOCAL:0:8} was '${conclusion:-absent}' — NO VERDICT, not a failure." >&2
      echo "push-both: a run is cancelled when a later push supersedes it, or when the" >&2
      echo "           runner cannot start (action-download outages look like this). Nothing" >&2
      echo "           was compiled, so this says nothing about the commit." >&2
      echo "push-both: re-run it — 'gh run rerun -R $REPO \$(gh run list -R $REPO --commit ${LOCAL} --limit 1 --json databaseId -q .[0].databaseId)'" >&2
      exit 1 ;;
    *)
      echo "push-both: CI for ${LOCAL:0:8} concluded '$conclusion'." >&2
      echo "push-both: main is red. Stop the line and fix it." >&2
      exit 1 ;;
  esac
  echo "push-both: CI green on ${LOCAL:0:8}"
else
  # Not waiting is the default now. Say so, and hand over the exact commands to check the
  # verdict later — an unwatched run is only acceptable if checking it is trivial.
  echo "push-both: not waiting for CI (default since 2026-08-07; --ci-wait to block)."
  echo "push-both: check it when convenient —"
  echo "    gh run list -R $REPO --workflow $CI_WORKFLOW --commit $LOCAL --event push --limit 1 \\"
  echo "      --json status,conclusion,url -q '.[0]|\"\\(.status) \\(.conclusion) \\(.url)\"'"
  echo "push-both: a 'cancelled' verdict is NO VERDICT — a later push superseded the run."
fi

echo "push-both: published ${LOCAL:0:8} on $REMOTE/$BRANCH"
