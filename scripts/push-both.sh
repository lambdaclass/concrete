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
# THAT REASONING EXPIRED (2026-08-07, owner's instruction: "stop waiting on CI in
# push-both", "we don't really need the CI to say things are fine, we can check later and
# keep going"). Two things changed under it:
#
#   * THERE WAS NO MIRROR. `MIRROR` defaulted to empty from 2026-07-31 to 2026-10-22, and the
#     mirror push was the only thing the wait gated. It held the exclusive lock for up to 70
#     minutes to protect a publication target that did not exist then. (The mirror is required
#     again since 2026-10-22, below; the wait stays opt-in, and the mirror still receives only
#     the exact commit the primary's hook accepted.)
#   * MOST RUNS NEVER REACH A VERDICT ANYWAY. The workflow sets `cancel-in-progress` for
#     pushes on a ref, so when two people push to main the earlier run is cancelled. Three
#     of the last four waits ended in `cancelled` — 70 minutes of held lock for no answer.
#
# So the wait is now OPT-IN (`--ci-wait`). What is lost is real and named: nothing stops
# main going red unnoticed. The compensating control is the existing CI-health ritual —
# check `gh run list --workflow CI` after pushing and treat red as stop-the-line. The run
# URL is printed below so that check is one click, not an investigation.
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
# BOTH REMOTES ARE REQUIRED (2026-10-22, the owner's decision: every landing goes to BOTH
# origin = unbalancedparentheses/concrete2 and lambdaclass = lambdaclass/concrete). This
# supersedes the 2026-07-31 instruction "stop pushing to lambdaclass", under which MIRROR
# defaulted to empty. `lambdaclass` is a DIFFERENT repository, so it is still pushed SECOND,
# fast-forward only, and only with the exact commit the primary accepted; the ancestry and
# parity checks below are unchanged. scripts/land/train.sh land mirrors the same way.
# `MIRROR=` (explicitly empty) skips the mirror for a one-off and says so; the remote named
# `fork` is a divergent repository and is never a mirror.
MIRROR="${MIRROR-lambdaclass}"
if [ "$MIRROR" = "fork" ]; then
  echo "push-both: refusing MIRROR=fork (a divergent repository)" >&2
  exit 2
fi
BRANCH="${BRANCH:-main}"
LOCAL="$(git rev-parse HEAD)"
CI_WAIT=0
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
    --no-ci-wait) CI_WAIT=0; shift ;;          # retained: scripts may still pass it
    --ci-wait)    CI_WAIT=1; shift ;;          # opt in to blocking on the verdict
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
    # FILTER TO THE PUSH EVENT. `--commit` alone matches ANY run for that SHA,
    # including `schedule`-triggered ones — and a nightly run sitting `pending`
    # on the same tip made this wait time out at 4200s while the push run had
    # already finished SUCCESS. Measured on efafe5e2: two schedule runs
    # (pending, in_progress) ahead of a completed push run in the same list.
    row="$(gh run list --workflow "$CI_WORKFLOW" --commit "$LOCAL" --event push \
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
        echo "push-both: treating absence as UNVERIFIED, not as success — mirror NOT touched." >&2
        exit 1
      fi
      echo "push-both: CI for ${LOCAL:0:8} did not conclude within ${CI_TIMEOUT}s (status='${status:-none}') — mirror NOT touched." >&2
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
  # Still exits non-zero — no verdict is not permission to publish the mirror — but it says
  # what happened and what to do, which is RE-RUN rather than debug.
  case "$conclusion" in
    success) : ;;
    cancelled|skipped|"")
      echo "push-both: CI for ${LOCAL:0:8} was '${conclusion:-absent}' — NO VERDICT, not a failure." >&2
      echo "push-both: a run is cancelled when a later push supersedes it, or when the" >&2
      echo "           runner cannot start (action-download outages look like this). Nothing" >&2
      echo "           was compiled, so this says nothing about the commit." >&2
      echo "push-both: re-run it — 'gh run rerun \$(gh run list --commit ${LOCAL} --limit 1 --json databaseId -q .[0].databaseId)'" >&2
      echo "push-both: mirror NOT touched." >&2
      exit 1 ;;
    *)
      echo "push-both: CI for ${LOCAL:0:8} concluded '$conclusion' — mirror NOT touched." >&2
      echo "push-both: $PRIMARY is red. Stop the line and fix it before publishing." >&2
      exit 1 ;;
  esac
  echo "push-both: CI green on ${LOCAL:0:8}"
else
  # Not waiting is the default now. Say so, and hand over the exact commands to check the
  # verdict later — an unwatched run is only acceptable if checking it is trivial.
  echo "push-both: not waiting for CI (default since 2026-08-07; --ci-wait to block)."
  echo "push-both: check it when convenient —"
  echo "    gh run list --workflow $CI_WORKFLOW --commit $LOCAL --event push --limit 1 \\"
  echo "      --json status,conclusion,url -q '.[0]|\"\\(.status) \\(.conclusion) \\(.url)\"'"
  echo "push-both: a 'cancelled' verdict is NO VERDICT — a later push superseded the run."
fi

# The mirror gets exactly the primary's tip. Gates are skipped because they
# already ran for this exact commit on the primary; running them twice proves
# nothing and doubles the wait.
if [ -z "$MIRROR" ]; then
  echo "push-both: MIRROR is empty — the mirror was SKIPPED on request. Both remotes are" >&2
  echo "push-both: required since 2026-10-22: publish ${LOCAL:0:8} to lambdaclass before calling this landed." >&2
  echo "push-both: published ${LOCAL:0:8}"
  exit 0
fi

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

# RE-VERIFY THE PRIMARY. The CI wait is ~45 minutes, and the primary is shared:
# another worktree or another agent can advance it in that window. If it has moved,
# ${LOCAL:0:8} is no longer what the primary publishes, and mirroring it would put
# the mirror behind the primary on a commit nobody is tracking — not harmful the way
# being AHEAD is, but still not the invariant this script promises (the mirror
# equals what the primary accepted). The newer tip needs its own gates and its own
# CI, so hand the job to that publish rather than half-doing it here.
pnow="$(git ls-remote "$PRIMARY" "refs/heads/$BRANCH" | cut -f1)"
if [ "$pnow" != "$LOCAL" ]; then
  echo "push-both: $PRIMARY/$BRANCH has advanced to ${pnow:0:8} during the CI wait." >&2
  echo "push-both: ${LOCAL:0:8} is CI-green but no longer the primary's tip, so the mirror is" >&2
  echo "push-both: NOT updated — publishing ${pnow:0:8} is that commit's own job." >&2
  exit 1
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
