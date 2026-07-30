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
# Order is the whole point: primary gates -> primary lands -> mirror
# fast-forwards to exactly the primary's tip.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

PRIMARY="${PRIMARY:-origin}"
MIRROR="${MIRROR:-lambdaclass}"
BRANCH="${BRANCH:-main}"
LOCAL="$(git rev-parse HEAD)"

echo "push-both: $PRIMARY (gated) then $MIRROR (fast-forward)"
if ! git push "$PRIMARY" "HEAD:$BRANCH"; then
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

# The mirror gets exactly the primary's tip. Gates are skipped because they
# already ran for this exact commit on the primary; running them twice proves
# nothing and doubles the wait.
if ! CONCRETE_SKIP_GATES=1 git push "$MIRROR" "HEAD:$BRANCH"; then
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
