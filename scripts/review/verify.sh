#!/usr/bin/env bash
# R-0544 — does a commit carry a clean review verdict? See docs/project/REVIEW.md.
#
#   scripts/review/verify.sh [--fetch] <commit>
#
# Reads the git note that scripts/review/review.sh wrote on <commit> under
# refs/notes/concrete-review. The verdict counts only for that exact commit: a
# clean note on an ancestor says nothing about the commits after it.
#
#   --fetch  first fetch refs/notes/concrete-review from $REVIEW_REMOTE (default
#            origin), replacing the local copy (a reader, not a writer).
#
# Exit codes: 0 clean; 1 reviewed, not clean (findings or blocked);
#             2 no note, a malformed note, or a usage error.

set -euo pipefail

NOTES_REF="refs/notes/concrete-review"
REMOTE="${REVIEW_REMOTE:-origin}"
FETCH=0
if [ "${1:-}" = --fetch ]; then FETCH=1; shift; fi
[ $# -eq 1 ] || { echo "usage: verify.sh [--fetch] <commit>" >&2; exit 2; }

if [ "$FETCH" = 1 ]; then
  git fetch -q "$REMOTE" "+$NOTES_REF:$NOTES_REF" \
    || { echo "verify: could not fetch $NOTES_REF from $REMOTE" >&2; exit 2; }
fi

SHA="$(git rev-parse -q --verify "$1^{commit}")" || { echo "verify: not a commit: $1" >&2; exit 2; }
NOTE="$(git notes --ref="$NOTES_REF" show "$SHA" 2>/dev/null)" || {
  echo "$SHA unreviewed: no note under $NOTES_REF"
  exit 2
}

# The note must have exactly the six fields review.sh writes, in order.
FORMAT='^verdict: (clean|findings|blocked)
rounds: [1-9][0-9]*
reviewer: [^[:space:]]+
utc: [0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z
base: [0-9a-f]{40}
pieces: [0-9]+$'
if ! [[ "$NOTE" =~ $FORMAT ]]; then
  echo "$SHA malformed review note:"
  printf '%s\n' "$NOTE" | sed 's/^/  /'
  exit 2
fi

field() { printf '%s\n' "$NOTE" | awk -v k="$1: " 'index($0, k) == 1 { print substr($0, length(k) + 1); exit }'; }
VERDICT="$(field verdict)"
echo "$SHA $VERDICT (round $(field rounds), reviewer $(field reviewer), $(field utc), base $(field base), $(field pieces) piece(s))"
[ "$VERDICT" = clean ] && exit 0
exit 1
