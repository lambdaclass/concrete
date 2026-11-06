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

# The note must carry each of the six fields review.sh writes exactly once, each well formed. Two
# optional keys are the landing pipeline's (scripts/land/notes.sh, docs/project/LANDING.md):
# `carried-from: <sha>`, written by the landing bot when it carries a clean verdict across a
# conflict-free rebase, and `order: N`. Any other key, a repeated key, or a line that is not
# `key: value` makes the note malformed. Keys are matched by name, not position, because notes.sh
# `put` (the bot's writer) appends `utc` last.
malformed() {
  echo "$SHA malformed review note${1:+ ($1)}:"
  printf '%s\n' "$NOTE" | sed 's/^/  /'
  exit 2
}
# Plain variables F_<key>, not an associative array: macOS /bin/bash is 3.2.
SEEN=" "
while IFS= read -r line; do
  [[ "$line" =~ ^([a-z-]+):\ (.*)$ ]] || malformed "not key: value: '$line'"
  k="${BASH_REMATCH[1]}" v="${BASH_REMATCH[2]}"
  case "$SEEN" in *" $k "*) malformed "repeated key $k" ;; esac
  SEEN="$SEEN$k "
  printf -v "F_${k//-/_}" '%s' "$v"
  case "$k" in
    verdict)      [[ "$v" =~ ^(clean|findings|blocked)$ ]] ;;
    rounds)       [[ "$v" =~ ^[1-9][0-9]*$ ]] ;;
    reviewer)     [[ "$v" =~ ^[^[:space:]]+$ ]] ;;
    utc)          [[ "$v" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$ ]] ;;
    base)         [[ "$v" =~ ^[0-9a-f]{40}$ ]] ;;
    pieces)       [[ "$v" =~ ^[0-9]+$ ]] ;;
    carried-from) [[ "$v" =~ ^[0-9a-f]{40}$ ]] ;;
    order)        [[ "$v" =~ ^[0-9]+$ ]] ;;
    *)            false ;;
  esac || malformed "bad $k"
done <<< "$NOTE"
for k in verdict rounds reviewer utc base pieces; do
  case "$SEEN" in *" $k "*) ;; *) malformed "missing $k" ;; esac
done

field() { local n="F_${1//-/_}"; printf '%s' "${!n:-}"; }
VERDICT="$(field verdict)"
CARRIED=""; [ -z "$(field carried-from)" ] || CARRIED=", carried from $(field carried-from)"
echo "$SHA $VERDICT (round $(field rounds), reviewer $(field reviewer), $(field utc), base $(field base), $(field pieces) piece(s)$CARRIED)"
[ "$VERDICT" = clean ] && exit 0
exit 1
