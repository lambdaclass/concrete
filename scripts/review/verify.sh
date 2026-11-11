#!/usr/bin/env bash
# R-0544 — does a commit carry a clean review verdict? See docs/REVIEW.md.
#
#   scripts/review/verify.sh [--fetch] [--required-base <commit>] <commit>
#
# Reads the git note that scripts/review/review.sh wrote on <commit> under
# refs/notes/concrete-review. The verdict counts only for that exact commit: a
# clean note on an ancestor says nothing about the commits after it.
#
#   --fetch  first fetch refs/notes/concrete-review from $REVIEW_REMOTE (default
#            origin), replacing the local copy (a reader, not a writer).
#
#   --required-base  require the note base to be an ancestor-or-equal of this commit.
#
# Exit codes: 0 clean; 1 reviewed, not clean (findings or blocked);
#             2 no note, a malformed or insufficient-range note, or a usage error.

set -euo pipefail

NOTES_REF="${REVIEW_NOTES_REF:-refs/notes/concrete-review}"
REMOTE="${REVIEW_REMOTE:-origin}"
FETCH=0
REQUIRED_BASE=""
while [ $# -gt 0 ]; do
  case "$1" in
    --fetch) FETCH=1; shift ;;
    --required-base)
      [ $# -ge 2 ] || { echo "verify: --required-base needs a commit" >&2; exit 2; }
      REQUIRED_BASE="$(git rev-parse -q --verify "$2^{commit}")" \
        || { echo "verify: required base is not a commit: $2" >&2; exit 2; }
      shift 2 ;;
    *) break ;;
  esac
done
[ $# -eq 1 ] || { echo "usage: verify.sh [--fetch] [--required-base <commit>] <commit>" >&2; exit 2; }

if [ "$FETCH" = 1 ]; then
  git fetch -q "$REMOTE" "+$NOTES_REF:$NOTES_REF" \
    || { echo "verify: could not fetch $NOTES_REF from $REMOTE" >&2; exit 2; }
fi

SHA="$(git rev-parse -q --verify "$1^{commit}")" || { echo "verify: not a commit: $1" >&2; exit 2; }
NOTE="$(git notes --ref="$NOTES_REF" show "$SHA" 2>/dev/null)" || {
  echo "$SHA unreviewed: no note under $NOTES_REF"
  exit 2
}

# The note must carry each of the six fields review.sh writes exactly once, each well formed. Any
# other key, a repeated key, or a line that is not `key: value` makes the note malformed. Keys are
# matched by name, not position, because notes.sh `put` appends `utc` last.
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
    *)            false ;;
  esac || malformed "bad $k"
done <<< "$NOTE"
for k in verdict rounds reviewer utc base pieces; do
  case "$SEEN" in *" $k "*) ;; *) malformed "missing $k" ;; esac
done

field() { local n="F_${1//-/_}"; printf '%s' "${!n:-}"; }
if [ -n "$REQUIRED_BASE" ]; then
  git merge-base --is-ancestor "$(field base)" "$REQUIRED_BASE" 2>/dev/null || {
    echo "$SHA review range does not cover required base $REQUIRED_BASE: note base $(field base) is not an ancestor-or-equal"
    exit 2
  }
fi
VERDICT="$(field verdict)"
echo "$SHA $VERDICT (round $(field rounds), reviewer $(field reviewer), $(field utc), base $(field base), $(field pieces) piece(s))"
[ "$VERDICT" = clean ] && exit 0
exit 1
