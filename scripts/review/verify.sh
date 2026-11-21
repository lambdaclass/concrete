#!/usr/bin/env bash
# R-0544 — does a commit carry a clean review verdict? See docs/REVIEW.md.
#
#   scripts/review/verify.sh [--fetch] [--notes-ref <ref>] [--required-base <commit>] <commit>
#
# Reads the git note that scripts/review/review.sh wrote on <commit> under
# refs/notes/concrete-review. The verdict counts only for that exact commit: a
# clean note on an ancestor says nothing about the commits after it.
#
#   --fetch  first fetch refs/notes/concrete-review from origin, replacing the local copy (a reader, not a writer).
#
#   --required-base  require the note base to be an ancestor-or-equal of this commit.
#
# Exit codes: 0 clean; 1 reviewed, not clean (findings or blocked);
#             2 no note, a malformed or insufficient-range note, or a usage error.

set -euo pipefail

NOTES_REF="refs/notes/concrete-review"
REMOTE=origin
FETCH=0
REQUIRED_BASE=""
METADATA_ONLY=0
while [ $# -gt 0 ]; do
  case "$1" in
    --metadata-only) METADATA_ONLY=1; shift ;; # round accounting only, never coverage
    --fetch) FETCH=1; shift ;;
    --notes-ref)
      [ $# -ge 2 ] && git check-ref-format "$2" || { echo "verify: --notes-ref needs a full ref" >&2; exit 2; }
      NOTES_REF="$2"; shift 2 ;;
    --required-base)
      [ $# -ge 2 ] || { echo "verify: --required-base needs a commit" >&2; exit 2; }
      REQUIRED_BASE="$(git rev-parse -q --verify "$2^{commit}")" \
        || { echo "verify: required base is not a commit: $2" >&2; exit 2; }
      shift 2 ;;
    *) break ;;
  esac
done
[ $# -eq 1 ] || { echo "usage: verify.sh [--fetch] [--notes-ref <ref>] [--required-base <commit>] <commit>" >&2; exit 2; }

if [ "$FETCH" = 1 ]; then
  git fetch -q "$REMOTE" "+$NOTES_REF:$NOTES_REF" \
    || { echo "verify: could not fetch $NOTES_REF from $REMOTE" >&2; exit 2; }
fi

SHA="$(git rev-parse -q --verify "$1^{commit}")" || { echo "verify: not a commit: $1" >&2; exit 2; }
invalid() { echo "$SHA unverified: $*"; exit 2; }
# Note fields are unordered. Read the exact blob whose identity was checked,
# rather than looking up the annotation again to read its contents.
malformed() {
  echo "$SHA malformed review note${1:+ ($1)}:"
  printf '%s\n' "$NOTE" | sed 's/^/  /'
  exit 2
}
# Plain variables F_<key>, not an associative array: macOS /bin/bash is 3.2.
read_note() {
  local expected="$1"
  local blob
  blob="$(git notes --ref="$NOTES_REF" list "$SHA" 2>/dev/null)" \
    || invalid "no note under $NOTES_REF"
  [ -z "$expected" ] || [ "$blob" = "$expected" ] \
    || invalid "predecessor note changed (expected $expected, found $blob)"
  NOTE="$(git cat-file blob "$blob")" || invalid "could not read note blob $blob"
  local SEEN=" " line k v
  F_diff_start="" F_predecessor_note="" F_findings_note=""
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
      base|diff-start|predecessor-note|findings-note) [[ "$v" =~ ^[0-9a-f]{40}$ ]] ;;
      pieces)       [[ "$v" =~ ^[0-9]+$ ]] ;;
      *)            false ;;
    esac || malformed "bad $k"
  done <<< "$NOTE"
  for k in verdict rounds reviewer utc base pieces; do
    case "$SEEN" in *" $k "*) ;; *) malformed "missing $k" ;; esac
  done
  if [ -n "$F_diff_start" ]; then
    if [ "$F_diff_start" = "$F_base" ]; then
      [ -z "$F_predecessor_note" ] || malformed "full review has a predecessor"
    else
      [ -n "$F_predecessor_note" ] || malformed "missing predecessor-note"
    fi
    if [ "$F_verdict" = clean ]; then
      [ -z "$F_findings_note" ] || malformed "clean note has findings-note"
    fi
  else
    [ -z "$F_predecessor_note$F_findings_note" ] || malformed "missing diff-start"
  fi
}

check_range() {
  if [ -n "$REQUIRED_BASE" ]; then
    git merge-base --is-ancestor "$F_base" "$REQUIRED_BASE" 2>/dev/null \
      || invalid "review range does not cover required base $REQUIRED_BASE: note base $F_base is not an ancestor-or-equal"
  fi
  git merge-base --is-ancestor "$F_base" "$SHA" 2>/dev/null \
    || invalid "note base $F_base is not an ancestor-or-equal of the reviewed commit"
  if [ -z "$F_diff_start" ] && [ "$F_rounds" != 1 ]; then
    invalid "legacy multi-round note cannot establish full or delta coverage"
  fi
}

read_note ""
VERDICT="$F_verdict"
summary="$SHA $VERDICT (round $F_rounds, reviewer $F_reviewer, $F_utc, base $F_base, $F_pieces piece(s))"
if [ "$METADATA_ONLY" = 0 ]; then
  if [ -n "$F_diff_start" ] && [ "$VERDICT" != clean ] && [ -z "$F_findings_note" ]; then
    malformed "missing findings-note"
  fi
  check_range
  while [ -n "$F_predecessor_note" ]; do
    [ "$F_diff_start" != "$SHA" ] && \
      git merge-base --is-ancestor "$F_diff_start" "$SHA" 2>/dev/null && \
      git merge-base --is-ancestor "$F_base" "$F_diff_start" 2>/dev/null \
      || invalid "delta start $F_diff_start must be between base and head"
    REQUIRED_BASE="$F_base"
    SHA="$F_diff_start"
    read_note "$F_predecessor_note"
    [ "$F_verdict" = clean ] || invalid "predecessor verdict is $F_verdict; expected clean"
    check_range
  done
fi
echo "$summary"
[ "$VERDICT" = clean ] && exit 0
exit 1
