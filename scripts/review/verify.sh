#!/usr/bin/env bash
# R-0544 — does a commit carry a clean review verdict? See docs/REVIEW.md.
#
#   scripts/review/verify.sh [--fetch] [--metadata-only] [--notes-ref <ref>] [--required-base <commit>] <commit>
#
# Reads the git note that scripts/review/review.sh wrote on <commit> under
# refs/notes/concrete-review-v2. The verdict counts only for that exact commit: a
# clean note on an ancestor says nothing about the commits after it.
#
#   --fetch  first fetch review evidence from origin into the selected snapshot.
#
#   --required-base  require the note base to be an ancestor-or-equal of this commit.
#   --metadata-only  validate schema and base for round accounting, with legacy fallback.
#                    Returns 0 for valid metadata regardless of verdict.
#   --legacy-accounting  internal migration mode for schema-only legacy accounting.
#
# Exit codes: 0 clean; 1 reviewed, not clean (findings or blocked);
#             2 no note, a malformed or insufficient-range note, or a usage error.

set -euo pipefail

source "$(dirname "${BASH_SOURCE[0]}")/refs.sh"
NOTES_REF="$REVIEW_NOTES_REF"
REMOTE=origin
FETCH=0
REQUIRED_BASE=""
METADATA_ONLY=0
LEGACY_ACCOUNTING=0
LEGACY=0
LEGACY_FALLBACK="$LEGACY_REVIEW_REF"
while [ $# -gt 0 ]; do
  case "$1" in
    --legacy-accounting) LEGACY_ACCOUNTING=1; METADATA_ONLY=1; shift ;;
    --metadata-only) METADATA_ONLY=1; shift ;; # round accounting only, never coverage
    --fetch) FETCH=1; shift ;;
    --notes-ref)
      [ $# -ge 2 ] && git check-ref-format "$2" || { echo "verify: --notes-ref needs a full ref" >&2; exit 2; }
      NOTES_REF="$2"; LEGACY_FALLBACK=""; shift 2 ;;
    --required-base)
      [ $# -ge 2 ] || { echo "verify: --required-base needs a commit" >&2; exit 2; }
      REQUIRED_BASE="$(git rev-parse -q --verify "$2^{commit}")" \
        || { echo "verify: required base is not a commit: $2" >&2; exit 2; }
      shift 2 ;;
    *) break ;;
  esac
done
[ $# -eq 1 ] || { echo "usage: verify.sh [--fetch] [--metadata-only] [--notes-ref <ref>] [--required-base <commit>] <commit>" >&2; exit 2; }

if [ "$FETCH" = 1 ]; then
  case "$NOTES_REF" in
    "$REVIEW_NOTES_REF"|"$PUBLISHED_REVIEW_REF")
      NOTES_REF="$PUBLISHED_REVIEW_REF"
      fetch_review_ref "$REMOTE" "$REVIEW_NOTES_REF" "$NOTES_REF" || exit 2
      if [ "$METADATA_ONLY" = 1 ]; then
        fetch_review_ref "$REMOTE" "$LEGACY_REVIEW_REF" "$PUBLISHED_LEGACY_REVIEW_REF" || exit 2
      fi ;;
    "$LEGACY_REVIEW_REF"|"$PUBLISHED_LEGACY_REVIEW_REF")
      NOTES_REF="$PUBLISHED_LEGACY_REVIEW_REF"
      fetch_review_ref "$REMOTE" "$LEGACY_REVIEW_REF" "$NOTES_REF" || exit 2 ;;
    *) fetch_review_ref "$REMOTE" "$NOTES_REF" "$NOTES_REF" || exit 2 ;;
  esac
fi
if [ "$NOTES_REF" = "$PUBLISHED_REVIEW_REF" ]; then
  LEGACY_FALLBACK="$PUBLISHED_LEGACY_REVIEW_REF"
fi

SHA="$(git rev-parse -q --verify "$1^{commit}")" || { echo "verify: not a commit: $1" >&2; exit 2; }
invalid() { echo "$SHA unverified: $*"; exit 2; }
if [ "$METADATA_ONLY" = 1 ] && [ -n "$LEGACY_FALLBACK" ] &&
   ! git notes --ref="$NOTES_REF" list "$SHA" >/dev/null 2>&1; then
  NOTES_REF="$LEGACY_FALLBACK"
fi
case "$NOTES_REF" in "$LEGACY_REVIEW_REF"|"$PUBLISHED_LEGACY_REVIEW_REF") LEGACY=1 ;; esac
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
  local header="$NOTE"
  local findings=""
  if [[ "$NOTE" == *$'\n\n'* ]]; then
    header="${NOTE%%$'\n\n'*}"
    findings="${NOTE#*$'\n\n'}"
  fi
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
  done <<< "$header"
  for k in verdict rounds reviewer utc base pieces; do
    case "$SEEN" in *" $k "*) ;; *) malformed "missing $k" ;; esac
  done
  if [ "$LEGACY" = 1 ]; then
    [ "$METADATA_ONLY" = 1 ] || invalid "legacy note is only eligible for round accounting"
    case "$F_reviewer" in v2-denial-*) invalid "legacy denial requires its v2 evidence" ;; esac
    [ -z "$F_diff_start$F_predecessor_note$F_findings_note$findings" ] \
      || malformed "unexpected fields in legacy note"
    return
  fi
  [ -n "$F_diff_start" ] || malformed "missing diff-start"
  if [ "$F_diff_start" = "$F_base" ]; then
    [ -z "$F_predecessor_note" ] || malformed "full review has a predecessor"
  else
    [ -n "$F_predecessor_note" ] || malformed "missing predecessor-note"
  fi
  if [ "$F_verdict" = clean ]; then
    [ "$header" = "$NOTE" ] && [ -z "$F_findings_note" ] || malformed "clean note has findings"
  else
    [[ "$findings" =~ [^[:space:]] ]] &&
      [ "$(printf '%s\n' "$findings" | git hash-object --stdin)" = "$F_findings_note" ] \
      || malformed "missing or mismatched findings text"
  fi
}

check_range() {
  if [ -n "$REQUIRED_BASE" ]; then
    git merge-base --is-ancestor "$F_base" "$REQUIRED_BASE" 2>/dev/null \
      || invalid "review range does not cover required base $REQUIRED_BASE: note base $F_base is not an ancestor-or-equal"
  fi
  git merge-base --is-ancestor "$F_base" "$SHA" 2>/dev/null \
    || invalid "note base $F_base is not an ancestor-or-equal of the reviewed commit"
  if [ -n "$F_predecessor_note" ]; then
    [ "$F_diff_start" != "$SHA" ] &&
      git merge-base --is-ancestor "$F_diff_start" "$SHA" 2>/dev/null &&
      git merge-base --is-ancestor "$F_base" "$F_diff_start" 2>/dev/null \
      || invalid "delta start $F_diff_start must be between base and head"
  fi
}

read_note ""
# Historical writers accepted a base off the reviewed branch. Only migration
# accounting may use that metadata without checking its range.
if [ "$LEGACY_ACCOUNTING" = 1 ]; then
  [ "$LEGACY" = 1 ] && [ -z "$REQUIRED_BASE" ] || invalid "legacy accounting requires a legacy ref without a required base"
else
  check_range
fi
VERDICT="$F_verdict"
summary="$SHA $VERDICT (round $F_rounds, reviewer $F_reviewer, $F_utc, base $F_base, $F_pieces piece(s))"
if [ "$METADATA_ONLY" = 1 ]; then
  echo "$summary [round accounting only]"
  exit 0
fi
while [ -n "$F_predecessor_note" ]; do
  REQUIRED_BASE="$F_base"
  SHA="$F_diff_start"
  read_note "$F_predecessor_note"
  [ "$F_verdict" = clean ] || invalid "predecessor verdict is $F_verdict; expected clean"
  check_range
done
echo "$summary"
[ "$VERDICT" = clean ] && exit 0
exit 1
