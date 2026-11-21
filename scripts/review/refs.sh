# Review evidence refs. Legacy refs are read-only inputs to the new tooling.
REVIEW_NOTES_REF=refs/notes/concrete-review-v2
LEGACY_REVIEW_REF=refs/notes/concrete-review
LEGACY_FINDINGS_REF=refs/notes/concrete-review-findings
PUBLISHED_REVIEW_REF="refs/notes/remote/${REVIEW_NOTES_REF##*/}"
PUBLISHED_LEGACY_REVIEW_REF="refs/notes/remote/${LEGACY_REVIEW_REF##*/}"
PUBLISHED_LEGACY_FINDINGS_REF="refs/notes/remote/${LEGACY_FINDINGS_REF##*/}"

# Fetch an optional notes ref without mistaking a transport error for absence.
fetch_review_ref() {
  local remote="$1"
  local ref="$2"
  local dest="$3"
  local out
  if out="$(LC_ALL=C git fetch -q "$remote" "+$ref:$dest" 2>&1)"; then return 0; fi
  case "$out" in
    *"couldn't find remote ref $ref") git update-ref -d "$dest" ;;
    *) printf '%s\n' "$out" >&2; return 1 ;;
  esac
}
