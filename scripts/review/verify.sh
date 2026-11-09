#!/usr/bin/env bash
# R-0544 — does a commit carry a clean review verdict? See docs/REVIEW.md.
#
#   scripts/review/verify.sh [--fetch] [--required-base BASE] [--fields]
#                            [--brief-file FILE] [--output-file FILE] [--evidence-dir DIR] <commit>
#
# Reads the git note that scripts/review/review.sh wrote on <commit> under
# refs/notes/concrete-review. The verdict counts only for that exact commit: a
# clean note on an ancestor says nothing about the commits after it.
#
#   --fetch           first fetch refs/notes/concrete-review (and concrete-writer) from $REVIEW_REMOTE
#                     (default origin), replacing the local copy (a reader, not a writer).
#   --required-base   accept a clean note only when its base is an ancestor of BASE,
#                     or BASE itself, so its reviewed range covers BASE..commit.
#   --fields          validate the note's schema and print its structured fields (`key: value` lines)
#                     independent of clean-provenance eligibility (exits 0 on any well-formed note,
#                     including findings, blocked and provenance-invalid clean notes;
#                     exits 2 when no note exists or the note is malformed).
#   --brief-file      optional brief file (or <commit>=<path>) to re-check against backfill brief-sha256.
#   --output-file     optional output file (or <commit>=<path>) to re-check against backfill output-sha256.
#   --evidence-dir    optional directory of <commit>.brief and <commit>.output files for backfill checks.
#
# Exit codes: 0 clean provenance (or well-formed note under --fields); 1 reviewed but not clean
#             provenance (findings, blocked, a clean note missing repository: readable or failing
#             writer-provenance / self-review / base-coverage checks); 2 no note, a malformed note,
#             or a usage error.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
NOTES_REF="${REVIEW_NOTES_REF:-refs/notes/concrete-review}"
WRITER_NOTES_REF="${REVIEW_WRITER_NOTES_REF:-refs/notes/concrete-writer}"
export REVIEW_WRITER_NOTES_REF="$WRITER_NOTES_REF"
REMOTE="${REVIEW_REMOTE:-origin}"
FETCH=0
FIELDS=0
REQUIRED_BASE=""
PROV_ARGS=()
while [ $# -gt 0 ]; do
  case "$1" in
    --fetch) FETCH=1; shift ;;
    --fields) FIELDS=1; shift ;;
    --required-base)
      [ $# -ge 2 ] && [ -n "$2" ] || { echo "verify: --required-base needs a commit" >&2; exit 2; }
      REQUIRED_BASE="$2"; shift 2 ;;
    --brief-file|--output-file|--evidence-dir|--allowed-signers)
      [ $# -ge 2 ] && [ -n "$2" ] || { echo "verify: $1 needs a value" >&2; exit 2; }
      PROV_ARGS+=("$1" "$2"); shift 2 ;;
    --) shift; break ;;
    -*) echo "verify: unknown option: $1" >&2; exit 2 ;;
    *) break ;;
  esac
done
[ $# -eq 1 ] || { echo "usage: verify.sh [--fetch] [--required-base BASE] [--fields] <commit>" >&2; exit 2; }
if [ "$FIELDS" = 1 ] && [ -n "$REQUIRED_BASE" ]; then
  echo "verify: --fields cannot be combined with --required-base" >&2
  exit 2
fi
if [ -n "$REQUIRED_BASE" ]; then
  REQUIRED_BASE="$(git rev-parse -q --verify "$REQUIRED_BASE^{commit}")" \
    || { echo "verify: required base is not a commit: $REQUIRED_BASE" >&2; exit 2; }
fi

if [ "$FETCH" = 1 ]; then
  git fetch -q "$REMOTE" "+$NOTES_REF:$NOTES_REF" \
    || { echo "verify: could not fetch $NOTES_REF from $REMOTE" >&2; exit 2; }
  werr=""
  if ! werr="$(git fetch -q "$REMOTE" "+$WRITER_NOTES_REF:$WRITER_NOTES_REF" 2>&1)"; then
    case "$werr" in
      *"couldn't find remote ref"*) git update-ref -d "$WRITER_NOTES_REF" 2>/dev/null || true ;;
      *) echo "verify: could not fetch $WRITER_NOTES_REF from $REMOTE: ${werr//$'\n'/ }" >&2; exit 2 ;;
    esac
  fi
fi

SHA="$(git rev-parse -q --verify "$1^{commit}")" || { echo "verify: not a commit: $1" >&2; exit 2; }
NOTE="$(git notes --ref="$NOTES_REF" show "$SHA" 2>/dev/null)" || {
  echo "$SHA unreviewed: no note under $NOTES_REF"
  exit 2
}

# The note must carry each of review.sh's seven common fields (verdict, rounds, reviewer, writer,
# utc, base, pieces) exactly once, each well formed. A clean note must also carry
# repository: readable. Any other key, a missing required key, a repeated key, or a line that is
# not `key: value` makes the note malformed. Keys are matched by name, not position, because
# notes.sh `put` appends `utc` last.
malformed() {
  echo "$SHA malformed review note${1:+ ($1)}:"
  printf '%s\n' "$NOTE" | sed 's/^/  /'
  exit 2
}
valid_writer_field() {
  [[ "$1" =~ ^(codex|kimi|agy|claude)(,(codex|kimi|agy|claude))*$ ]] || return 1
  local w seen=","
  local IFS=,
  for w in $1; do
    case "$seen" in *",$w,"*) return 1 ;; esac
    seen="$seen$w,"
  done
  return 0
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
    verdict)
      [ "$v" != invalid ] || malformed "invalid reviewer evidence is not a verdict"
      [[ "$v" =~ ^(clean|findings|blocked)$ ]] ;;
    rounds)       [[ "$v" =~ ^[1-9][0-9]*$ ]] ;;
    reviewer)     [[ "$v" =~ ^(codex|kimi|agy)$ ]] ;;
    writer)       valid_writer_field "$v" ;;
    utc)          [[ "$v" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$ ]] ;;
    base)         [[ "$v" =~ ^[0-9a-f]{40}$ ]] ;;
    pieces)       [[ "$v" =~ ^[0-9]+$ ]] ;;
    repository)   [ "$v" = readable ] ;;
    *)            false ;;
  esac || malformed "bad $k"
done <<< "$NOTE"
for k in verdict rounds reviewer writer utc base pieces; do
  case "$SEEN" in *" $k "*) ;; *) malformed "missing $k" ;; esac
done

field() { local n="F_${1//-/_}"; printf '%s' "${!n:-}"; }
VERDICT="$(field verdict)"
WRITER="$(field writer)"
REVIEWER="$(field reviewer)"

if [ "$FIELDS" = 1 ]; then
  printf 'commit: %s\nverdict: %s\nrounds: %s\nreviewer: %s\nwriter: %s\nutc: %s\nbase: %s\npieces: %s\nrepository: %s\n' \
    "$SHA" "$VERDICT" "$(field rounds)" "$REVIEWER" "$WRITER" \
    "$(field utc)" "$(field base)" "$(field pieces)" "$(field repository)"
  exit 0
fi

PROV_SUMMARY=""
if [ "$VERDICT" = clean ]; then
  if [ "$(field repository)" != readable ]; then
    echo "$SHA clean review note is not clean provenance: missing repository: readable"
    exit 1
  fi
  case ",$WRITER," in
    *,"$REVIEWER",*)
      echo "$SHA clean review note is not clean provenance: reviewer $REVIEWER reviewed its own work"
      exit 1 ;;
  esac
  if ! git merge-base --is-ancestor "$(field base)" "$SHA" 2>/dev/null; then
    echo "$SHA clean review note is not clean provenance: base $(field base) is not an ancestor-or-equal of $SHA"
    exit 1
  fi
  if [ -n "$REQUIRED_BASE" ] && ! git merge-base --is-ancestor "$(field base)" "$REQUIRED_BASE" 2>/dev/null; then
    echo "$SHA clean review note is not clean provenance: base $(field base) is not an ancestor-or-equal of required base $REQUIRED_BASE"
    exit 1
  fi
  if ! prov_out="$(python3 -I "$SCRIPT_DIR/writer_provenance.py" verify --base "$(field base)" --head "$SHA" --claimed-writer "$WRITER" --reviewer "$REVIEWER" "${PROV_ARGS[@]}" 2>&1)"; then
    echo "$SHA clean review note is not clean provenance: ${prov_out//$'\n'/ }"
    exit 1
  fi
  PROV_SUMMARY="${prov_out//$'\n'/ }"
fi
echo "$SHA $VERDICT (round $(field rounds), reviewer $(field reviewer), writer $WRITER, $(field utc), base $(field base), $(field pieces) piece(s)${PROV_SUMMARY:+; $PROV_SUMMARY})"
[ "$VERDICT" = clean ] && exit 0
exit 1
