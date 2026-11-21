#!/usr/bin/env bash
# Readiness notes (docs/LANDING.md, "Readiness"). A branch is ready to land when its head
# commit carries BOTH notes, published on origin:
#
#   refs/notes/concrete-review-v2 verdict: clean|findings|blocked   rounds: N   reviewer: codex   utc: ...
#                                base: <sha>   pieces: N   (format owned by scripts/review/review.sh and
#                                checked by scripts/review/verify.sh)
#   refs/notes/concrete-gates    gates: pass|fail          selected: N                   utc: ...
#
# One `key: value` per line; review schema is checked by verify.sh, gates use the last value. A note is attached to a
# commit, so a verdict is bound to that exact SHA: a new commit starts with no notes, and no rule has to say so.
#
#   notes.sh fetch                          copy the remote notes to refs/notes/remote/concrete-*
#   notes.sh get <kind> <sha> [<key>]       print the published note (or one key) of <sha>
#   notes.sh put gates <sha> key=value...   replace <sha>'s gates note on the remote, keeping every other
#                                           commit's note (fetch, rewrite, fast-forward push, retry)
#   notes.sh ready <sha> [required-base]    verify review scope and gates (docs/REVIEW.md)
#
# <kind> is review or gates. The remote is origin; see AGENTS.md, "Branches", for repository policy.
# Writers: scripts/review/review.sh (review) and scripts/land/ready.sh (gates).
# `put` never force-pushes: it rewrites on top of the remote's notes tip and retries on a race.
set -euo pipefail
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SELF_DIR/../review/refs.sh"
REMOTE=origin
DRY="${LAND_NOTES_DRY:-0}"
die() { echo "notes: $*" >&2; exit 1; }

kind_ok() { case "$1" in review|gates) ;; *) die "kind must be review or gates, not '$1'" ;; esac; }

kind_ref() {
  case "$1" in review) echo "$REVIEW_NOTES_REF" ;; gates) echo refs/notes/concrete-gates ;; esac
}

fetch_kind() {
  local ref
  ref="$(kind_ref "$1")"
  fetch_review_ref "$REMOTE" "$ref" "refs/notes/remote/${ref##*/}"
}

get_note() { # kind sha [key]
  local txt ref
  ref="$(kind_ref "$1")"
  txt="$(git notes --ref="refs/notes/remote/${ref##*/}" show "$2" 2>/dev/null)" || return 0
  if [ -n "${3:-}" ]; then
    printf '%s\n' "${txt%%$'\n\n'*}" | sed -n "s/^$3:[[:space:]]*//p" | tail -n 1
  else
    printf '%s\n' "$txt"
  fi
}

put_gates() { # sha key=value...
  local ref=refs/notes/concrete-gates
  local sha="$1"; shift
  sha="$(git rev-parse --verify "$sha^{commit}")" || die "put: '$2' is not a commit"
  local msg="" kv
  for kv in "$@"; do
    case "$kv" in *=*) ;; *) die "put: '$kv' is not key=value" ;; esac
    msg="$msg${kv%%=*}: ${kv#*=}"$'\n'
  done
  msg="${msg}utc: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  # Each write is built on its OWN temporary ref, so writers sharing this clone (or its linked
  # worktrees) never rewrite each other's local state; what is pushed is the captured commit.
  local try
  local tip
  local tmp="refs/notes/tmp/concrete-gates-$$-$RANDOM"
  local new
  trap 'git update-ref -d "'"$tmp"'" 2>/dev/null || true' RETURN
  for try in 1 2 3 4 5; do
    fetch_kind gates
    tip="$(git rev-parse -q --verify "refs/notes/remote/${ref##*/}" || true)"
    if [ -n "$tip" ]; then git update-ref "$tmp" "$tip"
    else git update-ref -d "$tmp" 2>/dev/null || true; fi
    git notes --ref="$tmp" add -f -m "$msg" "$sha" 2>/dev/null
    new="$(git rev-parse "$tmp")"
    if [ "$DRY" = 1 ]; then
      echo "notes (dry-run): would publish $ref on $REMOTE: $sha"; printf '%s\n' "$msg" | sed 's/^/    /'
      return 0
    fi
    # Notes carry no source tree: the pre-push gates have nothing of theirs to validate, and this
    # push is a fast-forward of a metadata ref (refused if anyone else moved it), so it skips them
    # by the documented switch.
    if CONCRETE_SKIP_GATES=1 git push -q "$REMOTE" "$new:$ref" 2>/dev/null; then
      git update-ref "refs/notes/remote/${ref##*/}" "$new"
      return 0
    fi
    sleep "$try"
  done
  die "put: could not publish $ref after 5 attempts (concurrent writers?)"
}

sub="${1:-}"; [ $# -gt 0 ] && shift
case "$sub" in
  fetch) fetch_kind review; fetch_kind gates ;;
  get) kind_ok "${1:-}"; [ -n "${2:-}" ] || die "get <kind> <sha> [key]"; get_note "$1" "$(git rev-parse "$2")" "${3:-}" ;;
  put)
    [ $# -ge 3 ] && [ "$1" = gates ] || die "put gates <sha> key=value... (review evidence is written by review.sh)"
    shift
    put_gates "$@" ;;
  ready)
    [ -n "${1:-}" ] || die "ready <sha>"
    s="$(git rev-parse "$1")"
    bash "$SELF_DIR/../review/verify.sh" \
      --notes-ref "refs/notes/remote/${REVIEW_NOTES_REF##*/}" --required-base "${2-$REMOTE/main}" "$s" && [ "$(get_note gates "$s" gates)" = pass ] ;;
  *) sed -n '2,22p' "$0" | sed 's/^# \{0,1\}//'; exit 2 ;;
esac
