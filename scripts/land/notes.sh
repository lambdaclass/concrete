#!/usr/bin/env bash
# Readiness notes (docs/project/LANDING.md, "Readiness"). A branch is ready to land when its head
# commit carries BOTH notes, published on the primary remote:
#
#   refs/notes/concrete-review   verdict: clean|findings   rounds: N   reviewer: codex   utc: ...
#   refs/notes/concrete-gates    gates: pass|fail          selected: N                   utc: ...
#
# One `key: value` per line; when a key repeats, the last line wins. Optional in either note:
# `order: N` (explicit landing order, lower first). A note is attached to a commit, so a verdict
# is bound to that exact SHA: a new commit starts with no notes, and no rule has to say so.
#
#   notes.sh fetch                          copy the remote notes to refs/notes/remote/concrete-*
#   notes.sh get <kind> <sha> [<key>]       print the published note (or one key) of <sha>
#   notes.sh put <kind> <sha> key=value...  replace <sha>'s note on the remote, keeping every other
#                                           commit's note (fetch, rewrite, fast-forward push, retry)
#   notes.sh ready <sha>                    exit 0 iff verdict is clean and gates is pass
#
# <kind> is review or gates. LAND_REMOTE (default origin) names the remote; `fork` is refused.
# Writers: scripts/review/review.sh (review), scripts/land/ready.sh and the landing bot (gates).
# `put` never force-pushes: it rewrites on top of the remote's notes tip and retries on a race.
set -euo pipefail
REMOTE="${LAND_REMOTE:-origin}"
DRY="${LAND_NOTES_DRY:-0}"
die() { echo "notes: $*" >&2; exit 1; }
[ "$REMOTE" != fork ] || die "refusing the remote named 'fork'"

kind_ok() { case "$1" in review|gates) ;; *) die "kind must be review or gates, not '$1'" ;; esac; }

fetch_kind() { # kind
  local out
  if out="$(git fetch -q "$REMOTE" "+refs/notes/concrete-$1:refs/notes/remote/concrete-$1" 2>&1)"; then return 0; fi
  case "$out" in
    *"couldn't find remote ref"*) git update-ref -d "refs/notes/remote/concrete-$1" 2>/dev/null || true; return 0 ;;
  esac
  echo "$out" >&2; return 1
}

get_note() { # kind sha [key]
  local txt
  txt="$(git notes --ref="refs/notes/remote/concrete-$1" show "$2" 2>/dev/null)" || return 0
  if [ -n "${3:-}" ]; then
    printf '%s\n' "$txt" | sed -n "s/^$3:[[:space:]]*//p" | tail -n 1
  else
    printf '%s\n' "$txt"
  fi
}

put_note() { # kind sha key=value...
  local kind="$1" sha="$2"; shift 2
  sha="$(git rev-parse --verify "$sha^{commit}")" || die "put: '$2' is not a commit"
  local msg="" kv
  for kv in "$@"; do
    case "$kv" in *=*) ;; *) die "put: '$kv' is not key=value" ;; esac
    msg="$msg${kv%%=*}: ${kv#*=}"$'\n'
  done
  msg="${msg}utc: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  local try tip
  for try in 1 2 3 4 5; do
    fetch_kind "$kind"
    tip="$(git rev-parse -q --verify "refs/notes/remote/concrete-$kind" || true)"
    if [ -n "$tip" ]; then git update-ref "refs/notes/concrete-$kind" "$tip"
    else git update-ref -d "refs/notes/concrete-$kind" 2>/dev/null || true; fi
    git notes --ref="refs/notes/concrete-$kind" add -f -m "$msg" "$sha" 2>/dev/null
    if [ "$DRY" = 1 ]; then
      echo "notes (dry-run): would publish refs/notes/concrete-$kind on $REMOTE: $sha"; printf '%s\n' "$msg" | sed 's/^/    /'
      return 0
    fi
    # Notes carry no source tree: the pre-push gates have nothing of theirs to validate, and this
    # push is a fast-forward of a metadata ref, so it skips them by the documented switch.
    if CONCRETE_SKIP_GATES=1 git push -q "$REMOTE" "refs/notes/concrete-$kind:refs/notes/concrete-$kind" 2>/dev/null; then
      git update-ref "refs/notes/remote/concrete-$kind" "refs/notes/concrete-$kind"
      return 0
    fi
    sleep "$try"
  done
  die "put: could not publish refs/notes/concrete-$kind after 5 attempts (concurrent writers?)"
}

sub="${1:-}"; [ $# -gt 0 ] && shift
case "$sub" in
  fetch) fetch_kind review; fetch_kind gates ;;
  get) kind_ok "${1:-}"; [ -n "${2:-}" ] || die "get <kind> <sha> [key]"; get_note "$1" "$(git rev-parse "$2")" "${3:-}" ;;
  put) kind_ok "${1:-}"; [ $# -ge 3 ] || die "put <kind> <sha> key=value..."; put_note "$@" ;;
  ready)
    [ -n "${1:-}" ] || die "ready <sha>"
    s="$(git rev-parse "$1")"
    [ "$(get_note review "$s" verdict)" = clean ] && [ "$(get_note gates "$s" gates)" = pass ] ;;
  *) sed -n '2,22p' "$0" | sed 's/^# \{0,1\}//'; exit 2 ;;
esac
