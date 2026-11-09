#!/usr/bin/env bash
# Readiness notes (docs/LANDING.md, "Readiness"). A branch is ready to land when its head
# commit carries BOTH notes, published on the primary remote:
#
#   refs/notes/concrete-review   verdict: clean|findings|blocked   rounds: N   reviewer: codex|kimi|agy
#                                writer: codex|kimi|agy|claude   utc: ...   base: <sha>   pieces: N
#                                repository: readable (clean notes only; format owned by review.sh,
#                                checked by verify.sh)
#   refs/notes/concrete-gates    gates: pass|fail          selected: N                   utc: ...
#
# One `key: value` per line; when a key repeats, the last line wins. A note is attached to a
# commit, so a verdict is bound to that exact SHA: a new commit starts with no notes, and no rule has to say so.
#
#   notes.sh fetch                          copy the remote notes to refs/notes/remote/concrete-*
#   notes.sh get <kind> <sha> [<key>]       print the published note (or one key) of <sha>
#   notes.sh put <kind> <sha> key=value...  replace <sha>'s note on the remote, keeping every other
#                                           commit's note (fetch, rewrite, fast-forward push, retry)
#   notes.sh ready [--base <ref>] <sha>     exit 0 iff verify.sh accepts the review note (covering
#                                           merge-base(<base>, <sha>)) and gates is pass
#
# <kind> is review, gates or writer (`put` accepts only review or gates; writer notes are signed and
# append-only, managed only through scripts/review/writer_provenance.py). LAND_REMOTE (default origin)
# names the remote; `fork` is refused.
# Writers: scripts/review/review.sh (review), scripts/review/writer_provenance.py (writer) and
# scripts/land/ready.sh (gates).
# `put` never force-pushes: it rewrites on top of the remote's notes tip and retries on a race.
set -euo pipefail
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VERIFY="$SELF_DIR/../review/verify.sh"
WRITER_PROV="$SELF_DIR/../review/writer_provenance.py"
REMOTE="${LAND_REMOTE:-origin}"
DRY="${LAND_NOTES_DRY:-0}"
die() { echo "notes: $*" >&2; exit 1; }
[ "$REMOTE" != fork ] || die "refusing the remote named 'fork'"

kind_ok() { case "$1" in review|gates|writer) ;; *) die "kind must be review, gates or writer, not '$1'" ;; esac; }

fetch_kind() { # kind
  local out
  if [ "$1" = writer ]; then
    local prev_remote prev_local old_rev tmp_ref="refs/notes/remote/concrete-writer-fetch-$$" werr
    prev_remote="$(git rev-parse -q --verify refs/notes/remote/concrete-writer 2>/dev/null || true)"
    prev_local="$(git rev-parse -q --verify refs/notes/concrete-writer 2>/dev/null || true)"
    old_rev="${prev_remote:-${prev_local:-0000000000000000000000000000000000000000}}"
    git update-ref -d "$tmp_ref" 2>/dev/null || true
    if out="$(git fetch -q "$REMOTE" "+refs/notes/concrete-writer:$tmp_ref" 2>&1)"; then
      if ! werr="$(python3 -B -I "$WRITER_PROV" validate-notes --allow-missing-targets --notes-rev "$tmp_ref" --old-notes-rev "$old_rev" 2>&1)"; then
        git update-ref -d "$tmp_ref" 2>/dev/null || true
        echo "notes: invalid or discontinuous refs/notes/concrete-writer on $REMOTE: ${werr//$'\n'/ }" >&2
        return 1
      fi
      git update-ref refs/notes/remote/concrete-writer "$tmp_ref"
      git update-ref -d "$tmp_ref" 2>/dev/null || true
      return 0
    fi
    git update-ref -d "$tmp_ref" 2>/dev/null || true
    case "$out" in
      *"couldn't find remote ref"*)
        if [ -n "$prev_remote" ]; then
          echo "notes: remote $REMOTE deleted refs/notes/concrete-writer (was ${prev_remote:0:10})" >&2
          return 1
        fi
        git update-ref -d refs/notes/remote/concrete-writer 2>/dev/null || true
        return 0 ;;
    esac
    echo "$out" >&2; return 1
  fi
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
  [ "$kind" != writer ] || die "put: writer notes are signed and append-only; publish them only through scripts/review/writer_provenance.py"
  sha="$(git rev-parse --verify "$sha^{commit}")" || die "put: '$2' is not a commit"
  local msg="" kv
  for kv in "$@"; do
    case "$kv" in *=*) ;; *) die "put: '$kv' is not key=value" ;; esac
    msg="$msg${kv%%=*}: ${kv#*=}"$'\n'
  done
  msg="${msg}utc: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  # Each write is built on its OWN temporary ref, so writers sharing this clone (or its linked
  # worktrees) never rewrite each other's local state; what is pushed is the captured commit.
  local try tip tmp="refs/notes/tmp/concrete-$kind-$$-$RANDOM" new
  trap 'git update-ref -d "'"$tmp"'" 2>/dev/null || true' RETURN
  for try in 1 2 3 4 5; do
    fetch_kind "$kind"
    tip="$(git rev-parse -q --verify "refs/notes/remote/concrete-$kind" || true)"
    if [ -n "$tip" ]; then git update-ref "$tmp" "$tip"
    else git update-ref -d "$tmp" 2>/dev/null || true; fi
    git notes --ref="$tmp" add -f -m "$msg" "$sha" 2>/dev/null
    new="$(git rev-parse "$tmp")"
    if [ "$DRY" = 1 ]; then
      echo "notes (dry-run): would publish refs/notes/concrete-$kind on $REMOTE: $sha"; printf '%s\n' "$msg" | sed 's/^/    /'
      return 0
    fi
    # Review and gates notes take the pre-push hook's review-notes tier directly (no gate bypass).
    if git push -q "$REMOTE" "$new:refs/notes/concrete-$kind" 2>/dev/null; then
      git update-ref "refs/notes/remote/concrete-$kind" "$new"
      return 0
    fi
    sleep "$try"
  done
  die "put: could not publish refs/notes/concrete-$kind after 5 attempts (concurrent writers?)"
}

sub="${1:-}"; [ $# -gt 0 ] && shift
case "$sub" in
  fetch) fetch_kind review; fetch_kind gates; fetch_kind writer ;;
  get) kind_ok "${1:-}"; [ -n "${2:-}" ] || die "get <kind> <sha> [key]"; get_note "$1" "$(git rev-parse "$2")" "${3:-}" ;;
  put) kind_ok "${1:-}"; [ $# -ge 3 ] || die "put <kind> <sha> key=value..."; put_note "$@" ;;
  ready)
    baseref=""
    while [ $# -gt 0 ]; do
      case "$1" in
        --base) [ $# -ge 2 ] || die "ready: --base needs a ref"; baseref="$2"; shift 2 ;;
        *) break ;;
      esac
    done
    [ -n "${1:-}" ] || die "ready [--base <ref>] <sha>"
    s="$(git rev-parse "$1")"
    req_base=""
    if [ -n "$baseref" ]; then
      req_base="$(git merge-base "$baseref" "$s")"
    elif git rev-parse -q --verify "$REMOTE/main^{commit}" >/dev/null 2>&1; then
      req_base="$(git merge-base "$REMOTE/main" "$s" 2>/dev/null || true)"
    fi
    REVIEW_NOTES_REF="refs/notes/remote/concrete-review" \
    REVIEW_WRITER_NOTES_REF="refs/notes/remote/concrete-writer" \
      bash "$VERIFY" ${req_base:+--required-base "$req_base"} "$s" >/dev/null 2>&1 \
      && [ "$(get_note gates "$s" gates)" = pass ] ;;
  *) sed -n '2,23p' "$0" | sed 's/^# \{0,1\}//'; exit 2 ;;
esac
