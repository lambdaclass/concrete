#!/usr/bin/env bash
# CHECKER PIN FRESHNESS — are the external checkers we re-check proofs with (external_checker.pins)
# the revisions the Lean Kernel Arena currently evaluates?
#
# Independent checkers protect us only while they are current: kernel bug lean4#14576 got past a
# week-old nanoda, and Lean 4.35.0's #15288 fixed thread-sharing undefined behaviour that it says may
# affect Lean-based checkers such as con-leche. A pin that lags upstream soundness fixes is tracked in
# docs/verification/KNOWN_HOLES.md ("Checker pins can lag upstream soundness fixes"); this is its
# control. See docs/verification/EXTERNAL_CHECKER.md.
#
# Source of truth, per checker (con-leche, nanoda):
#   1. the Arena's published page https://arena.lean-lang.org/checker/<name>/ links the checker's
#      definition at the exact lean-kernel-arena commit the published results were produced from
#      (…/lean-kernel-arena/blob/<sha>/checkers/<name>.yaml); that file's `rev:` is the evaluated
#      revision. That is the comparison.
#   2. If the page or the file cannot be read, the checker repository's default-branch head
#      (`git ls-remote <url> HEAD`) is used instead, and the output says so.
# Also printed, never compared: the upstream head and the revision the Arena's master branch names
# for its next round, so a reviewer sees what a bump would move to.
#
# Tiers (never a merge blocker — landings must stay reproducible, and an upstream commit is not a
# defect in the candidate):
#   --tier landing   (default) a stale pin is a WARNING (a GitHub `::warning::` annotation on CI);
#                    exit 0. Unreachable sources are an explicit SKIP; exit 0.
#   --tier campaign  a stale pin is a FINDING: exit 1. The nightly campaign runs on CI with network
#                    access, so an unreachable source is also a finding there (exit 1): a check that
#                    silently skipped every night would never report anything.
# The gate makes no network request beyond the two reads above and runs nothing it downloads.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
for git_local_var in $(git rev-parse --local-env-vars 2>/dev/null); do unset "$git_local_var"; done
TIER=landing
case "${1:-}" in
  "") ;;
  --tier) TIER="${2:-}" ;;
  *) echo "usage: $0 [--tier landing|campaign]" >&2; exit 2 ;;
esac
case "$TIER" in landing|campaign) ;; *) echo "error: unknown tier '$TIER'" >&2; exit 2 ;; esac
ARENA="${CONCRETE_ARENA_URL:-https://arena.lean-lang.org}"
ARENA_RAW="${CONCRETE_ARENA_RAW:-https://raw.githubusercontent.com/leanprover/lean-kernel-arena}"
# shellcheck source=external_checker.pins
. scripts/tests/external_checker.pins
PIN_TMP="$(mktemp -d)"; trap 'rm -rf "$PIN_TMP"' EXIT
BOUNDED_PARENT="$PIN_TMP"; source scripts/tests/lib/bounded.sh

STALE=0; UNKNOWN=0; FRESH=0
note(){ echo "  info $1"; }
warn(){
  echo "  WARN $1"
  [ "${GITHUB_ACTIONS:-}" = true ] && echo "::warning title=Checker pin freshness::$1"
  return 0
}
fetch(){ curl -fsSL --max-time 30 --retry 2 "$1" 2>/dev/null; }
ls_head(){
  local out
  # Bounded: an unreachable remote that hangs is the same finding as one that refuses (no head).
  out=$(bounded_run 60 -- git ls-remote "$1" HEAD 2>/dev/null)
  awk 'NR==1 && $1 ~ /^[0-9a-f]{40}$/ {print $1}' <<<"$out"
}
yaml_rev(){ sed -n 's/^rev:[[:space:]]*\([0-9a-f]\{40\}\)[[:space:]]*$/\1/p' | awk 'NR==1'; }

# check NAME PIN URL
check(){
  local name=$1 pin=$2 url=$3 page commit evaluated="" source="" head next
  page=$(fetch "$ARENA/checker/$name/") || page=""
  commit=$(grep -oE "lean-kernel-arena/blob/[0-9a-f]{40}/checkers/$name\.yaml" <<<"$page" \
    | sed -E 's#lean-kernel-arena/blob/([0-9a-f]{40})/.*#\1#' | sort -u)
  if [ -n "$commit" ] && [ "$(grep -c . <<<"$commit")" = 1 ]; then
    evaluated=$(fetch "$ARENA_RAW/$commit/checkers/$name.yaml" | yaml_rev)
    source="the Arena's published results (lean-kernel-arena@${commit:0:12})"
  fi
  head=$(ls_head "$url")
  if [ -z "$evaluated" ] && [ -n "$head" ]; then
    evaluated=$head; source="the upstream default-branch head (the Arena page was unreadable)"
  fi
  next=$(fetch "$ARENA_RAW/master/checkers/$name.yaml" | yaml_rev)
  if [ -z "$evaluated" ]; then
    UNKNOWN=$((UNKNOWN+1))
    echo "  SKIP $name: neither $ARENA/checker/$name/ nor $url is reachable (offline?); pin ${pin:0:12} not compared"
    return
  fi
  note "$name: pinned ${pin:0:12}; upstream head ${head:-unknown}; Arena master (next round) ${next:-unknown}"
  if [ "$pin" = "$evaluated" ]; then
    FRESH=$((FRESH+1)); echo "  ok   $name pin ${pin:0:12} is the revision of $source"
  else
    STALE=$((STALE+1))
    warn "$name pin ${pin:0:12} differs from ${evaluated:0:12}, the revision of $source. Evaluate the bump (docs/verification/EXTERNAL_CHECKER.md)."
  fi
}

echo "=== checker pins vs the Lean Kernel Arena (tier: $TIER) ==="
check con-leche "$CONLECHE_REV" "$CONLECHE_URL"
check nanoda "$NANODA_REV" "$NANODA_URL"
note "lean4export is not an Arena checker: it is pinned to the tag for this repository's lean-toolchain (AXIOMS.md, Updating the pins)"
echo "CHECKER-PIN-FRESHNESS: tier=$TIER FRESH=$FRESH STALE=$STALE UNKNOWN=$UNKNOWN"
if [ "$TIER" = campaign ] && [ $((STALE+UNKNOWN)) -gt 0 ]; then
  echo "FINDING: $STALE stale and $UNKNOWN undetermined checker pin(s)"; exit 1
fi
[ "$STALE" -gt 0 ] && echo "(landing tier: stale pins warn, never fail)"
[ "$UNKNOWN" -gt 0 ] && echo "SKIP: $UNKNOWN pin(s) not compared (no network)"
exit 0
