#!/usr/bin/env bash
# The impact-selected gates for exactly the range base..head, run in the current checkout (which must
# be at head). Shared by train.sh check, ready.sh and the landing bot's auto-rebase.
#
#   impact_gates.sh --base <sha> --head <sha> [--src-ref R] [--dst-ref R] [--report FILE]
#
# 1. The pre-push hook's own selection (R-0022 fast surface gates + touched areas), fed the line
#    `<src-ref> <head> <dst-ref> <base>`, so a later push presenting that range reuses its record.
#    The dst ref must not be wip/* (the hook's backup tier runs no gates); default refs/heads/ci/impact.
# 2. scripts/tests/select_gates.sh --base --head (R-0513), when it exists and the hook does not
#    already call it, run through run_ci_gates_local.sh --first.
# --report FILE receives `selected: N` (the select_gates count, or `hook` when only the hook ran).
# LAND_HOOK, LAND_REMOTE, LAND_NO_NIX as in train.sh.
set -euo pipefail
SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/land/lib.sh
. "$SELF_DIR/lib.sh"
die() { echo "impact: $*" >&2; exit 1; }
base="" head="" src="refs/heads/impact" dst="refs/heads/ci/impact" report=""
while [ $# -gt 0 ]; do
  case "$1" in
    --base) base="$2"; shift ;;
    --head) head="$2"; shift ;;
    --src-ref) src="$2"; shift ;;
    --dst-ref) dst="$2"; shift ;;
    --report) report="$2"; shift ;;
    *) die "unknown argument $1" ;;
  esac; shift
done
[ -n "$base" ] && [ -n "$head" ] || die "--base and --head are required"
case "$dst" in refs/heads/wip/*) die "--dst-ref $dst is in the hook's wip tier, which runs no gates" ;; esac
cd "$(git rev-parse --show-toplevel)"
HOOK="${LAND_HOOK:-.githooks/pre-push}"
REMOTE="${LAND_REMOTE:-origin}"
[ "$(git rev-parse HEAD)" = "$(git rev-parse "$head")" ] || die "the checkout is not at --head $head"
[ -f "$HOOK" ] || die "no pre-push hook at $HOOK"
url="$(git remote get-url "$REMOTE" 2>/dev/null || echo "$REMOTE")"
printf '%s %s %s %s\n' "$src" "$(git rev-parse "$head")" "$dst" "$(git rev-parse "$base")" \
  | land_in_nix bash "$HOOK" "$REMOTE" "$url"
selected=hook
if [ -f scripts/tests/select_gates.sh ] && ! grep -q select_gates "$HOOK" 2>/dev/null; then
  sel="$(bash scripts/tests/select_gates.sh --base "$base" --head "$head")"
  args=() c=""
  while IFS= read -r c; do [ -n "$c" ] && args+=(--first "$c"); done <<< "$sel"
  selected="$(( ${#args[@]} / 2 ))"
  if [ "${#args[@]}" -gt 0 ]; then land_in_nix bash scripts/tests/run_ci_gates_local.sh "${args[@]}"; fi
fi
[ -z "$report" ] || echo "selected: $selected" > "$report"
echo "impact: gates passed for ${base:0:10}..${head:0:10} (selected: $selected)"
