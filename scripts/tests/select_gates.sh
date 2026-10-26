#!/usr/bin/env bash
# WHICH CI GATE COMMANDS CAN THIS DIFF BREAK? (R-0513, impact-based pre-push selection)
#
# Usage: scripts/tests/select_gates.sh [--explain] [--files FILE] [<base-ref>]
#   <base-ref>  compare HEAD against merge-base(<base-ref>, HEAD), plus uncommitted and untracked
#               changes (default origin/main)
#   --files F   take the changed paths from F instead of git (one per line)
#   --explain   print `command<TAB>reason`
#        scripts/tests/select_gates.sh --base <sha> --head <sha>
#               exactly the range base..head (head must be the checked-out commit); the
#               landing script's interface
#
# Exit status is nonzero on any error; an empty stdout means nothing was selected.
# Prints the selected workflow gate commands, in the runner's spelling, one per line, and a
# count with a time estimate on stderr. `run_ci_gates_local.sh --impact <base>` runs them.
#
# WHY. The pre-push hook used to pick gates by AREA NAME, a hand-kept map from paths to
# gate-name substrings. On 2026-10-22 four failures reached CI that it did not select (the
# stack-budget gate after a resolver change, the trust gate's bug-audit corpus, the float
# round-trip judge, check_linear_discard after a heap rule), each costing a ~1.5 h CI round.
# This selector derives the map from the gates themselves on every run (see
# scripts/tests/lib/select_gates.py for the rules); check_gate_selection.sh replays those
# misses as fixed controls.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

EXPLAIN=(); FILES=""; BASE=""; XBASE=""; XHEAD=""
while [ $# -gt 0 ]; do
  case "$1" in
    --explain) EXPLAIN=(--explain); shift ;;
    --files)   [ -n "${2:-}" ] || { echo "select_gates: --files needs a path" >&2; exit 2; }
               FILES="$2"; shift 2 ;;
    # The landing script's interface (wip/landing-script): exactly the range base..head, no
    # working-tree changes. The gate definitions are read from this checkout, so head must be the
    # commit checked out here; anything else is refused rather than answered from other sources.
    --base)    [ -n "${2:-}" ] || { echo "select_gates: --base needs a commit" >&2; exit 2; }
               XBASE="$2"; shift 2 ;;
    --head)    [ -n "${2:-}" ] || { echo "select_gates: --head needs a commit" >&2; exit 2; }
               XHEAD="$2"; shift 2 ;;
    -h|--help) sed -n '2,10p' "$0"; exit 0 ;;
    -*)        echo "select_gates: unknown option $1" >&2; exit 2 ;;
    *)         BASE="$1"; shift ;;
  esac
done

TMPD="$(mktemp -d)"
trap 'rm -rf "$TMPD"' EXIT

if [ -n "$XBASE$XHEAD" ]; then
  [ -n "$XBASE" ] && [ -n "$XHEAD" ] && [ -z "$FILES$BASE" ] \
    || { echo "select_gates: --base and --head go together, without --files or a base-ref" >&2; exit 2; }
  b="$(git rev-parse --verify "$XBASE^{commit}")" || { echo "select_gates: unknown base $XBASE" >&2; exit 2; }
  h="$(git rev-parse --verify "$XHEAD^{commit}")" || { echo "select_gates: unknown head $XHEAD" >&2; exit 2; }
  [ "$h" = "$(git rev-parse HEAD)" ] \
    || { echo "select_gates: --head $XHEAD is not the checked-out commit; the gate definitions would not match" >&2; exit 2; }
  git diff --name-only "$b" "$h" | sed '/^$/d' | LC_ALL=C sort -u > "$TMPD/changed"
  MB="$b"
elif [ -n "$FILES" ]; then
  [ -f "$FILES" ] || { echo "select_gates: no such file $FILES" >&2; exit 2; }
  sed '/^$/d; /^#/d' "$FILES" | LC_ALL=C sort -u > "$TMPD/changed"
  # The base a workflow edit is compared against (which commands it added or re-spelled):
  # SELECT_GATES_BASE, else where HEAD left origin/main. With neither, a workflow edit selects all.
  MB="${SELECT_GATES_BASE:-$(git merge-base origin/main HEAD 2>/dev/null || true)}"
else
  BASE="${BASE:-origin/main}"
  MB="$(git merge-base "$BASE" HEAD)" || { echo "select_gates: no merge base with $BASE" >&2; exit 2; }
  { git diff --name-only "$MB" HEAD
    git diff --name-only
    git diff --name-only --cached
    git ls-files --others --exclude-standard
  } | sed '/^$/d' | LC_ALL=C sort -u > "$TMPD/changed"
fi

bash scripts/tests/run_ci_gates_local.sh --plan > "$TMPD/inventory" \
  || { echo "select_gates: the runner could not resolve the gate inventory" >&2; exit 2; }
[ -s "$TMPD/inventory" ] || { echo "select_gates: empty gate inventory" >&2; exit 2; }

if [ ! -s "$TMPD/changed" ]; then
  echo "select_gates: no changed files" >&2
  exit 0
fi
BASE_ARG=(); [ -n "${MB:-}" ] && [ "$MB" != none ] && BASE_ARG=(--base "$MB")
python3 scripts/tests/lib/select_gates.py --inventory "$TMPD/inventory" --changed "$TMPD/changed" \
  "${BASE_ARG[@]}" --durations scripts/tests/lib/gate_durations.tsv "${EXPLAIN[@]}"
