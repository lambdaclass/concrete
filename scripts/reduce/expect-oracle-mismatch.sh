#!/usr/bin/env bash
# Reducer predicate: exit 0 iff the candidate exhibits an
# oracle mismatch — compiled binary stdout differs from `--interp`
# stdout, both produce stdout, and neither side raises a PENDING
# (`interp: ...`) diagnostic.
#
# Usage: expect-oracle-mismatch.sh <candidate.con>
#
# This is the predicate to use when reducing a real semantic
# disagreement surfaced by the Phase A oracle harness. It rejects
# candidates that compile-fail, that the interpreter does not yet
# support, or that happen to agree.

set -uo pipefail

if [ $# -lt 1 ]; then
  echo "usage: $0 <candidate.con>" >&2
  exit 2
fi

CANDIDATE="$1"

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"

if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER" >&2
  exit 2
fi

BIN=$(mktemp)
trap 'rm -f "$BIN"' EXIT

# self-printing wrapper (MAIN_EXIT_MODEL stage 2): compiled side prints its
# own result so its stdout is comparable with interp's value print.
source "$ROOT_DIR/scripts/tests/lib/selfprint.sh"
WRAPPED=$(mktemp).con
trap 'rm -f "$BIN" "$WRAPPED"' EXIT
gate_selfprint_wrap "$CANDIDATE" "$WRAPPED"

if ! "$COMPILER" "$WRAPPED" -o "$BIN" >/dev/null 2>&1; then
  exit 1
fi
COMPILED=$("$BIN" 2>/dev/null) || true
COMPILED=$(printf '%s' "$COMPILED" | sed -e 's/[[:space:]]*$//')

INTERP=$("$COMPILER" "$CANDIDATE" --interp 2>&1)
case "$INTERP" in
  "interp: "*) exit 1 ;;
esac
INTERP=$(printf '%s' "$INTERP" | sed -e 's/[[:space:]]*$//')

[ "$COMPILED" != "$INTERP" ] && exit 0 || exit 1
