#!/usr/bin/env bash
# Reducer predicate: exit 0 iff the candidate compiles AND the
# compiled binary's trimmed stdout matches the expected value.
#
# Usage: expect-runtime-output.sh <EXPECTED> <candidate.con>
#
# EXPECTED may contain literal `\n` for multi-line output (matching
# the wrong-code corpus convention).

set -uo pipefail

if [ $# -lt 2 ]; then
  echo "usage: $0 <EXPECTED> <candidate.con>" >&2
  exit 2
fi

EXPECTED=$(printf '%b' "$1")
CANDIDATE="$2"

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"

if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER" >&2
  exit 2
fi

BIN=$(mktemp)
trap 'rm -f "$BIN"' EXIT

if ! "$COMPILER" "$CANDIDATE" -o "$BIN" >/dev/null 2>&1; then
  exit 1
fi

ACTUAL=$("$BIN" 2>/dev/null) || true
ACTUAL=$(printf '%s' "$ACTUAL" | sed -e 's/[[:space:]]*$//')

[ "$ACTUAL" = "$EXPECTED" ] && exit 0 || exit 1
