#!/usr/bin/env bash
# Reducer predicate: exit 0 iff `concrete <candidate> --report <KIND>`
# output contains <SUBSTR>.
#
# Usage: expect-report-contains.sh <KIND> <SUBSTR> <candidate.con>
#
# Used to reduce around a specific report finding (a fact-report
# mismatch, a leaked authority claim, an unexpected proof status, ...).
# The candidate must compile far enough for the report to be produced;
# otherwise the predicate returns false.

set -uo pipefail

if [ $# -lt 3 ]; then
  echo "usage: $0 <KIND> <SUBSTR> <candidate.con>" >&2
  exit 2
fi

KIND="$1"
SUBSTR="$2"
CANDIDATE="$3"

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"

if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER" >&2
  exit 2
fi

OUT=$("$COMPILER" "$CANDIDATE" --report "$KIND" 2>&1)
RC=$?
if [ $RC -ne 0 ]; then
  exit 1
fi

echo "$OUT" | grep -qF "$SUBSTR" && exit 0 || exit 1
