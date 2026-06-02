#!/usr/bin/env bash
# Reducer predicate: exit 0 iff compiling the candidate produces a
# diagnostic containing the given error code (e.g. "E0708").
#
# Usage: expect-error-code.sh <CODE> <candidate.con>
#
# Designed to be called by the Lean reducer through the
# `external:<cmd>` predicate kind. The candidate file path is the
# final argument; any preceding args are predicate arguments.

set -uo pipefail

if [ $# -lt 2 ]; then
  echo "usage: $0 <CODE> <candidate.con>" >&2
  exit 2
fi

CODE="$1"
CANDIDATE="$2"

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"

if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER" >&2
  exit 2
fi

OUT=$("$COMPILER" "$CANDIDATE" -o /dev/null 2>&1)
RC=$?
if [ $RC -eq 0 ]; then
  exit 1
fi
echo "$OUT" | grep -q "$CODE" && exit 0 || exit 1
