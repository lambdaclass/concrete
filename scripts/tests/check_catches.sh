#!/usr/bin/env bash
# "Concrete catches this" CI gate.
#
# Walks every examples/*/catches/*.con, compiles each, and asserts
# the program is REJECTED with the diagnostic substring declared in
# its file header. Each case must carry a comment header line:
#
#   // catches-substring: <text-that-must-appear-in-diagnostics>
#
# An optional sibling line `// catches: E####` is informational only
# (the substring is the authoritative match — error codes can
# evolve, the substring should be stable to the user-visible
# rejection).
#
# Contract: examples/<example>/CATCHES.md per example.

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi

mapfile -t CASES < <(find examples -path '*/catches/*.con' | sort)

if [ ${#CASES[@]} -eq 0 ]; then
  echo "No catches/*.con files found under examples/. Nothing to check."
  exit 0
fi

PASS=0
FAIL=0

for case in "${CASES[@]}"; do
  substring=$(grep -oE '^// catches-substring:\s*.*' "$case" \
              | sed -E 's|^// catches-substring:\s*||')
  if [ -z "$substring" ]; then
    echo "  FAIL $case — missing '// catches-substring: <text>' header"
    FAIL=$((FAIL + 1))
    continue
  fi

  out=$("$COMPILER" "$case" -o /dev/null 2>&1)
  rc=$?

  if [ $rc -eq 0 ]; then
    echo "  FAIL $case — expected rejection (substring '$substring') but compile succeeded"
    FAIL=$((FAIL + 1))
    continue
  fi

  if ! grep -qF "$substring" <<<"$out"; then
    echo "  FAIL $case — rejected with wrong message"
    echo "    expected substring: $substring"
    echo "    actual (first line): $(echo "$out" | head -1)"
    FAIL=$((FAIL + 1))
    continue
  fi

  echo "  ok   $case — rejected with '$substring'"
  PASS=$((PASS + 1))
done

echo ""
echo "CATCHES: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
