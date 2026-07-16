#!/usr/bin/env bash
# Smoke test for the wrong-code bundle capture script.
#
# Captures a bundle on a known failing source (an E0209 rejection)
# and asserts every always-included file is present, plus at least
# one stage-conditional file. Does not run a long reduction or a
# full pipeline test.
#
# Contract: docs/BUG_BUNDLE.md

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi

PASS=0
FAIL=0
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

check_file() {
  local label="$1" path="$2"
  if [ -e "$path" ]; then
    echo "  ok   $label exists"
    PASS=$((PASS + 1))
  else
    echo "  FAIL $label missing: $path"
    FAIL=$((FAIL + 1))
  fi
}

check_file_contains() {
  local label="$1" path="$2" needle="$3"
  if [ ! -e "$path" ]; then
    echo "  FAIL $label missing: $path"
    FAIL=$((FAIL + 1))
    return
  fi
  if grep -qF "$needle" "$path"; then
    echo "  ok   $label contains '$needle'"
    PASS=$((PASS + 1))
  else
    echo "  FAIL $label does not contain '$needle': $path"
    FAIL=$((FAIL + 1))
  fi
}

# --------------------------------------------------------------------
# Case A: failing source (E0209). The pipeline fails at `check`, so
# we expect diagnostics but no IR / reports.
# --------------------------------------------------------------------
echo "=== bundle capture on E0209 source ==="
BDIR="$TMP/WC-smoke-fail"
bash scripts/tests/capture_wrong_code_bundle.sh \
    tests/programs/bug_int_match_disagree.con \
    --case WC-smoke-fail \
    --predicate error-code:E0209 \
    -o "$BDIR" >/dev/null 2>&1

# Always-included files
for f in bundle.json manifest.json command.txt predicate.txt \
         compiler-version.txt stdout.txt stderr.txt source/program.con; do
  check_file "$f" "$BDIR/$f"
done

# Predicate / case id round-trip into bundle.json
check_file_contains "bundle.json case_id" "$BDIR/bundle.json" '"case_id": "WC-smoke-fail"'
check_file_contains "bundle.json predicate" "$BDIR/bundle.json" 'error-code:E0209'

# predicate.txt and compiler-version.txt have content
check_file_contains "predicate.txt content" "$BDIR/predicate.txt" "error-code:E0209"
check_file_contains "compiler-version.txt mentions concrete" "$BDIR/compiler-version.txt" "concrete"

# Stage-conditional: this is a check-stage failure, so diagnostics.txt
# should be present and stderr should mention E0209.
check_file "diagnostics.txt (check failed)" "$BDIR/diagnostics.txt"
check_file_contains "stderr.txt mentions E0209" "$BDIR/stderr.txt" "E0209"

# IR / reports must NOT be present for a check-stage failure.
if [ -f "$BDIR/core.txt" ]; then
  echo "  FAIL core.txt unexpectedly present for check-stage failure"
  FAIL=$((FAIL + 1))
else
  echo "  ok   core.txt absent (check failed before elab)"
  PASS=$((PASS + 1))
fi

# --------------------------------------------------------------------
# Case B: passing source. Pipeline runs to completion, so we expect
# IR dumps and at least one report.
# --------------------------------------------------------------------
echo ""
echo "=== bundle capture on passing source ==="
BDIR2="$TMP/WC-smoke-pass"
bash scripts/tests/capture_wrong_code_bundle.sh \
    tests/programs/fib.con \
    --case WC-smoke-pass \
    --predicate runtime-output:55 \
    -o "$BDIR2" >/dev/null 2>&1

for f in bundle.json manifest.json command.txt predicate.txt \
         compiler-version.txt stdout.txt stderr.txt source/program.con \
         core.txt ssa.txt llvm.ll; do
  check_file "$f" "$BDIR2/$f"
done

if [ -d "$BDIR2/reports" ] && [ -n "$(ls -A "$BDIR2/reports" 2>/dev/null)" ]; then
  echo "  ok   reports/ has at least one entry"
  PASS=$((PASS + 1))
else
  echo "  FAIL reports/ missing or empty for clean compile"
  FAIL=$((FAIL + 1))
fi

# --------------------------------------------------------------------
# Wrapper integration: capture_wrong_code_bundle.sh refuses without
# required flags.
# --------------------------------------------------------------------
echo ""
echo "=== argument validation ==="
if bash scripts/tests/capture_wrong_code_bundle.sh tests/programs/fib.con --case WC-x -o "$TMP/x" >/dev/null 2>&1; then
  echo "  FAIL accepted invocation missing --predicate"
  FAIL=$((FAIL + 1))
else
  echo "  ok   rejects missing --predicate"
  PASS=$((PASS + 1))
fi
if bash scripts/tests/capture_wrong_code_bundle.sh tests/programs/fib.con --predicate runtime-output:55 -o "$TMP/y" >/dev/null 2>&1; then
  echo "  FAIL accepted invocation missing --case"
  FAIL=$((FAIL + 1))
else
  echo "  ok   rejects missing --case"
  PASS=$((PASS + 1))
fi

echo ""
echo "BUNDLE-SMOKE: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
