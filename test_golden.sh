#!/bin/bash
# Verify golden test baselines for --emit-core, --emit-ssa, and --fmt
set -e

COMPILER=".lake/build/bin/concrete"
SRC_DIR="golden_tests/src"
CORE_DIR="golden_tests/core"
SSA_DIR="golden_tests/ssa"
FMT_DIR="golden_tests/fmt"

if [ ! -x "$COMPILER" ]; then
  echo "Error: compiler not found at $COMPILER. Run 'lake build' first."
  exit 1
fi

passed=0
failed=0
errors=""

for src in "$SRC_DIR"/*.con; do
  name=$(basename "$src" .con)

  # Test Core IR
  core_expected="$CORE_DIR/$name.expected"
  if [ -f "$core_expected" ]; then
    core_actual=$("$COMPILER" "$src" --emit-core 2>&1) || true
    core_expected_content=$(cat "$core_expected")
    if [ "$core_actual" = "$core_expected_content" ]; then
      passed=$((passed + 1))
    else
      failed=$((failed + 1))
      errors="$errors\n  FAIL core/$name"
      echo "FAIL: core/$name"
      diff -u "$core_expected" <(echo "$core_actual") | head -20
      echo "---"
    fi
  fi

  # Test SSA IR
  ssa_expected="$SSA_DIR/$name.expected"
  if [ -f "$ssa_expected" ]; then
    ssa_actual=$("$COMPILER" "$src" --emit-ssa 2>&1) || true
    ssa_expected_content=$(cat "$ssa_expected")
    if [ "$ssa_actual" = "$ssa_expected_content" ]; then
      passed=$((passed + 1))
    else
      failed=$((failed + 1))
      errors="$errors\n  FAIL ssa/$name"
      echo "FAIL: ssa/$name"
      diff -u "$ssa_expected" <(echo "$ssa_actual") | head -20
      echo "---"
    fi
  fi

  # Test Formatter
  fmt_expected="$FMT_DIR/$name.expected"
  if [ -f "$fmt_expected" ]; then
    fmt_actual=$("$COMPILER" "$src" --fmt 2>&1) || true
    fmt_expected_content=$(cat "$fmt_expected")
    if [ "$fmt_actual" = "$fmt_expected_content" ]; then
      passed=$((passed + 1))
    else
      failed=$((failed + 1))
      errors="$errors\n  FAIL fmt/$name"
      echo "FAIL: fmt/$name"
      diff -u "$fmt_expected" <(echo "$fmt_actual") | head -20
      echo "---"
    fi
    # Also check idempotency: format(format(x)) == format(x)
    fmt_round2=$(echo "$fmt_actual" | "$COMPILER" /dev/stdin --fmt 2>&1) || true
    if [ "$fmt_actual" != "$fmt_round2" ]; then
      failed=$((failed + 1))
      errors="$errors\n  FAIL fmt/$name (not idempotent)"
      echo "FAIL: fmt/$name (not idempotent)"
      diff -u <(echo "$fmt_actual") <(echo "$fmt_round2") | head -20
      echo "---"
    else
      passed=$((passed + 1))
    fi
  fi
done

echo ""
echo "=== Golden Tests: $passed passed, $failed failed ==="
if [ $failed -gt 0 ]; then
  echo -e "Failures:$errors"
  exit 1
fi
