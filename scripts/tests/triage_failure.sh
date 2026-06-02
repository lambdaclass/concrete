#!/usr/bin/env bash
# Failure triage: auto-reduce and bundle a failing .con file.
#
# Usage:
#   bash scripts/tests/triage_failure.sh <file.con> [predicate]
#
# If no predicate is given, auto-detects the failure mode.
# Produces:
#   <file>_triage/
#     reduced.con    — minimized reproduction
#     bundle/        — debug bundle (pipeline state at failure)
#     triage.txt     — summary: predicate, original size, reduced size, error
#
# Examples:
#   bash scripts/tests/triage_failure.sh tests/programs/broken.con
#   bash scripts/tests/triage_failure.sh tests/programs/broken.con check-error
#   bash scripts/tests/triage_failure.sh tests/programs/broken.con "check-error:non-copy field"

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"

if [[ $# -lt 1 ]]; then
  echo "Usage: triage_failure.sh <file.con> [predicate]"
  echo
  echo "Predicates: parse-error, resolve-error, check-error, elab-error,"
  echo "            core-check-error, mono-error, lower-error, crash,"
  echo "            consistency-violation, verify-warning"
  echo "  Add :substring to match specific error text (e.g. check-error:non-copy)"
  exit 1
fi

SOURCE="$1"
PREDICATE="${2:-}"

if [[ ! -f "$SOURCE" ]]; then
  echo "error: file not found: $SOURCE"
  exit 1
fi

if [[ ! -x "$COMPILER" ]]; then
  echo "error: compiler not built. Run 'lake build' first."
  exit 1
fi

BASENAME=$(basename "$SOURCE" .con)
TRIAGE_DIR="${SOURCE%.con}_triage"
mkdir -p "$TRIAGE_DIR"

echo "=== Failure Triage: $SOURCE ==="
echo

# Step 1: Capture the failure
echo "--- Capturing failure ---"
ERROR_OUTPUT=$($COMPILER "$SOURCE" 2>&1 || true)
EXIT_CODE=$($COMPILER "$SOURCE" >/dev/null 2>&1; echo $?)

if [[ "$EXIT_CODE" -eq 0 ]]; then
  # Compilation succeeded — check if it's a wrong-output bug
  BINARY="${SOURCE%.con}"
  if [[ -x "$BINARY" ]]; then
    RUNTIME_OUTPUT=$("$BINARY" 2>&1 || true)
    RUNTIME_EXIT=$("$BINARY" >/dev/null 2>&1; echo $?)
    echo "  Compilation succeeded. Runtime exit=$RUNTIME_EXIT"
    echo "  Runtime output: $RUNTIME_OUTPUT"
    echo "  Note: wrong-output bugs need manual oracle comparison."
    echo "  Cannot auto-reduce without a predicate."
    if [[ -z "$PREDICATE" ]]; then
      echo
      echo "Provide an expected exit code or predicate to enable reduction."
      # Still generate debug bundle
      echo "--- Generating debug bundle ---"
      $COMPILER debug-bundle "$SOURCE" -o "$TRIAGE_DIR/bundle" 2>&1 || true
      echo "  Bundle: $TRIAGE_DIR/bundle/"
      # Write summary
      cat > "$TRIAGE_DIR/triage.txt" <<EOF
Triage: $SOURCE
Date: $(date -u +%Y-%m-%dT%H:%M:%SZ)
Predicate: none (compilation succeeded)
Compile exit: 0
Runtime exit: $RUNTIME_EXIT
Runtime output: $RUNTIME_OUTPUT
Reduced: no (no failure predicate)
EOF
      echo
      echo "Triage output: $TRIAGE_DIR/"
      exit 0
    fi
  fi
fi

echo "  Exit code: $EXIT_CODE"
echo "  Error: $(echo "$ERROR_OUTPUT" | head -3)"

# Step 2: Auto-detect predicate if not given
if [[ -z "$PREDICATE" ]]; then
  echo
  echo "--- Auto-detecting predicate ---"
  if echo "$ERROR_OUTPUT" | grep -q "error\[parse\]"; then
    PREDICATE="parse-error"
  elif echo "$ERROR_OUTPUT" | grep -q "error\[resolve\]"; then
    PREDICATE="resolve-error"
  elif echo "$ERROR_OUTPUT" | grep -q "error\[check\]"; then
    PREDICATE="check-error"
  elif echo "$ERROR_OUTPUT" | grep -q "error\[elab\]"; then
    PREDICATE="elab-error"
  elif echo "$ERROR_OUTPUT" | grep -q "error\[core-check\]"; then
    PREDICATE="core-check-error"
  elif echo "$ERROR_OUTPUT" | grep -q "error\[post-mono\]"; then
    PREDICATE="mono-error"
  elif echo "$ERROR_OUTPUT" | grep -q "error\[lower\]"; then
    PREDICATE="lower-error"
  elif echo "$ERROR_OUTPUT" | grep -q "LLVM IR validation failed"; then
    PREDICATE="crash"
  elif echo "$ERROR_OUTPUT" | grep -q "clang.*error"; then
    PREDICATE="crash"
  else
    PREDICATE="crash"
  fi
  echo "  Detected: $PREDICATE"
fi

# Step 3: Reduce
echo
echo "--- Reducing with predicate: $PREDICATE ---"
REDUCED="$TRIAGE_DIR/reduced.con"
if $COMPILER reduce "$SOURCE" --predicate "$PREDICATE" -o "$REDUCED" 2>&1; then
  ORIG_LINES=$(wc -l < "$SOURCE")
  REDUCED_LINES=$(wc -l < "$REDUCED")
  echo "  Reduced: $ORIG_LINES → $REDUCED_LINES lines"
else
  echo "  Reduction failed or made no progress."
  cp "$SOURCE" "$REDUCED"
fi

# Step 4: Debug bundle
echo
echo "--- Generating debug bundle ---"
$COMPILER debug-bundle "$SOURCE" -o "$TRIAGE_DIR/bundle" 2>&1 || true
echo "  Bundle: $TRIAGE_DIR/bundle/"

# Step 5: Capture reduced error
REDUCED_ERROR=$($COMPILER "$REDUCED" 2>&1 || true)

# Step 6: Write summary
cat > "$TRIAGE_DIR/triage.txt" <<EOF
Triage: $SOURCE
Date: $(date -u +%Y-%m-%dT%H:%M:%SZ)
Predicate: $PREDICATE
Original lines: $(wc -l < "$SOURCE")
Reduced lines: $(wc -l < "$REDUCED")

--- Original error ---
$ERROR_OUTPUT

--- Reduced error ---
$REDUCED_ERROR
EOF

echo
echo "=== Triage complete ==="
echo "  Reduced:  $REDUCED"
echo "  Bundle:   $TRIAGE_DIR/bundle/"
echo "  Summary:  $TRIAGE_DIR/triage.txt"
echo
echo "Next steps:"
echo "  1. Review the reduced reproduction"
echo "  2. File as docs/bugs/NNN_<name>.md"
echo "  3. Copy reduced.con to tests/programs/bug_<name>.con"
echo "  4. Add to BUG_TEST_MAP in scripts/tests/audit_bug_corpus.sh"
echo "  5. Add run_ok/run_err entry in scripts/tests/run_tests.sh"
